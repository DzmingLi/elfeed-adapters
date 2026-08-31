;;; elfeed-adapters-douban.el --- Douban timelines for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Consume the public Douban mobile timeline API directly.  When a broadcast
;; links to a personal topic or public review, replace the API's truncated card
;; subtitle with the full web article.  The optional reply-notification source
;; reads only direct comment replies to the logged-in user; likes and other
;; notification kinds are deliberately excluded.

;;; Code:

(require 'browser-cookies)
(require 'dom)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defgroup elfeed-adapters-douban nil
  "Douban sources for Elfeed Adapters."
  :group 'elfeed-adapters
  :prefix "elfeed-adapters-douban-")

(defcustom elfeed-adapters-douban-browser 'firefox
  "Browser backend used to read the Douban session."
  :type '(choice (const firefox) (const chromium) (const chrome)
                 (const brave) (const edge) (const vivaldi))
  :group 'elfeed-adapters-douban)

(defcustom elfeed-adapters-douban-profile-directory nil
  "Explicit browser profile directory containing the Douban session."
  :type '(choice (const :tag "Not configured" nil) directory)
  :group 'elfeed-adapters-douban)

(defun elfeed-adapters-douban--match (url)
  "Return parameters when URL is a supported Douban adapter URL."
  (cond
   ((string-match-p
     (rx string-start "adapter:douban/notifications/replies"
         (optional "?" (* nonl)) string-end)
     url)
    '(:kind replies))
   ((string-match
     (rx string-start "adapter:douban/people/"
         (group (+ digit)) "/status"
         (optional "?" (group (* nonl))) string-end)
     url)
    ;; `url-parse-query-string' changes match data, so save both captures
    ;; before parsing the query.
    (let* ((user-id (match-string 1 url))
           (query-string (match-string 2 url))
           (query (and query-string
                       (url-parse-query-string query-string)))
           (filter (cadr (assoc "filterout_title" query))))
      (list :kind 'timeline
            :user-id user-id
            :filter-title
            (and filter
                 (decode-coding-string (url-unhex-string filter) 'utf-8)))))))

(defun elfeed-adapters-douban--image-html (image)
  "Render IMAGE plist as HTML."
  (when-let* ((large (plist-get image :large))
              (url (plist-get large :url)))
    (format "<p><img src=\"%s\"></p>" (xml-escape-string url))))

(defun elfeed-adapters-douban--date (value)
  "Parse Douban local timestamp VALUE as China Standard Time."
  (when (and (stringp value)
             (string-match-p
              (rx string-start (= 4 digit) "-" (= 2 digit) "-" (= 2 digit)
                  " " (= 2 digit) ":" (= 2 digit) ":" (= 2 digit)
                  string-end)
              value))
    (truncate (float-time (date-to-time (concat value " +0800"))))))

(defun elfeed-adapters-douban--text-html (text)
  "Escape Douban plain TEXT and preserve its source line breaks."
  (replace-regexp-in-string
   "\n" "<br>"
   (xml-escape-string
    (replace-regexp-in-string "\r\n?" "\n" (or text "")))
   t t))

(defun elfeed-adapters-douban--topic-url (status)
  "Return the personal-topic web URL linked by STATUS, if any."
  (let* ((card (plist-get status :card))
         (url (plist-get card :url)))
    (when (and (equal (plist-get card :type) "topic")
               (stringp url)
               (string-match-p
                (rx string-start "https://www.douban.com/topic/" (+ digit) "/")
                url))
      url)))

(defun elfeed-adapters-douban--review-url (status)
  "Return the public review URL linked by STATUS, if any."
  (let* ((card (plist-get status :card))
         (url (plist-get card :url)))
    (when (and (equal (plist-get card :type) "review")
               (stringp url)
               (string-match-p
                (rx string-start "https://"
                    (or "book" "movie") ".douban.com/review/"
                    (+ digit) "/" string-end)
                url))
      url)))

(defun elfeed-adapters-douban--headers (url)
  "Build browser-authenticated headers for Douban URL."
  (when-let* ((cookie
               (browser-cookies-header
                url :browser elfeed-adapters-douban-browser
                :profile-directory elfeed-adapters-douban-profile-directory)))
    `(("Cookie" . ,cookie)
      ("Referer" . "https://www.douban.com/"))))

(defun elfeed-adapters-douban--notification-session ()
  "Return authenticated headers and the current Douban user ID.

Signal an error when the configured browser profile has no usable Douban
session."
  (let* ((url "https://www.douban.com/reply_notify/")
         (cookies
          (browser-cookies-get
           url :browser elfeed-adapters-douban-browser
           :profile-directory elfeed-adapters-douban-profile-directory))
         (dbcl2 (cdr (assoc-string "dbcl2" cookies t)))
         (user-id
          (and dbcl2
               (string-match (rx string-start (optional "\"")
                                 (group (+ digit)) ":")
                             dbcl2)
               (match-string 1 dbcl2))))
    (unless (and cookies user-id)
      (error "No logged-in Douban browser session found"))
    (list :user-id user-id
          :headers
          `(("Cookie" .
             ,(mapconcat (lambda (cookie)
                           (format "%s=%s" (car cookie) (cdr cookie)))
                         cookies "; "))
            ("Referer" . "https://www.douban.com/")))))

(defun elfeed-adapters-douban--absolute-url (url)
  "Make a root-relative Douban URL URL absolute."
  (when url
    (if (string-prefix-p "/" url)
        (concat "https://www.douban.com" url)
      url)))

(defun elfeed-adapters-douban--reply-notifications (html)
  "Parse reply-notification descriptors from authenticated Douban HTML."
  (with-temp-buffer
    (insert html)
    (let* ((document (libxml-parse-html-region (point-min) (point-max)))
           (nodes (dom-by-class document "new-reply-item")))
      (delq
       nil
       (mapcar
        (lambda (node)
          (when-let* ((node-id (dom-attr node 'id))
                      ((string-match
                        (rx string-start "reply_notify_"
                            (group (+ digit)) string-end)
                        node-id))
                      (notification-id (match-string 1 node-id))
                      (content (car (dom-by-class node "content")))
                      (anchor (car (dom-by-tag content 'a)))
                      (href (dom-attr anchor 'href)))
            (list :notification-id notification-id
                  :resolver-url
                  (elfeed-adapters-douban--absolute-url href))))
        nodes)))))

(defun elfeed-adapters-douban--status-target (html)
  "Return (STATUS-ID SOURCE-URL) discovered in status page HTML."
  (when (string-match
         (rx (group "https://www.douban.com/people/" (+ digit) "/status/"
                    (group (+ digit))))
         html)
    (list (match-string 2 html) (match-string 1 html))))

(defun elfeed-adapters-douban--all-comments (comments)
  "Flatten Douban COMMENTS and their nested replies."
  (cl-mapcan
   (lambda (comment)
     (cons comment
           (elfeed-adapters-douban--all-comments
            (or (plist-get comment :replies) nil))))
   comments))

(defun elfeed-adapters-douban--direct-replies (payload user-id)
  "Return comments in PAYLOAD that directly reply to USER-ID."
  (seq-filter
   (lambda (comment)
     (let* ((author (plist-get comment :author))
            (ref-comment (plist-get comment :ref_comment))
            (ref-author (plist-get ref-comment :author)))
       (and (not (plist-get comment :is_deleted))
            (equal (format "%s" (plist-get ref-author :id)) user-id)
            (not (equal (format "%s" (plist-get author :id)) user-id)))))
   (elfeed-adapters-douban--all-comments
    (or (plist-get payload :comments) nil))))

(defun elfeed-adapters-douban--reply-item (comment source-url)
  "Convert direct reply COMMENT associated with SOURCE-URL to an item."
  (let* ((id (format "%s" (plist-get comment :id)))
         (author (plist-get comment :author))
         (author-name (or (plist-get author :name) "豆瓣用户"))
         (text (or (plist-get comment :text) ""))
         (comment-url (format "%s#comment_%s" source-url id))
         (title-text
          (string-trim
           (replace-regexp-in-string (rx (+ (or "\n" "\r" space)))
                                     " " text))))
    (list :guid (concat "douban-reply-comment-" id)
          :title
          (format "%s 回复了你：%s"
                  author-name
                  (truncate-string-to-width title-text 100 nil nil "…"))
          :link comment-url
          :base-url source-url
          :date (elfeed-adapters-douban--date
                 (plist-get comment :create_time))
          :authors (list author-name)
          :categories '("notification" "reply")
          :content
          (format
           "<blockquote><p>%s</p></blockquote><p><a href=\"%s\">在豆瓣查看所回复的评论或原广播</a></p>"
           (elfeed-adapters-douban--text-html text)
           (xml-escape-string comment-url))
          :content-type 'html)))

(defun elfeed-adapters-douban--fetch-notification-comments
    (notification user-id headers callback)
  "Fetch direct comments for NOTIFICATION and call CALLBACK with items.

USER-ID identifies the logged-in account and HEADERS contains its browser
session.  Failures for an individual notification yield an empty list."
  (let ((resolver-url (plist-get notification :resolver-url)))
    (elfeed-adapters-request
     resolver-url
     (lambda (resolver-error status-html)
       (if resolver-error
           (funcall callback nil)
         (if-let* ((target (elfeed-adapters-douban--status-target status-html))
                   (status-id (car target))
                   (source-url (cadr target)))
             (let ((comments-url
                    (format
                     "https://m.douban.com/rexxar/api/v2/status/%s/comments?start=0&count=20"
                     status-id)))
               (elfeed-adapters-request
                comments-url
                (lambda (comments-error comments-body)
                  (if comments-error
                      (funcall callback nil)
                    (condition-case nil
                        (let* ((payload
                                (json-parse-string
                                 comments-body :object-type 'plist
                                 :array-type 'list :null-object nil
                                 :false-object nil))
                               (comments
                                (elfeed-adapters-douban--direct-replies
                                 payload user-id)))
                          (funcall
                           callback
                           (mapcar
                            (lambda (comment)
                              (elfeed-adapters-douban--reply-item
                               comment source-url))
                            comments)))
                      (error (funcall callback nil)))))
                `(("Cookie" . ,(cdr (assoc-string "Cookie" headers t)))
                  ("Referer" . ,source-url))))
           (funcall callback nil))))
     headers)))

(defun elfeed-adapters-douban--reply-result (items)
  "Return a normalized reply-notification result containing ITEMS."
  (list :title "豆瓣回应我的"
        :namespace "douban.com"
        :items items))

(defun elfeed-adapters-douban--collect-reply-notifications
    (notifications user-id headers callback)
  "Collect direct replies from NOTIFICATIONS, then call CALLBACK.

USER-ID identifies the logged-in account and HEADERS carries its session."
  (let ((pending (length notifications))
        items)
    (if (zerop pending)
        (funcall callback nil (elfeed-adapters-douban--reply-result nil))
      (dolist (notification notifications)
        (elfeed-adapters-douban--fetch-notification-comments
         notification user-id headers
         (lambda (reply-items)
           (setq items (nconc reply-items items))
           (when (zerop (cl-decf pending))
             (setq items
                   (seq-uniq
                    items
                    (lambda (left right)
                      (equal (plist-get left :guid)
                             (plist-get right :guid)))))
             (funcall callback nil
                      (elfeed-adapters-douban--reply-result items)))))))))

(defun elfeed-adapters-douban--fetch-replies (callback)
  "Fetch direct Douban comment replies and call CALLBACK."
  (condition-case session-error
      (let* ((session (elfeed-adapters-douban--notification-session))
             (user-id (plist-get session :user-id))
             (headers (plist-get session :headers))
             (index-url "https://www.douban.com/reply_notify/"))
        (elfeed-adapters-request
         index-url
         (lambda (index-error body)
           (if index-error
               (funcall callback index-error nil)
             (condition-case parse-error
                 (elfeed-adapters-douban--collect-reply-notifications
                  (elfeed-adapters-douban--reply-notifications body)
                  user-id headers callback)
               (error (funcall callback parse-error nil)))))
         headers))
    (error (funcall callback session-error nil))))

(defun elfeed-adapters-douban--extract-topic-html (html)
  "Extract the full personal-topic body from Douban HTML."
  (with-temp-buffer
    (insert html)
    (let* ((document (libxml-parse-html-region (point-min) (point-max)))
           (content (car (dom-by-class document "topic-richtext"))))
      (when content
        (with-temp-buffer
          (dom-print content)
          (buffer-string))))))

(defun elfeed-adapters-douban--extract-review-html (html)
  "Extract the full public review body from Douban HTML."
  (with-temp-buffer
    (insert html)
    (let* ((document (libxml-parse-html-region (point-min) (point-max)))
           (content (car (dom-by-class document "review-content"))))
      (when content
        (with-temp-buffer
          (dom-print content)
          (buffer-string))))))

(defun elfeed-adapters-douban--enrich-cards (wrappers callback)
  "Fetch full topic and review bodies in WRAPPERS, then call CALLBACK.

Failure to expand an individual card leaves its API subtitle intact."
  (let* ((jobs
          (delq nil
                (mapcar
                 (lambda (wrapper)
                   (when-let* ((status (plist-get wrapper :status)))
                     (cond
                      ((when-let* ((url
                                    (elfeed-adapters-douban--topic-url status))
                                   (headers
                                    (elfeed-adapters-douban--headers url)))
                         (list status url headers
                               #'elfeed-adapters-douban--extract-topic-html)))
                      ((when-let* ((url
                                    (elfeed-adapters-douban--review-url status)))
                         (list status url
                               '(("Referer" . "https://www.douban.com/"))
                               #'elfeed-adapters-douban--extract-review-html))))))
                 wrappers)))
         (pending (length jobs)))
    (if (zerop pending)
        (funcall callback wrappers)
      (dolist (job jobs)
        (pcase-let ((`(,status ,url ,headers ,extractor) job))
        (elfeed-adapters-request
         url
         (lambda (error body)
           (unless error
             (when-let* ((full-html
                          (funcall extractor body)))
               (let ((card (plist-get status :card)))
                 (setq card (plist-put card :full-html full-html))
                 (plist-put status :card card))))
           (when (zerop (cl-decf pending))
             (funcall callback wrappers)))
         headers))))))

(defun elfeed-adapters-douban--status-html (status)
  "Render a Douban STATUS plist as compact HTML."
  (let ((text (or (plist-get status :text) ""))
        (images (plist-get status :images))
        (card (plist-get status :card))
        (reshared (or (plist-get status :reshared_status)
                      (plist-get status :parent_status))))
    (concat
     (format "<p>%s</p>" (elfeed-adapters-douban--text-html text))
     (mapconcat #'elfeed-adapters-douban--image-html images "")
     (when card
       (let ((title (plist-get card :title))
             (url (plist-get card :url))
             (subtitle (plist-get card :subtitle))
             (full-html (plist-get card :full-html)))
         (concat
          "<blockquote>"
          (when (and title (not (string-empty-p title)))
            (if url
                (format "<p><a href=\"%s\"><strong>%s</strong></a></p>"
                        (xml-escape-string url) (xml-escape-string title))
              (format "<p><strong>%s</strong></p>"
                      (xml-escape-string title))))
          (if full-html
              full-html
            (when subtitle
              (format "<p>%s</p>"
                      (elfeed-adapters-douban--text-html subtitle))))
          (when-let* ((image (plist-get card :image)))
            (elfeed-adapters-douban--image-html image))
          "</blockquote>")))
     (when reshared
       (format "<blockquote>%s</blockquote>"
               (elfeed-adapters-douban--status-html reshared))))))

(defun elfeed-adapters-douban--status-title (status)
  "Return a useful title for Douban STATUS."
  (let* ((author (plist-get status :author))
         (name (or (plist-get author :name) "豆瓣用户"))
         (activity (or (plist-get status :activity) ""))
         (card (plist-get status :card))
         (text (string-trim (replace-regexp-in-string
                             "[\n\r]+" " "
                             (or (plist-get status :text) "")))))
    (string-trim
     (format "%s %s: %s%s"
             name activity
             (if-let* ((card-title (plist-get card :title))
                       ((not (string-empty-p card-title))))
                 (format "《%s》" card-title)
               "")
             text))))

(defun elfeed-adapters-douban--item (wrapper)
  "Convert a Douban timeline WRAPPER to a normalized adapter item."
  (let* ((status (plist-get wrapper :status))
         (id (format "%s" (plist-get status :id)))
         (author (plist-get status :author))
         (uri (plist-get status :uri))
         (link (or (plist-get status :sharing_url)
                   (and uri
                        (replace-regexp-in-string
                         "\\`douban://douban.com"
                         "https://www.douban.com/doubanapp/dispatch?uri=" uri))
                   (format "https://m.douban.com/status/%s/" id))))
    (list :guid (concat "douban-status-" id)
          :title (elfeed-adapters-douban--status-title status)
          :link (car (split-string link "?_i=" t))
          :date (elfeed-adapters-douban--date
                 (plist-get status :create_time))
          :authors (list (or (plist-get author :name) "豆瓣用户"))
          :content (elfeed-adapters-douban--status-html status)
          :content-type 'html)))

(defun elfeed-adapters-douban--fetch-timeline (parameters callback)
  "Fetch a Douban timeline described by PARAMETERS and call CALLBACK."
  (let* ((user-id (plist-get parameters :user-id))
         (filter (plist-get parameters :filter-title))
         (api-url
          (format "https://m.douban.com/rexxar/api/v2/status/user_timeline/%s?start=0&count=20"
                  user-id)))
    (elfeed-adapters-request
     api-url
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (let* ((payload (json-parse-string
                              body :object-type 'plist :array-type 'list
                              :null-object nil :false-object nil))
                    (wrappers (seq-remove
                               (lambda (item) (plist-get item :deleted))
                               (plist-get payload :items)))
                    (first-status (plist-get (car wrappers) :status))
                    (author (plist-get first-status :author))
                    (name (or (plist-get author :name) user-id)))
               (elfeed-adapters-douban--enrich-cards
                wrappers
                (lambda (enriched)
                  (let* ((items (mapcar #'elfeed-adapters-douban--item enriched))
                         (items
                          (if filter
                              (seq-remove
                               (lambda (item)
                                 (string-match-p filter
                                                 (plist-get item :title)))
                               items)
                            items)))
                    (funcall callback nil
                             (list :title (format "豆瓣广播 - %s" name)
                                   :namespace "douban.com"
                                   :authors (list name)
                                   :items items))))))
           (error (funcall callback parse-error nil)))))
     '(("Referer" . "https://m.douban.com/")))))

(defun elfeed-adapters-douban--fetch (_url parameters callback)
  "Fetch the Douban source described by PARAMETERS and call CALLBACK."
  (pcase (plist-get parameters :kind)
    ('replies (elfeed-adapters-douban--fetch-replies callback))
    (_ (elfeed-adapters-douban--fetch-timeline parameters callback))))

;;;###autoload
(defun elfeed-adapters-douban-register ()
  "Register the Douban adapter."
  (elfeed-adapters-register
   'douban #'elfeed-adapters-douban--match
   #'elfeed-adapters-douban--fetch))

(elfeed-adapters-douban-register)

(provide 'elfeed-adapters-douban)
;;; elfeed-adapters-douban.el ends here
