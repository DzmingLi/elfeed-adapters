;;; elfeed-adapters-douban.el --- Douban timelines for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Consume the public Douban mobile timeline API directly.  When a broadcast
;; links to a personal topic or public review, replace the API's truncated
;; card subtitle with the full web article.

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
  (when (string-match
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
      (list :user-id user-id
            :filter-title
            (and filter
                 (decode-coding-string (url-unhex-string filter) 'utf-8))))))

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

(defun elfeed-adapters-douban--fetch (_url parameters callback)
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

;;;###autoload
(defun elfeed-adapters-douban-register ()
  "Register the Douban adapter."
  (elfeed-adapters-register
   'douban #'elfeed-adapters-douban--match
   #'elfeed-adapters-douban--fetch))

(elfeed-adapters-douban-register)

(provide 'elfeed-adapters-douban)
;;; elfeed-adapters-douban.el ends here
