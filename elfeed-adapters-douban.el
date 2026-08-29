;;; elfeed-adapters-douban.el --- Douban timelines for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Consume the public Douban mobile timeline API directly.  This adapter does
;; not read browser cookies.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

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

(defun elfeed-adapters-douban--status-html (status)
  "Render a Douban STATUS plist as compact HTML."
  (let ((text (or (plist-get status :text) ""))
        (images (plist-get status :images))
        (card (plist-get status :card))
        (reshared (or (plist-get status :reshared_status)
                      (plist-get status :parent_status))))
    (concat
     (format "<p>%s</p>"
             (replace-regexp-in-string
              "\n" "<br>" (xml-escape-string text) t t))
     (mapconcat #'elfeed-adapters-douban--image-html images "")
     (when card
       (let ((title (plist-get card :title))
             (url (plist-get card :url))
             (subtitle (plist-get card :subtitle)))
         (concat
          "<blockquote>"
          (when title
            (if url
                (format "<p><a href=\"%s\"><strong>%s</strong></a></p>"
                        (xml-escape-string url) (xml-escape-string title))
              (format "<p><strong>%s</strong></p>"
                      (xml-escape-string title))))
          (when subtitle
            (format "<p>%s</p>" (xml-escape-string subtitle)))
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
             (if-let* ((card-title (plist-get card :title)))
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
          :date (plist-get status :create_time)
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
                    (items (mapcar #'elfeed-adapters-douban--item wrappers))
                    (items (if filter
                               (seq-remove
                                (lambda (item)
                                  (string-match-p filter
                                                  (plist-get item :title)))
                                items)
                             items))
                    (first-status (plist-get (car wrappers) :status))
                    (author (plist-get first-status :author))
                    (name (or (plist-get author :name) user-id)))
               (funcall callback nil
                        (list :title (format "豆瓣广播 - %s" name)
                              :namespace "douban.com"
                              :authors (list name)
                              :items items)))
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
