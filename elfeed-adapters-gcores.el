;;; elfeed-adapters-gcores.el --- GCORES user talks for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defconst elfeed-adapters-gcores--base-url "https://www.gcores.com/")
(defconst elfeed-adapters-gcores--image-base-url "https://image.gcores.com/")

(defun elfeed-adapters-gcores--match (url)
  "Return parameters when URL names a GCORES user talk source."
  (when (string-match
         (rx string-start "adapter:gcores/users/"
             (group (+ digit)) "/talks"
             (optional "?" (* nonl)) string-end)
         url)
    (list :user-id (match-string 1 url))))

(defun elfeed-adapters-gcores--absolute-image (path)
  "Return absolute GCORES image URL for PATH."
  (when path
    (if (string-match-p (rx string-start "http" (optional "s") "://") path)
        path
      (concat elfeed-adapters-gcores--image-base-url
              (string-remove-prefix "/" path)))))

(defun elfeed-adapters-gcores--content-html (node)
  "Render a GCORES rich-content NODE recursively as HTML."
  (cond
   ((stringp node) (xml-escape-string node))
   ((vectorp node)
    (mapconcat #'elfeed-adapters-gcores--content-html (append node nil) ""))
   ((and (listp node) (not (keywordp (car node))))
    (mapconcat #'elfeed-adapters-gcores--content-html node ""))
   ((listp node)
    (let* ((type (plist-get node :type))
           (text (plist-get node :text))
           (children (or (plist-get node :children)
                         (plist-get node :content)))
           (source (or (plist-get node :src)
                       (plist-get node :url)
                       (plist-get (plist-get node :attributes) :src)))
           (inner (cond
                   (text (xml-escape-string text))
                   (children
                    (mapconcat #'elfeed-adapters-gcores--content-html
                               children ""))
                   (t ""))))
      (pcase type
        ((or "paragraph" "p") (format "<p>%s</p>" inner))
        ((or "heading-one" "h1") (format "<h1>%s</h1>" inner))
        ((or "heading-two" "h2") (format "<h2>%s</h2>" inner))
        ((or "heading-three" "h3") (format "<h3>%s</h3>" inner))
        ((or "blockquote" "quote") (format "<blockquote>%s</blockquote>" inner))
        ((or "bulleted-list" "ul") (format "<ul>%s</ul>" inner))
        ((or "numbered-list" "ol") (format "<ol>%s</ol>" inner))
        ((or "list-item" "li") (format "<li>%s</li>" inner))
        ((or "image" "img")
         (if-let* ((image (elfeed-adapters-gcores--absolute-image source)))
             (format "<p><img src=\"%s\"></p>" (xml-escape-string image))
           inner))
        (_ inner))))
   (t "")))

(defun elfeed-adapters-gcores--parse-content (content)
  "Parse serialized GCORES CONTENT and return HTML."
  (if (not (and (stringp content) (not (string-empty-p content))))
      ""
    (condition-case nil
        (elfeed-adapters-gcores--content-html
         (json-parse-string content :object-type 'plist :array-type 'list
                            :null-object nil :false-object nil))
      (error (format "<p>%s</p>" (xml-escape-string content))))))

(defun elfeed-adapters-gcores--included-author (item included)
  "Find ITEM's author object in INCLUDED resources."
  (let* ((relationship (plist-get
                        (plist-get (plist-get item :relationships) :user)
                        :data))
         (id (plist-get relationship :id))
         (type (plist-get relationship :type)))
    (seq-find
     (lambda (candidate)
       (and (equal id (plist-get candidate :id))
            (equal type (plist-get candidate :type))))
     included)))

(defun elfeed-adapters-gcores--item (item included)
  "Convert GCORES ITEM using INCLUDED resources."
  (let* ((id (format "%s" (plist-get item :id)))
         (type (plist-get item :type))
         (attributes (plist-get item :attributes))
         (title (or (plist-get attributes :title) "机核动态"))
         (cover (or (plist-get attributes :cover)
                    (plist-get attributes :thumb)))
         (author-object (elfeed-adapters-gcores--included-author item included))
         (author-attributes (plist-get author-object :attributes))
         (author (or (plist-get author-attributes :nickname)
                     (plist-get author-object :nickname)
                     "机核用户"))
         (intro (or (plist-get attributes :desc)
                    (plist-get attributes :excerpt)))
         (body (elfeed-adapters-gcores--parse-content
                (plist-get attributes :content))))
    (list
     :guid (format "gcores-%s-%s" type id)
     :title title
     :link (format "%s%s/%s" elfeed-adapters-gcores--base-url type id)
     :date (or (plist-get attributes :created-at)
               (plist-get attributes :published-at))
     :authors (list author)
     :content
     (concat
      (when cover
        (format "<p><img src=\"%s\" alt=\"%s\"></p>"
                (xml-escape-string
                 (elfeed-adapters-gcores--absolute-image cover))
                (xml-escape-string title)))
      (when intro (format "<p>%s</p>" (xml-escape-string intro)))
      body)
     :content-type 'html)))

(defun elfeed-adapters-gcores--fetch (_url parameters callback)
  "Fetch GCORES talks described by PARAMETERS and call CALLBACK."
  (let* ((user-id (plist-get parameters :user-id))
         (api-url
          (concat elfeed-adapters-gcores--base-url
                  "gapi/v1/users/" user-id "/talks?"
                  (url-build-query-string
                   '(("page[limit]" "60")
                     ("sort" "-created-at")
                     ("include" "user"))))))
    (elfeed-adapters-request
     api-url
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (let* ((payload (json-parse-string
                              body :object-type 'plist :array-type 'list
                              :null-object nil :false-object nil))
                    (included (plist-get payload :included))
                    (data (seq-filter
                           (lambda (item)
                             (not (member (plist-get item :type)
                                          '("radios" "videos"))))
                           (plist-get payload :data)))
                    (items (mapcar
                            (lambda (item)
                              (elfeed-adapters-gcores--item item included))
                            (seq-take data 30)))
                    (author (car (plist-get (car items) :authors))))
               (funcall callback nil
                        (list :title (format "%s的机核动态"
                                             (or author user-id))
                              :namespace "gcores.com"
                              :authors (and author (list author))
                              :items items)))
           (error (funcall callback parse-error nil))))))))

;;;###autoload
(defun elfeed-adapters-gcores-register ()
  "Register the GCORES adapter."
  (elfeed-adapters-register
   'gcores #'elfeed-adapters-gcores--match
   #'elfeed-adapters-gcores--fetch))

(elfeed-adapters-gcores-register)

(provide 'elfeed-adapters-gcores)
;;; elfeed-adapters-gcores.el ends here
