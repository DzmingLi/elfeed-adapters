;;; elfeed-adapters-theatlantic.el --- The Atlantic adapter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Native Elfeed source for The Atlantic author pages.  The official author
;; Atom feed supplies the article list; embedded Next.js data supplies the full
;; article body.

;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'json)
(require 'seq)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defconst elfeed-adapters-theatlantic--origin
  "https://www.theatlantic.com"
  "The Atlantic origin URL.")

(defcustom elfeed-adapters-theatlantic-max-items 25
  "Maximum number of articles fetched for one author update."
  :type 'natnum
  :group 'elfeed-adapters)

(defun elfeed-adapters-theatlantic--match (url)
  "Return author parameters when URL is a supported The Atlantic URL."
  (when (string-match
         (rx string-start
             "adapter+theatlantic://author/"
             (group (+ (or alnum "-" "_" ".")))
             (? "/") string-end)
         url)
    (list :slug (match-string 1 url))))

(defun elfeed-adapters-theatlantic--parse-xml (body)
  "Parse XML BODY and return its document element."
  (with-temp-buffer
    (insert body)
    (car (xml-parse-region (point-min) (point-max)))))

(defun elfeed-adapters-theatlantic--parse-html (body)
  "Parse HTML BODY and return its document element."
  (with-temp-buffer
    (insert body)
    (libxml-parse-html-region (point-min) (point-max))))

(defun elfeed-adapters-theatlantic--node-text (node)
  "Return trimmed text contained in NODE, or nil."
  (when node
    (string-trim
     (funcall (if (fboundp 'dom-inner-text) 'dom-inner-text 'dom-text)
              node))))

(defun elfeed-adapters-theatlantic--atom-link (entry)
  "Return the preferred article link from Atom ENTRY."
  (when-let* ((link
              (or (cl-find-if
                   (lambda (node)
                     (member (dom-attr node 'rel) '(nil "alternate")))
                   (dom-by-tag entry 'link))
                  (car (dom-by-tag entry 'link)))))
    (dom-attr link 'href)))

(defun elfeed-adapters-theatlantic--atom-authors (entry)
  "Return author names from Atom ENTRY."
  (delq nil
        (mapcar
         (lambda (author)
           (elfeed-adapters-theatlantic--node-text
            (car (dom-by-tag author 'name))))
         (dom-by-tag entry 'author))))

(defun elfeed-adapters-theatlantic--parse-feed (body)
  "Parse The Atlantic Atom BODY into a feed result plist."
  (let* ((feed (elfeed-adapters-theatlantic--parse-xml body))
         (title (elfeed-adapters-theatlantic--node-text
                 (car (dom-by-tag feed 'title))))
         (items
          (mapcar
           (lambda (entry)
             (let ((link (elfeed-adapters-theatlantic--atom-link entry)))
               (list
                :guid link
                :link link
                :title (elfeed-adapters-theatlantic--node-text
                        (car (dom-by-tag entry 'title)))
                :date (elfeed-adapters-theatlantic--node-text
                       (or (car (dom-by-tag entry 'published))
                           (car (dom-by-tag entry 'updated))))
                :authors (elfeed-adapters-theatlantic--atom-authors entry)
                :content (elfeed-adapters-theatlantic--node-text
                          (or (car (dom-by-tag entry 'content))
                              (car (dom-by-tag entry 'summary))))
                :content-type 'html)))
           (seq-take (dom-by-tag feed 'entry)
                     elfeed-adapters-theatlantic-max-items))))
    (list :title title :items items)))

(defun elfeed-adapters-theatlantic--json-get (object &rest keys)
  "Look up KEYS successively in nested JSON hash-table OBJECT."
  (cl-reduce (lambda (value key)
               (and (hash-table-p value) (gethash key value)))
             keys :initial-value object))

(defun elfeed-adapters-theatlantic--article-data (body)
  "Extract The Atlantic article data from HTML BODY."
  (let* ((document (elfeed-adapters-theatlantic--parse-html body))
         (script (dom-by-id document "__NEXT_DATA__")))
    (unless script
      (error "The Atlantic page has no __NEXT_DATA__ payload"))
    (let* ((next-data
            (json-parse-string
             (funcall
              (if (fboundp 'dom-inner-text) 'dom-inner-text 'dom-text)
              script)
             :object-type 'hash-table
             :array-type 'list
             :null-object nil
             :false-object nil))
           (state (elfeed-adapters-theatlantic--json-get
                   next-data "props" "pageProps" "urqlState"))
           article)
      (unless (hash-table-p state)
        (error "The Atlantic page has no urqlState"))
      (maphash
       (lambda (_key value)
         (when-let* ((payload (and (hash-table-p value)
                                  (gethash "data" value))))
           (when (and (stringp payload)
                      (string-match-p "\"content\"" payload))
             (let ((candidate
                    (elfeed-adapters-theatlantic--json-get
                     (json-parse-string
                      payload
                      :object-type 'hash-table
                      :array-type 'list
                      :null-object nil
                      :false-object nil)
                     "article")))
               (when candidate
                 (setq article candidate))))))
       state)
      (or article (error "The Atlantic article payload was not found")))))

(defun elfeed-adapters-theatlantic--escape (text)
  "Escape TEXT for inclusion in generated HTML."
  (xml-escape-string (or text "")))

(defun elfeed-adapters-theatlantic--content-html (article)
  "Render ARTICLE data as semantic HTML."
  (let* ((dek (gethash "dek" article))
         (lead-art (gethash "leadArt" article))
         (image (and (hash-table-p lead-art)
                     (gethash "image" lead-art)))
         (image-url (and (hash-table-p image) (gethash "url" image)))
         (image-alt (and (hash-table-p image) (gethash "altText" image)))
         (attribution (and (hash-table-p image)
                           (gethash "attributionText" image)))
         (blocks
          (delq
           nil
           (mapcar
            (lambda (block)
              (when-let* ((html (and (hash-table-p block)
                                    (gethash "innerHtml" block))))
                (unless (string-empty-p html)
                  (if (gethash "tagName" block)
                      html
                    (format "<p>%s</p>" html)))))
            (gethash "content" article)))))
    (concat
     "<div>"
     (when dek
       (format "<p>%s</p>" (elfeed-adapters-theatlantic--escape dek)))
     (when image-url
       (concat
        "<figure><img src=\""
        (elfeed-adapters-theatlantic--escape image-url)
        "\" alt=\""
        (elfeed-adapters-theatlantic--escape image-alt)
        "\">"
        (when attribution
          (format "<figcaption>%s</figcaption>"
                  (elfeed-adapters-theatlantic--escape attribution)))
        "</figure>"))
     (string-join blocks "")
     "</div>")))

(defun elfeed-adapters-theatlantic--enrich-item (item body)
  "Return ITEM enriched with full article data extracted from BODY."
  (let* ((article (elfeed-adapters-theatlantic--article-data body))
         (title (or (gethash "shareTitle" article)
                    (plist-get item :title)))
         (authors
          (delq nil
                (mapcar (lambda (author)
                          (and (hash-table-p author)
                               (or (gethash "displayName" author)
                                   (gethash "name" author))))
                        (gethash "authors" article))))
         (categories
          (delq nil
                (mapcar (lambda (category)
                          (and (hash-table-p category)
                               (gethash "slug" category)))
                        (gethash "categories" article)))))
    (plist-put item :title title)
    (plist-put item :authors authors)
    (plist-put item :categories categories)
    (plist-put item :content
               (elfeed-adapters-theatlantic--content-html article))
    (plist-put item :content-type 'html)
    item))

(defun elfeed-adapters-theatlantic--enrich-items (items callback)
  "Fetch full text for ITEMS sequentially, then call CALLBACK."
  (let ((pending (copy-sequence items))
        enriched)
    (cl-labels
        ((next
          ()
          (if-let* ((item (pop pending)))
              (if (elfeed-adapters-theatlantic--full-content-p
                   (plist-get item :content))
                  (progn
                    (push item enriched)
                    (next))
                (elfeed-adapters-request
                 (plist-get item :link)
                 (lambda (error body)
                   ;; A single unavailable article should not discard the feed.
                   (push (if error
                             item
                           (condition-case nil
                               (elfeed-adapters-theatlantic--enrich-item
                                item body)
                             (error item)))
                         enriched)
                   (next))))
            (funcall callback (nreverse enriched)))))
      (next))))

(defun elfeed-adapters-theatlantic--full-content-p (content)
  "Return non-nil when CONTENT looks like a complete Atom article body."
  (and (stringp content)
       (> (length content) 500)
       (string-match-p "<p\\(?:[ >]\\)" content)))

(defun elfeed-adapters-theatlantic--fetch (_url parameters callback)
  "Fetch an author feed using PARAMETERS, then call CALLBACK."
  (let* ((slug (plist-get parameters :slug))
         (feed-url (format "%s/feed/author/%s/"
                           elfeed-adapters-theatlantic--origin slug)))
    (elfeed-adapters-request
     feed-url
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (let ((feed (elfeed-adapters-theatlantic--parse-feed body)))
               (elfeed-adapters-theatlantic--enrich-items
                (plist-get feed :items)
                (lambda (items)
                  (plist-put feed :items items)
                  (plist-put feed :namespace "theatlantic.com")
                  (funcall callback nil feed))))
           (error (funcall callback parse-error nil))))))))

;;;###autoload
(defun elfeed-adapters-theatlantic-register ()
  "Register the The Atlantic author adapter."
  (elfeed-adapters-register
   'theatlantic
   #'elfeed-adapters-theatlantic--match
   #'elfeed-adapters-theatlantic--fetch))

(elfeed-adapters-theatlantic-register)

(provide 'elfeed-adapters-theatlantic)
;;; elfeed-adapters-theatlantic.el ends here
