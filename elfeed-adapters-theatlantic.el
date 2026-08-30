;;; elfeed-adapters-theatlantic.el --- Atlantic author feeds  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Consume The Atlantic's official full-content author Atom feeds directly.
;; Lead images come from media:content, while site-specific drop-cap sections
;; are converted to portable HTML during the same fetch.

;;; Code:

(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defun elfeed-adapters-theatlantic--match (url)
  "Return parameters when URL names an Atlantic author SLUG."
  (when (string-match
         (rx string-start "adapter:theatlantic/author/"
             (group (+ (or alnum "-")))
             (optional "?" (* nonl)) string-end)
         url)
    (list :slug (match-string 1 url))))

(defun elfeed-adapters-theatlantic--section-html (html)
  "Make Atlantic drop-cap sections in HTML visible to simple readers.

The first drop-cap paragraph is the article opening.  Prepend an HTML
horizontal rule to each later one, corresponding to Org's `-----' separator,
and render the site's small-caps span as portable strong emphasis."
  (let ((first t)
        (result (or html "")))
    (setq result
          (replace-regexp-in-string
           "<p class=\"dropcap\">"
           (lambda (opening)
             (if first
                 (progn (setq first nil) opening)
               (concat "<hr>" opening)))
           result t t))
    (replace-regexp-in-string
     "<span class=\"smallcaps\">\\([^<]*\\)</span>"
     "<strong>\\1</strong>"
     result t)))

(defun elfeed-adapters-theatlantic--text (node)
  "Return trimmed text contained by DOM NODE."
  (when node
    (string-trim (dom-inner-text node))))

(defun elfeed-adapters-theatlantic--child (node tag)
  "Return NODE's first direct child named TAG."
  (seq-find (lambda (child)
              (and (listp child) (eq (dom-tag child) tag)))
            (dom-children node)))

(defun elfeed-adapters-theatlantic--content (entry)
  "Return the HTML content string from Atom ENTRY."
  (when-let* ((node
               (seq-find
                (lambda (content)
                  (equal (dom-attr content 'type) "html"))
                (dom-by-tag entry 'content))))
    (dom-inner-text node)))

(defun elfeed-adapters-theatlantic--image-url (entry)
  "Return the media:content image URL from Atom ENTRY."
  (when-let* ((node
               (seq-find
                (lambda (content) (dom-attr content 'url))
                (dom-by-tag entry 'content))))
    (dom-attr node 'url)))

(defun elfeed-adapters-theatlantic--link (entry)
  "Return the alternate article URL from Atom ENTRY."
  (when-let* ((node
               (seq-find
                (lambda (link)
                  (equal (dom-attr link 'rel) "alternate"))
                (dom-by-tag entry 'link))))
    (dom-attr node 'href)))

(defun elfeed-adapters-theatlantic--image-html (title image-url)
  "Return lead-image markup using TITLE and IMAGE-URL."
  (when image-url
    (format
     "<figure class=\"elfeed-adapters-theatlantic-lead\"><img src=\"%s\" alt=\"%s\"></figure>"
     (xml-escape-string image-url)
     (xml-escape-string (or title "The Atlantic")))))

(defun elfeed-adapters-theatlantic--item (entry)
  "Convert an Atlantic Atom ENTRY to a normalized adapter item."
  (let* ((title
          (elfeed-adapters-theatlantic--text
           (elfeed-adapters-theatlantic--child entry 'title)))
         (author-node
          (elfeed-adapters-theatlantic--child entry 'author))
         (author
          (elfeed-adapters-theatlantic--text
           (and author-node
                (elfeed-adapters-theatlantic--child author-node 'name))))
         (content
          (elfeed-adapters-theatlantic--section-html
           (elfeed-adapters-theatlantic--content entry)))
         (image-url (elfeed-adapters-theatlantic--image-url entry)))
    (list
     :guid
     (elfeed-adapters-theatlantic--text
      (elfeed-adapters-theatlantic--child entry 'id))
     :title title
     :link (elfeed-adapters-theatlantic--link entry)
     :date
     (elfeed-adapters-theatlantic--text
      (or (elfeed-adapters-theatlantic--child entry 'published)
          (elfeed-adapters-theatlantic--child entry 'updated)))
     :authors (and author (list author))
     :content (concat
               (elfeed-adapters-theatlantic--image-html title image-url)
               content)
     :content-type 'html)))

(defun elfeed-adapters-theatlantic--result (xml slug)
  "Parse Atlantic author feed XML for SLUG into an adapter result."
  (with-temp-buffer
    (insert xml)
    (let* ((document
            (libxml-parse-xml-region (point-min) (point-max)))
           (title
            (elfeed-adapters-theatlantic--text
             (elfeed-adapters-theatlantic--child document 'title)))
           (items
            (mapcar #'elfeed-adapters-theatlantic--item
                    (dom-by-tag document 'entry))))
      (unless items
        (error "No Atlantic articles found for %s" slug))
      (list :title (or title (format "The Atlantic - %s" slug))
            ;; Match Elfeed's namespace for the former official feed URL so
            ;; switching to the adapter updates existing entries in place.
            :namespace "www.theatlantic.com"
            :items items))))

(defun elfeed-adapters-theatlantic--fetch (_url parameters callback)
  "Fetch an Atlantic author feed described by PARAMETERS and call CALLBACK."
  (let* ((slug (plist-get parameters :slug))
         (feed-url
          (format "https://www.theatlantic.com/feed/author/%s/" slug)))
    (elfeed-adapters-request
     feed-url
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (funcall callback nil
                      (elfeed-adapters-theatlantic--result body slug))
           (error (funcall callback parse-error nil))))))))

;;;###autoload
(defun elfeed-adapters-theatlantic-register ()
  "Register The Atlantic author adapter."
  (elfeed-adapters-register
   'theatlantic #'elfeed-adapters-theatlantic--match
   #'elfeed-adapters-theatlantic--fetch))

(elfeed-adapters-theatlantic-register)

(provide 'elfeed-adapters-theatlantic)
;;; elfeed-adapters-theatlantic.el ends here
