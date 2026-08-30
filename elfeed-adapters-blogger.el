;;; elfeed-adapters-blogger.el --- Full Blogger posts for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Use a Blogger Atom feed as the article index, then replace its summaries
;; with the public HTML from each post page.  This is useful for blogs whose
;; Blogger feed setting exposes only summaries.

;;; Code:

(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defun elfeed-adapters-blogger--match (url)
  "Return parameters when URL names a Blogger HOST."
  (when (string-match
         (rx string-start "adapter:blogger/"
             (group (+ (or alnum "." "-")))
             (optional "?" (* nonl)) string-end)
         url)
    (list :host (match-string 1 url))))

(defun elfeed-adapters-blogger--child (node tag)
  "Return NODE's first direct child named TAG."
  (seq-find (lambda (child)
              (and (listp child) (eq (dom-tag child) tag)))
            (dom-children node)))

(defun elfeed-adapters-blogger--text (node)
  "Return trimmed text contained by DOM NODE."
  (when node
    (string-trim (dom-inner-text node))))

(defun elfeed-adapters-blogger--link (entry)
  "Return the alternate article URL from Atom ENTRY."
  (when-let* ((node
               (seq-find
                (lambda (link)
                  (equal (dom-attr link 'rel) "alternate"))
                (dom-by-tag entry 'link))))
    (dom-attr node 'href)))

(defun elfeed-adapters-blogger--summary-html (entry)
  "Return portable fallback HTML for Atom ENTRY's summary."
  (when-let* ((summary (elfeed-adapters-blogger--child entry 'summary))
              (text (elfeed-adapters-blogger--text summary)))
    (format "<p>%s</p>" (xml-escape-string text))))

(defun elfeed-adapters-blogger--item (entry)
  "Convert a Blogger Atom ENTRY to a normalized adapter item."
  (let* ((author-node (elfeed-adapters-blogger--child entry 'author))
         (author
          (elfeed-adapters-blogger--text
           (and author-node
                (elfeed-adapters-blogger--child author-node 'name)))))
    (list
     :guid
     (elfeed-adapters-blogger--text
      (elfeed-adapters-blogger--child entry 'id))
     :title
     (elfeed-adapters-blogger--text
      (elfeed-adapters-blogger--child entry 'title))
     :link (elfeed-adapters-blogger--link entry)
     :date
     (elfeed-adapters-blogger--text
      (or (elfeed-adapters-blogger--child entry 'published)
          (elfeed-adapters-blogger--child entry 'updated)))
     :authors (and author (list author))
     :content (elfeed-adapters-blogger--summary-html entry)
     :content-type 'html)))

(defun elfeed-adapters-blogger--parse-feed (xml host)
  "Parse Blogger feed XML for HOST into an adapter result."
  (with-temp-buffer
    (insert xml)
    (let* ((document
            (libxml-parse-xml-region (point-min) (point-max)))
           (title
            (elfeed-adapters-blogger--text
             (elfeed-adapters-blogger--child document 'title)))
           (items
            (mapcar #'elfeed-adapters-blogger--item
                    (dom-by-tag document 'entry))))
      (unless items
        (error "No Blogger posts found for %s" host))
      (list :title (or title host)
            :namespace host
            :items items))))

(defun elfeed-adapters-blogger--children-html (node)
  "Serialize the children of DOM NODE as HTML."
  (mapconcat
   (lambda (child)
     (if (stringp child)
         (xml-escape-string child)
       (with-temp-buffer
         (dom-print child)
         (buffer-string))))
   (dom-children node) ""))

(defun elfeed-adapters-blogger--post-html (html)
  "Extract full post HTML from a Blogger article page HTML."
  (with-temp-buffer
    (insert html)
    (let* ((document
            (libxml-parse-html-region (point-min) (point-max)))
           (post (car (dom-by-class document "post-body"))))
      (when post
        (string-trim
         (elfeed-adapters-blogger--children-html post))))))

(defun elfeed-adapters-blogger--enrich-items (items callback)
  "Fetch full article bodies for ITEMS, then call CALLBACK.

If an individual article cannot be fetched or parsed, retain its Atom summary
so one unavailable page does not discard the complete feed update."
  (let ((remaining (length items)))
    (if (zerop remaining)
        (funcall callback items)
      (dolist (item items)
        (let ((current item))
          (elfeed-adapters-request
           (plist-get current :link)
           (lambda (error body)
             (unless error
               (when-let* ((full-html
                            (condition-case nil
                                (elfeed-adapters-blogger--post-html body)
                              (error nil)))
                           ((not (string-empty-p full-html))))
                 (setf (plist-get current :content) full-html)))
             (setq remaining (1- remaining))
             (when (zerop remaining)
               (funcall callback items)))))))))

(defun elfeed-adapters-blogger--fetch (_url parameters callback)
  "Fetch the Blogger source described by PARAMETERS and call CALLBACK."
  (let* ((host (plist-get parameters :host))
         (feed-url (format "https://%s/feeds/posts/default" host)))
    (elfeed-adapters-request
     feed-url
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (let ((result (elfeed-adapters-blogger--parse-feed body host)))
               (elfeed-adapters-blogger--enrich-items
                (plist-get result :items)
                (lambda (items)
                  (setf (plist-get result :items) items)
                  (funcall callback nil result))))
           (error (funcall callback parse-error nil))))))))

;;;###autoload
(defun elfeed-adapters-blogger-register ()
  "Register the full-content Blogger adapter."
  (elfeed-adapters-register
   'blogger #'elfeed-adapters-blogger--match
   #'elfeed-adapters-blogger--fetch))

(elfeed-adapters-blogger-register)

(provide 'elfeed-adapters-blogger)
;;; elfeed-adapters-blogger.el ends here
