;;; elfeed-adapters-theatlantic.el --- Enrich Atlantic feeds  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Keep using The Atlantic's official full-content author feeds, but enrich
;; their entries with the lead image advertised by the article page.

;;; Code:

(require 'dom)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)
(require 'xml)

(declare-function elfeed-show-refresh "elfeed-show")
(defvar elfeed-show-entry)

(defvar elfeed-adapters-theatlantic--pending
  (make-hash-table :test #'equal)
  "Entry IDs currently being enriched.")

(defun elfeed-adapters-theatlantic--entry-p (entry)
  "Return non-nil when ENTRY belongs to an official Atlantic author feed."
  (string-prefix-p "https://www.theatlantic.com/feed/author/"
                   (elfeed-entry-feed-id entry)))

(defun elfeed-adapters-theatlantic--extract-image (html)
  "Return the Open Graph image URL found in HTML, or nil."
  (when (and (stringp html) (libxml-available-p))
    (with-temp-buffer
      (insert html)
      (let ((document (libxml-parse-html-region (point-min) (point-max))))
        (cl-loop for element in (dom-by-tag document 'meta)
                 when (equal (dom-attr element 'property) "og:image")
                 return (dom-attr element 'content))))))

(defun elfeed-adapters-theatlantic--image-html (entry image-url)
  "Return lead-image markup for ENTRY using IMAGE-URL."
  (format
   "<figure class=\"elfeed-adapters-theatlantic-lead\"><img src=\"%s\" alt=\"%s\"></figure>"
   (xml-escape-string image-url)
   (xml-escape-string (or (elfeed-entry-title entry) "The Atlantic"))))

(defun elfeed-adapters-theatlantic--refresh-visible-entry (entry)
  "Refresh Elfeed buffers after enriching ENTRY."
  (when-let* ((buffer (get-buffer "*elfeed-search*")))
    (with-current-buffer buffer
      (revert-buffer nil t)))
  (when-let* ((buffer (get-buffer "*elfeed-entry*")))
    (with-current-buffer buffer
      (when (and (boundp 'elfeed-show-entry)
                 elfeed-show-entry
                 (equal (elfeed-entry-id elfeed-show-entry)
                        (elfeed-entry-id entry)))
        (setq elfeed-show-entry entry)
        (elfeed-show-refresh)))))

(defun elfeed-adapters-theatlantic--enrich (entry)
  "Asynchronously add The Atlantic lead image to ENTRY."
  (let ((id (elfeed-entry-id entry)))
    (when (and (elfeed-adapters-theatlantic--entry-p entry)
               (eq (elfeed-entry-content-type entry) 'html)
               (not (elfeed-meta entry :elfeed-adapters-theatlantic-image))
               (not (gethash id elfeed-adapters-theatlantic--pending)))
      (puthash id t elfeed-adapters-theatlantic--pending)
      (elfeed-adapters-request
       (elfeed-entry-link entry)
       (lambda (error html)
         (unwind-protect
             (if error
                 (elfeed-log 'error "Atlantic image lookup failed for %s: %s"
                             (elfeed-entry-link entry) error)
               (if-let* ((image-url
                          (elfeed-adapters-theatlantic--extract-image html)))
                   (let ((content (or (elfeed-deref
                                       (elfeed-entry-content entry))
                                      "")))
                     (unless (string-match-p (regexp-quote image-url) content)
                       (setf (elfeed-entry-content entry)
                             (elfeed-ref
                              (concat
                               (elfeed-adapters-theatlantic--image-html
                                entry image-url)
                               content))))
                     (setf (elfeed-meta
                            entry :elfeed-adapters-theatlantic-image)
                           image-url)
                     (elfeed-db-set-update-time)
                     (elfeed-db-save)
                     (elfeed-adapters-theatlantic--refresh-visible-entry entry))
                 (setf (elfeed-meta
                        entry :elfeed-adapters-theatlantic-image)
                       'missing)
                 (elfeed-db-set-update-time)
                 (elfeed-db-save)))
           (remhash id elfeed-adapters-theatlantic--pending)))))))

;;;###autoload
(defun elfeed-adapters-theatlantic-backfill ()
  "Enrich existing official Atlantic entries that have not been processed."
  (interactive)
  (let ((count 0))
    (with-elfeed-db-visit (entry _feed)
      (when (and (elfeed-adapters-theatlantic--entry-p entry)
                 (not (elfeed-meta
                       entry :elfeed-adapters-theatlantic-image)))
        (setq count (1+ count))
        (elfeed-adapters-theatlantic--enrich entry)))
    (when (called-interactively-p 'interactive)
      (message "Started Atlantic image enrichment for %d entries" count))
    count))

;;;###autoload
(define-minor-mode elfeed-adapters-theatlantic-mode
  "Enrich new entries from official Atlantic author feeds with lead images."
  :global t
  :group 'elfeed-adapters
  (if elfeed-adapters-theatlantic-mode
      (add-hook 'elfeed-new-entry-hook #'elfeed-adapters-theatlantic--enrich)
    (remove-hook 'elfeed-new-entry-hook #'elfeed-adapters-theatlantic--enrich)))

(provide 'elfeed-adapters-theatlantic)
;;; elfeed-adapters-theatlantic.el ends here
