;;; elfeed-adapters.el --- Native source adapters for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li

;; Author: Dzming Li <i@dzming.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (elfeed "4.0.0") (plz "0.10-pre") (browser-cookies "0.1.0") (zhihu "0.1.0"))
;; Keywords: news, comm, hypermedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;;; Commentary:

;; Elfeed Adapters lets Elfeed consume websites and APIs that do not expose a
;; useful web feed.  Adapters fetch a source asynchronously and return a small,
;; normalized data structure.  This library translates that structure into
;; native `elfeed-entry' objects and stores them with `elfeed-db-add'.

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'subr-x)

(declare-function org-link-set-parameters "ol" (type &rest parameters))
(declare-function rmh-elfeed-org-filter-relevant "elfeed-org" (list))

(defvar rmh-elfeed-org-ignore-tag)
(defvar elfeed-adapters-mode nil)

(defgroup elfeed-adapters nil
  "Native source adapters for Elfeed."
  :group 'elfeed
  :prefix "elfeed-adapters-")

(cl-defstruct (elfeed-adapters-adapter
               (:constructor elfeed-adapters--make-adapter))
  "An Elfeed source adapter.

MATCH receives a feed URL and returns adapter-specific parameters when the
adapter accepts it.  FETCH receives the URL, those parameters, and a callback.
It must eventually call the callback with (ERROR RESULT)."
  name match fetch)

(defvar elfeed-adapters--registry nil
  "Registered adapters, in matching order.")

(with-eval-after-load 'org
  ;; Teach Org that adapter: links are external links rather than fuzzy links.
  ;; This lets elfeed-org preserve them verbatim when it imports subscriptions.
  (org-link-set-parameters "adapter"))

(defun elfeed-adapters--elfeed-org-filter-relevant (original entries)
  "Let adapter subscriptions pass elfeed-org's URL filter.

Call ORIGINAL with ENTRIES first, then restore entries whose URL begins with
`adapter:' unless they carry elfeed-org's ignore tag.  elfeed-org currently
hard-codes a small set of URL schemes, so registering the Org link type alone
is not sufficient."
  (let ((result (funcall original entries))
        (ignore-name (and (boundp 'rmh-elfeed-org-ignore-tag)
                          (stringp rmh-elfeed-org-ignore-tag)
                          rmh-elfeed-org-ignore-tag)))
    (dolist (entry entries result)
      (when (and (string-prefix-p "adapter:" (car entry))
                 (not (and ignore-name
                           (cl-find ignore-name (cdr entry)
                                    :test (lambda (name tag)
                                            (and (symbolp tag)
                                                 (string= name
                                                          (symbol-name tag)))))))
                 (not (member entry result)))
        (setq result (append result (list entry)))))))

(defun elfeed-adapters--install-elfeed-org-support ()
  "Install compatibility for adapter subscriptions in elfeed-org."
  (when (and (fboundp 'rmh-elfeed-org-filter-relevant)
             (not (advice-member-p
                   #'elfeed-adapters--elfeed-org-filter-relevant
                   'rmh-elfeed-org-filter-relevant)))
    (advice-add 'rmh-elfeed-org-filter-relevant :around
                #'elfeed-adapters--elfeed-org-filter-relevant)))

(defun elfeed-adapters--remove-elfeed-org-support ()
  "Remove elfeed-org compatibility installed by Elfeed Adapters."
  (when (fboundp 'rmh-elfeed-org-filter-relevant)
    (advice-remove 'rmh-elfeed-org-filter-relevant
                   #'elfeed-adapters--elfeed-org-filter-relevant)))

(with-eval-after-load 'elfeed-org
  (when elfeed-adapters-mode
    (elfeed-adapters--install-elfeed-org-support)))

(defun elfeed-adapters--site-from-url (url)
  "Return the adapter site name encoded in URL, or nil."
  (when (string-match
         (rx string-start "adapter:"
             (group (+ (or lower digit "-"))) "/")
         url)
    (match-string 1 url)))

(defun elfeed-adapters--load-for-url (url)
  "Load the adapter module named by URL.

An URL beginning with adapter:SITE/ maps to the feature and library
`elfeed-adapters-SITE'.  Return non-nil when URL is not an adapter URL or the
corresponding feature was loaded successfully."
  (if-let* ((site (elfeed-adapters--site-from-url url)))
      (let* ((feature (intern (concat "elfeed-adapters-" site)))
             (loaded (or (featurep feature) (require feature nil t))))
        (when loaded
          (let ((register
                 (intern (concat (symbol-name feature) "-register"))))
            (when (fboundp register)
              (funcall register))))
        loaded)
    t))

(defun elfeed-adapters-register (name match fetch)
  "Register adapter NAME using MATCH and FETCH.

Replace an existing adapter with the same NAME.  MATCH is called with a feed
URL and returns non-nil adapter parameters on a match.  FETCH is called with
(URL PARAMETERS CALLBACK), and CALLBACK accepts (ERROR RESULT)."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  (unless (functionp match)
    (signal 'wrong-type-argument (list 'functionp match)))
  (unless (functionp fetch)
    (signal 'wrong-type-argument (list 'functionp fetch)))
  (setq elfeed-adapters--registry
        (cl-remove name elfeed-adapters--registry
                   :key #'elfeed-adapters-adapter-name))
  (push (elfeed-adapters--make-adapter
         :name name
         :match match
         :fetch fetch)
        elfeed-adapters--registry)
  name)

(defun elfeed-adapters-unregister (name)
  "Unregister adapter NAME."
  (setq elfeed-adapters--registry
        (cl-remove name elfeed-adapters--registry
                   :key #'elfeed-adapters-adapter-name)))

(defun elfeed-adapters--find (url)
  "Return (ADAPTER . PARAMETERS) for URL, or nil."
  (cl-loop for adapter in elfeed-adapters--registry
           for parameters = (funcall (elfeed-adapters-adapter-match adapter)
                                     url)
           when parameters
           return (cons adapter parameters)))

(defun elfeed-adapters--authors (authors)
  "Normalize AUTHORS to Elfeed author property lists."
  (mapcar (lambda (author)
            (if (stringp author)
                (list :name author)
              author))
          authors))

(defun elfeed-adapters--date (date)
  "Normalize DATE to a value accepted by `elfeed-float-time'.

Elfeed accepts integer Unix timestamps and date strings, but not floating-point
timestamps.  Preserve strings for Elfeed to parse and truncate numeric values
to whole seconds."
  (cond
   ((integerp date) date)
   ((floatp date) (truncate date))
   ((stringp date) date)
   (t nil)))

(defun elfeed-adapters--entry (feed-id namespace item)
  "Create an Elfeed entry for FEED-ID and NAMESPACE from ITEM."
  (let* ((guid (or (plist-get item :guid)
                   (plist-get item :link)
                   (elfeed-generate-id (plist-get item :content))))
         (id (cons namespace (elfeed-cleanup guid)))
         (original (elfeed-db-get-entry id))
         (date (elfeed-adapters--date (plist-get item :date)))
         (tags (elfeed-normalize-tags
                (elfeed-feed-autotags feed-id)
                elfeed-initial-tags))
         (authors (elfeed-adapters--authors (plist-get item :authors)))
         (categories (plist-get item :categories)))
    (elfeed-entry--create
     :id id
     :feed-id feed-id
     :title (elfeed-cleanup (or (plist-get item :title) ""))
     :link (elfeed-cleanup (plist-get item :link))
     :date (elfeed-new-date-for-entry
            (and original (elfeed-entry-date original)) date)
     :content (plist-get item :content)
     :content-type (or (plist-get item :content-type) 'html)
     :enclosures (plist-get item :enclosures)
     :tags tags
     :meta `(,@(when authors (list :authors authors))
             ,@(when categories (list :categories categories))))))

(defun elfeed-adapters--store (feed-id adapter result)
  "Store adapter RESULT for FEED-ID using ADAPTER."
  (let* ((feed (elfeed-db-get-feed feed-id))
         (namespace (or (plist-get result :namespace)
                        (symbol-name
                         (elfeed-adapters-adapter-name adapter))))
         (items (plist-get result :items)))
    (unless (listp items)
      (error "Adapter %s returned invalid :items"
             (elfeed-adapters-adapter-name adapter)))
    (setf (elfeed-feed-url feed) feed-id
          (elfeed-feed-title feed) (or (plist-get result :title) feed-id)
          (elfeed-feed-author feed)
          (elfeed-adapters--authors (plist-get result :authors)))
    (elfeed-db-add
     (mapcar (lambda (item)
               (elfeed-adapters--entry feed-id namespace item))
             items))))

(defun elfeed-adapters-fetch (url callback)
  "Fetch URL through a registered adapter.

This function follows the contract of `elfeed-fetch-functions'.  Return nil
when no adapter accepts URL, allowing Elfeed's normal fetcher to continue."
  (let ((site (elfeed-adapters--site-from-url url)))
    (cond
     ((and site (not (elfeed-adapters--load-for-url url)))
      (elfeed-log 'error "No adapter module found for %s" url)
      (funcall callback :error)
      t)
     ((when-let* ((match (elfeed-adapters--find url))
                  (adapter (car match))
                  (parameters (cdr match)))
        (let ((finished nil))
          (cl-labels
              ((finish
                (error result)
                (unless finished
                  (setq finished t)
                  (if error
                      (progn
                        (elfeed-log 'error "Adapter %s failed for %s: %s"
                                    (elfeed-adapters-adapter-name adapter)
                                    url error)
                        (funcall callback :error))
                    (condition-case store-error
                        (progn
                          (elfeed-adapters--store url adapter result)
                          (funcall callback :success))
                      (error
                       (elfeed-log
                        'error "Adapter %s returned invalid data for %s: %s"
                        (elfeed-adapters-adapter-name adapter)
                        url store-error)
                       (funcall callback :error)))))))
            (condition-case fetch-error
                (funcall (elfeed-adapters-adapter-fetch adapter)
                         url parameters #'finish)
              (error (finish fetch-error nil)))))
        t)))))

;;;###autoload
(define-minor-mode elfeed-adapters-mode
  "Use registered native adapters when Elfeed updates feeds."
  :global t
  :group 'elfeed-adapters
  (if elfeed-adapters-mode
      (progn
        (add-hook 'elfeed-fetch-functions #'elfeed-adapters-fetch)
        (elfeed-adapters--install-elfeed-org-support))
    (remove-hook 'elfeed-fetch-functions #'elfeed-adapters-fetch)
    (elfeed-adapters--remove-elfeed-org-support)))

(provide 'elfeed-adapters)
;;; elfeed-adapters.el ends here
