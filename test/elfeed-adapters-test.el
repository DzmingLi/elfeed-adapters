;;; elfeed-adapters-test.el --- Tests for Elfeed Adapters  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

(require 'ert)
(require 'elfeed-adapters)

(defmacro elfeed-adapters-test--with-database (&rest body)
  "Run BODY with an isolated temporary Elfeed database."
  (declare (indent 0) (debug t))
  `(let* ((temporary-directory (make-temp-file "elfeed-adapters-test-" t))
          (elfeed-db-directory temporary-directory)
          (elfeed-db nil)
          (elfeed-db-feeds nil)
          (elfeed-db-entries nil)
          (elfeed-db-index nil)
          (elfeed-feeds nil))
     (unwind-protect
         (progn
           (elfeed-db-load)
           ,@body)
       (when elfeed-db
         (elfeed-db-unload))
       (delete-directory temporary-directory t))))

(ert-deftest elfeed-adapters-register-replaces-adapter ()
  (let ((elfeed-adapters--registry nil))
    (elfeed-adapters-register 'test (lambda (_url) '(one)) #'ignore)
    (elfeed-adapters-register 'test (lambda (_url) '(two)) #'ignore)
    (should (= (length elfeed-adapters--registry) 1))
    (should (equal (cdr (elfeed-adapters--find "native:test")) '(two)))))

(ert-deftest elfeed-adapters-infers-module-name-from-url ()
  (should
   (equal (elfeed-adapters--site-from-url
           "adapter+example-site://author/name")
          "example-site"))
  (should-not
   (elfeed-adapters--site-from-url "https://example.com/feed.atom")))

(ert-deftest elfeed-adapters-ignores-normal-feed-urls ()
  (let ((elfeed-adapters--registry nil)
        callback-called)
    (should-not
     (elfeed-adapters-fetch
      "https://example.com/feed.atom"
      (lambda (_status) (setq callback-called t))))
    (should-not callback-called)))

(ert-deftest elfeed-adapters-preserves-entry-tags-on-refresh ()
  (elfeed-adapters-test--with-database
    (let* ((feed-url "native:test")
           (item-url "https://example.com/article")
           (elfeed-feeds (list feed-url))
           (elfeed-adapters--registry nil)
           (id (cons "example.com" item-url)))
      (elfeed-adapters-register
       'test
       (lambda (url) (and (equal url feed-url) 'matched))
       (lambda (_url _parameters callback)
         (funcall callback nil
                  `(:title "Example"
                    :namespace "example.com"
                    :items ((:guid ,item-url
                             :link ,item-url
                             :title "Article"
                             :date "2026-08-30T00:00:00Z"
                             :content "<p>Full text.</p>"
                             :content-type html))))))
      (elfeed-adapters-fetch feed-url #'ignore)
      (elfeed-tag (elfeed-db-get-entry id) 'saved)
      (elfeed-adapters-fetch feed-url #'ignore)
      (should (elfeed-tagged-p 'saved (elfeed-db-get-entry id))))))

(provide 'elfeed-adapters-test)
;;; elfeed-adapters-test.el ends here
