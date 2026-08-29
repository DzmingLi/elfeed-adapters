;;; elfeed-adapters-test.el --- Tests for Elfeed Adapters  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

(require 'ert)
(require 'elfeed-adapters)
(require 'elfeed-adapters-theatlantic)

(defconst elfeed-adapters-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun elfeed-adapters-test--fixture (relative-path)
  "Return fixture contents at RELATIVE-PATH."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative-path elfeed-adapters-test--directory))
    (buffer-string)))

(defun elfeed-adapters-test--request (url callback &optional _headers)
  "Serve an offline fixture for URL, then invoke CALLBACK."
  (cond
   ((string-match-p "/feed/author/test-author/\\'" url)
    (funcall callback nil
             (elfeed-adapters-test--fixture
              "fixtures/theatlantic/author.atom")))
   ((string= url "https://www.theatlantic.com/test/article/")
    (funcall callback nil
             (elfeed-adapters-test--fixture
              "fixtures/theatlantic/article.html")))
   (t (funcall callback (format "Unexpected test URL: %s" url) nil))))

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
    (should (equal (cdr (elfeed-adapters--find "adapter+test://source"))
                   '(two)))))

(ert-deftest elfeed-adapters-theatlantic-matches-author-url ()
  (should
   (equal
    (elfeed-adapters-theatlantic--match
     "adapter+theatlantic://author/rose-horowitch")
    '(:slug "rose-horowitch")))
  (should-not
   (elfeed-adapters-theatlantic--match
    "https://www.theatlantic.com/author/rose-horowitch/")))

(ert-deftest elfeed-adapters-loads-site-module-from-url ()
  (let ((elfeed-adapters--registry nil))
    (should
     (elfeed-adapters--load-for-url
      "adapter+theatlantic://author/rose-horowitch"))
    (should
     (elfeed-adapters--find
      "adapter+theatlantic://author/rose-horowitch"))))

(ert-deftest elfeed-adapters-theatlantic-keeps-full-atom-content ()
  (let ((content (concat "<p>" (make-string 600 ?x) "</p>"))
        request-called)
    (let ((elfeed-adapters-request-function
           (lambda (&rest _arguments) (setq request-called t)))
          result)
      (elfeed-adapters-theatlantic--enrich-items
       (list (list :link "https://example.invalid/" :content content))
       (lambda (items) (setq result items)))
      (should-not request-called)
      (should (equal (plist-get (car result) :content) content)))))

(ert-deftest elfeed-adapters-theatlantic-fetches-full-text ()
  (elfeed-adapters-test--with-database
    (let* ((feed-url "adapter+theatlantic://author/test-author")
           (elfeed-feeds (list feed-url))
           (elfeed-adapters-request-function
            #'elfeed-adapters-test--request)
           status)
      (elfeed-adapters-fetch feed-url (lambda (result) (setq status result)))
      (should (eq status :success))
      (let* ((id '("theatlantic.com"
                   . "https://www.theatlantic.com/test/article/"))
             (entry (elfeed-db-get-entry id))
             (content (elfeed-deref (elfeed-entry-content entry))))
        (should entry)
        (should (equal (elfeed-entry-title entry)
                       "Enriched article title"))
        (should (eq (elfeed-entry-content-type entry) 'html))
        (should (string-match-p
                 (regexp-quote
                  "<figcaption>Illustration by Test Artist</figcaption>")
                 content))
        (should (string-match-p
                 (regexp-quote "<p>First <strong>paragraph</strong>.</p>")
                 content))
        (should (string-match-p
                 (regexp-quote "</figure><p>First") content))
        (should (equal (plist-get (elfeed-entry-meta entry) :authors)
                       '((:name "Test Author"))))))))

(ert-deftest elfeed-adapters-preserves-entry-tags-on-refresh ()
  (elfeed-adapters-test--with-database
    (let* ((feed-url "adapter+theatlantic://author/test-author")
           (elfeed-feeds (list feed-url))
           (elfeed-adapters-request-function
            #'elfeed-adapters-test--request)
           (id '("theatlantic.com"
                 . "https://www.theatlantic.com/test/article/")))
      (elfeed-adapters-fetch feed-url #'ignore)
      (elfeed-tag (elfeed-db-get-entry id) 'saved)
      (elfeed-adapters-fetch feed-url #'ignore)
      (should (elfeed-tagged-p 'saved (elfeed-db-get-entry id))))))

(provide 'elfeed-adapters-test)
;;; elfeed-adapters-test.el ends here
