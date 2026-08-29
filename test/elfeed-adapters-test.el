;;; elfeed-adapters-test.el --- Tests for Elfeed Adapters  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

(require 'ert)
(require 'elfeed-adapters)

(defconst elfeed-adapters-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defun elfeed-adapters-test--fixture (path)
  "Read fixture PATH relative to the test directory."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat "fixtures/" path)
                       elfeed-adapters-test--directory))
    (buffer-string)))

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

(ert-deftest elfeed-adapters-douban-fetches-public-timeline-without-cookies ()
  (require 'elfeed-adapters-douban)
  (let ((elfeed-adapters-request-function
         (lambda (url callback headers)
           (should (string-match-p "start=0&count=20" url))
           (should-not (assoc-string "Cookie" headers t))
           (funcall callback nil
                    (elfeed-adapters-test--fixture
                     "douban/timeline.json"))))
        result)
    (elfeed-adapters-douban--fetch
     nil '(:user-id "42")
     (lambda (error value) (should-not error) (setq result value)))
    (let ((item (car (plist-get result :items))))
      (should (equal (plist-get item :guid) "douban-status-101"))
      (should (string-match-p "第一行<br>第二行"
                              (plist-get item :content))))))

(ert-deftest elfeed-adapters-gcores-parses-rich-content ()
  (require 'elfeed-adapters-gcores)
  (let ((elfeed-adapters-request-function
         (lambda (_url callback _headers)
           (funcall callback nil
                    (elfeed-adapters-test--fixture "gcores/talks.json"))))
        result)
    (elfeed-adapters-gcores--fetch
     nil '(:user-id "7")
     (lambda (error value) (should-not error) (setq result value)))
    (let ((item (car (plist-get result :items))))
      (should (equal (plist-get item :title) "机核测试动态"))
      (should (string-match-p "完整正文" (plist-get item :content))))))

(ert-deftest elfeed-adapters-netease-music-parses-events ()
  (require 'elfeed-adapters-netease-music)
  (let ((elfeed-adapters-request-function
         (lambda (_url callback _headers)
           (funcall callback nil
                    (elfeed-adapters-test--fixture
                     "netease-music/events.json"))))
        result)
    (elfeed-adapters-netease-music--fetch
     nil '(:user-id "9")
     (lambda (error value) (should-not error) (setq result value)))
    (let ((item (car (plist-get result :items))))
      (should (equal (plist-get item :title) "分享单曲"))
      (should (string-match-p "云村正文" (plist-get item :content)))
      (should (string-match-p "测试歌曲" (plist-get item :content)))
      (should (string-match-p "outchain/player" (plist-get item :content)))
      (should (string-match-p "▶ 在网易云音乐播放"
                              (plist-get item :content))))))

(ert-deftest elfeed-adapters-zhihu-reads-browser-cookies-and-full-content ()
  (require 'elfeed-adapters-zhihu)
  (let ((requests nil)
        (elfeed-adapters-zhihu-profile-directory "/explicit/firefox/profile")
        result)
    (cl-letf (((symbol-function 'browser-cookies-get)
               (lambda (_url &rest arguments)
                 (should (equal (plist-get arguments :profile-directory)
                                "/explicit/firefox/profile"))
                 '(("d_c0" . "dc0-value") ("z_c0" . "login-value"))))
              ((symbol-function 'zhihu--zse-request-headers)
               (lambda (_url _body dc0)
                 (should (equal dc0 "dc0-value"))
                 '(("x-zse-93" . "test") ("x-zse-96" . "test"))))
              (elfeed-adapters-request-function
               (lambda (url callback headers)
                 (push (cons url headers) requests)
                 (should (string-match-p "d_c0=dc0-value"
                                         (cdr (assoc-string "Cookie" headers t))))
                 (funcall callback nil
                          (elfeed-adapters-test--fixture
                           (if (string-match-p "/articles?" url)
                               "zhihu/articles.json"
                             "zhihu/profile.json"))))))
      (elfeed-adapters-zhihu--fetch
       nil '(:kind posts :user-type "people" :user-id "tester")
       (lambda (error value) (should-not error) (setq result value))))
    (should (= (length requests) 2))
    (let ((item (car (plist-get result :items))))
      (should (equal (plist-get item :title) "知乎完整文章"))
      (should (string-match-p "完整正文" (plist-get item :content)))
      (should (string-match-p "src=\"https://img.example/zhihu.jpg\""
                              (plist-get item :content))))))

(provide 'elfeed-adapters-test)
;;; elfeed-adapters-test.el ends here
