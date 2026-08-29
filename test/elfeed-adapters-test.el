;;; elfeed-adapters-test.el --- Tests for Elfeed Adapters  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: AGPL-3.0-or-later

(require 'ert)
(require 'elfeed-adapters)

(defvar rmh-elfeed-org-ignore-tag)

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
           "adapter:example-site/author/name")
          "example-site"))
  (should-not
   (elfeed-adapters--site-from-url "https://example.com/feed.atom")))

(ert-deftest elfeed-adapters-registers-org-link-type ()
  (require 'org)
  (with-temp-buffer
    (org-mode)
    (insert "[[adapter:zhihu/answers/example]]")
    (goto-char 3)
    (let ((link (org-element-context)))
      (should (eq (org-element-type link) 'link))
      (should (equal (org-element-property :type link) "adapter"))
      (should (equal (org-element-property :raw-link link)
                     "adapter:zhihu/answers/example")))))

(ert-deftest elfeed-adapters-extends-elfeed-org-url-filter ()
  (let ((rmh-elfeed-org-ignore-tag "ignore"))
    (should
     (equal
      (elfeed-adapters--elfeed-org-filter-relevant
       (lambda (entries)
         (cl-remove-if-not
          (lambda (entry) (string-prefix-p "https:" (car entry)))
          entries))
       '(("https://example.com/feed")
         ("adapter:zhihu/posts/people/example" zhihu)
         ("adapter:douban/people/example/status" ignore)))
      '(("https://example.com/feed")
        ("adapter:zhihu/posts/people/example" zhihu))))))

(ert-deftest elfeed-adapters-ignores-normal-feed-urls ()
  (let ((elfeed-adapters--registry nil)
        callback-called)
    (should-not
     (elfeed-adapters-fetch
      "https://example.com/feed.atom"
      (lambda (_status) (setq callback-called t))))
    (should-not callback-called)))

(ert-deftest elfeed-adapters-reports-a-missing-site-module ()
  (let ((elfeed-adapters--registry nil)
        status)
    (should
     (elfeed-adapters-fetch
      "adapter:missing-test-site/source"
      (lambda (value) (setq status value))))
    (should (eq status :error))))

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

(ert-deftest elfeed-adapters-preserves-source-publication-times ()
  (let* ((feed-url "adapter:test/source")
         (timestamp 1700000000)
         (item '(:guid "dated-entry"
                 :link "https://example.com/dated"
                 :title "Dated entry"
                 :date 1700000000.75
                 :content "<p>Published earlier.</p>"))
         (entry (elfeed-adapters--entry feed-url "example.com" item)))
    (should (= (elfeed-entry-date entry) timestamp))
    (setf (elfeed-entry-date entry) (float-time))
    (cl-letf (((symbol-function 'elfeed-db-get-entry)
               (lambda (_id) entry)))
      (should (= (elfeed-entry-date
                  (elfeed-adapters--entry feed-url "example.com" item))
                 timestamp)))))

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
      (should
       (= (plist-get item :date)
          (truncate
           (float-time (date-to-time "2026-08-30 08:00:00 +0800")))))
      (should (string-match-p "第一行<br>第二行"
                              (plist-get item :content)))
      (should (string-match-p "卡片第一行<br>卡片第二行"
                              (plist-get item :content)))
      (should-not (string-match-p "<strong></strong>"
                                  (plist-get item :content)))
      (should-not (string-match-p "《》" (plist-get item :title))))))

(ert-deftest elfeed-adapters-douban-preserves-user-id-with-query-parameters ()
  (should
   (equal
    (elfeed-adapters-douban--match
     "adapter:douban/people/215524359/status?filterout_title=%E6%83%B3%E8%AF%BB%3A")
    '(:user-id "215524359" :filter-title "想读:"))))

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
      (should (string-match-p "测试音乐人" (plist-get item :content)))
      (should (string-match-p "测试专辑" (plist-get item :content)))
      (should-not (string-match-p "album.jpg" (plist-get item :content)))
      (should (string-match-p "outchain/player" (plist-get item :content)))
      (should (string-match-p "▶ 在网易云音乐播放"
                              (plist-get item :content))))))

(ert-deftest elfeed-adapters-telegram-parses-public-channel-messages ()
  (require 'elfeed-adapters-telegram)
  (let ((elfeed-adapters-request-function
         (lambda (url callback _headers)
           (should (equal url "https://t.me/s/TestChannel"))
           (funcall callback nil
                    (elfeed-adapters-test--fixture
                     "telegram/channel.html"))))
        result)
    (elfeed-adapters-telegram--fetch
     nil '(:channel "TestChannel")
     (lambda (error value) (should-not error) (setq result value)))
    (let ((item (car (plist-get result :items))))
      (should (equal (plist-get result :title) "Telegram - Test Channel"))
      (should (equal (plist-get item :guid) "telegram-TestChannel-42"))
      (should (equal (plist-get item :date) "2026-08-30T01:02:03+00:00"))
      (should (string-match-p "<b>Important update</b>"
                              (plist-get item :content)))
      (should (string-match-p "https://cdn.example/photo.jpg"
                              (plist-get item :content)))
      (should (string-match-p "Full message with"
                              (plist-get item :title))))))

(ert-deftest elfeed-adapters-theatlantic-extracts-and-renders-lead-image ()
  (require 'elfeed-adapters-theatlantic)
  (let* ((html (elfeed-adapters-test--fixture
                "theatlantic/article.html"))
         (image-url
          (elfeed-adapters-theatlantic--extract-image html))
         (entry
          (elfeed-entry--create
           :id '("theatlantic.com" . "article")
           :feed-id "https://www.theatlantic.com/feed/author/example/"
           :title "An Atlantic Article"
           :link "https://www.theatlantic.com/example/"
           :date 1700000000
           :content "<p>Full text.</p>"
           :content-type 'html
           :tags nil
           :meta nil)))
    (should (equal image-url "https://cdn.theatlantic.com/lead.jpg"))
    (should
     (equal
      (elfeed-adapters-theatlantic--image-html entry image-url)
      "<figure class=\"elfeed-adapters-theatlantic-lead\"><img src=\"https://cdn.theatlantic.com/lead.jpg\" alt=\"An Atlantic Article\"></figure>"))))

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
