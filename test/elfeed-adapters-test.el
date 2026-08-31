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
      (let ((entry (elfeed-db-get-entry id)))
        (should (elfeed-tagged-p 'saved entry))
        (should (equal (elfeed-meta entry :base-url) item-url))))))

(ert-deftest elfeed-adapters-prefers-an-explicit-entry-base-url ()
  (let* ((feed-url "adapter:test/source")
         (item '(:guid "entry-with-base"
                 :link "https://example.com/article"
                 :base-url "https://static.example.net/assets/"
                 :title "Entry with an explicit base"
                 :date 1700000000
                 :content "<img src=\"cover.jpg\">"))
         (entry (elfeed-adapters--entry feed-url "example.com" item)))
    (should (equal (elfeed-meta entry :base-url)
                   "https://static.example.net/assets/"))))

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

(ert-deftest elfeed-adapters-blogger-fetches-full-public-posts ()
  (require 'elfeed-adapters-blogger)
  (let* ((requests nil)
         (elfeed-adapters-request-function
         (lambda (url callback headers)
           (push url requests)
           (should-not (assoc-string "Cookie" headers t))
           (funcall
            callback nil
            (elfeed-adapters-test--fixture
             (if (string-suffix-p "/feeds/posts/default" url)
                 "blogger/feed.xml"
               "blogger/article.html")))))
        result)
    (elfeed-adapters-blogger--fetch
     nil '(:host "dostoe.blogspot.com")
     (lambda (error value) (should-not error) (setq result value)))
    (should (= (length requests) 2))
    (should (equal (plist-get result :title) "Dostoe"))
    (should (equal (plist-get result :namespace) "dostoe.blogspot.com"))
    (let ((item (car (plist-get result :items))))
      (should (equal (plist-get item :title) "定海水操"))
      (should (equal (plist-get item :authors) '("Dostoe")))
      (should (string-match-p "这是完整正文" (plist-get item :content)))
      (should (string-match-p "水操尤奇在夜战" (plist-get item :content)))
      (should (string-match-p "blogger.jpg" (plist-get item :content)))
      (should-not (string-match-p "被截断的摘要"
                                  (plist-get item :content))))))

(ert-deftest elfeed-adapters-blogger-retains-summary-on-page-failure ()
  (require 'elfeed-adapters-blogger)
  (let ((elfeed-adapters-request-function
         (lambda (url callback _headers)
           (if (string-suffix-p "/feeds/posts/default" url)
               (funcall callback nil
                        (elfeed-adapters-test--fixture "blogger/feed.xml"))
             (funcall callback "HTTP 503" nil))))
        result)
    (elfeed-adapters-blogger--fetch
     nil '(:host "dostoe.blogspot.com")
     (lambda (error value) (should-not error) (setq result value)))
    (should (string-match-p
             "被截断的摘要"
             (plist-get (car (plist-get result :items)) :content)))))

(ert-deftest elfeed-adapters-douban-preserves-user-id-with-query-parameters ()
  (should
   (equal
    (elfeed-adapters-douban--match
     "adapter:douban/people/215524359/status?filterout_title=%E6%83%B3%E8%AF%BB%3A")
    '(:kind timeline :user-id "215524359" :filter-title "想读:"))))

(ert-deftest elfeed-adapters-douban-matches-reply-notifications-only ()
  (should
   (equal
    (elfeed-adapters-douban--match
     "adapter:douban/notifications/replies")
    '(:kind replies)))
  (should-not
   (elfeed-adapters-douban--match
    "adapter:douban/notifications/likes")))

(ert-deftest elfeed-adapters-douban-fetches-specific-direct-replies-only ()
  (require 'elfeed-adapters-douban)
  (let ((elfeed-adapters-douban-profile-directory "/firefox/profile")
        requests result)
    (cl-letf (((symbol-function 'browser-cookies-get)
               (lambda (url &rest arguments)
                 (should (equal url "https://www.douban.com/reply_notify/"))
                 (should (equal (plist-get arguments :profile-directory)
                                "/firefox/profile"))
                 '(("dbcl2" . "\"42:session\"") ("ck" . "csrf"))))
              (elfeed-adapters-request-function
               (lambda (url callback headers)
                 (push (cons url headers) requests)
                 (should (assoc-string "Cookie" headers t))
                 (funcall
                  callback nil
                  (elfeed-adapters-test--fixture
                   (cond
                    ((string-suffix-p "/reply_notify/" url)
                     "douban/notifications.html")
                    ((string-match-p "/notification/reply_notify" url)
                     "douban/notification-status.html")
                    ((string-match-p "/status/12345/comments" url)
                     "douban/comments.json")
                    (t (ert-fail (format "Unexpected request: %s" url)))))))))
      (elfeed-adapters-douban--fetch
       nil '(:kind replies)
       (lambda (error value) (should-not error) (setq result value))))
    (should (= (length requests) 3))
    (let ((items (plist-get result :items)))
      (should (= (length items) 1))
      (let ((item (car items)))
        (should (equal (plist-get item :guid)
                       "douban-reply-comment-502"))
        (should (equal (plist-get item :authors) '("广播作者")))
        (should (string-match-p "这是作者具体回复的第一行<br>以及第二行"
                                (plist-get item :content)))
        (should (string-match-p "status/12345#comment_502"
                                (plist-get item :content)))
        (should-not (string-match-p "普通跟帖"
                                    (plist-get item :content)))
        (should-not (string-match-p "赞了你的广播"
                                    (plist-get item :content)))))))

(ert-deftest elfeed-adapters-douban-expands-personal-topic-with-browser-cookies ()
  (require 'elfeed-adapters-douban)
  (let ((elfeed-adapters-douban-profile-directory "/explicit/firefox/profile")
        (wrapper
         '(:status
           (:id "101" :text "" :card
            (:type "topic"
             :url "https://www.douban.com/topic/498511065/"
             :subtitle "截断摘要…"))))
        result)
    (cl-letf (((symbol-function 'browser-cookies-header)
               (lambda (url &rest arguments)
                 (should (equal url "https://www.douban.com/topic/498511065/"))
                 (should (equal (plist-get arguments :profile-directory)
                                "/explicit/firefox/profile"))
                 "dbcl2=test"))
              (elfeed-adapters-request-function
               (lambda (_url callback headers)
                 (should (equal (cdr (assoc-string "Cookie" headers t))
                                "dbcl2=test"))
                 (funcall callback nil
                          (elfeed-adapters-test--fixture
                           "douban/personal-topic.html")))))
      (elfeed-adapters-douban--enrich-cards
       (list wrapper) (lambda (value) (setq result value))))
    (let ((html (elfeed-adapters-douban--status-html
                 (plist-get (car result) :status))))
      (should (string-match-p "完整正文" html))
      (should (string-match-p "与自己，与外界" html))
      (should-not (string-match-p "截断摘要" html)))))

(ert-deftest elfeed-adapters-douban-expands-public-review-without-cookies ()
  (require 'elfeed-adapters-douban)
  (let ((wrapper
         '(:status
           (:id "102" :text "" :card
            (:type "review"
             :url "https://book.douban.com/review/17756318/"
             :subtitle "截断书评…"))))
        result)
    (cl-letf (((symbol-function 'browser-cookies-header)
               (lambda (&rest _arguments)
                 (ert-fail "Public reviews must not require browser cookies")))
              (elfeed-adapters-request-function
               (lambda (url callback headers)
                 (should (equal url
                                "https://book.douban.com/review/17756318/"))
                 (should-not (assoc-string "Cookie" headers t))
                 (funcall callback nil
                          (elfeed-adapters-test--fixture
                           "douban/review.html")))))
      (elfeed-adapters-douban--enrich-cards
       (list wrapper) (lambda (value) (setq result value))))
    (let ((html (elfeed-adapters-douban--status-html
                 (plist-get (car result) :status))))
      (should (string-match-p "书评的完整第一段" html))
      (should (string-match-p "书评的完整末段" html))
      (should-not (string-match-p "截断书评" html)))))

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

(ert-deftest elfeed-adapters-theatlantic-fetches-and-converts-official-feed ()
  (require 'elfeed-adapters-theatlantic)
  (let ((elfeed-adapters-request-function
         (lambda (url callback _headers)
           (should
            (equal url
                   "https://www.theatlantic.com/feed/author/rose-horowitch/"))
           (funcall callback nil
                    (elfeed-adapters-test--fixture
                     "theatlantic/author.xml"))))
        result)
    (elfeed-adapters-theatlantic--fetch
     nil '(:slug "rose-horowitch")
     (lambda (error value) (should-not error) (setq result value)))
    (let ((item (car (plist-get result :items))))
      (should (equal (plist-get result :namespace) "www.theatlantic.com"))
      (should (equal (plist-get item :guid)
                     "tag:theatlantic.com,2026:50-687618"))
      (should (equal (plist-get item :title)
                     "The End of Reading Is Here"))
      (should (equal (plist-get item :authors) '("Rose Horowitch")))
      (should (equal (plist-get item :date) "2026-07-08T09:55:00Z"))
      (should (string-prefix-p "<h1>The End of Reading Is Here</h1>"
                               (plist-get item :content)))
      (should (string-match-p "https://cdn.theatlantic.com/lead.jpg"
                              (plist-get item :content)))
      (should (string-match-p
               "<p class=\"dropcap\"><strong>Second section</strong>"
               (plist-get item :content)))
      (should-not (string-match-p "<hr>" (plist-get item :content))))))

(ert-deftest elfeed-adapters-theatlantic-makes-dropcap-sections-portable ()
  (require 'elfeed-adapters-theatlantic)
  (let* ((html (elfeed-adapters-test--fixture
                "theatlantic/article.html"))
         (normalized
          (elfeed-adapters-theatlantic--section-html html)))
    (should-not (string-match-p "<hr>" normalized))
    (should (string-match-p
             "<p class=\"dropcap\"><strong>Opening words</strong>"
             normalized))
    (should (string-match-p
             "<p class=\"dropcap\"><strong>Second section</strong>"
             normalized))
    (should-not (string-match-p "class=\"smallcaps\"" normalized))
    (should (equal (elfeed-adapters-theatlantic--section-html
                    "<p>Before</p><hr class=\"author\"><p>After</p>")
                   "<p>Before</p><hr class=\"author\"><p>After</p>"))))

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
