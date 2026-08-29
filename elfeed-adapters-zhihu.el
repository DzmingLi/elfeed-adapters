;;; elfeed-adapters-zhihu.el --- Zhihu authors for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Fetch Zhihu articles and answers directly.  Cookies are read at request
;; time from an explicitly configured browser profile; they are never copied
;; into configuration files or persisted by this package.

;;; Code:

(require 'browser-cookies)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'xml)
(require 'zhihu) ; Reuse the maintained ZSE v4 signer.
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defgroup elfeed-adapters-zhihu nil
  "Zhihu sources for Elfeed Adapters."
  :group 'elfeed-adapters
  :prefix "elfeed-adapters-zhihu-")

(defcustom elfeed-adapters-zhihu-browser 'firefox
  "Browser backend used to read the Zhihu session."
  :type '(choice (const firefox) (const chromium) (const chrome)
                 (const brave) (const edge) (const vivaldi))
  :group 'elfeed-adapters-zhihu)

(defcustom elfeed-adapters-zhihu-profile-directory nil
  "Explicit browser profile directory containing the Zhihu session."
  :type '(choice (const :tag "Not configured" nil) directory)
  :group 'elfeed-adapters-zhihu)

(defun elfeed-adapters-zhihu--match (url)
  "Return parameters when URL is a supported Zhihu adapter URL."
  (cond
   ((string-match
     (rx string-start "adapter:zhihu/posts/"
         (group (or "people" "org")) "/"
         (group (+ (not (any "/?#")))) string-end)
     url)
    (list :kind 'posts :user-type (match-string 1 url)
          :user-id (match-string 2 url)))
   ((string-match
     (rx string-start "adapter:zhihu/answers/"
         (group (+ (not (any "/?#")))) string-end)
     url)
    (list :kind 'answers :user-type "people"
          :user-id (match-string 1 url)))))

(defun elfeed-adapters-zhihu--cookies (url)
  "Read the browser cookies applicable to Zhihu URL."
  (browser-cookies-get
   url :browser elfeed-adapters-zhihu-browser
   :profile-directory elfeed-adapters-zhihu-profile-directory))

(defun elfeed-adapters-zhihu--headers (url referer)
  "Build authenticated and signed request headers for URL and REFERER."
  (let* ((cookies (elfeed-adapters-zhihu--cookies url))
         (dc0 (cdr (assoc-string "d_c0" cookies)))
         (cookie-header
          (mapconcat (lambda (cookie)
                       (format "%s=%s" (car cookie) (cdr cookie)))
                     cookies "; ")))
    (unless dc0
      (error "Zhihu browser profile has no d_c0 cookie"))
    (append
     `(("Cookie" . ,cookie-header)
       ("Referer" . ,referer)
       ("x-api-version" . "3.0.91")
       ("x-app-za" . "OS=Web")
       ("x-requested-with" . "fetch"))
     (zhihu--zse-request-headers url nil dc0))))

(defun elfeed-adapters-zhihu--clean-content (content)
  "Make common Zhihu HTML CONTENT friendlier to feed readers."
  (let ((result (or content "")))
    (setq result
          (replace-regexp-in-string
           "<noscript\\(?:[[:space:]][^>]*\\)?>.*?</noscript>" "" result t))
    (setq result
          (replace-regexp-in-string
           "\\(?:data-actualsrc\\|data-original\\)=\"\\([^\"]+\\)\""
           "src=\"\\1\"" result t))
    result))

(defun elfeed-adapters-zhihu--profile-url (user-type user-id)
  "Return profile URL for USER-TYPE and USER-ID."
  (format "https://www.zhihu.com/%s/%s/" user-type user-id))

(defun elfeed-adapters-zhihu--api-url (parameters)
  "Return the item API URL described by PARAMETERS."
  (let ((id (plist-get parameters :user-id)))
    (pcase (plist-get parameters :kind)
      ('posts
       (concat
        "https://www.zhihu.com/api/v4/members/" id "/articles?"
        (url-build-query-string
         '(("include"
            "data[*].comment_count,content,voteup_count,created,updated;data[*].author.vip_info")
           ("offset" "0") ("limit" "20") ("sort_by" "created")))))
      ('answers
       (concat
        "https://www.zhihu.com/api/v4/members/" id "/answers?"
        (url-build-query-string
         '(("limit" "20")
           ("include" "data[*].is_normal,content")))))
      (_ (error "Unsupported Zhihu source")))))

(defun elfeed-adapters-zhihu--post-item (item)
  "Convert Zhihu article ITEM to a normalized adapter item."
  (let* ((id (format "%s" (plist-get item :id)))
         (author (plist-get item :author)))
    (list :guid (concat "zhihu-article-" id)
          :title (or (plist-get item :title) "知乎文章")
          :link (format "https://zhuanlan.zhihu.com/p/%s" id)
          :date (plist-get item :created)
          :authors (list (or (plist-get author :name) "知乎用户"))
          :content (elfeed-adapters-zhihu--clean-content
                    (plist-get item :content))
          :content-type 'html)))

(defun elfeed-adapters-zhihu--answer-item (item)
  "Convert Zhihu answer ITEM to a normalized adapter item."
  (let* ((id (format "%s" (plist-get item :id)))
         (question (plist-get item :question))
         (question-id (format "%s" (plist-get question :id)))
         (author (plist-get item :author)))
    (list :guid (concat "zhihu-answer-" id)
          :title (or (plist-get question :title) "知乎回答")
          :link (format "https://www.zhihu.com/question/%s/answer/%s"
                        question-id id)
          :date (plist-get item :created_time)
          :authors (list (or (plist-get author :name) "知乎用户"))
          :content (elfeed-adapters-zhihu--clean-content
                    (plist-get item :content))
          :content-type 'html)))

(defun elfeed-adapters-zhihu--fetch-items
    (parameters profile callback profile-data)
  "Fetch PARAMETERS items and call CALLBACK, using PROFILE and PROFILE-DATA."
  (let* ((api-url (elfeed-adapters-zhihu--api-url parameters))
         (headers (elfeed-adapters-zhihu--headers api-url profile)))
    (elfeed-adapters-request
     api-url
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (let* ((payload (json-parse-string
                              body :object-type 'plist :array-type 'list
                              :null-object nil :false-object nil))
                    (data (plist-get payload :data))
                    (kind (plist-get parameters :kind))
                    (name (or (plist-get profile-data :name)
                              (plist-get parameters :user-id)))
                    (items (mapcar
                            (if (eq kind 'posts)
                                #'elfeed-adapters-zhihu--post-item
                              #'elfeed-adapters-zhihu--answer-item)
                            data)))
               (funcall callback nil
                        (list :title
                              (format "%s 的知乎%s" name
                                      (if (eq kind 'posts) "文章" "回答"))
                              :namespace "zhihu.com"
                              :authors (list name)
                              :items items)))
           (error (funcall callback parse-error nil)))))
     headers)))

(defun elfeed-adapters-zhihu--fetch (_url parameters callback)
  "Fetch the Zhihu source described by PARAMETERS and call CALLBACK."
  (condition-case setup-error
      (let* ((user-id (plist-get parameters :user-id))
             (user-type (plist-get parameters :user-type))
             (profile (elfeed-adapters-zhihu--profile-url user-type user-id))
             (profile-api
              (format "https://www.zhihu.com/api/v4/members/%s" user-id))
             (headers (elfeed-adapters-zhihu--headers profile-api profile)))
        (elfeed-adapters-request
         profile-api
         (lambda (error body)
           (if error
               (funcall callback error nil)
             (condition-case parse-error
                 (elfeed-adapters-zhihu--fetch-items
                  parameters profile callback
                  (json-parse-string
                   body :object-type 'plist :array-type 'list
                   :null-object nil :false-object nil))
               (error (funcall callback parse-error nil)))))
         headers))
    (error (funcall callback setup-error nil))))

;;;###autoload
(defun elfeed-adapters-zhihu-register ()
  "Register the Zhihu adapter."
  (elfeed-adapters-register
   'zhihu #'elfeed-adapters-zhihu--match
   #'elfeed-adapters-zhihu--fetch))

(elfeed-adapters-zhihu-register)

(provide 'elfeed-adapters-zhihu)
;;; elfeed-adapters-zhihu.el ends here
