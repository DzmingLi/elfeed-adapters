;;; elfeed-adapters-netease-music.el --- NetEase Music events  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Code:

(require 'json)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defun elfeed-adapters-netease-music--match (url)
  "Return parameters when URL names a NetEase Music user event source."
  (when (string-match
         (rx string-start "adapter:netease-music/user/events/"
             (group (+ digit)) string-end)
         url)
    (list :user-id (match-string 1 url))))

(defun elfeed-adapters-netease-music--https (url)
  "Upgrade protocol-relative or HTTP URL to HTTPS."
  (when url
    (cond
     ((string-prefix-p "//" url) (concat "https:" url))
     ((string-prefix-p "http://" url) (concat "https://" (substring url 7)))
     (t url))))

(defun elfeed-adapters-netease-music--song-html (song)
  "Render SONG as an official NetEase Music player card."
  (when song
    (let* ((id (format "%s" (plist-get song :id)))
           (name (or (plist-get song :name) "网易云单曲"))
           (artists
            (mapconcat (lambda (artist)
                         (or (plist-get artist :name) ""))
                       (plist-get song :artists) " / "))
           (album (plist-get song :album))
           (album-name (plist-get album :name))
           (cover (elfeed-adapters-netease-music--https
                   (or (plist-get album :picUrl)
                       (plist-get album :blurPicUrl))))
           (song-url (format "https://music.163.com/#/song?id=%s" id))
           (player-url
            (format
             "https://music.163.com/outchain/player?type=2&amp;id=%s&amp;auto=0&amp;height=66"
             id)))
      (concat
       "<div class=\"netease-music-player\">"
       (when cover
         (format
          "<p><a href=\"%s\"><img src=\"%s\" width=\"96\" alt=\"%s\"></a></p>"
          song-url (xml-escape-string cover) (xml-escape-string name)))
       (format "<p><strong>%s</strong>%s%s</p>"
               (xml-escape-string name)
               (if (string-empty-p artists) ""
                 (format " — %s" (xml-escape-string artists)))
               (if album-name
                   (format "<br><small>%s</small>"
                           (xml-escape-string album-name))
                 ""))
       ;; Full web readers can render the official iframe.  SHR ignores
       ;; iframes, so keep a visible play link immediately after it.
       (format
        "<iframe src=\"%s\" width=\"330\" height=\"86\" frameborder=\"0\"></iframe>"
        player-url)
       (format "<p><a href=\"%s\">▶ 在网易云音乐播放</a></p>" song-url)
       "</div>"))))

(defun elfeed-adapters-netease-music--event (event nickname)
  "Convert EVENT belonging to NICKNAME to a normalized adapter item."
  (let* ((id (format "%s" (plist-get event :id)))
         (user (plist-get event :user))
         (user-id (format "%s" (plist-get user :userId)))
         (info (plist-get event :info))
         (thread (plist-get info :commentThread))
         (payload (condition-case nil
                      (json-parse-string
                       (or (plist-get event :json) "{}")
                       :object-type 'plist :array-type 'list
                       :null-object nil :false-object nil)
                    (error nil)))
         (message (or (plist-get payload :msg) ""))
         (song (plist-get payload :song))
         (pictures (plist-get event :pics)))
    (list
     :guid (concat "netease-event-" id)
     :title (or (plist-get thread :resourceTitle)
                (and (not (string-empty-p message)) message)
                (format "%s 的云村动态" nickname))
     :link (format "https://music.163.com/#/event?id=%s&uid=%s" id user-id)
     :date (/ (float (plist-get event :eventTime)) 1000.0)
     :authors (list nickname)
     :content
     (concat
      (format "<p>%s</p>"
              (replace-regexp-in-string
               "\n" "<br>" (xml-escape-string message) t t))
      (elfeed-adapters-netease-music--song-html song)
      (mapconcat
       (lambda (picture)
         (if-let* ((source (plist-get picture :originUrl)))
             (format "<p><img src=\"%s\"></p>"
                     (xml-escape-string source))
           ""))
       pictures ""))
     :content-type 'html)))

(defun elfeed-adapters-netease-music--fetch (_url parameters callback)
  "Fetch NetEase Music events described by PARAMETERS and call CALLBACK."
  (let ((user-id (plist-get parameters :user-id)))
    (elfeed-adapters-request
     (format "https://music.163.com/api/event/get/%s" user-id)
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (let* ((payload (json-parse-string
                              body :object-type 'plist :array-type 'list
                              :null-object nil :false-object nil))
                    (events (plist-get payload :events))
                    (user (plist-get (car events) :user))
                    (nickname (or (plist-get user :nickname) user-id)))
               (funcall callback nil
                        (list :title (format "%s的云村动态" nickname)
                              :namespace "music.163.com"
                              :authors (list nickname)
                              :items
                              (mapcar
                               (lambda (event)
                                 (elfeed-adapters-netease-music--event
                                  event nickname))
                               events))))
           (error (funcall callback parse-error nil)))))
     '(("Referer" . "https://music.163.com/")))))

;;;###autoload
(defun elfeed-adapters-netease-music-register ()
  "Register the NetEase Music adapter."
  (elfeed-adapters-register
   'netease-music #'elfeed-adapters-netease-music--match
   #'elfeed-adapters-netease-music--fetch))

(elfeed-adapters-netease-music-register)

(provide 'elfeed-adapters-netease-music)
;;; elfeed-adapters-netease-music.el ends here
