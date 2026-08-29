;;; elfeed-adapters-telegram.el --- Telegram channels for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Consume Telegram's public channel preview pages without authentication.

;;; Code:

(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'xml)
(require 'elfeed-adapters)
(require 'elfeed-adapters-http)

(defun elfeed-adapters-telegram--match (url)
  "Return parameters when URL names a Telegram CHANNEL source."
  (when (string-match
         (rx string-start "adapter:telegram/channel/"
             (group (+ (or alnum "_")))
             (optional "?" (* nonl)) string-end)
         url)
    (list :channel (match-string 1 url))))

(defun elfeed-adapters-telegram--parse-html (html)
  "Parse Telegram preview HTML into a DOM tree."
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun elfeed-adapters-telegram--children-html (node)
  "Serialize the children of DOM NODE as HTML."
  (mapconcat
   (lambda (child)
     (if (stringp child)
         (xml-escape-string child)
       (with-temp-buffer
         (dom-print child)
         (buffer-string))))
   (dom-children node) ""))

(defun elfeed-adapters-telegram--absolute-url (url)
  "Make a protocol-relative Telegram URL URL absolute."
  (when url
    (if (string-prefix-p "//" url)
        (concat "https:" url)
      url)))

(defun elfeed-adapters-telegram--photo-url (node)
  "Extract the background image URL from Telegram photo NODE."
  (when-let* ((style (dom-attr node 'style)))
    (when (string-match
           (rx "background-image:url(" (optional (any "\"'"))
               (group (+ (not (any "\"')"))))
               (optional (any "\"'")) ")")
           style)
      (elfeed-adapters-telegram--absolute-url (match-string 1 style)))))

(defun elfeed-adapters-telegram--media-html (message)
  "Render supported media from Telegram MESSAGE as HTML."
  (concat
   (mapconcat
    (lambda (photo)
      (when-let* ((source (elfeed-adapters-telegram--photo-url photo)))
        (format "<p><img src=\"%s\"></p>" (xml-escape-string source))))
    (dom-by-class message "tgme_widget_message_photo_wrap") "")
   (mapconcat
    (lambda (video)
      (when-let* ((source (elfeed-adapters-telegram--absolute-url
                           (dom-attr video 'src))))
        (format "<p><a href=\"%s\">▶ Telegram 视频</a></p>"
                (xml-escape-string source))))
    (dom-by-tag message 'video) "")))

(defun elfeed-adapters-telegram--title (text-node post-id)
  "Derive an entry title from TEXT-NODE or POST-ID."
  (let ((text (and text-node
                   (string-trim
                    (replace-regexp-in-string
                     (rx (+ (or space "\n" "\r"))) " "
                     (dom-inner-text text-node))))))
    (if (and text (not (string-empty-p text)))
        (truncate-string-to-width text 100 nil nil "…")
      (format "Telegram 消息 #%s" post-id))))

(defun elfeed-adapters-telegram--item (message channel channel-title)
  "Convert Telegram MESSAGE from CHANNEL named CHANNEL-TITLE to an item."
  (when-let* ((post (dom-attr message 'data-post))
              (post-id (car (last (split-string post "/" t))))
              (date-link (car (dom-by-class
                               message "tgme_widget_message_date")))
              (time-node (car (dom-by-tag date-link 'time)))
              (date (dom-attr time-node 'datetime)))
    (let* ((text-node (car (dom-by-class
                            message "tgme_widget_message_text")))
           (author-node (car (dom-by-class
                              message "tgme_widget_message_owner_name")))
           (author (or (and author-node
                            (string-trim (dom-inner-text author-node)))
                       channel-title
                       channel))
           (link (or (dom-attr date-link 'href)
                     (format "https://t.me/%s/%s" channel post-id)))
           (text-html (and text-node
                           (elfeed-adapters-telegram--children-html text-node)))
           (media-html (elfeed-adapters-telegram--media-html message)))
      (list :guid (format "telegram-%s-%s" channel post-id)
            :title (elfeed-adapters-telegram--title text-node post-id)
            :link link
            :date date
            :authors (list author)
            :content (concat (when text-html
                               (format "<div>%s</div>" text-html))
                             media-html)
            :content-type 'html))))

(defun elfeed-adapters-telegram--result (html channel)
  "Parse Telegram preview HTML for CHANNEL into an adapter result."
  (let* ((document (elfeed-adapters-telegram--parse-html html))
         (title-node (car (dom-by-class document "tgme_header_title")))
         (title (or (and title-node
                         (string-trim (dom-inner-text title-node)))
                    channel))
         (items (delq nil
                      (mapcar
                       (lambda (message)
                         (elfeed-adapters-telegram--item
                          message channel title))
                       (dom-by-class document "js-widget_message")))))
    (unless items
      (error "No public Telegram messages found for %s" channel))
    (list :title (format "Telegram - %s" title)
          :namespace "t.me"
          :authors (list title)
          :items items)))

(defun elfeed-adapters-telegram--fetch (_url parameters callback)
  "Fetch a Telegram channel described by PARAMETERS and call CALLBACK."
  (let* ((channel (plist-get parameters :channel))
         (preview-url (format "https://t.me/s/%s" channel)))
    (elfeed-adapters-request
     preview-url
     (lambda (error body)
       (if error
           (funcall callback error nil)
         (condition-case parse-error
             (funcall callback nil
                      (elfeed-adapters-telegram--result body channel))
           (error (funcall callback parse-error nil))))))))

;;;###autoload
(defun elfeed-adapters-telegram-register ()
  "Register the Telegram channel adapter."
  (elfeed-adapters-register
   'telegram #'elfeed-adapters-telegram--match
   #'elfeed-adapters-telegram--fetch))

(elfeed-adapters-telegram-register)

(provide 'elfeed-adapters-telegram)
;;; elfeed-adapters-telegram.el ends here
