;;; elfeed-adapters-http.el --- HTTP helpers for Elfeed Adapters  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; Small asynchronous HTTP abstraction shared by source adapters.  Tests can
;; bind `elfeed-adapters-request-function' to a fixture-backed implementation.

;;; Code:

(require 'url-http)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defcustom elfeed-adapters-user-agent
  "Mozilla/5.0 (Linux; Android 15) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0 Mobile Safari/537.36"
  "User-Agent sent by adapter HTTP requests."
  :type 'string
  :group 'elfeed-adapters)

(defun elfeed-adapters-url-retrieve (url callback &optional headers)
  "Asynchronously retrieve URL, then call CALLBACK with (ERROR BODY).

HEADERS is an alist of additional HTTP request headers."
  (let ((url-request-extra-headers headers)
        (url-user-agent elfeed-adapters-user-agent))
    (url-retrieve
     url
     (lambda (status)
       (let ((buffer (current-buffer)))
         (unwind-protect
             (condition-case error
                 (cond
                  ((plist-get status :error)
                   (funcall callback (plist-get status :error) nil))
                  ((not (and (numberp url-http-response-status)
                             (<= 200 url-http-response-status)
                             (< url-http-response-status 300)))
                   (funcall callback
                            (format "HTTP %s" url-http-response-status) nil))
                  (t
                   (goto-char (or url-http-end-of-headers (point-min)))
                   (funcall callback nil
                            (buffer-substring-no-properties
                             (point) (point-max)))))
               (error (funcall callback error nil)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))
     nil t t)))

(defcustom elfeed-adapters-request-function
  #'elfeed-adapters-url-retrieve
  "Function used to retrieve adapter resources.

The function receives URL, CALLBACK, and optional HEADERS.  CALLBACK receives
(ERROR BODY)."
  :type 'function
  :group 'elfeed-adapters)

(defun elfeed-adapters-request (url callback &optional headers)
  "Retrieve URL and call CALLBACK with (ERROR BODY), passing HEADERS."
  (funcall elfeed-adapters-request-function url callback headers))

(provide 'elfeed-adapters-http)
;;; elfeed-adapters-http.el ends here
