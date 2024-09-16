;;; fetch-binary.el --- Fetch Supermaven binary -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides functionality to fetch the Supermaven binary.

;;; Code:

(require 'supermaven-process)
(require 'url)
(require 'json)

(defun supermaven--get-platform ()
  (pcase system-type
    ('gnu/linux "linux")
    ('darwin "macosx")
    ('windows-nt "windows")
    (_ "")))

(defun supermaven--get-architecture ()
  (car (split-string system-configuration "-")))

(defun supermaven--get-json-url ()
  "Construct the URL for fetching Supermaven binary."
  (format "https://supermaven.com/api/download-path?platform=%s&arch=%s&editor=neovim"
          (supermaven--get-platform)
          (supermaven--get-architecture)))

(defun supermaven-install ()
  "Fetch the Supermaven binary if it doesn't exist."
  (interactive)
  (let ((directory (file-name-directory supermaven-blob-path)))
    (if (file-exists-p supermaven-blob-path)
        (progn
          (message (format "Supermaven binary already exists at %s" supermaven-blob-path))
          supermaven-blob-path)
      (supermaven--download-binary directory))))

(defun supermaven--fetch-json (url)
  "Fetch and parse returned json from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    ;; `url-retrieve-synchronously' also returns the HTTP headers,
    ;; I don't want them, I just want the JSON, so I'm removing them.
    ;; There's an empty line after the headers, so I'm deleting
    ;; everything from that point, up to the start of the buffer.
    (re-search-forward "^$")
    (delete-region (point) (point-min))
    (condition-case err
        (json-parse-buffer)
      (json-error (error "Failed to parse JSON response: %s" (error-message-string err))))))

(defun supermaven--download-binary (directory)
  "Download the supermaven binary to DIRECTORY."
  (make-directory directory t)
      (let* ((url (supermaven--get-json-url))
             (json (supermaven--fetch-json url)))
        (message "Downlaoding Supermaven binary, please wait...")
        (url-copy-file (gethash "downloadUrl" json) supermaven-blob-path)
        (if (file-exists-p supermaven-blob-path)
            (set-file-modes supermaven-blob-path #o755))))

(provide 'fetch-binary)
