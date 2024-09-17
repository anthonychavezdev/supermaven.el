;;; supermaven-process.el --- Supermaven process management -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides functionality to manage the Supermaven process.

;;; Code:


(require 'json)

(defconst supermaven-process-buffer "*supermaven-process*")

(defcustom supermaven-blob-path (concat user-emacs-directory "supermaven/sm-agent")
  "Path to the supermaven binary"
  :type 'string
  :group 'supermaven)

(defcustom supermaven-log-level 'info
  "Logging level for Supermaven."
  :type '(choice (const :tag "Info" info)
                 (const :tag "Debug" debug)
                 (const :tag "Warning" warning)
                 (const :tag "Error" error))
  :group 'supermaven)

(defvar supermaven-process nil)
(defvar supermaven-compl-callback nil)
(defvar supermaven-current-state-id -1)
(defvar supermaven-current-compl "")

(defun supermaven-log (level message &rest args)
  "Log MESSAGE with LEVEL and ARGS if `supermaven-log-level' allows it."
  (when (>= (cl-position supermaven-log-level
                         '(info debug warning error))
            (cl-position level '(info debug warning error)))
    (apply #'message (concat "Supermaven: " message) args)))

(defun supermaven-parse-messages (string)
  "Parse incoming messages from STRING."
  (let ((start 0))
    (while (string-match "SM-MESSAGE \\({.*\\)$" string start)
      (let* ((json-string (match-string 1 string))
             (json-object (condition-case err
                              (json-read-from-string json-string)
                            (error
                             (supermaven-log 'error "Invalid JSON: %S" err)
                             nil))))
        (when json-object
          (supermaven-process-message json-object)))
      (setq start (match-end 0)))))

(defun supermaven-filter(process string)
  "Filter function for the Supermaven PROCESS, handling incoming STRING."
  (supermaven-log 'debug "Recieved %s" string)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string))
  (supermaven-parse-messages string))

(defun supermaven-process-start (compl-callback)
  (when supermaven-process
	(delete-process supermaven-process))
  (setq supermaven-compl-callback compl-callback)
  (condition-case err
      (progn
        (setq supermaven-process (make-process
                                  :name "supermaven"
                                  :buffer supermaven-process-buffer
								  :command (list supermaven-blob-path "stdio")
								  :connection-type 'pipe
								  :filter #'supermaven-filter
                                  :sentinel #'supermaven-sentinel)))
    (error
     (supermaven-log 'error "Failed to start Supermaven process: %s" err))))

(defun supermaven-sentinel (process event)
  "Handle Supermaven PROCESS events."
  (supermaven-log 'info "Supermaven process %s" (string-trim event))
  (when (string-match-p "\\(?:exited\\|finished\\)" event)
    (supermaven-log 'warning "Supermaven process has stopped. Restarting...")
    (run-with-timer 1 nil #'supermaven-process-start supermaven-compl-callback)))

(defun supermaven-greetings()
  (supermaven-send-json (list :kind "greeting")))

(defun supermaven-process-message (json)
  "Process a JSON message from Supermaven."
  (pcase (alist-get 'kind json)
    ("response" (supermaven-update-state-id json))
    ("metadata" (supermaven-update-metadata json))
    ("activation_request" (supermaven-handle-activation-request json))
    ("activation_success" (supermaven-handle-activation-success))))

(defun supermaven-send (message)
  "Send MESSAGE to the Supermaven process."
  (when supermaven-process
    (process-send-string supermaven-process (concat message "\n"))))

(defun supermaven-send-json (data)
  (supermaven-send (json-serialize data)))

(defun supermaven-process-items (items)
  "Process completion ITEMS from Supermaven."
  (seq-doseq (item items)
    (pcase (alist-get 'kind item)
      ("text" (setq supermaven-current-compl
                   (concat supermaven-current-compl (alist-get 'text item))))
      ("end" (when supermaven-compl-callback
               (if (string-empty-p supermaven-current-compl)
                   (supermaven-log 'debug "Received empty completion")
                 (funcall supermaven-compl-callback supermaven-current-compl))
               (setq supermaven-current-compl ""
                     supermaven-current-state-id (1+ supermaven-current-state-id)))))))

(defun supermaven-update-state-id (json)
  "Update state ID based on JSON response."
  (let ((state-id (string-to-number (alist-get 'stateId json))))
	(if (= state-id supermaven-current-state-id)
		(supermaven-process-items (alist-get 'items json))
	  (setq supermaven-current-compl ""
			supermaven-current-state-id state-id)
	  (supermaven-update-state-id json))))


(defun supermaven-update-metadata (json)
  (supermaven-log 'debug "Received metadata update: %s" json))

(defun supermaven-handle-activation-request (json)
  (with-current-buffer (get-buffer-create supermaven-process-buffer)
    (let ((activation-msg (format "Activation reqest: %s\n" (alist-get 'activateUrl json))))
      (goto-char (point-max))
      (insert activation-msg)
      (message activation-msg))))

(defun supermaven-handle-activation-success ()
  (with-current-buffer (get-buffer-create supermaven-process-buffer)
    (goto-char (point-max))
    (message "Supermaven was activated successfully.")
    (insert "Supermaven was activated successfully.\n")))

(provide 'supermaven-process)
