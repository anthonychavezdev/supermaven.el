(require 'supermaven-process)
(require 'fetch-binary)

(defvar supermaven-next-id 0)

(defvar-local supermaven-overlay nil
  "Overlay for Supermaven completion.")

(defface supermaven-overlay-face
  '((t :inherit shadow))
  "Face for Supermaven overlay.")

(defun supermaven-install ()
  "Fetch the Supermaven binary if it doesn't exist."
  (interactive)
  (let ((directory (file-name-directory supermaven-blob-path)))
    (if (file-exists-p supermaven-blob-path)
        (progn
          (message (format "Supermaven binary already exists at %s" supermaven-blob-path))
          supermaven-blob-path)
      (supermaven--download-binary directory))))


(defun supermaven-start()
  (interactive)
  (supermaven-process-start 'supermaven-compl-callback)
  (setq supermaven-next-id 0))

(defun supermaven-use-free ()
  (interactive)
  (supermaven-send-json (list :kind "use_free_version")))

(defun supermaven-complete()
  (interactive)
  (supermaven-on-update (current-buffer) t))

(defun supermaven-make-cursor-update (buffer)
  "Create a cursor update for BUFFER."
  (list :kind "cursor_update"
        :path (buffer-file-name buffer)
        :offset (with-current-buffer buffer (point))))

(defun supermaven-make-file-update (buffer)
  "Create a file update for BUFFER."
  (list :kind "file_update"
        :path (buffer-file-name buffer)
        :content (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))

(defun supermaven-on-update (buffer do-send-file)
  (let ((hash (make-hash-table :test 'equal))
		 (updates (vector (supermaven-make-cursor-update buffer))))
	(puthash "newId" (number-to-string supermaven-next-id) hash)
	(setq supermaven-next-id (+ 1 supermaven-next-id))
	(puthash "kind" "state_update" hash)

	(when do-send-file
	  (setq updates (vconcat updates (vector (supermaven-make-file-update buffer)))))

	(puthash "updates" updates hash)

	(supermaven-send (json-serialize hash))))

(defun supermaven-compl-callback(completion)
  (supermaven-clear-overlay)
  (let* ((ov (supermaven-get-overlay))
		 (tail (buffer-substring (point) (line-end-position)))
		 (p-completion (concat (propertize completion 'face 'supermaven-overlay-face) tail)))
	(move-overlay ov (point) (line-end-position))

	(if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
		  (put-text-property 0 1 'cursor t p-completion)
		  (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
	  (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
	(overlay-put ov 'completion completion)
	(overlay-put ov 'start (point))))


(defun supermaven-get-overlay ()
  "Create or get overlay for Supermaven."
  (unless (overlayp supermaven-overlay)
    (setq supermaven-overlay (make-overlay 1 1 nil nil t)))
  supermaven-overlay)

(defun supermaven-clear-overlay ()
  (when (overlayp supermaven-overlay)
	(message "clear")
	(delete-overlay supermaven-overlay)))

(defun supermaven-accept-completion ()
  (interactive)
  (let* ((ov (supermaven-get-overlay))
		 (completion (overlay-get ov 'completion)))
	(insert completion)
	(supermaven-clear-overlay)))

(defun supermaven-reject-completion ()
  (interactive)
  (supermaven-clear-overlay))

(provide 'supermaven)
