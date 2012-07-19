;; csearch-mode.el
;;
;; url: github.com/filsinger/csearch-mode
;;
;; author: Jason Filsinger
;;
;;


(defvar csearch/result-regexp "^\\(.+?\\):\\([0-9]+?\\):\\(.+\\)[\n]")

(defvar csearch/keymap (make-sparse-keymap))
(define-key csearch/keymap (kbd "RET") 'csearch/select-match)

(defun csearch/cancel ()
  ""
  (kill-buffer "*csearch-list*")
)

(defun csearch/get-result-match (pt &optional sub)
  ""
  (message "searching for string")
  (let (ret-match)
	(save-excursion
	  (goto-char pt)
	  (goto-char (point-at-bol))

	  (when (re-search-forward csearch/result-regexp)
		(if sub
			(setq ret-match (match-string sub))
		  (setq ret-match (match-string 0)))
		)) ret-match))


(defun csearch/get-match-file-at-point (pt)
  ""
  (csearch/get-result-match pt 1))

(defun csearch/get-match-string-at-point (pt)
  ""
  (csearch/get-result-match pt 3))

(defun csearch/get-match-line-at-point (pt)
  ""
  (string-to-number (csearch/get-result-match pt 2)))

(defun csearch/select-match ()
  ""
  (let ((selected-file (csearch/get-match-file-at-point (point) ))
		(selected-line (csearch/get-match-line-at-point (point))) )
	(if selected-file
		(progn
		  (find-file-existing selected-file)
		  (goto-char (point-min))
		  (forward-line selected-line)
		  )
	  (error (concat "File not found: " selected-file))
	  )
	)
  )

(defun csearch/insert-sorted-lines (ARG &optional REVERSE)
  ""
  (save-excursion
	(let ((entries-start (point)))
	  (insert (shell-command-to-string (concat "~/bin/csearch -n " regexp)))
	  (sort-lines nil entries-start (point))
	  )
	)
)

(defun csearch/csearch (regexp)
  "run the csearch tool"
  (with-current-buffer (switch-to-buffer-other-window "*csearch-list*")
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert "Type RET on an entry to open file\n\n")
	(insert (concat "Codesearch matches for regexp `" regexp "`:\n\n"))

	(csearch/insert-sorted-lines (shell-command-to-string (concat "~/bin/csearch -n " regexp)))
	(while (re-search-backward csearch/result-regexp nil t)
	  (let ((curr-overlay (make-overlay (match-beginning 0) (match-end 0) nil nil t)))
		(overlay-put curr-overlay 'priority 100)
		(overlay-put curr-overlay 'keymap csearch/keymap)
		(overlay-put curr-overlay 'evaporate t)
		(overlay-put curr-overlay 'before-string " ")
		))
	(setq buffer-read-only t)))

(defun csearch/apropos (REGEXP)
  "Display a list of all symbols in the csearch index that REGEXP matches"
  (interactive "MSymbol (word or regexp): ")
  (csearch/csearch REGEXP))

(provide 'csearch-mode)
