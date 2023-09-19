;;; -*- lexical-binding: t -*-
;;;
;;; References:
;;;
;;; https://www.dr-qubit.org/Evil_cursor_model.html
;;;
;;;

;; set the postion and size of inital startup frame
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun za/set-initial-frame-size ()
  (let ((frame (selected-frame)))
    ;;  (set-frame-position frame 100 100)
    (set-frame-size frame 120 50)))

(add-hook 'after-init-hook 'za/set-initial-frame-size)


(defun za/save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun za/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

      Move point to the first non-whitespace character on this line.
      If point is already there, move to the beginning of the line.
      Effectively toggle between the first non-whitespace character and
      the beginning of the line.

      If ARG is not nil or 1, move forward ARG - 1 lines first.  If
      point reaches the beginning or end of the buffer, stop there."

  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `za/smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'za/smarter-move-beginning-of-line)




;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(provide 'helpers)
