;;; my-evil.el -*- lexical-binding: t -*-

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(after! evil
  (setq evil-move-beyond-eol t)
  (setq evil-move-cursor-back nil)
  (setq evil-highlight-closing-paren-at-point-states nil)


  (setq evil-emacs-state-cursor    '("red" box))
  (setq evil-normal-state-cursor   '("orange" box))
  (setq evil-visual-state-cursor   '("pink" box))
  (setq evil-insert-state-cursor   '("red" bar))
  (setq evil-replace-state-cursor  '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  (setq evil-snipe-enable-alternate-f-and-t-behaviors t)

  (evil-define-key 'normal 'global "p" #'evil-paste-before)

  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "fd")

  (evil-ex-define-cmd "wx" #'za/save-and-kill-this-buffer)

  )

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(general-evil-setup)
(general-mmap
  "H" 'za/smarter-move-beginning-of-line
  "L" 'evil-end-of-visual-line

  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line

  "t" 'evil-find-char
  "T" 'evil-find-char-to-backward

  "f" 'za/evil-find-char-after
  "F" 'evil-find-char-to-backward

  ;; make e land on first char then after word
  ;; (evil-define-type inclusive in my-evil-helpers fixes w and W
  "e" 'za/evil-forward-after-word-end
  "E" 'za/evil-forward-after-WORD-end
  "ge"'za/evil-backward-after-word-end
  "gE"'za/evil-backward-after-WORD-end

  )


(general-imap
  "C-h" 'evil-backward-char
  "C-l" 'evil-forward-char

  "C-j" 'evil-next-visual-line
  "C-k" 'evil-previous-visual-line

  )

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(provide 'myevil)
