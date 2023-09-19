;;
;; Settings
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  (setq user-full-name "Matt Rockwell"
        user-mail-address "matt@zaatas.com"
        display-time-12hr-format t

        ;; Never use tabs, use spaces instead.
           indent-tabs-mode nil
           tab-width 2

   )

   (display-time-mode t)
   (global-set-key (kbd "C-k") nil )

   ;; Revert (update) buffers automatically when underlying files are changed externally.
   (global-auto-revert-mode t)




  ;; org mode
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    (setq org-directory "~/Dotfiles/org/")


  ;; Font and Theme
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    (setq line-spacing 0.4)
    (setq doom-font (font-spec :family "ComicCodeLigatures Nerd Font"
                     :size 18
                     :weight 'regular)

        doom-variable-pitch-font (font-spec :family "Fira Sans"
                     :size 16))

    (setq display-line-numbers-type t)


    (setq doom-theme 'solarized-dark)
    (setq doom-themes-enable-bold t)
    (setq doom-themes-enable-italic t)

    (custom-set-faces!
      '(font-lock-comment-face :slant italic)
      '(font-lock-keyword-face :slant italic))


  

  ;; Weather
  ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     (use-package wttrin
        :ensure t
        :commands (wttrin)
        :init
          (setq wttrin-default-cities '("Denver")))










(provide 'mydefaults)
