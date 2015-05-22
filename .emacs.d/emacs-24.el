;;; -*- Mode: Emacs-Lisp ; coding: utf-8-unix -*-;;;

(defvar is_mac (or (eq window-system 'mac) (featurep 'ns)))

;; ----------------------------------------------------
;; Mac 濁点 (Emacs 24のみ対応)
;; ----------------------------------------------------

(when is_mac
  (require 'ucs-normalize)
  (prefer-coding-system 'utf-8)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)

  (defun ucs-normalize-NFC-buffer ()
    (interactive)
    (ucs-normalize-NFC-region (point-min) (point-max))
    )

  (global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)
  )

;; ----------------------------------------------------
;; フォント関連
;; ----------------------------------------------------

(when is_mac
  (progn
    (create-fontset-from-ascii-font "Monaco-12:weight=normal:slant=normal" nil "menlokakugo")
    (set-fontset-font "fontset-menlokakugo"
                      'unicode
                      (font-spec :family "Hiragino Kaku Gothic ProN" :size 14)
                      nil
                      'append)
    (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))
    )
  )

;; ----------------------------------------------------
;; Look and Feel
;; ----------------------------------------------------

; theme
(load-theme 'deeper-blue t)

; 透過
(if window-system (set-frame-parameter nil 'alpha 85))
