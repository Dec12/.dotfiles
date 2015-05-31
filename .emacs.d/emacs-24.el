;;; -*- Mode: Emacs-Lisp ; coding: utf-8-unix -*-;;;

;; ----------------------------------------------------
;; package
;; ----------------------------------------------------
(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))

;; 初期化
(package-initialize)


;; ----------------------------------------------------
;; migemo
;; ----------------------------------------------------

(require 'migemo)
(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs"))

;; Set your installed path
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)


;; ----------------------------------------------------
;; auto-complete
;; ----------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "C-m") 'ac-complete)


;; ----------------------------------------------------
;; minimap
;; ----------------------------------------------------
(when (display-graphic-p)
  (require 'minimap)
  ;; enable minimap
  (global-set-key (kbd "C-c m") 'minimap-toggle))

;; ----------------------------------------------------
;; helm
;; ----------------------------------------------------
(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))


;; ----------------------------------------------------
;; ace-isearch
;; ----------------------------------------------------
;; この前にmigemoの設定が必要
(require 'helm-migemo)
;; この修正が必要
(eval-after-load "helm-migemo"
  '(defun helm-compile-source--candidates-in-buffer (source)
     (helm-aif (assoc 'candidates-in-buffer source)
         (append source
                 `((candidates
                    . ,(or (cdr it)
                           (lambda ()
                             ;; Do not use `source' because other plugins
                             ;; (such as helm-migemo) may change it
                             (helm-candidates-in-buffer (helm-get-current-source)))))
                   (volatile) (match identity)))
       source)))

(require 'helm-swoop)
;; isearchからの連携を考えるとC-r/C-sにも割り当て推奨
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)

;; 検索結果をcycleしない、お好みで
(setq helm-swoop-move-to-line-cycle nil)

(cl-defun helm-swoop-nomigemo (&key $query ($multiline current-prefix-arg))
  "シンボル検索用Migemo無効版helm-swoop"
  (interactive)
  (let ((helm-swoop-pre-input-function
         (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol)))))
    (helm-swoop :$source (delete '(migemo) (copy-sequence (helm-c-source-swoop)))
                :$query $query :$multiline $multiline)))
;; C-M-:に割り当て
(global-set-key (kbd "C-M-:") 'helm-swoop-nomigemo)

;; [2014-11-25 Tue]
(when (featurep 'helm-anything)
  (defadvice helm-resume (around helm-swoop-resume activate)
    "helm-anything-resumeで復元できないのでその場合に限定して無効化"
    ad-do-it))

;; ace-isearch
(global-ace-isearch-mode 1)

;; [2015-03-23 Mon]C-u C-s / C-u C-u C-s
(defun isearch-forward-or-helm-swoop (use-helm-swoop)
  (interactive "p")
  (let (current-prefix-arg
        (helm-swoop-pre-input-function 'ignore))
    (call-interactively
     (case use-helm-swoop
       (1 'isearch-forward)
       (4 'helm-swoop)
       (16 'helm-swoop-nomigemo)))))
(global-set-key (kbd "C-s") 'isearch-forward-or-helm-swoop)


;; ----------------------------------------------------
;; magit
;; ----------------------------------------------------

(require 'magit)

;; http://d.hatena.ne.jp/syohex/20130904/1378310201
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun my/magit-quit-session ()
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'my/magit-quit-session)
(defadvice git-commit-commit (after move-to-magit-buffer activate)
  (delete-window))

;; http://qiita.com/dtan4/items/658a8a7ca06aa8c2da4c
(set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient")

(setq magit-last-seen-setup-instructions "1.4.0")

;; http://www.clear-code.com/blog/2012/4/3.html
(set-face-background 'magit-diff-file-header "gray15")
(set-face-background 'magit-diff-hunk-header "gray18")
(set-face-background 'magit-diff-none (face-background 'default))
(set-face-foreground 'magit-diff-none (face-foreground 'diff-context))
(set-face-background 'magit-diff-add (face-background 'diff-added))
(set-face-foreground 'magit-diff-add (face-foreground 'diff-added))
(set-face-background 'magit-diff-del (face-background 'diff-removed))
(set-face-foreground 'magit-diff-del (face-foreground 'diff-removed))
(set-face-background 'magit-item-highlight (face-background 'helm-selection))
(setq magit-diff-refine-hunk 'all)

;; ----------------------------------------------------
;; Look and Feel
;; ----------------------------------------------------

;; theme
(load-theme 'deeper-blue t)

;; 透過
(if window-system (set-frame-parameter nil 'alpha 85))

