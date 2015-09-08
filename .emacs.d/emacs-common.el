;;; -*- Mode: Emacs-Lisp ; coding: utf-8-unix -*-;;;

;; ----------------------------------------------------
;; 基本設定
;; ----------------------------------------------------

;; clipboard
(setq x-select-enable-clipboard t)

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
(add-to-load-path "elisp")

;; 言語
(set-language-environment "Japanese")

;; 文字コード
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)

;; 起動時の画面はいらない
(setq inhibit-startup-message t)

;; スクロールで改行しない
(setq next-line-add-newlines nil)

;; ツールバー、メニューバー非表示
(tool-bar-mode -1)
(menu-bar-mode -1)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 画面スクロール時の重複行数
(setq next-screen-context-lines 1)

;; 1行ずつスクロール
(setq scroll-step 1)

;; shell
(global-set-key [f5] 'shell)


;; ----------------------------------------------------
;; 表示関連
;; ----------------------------------------------------

;; 行番号を左に表示
(require 'linum)
(global-linum-mode t)
(set-face-attribute 'linum nil :foreground "IndianRed" :height 0.8)
(setq linum-format "%4d ")

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; 時間を表示
(display-time-mode 1)

;; 行番号をバーに表示
(column-number-mode 1)

;; ビープ音を鳴らさない
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; 対応する括弧を光らせる。
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)

;; C-x C-f での意味の無いパス表示をグレーアウトする
(file-name-shadow-mode t)

;; scratchバッファの初期メッセージを消す
(setq initial-scratch-message "")

;; yes/no を y/n へ簡略化
(fset 'yes-or-no-p 'y-or-n-p)

;; リージョンをハイライト
(setq-default transient-mark-mode t)

;; 行間を指定
(setq-default line-spacing 0.2)

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)


;; ----------------------------------------------------
;; Look and Feel
;; ----------------------------------------------------

;; mode line 平面化
(set-face-attribute 'mode-line          nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(let ((ws window-system))
  (cond ((eq ws 'ns)
         ;; フォントセットを作る
         (let* ((fontset-name "myfonts") ; フォントセットの名前
                (size 12) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
                (asciifont "Menlo") ; ASCIIフォント
                (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
                (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
                (fontspec (font-spec :family asciifont))
                (jp-fontspec (font-spec :family jpfont)) 
                (fsn (create-fontset-from-ascii-font font nil fontset-name)))
           (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
           (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
           (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
           (set-fontset-font fsn '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
           (set-fontset-font fsn '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
           )
 
         ;; デフォルトのフレームパラメータでフォントセットを指定
         (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))
 
         ;; フォントサイズの比を設定
         (dolist (elt '(("^-apple-hiragino.*" . 1.2)
                        (".*osaka-bold.*" . 1.2)
                        (".*osaka-medium.*" . 1.2)
                        (".*courier-bold-.*-mac-roman" . 1.0)
                        (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                        (".*monaco-bold-.*-mac-roman" . 0.9)))
           (add-to-list 'face-font-rescale-alist elt))
 
         ;; デフォルトフェイスにフォントセットを設定
         ;; (これは起動時に default-frame-alist に従ったフレームが作成されない現象への対処)
         (set-face-font 'default "fontset-myfonts")
         )))


;; ----------------------------------------------------
;; 文字関連
;; ----------------------------------------------------

;; Tab幅4
(setq-default tab-width 4)

;; インデントはTabではなくSpace
(setq-default indent-tabs-mode nil)

;;インデントは4スペース
(setq-default c-basic-offset 4)

;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)

;; リージョンがactiveな時の挙動をWindowsと同じにする
(delete-selection-mode 1)

;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;; 複数の whitespace を削除
(setq backward-delete-char-untabify-method 'hungry)

;; バッファ切り替え時にリージョンを保持
(setq highlight-nonselected-windows t)


;; ----------------------------------------------------
;; キーバインド
;; ----------------------------------------------------

;; 補完
(define-key global-map (kbd "C-c C-i") 'dabbrev-expand)

;; undo
(define-key global-map (kbd "C-z") 'undo)

;; goto-line
(define-key global-map (kbd "M-g") 'goto-line)

;; ある程度 Mac 標準に準拠させる
(setq mac-command-key-is-meta nil)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(global-set-key [?\s-c] 'kill-ring-save)
(global-set-key [?\s-v] 'yank)
(global-set-key [?\s-x] 'kill-region)
(global-set-key [?\s-z] 'undo)
(global-set-key [?\s-s] 'save-buffer)
(global-set-key [?\s-q] 'save-buffers-kill-terminal)
(global-set-key [?\s-f] 'isearch-forward)
(global-set-key [?\s-g] 'isearch-repeat-forward)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'help)


;; ----------------------------------------------------
;; バックアップ関連
;; ----------------------------------------------------

;; バックアップファイルをまとめる
(setq make-backup-files t)
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))

;; 複数のバックアップ世代を管理
(setq version-control t)

;; 新しいものをいくつ残すか
(setq kept-new-versions 5)

;; 古いものをいくつ残すか
(setq kept-old-versions 5)

;; 確認せずに古いものを消す
(setq delete-old-versions t)

;; バージョン管理下のファイルもバックアップを作る
(setq vc-make-backup-files t)

;; Dropbox
(setq auto-save-file-name-transforms
      `((".*/Dropbox/.*" ,temporary-file-directory t)))

