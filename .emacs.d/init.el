; (setq debug-on-error t)
;;; 外部依存なし Emacs本体設定
;; elファイルを読み込むようにするロードパスの設定
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(unless (file-exists-p default-directory)
	  (make-directory default-directory))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")
    
;; Emacs自体が書き込む設定先の変更
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; キーバインドの追加
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines) ; 折り返し
(define-key global-map (kbd "C-t") 'other-window) ; ウィンドウ切り替え
(define-key global-map (kbd "C-x f") 'find-file) ; C-x C-f と同等。もともとはset-fill-columnだけど使わないので上書き

;; 文字数カウント
(defun count-lines-and-chars ()
  (if mark-active
      (format " @%dlines, %dchars@"
	      (count-lines (region-beginning) (region-end))
	      (- (region-end) (region-beginning)))
    (format "\t%dchars" (buffer-size))))

(add-to-list 'default-mode-line-format
	     '(:eval (count-lines-and-chars)) t)

;; 現在位置列数表示
(column-number-mode t)
;; tab文字削除 やったね
(setq-default indent-tabs-mode nil)
;; line番号表示
(global-linum-mode t)
;; よくわからん挨拶メッセージは非表示
(setq inhibit-startup-message t)
;; quitコマンドを用意...C-x C-cと違い強制終了させたい
(defun quit ()
  (interactive)
; (save-buffers-kill-terminal))
  (kill-emacs))

;; 単体行コメントアウト用コマンド
(defun comment-out-current-line ()
  "toggle comment out using comment-dwim"
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1)
  (comment-dwim nil))
(global-set-key (kbd "C-c /") 'comment-out-current-line)

;; chromeでプレビューするようにしたい
(when (executable-find "google-chrome")
  (defun preview-by-chrome (file)
    (interactive "ffilename: ")
    (shell-command (format "google-chrome %s" file)))
  (global-set-key (kbd "C-c g") 'preview-by-chrome))

;; init.elを素早く開けるようにする
(defun init-el ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))
;; paren-mode : 対応するカッコを強調して表示
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "gray")
;; regionの背景色文字色変更
(set-face-background 'region "gray")
(set-face-foreground 'region "black")

;;; emacsclient関連
;; chmod 775 emcs は各自で行ってください
(unless (file-exists-p (setq emcs (concat (getenv "HOME") "/bin/emcs")))
  (write-region "#!/bin/bash\n" nil emcs t)
  (write-region "emacsclient -e '(other-window -1)'\n" nil emcs t)
  (write-region "emacsclient $@\n" nil emcs t))
  ;(shell-command "chmod 775 emcs"))

; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))

;;; ここから外部依存 leafを使用

(prog1 "prepare leaf"
  (prog1 "package"
    (custom-set-variables
     '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("gnu"   . "https://elpa.gnu.org/packages/"))))
    (package-initialize))

  (prog1 "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)       ; renew local melpa cache if fail
         (package-install 'leaf))))

    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init)))

  (prog1 "optional packages for leaf-keywords"
    ;; optional packages if you want to use :hydra, :el-get,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone  . t)))))

;;; leaf群

(leaf undo-tree
  :ensure t
  :leaf-defer nil
  :bind (("M-/" . undo-tree-redo))
  :custom ((global-undo-tree-mode . t)))

(leaf zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

(leaf init-loader
  :ensure t)

(leaf helm
  :ensure t
  :config
  (leaf helm-c-moccur
    :ensure t
    :bind (("M-o" . occur-by-moccur))
    :custom ((moccur-split-word . t))))
    ;; :config
    ;; (leaf moccur-edit
    ;;   :ensure t)))

;; Helm occurがまともに使えるようになるまでの代用
(defun finder ()
  "find with regexp from current buffer"
  (interactive)
  (beginning-of-buffer)
  (isearch-forward-regexp))
(define-key global-map (kbd "C-c f") 'finder)

(defun replacer (reg str)
  "find with regexp and replace from current buffer"
  (interactive "sregexp: \nsto-string: ")
  (beginning-of-buffer)
  (query-replace-regexp reg str))
(define-key global-map (kbd "C-c r") 'replacer)
;; 代用ここまで

(leaf multi-term
  :ensure t
  :custom `((multi-term-program . ,(getenv "SHELL")))
  :preface
  (defun n/open-shell-sub (new)
   (split-window-below)
   (enlarge-window 5)
   (other-window 1)
   (let ((term) (res))
     (if (or new (null (setq term (dolist (buf (buffer-list) res)
                                    (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
                                        (setq res buf))))))
         (multi-term)
       (switch-to-buffer term))))
  (defun n/open-shell ()
    (interactive)
    (n/open-shell-sub t))
  (defun n/to-shell ()
    (interactive)
    (n/open-shell-sub nil))
  :bind (("C-^"   . n/to-shell)
         ("C-M-^" . n/open-shell)
         (:term-raw-map
          ("C-t" . other-window))))

(leaf auto-complete
  :ensure t
  :leaf-defer nil
  :config
  (ac-config-default)
  :custom ((ac-use-menu-map . t)
           (ac-ignore-case . nil))
  :bind (:ac-mode-map
         ; ("M-TAB" . auto-complete))
         ("M-t" . auto-complete)))

(leaf yatex
  :ensure t
  ;; :init
  ;; (setq YaTeX-inhibit-prefix-letter t) ; C-c C-t letter
  :custom ((tex-command . "ptex2pdf -u -l")
           (bibtex-command . "pbibtex"))
  :bind (("C-c C-t" . YaTeX-typeset-menu))
  :mode (("\\.tex\\'" . yatex-mode))
  :config
  (add-hook 'yatex-mode-hook
            #'(lambda ()
                (reftex-mode t)
                (define-key reftex-mode-map
                  (concat YaTeX-prefix ">") 'YaTeX-comment-region)
                (define-key reftex-mode-map
                  (concat YaTeX-prefix "<") 'YaTeX-uncomment-region))))

(leaf markdown-mode
  :ensure t)
  ;; markdown-preview-modeはいまいちだった => pandocを検討中
  ;; :preface
  ;; (unless (executable-find "markdown")
  ;;   (message "!!caution!!: you have to install 'markdown' command to preview markdown"))
  ;; :config
  ;; (leaf markdown-preview-mode
  ;;   :ensure t
  ;;   :custom (`(markdown-preview-stylesheets . ,(list "github.css")))
  ;;   :mode (("\\.md\\'" . gfm-mode))
  ;;   :bind (("C-c m" . markdown-preview-mode))))
