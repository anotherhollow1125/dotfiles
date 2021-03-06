; (setq debug-on-error t)
;;; 外部依存なし Emacs本体設定
;; elファイルを読み込むようにするロードパスの設定
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(defun namn/add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (unless (file-exists-p default-directory)
          (make-directory default-directory))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(namn/add-to-load-path "elisp" "conf" "public_repos")

;; Emacs自体が書き込む設定先の変更
(setq custom-file (locate-user-emacs-file "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; キーバインドの追加
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines) ; 折り返し
(define-key global-map (kbd "C-t") 'other-window) ; ウィンドウ切り替え
(define-key global-map (kbd "C-x f") 'find-file) ; C-x C-f と同等。もともとはset-fill-columnだけど使わないので上書き
(define-key global-map (kbd "C-<up>") 'scroll-down-line)
(define-key global-map (kbd "C-<down>") 'scroll-up-line)
(define-key global-map (kbd "C-c k") 'kill-buffer-and-window)
(define-key global-map (kbd "C-c i d") 'electric-indent-local-mode)

;; 現在位置列数表示
(column-number-mode t)
;; tab文字削除 やったね
(setq-default indent-tabs-mode nil)
;; line番号表示
(global-linum-mode t)
;; よくわからん挨拶メッセージは非表示
(setq inhibit-startup-message t)
;; バックアップファイルの保存先の変更
(setq backup-directory-alist '((".*" . "~/.ehist")))
;; quitコマンドを用意...C-x C-cと違い強制終了させたい
(defalias 'quit 'kill-emacs)
;; ビープ音削除
(setq ring-bell-function 'ignore)

;; 単体行コメントアウト用コマンド
(defun namn/comment-out-current-line ()
  "toggle comment out using comment-dwim"
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1)
  (comment-dwim nil))
(global-set-key (kbd "C-c /") 'namn/comment-out-current-line)

;; chromeでプレビューするようにしたい
(when (executable-find "google-chrome")
  (defun namn/preview-by-chrome (file)
    (interactive "ffilename: ")
    (shell-command (format "google-chrome %s" file)))
  (global-set-key (kbd "C-c g") 'namn/preview-by-chrome))

;; init.elを素早く開けるようにする
(defun init-el ()
  (interactive)
  (find-file (expand-file-name (concat user-emacs-directory "init.el"))))

;; カレントバッファのファイルをゴミ箱に移動する
(defun trash (bname)
  (interactive "btrash: ")
  (let (fname bf)
    (when (setq fname (buffer-file-name (setq bf (get-buffer bname))))
      (shell-command (format "mv %s $TRASH" fname))
      (kill-buffer bf))))

;; backward-kill-line ほしかったので取ってきた
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-x DEL") 'backward-kill-line)

;; 行頭の最初の空白文字ではない文字に移動
(defun move-the-first-char-of-line ()
  "Function 行頭の最初の空白文字ではない文字に移動."
  (interactive)
  (move-beginning-of-line nil)
  (while (or (eq 9 (following-char)) (eq 32 (following-char)))
    (forward-char)))
(global-set-key (kbd "C-c a") 'move-the-first-char-of-line)
(global-set-key (kbd "C-c C-a") 'move-the-first-char-of-line)
(global-set-key (kbd "C-#") 'hs-toggle-hiding)

;;; emacsclient関連
(unless (file-exists-p (setq emcs (concat (getenv "HOME") "/bin/emcs")))
  (write-region "#!/bin/bash\n" nil emcs t)
  (write-region "emacsclient -e '(other-window -1)'\n" nil emcs t)
  (write-region "emacsclient $@\n" nil emcs t)
  (shell-command (format "chmod u+x %s" emcs)))

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

;;; 表示関連ここから

(leaf zenburn-theme
  :ensure t
  :config (load-theme 'zenburn t))

;; paren-mode : 対応するカッコを強調して表示
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
;(set-face-background 'show-paren-match-face "gray")
(set-face-attribute 'show-paren-match nil
      :background "gray"
      :underline 'unspecified)

;; regionの背景色文字色変更
(set-face-attribute 'region nil
                    :background "gray"
                    :foreground "black")


(leaf smart-mode-line
  :ensure t
  :custom ((sml/no-confirm-load-theme . t)
           (sml/theme . 'dark)
           (sml/shorten-directory . -1))
  :config
  (sml/setup))

;; 文字数カウント
(defun namn/count-lines-and-chars ()
  (if mark-active
      (format " @%dl, %dc@"
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))
    (format " (%d)" (buffer-size))))

;; (add-to-list 'mode-line-format
;;              '(:eval (namn/count-lines-and-chars)) t)
(defun namn/set-cl-counter ()
  (setq-default mode-line-front-space
                (add-to-list 'mode-line-front-space
                             '(:eval (namn/count-lines-and-chars)) t)))
(add-hook 'after-init-hook 'namn/set-cl-counter)

;; TAB狩り

(leaf whitespace
  :ensure t
  :custom
  ((whitespace-style . '(face
                         trailing
                         tabs
                         ;; spaces
                         ;; empty
                         space-mark
                         tab-mark))
   (whitespace-display-mappings . '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
   (global-whitespace-mode . t)))

;;; 表示関連ここまで

;; (leaf init-loader
;;   :ensure t)

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

;; Helm moccurになれるまで用
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
  :disabled (eq system-type 'windows-nt)
  :ensure t
  :custom `((multi-term-program . ,(getenv "SHELL")))
  :preface
  (defun namn/open-shell-sub (new)
   (split-window-below)
   (enlarge-window 5)
   (other-window 1)
   (let ((term) (res))
     (if (or new (null (setq term (dolist (buf (buffer-list) res)
                                    (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
                                        (setq res buf))))))
         (multi-term)
       (switch-to-buffer term))))
  (defun namn/open-shell ()
    (interactive)
    (namn/open-shell-sub t))
  (defun namn/to-shell ()
    (interactive)
    (namn/open-shell-sub nil))
  :bind (("C-^"   . namn/to-shell)
         ("C-M-^" . namn/open-shell)
         (:term-raw-map
          ("C-t" . other-window))))

;; もしWindowsの場合...応急処置的です。
;; (when (eq system-type 'windows-nt)
;;   (defun namn/to-shell ()
;;     (interactive)
;;     (eshell)
;;     (shrink-window 5))
;;   (define-key global-map (kbd "C-^") 'namn/to-shell))

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
  :ensure t
  :init
  (leaf *markdown-preview
    :disabled (not (and (executable-find "md2pdf_by_pandoc")
                        (setq viewer (or (executable-find "evince") (executable-find "google-chrome")))))
    :preface
    (defun namn/md-compile ()
      (interactive)
      (call-process-shell-command (format "md2pdf_by_pandoc %s" (buffer-file-name)) nil "*Shell Command Output*" t))
    (defun namn/md-preview ()
      (interactive)
      (namn/md-compile)
      (call-process-shell-command (format "%s %s &" viewer (replace-regexp-in-string "\.md" "\.pdf" (buffer-file-name))) nil 0))
    :bind (:markdown-mode-map
           :package markdown-mode
           ("C-c m" . namn/md-preview)
           ("C-c c" . namn/md-compile))))

;; (define-key markdown-mode-map (kbd "C-c m") 'namn/md-preview)

  ;; markdown-preview-modeはいまいちだった => pandocを使用することにした
  ;; :preface
  ;; (unless (executable-find "markdown")
  ;;   (message "!!caution!!: you have to install 'markdown' command to preview markdown"))
  ;; :config
  ;; (leaf markdown-preview-mode
  ;;   :ensure t
  ;;   :custom (`(markdown-preview-stylesheets . ,(list "github.css")))
  ;;   :mode (("\\.md\\'" . gfm-mode))
  ;;   :bind (("C-c m" . markdown-preview-mode))))

(when (executable-find "~/bin/mozc_emacs_helper.sh")
  (load "setting-for-wsl"))

(leaf magit
  :ensure t)
  ;; 表示バグった
  ;; :config
  ;; (leaf git-gutter
  ;;   :ensure t
  ;;   :custom ((global-git-gutter-mode . t))
  ;;   :config
  ;;   (git-gutter:linum-setup)))

(leaf fish-mode
  :disabled (not (executable-find "fish"))
  :ensure t)

(leaf flycheck
  :ensure t
  :init (global-flycheck-mode))

(leaf flycheck-rust
  :ensure t)

;; rust-mode
(leaf *rust
  :config
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
  (leaf rust-mode
    :disabled (not (executable-find "rustc"))
    :ensure t racer
    :hook
    (rust-mode-hook . (lambda ()
                        (racer-mode)
                        (hs-minor-mode 1)
                        (flycheck-rust-setup)))
    (racer-mode-hook . eldoc-mode)
    (racer-mode-hook . (lambda () (auto-complete-mode)))
    :custom ((rust-format-on-save . t))
    :mode "\\.rs\\'"))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; yasnippet
(leaf yasnippet
  :ensure t
  :leaf-defer nil
  :bind (:yas-minor-mode-map
         :package yasnippet
         ("C-c i i" . yas-insert-snippet)
         ("C-c i n" . yas-new-snippet)
         ("C-c i v" . yas-visit-snippet-file))
  :config
  (let ((snippet-directory "~/.emacs.d/mysnippets"))
    (unless (file-exists-p snippet-directory)
      (make-directory snippet-directory))
    (setq yas-snippet-dirs
          `(,snippet-directory)))
  (yas-global-mode 1))

(leaf *go
  :config
  (add-to-list 'exec-path (expand-file-name "~/.goenv/shims/"))
  (leaf go-mode
    :disabled (not (executable-find "go"))
    :ensure t go-eldoc
    :hook (go-mode-hook . (lambda ()
                            (go-eldoc-setup)
                            (set (make-local-variable 'whitespace-style) '())
                            (set (make-local-variable 'tab-width) 2)))
    :mode "\\.go\\'")
  (leaf go-autocomplete
    :ensure t))

(leaf php-mode
  :ensure t)

(provide 'init)

(leaf web-mode
  :ensure t
  :hook (web-mode-hook . (lambda ()
                          (set (make-local-variable 'whitespace-style) '()))))
;;; init.el ends here
