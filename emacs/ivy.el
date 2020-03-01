(use-package ivy
  :ensure t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")))


;;; Ivy mini buffer bindings
(define-key ivy-minibuffer-map (kbd "C-n") #'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-p") #'ivy-previous-line)
;;; Ivy mini buffer vim bindings
(define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
(define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-l") #'ivy-alt-done)

(setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(setq counsel-mode t)

(use-package ivy-rich
  :ensure t)

(ivy-rich-mode t)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)
(defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
    (get-buffer candidate)
    (let ((icon (all-the-icons-icon-for-mode major-mode)))
    (if (symbolp icon)
        (all-the-icons-icon-for-mode 'fundamental-mode)
        icon))))

(setq ivy-rich--display-transformers-list
'(counsel-switch-buffer
  (:columns
   ((ivy-rich-candidate (:width 30))  ; return the candidate itself
    (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
    (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
   :predicate
   (lambda (cand) (get-buffer cand)))
  counsel-M-x
  (:columns
   ((counsel-M-x-transformer (:width 40))  ; thr original transformer
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
  counsel-describe-function
  (:columns
   ((counsel-describe-function-transformer (:width 40))  ; the original transformer
    (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
  counsel-describe-variable
  (:columns
   ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
    (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
  counsel-recentf
  (:columns
   ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
    (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))) ; return the last modified time of the file

