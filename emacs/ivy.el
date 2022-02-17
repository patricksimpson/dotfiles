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
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(setq counsel-mode t)
(setq ivy-warp t)
(setq ivy-height 6)
(setq ivy-use-selectable-prompt t)
