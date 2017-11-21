;;; ivy-window-configuration.el --- An ivy/counsel interface for managing and saving window-configurations.
;; -*- lexical-binding: t; -*-

;; Adam Simpson <adam@adamsimpson.net>
;; Version: 0.0.3
;; Package-Requires: (ivy hydra)
;; Keywords: ivy, window, window-configuration
;; URL: https://github.com/asimpson/ivy-window-configuration/

;;; Commentary:
;; ivy-window-configuration offers a more robust solution to managing window-configurations than registers.  I was inspired by ivy-switch-view.
;; However ivy-switch-view has a different design as stated in this issue thread: https://github.com/abo-abo/swiper/pull/587#issuecomment-233167085.
;; ivy-window-configuration comes with a hydra: ivy-window-configuration--hydra/body which exposes all the functionality in a single screen.
;; ivy-window-configuration also works via a standard ivy/counsel interface with deletes bound to M-o d and M-o D respectively.

;;; Code:
(require 'ivy)
(require 'hydra)
(require 'seq)

(defvar ivy-window-configuration--views
  nil
  "Storage for window configs.")

(defun ivy-window-configuration--save()
  "Retrieves all buffer names from current window and adds that along with the window-configuration
to ivy-window-configuration--views which is an alist data structure."
  (interactive)
  (let (config names)
    (setq config (current-window-configuration))
    (setq names (mapconcat
                'identity
                (mapcar
                  (lambda(win)
                    (buffer-name (window-buffer win)))
                  (window-list))
                " "))
    (push (cons names config) ivy-window-configuration--views)
    (neotree-hide)
    (delete-other-windows)))

(defun ivy-window-configuration--restore(target)
  "Given a string that matches a key in our data structure restore a window configuration."
  (interactive)
    (set-window-configuration (cdr (assoc target ivy-window-configuration--views))))

(defun ivy-window-configuration--pop()
  "Go to the most recently stored view."
  (interactive) (set-window-configuration (cdr (car ivy-window-configuration--views))))

(defun ivy-window-configuration--list()
  "Generate a list of the stored views for ivy."
  (mapcar (lambda(view)
            (car view))
          ivy-window-configuration--views))

;;;###autoload
(defun ivy-window-configuration(&optional should-delete)
  "Present an ivy/counsel interface to the stored view alist.
Takes an optional boolean which can initiate a delete."
  (interactive)
  (if ivy-window-configuration--views
    (ivy-read
    "Choose view: "
    (ivy-window-configuration--list)
    :action (lambda(view)
              (if should-delete
                  (ivy-window-configuration--delete view)
                (ivy-window-configuration--restore view))))
    (message "No saved views.")))

(ivy-set-actions
 'ivy-window-configuration
 '(
   ("d" ivy-window-configuration--delete "remove from view list")
   ("D" ivy-window-configuration--delete-all "remove all views from view list")))

(defun ivy-window-configuration--delete-all(&optional selection)
  "Wipe out saved views. The optional paramter is ignored and only serves to allow ivy to call this function."
  (interactive)
  (setq ivy-window-configuration--views nil))

(defun ivy-window-configuration--delete-alt()
  "Alternative delete function when done outside ivy/counsel."
  (interactive)
  (ivy-window-configuration t))

(defun ivy-window-configuration--delete(view)
  "Given a view removes that view from the alist of stored views."
  (setq ivy-window-configuration--views (seq-filter
                                         (lambda(x)
                                           (not (string= view (car x))))
                                         ivy-window-configuration--views)))
;;;###autoload
(defhydra ivy-window-configuration--hydra(:exit t)
  "
  Views:
  _p_: Pop most recent view
  _s_: Select view
  _a_: Store new view
  _D_: Delete all views
  _d_: Delete specific view
  "

  ("p" ivy-window-configuration--pop)
  ("s" ivy-window-configuration)
  ("a" ivy-window-configuration--save)
  ("D" ivy-window-configuration--delete-all)
  ("d" ivy-window-configuration--delete-alt))

(provide 'ivy-window-configuration)

;;; ivy-window-configuration.el ends here
