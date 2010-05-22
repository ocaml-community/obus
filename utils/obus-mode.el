;; obus-mode.el
;; ------------
;; Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
;; Licence   : BSD3
;;
;; This file is a part of obus, an ocaml implementation of D-Bus.

(require 'tuareg)

(defconst obus-keywords '("interface" "method" "signal"
                          "property.r" "property.w" "property.rw"
                          "annotation" "with")
  "List of keywords for the obus-mode")

(defvar obus-file nil
  "Whether the current buffer is an obus idl file")

(defun obus-tuareg-mode-hook ()
  "Setup the tuareg mode for obus idl files"
  (if obus-file
      (progn
        (setq obus-file nil)
        (make-local-variable 'tuareg-governing-phrase-regexp)
        (make-local-variable 'tuareg-keyword-alist)
        (make-local-variable 'tuareg-font-lock-keywords)

        (setq tuareg-governing-phrase-regexp
              (regexp-opt
               '("interface" "method" "signal"
                 "property.r" "property.w" "property.rw"
                 "annotation")
                 `words))
        (setq tuareg-keyword-alist (mapcar (lambda (x) (cons x 2)) obus-keywords))

        (setq tuareg-font-lock-keywords
              (list
               (list "[(){}:*]\\|->"
                     0 'font-lock-keyword-face nil nil)
               (list (concat (regexp-opt '("method" "signal" "property.r" "property.w" "property.rw") `words) "[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)")
                     2 'font-lock-function-name-face 'keep nil)
               (list "\\<interface\\>[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\([.][A-Za-z0-9_]+\\)+\\)[ \t\n]*{"
                     1 'font-lock-constant-face 'keep nil)
               (list "\\([A-Za-z0-9_.]+\\)[ \t\n]*\\(=\\|:\\)"
                     1 'font-lock-variable-name-face 'keep nil)
               (list ":[ \t\n]*\\(\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)"
                     1 'font-lock-type-face 'keep nil)
               (list (regexp-opt obus-keywords `words)
                     0 'font-lock-keyword-face nil nil))))))

(add-hook 'tuareg-mode-hook 'obus-tuareg-mode-hook)

;;;###autoload (add-to-list 'auto-mode-alist '("\\.obus\\'" . obus-mode))

;;;###autoload
(defun obus-mode ()
  "Major mode for editing obus idl files"
  (interactive)
  (print "toto")
  (setq obus-file t)
  (tuareg-mode))

(provide 'obus-mode)
