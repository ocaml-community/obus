#!/bin/bash

# htmlize.sh
# ----------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3

if [ -n "$OBUS_COLORED_MANUAL" ]; then
    exec emacs -fg "#5fbf77" -bg "#323232" --eval "
(progn
  (set-default-font \"Monospace\")
  (with-current-buffer (find-file \"$1\")
    (hl-line-mode nil)
    (with-current-buffer (htmlize-buffer)
      (write-region (point-min) (point-max) \"$2\")))
  (kill-emacs))
"
else
    exec emacs -fg black -bg lightgrey --eval "
(progn
  (set-default-font \"Monospace\")
  (with-current-buffer (find-file \"$1\")
    (hl-line-mode nil)
    (font-lock-mode)
    (with-current-buffer (htmlize-buffer)
      (write-region (point-min) (point-max) \"$2\")))
  (kill-emacs))
"
fi
