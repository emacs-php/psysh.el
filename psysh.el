;;; psysh.el --- PsySH, PHP interactive shell (REPL) -*- lexical-binding: t -*-

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: process, php

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Setup:

;; A: The easy way
;; $ wget psysh.org/psysh
;; $ chmod +x psysh
;; And copy or make symlink to your $PATH dir.

;; B: The other easy way
;; Get Composer.  See https://getcomposer.org/download/
;; $ composer g require psy/psysh:@stable

;;; Code:
(require 'comint)

(defvar psysh-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode psysh-mode comint-mode "PsySH"
  "Major-mode for PsySH REPL."
  (setq-local comint-process-echoes t))

(defun psysh--make-process (name program)
  "Make a Comint process NAME in BUFFER, running PROGRAM."
  (apply 'make-comint name program '()))

;;;###autoload
(defun psysh ()
  "Run PsySH interactive shell."
  (interactive)
  (switch-to-buffer (psysh--make-process "PsySH" "psysh"))
  (psysh-mode))

(provide 'psysh)
;;; psysh.el ends here
