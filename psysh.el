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
    ;;
    map))

(define-derived-mode psysh-mode comint-mode "PsySH"
  "Major-mode for PsySH REPL."
  (setq-local comint-process-echoes t))

(defun psysh--detect-buffer ()
  "Return tuple list, comint buffer name and program."
  '("PsySH" "psysh"))

(defun psysh--make-process ()
  "Make a Comint process NAME in BUFFER, running PROGRAM."
  (apply 'make-comint (psysh--detect-buffer)))

(defun psysh--copy-variables-from-php-mode ()
  "Set ac-sources from php-mode."
  (when (fboundp 'php-mode)
    (let ((current-major-mode major-mode)
          (php-mode-ac-sources nil))

      (php-mode)

      (when (boundp 'psysh-enable-eldoc)
        (setq psysh-enable-eldoc (and (boundp 'eldoc-mode) eldoc-mode)))

      (if (and (boundp 'auto-complete-mode)
               auto-complete-mode
               (boundp 'ac-sources))
          (progn
            (setq php-mode-ac-sources ac-sources)
            (funcall current-major-mode)
            (setq ac-sources (append ac-sources php-mode-ac-sources)))
        (funcall current-major-mode)))))

(defun psysh--enable-eldoc ()
  "Turn on php-eldoc."
  (when (fboundp 'php-eldoc-enable)
    (php-eldoc-enable)
    (eldoc-mode 1)))

(defun psysh-eval-region (begin end)
  "Evalute PHP code BEGIN to END."
  (interactive "r")
  (let ((buf (psysh--make-process)))
    (comint-send-region buf begin end)))

;;;###autoload
(defun psysh ()
  "Run PsySH interactive shell."
  (interactive)
  (switch-to-buffer (psysh--make-process))

  (make-local-variable 'psysh-enable-eldoc)
  (psysh--copy-variables-from-php-mode)

  (when (and (boundp 'psysh-enable-eldoc) psysh-enable-eldoc)
    (add-hook 'psysh-mode-hook #'psysh--enable-eldoc))

  (psysh-mode))

(provide 'psysh)
;;; psysh.el ends here
