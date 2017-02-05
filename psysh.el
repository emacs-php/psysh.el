;;; psysh.el --- PsySH, PHP interactive shell (REPL) -*- lexical-binding: t -*-

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 22 Jan 2016
;; Version: 0.0.3
;; Package-Requires: ((emacs "24.3"))
;; Keywords: processes php
;; URL: https://github.com/zonuexe/psysh.el

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

;; ## Installation
;;
;; ### A: The easy way
;;
;;     $ wget psysh.org/psysh
;;     $ chmod +x psysh
;;
;; And copy or make symlink to your $PATH dir.
;;
;;
;; ### B: The other easy way
;;
;; Get Composer.  See https://getcomposer.org/download/
;;
;;     $ composer g require psy/psysh:@stable
;;
;;
;; ### C: Project local REPL
;;
;; Set `psysh-comint-buffer-process' (buffer local variable).
;;
;;     (setq psysh-comint-buffer-process "path/to/shell.php")
;;
;; `shell.php' is for example:
;;
;;     #!/usr/bin/env php
;;     <?php
;;     // ↓Namespace for your project
;;     namespace Nyaan;
;;     
;;     // load Composer autoload file
;;     require_once __DIR__ . '/vendor/autoload.php';
;;     // load other initialize PHP files
;;     // require_once …
;;     
;;     echo __NAMESPACE__ . " shell\n";
;;     
;;     $sh = new \Psy\Shell();
;;     
;;     // Set project namespace in REPL
;;     if (defined('__NAMESPACE__') && __NAMESPACE__ !== '') {
;;         $sh->addCode(sprintf("namespace %s;", __NAMESPACE__));
;;     }
;;     
;;     $sh->run();
;;     
;;     // Termination message
;;     echo "Bye.\n";
;;
;; See also http://cho-phper.hateblo.jp/entry/2015/11/10/031000 *(Japanese)*
;;

;;; Code:
(require 'comint)


(defgroup psysh nil
  "PsySH"
  :tag "PsySH"
  :group 'php
  :group 'tools)

;; PsySH REPL Mode functions
(defvar psysh-mode-map
  (let ((map (make-sparse-keymap)))
    ;;
    map))

(define-derived-mode psysh-mode comint-mode "PsySH"
  "Major-mode for PsySH REPL."
  (setq-local comint-process-echoes t))

(defvar psysh-comint-buffer-process
  nil
  "A list (buffer-name process) is arguments for `make-comint'.")
(make-variable-buffer-local 'psysh-comint-buffer-process)

(defvar psysh-mode-hook nil
  "List of functions to be executed on entry to `psysh-mode'.")

(defconst psysh--re-prompt "^>>> ")

(defun psysh--detect-buffer ()
  "Return tuple list, comint buffer name and program."
  (or psysh-comint-buffer-process
      '("psysh" "psysh")))

(defun psysh--make-process ()
  "Make a Comint process NAME in BUFFER, running PROGRAM."
  (apply 'make-comint (psysh--detect-buffer)))

(defun psysh--copy-variables-from-php-mode ()
  "Set ac-sources from php-mode."
  (when (fboundp 'php-mode)
    (let ((current-major-mode major-mode)
          (php-mode-ac-sources nil))
      (with-temp-buffer
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
          (funcall current-major-mode))))))

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

(defun psysh-restart ()
  "Restart PsySH process."
  (interactive)
  (when (eq major-mode 'psysh-mode)
    (delete-process (get-buffer-process (current-buffer)))
    (psysh)))


;; PsySH Doc Mode functions
(defvar psysh-doc-buffer-name "*psysh doc*")

(defcustom psysh-doc-buffer-color 'auto
  "Coloring PsySH buffer."
  :type '(choice (const :tag "Auto detect color mode." 'auto)
                 (const :tag "Use only PsySH original coloring." 'only-psysh)
                 (const :tag "Use only Emacs font-lock coloring." 'only-emacs)
                 (const :tag "Use multiple coloring mechanism." 'mixed)
                 (const :tag "No coloring." 'none)))

(defcustom psysh-doc-display-function #'view-buffer-other-window
  "Function to display PsySH doc buffer."
  :type '(function))

;;;###autoload
(defun psysh-doc-buffer (target &optional buf)
  "Execute PsySH Doc `TARGET' and Return PsySH buffer `BUF'."
  (if (null buf) (setq buf (get-buffer-create psysh-doc-buffer-name)))
  (with-current-buffer buf
    (read-only-mode -1)
    (erase-buffer)
    (insert "doc " target)
    (message "%s %s" (nth 1 (psysh--detect-buffer)) (buffer-substring (point-min) (point-max)))
    (apply 'call-process-region (point-min) (point-max) (nth 1 (psysh--detect-buffer)) t t nil
           (if (memq psysh-doc-buffer-color '(none only-emacs))
               '("--no-color")
             '("--color")))
    (unless (memq psysh-doc-buffer-color '(none only-emacs))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))
    (when (search-forward-regexp psysh--re-prompt)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (search-backward-regexp (concat psysh--re-prompt "$"))
      (delete-region (match-beginning 0) (point-max)))
    (goto-char (point-min))
    (unless (eq major-mode 'psysh-doc-mode)
      (psysh-doc-mode)))
  buf)

;;;###autoload
(define-derived-mode psysh-doc-mode prog-mode "PsySH-doc"
  "Major mode for viewing PsySH Doc."
  (setq show-trailing-whitespace nil)
  (read-only-mode +1))

;;;###autoload
(defun psysh-doc-string (target)
  "Return string of PsySH Doc `TARGET'."
  (let ((psysh-doc-buffer-color nil))
    (with-current-buffer (psysh-doc-buffer target (current-buffer))
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun psysh-doc (target)
  "Display PsySH doc `TARGET'."
  (interactive
   (list (read-string
          "Input class or function name: "
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            ""))))
  (funcall psysh-doc-display-function (psysh-doc-buffer target)))


;; PsySH Command

;;;###autoload
(defun psysh ()
  "Run PsySH interactive shell."
  (interactive)
  (switch-to-buffer (psysh--make-process))

  (set (make-local-variable 'psysh-enable-eldoc) nil)
  (psysh--copy-variables-from-php-mode)

  (when (and (boundp 'psysh-enable-eldoc) psysh-enable-eldoc)
    (add-hook 'psysh-mode-hook #'psysh--enable-eldoc))

  (psysh-mode)
  (run-hooks 'psysh-mode-hook))
(put 'psysh 'interactive-only 'psysh-run)

;;;###autoload
(defun psysh-run (buffer-name process)
  "Run PsySH interactive-shell in `BUFFER-NAME' and `PROCESS'."
  (let ((psysh-comint-buffer-process (list buffer-name process)))
    (call-interactively 'psysh)))

(provide 'psysh)
;;; psysh.el ends here
