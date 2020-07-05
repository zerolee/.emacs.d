;;; smart-compile.el --- an interface to `compile'

;; Copyright (C) 1998-2020  by Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: 20200619
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Compatibility: Emacs 21 or later
;; URL(en): https://github.com/zenitani/elisp/blob/master/smart-compile.el
;; URL(jp): http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#smart-compile

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides `smart-compile' function.
;; You can associate a particular file with a particular compile function,
;; by editing `smart-compile-alist'.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'smart-compile)
;;
;; Note that it requires emacs 21 or later.

;;; Code:

(defgroup smart-compile nil
  "An interface to `compile'."
  :group 'processes
  :prefix "smart-compile")

(defcustom smart-compile-alist '(
                                 (emacs-lisp-mode    . (emacs-lisp-byte-compile))
                                 (html-mode          . (browse-url-of-buffer))
                                 (nxhtml-mode        . (browse-url-of-buffer))
                                 (html-helper-mode   . (browse-url-of-buffer))
                                 (octave-mode        . (run-octave))
                                 ("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
                                 ;;  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n && ./%n") ;; unix, macOS
                                 ;;  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n && %n") ;; win
                                 ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
                                 ("\\.[Ss]\\'"       . "as  %f")
                                 ("\\.cs\\'"         . "mcs %f")
                                 ("\\.cron\\(tab\\)?\\'" . "crontab %f")
                                 ("\\.cu\\'"         . "nvcc %f -o %n")
                                 ("\\.cuf\\'"        . "pgfortran %f -o %n")
                                 ("\\.[Ff]\\'"       . "gfortran %f -o %n")
                                 ("\\.[Ff]90\\'"     . "gfortran %f -o %n")
                                 ("\\.go\\'"         . "go run %f")
                                 ("\\.hs\\'"         . "ghc %f -o %n")
                                 ("\\.java\\'"       . "javac -Xlint:deprecation -Xlint:fallthrough %f")
                                 ("\\.jl\\'"         . "julia %f")
                                 ("\\.js\\'"         . "js %f")
                                 ("\\.lua\\'"        . "lua %f")
                                 ("\\.m\\'"          . "gcc -O2 %f -lobjc -lpthread -o %n")
                                 ("\\.mp\\'"         . "mptopdf %f")
                                 ("\\.php\\'"        . "php -l %f")
                                 ("\\.pl\\'"         . "perl %f")
                                 ("\\.p[l]?6\\'"     . "perl6 %f")
                                 ("\\.py\\'"         . "python3 %f")
                                 ("\\.raku\\'"       . "perl6 %f")
                                 ("\\.rb\\'"         . "ruby %f")
                                 ("\\.rs\\'"         . "rustc %f -o %n")
                                 ("Rakefile\\'"      . "rake")
                                 ("Gemfile\\'"       . "bundle install")
                                 ("\\.tex\\'"        . (tex-file))
                                 ("\\.texi\\'"       . "makeinfo %f")
                                 ;;  ("\\.pl\\'"         . "perl -cw %f") ; syntax check
                                 ;;  ("\\.rb\\'"         . "ruby -cw %f") ; syntax check
                                 )  "Alist of filename patterns vs corresponding format control strings.
Each element looks like (REGEXP . STRING) or (MAJOR-MODE . STRING).
Visiting a file whose name matches REGEXP specifies STRING as the
format control string.  Instead of REGEXP, MAJOR-MODE can also be used.
The compilation command will be generated from STRING.
The following %-sequences will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extension  ( netscape )
  %e  extension of file name       ( bin )

  %o  value of `smart-compile-option-string'  ( \"user-defined\" ).

If the second item of the alist element is an emacs-lisp FUNCTION,
evaluate FUNCTION instead of running a compilation command.
"
                                    :type '(repeat
                                            (cons
                                             (choice
                                              (regexp :tag "Filename pattern")
                                              (function :tag "Major-mode"))
                                             (choice
                                              (string :tag "Compilation command")
                                              (sexp :tag "Lisp expression"))))
                                    :group 'smart-compile)
(put 'smart-compile-alist 'risky-local-variable t)

(defconst smart-compile-replace-alist '(
                                        ("%F" . (buffer-file-name))
                                        ("%f" . (file-name-nondirectory (buffer-file-name)))
                                        ("%n" . (file-name-sans-extension
                                                 (file-name-nondirectory (buffer-file-name))))
                                        ("%e" . (or (file-name-extension (buffer-file-name)) ""))
                                        ("%o" . smart-compile-option-string)
                                        ;;   ("%U" . (user-login-name))
                                        )
  "Alist of %-sequences for format control strings in `smart-compile-alist'.")
(put 'smart-compile-replace-alist 'risky-local-variable t)

(defvar smart-compile-check-makefile t)
(make-variable-buffer-local 'smart-compile-check-makefile)

(defcustom smart-compile-make-program "make "
  "The command by which to invoke the make program."
  :type 'string
  :group 'smart-compile)

(defcustom smart-compile-option-string ""
  "The option string that replaces %o.  The default is empty."
  :type 'string
  :group 'smart-compile)


;;;###autoload
(defun smart-compile (&optional arg)
  "An interface to `compile'.
It calls `compile' or other compile function,
which is defined in `smart-compile-alist'."
  (interactive "p")
  (let ((name (buffer-file-name))
        (not-yet t))
    (cond
     ;; local command
     ;; The prefix 4 (C-u M-x smart-compile) skips this section
     ;; in order to re-generate the compile-command
     ((and (not (= arg 4)) ; C-u M-x smart-compile
           (local-variable-p 'compile-command)
           compile-command)
      (call-interactively 'compile)
      (setq not-yet nil))

     ;; make?
     ((and smart-compile-check-makefile
           (or (file-readable-p "Makefile")
               (file-readable-p "makefile")))
      (if (y-or-n-p "Makefile is found.  Try 'make'? ")
          (progn
            (set (make-local-variable 'compile-command) "make ")
            (call-interactively 'compile)
            (setq not-yet nil))
        (setq smart-compile-check-makefile nil))))
    ;; end of (cond ...)

    (if (not name)(error "cannot get filename."))
    ;;     (message (number-to-string arg))

    ;; compile
    (let((alist smart-compile-alist)
         (case-fold-search nil)
         (function nil))
      (while (and alist not-yet)
        (if (or
             (and (symbolp (caar alist))
                  (eq (caar alist) major-mode))
             (and (stringp (caar alist))
                  (string-match (caar alist) name)))
            (progn
              (setq function (cdar alist))
              (if (listp function)
                  (eval function)
                (set (make-local-variable 'compile-command)
                     (smart-compile-string function))
                (call-interactively 'compile))
              (setq alist nil)
              (setq not-yet nil))
          (setq alist (cdr alist)))))

    ;; If compile-command is not defined and the contents begins with "#!",
    ;; set compile-command to filename.
    (if (and not-yet
             (not (memq system-type '(windows-nt ms-dos)))
             (not (string-match "/\\.[^/]+$" name))
             (not
              (and (local-variable-p 'compile-command)
                   compile-command)))
        (save-restriction
          (widen)
          (if (equal "#!" (buffer-substring 1 (min 3 (point-max))))
              (set (make-local-variable 'compile-command) name))))

    ;; compile
    (and not-yet (call-interactively 'compile))))

(defun smart-compile-string (format-string)
  "Document forthcoming..."
  (if (and (boundp 'buffer-file-name)
           (stringp buffer-file-name))
      (let ((rlist smart-compile-replace-alist)
            (case-fold-search nil))
        (while rlist
          (while (string-match (caar rlist) format-string)
            (setq format-string
                  (replace-match
                   (eval (cdar rlist)) t nil format-string)))
          (setq rlist (cdr rlist)))))
  format-string)

(provide 'smart-compile)

;;; smart-compile.el ends here
