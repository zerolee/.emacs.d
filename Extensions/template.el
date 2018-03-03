;;; template.el --- commenting, (auto-)updating, file templates

;; Copyright (C) 1995-1999 Christoph Wedler
;;
;; Author: Christoph Wedler <wedler@fmi.uni-passau.de>
;; Version: $Id: template.el,v 1.1 2001/02/01 20:15:58 lasse Exp $
;; Keywords: templates, comments, updating, data, tools
;; Requires: XEmacs-20.2+ (XEmacs-19.14+ & custom cmds), Emacs-20.2+
;; X-URL: http://www.fmi.uni-passau.de/~wedler/template/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Supports writing standardized comments, (auto-)updating parts of the buffer,
;; e.g., time stamps or file names in headers, using templates when creating a
;; new file and creating templates.  A template is a file with normal text,
;; pre-defined "expansion forms" like inserting (parts of) the file name,
;; date/time, user name etc (see below for more), setting point and register
;; positions, and "expansion forms" which are defined by elisp sexps at the end
;; of the template.  Some examples and default templates are distributed with
;; this package.  See file 00readme.txt of the distribution for details.

;; Bug fixes, bug reports, improvements, and suggestions for the NEWEST VERSION
;; are strongly appreciated.  Please check the newest version first:
;;   http://www.fmi.uni-passau.de/~wedler/template/changes.html

;;; Overview, comments: ======================================================

;; \\[template-single-comment] fills the current comment line with specific
;; characters up to a specific column (`template-max-column' is 78).
;; \\[template-block-comment] surrounds the current comment block by delimiter
;; and empty lines.  If the current line is empty, go to the previous non-empty
;; line before.  The comment style is determined by
;; `template-comment-specification-alist' and either by
;;  * the prefix argument,
;;  * the old comment style when present
;;  * specified by `template-comment-specification-special' if non-nil, or
;;  * if `comment-start' is a string of length 1: how often is it repeated,
;;    otherwise: does the comment starts at the beginning of the line or not.

;;; Overview, updating: ======================================================

;; \\[template-update-buffer] updates the buffer contents according to
;; `template-update-buffer-alist'.  By default, it updates the file header (see
;; below), and the date inside <address> in HTML buffers with user confirmation
;; (only if there is a change, of course).  If `template-auto-update' is
;; non-nil (default), this command is automatically executed when saving the
;; buffer.

;; \\[template-update-header] updates the file name in the header as specified
;; in `template-header-regexp-alist'.  This command is automatically executed
;; at the end of \\[template-new-file] and when saving the buffer.

;;; Overview, templates: =====================================================

;; \\[template-new-file] asks the user of the name for a new file and uses a
;; file template for its initial contents.  The steps:
;;  * Template derivation: this package provides sophisticated methods for
;;    suggesting a reasonable template file to the user.
;;  * File name refinement: e.g., if the given file name is "exercise" and
;;    there are two files "exercise1.tex" and "exercise2.tex" in the same
;;    directory and if we have a template "exercise.tex.tpl", the file name is
;;    refined to "exercise3.tex".  This is turned off when
;;    \\[template-new-file] is called with a prefix argument.
;;  * Template insertion: insert the template file into the empty buffer.
;;  * Expansion: expand the expansion forms (text matched by a specific
;;    regexp).  There are pre-defined and per-template expansion forms.
;;    Some expansion forms require user interaction (the buffer is set
;;    as modified in this case).  Some expansion forms (elisp commands
;;    and expressions) are considered to be insecure (requires user
;;    confirmation).
;;  * Report: display a temporary message at point and in the minibuffer area.

;; If `template-auto-insert' is non-nil (default), C-x C-f also uses a template
;; if the provided name corresponds to a non-existent file.

;; Any file name FULL consists of the directory part DIR and the non-directory
;; part FILE.  FILE consists of its extension EXT, RAW and a numbering NUMBER
;; just in front of the extension.

;; Template derivation searches for the most specific readable template
;; file.  By default, template files with the same RAW part as the name
;; of the new file are considered to be more specific than files with
;; just the same EXT part.  Also, files in the same directory are
;; considered to be more specific than files in their parent directory
;; or any default template directory.

;; Thus, we search for the first readable file with extension ".tpl" (value of
;; `template-extension') which is found by the following algorithm:
;;   forall FORMs in `template-derivation-alist' do
;;     forall directories BASEs from the directory of the given file name up to
;;	      the home directory (`template-home-directory' is "~/") do
;;       forall subdirectories DIRs in "./" and "Templates/ (list value of
;;		`template-subdirectories') relative to BASE do
;;         forall TEMPLATEs in DIR do check_form FORM
;;     forall directories DIRs in "~/lib/templates/" (list value of
;;	      `template-default-directories') do
;;       forall TEMPLATEs in DIR do check_form FORM

;; check_form may check whether the template file name without ".tpl" is
;; matched by some regexp, whether RAW, NUMBER and/or EXT of it is equal to the
;; corresponding part of the given file name, or whether it passes any
;; predicate, see `template-derivation-alist'.  The chosen FORM also determines
;; the file name refinement.

;; After inserting the template into the empty buffer, the per-template
;; expansion definitions are read and the corresponding region is deleted, see
;; `template-definition-start'.  Then, all strings in the template matched by
;; `template-expansion-regexp' are expanded.  The expansion can be customized
;; by setting `template-expansion-alist' or on a per-template basis mentioned
;; above.  Predefined expansions are (there are one-letter aliases):
;;  * Set point "(>>>POINT<<<)", mark "(>>>MARK<<<)", registers to current
;;    position "(>>>0<<<)" to "(>>>9<<<)", see `template-register-regexp'.
;;  * Insert file name and parts of it "(>>>DIR<<<"), "(>>>FILE<<<)",
;;    "(>>>FILE_RAW<<<)", "(>>>FILE_NUM<<<)" "(>>>FILE_EXT<<<)" and
;;    "(>>>FILE_UPCASE<<<)".
;;  * Insert user and system name "(>>>AUTHOR<<<)", "(>>>USER_NAME<<<"),
;;    "(>>>LOGIN_NAME<<<)", "(>>>HOST_ADDR<<<"), UTC date/time for VC
;;    "(>>>VC_DATE<<<)", ISO-8601 date "(>>>ISO_DATE<<<)", year "(>>>YEAR<<<)",
;;    date/time "(>>>DATE<<<)", see `template-time-format'.
;;  * Ask for the initial comment "(>>>COMMENT<<<)" or other text parts.

;; It is also possible to set registers to some contents and define a temporary
;; message which disappears with the first user event or after 10 min
;; (`template-message-timeout' is 600).  Local variables which could be useful
;; for the expansion can also be set.  Any emacs-lisp command and any
;; expression can be used to determine the expansion.  Because this is a
;; security hole, the user will be asked first (`template-confirm-insecure' is
;; t).  Specified local variables of the file are processed at the end of the
;; expansion, see also `enable-local-variables' and `enable-local-eval').

;;; Key bindings, Menus: =====================================================

;; With the default installation (see `template-initialize'), this package adds
;; the following menus and menu entry to the menubar of XEmacs:
;;  * "File >> New File Using Template..." for \\[template-new-file],
;;  * "Edit >> Comments" (when buffer is writable) for some comment commands,
;;  * "Edit >> Template Creation" for commands useful for template creation,
;;    i.e., \\[template-open-template], \\[template-define-prompt],
;;    \\[template-define-message], \\[template-define-register], and
;;    \\[template-insert-form] with all predefined expansion forms.

;; The default installation also adds the following key bindings:
;;  * "C-x t" runs \\[template-new-file] (also with Emacs),
;;  * "C-x C-=" runs \\[template-single-comment],
;;  * "C-x C-;" runs \\[template-block-comment].

;;; Related packages: ========================================================

;; Terminology:
;;  * File template: a template is defined by a file [used here].
;;  * Command template: a template is defined by a command (in your ~/.emacs).

;; Packages which provide template support:
;;  * XEMACS/packages/autoinsert: A global alist determines which template to
;;    use use when visiting an empty file.  A template is a mixture of files to
;;    insert, elisp actions and command templates of package "skeleton".
;;  * XEMACS/utils/skeleton: Use command templates.
;;  * XEMACS/psgml/tempo: Use command templates.  Predefined expansions are
;;    similar to some predefined expansions of this package "template".  A
;;    specific template is used by `html-mode' when creating a new html file.
;;  * XEMACS/hm--html-menus/tmpl-minor-mode: Insert file templates.  No
;;    predefined expansions.
;;  * LCD/misc/auto-template: Insert file templates.  No predefined expansion.
;;    Non-trivial expansions requires a "substitution file".
;;  * LCD/misc/suffix-template: Insert file templates.  No predefined
;;    expansion.  Defining non-trivial expansions is tedious.

;; Packages which support commenting:
;;  * LCD/misc/comment: Commands for commenting out regions.
;;  * LCD/misc/tinycom: Commands for comments which might be useful in buffers
;;    having major modes which do not support commenting.

;; The main differences of this package "template" to the other packages
;; which provide template support:
;;  * Commands for commenting.
;;  * Sophisticated derivation which template to use, file name refinement.
;;  * Very flexible templates without any code in your ~/.emacs.

;;; Installation: ============================================================

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'template)
;;   (template-initialize)

;; To customize, use `M-x customize-group RET template RET' or the custom
;; menus (Emacs->Data->Template).

;; Use ffap-1.10 or higher to make \\[find-file-at-point] (command in ffap)
;; also use templates as last resort, see `template-find-file-commands'.  As of
;; January 1999, apply the following patch to ffap-1.9-gen:
;;   http://www.fmi.uni-passau.de/~wedler/template/ffap.el.diff

;; With this package, you probably don't want to the special template for html
;; buffers which comes with psgml-html/html-helper-mode and not use its special
;; timestamp updating:
;;   (setq html-helper-build-new-buffer nil)
;;   (setq html-helper-do-write-file-hooks nil)

;; The time formats for (>>>T<<<), an alias for (>>>DATE<<<), and (>>>Y<<<), an
;; alias for (>>>YEAR<<<), have changed with Version 2.2e.  If you prefer the
;; old ones (because you have used them in old templates), use:
;;   (setq template-date-format "%d %b %Y")
;;   (setq template-time-format "%T")

;;; Code:

(provide 'template)
(require 'cl)
(require 'custom)

(eval-and-compile
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (defalias 'template-directory-files 'directory-files)
    (defun template-directory-files (dirname &optional full match nosort
					     files-only)
      (let ((files (directory-files dirname full match nosort))
	    result)
	(if (null files-only)
	    files
	  (while files
	    (if (if (file-directory-p (car files))
		    (null (eq files-only t))
		  (eq files-only t))
		(push (car files) result))
	    (setq files (cdr files)))
	  (nreverse result)))))
  (or (fboundp 'characterp) (defalias 'characterp 'numberp))
  (or (fboundp 'point-at-bol)
      (defun point-at-bol (&optional arg) ; Only XEmacs has BUFFER
	(save-excursion
	  (beginning-of-line arg)
	  (point))))
  (or (fboundp 'point-at-eol)
      (defun point-at-eol (&optional arg) ; Only XEmacs has BUFFER
	(save-excursion
	  (end-of-line arg)
	  (point)))))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


(defconst template-version "2.4a"
  "Current version of package template.
Check <http://www.fmi.uni-passau.de/~wedler/template/changes.html> for
the newest.")


;;;===========================================================================
;;;  Customization and initialization
;;;===========================================================================

(defgroup template nil
  "Use vars, registers and file/buffer places across sessions."
  :group 'data
  :link '(emacs-commentary-link "template.el")
  :link '(url-link "http://www.fmi.uni-passau.de/~wedler/template/")
  :prefix "template-")

(defgroup template-comments nil
  "Comments in package template."
  :group 'template
  :prefix "template-")

(defgroup template-updating nil
  "(Auto-)Updating with package template."
  :group 'template
  :prefix "template-")

(defgroup template-derivation nil
  "Deriving templates for new files."
  :group 'template
  :prefix "template-")

(defgroup template-expansion nil
  "Expanding the expansion forms of templates."
  :group 'template
  :prefix "template-")

(defgroup template-miscellaneous nil
  "Miscellaneous configurations of package template."
  :group 'template
  :prefix "template-")

;; I could imagine that a future version of package custom could make this
;; `PACKAGE-initialize' stuff easier
(defcustom template-use-package nil
  "Pseudo variable.  Used to initialize template in custom buffer.
Put `(template-initialize)' into your ~/.emacs to initialize package
template in future sessions.  See variable `template-initialize'."
  :group 'template
  :type '(boolean :format "%{%t%}: %[(template-initialize)%], %v\n"
		  :on "in use" :off "not yet initialized"
		  :help-echo "Initialize package Template."
		  :action template-initialize))

(defcustom template-initialize t
  "Whether/what to initialize with `template-initialize'.
If t, do full initialization.  Otherwise, the value should be a list
with element.  To enable, include

 * `auto' to enable `template-auto-update' and `template-auto-insert',
 * `keys' to setup the default key bindings,
 * `menus' to setup the menus (XEmacs only, no effect with Emacs)."
  :group 'template-miscellaneous
  :type '(choice (const :tag "All" t)
		 (set :value (auto keys menus)
		      (const :tag "Auto Updating/Inserting" auto)
		      (const :tag "Setup Key Bindings" keys)
		      (const :tag "Setup Menus (XEmacs)" menus))))


;;;===========================================================================
;;;  Menu (XEmacs only)
;;;===========================================================================

;; easymenu only for top-level menus which would appear in popup menu :-(
(defvar template-menu
  '("Comments"
    :included (not buffer-read-only)
    ["Comment Single Line" template-single-comment t]
    ["Comment Block" template-block-comment
     :active (string= comment-end "")]
    "---"
    ["Indent for Comment" indent-for-comment
     :active comment-start]
    ["Continue Comment" indent-new-comment-line
     :active comment-start]
    ["Comment Region" comment-region
     :active (and comment-start (mark))]
    ["Comment Region 2" (comment-region 2)
     :active (and comment-start (mark))]
    ["Comment Region 3" (comment-region 3)
     :active (and comment-start (mark))]
    "---"
    ["Update Buffer" template-update-buffer t]
    ["Update Filename in Header" template-update-header
     :active (template-update-header t)])
  "Menu for comment functions.")

(defvar template-creation-menu
  '("Template Creation"
    :filter template-menu-filter
    ["Open Template" template-open-template
     :active (null (template-buffer-template-p))]
    "--"
    ["Define Prompt" template-define-prompt t]
    ["Define Message" template-define-message t]
    ["Define Register" template-define-register t]
    "---"
    ["Insert expansion form" template-insert-form t])
  "Menu for template creation.")


;;;===========================================================================
;;;  Commenting
;;;===========================================================================

(defcustom template-max-column 78
  "*Width of the separator line."
  :group 'template-comments
  :type 'integer)

(defcustom template-comment-specification-alist
  '(("-" "" "" 0)
    ("-" "" "" 0)
    ("=" "\n\n" "\n" 1)
    ("#" "\n\n\f\n" "\n\n" 2))
  "List of specifications for comment functions.
Each specification at LEVEL, starting at 1, is a list
  (SEPARATOR BEFORE-BLOCK AFTER-BLOCK DELETE-LINES)

SEPARATOR is the string which is inserted repeatedly by commands
\\[template-single-comment] and \\[template-block-comment] up to
`template-max-column'.

After that, \\[template-block-comment] deletes DELETE-LINES after the
comment block and inserts string AFTER-BLOCK at the end of the block and
BEFORE-BLOCK at the front of the block.

The specification LEVEL to use is determined by:
 (1) If the prefix argument is non-nil and its numeric value is > 0,
     this value is the LEVEL.
 (2) If the prefix argument is nil, and there is an old comment style,
     use old comment style.
 (3) If `template-comment-specification-special' is a function or the
     current major mode has a property with this name and its value is a
     function, this function returns the specification.
 (4) If `comment-start' is a string of length 1: LEVEL is number of
     repetitions of `comment-start' at the beginning of the line.
     Otherwise, if the correctly indented line starts at the beginning
     of the line, LEVEL=3, else LEVEL=2."
  :group 'template-comments
  :type '(repeat (group (string :tag "Separator" :value "-")
			(string :tag "Before block" :value "")
			(string :tag "After block" :value "")
			(integer :tag "Delete lines" :value 0))))

(defcustom template-comment-specification-special nil
  "Function used for special commenting styles or nil.
See `template-comment-specification-alist' for details."
  :group 'template-comments
  :type '(choice (const nil) function))


;;;===========================================================================
;;;  Auto updating
;;;===========================================================================

(defcustom template-auto-update 'query
  "*Whether to update parts of the file when saving the buffer.
When non-nil and `template-auto-update-disable-regexp' does not match
the file name, automatically updates parts of the buffer, see
`template-update-buffer-alist'.  With value t or if the entry in the
alist has no prompt, do not ask for confirmation.

You should have called `template-initialize' to enable this feature."
  :group 'template-updating
  :type '(radio (const :tag "No" nil)
		(const :tag "Without confirmation" t)
		(sexp :tag "With confirmation" :format "%t" :value query)))

(defcustom template-auto-update-disable-regexp nil
  "*Regexp matching files not to automatically update.
Value nil matches no file.  See `template-auto-update'."
  :group 'template-updating
  :type '(choice (const :tag "none" nil) regexp))

(defcustom template-update-buffer-alist
  '((t "Update header in %s? "
       (template-update-header t)
       (file-name-sans-versions (file-name-nondirectory buffer-file-name)))
    ((html-mode) "Update date inside <address> in %s? "
     (-2000
      "\\([0-9]+[ \t]+[A-Za-z][A-Za-z][A-Za-z][ \t]+[0-9]+\\)[ \t\n]*</address>"
      1)
     (format-time-string "%d %b %Y")))
  "Alist used how to update parts of the buffer.
Used by function `template-update-buffer'.  Elements look like
  (MODES-OR-REGEXP PROMPT TEST NEW REPLACEMENT-FUN)

Each element must \"pass\" MODES-OR-REGEXP.  If this is a list, it must
include the current major-mode, if this is a regexp, it must match the
`buffer-file-name' without version, otherwise it must be non-nil.

Then, TEST is `eval'd and must return the region = (BEG . END) to be
replaced.  If TEST is a list and the `car' of TEST is not a function,
`template-update-buffer-region' is used as the default function.

Then, NEW is `eval'd.  If it is a string, it is considered as
replacement for the region, otherwise REPLACE-FUN must be non-nil.

Then, ask user for confirmation with PROMPT where %s is substituted by
the buffer name if PROMPT is a string and `template-auto-update' is not
t.

Finally, REPLACEMENT-FUN is called the `eval'd NEW and the beginning and
the end of the region returned by TEST.  If REPLACEMENT-FUN is nil, just
replace the region by the `eval'd NEW."
  :group 'template-updating
  :type '(repeat (group (choice (repeat :tag "In major modes" :value nil
					function)
				(regexp :tag "Buffer matching" :value "")
				(sexp :tag "Always" :value t))
			(string :tag "Prompt" :value "Update in %s? ")
			(choice (list :tag "Default test"
				      (choice (const :tag "No limit" nil)
					      (integer :tag "Limit" -1000))
				      regexp
				      (integer :tag "Regexp group" :value 0))
				(sexp :tag "Eval sexp"))
			(sexp :tag "Eval New string")
			(option (function :tag "Replacement function")))))

(defcustom template-header-lines 3
  "*Last line number which is checked by \\[template-update-header]."
  :group 'template-updating
  :type 'integer)

(put 'template-header-lines 'template-secure-value #'integerp)

(defcustom template-header-regexp-alist
  '(("@(#)\\([^ \t\n]+\\)" . 1)
    ("^%s[ \t]*\\([^ \t\n]+\\)[ \t]+--" . 1))
  "Alist of regexps matching the file name in the header.
The `car' of each element is the REGEXP with %s, if present, substituted
by the comment start.  The `cdr' is the regexp group to be replaced.
Used by \\[template-update-header].

The comment start is evaluated from `comment-start', the first character
in the buffer or \"#\".  It is assumed that a non-alpha single character
comment start may be repeated.  For example, the substituted regexp in
`emacs-lisp-mode' is \"\;+\", in `c++-mode' \"//\"."
  :group 'template-updating
  :type '(repeat (cons :format "%v"
		       regexp
		       (integer :tag "Regexp group" :value 0))))


;;;===========================================================================
;;;  Templates: finding templates
;;;===========================================================================

(defcustom template-auto-insert t
  "*Whether to automatically use template files for new files.
Used if the user gave a non-existent file as argument to a command in
`template-find-file-commands'.  When non-nil, use `template-new-file'
for those file.  With value t, do not ask for confirmation.

You should have called `template-initialize' to enable this feature."
  :group 'template-derivation
  :type '(radio (const :tag "No" nil)
		(const :tag "Without confirmation" t)
		(sexp :tag "With confirmation" :format "%t" :value query)))

(defcustom template-find-file-commands
  '(find-file find-file-other-frame find-file-other-screen
	      find-file-other-window find-file-at-point)
  "*Commands which use templates as last resort, see `template-auto-insert'.
See also `template-file-select-commands'."
  :group 'template-derivation
  :type '(repeat (function :tag "Command")))

(defcustom template-file-select-commands
  '(exit-minibuffer minibuffer-complete-and-exit
		    list-mode-item-mouse-selected
		    list-mode-item-keyboard-selected)
  "*Commands which select the file name via minibuffer/completions.
Checked with commands in `template-find-file-commands'."
  :group 'template-derivation
  :type '(repeat (function :tag "Command")))

(defface template-message-face
  '((t (:bold t)))
  "Face for temporary message at point.  This only works with XEmacs."
  :group 'template-miscellaneous)

(defcustom template-extension ".tpl"
  "Extension used for template files."
  :group 'template-derivation
  :type 'string)

(defcustom template-subdirectories '("./" "Templates/")
  "*List of subdirectories for template files.
See `template-derivation-alist' for details."
  :group 'template-derivation
  :type '(repeat directory))

(defcustom template-home-directory (expand-file-name "~/")
  "*Home directory where the search for template files stops.
See `template-derivation-alist' for details."
  :group 'template-derivation
  :type 'directory)

(defcustom template-default-directories
  (list (expand-file-name "~/lib/templates/"))
  "*List of default directories for template files.
See `template-derivation-alist' for details."
  :group 'template-derivation
  :type '(repeat directory))

(defcustom template-derivation-alist
  '(;;(("00readme" "" ".txt" "\\`00") . ("00readme" "" ".txt"))
    ((t "" t))
    ((t nil null) . (nil nil t 1))
    (("TEMPLATE" "" t))
    (("DEFAULT" nil "")))
  "Alist for template file name derivation and file name refinement.
Template derivation searches for the most specific readable template
file.  By default, files with the same RAW part as the name of the new
file are considered to be more specific than files with just the same
EXT part.  Also files in the same directory are considered to be more
specific than files in their parent directory or any default template
directory.  This behavior can be changed by this alist.

Each FORM in this alist has the form (TEMPLATE . REFINEMENT).  If
TEMPLATE matches, we have found a valid template file and the
corresponding REFINEMENT is used for the file name refinement.

Before the derivation, the given file name is split into the directory
part DIR, the file name without directory FILE, and the raw part RAW of
FILE, the numbering NUM and the extension EXT.  The result is stored in
`template-file'.

TEMPLATE can have the form (FUNCTION ARG...).  If TEMPLATE matches,
FUNCTION, called with arguments ARGs, should return the split template
file name, see `template-split-filename'.

TEMPLATE can also have the form (T-RAW T-NUM T-EXT F-REGEXP) where all
elements are optional, i.e., have value nil as default.  For TEMPLATE to
match, all conditions T-RAW, T-NUM and T-EXT must be met and F-REGEXP,
if non-nil, should match FILE, the non-directory part of the given file
name.  If a condition is a string, the corresponding part of the
template file must be equal to it.  If t, the part must be equal to
RAW/NUM/EXT of the given file name.  If nil, any value will do it.  Any
other value acts like t when the part of the given file name is
non-empty, as nil otherwise.

REFINEMENT can have the form (FUNCTION ARG...).  FUNCTION, called with
the list of the split template filename and ARGs as arguments, should
set `template-file' if the file name should be refined.

REFINEMENT can also have the form (F-RAW F-NUM F-EXT AUTO-NUM) where all
elements are optional, i.e., have value nil as default.  If F-RAW, F-NUM
and F-EXT are non-nil, they change RAW/NUM/EXT of `template-file'.  A
string will be used as the new part.  If t, the corresponding part of
the template name will be used.

We will use auto numbering in the following two cases: if NUM is
non-empty and the file exists already, or if NUM is empty and AUTO-NUM
is non-nil.  Auto numbering looks at the file names in DIR to generate
the next unique number which is at least as high as NUM in the first
case and AUTO-NUM in the second.

Let us use parts of the default value as examples:

Use a template with the same RAW part of the given file name and the
same EXT part if provided, e.g., for \"exercise2\" use template
\"exercise.tex.tpl\".  Refine file name to use the extension of the
template file, also use auto numbering, e.g., if files \"exercise2.tex\"
and \"exercise3.tex\" exist, refine name to \"exercise4.tex\":
  ((t nil null) . (nil nil t 1))

For a file with extension EXT, use TEMPLATE.EXT:
  ((\"TEMPLATE\" \"\" t))

We could define: If the given file name starts with \"00\", use template
\"00readme.txt.tpl\".  Refine file name to \"00readme.txt\":
  ((\"00readme\" \"\" \".txt\" \"\\`00\") . (\"00readme\" \"\" \".txt\"))

Since more than one template file could meet this conditions, the
template derivation searches for first readable file with extension
`template-extension' which is found by the following algorithm:

  forall FORMs in `template-derivation-alist' do
    forall directories BASEs from DIR up to `template-home-directory' do
      forall subdirectories DIRs in `template-subdirectories' relative
	     to BASE do
        forall TEMPLATEs in DIR do check_form FORM
    forall directories DIRs in `template-default-directories' do
      forall TEMPLATEs in DIR do check_form FORM"
  :group 'template-derivation
  :type '(repeat (cons :format "%v"
		       (sexp :tag "Derivation" :value ("TEMPLATE" nil t))
		       (sexp :tag "Refinement" :value nil))))


;;;===========================================================================
;;;  Templates: expanding templates
;;;===========================================================================

(defcustom template-confirm-insecure t
  "*Non-nil means, ask whether to use insecure template expansions.
Only set this to nil if you ALWAYS check template files before using
it!"
  :group 'template-expansion
  :type 'boolean)

(put 'template-confirm-insecure 'risky-local-variable t)

(defcustom template-message-timeout 600
  "*Maximum duration the temporary message will be displayed at point.
Any user event will also make the temporary message disappear.  The
temporary message uses face in `template-message-face'."
  :group 'template-miscellaneous
  :type 'integer)

(put 'template-message-timeout 'template-secure-value #'integerp)

(defvar template-date-format "%Y"
  "Obsolete time format used with the expansion form (>>>YEAR<<<).
See `template-default-expansion-alist' and `format-time-string'.  Its
default value is \"%Y\", it was \"%d %b %Y\" before template,v 2.2e.")

(if (fboundp 'make-obsolete-variable)
    (make-obsolete-variable 'template-date-format "\
use expansion form (>>>T<<<) with `template-time-format' instead"))

(defcustom template-time-format "%d %b %Y"
  "*Time format used with the expansion form (>>>DATE<<<).
See `template-default-expansion-alist' and `format-time-string'.  Also
used as default format in `template-insert-time'.  Rather than setting
this variable in a template, define an per-template expansion form, see
`template-definition-start'.

For example, to insert the time, e.g., 18:51:03, use the secure
expansion form (>>>time<<<) which is defined by:
  (\"time\" template-insert-time \"%T\")"
  :group 'template-expansion
  :type 'string)

(put 'template-time-format 'template-secure-value #'stringp)

(defcustom template-string-default "%0.0S"
  "*Format string used for non-string variable extensions.
If SYMBOL in (\"KEY\" . SYMBOL) is not a string, use string with
substitution SYMBOL/%S.  Default value \"%0.0S\" causes to print
nothing.  See `template-definition-start'."
  :group 'template-expansion
  :type 'string)

(put 'template-string-default 'template-secure-value #'stringp)

(defcustom template-expansion-format "(>>>%s<<<)"
  "Format string for expansion forms.
Is a expansion form with substitution KEY/%s.  The value should
correspond with `template-expansion-regexp'.  Used by
`template-insert-form'."
  :group 'template-expansion
  :type 'string)

(put 'template-expansion-format 'template-secure-value #'stringp)

(defcustom template-expansion-regexp "(>>>\\([A-Za-z0-9_]+\\)<<<)"
  "Regexp matching strings which are replaced by their expansions.
The first regexp group contains the KEY used by the per-template
expansion, see `template-definition-start' and the global expansions in
`template-expansion-alist' and `template-default-expansion-alist'.  The
value should correspond with `template-expansion-alist'.

If there is no defined expansion for the key, ask the user for a
replacement, see `template-read'.  If the key is matched by
`template-register-regexp', store buffer position in register, see
`template-register', .

If you want to use a text literally which is matched by this regexp, use
the zero expansion form (>>>ZERO_FORM<<<)."
  :group 'template-expansion
  :type 'regexp)

(put 'template-expansion-regexp 'template-secure-value #'stringp)

(defcustom template-literal-environment '("LITERAL" . "/LITERAL")
  "Environment for literal text in template.
Looks like (OPEN . CLOSE).  Text between expansion forms with keys OPEN
and CLOSE is not expanded.  If you change OPEN, you should change key
\"LITERAL\" in `template-default-expansion-alist' accordingly."
  :group 'template-expansion
  :type '(cons (string :tag "Open tag") (string :tag "Close tag")))

(defcustom template-register-regexp "\\`[0-9]\\'"
  "*Regexp matching keys for storing point positions in registers.
These keys use `template-register' as the default expansion instead of
`template-read'.  See `template-expansion-regexp'.  If a register is used
twice, it is marked by a \"*\" in the echo area after the expansion."
  :group 'template-expansion
  :type 'regexp)

(put 'template-register-regexp 'template-secure-value #'stringp)

(defcustom template-expansion-alist nil
  "User defined expansions forms.
Predefined expansion forms for `template-expansion-regexp'.  Each entry
has the form (KEY . SEXP).  These expansion forms shadow those in
`template-default-expansion-alist' and are shadowed by those in the
per-template definition section.  See `template-definition-start'."
  :group 'template-expansion
  :type '(repeat (cons :format "%v"
		       (string :tag "Key" :value "")
		       (repeat :tag "Evaluate all" sexp))))

(put 'template-expansion-alist 'risky-local-variable t)

(defvar template-default-expansion-alist
  '(("POINT" (setq template-point (point-marker))) ; point
    ("MARK" (setq template-mark (point-marker))) ; mark
    ("DIR" (insert (first template-file))) ; directory
    ("FILE" (insert (second template-file))) ; file name without directory
    ("FILE_RAW" (insert (third template-file))) ; raw file name without number
    ("FILE_NUM" (insert (fourth template-file))) ; number
    ("FILE_UPCASE" (insert (upcase (third template-file))
			   (fourth template-file)))
    ("FILE_EXT" (or (string= (fifth template-file) "")	; extension
		    (insert (substring (fifth template-file) 1))))
    ("DATE" (template-insert-time))
    ("VC_DATE" (insert (if (fboundp 'format-time-string) ; version control
			   ;;(let ((zone (current-time-zone)))
			   ;;(set-time-zone-rule (cadr zone))))
			   ;;(prog1 (format-time-string "%Y/%m/%d %T"
			   ;;(current-time))
			   ;;(set-time-zone-rule (cadr zone))))
			   (prog2 (set-time-zone-rule "UTC")
			       (format-time-string "%Y/%m/%d %T"
						   (current-time))
			     ;;(set-time-zone-rule (cadr zone))))
			     (set-time-zone-rule nil))
			 "0000/00/00 00:00:00")))
    ("YEAR" (insert (if (and (stringp template-date-format) ; year
			     (fboundp 'format-time-string))
			(format-time-string template-date-format (current-time))
		      "0000")))
    ("ISO_DATE" (insert (if (fboundp 'format-time-string) ; iso date
			    (format-time-string "%Y-%m-%d" (current-time))
			  "0000-00-00")))
    ("COMMENT" (template-read "Initial comment: ")) ; comment
    ("AUTHOR" (insert (or user-mail-address	; author
			  (and (fboundp 'user-mail-address)
			       (user-mail-address))
			  (concat (user-login-name) "@" (system-name)))))
    ("USER_NAME" (insert (or (and (boundp 'user-full-name) ; user name
				  user-full-name)
			     (user-full-name))))
    ("LOGIN_NAME" (insert (user-login-name)))	; login name
    ("HOST_ADDR" (insert (or (and (boundp 'mail-host-address) ; host address
			     (stringp mail-host-address)
			     mail-host-address)
			(system-name))))
    ("LITERAL" (if (search-forward (format template-expansion-format
					   (cdr template-literal-environment))
				   nil 'limit)
		   (delete-region (match-beginning 0) (match-end 0))))
    ("ZERO_FORM"))			; zero form
  "Predefined default expansions forms.
Predefined expansion forms for `template-expansion-regexp'.  Each entry
has the form (KEY . SEXP).  These expansion forms are shadowed by those
in `template-expansion-alist' and by those in the per-template
definition section.  See `template-definition-start'.

The default predefined expansion forms are --default is inserting--:
  (>>>POINT<<<)       set point
  (>>>MARK<<<)        set mark, jump to it with \\[exchange-point-and-mark]
  (>>>DIR<<<)	      directory: /home/clstaff/wedler/lib/
  (>>>FILE<<<)	      file w/o directory: text1.txt
  (>>>FILE_RAW<<<)    raw file name: text
  (>>>FILE_NUM<<<)    number in name: 1
  (>>>FILE_EXT<<<)    extension: txt
  (>>>FILE_UPCASE<<<) upcase file name w/o extension: TEXT1
  (>>>DATE<<<)	      date/time using `template-time-format': 11 Jan 1999
  (>>>YEAR<<<)	      the year: 1999
  (>>>ISO_DATE<<<)    ISO 8601 date: 1999-01-11
  (>>>VC_DATE<<<)     UTC date/time for vc: 1999/01/11 11:58:49
  (>>>COMMENT<<<)     ask user for initial comment
  (>>>AUTHOR<<<)      author, i.e., `user-mail-address'
  (>>>USER_NAME<<<)   user name: Christoph Wedler
  (>>>LOGIN_NAME<<<)  login name: wedler
  (>>>HOST_ADDR<<<)   Host address: fmi.uni-passau.de
  (>>>ZERO_FORM<<<)   zero form, i.e., insert nothing.  Useful to insert
      a text part matched by `template-expansion-regexp' literally.

There are aliases with one-letter keys, see `template-key-alias-alist'.

It is useful to follow the following conventions: upper case keys for
predefined extensions, lower case and digits for per-template and the
following default expansions:
  (>>>0<<<) to (>>>9<<<)  set registers 0 to 9, jump to it with
      \\[jump-to-register] 0 etc., see `template-register-regexp'
  (>>>x<<<) where x is any unused letter sequence: ask user.")

(put 'template-default-expansion-alist 'risky-local-variable t)

(defvar template-key-alias-alist
  '(("P" . "POINT")
    ("M" . "MARK")
    ("D" . "DIR")
    ("F" . "FILE")
    ("R" . "FILE_RAW")
    ("N" . "FILE_NUM")
    ("B" . "FILE_UPCASE")
    ("E" . "FILE_EXT")
    ("T" . "DATE")
    ("V" . "VC_DATE")
    ("Y" . "YEAR")
    ("I" . "ISO_DATE")
    ("C" . "COMMENT")
    ("A" . "AUTHOR")
    ("U" . "USER_NAME")
    ("L" . "LOGIN_NAME")
    ("H" . "HOST_ADDR")
    ("Z" . "ZERO_FORM"))
  "Alist to support the old one-letter predefined expansion forms.
Used for `template-expansion-alist' and
`template-default-expansion-alist'.")

(defcustom template-definition-start
  ">>>TEMPLATE-DEFINITION-SECTION<<<"
  "Header for the per-template definition section.
The region following the the first match of this regexp defines the
per-template definition section.  The region will be deleted before the
actual expansion, see `template-new-file'.  If you use the \"Local
Variables:\" section, define it before this region.

The definition section defines expansion forms for strings KEYs matched
by `template-expansion-regexp' which might shadow those in
`template-expansion-alist' and `template-default-expansion-alist':

  (\"KEY\"): zero form, same as (>>>ZERO_FORM<<<) in default value of
`template-default-expansion-alist', useful for inserting text matched by
`template-expansion-regexp' literally.

  (\"KEY\". CHAR): CHAR is the register where the current buffer
position is stored, see `template-register-regexp'.

  (\"KEY\" \"PROMPT\" \"PREFIX\" \"SUFFIX\" \"DEFAULT\" AGAIN-P) where
the last four arguments are optional: ask user with PROMPT for a STRING.
If REPLACE is not \"\", insert PREFIX STRING SUFFIX, otherwise DEFAULT.
For AGAIN-P, see `template-read'.

  (\"KEY\" . SYMBOL): insert value of SYMBOL; if value is no string at
the time of the replacement, use `template-string-default' as format
string for SYMBOL.

  (\"KEY\" COMMAND . PREFIX): COMMAND is a symbol or a vector and is
called with `command-execute' after setting `prefix-arg' to PREFIX, not
evaluated.  If COMMANDs symbol property `template-secure-command' is
nil, the form is insecure.  If that symbol property is a function, it is
called with PREFIX to check whether COMMAND could be called directly
with PREFIX as remaining args.

  (\"KEY\" SEXPR...): evaluate SEXPR during the expansion, see
`template-expansion-alist' for examples.  This form is insecure.

There are other per-template definitions:

  \"MESSAGE\": additional line displayed at point until first user event
or after `template-message-timeout' seconds.  The lines are displayed
with face in `template-message-face'.

  (CHAR . \"CONTENTS\"): Set register CHAR to have contents CONTENTS.
CONTENTS can then be inserted into a buffer with \\[insert-register] CHAR.

  (CHAR \"CONTENTS\" COMMENT): Set register CHAR to have contents
CONTENTS.  CONTENTS can then be inserted into a buffer with
\\[insert-register] CHAR.  Also display an additional line at point to
show the contents with the optional COMMENT.

  nil: the following forms depend on how often this form has appeared
yet.

  (VARIABLE . VALUE): set SYMBOL's local value to VALUE, not evaluated.
This form is only secure if VARIABLE has a symbol property
`template-secure-value' which returns non-nil when applied to VALUE, not
evaluated.  This form is useful for variables which determine the
expansion, like `template-time-format' and `template-date-format'.  For
local variables in your new file, use the normal way via the \"Local
Variables:\" section.  The nil form should not have appeared yet.

  COMMAND: COMMAND is a symbol or a vector and is called with
`command-execute' before the expansion if the nil form has appeared
once, and after the expansion if the nil form has appeared twice yet.
If COMMANDs symbol property `template-secure-command' is nil, the form
is insecure.  You should use the safe command `normal-mode' in the
pre-expansion forms if the expansion forms depend on the correct major
mode.

  SEXPR: evaluate SEXPR before the expansion if the nil form has
appeared once, and after the expansion if the nil form has appeared
twice yet.  This form is insecure.

If any insecure forms have been used, the use will be asked whether to
use the template, see `template-confirm-insecure'."
  :group 'template-expansion
  :type 'string)



;;;;##########################################################################
;;;;  Commenting
;;;;##########################################################################


;;;===========================================================================
;;;  Main functions
;;;===========================================================================

;;;###autoload
(defun template-single-comment (&optional arg)
  "Fill current line with specific characters.
Prefix argument ARG and `template-comment-specification' determines the
comment style to use.  This command can also be used with point in an
empty line after a comment line."
  (interactive "*P")
  (let ((comment (template-comment-start)))
    (let* ((beg (match-end 0))
	   (end (point-at-eol))
	   (def (template-comment-specification
		 arg
		 (and (> end beg) (buffer-substring (1- end) end))
		 comment)))
      (goto-char beg)
      (if (re-search-forward (template-comment-separator-regexp) end t)
	  (delete-region (match-beginning 0) (match-end 0)))
      (template-insert-separator (car def) nil comment-end))))
(put 'template-single-comment 'template-secure-command t)

;;;###autoload
(defun template-block-comment (&optional arg)
  "Insert delimiter lines around block comment.
Prefix argument ARG and `template-comment-specification' determines the
comment style to use.  This command can also be used with point in an
empty line after a block comment.  A block comment consists of all
neighboring lines which start with spaces and `comment-start'.  If
`comment-start' is a string of length 1, the number of repetitions of
`comment-start' must be the same or larger than in the line where the
command is invoked from, too.

A second invocation of this command directly after a successful
invocation deletes the remaining empty lines from the current line on."
  (interactive "*P")
  (unless (string= comment-end "")
    (error "Only supports comments terminated by end-of-line"))
  (if (and (eq last-command 'template-block-comment-success)
	   (looking-at "[ \t]*$"))
      (template-insert-newline "" nil (1- (point-at-bol)))
    (let* ((comment (template-comment-start))
	   (cstring (if (> (length comment) 1)
			comment
		      (looking-at (concat (regexp-quote comment) "+"))
		      (buffer-substring
		       (match-beginning 0)
		       (min (match-end 0)
			    (+ (length template-comment-specification-alist)
			       (match-beginning 0))))))
	   (prefix (concat "[ \t]*" (regexp-quote cstring)))
	   (sepline (concat prefix "[ \t]*"
			    (template-comment-separator-regexp)))
	   old block-beg block-end def)
      ;; go to the first line with same comment prefix -----------------------
      (while (and (not (bobp)) (looking-at prefix))
	(beginning-of-line 0))
      (or (looking-at prefix) (beginning-of-line 2))
      (while (looking-at sepline)
	(setq old (buffer-substring (1- (match-end 0)) (match-end 0)))
	(kill-line 1))
      (setq block-beg (point-marker))
      ;; go to the last line with same comment prefix ------------------------
      (while (looking-at prefix)
	(indent-according-to-mode)
	(beginning-of-line 2))
      (if (eobp) (newline))
      (setq block-end (copy-marker (point) t))
      (while (progn (forward-line -1) (looking-at sepline))
	(setq old (buffer-substring (1- (match-end 0)) (match-end 0)))
	(kill-line 1))
      ;; insert separator lines ----------------------------------------------
      (goto-char block-beg)
      (set-marker block-beg nil)
      (back-to-indentation)
      (setq def (template-comment-specification arg old comment))
      (beginning-of-line)
      (template-insert-newline (cadr def))
      (template-insert-separator (car def) cstring)
      (goto-char block-end)
      (set-marker block-end nil)
      (template-insert-separator (car def) cstring)
      (template-insert-newline (caddr def)
			       (and (cadddr def)
				    (save-excursion
				      (forward-line (cadddr def))
				      (point))))
      (setq this-command 'template-block-comment-success)))
  (indent-according-to-mode)
  (back-to-indentation))
(put 'template-block-comment 'template-secure-command t)


;;;===========================================================================
;;;  Check comment start, return specification
;;;===========================================================================

(defun template-default-comment ()
  "Return default comment according to current position."
  (if comment-start
      (substring comment-start 0 (string-match "[ \t]\\'" comment-start))
    (if (eolp) "#"
      (let ((default (buffer-substring (point) (1+ (point)))))
	(if (string-match "[A-Za-z]" default) "#" default)))))

(defun template-comment-start ()
  "Return `comment-start' to use without final whitespace.
Also, indent current line or previous non-empty line and return an error
if the line is no comment line."
  (end-of-line)
  (skip-chars-backward " \t\n\f")
  (indent-according-to-mode)
  (back-to-indentation)
  (if (eobp) (error "Empty buffer"))
  (let ((start (template-default-comment)))
    (or (looking-at (regexp-quote start))
	(error "Line does not start with \"%s\"" start))
    start))

(defun template-comment-separator-regexp ()
  "Return regexp matching separator comment lines."
  (concat "\\("
	  (mapconcat #'(lambda (x) (regexp-quote (or (car x) "#")))
		     template-comment-specification-alist
		     "\\|")
	  "\\)+$"))

(defun template-comment-specification (arg old comment)
  "Return the comment specification to use.
See `template-comment-specification-alist' for details.  ARG is the
prefix argument, OLD the SEPARATOR of the old comment style and COMMENT
is the comment start returned by `template-comment-start'."
  (and arg (setq arg (prefix-numeric-value arg)))
  ;; assumes point-at-indentation
  (or (and arg (> arg 0)
	   (if (< (length template-comment-specification-alist) arg)
	       (car (last template-comment-specification-alist))
	     (nth (1- arg) template-comment-specification-alist)))
      (and (null arg) old
	   (assoc old template-comment-specification-alist))
      (and (functionp template-comment-specification-special)
	   (funcall template-comment-specification-special))
      (and (functionp (get major-mode 'template-comment-specification-special))
	   (funcall (get major-mode 'template-comment-specification-special)))
      (if (> (length comment) 1)
	  (cadr (if (bolp)
		    (cdr template-comment-specification-alist)
		  template-comment-specification-alist))
	(looking-at (concat (regexp-quote comment) "+"))
	(template-comment-specification (- (match-end 0) (match-beginning 0))
					nil nil))))


;;;===========================================================================
;;;  Inserting
;;;===========================================================================

(defun template-insert-newline (string &optional limit start-limit)
  "Deletes blank lines around point and insert STRING.
After optional LIMIT and before optional START-LIMIT, no character will
be deleted."
  (let ((start (save-excursion
		 (skip-chars-backward " \t\n\f" start-limit)
		 (or (bobp) (forward-line 1))
		 (point)))
	(end (save-excursion
	       (skip-chars-forward " \t\n\f" limit)
	       (beginning-of-line)
	       (point))))
    (if (> end start) (delete-region start end)))
  (or (bobp) (insert string)))

(defun template-insert-separator (separator &optional cstring estring)
  "Insert separator line at point.
If CSTRING is not nil, insert in special line which starts with CSTRING.
Insert SEPARATOR repeatedly, then ESTRING once up to the column
`template-max-column'."
  (when separator
    (when cstring
      (open-line 1)
      (insert cstring)
      (indent-according-to-mode))
    (end-of-line)
    (let ((max (- template-max-column (length separator) (length estring))))
      (while (<= (current-column) max) (insert separator))
      (if (>= (length separator) (- (current-column) max))
	  (insert (substring separator 0 (- max (current-column))))))
    (if estring (insert estring))
    (if cstring (forward-line 1))))



;;;;##########################################################################
;;;;  Updating (File Name in Header)
;;;;##########################################################################


;;;===========================================================================
;;;  General updating
;;;===========================================================================

(defun template-update-buffer-region (limit regexp group)
  "Return region = (BEG . END) in buffer to be updated.
If LIMIT is positive, check first LIMIT characters in buffer, otherwise
check last -LIMIT characters in buffer for a text to be matched by
REGEXP.  Return region according to GROUP's regexp group in REGEXP."
  (let ((case-fold-search nil))
    (goto-char (if limit
		   (if (natnump limit) (point-min) (+ (point-max) limit))
		 (point-min)))
    (when (re-search-forward regexp
			     (if (natnump limit)
				 (+ (point-min) limit)
			       (point-max))
			     t)
      (cons (match-beginning group) (match-end group)))))

(defun template-update-buffer (&optional arg)
  "Update buffer according to `template-update-buffer-alist'.
Do not do anything if `template-auto-update-disable-regexp' matches the
file name or if `template-auto-update' is nil.  When optional ARG is
non-nil, i.e., if called interactively *without* prefix arg, always
update."
  (interactive (list (null current-prefix-arg)))
  (when (or arg
	    (and template-auto-update buffer-file-name
		 (null (and template-auto-update-disable-regexp
			    (string-match template-auto-update-disable-regexp
					  buffer-file-name)))))
    (save-excursion
      (save-restriction
	(widen)
	(let ((alist template-update-buffer-alist)
	      (name (and buffer-file-name
			 (file-name-sans-versions buffer-file-name)))
	      (case-fold-search (eq system-type 'vax-vms))
	      stamp prompt region new)
	  (while alist
	    (setq stamp (pop alist))
	    (condition-case nil
		(and (setq new (pop stamp))
		     (if (stringp new)
			 (and name (string-match new name))
		       (or (atom new) (memq major-mode new)))
		     ;; Run TEST ---------------------------------------------
		     (setq prompt (pop stamp)
			   region (pop stamp) ; TEST
			   region (eval (if (or (atom region)
						(functionp (car region)))
					    region
					  (cons 'template-update-buffer-region
						region))))
		     (if (stringp (setq new (eval (pop stamp))))
			 (null (string= (buffer-substring (car region)
							  (cdr region))
					new))
		       (car stamp))
		     ;; user confirmation, replacement -----------------------
		     (or (null prompt)
			 (eq template-auto-update t)
			 (y-or-n-p (format prompt (buffer-name))))
		     (progn
		       (goto-char (car region))
		       (if (car stamp)
			   (funcall (car stamp) new (car region) (cdr region))
			 (delete-region (car region) (cdr region))
			 (insert new))))
	      (error nil))))))))


;;;===========================================================================
;;;  Update header
;;;===========================================================================

;;;###autoload
(defun template-update-header (&optional show)
  "Replace old file name in header with current file name.
If SHOW is nil, signal an error if there is no filename in the header.
If SHOW is t or `update', just check, whether there is a name, otherwise
replace without signaling an error.  If SHOW is `update', return region
to be updated, or nil if it will keep the same.  See
`template-header-lines' and `template-header-regexp-alist'."
  (interactive "*P")
  (if buffer-file-name
      (save-excursion
	(goto-char (point-min))
	(let ((case-fold-search nil)
	      (comment-regexp (template-default-comment)) ; at `point-min'!
	      (end (progn (forward-line template-header-lines) (point)))
	      (alist template-header-regexp-alist)
	      group)
	  (setq comment-regexp
		(if (string-match "[A-Za-z]\\|.." comment-regexp)
		    (regexp-quote comment-regexp)
		  (concat (regexp-quote comment-regexp) "+")))
	  (while alist
	    (goto-char (point-min))
	    (if (re-search-forward (format (caar alist) comment-regexp)
				   end t)
		(setq group (cdar alist)
		      alist nil)
	      (setq alist (cdr alist))))
	  (if (and group (match-beginning group))
	      (if (eq show t)
		  (cons (match-beginning group) (match-end group))
		(goto-char (match-beginning group))
		(delete-region (point) (match-end group))
		(insert (file-name-sans-versions
			 (file-name-nondirectory buffer-file-name)))
		t)
	    (if show nil (error "No file name in header")))))
    (if show nil (error "Buffer is not visiting a file"))))



;;;;##########################################################################
;;;;  Templates
;;;;##########################################################################


(defvar template-history nil
  "History, used by `template-read'.")

(defvar template-choice-history nil
  "History, used by `template-choice'.")

(put 'normal-mode 'template-secure-command t)

(defvar template-all-templates nil
  "Internal variable.  Template files used for template derivation.")
(defvar template-file nil
  "Partitioned name of new file: (DIR FILE RAW NUMBER EXT).
Internal variable.  DIR is the directory part, FILE the file name
without directory part.  FILE consists of its extension EXT, RAW and a
numbering NUMBER just in front of the extension.  It is used by the
expansion forms D, F, R, N and E in `template-expansion-alist'.  Also
useful for user defined functions in `template-derivation-alist' and the
per-template definition section.")

(defvar template-modified nil
  "Internal variable.  Whether user is asked during the expansion process.")
(defvar template-secure t
  "Internal variable.  Whether all per-template definitions are secure.")
(defvar template-message nil
  "Internal variable.  List of lines for temporary message at point.")

(defvar template-point nil
  "Internal variable.  Position of point.  Set with expansion form P.")
(defvar template-mark nil
  "Internal variable.  Position of mark.  Set with expansion form M.")

(defvar template-current nil
  "Internal variable.  Current key of expansion form.")
(defvar template-string-alist nil
  "Internal variable.  Alist of user inputs for `template-read'.")
(defvar template-register-alist nil
  "Internal variable.  Alist of used registers.")
(defvar template-local-alist nil
  "Internal variable.  Alist of per-template defined expansions.")


;;;===========================================================================
;;;  Functions: Hooking into `find-file', Auto Update
;;;===========================================================================

(defun template-not-found-function ()
  "Use a template when visiting a non-existent file.
See `template-auto-insert' and `template-find-file-commands'.  Function
in `find-file-not-found-hooks'."
  (and template-auto-insert (not buffer-read-only) (bobp) (eobp)
       (or (memq this-command template-find-file-commands)
	   (and (memq this-command template-file-select-commands)
		;; thanks to Dave Love <d.love@dl.ac.uk>:
		(memq (car-safe (car command-history))
		      template-find-file-commands)))
       (let ((template (cdr (template-derivation buffer-file-name t))))
	 (and (file-readable-p template)
	      (or (eq template-auto-insert t)
		  (y-or-n-p
		   (format "Use template %s? "
			   (if (string-match "XEmacs\\|Lucid" emacs-version)
			       (abbreviate-file-name template t)
			     (abbreviate-file-name template)))))
	      (progn (template-new-file buffer-file-name template t)
		     t)))))


;;;===========================================================================
;;;  Main function
;;;===========================================================================

;;;###autoload
(defun template-new-file (file template &optional created)
  "Open a new file FILE by using a TEMPLATE.
Using a template for creating a new file consists of, steps 1 to 3 are
only executed when called interactively:
 (1) Prompt for the name of the new file.
 (2) Template derivation: suggest a reasonable template file to the user
     see `template-derivation-alist'.
 (3) File name refinement: e.g., if the given file name is \"exercise\"
     and there are two files \"exercise1.tex\" and \"exercise2.tex\" in
     the same directory and if we have a template \"exercise.tex.tpl\",
     the file name is refined to \"exercise3.tex\".  This is turned off
     when \\[template-new-file] is called with a prefix argument.
 (4) Template insertion: insert the template file into the empty buffer.
 (5) Read per-template expansion definition section starting at
     `template-definition-start' and delete it.
 (6) Execute pre-expansion commands defined in the definition section.
 (7) Set local variables defined in the definition section.
 (8) Expansion: expand the expansion forms (text matched by
     `template-expansion-regexp') They are defined in the definition
     section, in `template-expansion-alist', or provided by default, see
     `template-expansion-regexp' and `template-register-regexp'.
 (9) Execute post-expansion commands defined in the definition section.
 (10) Run `normal-mode' and functions in `find-file-hooks'.
 (11) Report: display a temporary message at point defined in the
      definition section and an automatically generated message in the
      minibuffer area, see `template-message-timeout'.

If optional CREATED is non-nil, the buffer for FILE has already been
created."
  (interactive
   (let ((use (template-derivation
	       (expand-file-name
		(read-file-name (if current-prefix-arg
				    "New file (+template, no name change): "
				  "New file (+template): ")))
	       current-prefix-arg)))
     (list (car use)
	   (expand-file-name
	    (read-file-name (format "File %s uses template: "
				    (file-name-nondirectory (car use)))
			    (file-name-directory (cdr use))
			    (file-name-nondirectory (cdr use))
			    t
			    (file-name-nondirectory (cdr use)))
	    (file-name-directory (cdr use))))))
  ;; check template and file name --------------------------------------------
  (if (file-readable-p template)
      (if (file-directory-p template)
	  (error "Template %s is a directory" template))
    (if (null (yes-or-no-p (format "Template %s does not exist.  Create? "
				   template)))
	(error "No template file to use")
      (template-make-directory (file-name-directory template))
      (switch-to-buffer (find-file-noselect template))
      (error "You should create this template first")))
  (if created
      (switch-to-buffer (current-buffer))
    (and (or (get-file-buffer file) (file-exists-p file))
	 (null (yes-or-no-p (format "File %s exists.  Delete contents? " file)))
	 (error "Cannot use templates for existing files"))
    (let ((auto-mode-alist nil)
	  (enable-local-variables nil)
	  (find-file-not-found-hooks nil)
	  (enable-local-eval nil))
      (switch-to-buffer (find-file-noselect file))))
  (setq buffer-undo-list t)
  (insert-file-contents template nil nil nil t)
  ;; start replacement -------------------------------------------------------
  (template-update-header 'if-exists)
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (setq template-secure t
	template-point nil
	template-mark nil
	template-modified nil
	template-message nil
	template-local-alist nil
	template-register-alist nil
	template-string-alist nil)
  (let ((form-selector 0)
	(pre-command-list nil)
	(post-command-list nil)
	(local-variable-list nil)
	val)
    ;; read per-template definition section ----------------------------------
    (goto-char (point-min))
    (when (re-search-forward
	   (concat "^[ \t]*" template-definition-start "[ \t]*$") nil t)
      (condition-case ()
	  (while t
	    (setq val (read (current-buffer)))
	    (cond ((and (consp val) (stringp (car val)))
		   (push (cons (car val) ; expansion forms
			       (template-translate-definition (cdr val)))
			 template-local-alist))
		  ((null val)
		   (setq form-selector (1+ form-selector))
		   (if (> form-selector 2) (error "More than two nil forms")))
		  ((stringp val)
		   (push val template-message))
		  ((and (consp val)
			(or (characterp (car val)) (numberp (car val))))
		   (if (consp (cdr val))
		       (progn
			 (set-register (car val) (cadr val))
			 (push (if (cddr val)
				   (format "   %c:\t\"%s\"\t%s"
					   (car val) (cadr val) (caddr val))
				 (format "   %c:\t\"%s\"" (car val) (cadr val)))
			       template-message))
		     (set-register (car val) (cdr val))))
		  ((and (= form-selector 0)
			(consp val)
			(symbolp (car val)))
		   (or (and (functionp (get (car val) 'template-secure-value))
			    (funcall (get (car val) 'template-secure-value)
				     (cdr val)))
		       (setq template-secure nil))
		   (push val local-variable-list))
		  ((= form-selector 1)
		   (push (template-elisp-in-definition val)
			 pre-command-list))
		  ((= form-selector 2)
		   (push (template-elisp-in-definition val)
			 post-command-list))
		  (t
		   (error "Illegal form"))))
	(error nil))
      (skip-chars-forward " \t\n\f")
      (or (eobp)
	  (error "No valid sexpr! Current value is %s" val))
      (or template-secure
	  (null (default-value template-confirm-insecure))
	  (y-or-n-p "Have you checked the template functions? ")
	  (error "Failed security check"))
      (delete-region (match-beginning 0) (point-max)))
    ;; expand ----------------------------------------------------------------
    (eval (cons 'progn (nreverse pre-command-list)))
    (while local-variable-list
      (make-local-variable (caar local-variable-list))
      (set (caar local-variable-list) (cdar local-variable-list))
      (setq local-variable-list (cdr local-variable-list)))
    (goto-char (point-min))
    (while (re-search-forward template-expansion-regexp nil t)
      (setq template-current (buffer-substring (match-beginning 1)
					       (match-end 1))
	    val (assoc template-current template-local-alist))
      (unless val
	(if (setq val (assoc template-current template-key-alias-alist))
	    (setq template-current (cdr val)))
	(setq val (or (assoc template-current template-expansion-alist)
		      (assoc template-current
			     template-default-expansion-alist))))
      (delete-region (match-beginning 0) (match-end 0))
      (cond (val
	     (eval (cons 'progn (cdr val))))
	    ((string-match template-register-regexp template-current)
	     (template-register))
	    (t
	     (template-read (format "Replacement for `%s': "
				    template-current)))))
    (eval (cons 'progn (nreverse post-command-list)))
    (normal-mode t)
    (run-hooks 'find-file-hooks)
    ;; message ---------------------------------------------------------------
    (cond ((null template-register-alist)
	   (message "%s, no buffer location in register"
		    (if template-mark "Mark set" "No mark")))
	  (t (message "%s, buffer location in register: %s"
		      (if template-mark "Mark set" "No mark")
		      (mapconcat (function
				  (lambda (x)
				    (if (cdr x)
					(concat (char-to-string (car x)) "*")
				      (char-to-string (car x)))))
				 (nreverse template-register-alist)
				 ", "))))
    (set-buffer-modified-p template-modified)
    (goto-char (point-min))
    (when template-point
      (goto-char template-point)
      (set-marker template-point nil))
    (when template-mark
      (set-mark template-mark)
      (set-marker template-mark nil)
      (if (fboundp 'zmacs-activate-region) (zmacs-activate-region)))
    (when template-message
      (let ((beg (point))
	    end)
	(insert (mapconcat 'identity (nreverse template-message) "\n") "\n")
	(setq end (point))
	(goto-char beg)
	(and (fboundp 'make-extent) (fboundp 'set-extent-face)
	     (set-extent-face (make-extent beg end) 'template-message-face))
	(recenter 2)
	(sit-for template-message-timeout)
	(delete-region beg end))))
  (setq buffer-undo-list nil)
  (set-buffer-modified-p template-modified))


;;;===========================================================================
;;;  Determine name of the new file and the template
;;;===========================================================================

(defun template-derivation (full arg)
  "Derive template file name and do file name refinement.
FULL is the initial file name given by the user.  File name refinement
is turned off when ARG is non-nil.  See `template-derivation-alist'."
  ;; Get all templates -------------------------------------------------------
  (setq template-all-templates nil)
  (let* ((regexp (concat "\\`" (regexp-quote (expand-file-name
					     template-home-directory))))
	 (dir (file-name-directory full))
	 (len (1+ (length dir))))
    (while (and dir (> len (setq len (length dir))) (string-match regexp dir))
      (template-all-templates template-subdirectories dir)
      (setq dir (file-name-directory (directory-file-name dir))))
    (template-all-templates template-default-directories)
    (setq template-all-templates (nreverse template-all-templates)))
  ;; Get template file -------------------------------------------------------
  (if (string= (file-name-nondirectory full) "")
      (error "You cannot use templates for directories"))
  (setq template-file (template-split-filename full))
  (let ((tests template-derivation-alist)
	test template file)
    (while tests
      (setq test (caar tests)
	    file (cdar tests))
      (if (setq template
		(if (functionp (car test))
		    (apply (car test) (cdr test))
		  (apply 'template-default-template test)))
	  (setq tests nil)
	(setq tests (cdr tests))))
    (if template
	(or arg
	    (if (functionp (car file))
		(apply (car file) template (cdr file))
	      (apply 'template-unique-file template file)))
      (setq template (template-split-filename
		      "DEFAULT"
		      (or (car template-default-directories)
			  (expand-file-name "~/lib/templates")))))
    (cons (expand-file-name (cadr template-file) (car template-file))
	  (expand-file-name (concat (cadr template) template-extension)
			    (car template)))))


;;;===========================================================================
;;;  Small functions
;;;===========================================================================

(defun template-make-directory (dir)
  "Create DIR if it does not exists yet."
  (cond ((file-exists-p dir))
	((yes-or-no-p (format "The directory %s does not exist.  Create? " dir))
	 (make-directory dir t))
	(t (error "You should create a template directory \"%s\"" dir)))
  dir)

(defun template-split-filename (file &optional dir)
  "Split file name into its parts.
If DIR is nil, FILE is a fully expanded file name, otherwise FILE is a
file name without its directory part DIR.  See `template-file'."
  (or dir (setq dir (template-make-directory (file-name-directory file))
		file (file-name-nondirectory file)))
  (let* ((ext (string-match "\\.[^.]*\\'" file))
	 (raw (substring file 0 ext))
	 (num (string-match "[^0-9][0-9]+\\'" raw)))
    (if num
	(list dir file
	      (substring raw 0 (1+ num))
	      (substring raw (1+ num))
	      (if ext (substring file ext) ""))
      (list dir file raw "" (if ext (substring file ext) "")))))

(defun template-translate-definition (def)
  "Translate DEF of expansion and set `template-secure' accordingly."
  (cond ((null def) ; zero form
	 nil)
	((or (characterp def) (numberp def))
	 `((template-register ,def)))
	((stringp def)
	 `((template-read ,def nil nil nil t)))
	((symbolp def)
	 `((insert (if (stringp ,def) ,def template-string-default))))
	((and (consp def) (stringp (car def)))
	 (if (consp (car-safe (cdr def)))
	     `((template-choice ,(car def) (quote ,(cdr def))))
	   `((apply (quote template-read) (quote ,def)))))
	((consp (car-safe def))
	 (setq template-secure nil)
	 def)
	(t
	 (list (template-elisp-in-definition (car def) (cdr def))))))

(defun template-elisp-in-definition (def &optional prefix)
  "Return valid elisp definition and set `template-secure' accordingly.
DEF is the elisp form, PREFIX would be the prefix argument if DEF is a
command."
  (cond ((consp def)
	 (setq template-secure nil)
	 def)
	((or (symbolp def) (vectorp def))
	 (or (and (symbolp def) (get def 'template-secure-command))
	     (setq template-secure nil))
	 (if (and (symbolp def)
		  (functionp (get def 'template-secure-command))
		  (listp prefix)
		  (funcall (get def 'template-secure-command) prefix))
	     `(apply (quote ,def) (quote ,prefix))
	   `(progn (setq prefix-arg (quote ,prefix))
		   (command-execute (quote ,def)))))
	(t
	 (error "Illegal form"))))


;;;===========================================================================
;;;  Compute template name
;;;===========================================================================

(defun template-all-templates (dirs &optional base)
  "Read names of template files in DIRS relatively to BASE.
Insert the names to internal variable `template-all-templates'."
  (let ((regexp (concat (regexp-quote template-extension) "\\'"))
	(endpos (- (length template-extension)))
	dir templates)
    (while dirs
      (setq dir (expand-file-name (car dirs) base)
	    dirs (cdr dirs))
      (and (file-accessible-directory-p dir)
	   (file-readable-p dir)
	   (setq templates (template-directory-files dir t regexp nil t))
	   (while templates
	     (if (file-readable-p (car templates))
		 (push (template-split-filename (substring (car templates)
							   0
							   endpos))
		       template-all-templates))
	     (setq templates (cdr templates)))))))

(defun template-set-template-part (part file-part)
  "Set template part according to definition PART and FILE-PART.
See `template-derivation-alist' for details."
  (when part
    (cond ((stringp part) part)
	  ((eq part t) file-part)
	  ((null (string= file-part "")) file-part))))

(defun template-default-template (&optional raw num ext regexp)
  "Return template according to RAW, NUM, EXT and REGEXP.
See `template-derivation-alist' for details."
  (if (or (null regexp) (string-match regexp (cadr template-file)))
      (let ((templates template-all-templates)
	    (file-rne (cddr template-file))
	    result template-rne)
	(setq raw (template-set-template-part raw (car file-rne))
	      num (template-set-template-part num (cadr file-rne))
	      ext (template-set-template-part ext (caddr file-rne)))
	(while templates
	  (setq template-rne (cddar templates))
	  (if (and (or (null raw) (string= (car template-rne) raw))
		   (or (null num) (string= (cadr template-rne) num))
		   (or (null ext) (string= (caddr template-rne) ext)))
	      (setq result (car templates)
		    templates nil)
	    (setq templates (cdr templates))))
	result)))


;;;===========================================================================
;;;  File name refinement
;;;===========================================================================

(defun template-default-file (template &optional raw num ext)
  "Refine file name according to TEMPLATE, RAW, NUM and EXT.
The result is in `template-file'.  See `template-derivation-alist'."
  (let ((template-rne (cddr template))
	(file-rne (cddr template-file)))
    (if raw
	(if (eq raw t) (setq raw (car template-rne)))
      (setq raw (car file-rne)))
    (if num
	(if (eq num t) (setq num (cadr template-rne)))
      (setq num (cadr file-rne)))
    (if ext
	(if (eq ext t) (setq ext (caddr template-rne)))
      (setq ext (caddr file-rne)))
    (setcdr template-file (list (concat raw num ext) raw num ext))))

(defun template-unique-file (template &optional raw num ext auto-num)
  "Refine file name according to TEMPLATE, RAW, NUM, EXT and AUTO-NUM.
Use auto numbering if NUM is not \"\" or START-NUM is non-nil.  The
result is in `template-file'.  See `template-derivation-alist'."
  (template-default-file template raw num ext)
  (let* ((dir (car template-file))
	 (full (expand-file-name (cadr template-file) dir)))
    (when (if (string= (fourth template-file) "")
	      auto-num
	    (setq auto-num
		  (and (or (get-file-buffer full)
			   (file-readable-p full))
		       (string-to-int (fourth template-file)))))
      (setq auto-num (1- auto-num)
	    raw (third template-file)
	    ext (fifth template-file))
      (let ((list (buffer-list))
	    file1 dir1)
	(while list
	  (and (setq file1 (buffer-file-name (car list)))
	       (setq dir1 (file-name-directory file1))
	       (string= dir1 dir)
	       (setq auto-num
		     (max (template-filename-number
			   (cddr (template-split-filename
				  (file-name-nondirectory file1)
				  dir1))
			   raw ext)
			  auto-num)))
	  (setq list (cdr list)))
	(setq list (template-directory-files dir nil nil t t))
	(while list
	  (setq auto-num
		(max (template-filename-number
		      (cddr (template-split-filename (car list) dir))
		      raw ext)
		     auto-num)
		list (cdr list)))
	(template-default-file template raw
			       (int-to-string (1+ auto-num))
			       ext)))))

(defun template-filename-number (file-rne raw ext)
  "Return numbering in FILE-RNE if the RAW and EXT parts are equal."
  (or (and (string= (car file-rne) raw)
	   (string= (caddr file-rne) ext)
	   (string-to-int (cadr file-rne)))
      0))


;;;===========================================================================
;;;  Safe commands for per-template expansions
;;;===========================================================================

(defun template-insert-time (&optional format)
  "Insert time into current buffer using time format FORMAT.
If FORMAT is not a string, use `template-time-format'."
  (interactive)
  (insert (if (fboundp 'format-time-string)
	      (format-time-string (cond ((stringp format) format)
					((stringp template-time-format)
					 template-time-format)
					(t "%d %b %Y"))
				  (current-time))
	    (current-time-string))))
(put 'template-insert-time 'template-secure-command
     (lambda (args)
       (or (null args) (and (stringp (car args)) (null (cdr args))))))


;;;===========================================================================
;;;  Functions for the predefined expansions
;;;===========================================================================

(defun template-register (&optional char)
  "Set current location in register CHAR.
That is, \\[jump-to-register] CHAR jumps to the current position.  If
CHAR is nil, use register of last character in `template-current'."
  (or char (setq char (aref template-current (1- (length template-current)))))
  (let ((elem (assoc char template-register-alist)))
    (point-to-register char)
    (if elem
	(setcdr elem t)
      (push (list char) template-register-alist))))

(defun template-read (prompt &optional prefix suffix default again-p)
  "Ask user with PROMPT for a STRING to be inserted.
If replace is not \"\", insert PREFIX STRING SUFFIX, otherwise DEFAULT.
If AGAIN-P is nil, do not ask if `template-current' appears another time
as key in a expansion form.  If AGAIN-P is `expand', the inserted region
is searched for expansion forms where STRING is marked as a literal
environment, see `template-literal-environment'."
  (setq template-modified t)
  (let ((pos (point))
	(elem (and (null again-p)
		   (assoc template-current template-string-alist))))
    (if elem
	(setq elem (cdr elem))
      (setq elem (read-from-minibuffer prompt nil nil nil
				       'template-history)
	    elem (cond ((string= elem "") (or default ""))
		       ((eq again-p 'expand)
			(concat prefix
				(format template-expansion-format
					(car template-literal-environment))
				elem
				(format template-expansion-format
					(cdr template-literal-environment))
				suffix))
		       (t
			(concat prefix elem suffix))))
      (or again-p (push (cons template-current elem) template-string-alist)))
    (insert elem)
    (if (eq again-p 'expand) (goto-char pos))))

(defun template-choice (prompt table)
  (setq template-modified t)
  (let ((pos (point)))
    (insert (or (cdr (assoc (completing-read prompt table nil t nil
					     'template-choice-history)
			    table))
		""))
    (goto-char pos)))


;;;===========================================================================
;;;  Menu filter
;;;===========================================================================

(defun template-menu-filter (menu-items)
  ;; checkdoc-params: (menu-items)
  "Menu filter for `template-creation-menu'."
  (let ((alist (append template-expansion-alist
		       template-default-expansion-alist))
	menu used key)
    (while alist
      (unless (member (setq key (car (pop alist))) used)
	(push key used)
	(push (vector (concat "Insert " key)
		      (list 'template-insert-form current-prefix-arg key)
		      t)
	      menu)))
    (append menu-items (nreverse menu))))


;;;===========================================================================
;;;  Insert and define forms
;;;===========================================================================

(defun template-buffer-template-p ()
  "Return non-nil, if current buffer is likely to be a template file."
  (and buffer-file-name
       (string-match (concat (regexp-quote template-extension) "\\'")
		     (file-name-sans-versions buffer-file-name))))

(defun template-open-template ()
  "If current buffer is no template file, open a new one."
  (interactive)
  (if (template-buffer-template-p)
      (barf-if-buffer-read-only)
    (let (name dir)
      (if (null buffer-file-name)
	  (setq name (concat "TEMPLATE" template-extension))
	(setq name (file-name-sans-versions
		    (file-name-nondirectory buffer-file-name)))
	(if (string-match ".\\.[^.]*\\'" name)
	    (setq name (concat "TEMPLATE"
			       (substring name (1+ (match-beginning 0)))
			       template-extension))
	  (setq name (concat name template-extension)
		dir (car template-default-directories))))
      (setq name (read-file-name "Open template file (empty=none): "
				 dir nil nil name))
      (or (string= name "")
	  (let ((template-auto-insert nil))
	    (find-file name))))))

(defun template-insert-form (arg key)
  "Insert an expansion form according to KEY into template.
When called interactively, allow completion over all keys in
`template-expansion-alist' and `template-default-expansion-alist'.
If prefix ARG is nil, run `template-open-template' first."
  (interactive
   (list current-prefix-arg
	 (completing-read "Insert key (0-9 for register pos): "
			  (append template-expansion-alist
				  template-default-expansion-alist))))
  (or arg (template-open-template))
  (insert (format template-expansion-format key))
  (if (equal key (car template-literal-environment))
      (let ((pos (point)))
	(insert (format template-expansion-format
			(cdr template-literal-environment)))
	(goto-char pos))))

(defun template-define-start (arg &rest args)
  "Insert a definition section and definition into template.
See `template-definition-start'.  If ARGS is non-nil, pass ARGS to
`format' for a new definition.  If prefix ARG is nil, run
`template-open-template' first."
  (interactive "P")
  (or arg (template-open-template))
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward (concat "^[ \t]*"
				       template-definition-start
				       "[ \t]*$") nil t)
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert template-definition-start))
    (goto-char (point-max))
    (or (bolp) (insert "\n"))
    (if args (insert (apply 'format args) "\n")))
  (message "Put definition at the end of the template"))

(defun template-define-message (arg message)
  "Insert a temporary message MESSAGE definition into template.
For ARG, see `template-define-start'."
  (interactive "P\nsTemporary message: ")
  (template-define-start arg "%S" message))

(defun template-define-prompt (arg key prompt &optional prefix suffix default)
  "Insert a definition for KEY as PROMPT into template.
For ARG, see `template-define-start'."
  (interactive "P\nsExpansion key: \nsExpansion prompt: \nsPrefix for non-empty input: \nsSuffix for non-empty input: \nsDefault for empty input: ")
  (template-define-start arg "(%S %S %S %S %S)"
			 key prompt prefix suffix default))

(defun template-define-register (arg register)
  "Insert a setting of REGISTER into template.
For ARG, see `template-define-start'."
  (interactive "P\ncDefine register: ")
  (let* ((old (get-register register))
	 (contents (read-from-minibuffer "Register contents: "
				    (and (stringp old)
					 (not (string-match "\n" old))
					 old)))
	 (comment (read-from-minibuffer "Comment (empty=none): ")))
    (if (string= comment "")
	(template-define-start arg "(%S %S)" register contents)
      (template-define-start arg "(%S %S %S)" register contents comment))))
  

;;;===========================================================================
;;;  Initialization
;;;===========================================================================

;;;###autoload
(defun template-initialize (&rest dummies)
  ;; checkdoc-params: (dummies)
  "Initialized package template.  See variable `template-initialize'."
  (interactive)
  (setq template-use-package t)
  (pushnew (list (concat (regexp-quote template-extension) "\\'")
		 nil 'template-new-file)
	   auto-mode-alist)
  (when (or (eq template-initialize t)
	    (memq 'auto template-initialize))
    (add-hook 'write-file-hooks 'template-update-buffer)
    (add-hook 'find-file-not-found-hooks 'template-not-found-function t))
  (when (or (eq template-initialize t)
	    (memq 'keys template-initialize))
    (define-key ctl-x-map "t" 'template-new-file)
    ;;(define-key ctl-x-map "#" 'template-update-header)
    (when (string-match "XEmacs\\|Lucid" emacs-version)
      ;; or Emacs, too?
      (define-key ctl-x-map [(control =)] 'template-single-comment)
      (define-key ctl-x-map [(control ?\;)] 'template-block-comment)))
  (when (or (eq template-initialize t)
	    (memq 'menus template-initialize))
    (when (string-match "XEmacs\\|Lucid" emacs-version)
      (add-menu-button '("File")
		       ["New File Using Template..." template-new-file
			:active t]
		       "Insert File...")
      (if template-menu (add-submenu '("Edit") template-menu))
      (if template-creation-menu
	  (add-submenu '("Edit") template-creation-menu)))))

;;; template.el ends here

;;; Local Variables:
;;; ispell-check-comments: t
;;; change-log-default-name: "LogFiles/template.ChangeLog"
;;; End:
; LocalWords:  forall FORMs BASEs DIRs TEMPLATEs readme COMMANDs SYMBOL's rne
; LocalWords:  SEXPRs cstring sepline estring endpos nosort tpl UTC LogFiles
; LocalWords:  psgml easymenu eval'd defface ARGs KEYs GROUP's nsTemporary gen
; LocalWords:  nsExpansion nsReplacement ncDefine checkdoc params nsPrefix
; LocalWords:  nsSuffix nsDefault
