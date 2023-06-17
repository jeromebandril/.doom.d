;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jerome Bandril"
      user-mail-address "jerome.bandril@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SECTION FOR CUSTOM CONFIGURATIONS

;; set notes as root for my agenda files and searches for .org files only
(setq org-agenda-files
      (directory-files-recursively "~/notes" "\\.org$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VISUALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme
;;(after! doom-themes
;;  (load-theme 'choose-theme))
(setq doom-theme 'doom-vibrant)

;; font 
(setq doom-font (font-spec :family "Roboto Mono" :size 15))

;; activate ligatures
(global-prettify-symbols-mode 1)
(setq font-smoothing 'antialias)

;; Load the custom org-bullets package
(load! "jerome-org-bullets/org-bullets")

;; better bullets
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;;better headers bullets 
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; better headers
;;  (let* ((variable-tuple
;;          (cond ((x-list-fonts "Roboto Mono")         '(:font "Roboto Mono"))
;;                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;         (base-font-color     (face-foreground 'default nil 'default))
;;         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
;;
;;    (custom-theme-set-faces
;;     'user
;;     `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))


;; custom ellipsis
;; Set custom ellipsis character
;(set-face-underline 'org-ellipsis nil)
(setq org-ellipsis " ▼")
;; ⤵ ▼ ⬎  

;; hide emphasis
(setq org-hide-emphasis-markers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;; default encoding system
(setq-default buffer-file-coding-system 'utf-8-unix)

;; make links to file be opened with system default apps
(setq org-file-apps '(("\\.pdf\\'" . default)
                      ("\\.docx\\'" . default)
                      ;; Add more file extensions and corresponding actions as needed
                      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; export options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LATEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-latex-listings t) ;; Uses listings package for code exports
(setq org-latex-compiler "xelatex") ;; XeLaTex rather than pdflatex

;; not sure what this is, look into it
;; '(org-latex-active-timestamp-format "\\texttt{%s}")
;; '(org-latex-inactive-timestamp-format "\\texttt{%s}")

;; LaTeX Classes
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex" ;; I use this in base class in all of my org exports.
                 "\\documentclass{extarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

;; org-latex-preview settings and org-fragtog-mode (that uses the latter) 
(add-hook 'org-mode-hook 'org-fragtog-mode)
;; scale
(after! setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))
;; color

;; https://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/
;; THIS DOESN'T WORK :(
;; specify the justification you want
(after! setq org-format-latex-options (plist-put org-format-latex-options :justify 'center))

(defun org-justify-fragment-overlay (beg end image imagetype)
  "Adjust the justification of a LaTeX fragment.
The justification is set by :justify in
`org-format-latex-options'. Only equations at the beginning of a
line are justified."
  (cond
   ;; Centered justification
   ((and (eq 'center (plist-get org-format-latex-options :justify)) 
         (= beg (line-beginning-position)))
    (let* ((img (create-image image 'imagemagick t))
           (width (car (image-size img)))
           (offset (floor (- (/ (window-text-width) 2) (/ width 2)))))
      (overlay-put (ov-at) 'before-string (make-string offset ? ))))
   ;; Right justification
   ((and (eq 'right (plist-get org-format-latex-options :justify)) 
         (= beg (line-beginning-position)))
    (let* ((img (create-image image 'imagemagick t))
           (width (car (image-display-size (overlay-get (ov-at) 'display))))
           (offset (floor (- (window-text-width) width (- (line-end-position) end)))))
      (overlay-put (ov-at) 'before-string (make-string offset ? ))))))

(defun org-latex-fragment-tooltip (beg end image imagetype)
  "Add the fragment tooltip to the overlay and set click function to toggle it."
  (overlay-put (ov-at) 'help-echo
               (concat (buffer-substring beg end)
                       "mouse-1 to toggle."))
  (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-1]
                                      `(lambda ()
                                         (interactive)
                                         (org-remove-latex-fragment-image-overlays ,beg ,end)))
                                    map)))

;; advise the function to a
(advice-add 'org--format-latex-make-overlay :after 'org-justify-fragment-overlay)
(advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-tooltip)


(eval-when-compile
  (require 'image-file))

(defgroup iimage nil
  "Support for inline images."
  :version "22.1"
  :group 'image)

(defconst iimage-version "1.1")
(defvar iimage-mode nil)
(defvar iimage-mode-map nil)

;; Set up key map.
(unless iimage-mode-map
  (setq iimage-mode-map (make-sparse-keymap))
  (define-key iimage-mode-map "\C-l" 'iimage-recenter))

(defun iimage-recenter (&optional arg)
"Re-draw images and recenter."
  (interactive "P")
  (iimage-mode-buffer 0)
  (iimage-mode-buffer 1)
  (recenter arg))

(defvar iimage-mode-image-filename-regex
  (concat "[-+./_0-9a-zA-Z]+\\."
	  (regexp-opt (nconc (mapcar #'upcase
				     image-file-name-extensions)
			     image-file-name-extensions)
		      t)))

(defvar iimage-mode-image-regex-alist
  `((,(concat "\\(`?file://\\|\\[\\[\\|<\\|`\\)?"
	      "\\(" iimage-mode-image-filename-regex "\\)"
	      "\\(\\]\\]\\|>\\|'\\)?") . 2))
"*Alist of filename REGEXP vs NUM.
Each element looks like (REGEXP . NUM).
NUM specifies which parenthesized expression in the regexp.

Examples of image filename regexps:
    file://foo.png
    `file://foo.png'
    \\[\\[foo.gif]]
    <foo.png>
     foo.JPG
")

(defvar iimage-mode-image-search-path nil
"*List of directories to search for image files for iimage-mode.")

;;;###autoload
(defun turn-on-iimage-mode ()
"Unconditionally turn on iimage mode."
  (interactive)
  (iimage-mode 1))

(defun turn-off-iimage-mode ()
"Unconditionally turn off iimage mode."
  (interactive)
  (iimage-mode 0))

(defalias 'iimage-locate-file 'locate-file)

(defun iimage-mode-buffer (arg)
"Display/undisplay images.
With numeric ARG, display the images if and only if ARG is positive."
  (interactive)
  (let ((ing (if (numberp arg)
		 (> arg 0)
	       iimage-mode))
	(modp (buffer-modified-p (current-buffer)))
	file buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (dolist (pair iimage-mode-image-regex-alist)
	(while (re-search-forward (car pair) nil t)
	  (if (and (setq file (match-string (cdr pair)))
		   (setq file (iimage-locate-file file
				   (cons default-directory
					 iimage-mode-image-search-path))))
	      (if ing
		  (add-text-properties (match-beginning 0) (match-end 0)
				       (list 'display (create-image file)))
		(remove-text-properties (match-beginning 0) (match-end 0)
					'(display)))))))
    (set-buffer-modified-p modp)))

;;;###autoload
(define-minor-mode iimage-mode
  "Toggle inline image minor mode."
  :group 'iimage :lighter " iImg" :keymap iimage-mode-map
  (run-hooks 'iimage-mode-hook)
  (iimage-mode-buffer iimage-mode))

(provide 'iimage)

;; arch-tag: f6f8e29a-08f6-4a12-9496-51e67441ce65
;;; iimage.el ends here
