;;; gregorian-mode.el
;; Author: Spencer King
;; Keywords: languages, gabc, gregorio

;;; Commentary:
;; A basic mode for typesetting gregorian chant.

;;; Code:

(require 'generic-x)

(defconst gregorian-keywords '("name" "gabc-copyright" "score-copyright" "office-part" "occasion" "meter" "commentary" "arranger" "author" "date" "manuscript" "manuscript-reference" "manuscript-storage-place" "book" "language" "transcriber" "transcription-date" "mode" "user-notes" "annotation"))

(defun make-pair (list face)
  ; Create a list of pairs of list elements and a constant
  (cond
   ((null list) '())
   (t (cons
       (cons (car list) face)
       (make-pair (cdr list) face)))))

(defun font-locks()
  ; Generate the list of font-lock pairs
  (append
   (make-pair gregorian-keywords 'font-lock-function-name-face)))

(define-generic-mode
  'gregorian-mode
  '()
  gregorian-keywords
  (font-locks)
  '("\\.gabc$" "\\.greg$")
  nil
  "A mode for typesetting gregorian chant."
  )

(defun gregorian-create-new-gabc()
  ; Create a new gabc file
  (interactive)
  (setq f (read-string "Enter file name:"))
  (if (not (string= (car (last (split-string f "\\."))) "gabc"))
      (setq f (concat f ".gabc")))
  (if (file-exists-p f)
      (message (concat  "File " (concat f " already exists")))
    (with-temp-buffer (write-file f))
    (append-to-file "name: incipit;\n" nil f)
    (append-to-file "gabc-copyright: copyright on this gabc file;\n" nil f)
    (append-to-file "score-copyright: copyright on the source score;\n" nil f)
    (append-to-file "office-part: introitus/...;\n" nil f)
    (append-to-file "occasion: in church calendar;\n" nil f)
    (append-to-file "meter: for metrical hymns;\n" nil f)
    (append-to-file "commentary: source of words;\n" nil f)
    (append-to-file "arranger: name of arranger;\n" nil f)
    (append-to-file "author: if known;\n" nil f)
    (append-to-file "date: xi c;\n" nil f)
    (append-to-file "manuscript: ms name;\n" nil f)
    (append-to-file "manuscript-reference: e.g. CAO reference;\n" nil f)
    (append-to-file "manuscript-storage-place: library/monastery;\n" nil f)
    (append-to-file "book: from which score taken;\n" nil f)
    (append-to-file "language: of the lyrics;\n" nil f)
    (append-to-file "transcriber: writer of gabc;\n" nil f)
    (append-to-file "transcription-date: 2009;\n" nil f)
    (append-to-file "mode: 6;\n" nil f)
    (append-to-file "user-notes: whatever other comments you wish to make;\n" nil f)
    (append-to-file "annotation: IN.;\n" nil f)
    (append-to-file "annotation: 6;\n" nil f)
    (append-to-file "%%\n" nil f)
    (find-file f)))

(defun gregorian-create-tex-template(fname)
  ; Create a generic LaTeX template for the score
  (setq outfile (concat fname ".tex"))
  (with-temp-buffer (write-file outfile))
  (append-to-file "\\documentclass{scrartcl}\n" nil outfile)
  (append-to-file "\\usepackage[osf,p]{libertine}\n" nil outfile)
  (append-to-file "\\usepackage{gregoriotex}\n" nil outfile)
  (append-to-file "\\usepackage[latin]{babel}\n" nil outfile)
  (append-to-file "\\setkomafont{section}{\\normalfont\\centering\\huge\\scshape}\n" nil outfile)
  (append-to-file "\\setcounter{secnumdepth}{-\\maxdimen}\n" nil outfile)
  (append-to-file "\\grechangedim{beforeinitialshift}{2.2mm}{scalable}\n" nil outfile)
  (append-to-file "\\grechangedim{afterinitialshift}{2.2mm}{scalable}\n" nil outfile)
  (append-to-file "\\grechangestyle{initial}{\\fontsize{43}{43}\\selectfont}\n" nil outfile)
  (append-to-file "\\gresetlinecolor{gregoriocolor}\n" nil outfile)
  (append-to-file "\\gresetheadercapture{commentary}{grecommentary}{string}\n" nil outfile)
  (append-to-file "\\grechangestyle{annotation}{\\small\\bfseries}\n" nil outfile)
  (append-to-file "\\begin{document}\n" nil outfile)
  (setq score (concat "\\gregorioscore[a]{" (file-name-base fname) "}\n"))
  (append-to-file score  nil outfile)
  (append-to-file "\\end{document}" nil outfile))

(defun gregorian-build-greg()
  ; Converts a greg to a gabc
  (interactive)
  (setq f (file-name-nondirectory buffer-file-name))
  (setq fname (file-name-sans-extension f))
  (setq ext (file-name-extension f))
  (if (not (string= "greg" ext))
      (print "not greg")
    (setq outfile (concat fname ".gabc"))
    (goto-char (point-min))
    (setq cont t)
    (while cont
      (let* ((lb (line-beginning-position))
             (le (line-end-position))
             (ln (buffer-substring-no-properties lb le)))
	(if (not (string= ln "%%"))
	    (append-to-file (concat ln "\n") nil outfile)
	  (setq cont nil)
	  (append-to-file (concat ln "\n") nil outfile))
	(forward-line 1)
	))

    (while (not (eobp))
      (forward-line 1)
      (let* ((lb (line-beginning-position))
             (le (line-end-position))
             (ln (buffer-substring-no-properties lb le)))
	(setq notes (split-string ln))
	(forward-line 1)
	(setq lyrics (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
	(forward-line 1)
	(if (string-match "\\`c[0-9]\\'" (car notes))
	    (progn
	      (append-to-file (concat "(" (car notes) ") ") nil outfile)
	      (setq notes (cdr notes))
	      )
	  )

	;; Interleave the notes and the lyrics like a traditional gabc file
	(setq line "")
	(while lyrics
	  (if (not (string= (car lyrics) "-"))
	      (progn
		;;(if (string= (car lyrics) "*")
		 ;;   (setq line (concat line (car lyrics) " (" (car notes) ")"))
		  (setq line (concat line (car lyrics) "(" (car notes) ")"))
		(setq notes (cdr notes))
		(setq lyrics (cdr lyrics))
		)
	    (setq line (concat line " "))
	    (setq lyrics (cdr lyrics))
	    )
	  )

	;; Add any additional notes
	(while notes
	  (setq line (concat line " (" (car notes) ")"))
	  (setq notes (cdr notes)))
	
	(append-to-file (concat line "\n") nil outfile)
	)
      )

    ))

(defun gregorian-build()
  ; Build the score
  (interactive)
  (setq f (file-name-nondirectory buffer-file-name))
  (setq fname (file-name-sans-extension f))
  (setq ext (file-name-extension f))
  (if (not (string= "gabc" ext))
      (print "not gabc")
    (shell-command (concat "gregorio " f))
    (gregorian-create-tex-template (file-name-sans-extension buffer-file-name))
    (shell-command (concat "lualatex --shell-escape " (file-name-base buffer-file-name) ".tex"))
    ;(call-process-shell-command (concat "lualatex --shell-escape " (file-name-base buffer-file-name) ".tex") nil 0)
    ;(when (not (get-buffer (concat (file-name-base buffer-file-name) ".pdf")))
      ;; (split-window-right)
      (other-window 1)
      ;; (find-file (concat (file-name-base buffer-file-name) ".pdf"));)
    ;; TODO: reload buffer on build if already open, do something like switch to the buffer and use revert-buffer
    )
  )

(provide 'gregorian-mode)

;;; gregorian-mode.el ends here
