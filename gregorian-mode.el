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
  '("\\.gabc$")
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
    ;(print "gabc")
    )
  )

(provide 'gregorian-mode)

;;; gregorian-mode.el ends here
