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
  (interactive)
  (setq f (read-string "Enter file name:"))
  (if (not (string= (car (last (split-string f "\\."))) "gabc"))
      (setq f (concat f ".gabc")))
  (if (file-exists-p f)
      (message (concat  "File " (concat f " already exists")))
    (with-temp-buffer (write-file f))
    (find-file f)))

(provide 'gregorian-mode)

;;; gregorian-mode.el ends here
