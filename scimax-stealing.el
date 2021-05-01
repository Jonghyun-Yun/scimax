;;; scimax-stealing.el --- Description -*- lexical-binding: t; -*-
;;
;; Scrapping codes in scimax-emacs
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;;; scimax-org-latex.el
;; ** numbering latex equations

;; Numbered equations all have (1) as the number for fragments with vanilla
;; org-mode. This code injects the correct numbers into the previews so they
;; look good.
(defun scimax-org-renumber-environment (orig-func &rest args)
  "A function to inject numbers in LaTeX fragment previews."
  (let ((results '())
        (counter -1)
        (numberp))
    (setq results (loop for (begin .  env) in
                        (org-element-map (org-element-parse-buffer) 'latex-environment
                          (lambda (env)
                            (cons
                             (org-element-property :begin env)
                             (org-element-property :value env))))
                        collect
                        (cond
                         ((and (string-match "\\\\begin{equation}" env)
                               (not (string-match "\\\\tag{" env)))
                          (incf counter)
                          (cons begin counter))
                         ((string-match "\\\\begin{align}" env)
                          (prog2
                              (incf counter)
                              (cons begin counter)
                            (with-temp-buffer
                              (insert env)
                              (goto-char (point-min))
                              ;; \\ is used for a new line. Each one leads to a number
                              (incf counter (count-matches "\\\\$"))
                              ;; unless there are nonumbers.
                              (goto-char (point-min))
                              (decf counter (count-matches "\\nonumber")))))
                         (t
                          (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))

  (apply orig-func args))


(defun scimax-toggle-latex-equation-numbering ()
  "Toggle whether LaTeX fragments are numbered."
  (interactive)
  (if (not (get 'scimax-org-renumber-environment 'enabled))
      (progn
        (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
        (put 'scimax-org-renumber-environment 'enabled t)
        (message "Latex numbering enabled"))
    (advice-remove 'org-create-formula-image #'scimax-org-renumber-environment)
    (put 'scimax-org-renumber-environment 'enabled nil)
    (message "Latex numbering disabled.")))

;;; scimax-org.el
;; * copy/kill-dwim

(defun scimax-copy-dwim (arg)
  "Copy and do what I mean.
If a region is active copy it.
If at the beginning of a sentence, copy it.
If at the beginning of a paragraph copy it.
Default to copying the word at point"
  (interactive "P")
  (cond
   ((region-active-p)
    (kill-ring-save (region-beginning) (region-end)))
   ;; paragraph
   ((let ((cp (point)))
      (save-excursion
        (forward-paragraph)
        (backward-paragraph)
        ;; if the preceding line is blank we end up on it. this moves us back to
        ;; the beginning of the paragraph.
        (when (looking-at "^$") (forward-line))
        (= cp (point))))
    (kill-ring-save (point) (save-excursion (forward-paragraph) (point))))
   ;; sentence
   ((let ((cp (point)))
      (save-excursion
        (forward-sentence)
        (backward-sentence)
        (= cp (point))))
    (let* ((bounds (bounds-of-thing-at-point 'sentence))
           (start (car bounds))
           (end (cdr bounds)))
      (kill-ring-save start end)))
   ((bolp)
    (kill-ring-save (line-beginning-position) (line-end-position)))
   ;; default to word
   (t
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (start (car bounds))
           (end (cdr bounds)))
      (kill-ring-save start end)))))

(defun scimax-kill-dwim (arg)
  "Kill and do what I mean.
If a region is active kill it.
If at the beginning of a sentence, kill it.
If at the beginning of a paragraph kill it.
Default to killing the word at point"
  (interactive "P")
  (cond
   ((region-active-p)
    (kill-region (region-beginning) (region-end)))
   ;; paragraph
   ((let ((cp (point)))
      (save-excursion
        (forward-paragraph)
        (backward-paragraph)
        ;; if the preceding line is blank we end up on it. this moves us back to
        ;; the beginning of the paragraph.
        (when (looking-at "^$") (forward-line))
        (= cp (point))))
    (kill-region (point) (save-excursion (forward-paragraph) (point))))
   ;; sentence
   ((let ((cp (point)))
      (save-excursion
        (forward-sentence)
        (backward-sentence)
        (= cp (point))))
    (let* ((bounds (bounds-of-thing-at-point 'sentence))
           (start (car bounds))
           (end (cdr bounds)))
      (kill-region start end)))
   ((bolp)
    (kill-line))
   ;; default to word
   (t
    (let* ((bounds (bounds-of-thing-at-point 'word))
           (start (car bounds))
           (end (cdr bounds)))
      (kill-region start end)))))

(provide 'scimax-stealing)
;;; scimax-stealing.el ends here
