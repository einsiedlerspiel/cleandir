(import
 (srfi-1)
 (chicken file)
 (chicken file posix)
 (chicken pathname)
 (pathname-expand)
 (chicken keyword)
 (chicken time)
 (chicken time posix))

(define lou-rules
  '(#:basedir "~/Downloads/"
              #:groups (("pdf" . ((#:dir . "PDF")))
                        ("odt" . ((#:dir . "office")))
                        ("docx" . ((#:dir . "office")))
                        (#f . ((#:dir . ""))))))

(define (classify-file x)
  (if (not (directory? x))
      (cons (pathname-extension x) x)
      (cons "dir" x)))

(define (classify-files dir)
   (map (lambda (x) (classify-file
                (make-absolute-pathname dir x)))
        (directory (normalize-pathname dir))))

(define (lou-ft? ft)
  (lambda (x) (equal? (car x)
                 ft)))
                     
(define (group-files-fn ext)
  (lambda (x) (cons ext (map cdr (filter (lou-ft? ext) x)))))

(define (group-files exts files)
  (map (lambda (x) (x files))
       (map group-files-fn exts)))

(define (group-directory-files dir rules)
    (map (lambda (x) (cons  (assoc (car x) rules)
                       (cdr x)))
         (group-files (map (lambda (x) (car x)) rules)
                      (classify-files dir))))

(define (year-month)
  (time->string (seconds->local-time) "%Y-%m"))

(define (move-files target-dir files)
  (map (lambda (x)
         (move-file x (pathname-replace-directory x target-dir)))
       files))

(define (apply-rule group target-dir)
  (let* ((rule (cdar group))
         (files (cdr group))
         (target-dir (make-absolute-pathname target-dir
                                             (alist-ref #:dir rule))))
    (create-directory target-dir #t)
    (move-files target-dir files)))
  

(define (clean-dir)
  (let* ((basedir (pathname-expand (get-keyword #:basedir lou-rules)))
        (rules (get-keyword #:groups lou-rules))
        (target-dir (make-absolute-pathname basedir (year-month)))
        (groups (group-directory-files basedir rules)))
    (map print groups)
    (map (lambda (x) (apply-rule x target-dir))
         groups)))

(clean-dir)
