(module readconfig
    (read-config-file)

  (import
    (scheme)
    (only srfi-1 append-map)
    (toml)
    (pathname-expand)
    (chicken base)
    (chicken file))

  (define toml-keys '(("enabled" . #:bool)
                      ("directory" . #:string)
                      ("target" . #:string)
                      ("extension" . #:string)
                      ("extensions" . #:list)))

  (define (read-array-values array index max)
    (if (= index max)
        '()
        (cons (toml-string array index)
              (read-array-values array (add1 index) max))))
  
  (define (toml-array->list table key)
    (let* ((array (toml-array table key))
           (values (toml-count-entries array)))
      (read-array-values array 0 values)))

  (define toml-getters `((#:string . ,toml-string)
                         (#:bool . ,toml-bool)
                         (#:list . ,toml-array->list)))

  (define (get-key-value table key)
    (when (toml-key-exists? table key)
      (and-let* ((type (assoc key toml-keys))
                 (getter (cdr (assoc (cdr type) toml-getters))))
        (getter table key))))

  (define (get-tables table index max)
    (if (= index max)
        '()
        (let ((key (toml-key-at table index)))
          (cons key (get-tables table (add1 index) max)))))

  (define (table-info table)
    (values  (+ (toml-count-key-vals table)
                (toml-count-arrays table))
             (toml-count-tables table)))

  (define (list-tables table)
    (let-values (((keys tables) (table-info table)))
      (get-tables table keys  (+ keys tables))))

  (define (extension-rule extension target)
    (cons extension (list (cons #:dir target))))

  (define (group-rule table)
    (when (get-key-value table "enabled")
      (cond ((toml-key-exists? table "extension")
             (list (extension-rule (get-key-value table "extension")
                                   (get-key-value table "target"))))
            ((toml-key-exists? table "extensions")
             (map (lambda (x) (extension-rule x (get-key-value table "target")))
                  (get-key-value table "extensions"))))))

  (define (directory-rule table)
    (when (get-key-value table "enabled")
      (list (cons #:basedir (get-key-value table "directory"))
            (cons #:targetbase "test")
            (cons #:groups (append
                            (append-map (lambda (x) (group-rule (toml-table table x)))
                                        (list-tables table))
                            (list (cons #f (list (cons #:dir "")))))))))

  (define (read-config-file file)
    (let ((table (table-from-file (pathname-expand file))))
      (map (lambda (x) (directory-rule (toml-table table x)))
           (list-tables table))))

  )

(module cleandir
    (clean-dir)

  (import
    (scheme)
    (only srfi-1 filter)
    (chicken base)
    (chicken file)
    (chicken file posix)
    (chicken pathname)
    (pathname-expand)
    (chicken time posix))

  (define (classify-file x groups)
    (let ((ext (pathname-extension x)))
      (cond  ((directory? x) (cons "dir" x))
             ((assoc ext groups) (cons ext x))
             (else (cons #f x)))))

  (define (classify-files dir groups)
    (map (lambda (x) (classify-file (make-absolute-pathname dir x)
                               groups))
         (directory (normalize-pathname dir))))

  (define (ft? ft)
    (lambda (x) (equal? (car x) ft)))
  
  (define (group-files-fn ext)
    (lambda (x) (cons ext (map cdr (filter (ft? ext) x)))))

  (define (group-files exts files)
    (map (lambda (x) (x files))
         (map group-files-fn exts)))

  (define (group-directory-files dir groups)
    (map (lambda (x) (cons  (assoc (car x) groups)
                       (cdr x)))
         (group-files (map (lambda (x) (car x)) groups)
                      (classify-files dir groups))))

  (define (year-month)
    (time->string (seconds->local-time) "%Y-%m"))

  (define (move-files target-dir files)
    (map (lambda (x)
           (let ((target (pathname-replace-directory x target-dir)))
             (if (file-exists? target)
                 (print target " exists ... skipping")
                 (begin (print x " -> " target)
                        (move-file x target)))))
         files))


  (define (apply-rule target-dir)
    (lambda (group)
      (let* ((rule (cdar group))
             (files (cdr group))
             (target-dir (make-absolute-pathname target-dir
                                                 (alist-ref #:dir rule))))
        (create-directory target-dir #t)
        (move-files target-dir files))))
  

  (define (clean-dir rules)
    (let* ((basedir (pathname-expand (alist-ref #:basedir rules)))
           (groupdefs (alist-ref #:groups rules))
           (target-dir (make-absolute-pathname basedir (year-month)))
           (groups (group-directory-files basedir groupdefs)))
      (print basedir " -> " target-dir)
      (map print groups)
      (map (apply-rule target-dir) groups)
      ))
  )

(module main
    (main)
    
  (import
    (scheme)
    (readconfig)
    (cleandir))
  
  (define default-config-file "~/.config/cleandir/config.toml")

  (define (main)
    (map clean-dir (read-config-file default-config-file)))

  (main)
  )

