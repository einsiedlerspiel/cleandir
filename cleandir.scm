;;; Copyright (C) 2024  Lou Woell

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, version 3 of the License.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

(module readconfig
    (read-config-file)

  (import
    (scheme)
    (only srfi-1 append-map)
    (toml)
    (chicken base)
    (chicken format)
    (only (chicken file) file-exists?))

  (define toml-keys '(("enabled" . #:bool)
                      ("move-ungrouped" . #:bool)
                      ("remove-duplicates" . #:bool)
                      ("target-prefix" . #:string)
                      ("directory" . #:string)
                      ("target" . #:string)
                      ("extension" . #:string)
                      ("extensions" . #:list)))

  (define (read-array-values array index max list)
    (if (= index max) list
        (read-array-values array (add1 index) max
                           (cons (toml-string array index) list))))

  (define (toml-array->list table key)
    (let* ((array (toml-array table key))
           (values (toml-count-entries array)))
      (read-array-values array 0 values '())))

  (define toml-getters `((#:string . ,toml-string)
                         (#:bool . ,toml-bool)
                         (#:list . ,toml-array->list)))

  (define (get-key-value table key)
    (when (toml-key-exists? table key)
      (and-let* ((type (alist-ref key toml-keys equal?))
                 (getter (alist-ref type toml-getters)))
        (getter table key))))

  (define (get-tables table index max list)
    (if (= index max) list
        (let ((key (toml-key-at table index)))
          (get-tables table (add1 index) max (cons key list)))))

  (define (table-info table)
    (values  (+ (toml-count-key-vals table)
                (toml-count-arrays table))
             (toml-count-tables table)))

  (define (list-tables table)
    (let-values (((keys tables) (table-info table)))
      (get-tables table keys  (+ keys tables) '())))

  (define (extension-rules extensions table lst)
    (if (eq? extensions '()) lst
        (let ((extension (car extensions))
              (target (get-key-value table "target")))
          (extension-rules (cdr extensions) table
                           (cons (cons extension (list (cons #:dir target)))
                                 lst)))))

  (define (group-rule table)
    (when (get-key-value table "enabled")
      (cond ((toml-key-exists? table "extension")
             (extension-rules (list (get-key-value table "extension")) table '()))
            ((toml-key-exists? table "extensions")
             (extension-rules (get-key-value table "extensions") table '())))))

  (define (make-group-rules table)
    (let ((rules (append-map
                  (lambda (x) (group-rule (toml-table table x)))
                  (list-tables table))))
      (if (get-key-value table "move-ungrouped")
          (cons (cons #f (list (cons #:dir "")))
                rules)
          rules)))

  (define (directory-rule table)
    (when (get-key-value table "enabled")
      (list (cons #:basedir (get-key-value table "directory"))
            (cons #:target-prefix (get-key-value table "target-prefix"))
            (cons #:dups (if (get-key-value table "remove-duplicates")
                             #t #f))
            (cons #:groups (make-group-rules table)))))

  (define (read-config-file file)
    (when (not (file-exists? file))
      (print (format #f "Config file ~A does not exist!" file))
      (exit 1))
    (print "config: " file)
    (let ((table (table-from-file file)))
      (map (lambda (x) (directory-rule (toml-table table x)))
           (list-tables table))))

  )

(module file-actions
    (find-files
     remove-files-in-list)

  (import
    (scheme)
    (only (srfi-1) filter-map)
    (pathname-expand)
    (only (chicken base) unless print)
    (only (chicken pathname) make-absolute-pathname)
    (only (chicken file posix) directory? symbolic-link?)
    (only (chicken file) directory delete-file*))

  (define (get-files dir #!key (dirs? #t) (symlinks? #t))
    (let ((dir (pathname-expand dir)))
      (filter-map (lambda (x) (let ((file (make-absolute-pathname dir x)))
                           (and
                            ;;options for directories
                            (cond ((not dirs?) (not (directory? file)))
                                  ((eq? dirs? 'only) (directory? file))
                                  (else #t))
                            ;;options for symlinks
                            (cond ((not symlinks?) (not (symbolic-link? file)))
                                  ((eq? symlinks? 'only) (symbolic-link? file))
                                  (else #t))
                            file)))
                  (directory dir))))

  (define (find-files dirs #!key (files '()) (recur? #t) (dirs? #f)
                      (follow-symlinks? #f) (return-symlinks? #f))
    (if (or (eq? dirs '())) files
        (find-files
         (if recur? (append (cdr dirs) (get-files (car dirs) #:dirs? 'only
                                                  #:symlinks? follow-symlinks?))
             (cdr dirs))
         #:files (append (get-files (car dirs)
                                    #:dirs? dirs?
                                    #:symlinks? return-symlinks?)
                         files)
         #:follow-symlinks? follow-symlinks?
         #:recur? recur?
         #:dirs? dirs?)))

  (define (remove-files-in-list list #!key (dryrun #f))
    (unless (eq? list '())
      (print "delete duplicate:" (car list))
      (unless dryrun (delete-file* (car list)))
      (remove-files-in-list (cdr list) #:dryrun dryrun)))

  )


(module checkdups
    (group-duplicates
     get-duplicate-groups
     remove-duplicates
     dir-remove-duplicates)

  (import
    (file-actions)
    (scheme)
    (srfi-1)
    (chicken base)
    (simple-md5))

  (define (get-duplicates alist dups)
    (if (eq? alist '()) dups
        (let ((head (car alist))
              (tail (cdr alist)))
          (get-duplicates
           tail
           (if (assoc (car head) tail)
               (let ((dups (if (or (eq? '() dups)
                                   (not (assoc (car head) dups)))
                               (cons (list (car head) (cdr head)) dups)
                               dups)))
                 (and (set-cdr! (assoc (car head) dups)
                                (cons (alist-ref (car head) tail equal?)
                                      (alist-ref (car head) dups equal?)))
                      dups))
               dups)))))


  (define (sum-cons x)
    (cons (file-md5sum x)
          x))

  (define (group-duplicates files)
    (get-duplicates (map sum-cons files) '()))

  (define (get-duplicate-groups dirs #!key (recur? #f) )
    (group-duplicates (find-files dirs #:recur? recur? #:follow-symlinks #f)))

  ;; takes a list of files as returned by `group-duplicates', then deletes
  ;; duplicate files.
  (define (remove-duplicates dups #!key (dryrun #f))
    (remove-files-in-list
      ;; 1. Element is always md5sum
      ;; 2. Element is first version of the file
      ;; 3. to last element are files to delete
     (append-map cddr dups) #:dryrun dryrun))

  ;; Like `remove-duplicates' but takes a list of directories to check for
  ;; duplicates. Optionally recurs through subdirectories. ignores symlinked
  ;; directories.
  (define (dir-remove-duplicates dirs #!optional (recur #f) #!key (dryrun #f))
    (remove-duplicates (get-duplicate-groups dirs #:recur? recur) #:dryrun dryrun))

  )

(module cleandir
    (clean-dir)

  (import
    (scheme)
    (checkdups)
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
    (map (lambda (x) (cons (assoc (car x) groups)
                      (cdr x)))
         (group-files (map car groups)
                      (classify-files dir groups))))

  (define (time-format fstring)
    (time->string (seconds->local-time) fstring))

  (define (make-target-pairs target-dir files list)
    (if (eq? files '()) list
        (make-target-pairs target-dir
                           (cdr files)
                           (cons (cons (car files)
                                       (pathname-replace-directory (car files)
                                                                   target-dir))
                                 list))))

  (define (move-files files #!key (dryrun #f))
    (unless (eq? files '())
      (let ((target (car files)))
        (cond
         ;; relevant if source file was deleted as a duplicate
         ((not (file-exists? (car target)))
          (move-files (cdr files) #:dryrun dryrun))
         ;; relevant if a different file with the same name exists
         ((file-exists? (cdr target))
          (print (cdr target) " exists ... renaming")
          (move-files (cons
                       (cons (car target)
                             (pathname-replace-file
                              (cdr target)
                              (string-append (pathname-file (cdr target))
                                             "-dup")))
                       (cdr files)) #:dryrun dryrun))
         ;; finally move file
         (else (print (car target) " -> " (cdr target))
               (unless dryrun (move-file (car target) (cdr target)))
               (move-files (cdr files) #:dryrun dryrun))))))


  (define (apply-rules basedir target-dir groups #!key (dryrun #f))
    (unless (eq? groups '())
      (let* ((group (car groups))
             (rule (cdar group))
             (files (cdr group))
             (target-dir (time-format
                          (make-absolute-pathname target-dir
                                                  (alist-ref #:dir rule))))
             (targets (make-target-pairs target-dir files '())))
        (unless dryrun (create-directory target-dir #t))
        ;;TODO: group based duplicate removal
        ;; (when #f (dir-remove-duplicates (list basedir target-dir)))
        (move-files targets #:dryrun dryrun))
      (apply-rules basedir target-dir (cdr groups) #:dryrun dryrun)))


  (define (clean-dir dir-rules #!key (dryrun #f))
    (unless (eq? dir-rules '())
      (let* ((rules (car dir-rules))
             (basedir (pathname-expand (alist-ref #:basedir rules)))
             (target-prefix (alist-ref #:target-prefix rules))
             (target-prefix (if (string? target-prefix) target-prefix ""))
             (target-dir (make-absolute-pathname basedir target-prefix))
             (groups (group-directory-files basedir (alist-ref #:groups rules))))
        (print basedir " -> " target-dir)
        (when (alist-ref #:dups rules)
          (print "Checking for Duplicates in " basedir)
          (dir-remove-duplicates (list basedir) #:recur? #t #:dryrun dryrun))
        (map print groups)
        (apply-rules basedir target-dir groups #:dryrun dryrun))
      (clean-dir (cdr dir-rules) #:dryrun dryrun)))
  )

(module cmd-args
    (parse-args
     config-file
     dryrun)

  (import
    (checkdups)
    (scheme)
    (chicken base)
    (chicken file)
    (pathname-expand)
    (chicken pathname)
    (chicken process-context)
    (only (chicken string) string-split)
    (args))


  (define dryrun #f)
  (define-constant default-config (get-environment-variable "CONFIG_PATH"))

  (define config-file  "~/.config/cleandir/config.toml")
  (define options
    (list
     (args:make-option (d dryrun) #:none
                       "Run command normally, but don't move or delete any files."
                       (print "Dry running...")
                       (newline)
                       (set! dryrun #t))
     (args:make-option (l list-duplicates) #:required
                       "List duplicate files in Directories. ARG must be a comma separated string of directory paths."
                       (map print (get-duplicate-groups (string-split arg ",") #:recur? #t))
                       (exit 0))
     (args:make-option (c config-file) #:required
                       "Config file to use."
                       (set! config-file arg))
     (args:make-option (s setup) #:none
                       "Setup user config"
                       (let* ((orig (pathname-expand default-config))
                              (dest (pathname-expand config-file))
                              (dest-dir (pathname-directory dest)))
                         (unless (directory-exists? dest-dir)
                           (create-directory dest-dir #t))
                         (if (file-exists? dest)
                             (print dest " already exists.")
                             (copy-file orig dest #f))
                         (exit 0)))
     (args:make-option (h help) #:none
                       "Show this help"
                       (usage))))

  (define (usage)
    (print "Usage:" (car (argv)) " [options...] ")
    (newline)
    (print (args:usage options))
    (print "User Config File: " config-file)
    (print "Default Config File: " default-config)
    (newline)
    (print "Copyright (C) 2024 Lou Woell")
    (print "See source code for licensing information")
    (exit 0))

  (define (parse-args)
    (args:parse (command-line-arguments) options))
  )

(module main
    (main)

  (import
    (only scheme define)
    (cmd-args)
    (readconfig)
    (pathname-expand)
    (cleandir))

  (define (main)
    (parse-args)
    (clean-dir (read-config-file (pathname-expand config-file)) #:dryrun dryrun))
  )
