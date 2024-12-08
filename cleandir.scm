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
    (chicken file))

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

(module checkdups
    (remove-duplicates)

  (import
    (scheme)
    (srfi-1)
    (chicken base)
    (chicken file)
    (chicken file posix)
    (chicken pathname)
    (pathname-expand)
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


  (define (get-files dir)
    (filter-map (lambda (x) (let ((file (make-absolute-pathname dir x)))
                         (and (not (directory? file))
                              file)))
                (directory dir)))

  (define (sum-cons x)
    (cons (file-md5sum x)
          x))

  ;; Returns a list of duplicate files in directory files.
  (define (directories-duplicates list)
    (let* ((expdirs (map pathname-expand list))
           (files (append-map get-files expdirs)))
      (get-duplicates (map sum-cons files) '())))

  (define (remove-files-in-list list)
    (unless (eq? list '())
      (print "delete duplicate:" (car list))
      (delete-file* (car list))
      (remove-files-in-list (cdr list))))

  ;; Takes a list of directories, checks for duplicate files and
  ;; removes them so only one copy of each remains.
  ;; The first copy encountered is the one that will survive.
  ;; TODO: Add some more specific control over which copy is kept.
  (define (remove-duplicates dirlist)
    (let ((dup-list (append-map
                     (lambda (x)
                       ;; 1. Element is always md5sum
                       ;; 2. Element is first version of the file
                       ;; 3. to last element are files to delete
                       (cddr x))
                     (directories-duplicates dirlist))))
      (remove-files-in-list dup-list)))

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
    (map (lambda (x) (cons  (assoc (car x) groups)
                       (cdr x)))
         (group-files (map (lambda (x) (car x)) groups)
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

  (define (move-files files)
    (unless (eq? files '())
      (let ((target (car files)))
        (cond
         ;; relevant if source file was deleted as a duplicate
         ((not (file-exists? (car target)))
          (move-files (cdr files)))
         ;; relevant if a different file with the same name exists
         ((file-exists? (cdr target))
          (print (cdr target) " exists ... renaming")
          (move-files (cons
                       (cons (car target)
                             (pathname-replace-file
                              (cdr target)
                              (string-append (pathname-file (cdr target))
                                             "-dup")))
                       (cdr files))))
         ;; finally move file
         (else (print (car target) " -> " (cdr target))
               (move-file (car target) (cdr target))
               (move-files (cdr files)))))))


  (define (apply-rules basedir target-dir groups dups)
    (unless (eq? groups '())
      (let* ((group (car groups))
             (rule (cdar group))
             (files (cdr group))
             (target-dir (time-format
                          (make-absolute-pathname target-dir
                                                  (alist-ref #:dir rule))))
             (targets (make-target-pairs target-dir files '())))
        (create-directory target-dir #t)
        (when dups (remove-duplicates (list basedir target-dir)))
        (move-files targets))
      (apply-rules basedir target-dir (cdr groups) dups)))


  (define (clean-dir dir-rules)
    (unless (eq? dir-rules '())
      (let* ((rules (car dir-rules))
             (basedir (pathname-expand (alist-ref #:basedir rules)))
             (target-prefix (alist-ref #:target-prefix rules))
             (target-prefix (if (string? target-prefix) target-prefix ""))
             (target-dir (make-absolute-pathname basedir target-prefix))
             (groups (group-directory-files basedir (alist-ref #:groups rules))))
        (print basedir " -> " target-dir)
        (map print groups)
        (apply-rules basedir target-dir groups (alist-ref #:dups rules)))
      (clean-dir (cdr dir-rules))))
  )

(module cmd-args
    (parse-args
     config-file)
  (import
    (scheme)
    (chicken base)
    (chicken file)
    (pathname-expand)
    (chicken pathname)
    (chicken process-context)
    (args))


  (define-constant default-config (get-environment-variable "CONFIG_PATH"))

  (define config-file  "~/.config/cleandir/config.toml")
  (define options
    (list
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
    ()

  (import
    (cmd-args)
    (readconfig)
    (pathname-expand)
    (cleandir))

  (parse-args)
  (clean-dir (read-config-file (pathname-expand config-file)))
  )
