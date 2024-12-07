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

  (define (time-format fstring)
    (time->string (seconds->local-time) fstring))

  (define (move-files target-dir files)
    (unless (eq? files '())
      (let ((target (pathname-replace-directory (car files)
                                                target-dir)))
        (if (file-exists? target)
            (print target " exists ... skipping")
            (begin (print (car files) " -> " target)
                   (move-file (car files) target))))
      (move-files target-dir (cdr files))))


  (define (apply-rules target-dir groups)
    (unless (eq? groups '())
      (let* ((group (car groups))
             (rule (cdar group))
             (files (cdr group))
             (target-dir (make-absolute-pathname target-dir
                                                 (alist-ref #:dir rule))))
        (create-directory target-dir #t)
        (move-files target-dir files))
      (apply-rules target-dir (cdr groups))))


  (define (clean-dir dir-rules)
    (unless (eq? dir-rules '())
      (let* ((rules (car dir-rules))
             (basedir (pathname-expand (alist-ref #:basedir rules)))
             (groupdefs (alist-ref #:groups rules))
             (target-prefix (alist-ref #:target-prefix rules))
             (target-prefix (if (string? target-prefix)
                                (time-format target-prefix)
                                ""))
             (target-dir (make-absolute-pathname basedir target-prefix))
             (groups (group-directory-files basedir groupdefs)))
        (print basedir " -> " target-dir)
        (map print groups)
        (apply-rules target-dir groups))
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
