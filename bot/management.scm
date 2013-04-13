;; Copyright (C) 2013 Andreas W (add^_)
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.


(define-module (bot management)
  #:version (0 0 2)
  #:use-module (gdbm)
  #:export (*default-filename*
            open-database
            close-database
            backup-database
            push-to-database!
            set-key-in-database!
            delete-key-from-database!
            get-value-from-database
            list-keys-from-database))

(define *default-filename* "bot-data.db")

;; TODO 
;; Change the path to something less dynamic, e.g we don't want to
;; create a new bot-data.db everytime we call this function from
;; another directory..
(define* (open-database #:key (filename *default-filename*))
  "Opens/Creates a database, with read and write permissions."
  (let ((path-to-file (string-concatenate (list (getcwd) "/" filename))))
    (gdbm-open path-to-file GDBM_WRCREAT)))

(define* (close-database db)
  "Close the database."
  (gdbm-close db))

(define* (backup-database #:key (filename *default-filename*))
  "Make a backup of the database."
  (let* ((formated-time (strftime "%F-%H-%M" (localtime (current-time))))
         (old-filename (string-concatenate (list (getcwd) "/" filename)))
         (new-filename (string-concatenate (list (getcwd) "/" filename "-" formated-time))))
    (copy-file old-filename new-filename)))

(define (set-key-in-database! database key value)
  (when (and (gdbm-db? database)
             (string? key)
             (string? value))
    (gdbm-set! database key value)
    "Key set!"))

(define (push-to-database! database key value)
  (when (and (gdbm-db? database)
             (string? key)
             (string? value))
    (if (gdbm-contains? database key)
        (begin
          (gdbm-set! database key (string-append (gdbm-ref database key)
                                                 ";; "
                                                 value))
          "Added to key!")
        (begin
          (gdbm-set! database key value)
          "Added key!"))))

(define (delete-key-from-database! database key)
  (when (gdbm-db? database)
    (if (gdbm-contains? database key)
        (begin
          (gdbm-delete! database key)
          "Key deleted!")
        "Error: No such key")))

(define (get-value-from-database database key)
  (when (gdbm-db? database)
    (if (gdbm-contains? database key)
        (gdbm-ref database key)
        "Error: No such key")))

(define (list-keys-from-database database)
  (when (gdbm-db? database)
    (string-drop
     (string-concatenate
      (map car
           (gdbm-fold (lambda (key value old)
                        (cons (cons (string-append " " key)
                                    value) old))
                      '()
                      database)))
     1)))
