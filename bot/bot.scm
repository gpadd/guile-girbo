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


(define-module (bot bot)
  #:version (0 0 2)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (irc irc)
  #:use-module (irc handlers)
  #:use-module ((irc message)
                #:renamer (symbol-prefix-proc 'msg:))
  #:use-module (bot management)
  #:use-module (gdbm)
  #:export (make-irc-instance
            do-start-bot
            make-your-choice
            install-command-hook!
            install-regexp-command-hook!))

(define-syntax-rule (make-irc-instance instance-name nick realname server port hostname)
  (define instance-name
    (make-irc #:nick nick
              #:realname realname
              #:server server
              #:port port
              #:hostname hostname)))

(define* (do-start-bot bot-instance #:key (db (open-database)) (room "#leetbots") (pass ""))
  "Start the bot, load the database and make a backup it."
  (when (file-exists? *default-filename*)
    (format #t "Backing up file: ~a~%" *default-filename*)
    (backup-database #:filename *default-filename*))
  (install-ping-handler! bot-instance)
  (install-command-hook! bot-instance db)
  (install-regexp-command-hook! bot-instance db)
  (install-printer! bot-instance)
  (do-connect bot-instance)
  (do-wrap-port/tls bot-instance)
  (do-register bot-instance)
  (sleep 1)
  (do-join bot-instance room pass)
  (catch #t
    (lambda ()
      (do-runloop bot-instance))
    (lambda (key . args)
      (gdbm-close db)
      (do-close bot-instance))))

(define (make-your-choice bot-instance database strng-lst)
  (let* ((key (car strng-lst))
         (rest (cdr strng-lst)))
    (cond ((or (string-ci=? key "a")
               (string-ci=? key "add"))
           (push-to-database! database
                              (car rest)
                              (string-concatenate
                               (map (lambda (strng)
                                      (string-append strng " "))
                                    (cdr rest)))))
          ((or (string-ci=? key "s")
               (string-ci=? key "set")
               (string-ci=? key "!"))
           (set-key-in-database! database
                                 (car rest)
                                 (string-concatenate
                                  (map (lambda (strng)
                                         (string-append strng " "))
                                       (cdr rest)))))
          ((or (string-ci=? key "r")
               (string-ci=? key "read"))
           (if (pair? rest)
               (get-value-from-database database (car rest))
               "Not a pair."))
          ((or (string-ci=? key "d")
               (string-ci=? key "delete"))
           (if (pair? rest)
               (delete-key-from-database! database (car rest))
               "Not a pair."))
          ((or (string-ci=? key "l")
               (string-ci=? key "list"))
           (list-keys-from-database database))
          ((or (string-ci=? key "m")
               (string-ci=? key "more"))
           "Not available yet.")
          ((or (string-ci=? key "h")
               (string-ci=? key "help")
               (string-ci=? key "?"))
           "a = add key value, s = set key value, r = read key, d = delete key, l = list, b = backup, q = quit")
          ((or (string-ci=? key "b")
               (string-ci=? key "backup"))
           (backup-database)
           "Backing up.")
          ((or (string-ci=? key "q")
               (string-ci=? key "quit"))
           (begin
             (gdbm-close database)
             (do-quit bot-instance)))
          (else "Not a valid command!"))))

(define* (install-command-hook! obj database #:key (prefix (nick obj)) (command ":"))
  (let ((handler
         (lambda (msg)
           (let* ((body (msg:trailing msg))
                  (food (cdr (string-split body #\space)))
                  (key (string-append prefix command)))
             (when (and body (string=? (car (string-split body #\space)) key))
               (do-privmsg obj (msg:parse-target msg) (make-your-choice obj database food)))))))
    (add-simple-message-hook! obj handler #:command 'PRIVMSG #:tag 'cmd)))

(define* (install-regexp-command-hook! obj database #:key (prefix ",,"))
  (let ((handler
         (lambda (msg)
           (let* ((body (msg:trailing msg))
                  (s (string-match
                      (string-concatenate (string-split body #\,))
                      (list-keys-from-database database)))
                  (key (string-append prefix (if s
                                                 (match:substring s)
                                                 ""))))
             (when (and body (string=? body key))
               (do-privmsg obj
                           (msg:parse-target msg)
                           (if s
                               (get-value-from-database database
                                                        (match:substring s))
                               "Naaah")
                           ))))))
    (add-simple-message-hook! obj handler #:command 'PRIVMSG #:tag 'regexp-cmd)))

