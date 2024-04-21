;;  sqlite-extensions/base.scm --- SQLite3 Utilities Library

;; Copyright (C) 2024 Aeron Paul Sioson <theblacksquid@protonmail.com>
;; 
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;
;;; Code:

(define-module (sqlite-extensions base)
  #:use-module ((sqlite3) #:prefix sql:)
  #:export (query
	    execute-non-query!
	    execute-batch!
	    execute-scalar
	    table-exists?))

(define call-with-sqlite3
  (lambda (db-filename proc)
    (let* ((sqlitedb (sql:sqlite-open db-filename))
	   (result (proc sqlitedb)))
      (sql:sqlite-close sqlitedb)
      result)))

(define row-to-hashmap
  (lambda (columns row)
    (let ((result (make-hash-table)))
      (for-each (λ (col cell) (hash-set! result col cell))
		columns row)
      result)))

(define resultset-to-hashmaps
  (lambda (columns result-list)
    (map (λ (row) (row-to-hashmap columns (vector->list row)))
	 result-list)))

(define query
  (lambda (db-filename sql . args)
    (call-with-sqlite3
     db-filename
     (lambda (db-connection)
       (let* ((statement (sql:sqlite-prepare db-connection sql))
	      (columns (vector->list (sql:sqlite-column-names statement))))
	 (begin
	   (if (> (length args) 0)
	       (apply sql:sqlite-bind-arguments statement args))
	   (let loop ((result '())
		      (next (sql:sqlite-step statement)))
	     (if next
		 (loop (cons next result)
		       (sql:sqlite-step statement))
		 (begin (sql:sqlite-finalize statement)
			(resultset-to-hashmaps columns result))))))))))

(define execute-non-query!
  (lambda (db-filename sql . args)
    (call-with-sqlite3
     db-filename
     (lambda (db-connection)
       (let ((statement (sql:sqlite-prepare db-connection sql))
	     (enable-foreign-keys
	      (sql:sqlite-prepare db-connection "PRAGMA foreign_keys = ON")))
	 (begin
	   (if (> (length args) 0)
	       (apply sql:sqlite-bind-arguments statement args))
	   (sql:sqlite-step enable-foreign-keys)
	   (sql:sqlite-step statement)
	   (sql:sqlite-finalize statement)
	   (sql:sqlite-finalize enable-foreign-keys)))))))

(define execute-scalar
  (lambda (db-filename sql . args)
    (call-with-sqlite3
     db-filename
     (lambda (db-connection)
       (let ((statement (sql:sqlite-prepare db-connection sql)))
	 (begin
	   (if (> (length args) 0)
	       (apply sql:sqlite-bind-arguments (cons statement args)))
	   (let ((result (sql:sqlite-step statement)))
	     (begin (sql:sqlite-finalize statement)
		    (vector-ref result 0)))))))))

(define execute-batch!
  (lambda (db-filename sql-forms)
    (call-with-sqlite3
     db-filename
     (lambda (db-connection)
       (for-each
	(λ (sql-form)
	  (let* ((command (car sql-form))
		 (statement (sql:sqlite-prepare db-connection command)))
	    (begin
	      (if (> (length (cdr sql-form)) 0)
		  (apply sql:sqlite-bind-arguments (cons statement (cdr sql-form))))
	      (sql:sqlite-step statement)
	      (sql:sqlite-finalize statement))))
	sql-forms)))))

(define table-exists?
  (lambda (db-filename tablename)
    (> (execute-scalar
	db-filename
	(string-append
	 "SELECT EXISTS "
	 "( "
	 "    SELECT name "
	 "    FROM sqlite_schema "
	 "    WHERE type='table' "
	 "    AND name=? "
	 ") ")
	tablename) 0)))
