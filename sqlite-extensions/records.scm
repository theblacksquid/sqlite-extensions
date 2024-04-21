;;  sqlite-extensions/records.scm --- SQLite3 extensions for SRFI-9 Records

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

;; This module exports the syntactic form 'define-sqlite-table-record-type'
;; which builds upon 'define-record-type' from (srfi srfi-9), mapping
;; the record type to a SQLite3 table, along with get, delete, upsert
;; procedures, along with some other auto-generated utilities.
;;
;; Usage example:
;;
;; (use-modules (sqlite-extensions base)
;;              (sqlite-extensions records))
;;
;; (define-sqlite-table-record-type test-table
;;     "TestTable"
;;     (make-test-table column1 column2 column3)
;;     test-table? 
;;     test-table-all test-table-by-id delete-test-table-by-id!
;;     (column1 get-column1 "column1" "INTEGER PRIMARY KEY NOT NULL")
;;     (column2 get-column2 "column2" "TEXT")
;;     (column3 get-column3 "column3" "INTEGER NOT NULL" set-column3!))
;;
;; (test-table-all "/some/sqlite-filename.db")
;;     -> list of test-table objects
;;
;; (test-table-by-id "/some/sqlite-filename.db" 3)
;;     -> return test-table with "column1" with value of 3 from "TestTable"
;;
;; (delete-test-table-by-id "/some/sqlite-filename.db" 3)
;;     -> unspecified, side-effect of deleting matching record from "TestTable"
;;
;; With above example, procedure 'make-test-table-from-db-row' is generated:
;; 
;; (make-test-table-from-db-row
;;     (query "/some/sqlite-filename.db"
;;            "SELECT * FROM TestTable WHERE column3 > ?"
;;            10))
;;     -> list of test-table objects
;;
;; test-table-table-definition
;;     -> variable containing string with the SQL table definition for "TestTable"
;;
;;; Code:

(define-module (sqlite-extensions records)
  #:use-module (srfi srfi-9)
  #:use-module (threading-macros)
  #:use-module (sqlite-extensions base)
  #:export (define-sqlite-table-record-type
	    define-sqlite-view-record-type))

(define sql-column-names
  (lambda (field-specs)
    (map (λ (field-spec)
	   (syntax-case field-spec ()
	     ((name getter column-name column-type) #'column-name)
	     ((name getter column-name column-type setter) #'column-name)))
	 field-specs)))

(define sql-record-fields
  (lambda (field-specs)
    (map (λ (field-spec)
	   (syntax-case field-spec ()
	     ((name getter column-name column-type) #'(column-name column-type))
	     ((name getter column-name column-type setter) #'(column-name column-type))))
	 field-specs)))

(define sql-view-column-names
  (lambda (field-specs)
    (map (λ (field-spec)
	   (syntax-case field-spec ()
	     ((name getter column-name) #'column-name)))
	 field-specs)))

(define get-constructor-name
  (lambda (constructor)
    (syntax-case constructor ()
      ((name field-name) #'name)
      ((name field-name ...) #'name))))

(define normal-getter-setter-fields
  (lambda (field-specs)
    (map (λ (field-spec)
	   (syntax-case field-spec ()
	     ((name getter column-name column-type) #'(name getter))
	     ((name getter column-name column-type setter) #'(name getter setter))))
	 field-specs)))

(define view-getter-fields
  (lambda (field-specs)
    (map (λ (field-spec)
	   (syntax-case field-spec ()
	     ((name getter column-name) #'(name getter))))
	 field-specs)))

(define (create-sql-table-definition table-name field-specs)
  (define column-name (λ (fs) (syntax-case fs () ((name type) #'name))))
  (define column-type (λ (fs) (syntax-case fs () ((name type) #'type))))
  (let* ((column-def-alist (sql-record-fields field-specs))
	 (column-definitions
	  (-> (λ (fs) (format #f "    ~a \t\t~a "
			      (syntax->datum (column-name fs))
			      (syntax->datum (column-type fs))))
	      (map column-def-alist)
	      (string-join ",\n"))))
    (format
     #f
"CREATE TABLE IF NOT EXISTS ~a
(
~a
) " table-name
  column-definitions)))

(define (create-sql-view-definition view-name view-query)
  (format
   #f
   "CREATE VIEW IF NOT EXISTS ~a
AS
     ~a  " view-name view-query))

(define-syntax from-db-row
  (lambda (x)
    (syntax-case x ()
      ((_ constructor column-name ...)
       #`(lambda (db-row)
	   (constructor
	    #,@(map (λ (col) #`(hash-ref db-row #,col)) #'(column-name ...))))))))

(define-syntax generic-sql-select-all
  (lambda (x) 
    (syntax-case x ()
      ((_ table-name constructor column-name ...)
       #`(lambda (db-filename)
	   (map
	    (λ (row) ((from-db-row constructor column-name ...) row))
	    (query db-filename (format #f "SELECT * FROM ~a" table-name))))))))

(define-syntax generic-sql-select-by-id
  (lambda (x)
    (syntax-case x ()
      ((_ table-name constructor id-column-name column-name ...)
       #`(lambda (db-filename id)
	   ((from-db-row constructor column-name ...)
	    (car (query db-filename
			(format #f "SELECT * FROM ~a WHERE ~a = ?"
				table-name
				id-column-name)
			id))))))))

(define-syntax generic-sql-delete-by-id
  (lambda (x)
    (syntax-case x ()
      ((_ table-name id-column-name)
       #`(lambda (db-filename id)
	   (execute-non-query!
	    db-filename (format #f "DELETE FROM ~a WHERE ~a = ?"
				table-name id-column-name) id))))))

(define-syntax %define-sqlite-table-record-type
  (lambda (x)
    (syntax-case x ()
      ((_ type-name table-name constructor predicate
	  all-selector by-id-selector by-id-deletor 
	  field-spec ...)
       (with-syntax
	   ((table-definition-accessor
	     (datum->syntax
	      #'table-name
	      (symbol-append (syntax->datum #'type-name) '-table-definition)))
	    (from-db-row-function
	     (datum->syntax
	      #'table-name
	      (symbol-append (syntax->datum (get-constructor-name #'constructor))
			     '-from-db-row))))
	 #`(begin
	     (define-record-type type-name
	       constructor
	       predicate
	       #,@(normal-getter-setter-fields #'(field-spec ...)))
	     (define table-definition-accessor
	       #,(create-sql-table-definition (syntax->datum #'table-name)
					      (syntax->datum #`(field-spec ...))))
	     (define all-selector
	       (generic-sql-select-all #,(syntax->datum #'table-name)
				       #,(get-constructor-name #'constructor)
				       #,@(sql-column-names #'(field-spec ...))))
	     (define by-id-selector
	       (generic-sql-select-by-id #,(syntax->datum #'table-name)
					 #,(get-constructor-name #'constructor)
					 #,(car (sql-column-names #'(field-spec ...)))
					 #,@(sql-column-names #'(field-spec ...))))
	     (define by-id-deletor
	       (generic-sql-delete-by-id #,(syntax->datum #'table-name)
					 #,(car (sql-column-names #'(field-spec ...)))))
	     (define from-db-row-function
	       (from-db-row #,(get-constructor-name #'constructor)
			    #,@(sql-column-names #'(field-spec ...))))))))))

(define-syntax %define-sqlite-view-record-type
  (lambda (x)
    (syntax-case x ()
      ((_ type-name
	  view-name constructor
	  predicate view-query
	  all-selector by-id-selector
	  field-spec ...)
       (with-syntax
	   ((view-definition-accessor
	     (datum->syntax
	      #'view-name
	      (symbol-append (syntax->datum #'type-name) '-view-definition)))
	    (from-db-row-function
	     (datum->syntax
	      #'view-name
	      (symbol-append (syntax->datum (get-constructor-name #'constructor))
			     '-from-db-row))))
	 #`(begin
	     (define-record-type type-name
	       constructor
	       predicate
	       #,@(view-getter-fields #'(field-spec ...)))
	     (define view-definition-accessor
	       #,(create-sql-view-definition (syntax->datum #'view-name)
					     (syntax->datum #'view-query)))
	     (define all-selector
	       (generic-sql-select-all #,(syntax->datum #'view-name)
				       #,(get-constructor-name #'constructor)
				       #,@(sql-view-column-names #'(field-spec ...))))
	     (define by-id-selector
	       (generic-sql-select-by-id #,(syntax->datum #'view-name)
					 #,(get-constructor-name #'constructor)
					 #,(car (sql-view-column-names #'(field-spec ...)))
					 #,@(sql-view-column-names #'(field-spec ...))))
	     (define from-db-row-function
	       (from-db-row #,(get-constructor-name #'constructor)
			    #,@(sql-view-column-names #'(field-spec ...))))))))))

(define-syntax-rule (define-sqlite-table-record-type type-name
	   table-name constructor predicate
	   all-selector by-id-selector by-id-deletor
	   field-spec ...)
  "A SQLite extension to srfi-9 record types.

-- Scheme Syntax: define-sqlite-table-record-type type-name
         table-name
         (constructor fieldname ...)
         predicate
         all-selector by-id-selector by-id-deletor
         (fieldname accessor sql-column-name sql-type-name [modifier]) ...

Create a new record type backed by a SQLite table, and make various
'define's for using it. This syntax can only occur at the top-level,
not nested within some other form. See the documentation for SRFI-9 in
the Guile Reference Manual for more details, especially for
CONSTRUCTOR, PREDICATE, ACCESSOR and MODIFIER.

ALL-SELECTOR is bound to a function to be called as '(ALL-SELECTOR
db-filename)' that returns all record objects mapped to the SQLite
table mapped to TYPE-NAME where DB-FILENAME is a string pointing to a
sqlite database.

BY-ID-SELECTOR is bound to a procedure to be called as '(BY-ID-SELECTOR
db-filename id)' that returns the record object matching ID

BY-ID-DELETOR is bound to a procedure to be called as '(BY-ID-SELECTOR
db-filename id)' that deletes the record object matching ID in the
SQlite table mapped to TYPE-NAME located in the database in
DB-FILENAME

One can view the generate table definition for the record type by
using the generated TYPE-NAME-table-definition variable.

The generated procedure CONSTRUCTOR-from-db-row can be used with 'query'
from (sqlite-extensions base) to automatically map the raw hash table
results into the record type, 

"
  (%define-sqlite-table-record-type
   type-name table-name constructor predicate
   all-selector by-id-selector by-id-deletor
   field-spec ...))

(define-syntax-rule (define-sqlite-view-record-type type-name
	  view-name constructor
	  predicate view-query
	  all-selector by-id-selector
	  field-spec ...)
  (%define-sqlite-view-record-type type-name
	  view-name constructor
	  predicate view-query
	  all-selector by-id-selector
	  field-spec ...))
