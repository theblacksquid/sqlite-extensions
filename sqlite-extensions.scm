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

(define-module (sqlite-extensions)
  #:use-module (sqlite-extensions base)
  #:use-module (sqlite-extensions records)
  #:re-export (query
	       execute-non-query!
	       execute-batch!
	       execute-scalar
	       table-exists?
	       define-sqlite-table-record-type))

