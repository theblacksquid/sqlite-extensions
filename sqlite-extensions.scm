(define-module (sqlite-extensions)
  #:use-module (sqlite-extensions base)
  #:use-module (sqlite-extensions records)
  #:re-export (query
	       execute-non-query!
	       execute-batch!
	       execute-scalar
	       table-exists?
	       define-sqlite-table-record-type))

