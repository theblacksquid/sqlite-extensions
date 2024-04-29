(define-module (sqlite-extensions threading-macros)
  #:export (->> ->))

;; threading macros based on a thread from the guile mailing list[1]
;; [1]: https://lists.gnu.org/archive/html/guile-user/2019-07/msg00006.html

(define-syntax ->
  (syntax-rules ()
    ((-> exp)
     exp)
    ((-> exp ... (op args ...))
     (op (-> exp ...) args ...))))

(define-syntax ->>
  (syntax-rules ()
    ((->> exp)
     exp)
    ((->> exp ... (op args ...))
     (op args ... (->> exp ...)))))
