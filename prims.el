;; -*- lexical-binding: t -*-

;;; Code:
(require 'cl)
(defvar *prims* nil)
(defun clear-prims () "Clear the primitives." (setq *prims* nil))

(defmacro prim (name prim-name arguments return-type body-expression)
  "Define a primitive."
  `(push
    '(:name ,(symbol-name name)
            :prim-name ,(concat "PRIM_" (symbol-name prim-name))
            :args ,arguments
            :return-type ,return-type
            :body ,body-expression)
    *prims*))

(defun flatten (list)
  "Flatten LIST one level."
  (mapcan (lambda (x) (if (consp x) (copy-list x) (list x))) list))

(defun tmpl (&rest args)
  "Templates ARGS as a string."
  (apply 'concat (mapcar (lambda (item) (if (stringp item) item (prin1-to-string item)))
                         (flatten args))))

(defun emit-arg (arg)
  "Emit ARG as an argument declaration."
  (let ((name (first arg))
        (type (second arg)))
    (tmpl "   VM_ARG(" type "," name ");
")))

(defun emit-body (body return-type)
  "Emit BODY and RETURN-TYPE."
  (if (eq return-type 'any)
      (tmpl " return " body ";")
    (tmpl "  return to("return-type",("body"));")))

(defun emit-prim-impl (prim)
  "Emits a primitive PRIM."
  (tmpl "Ptr " (getf prim :prim-name) "_impl(VM *vm) {
" (mapcar 'emit-arg (reverse (getf prim :args))) "
" (emit-body (getf prim :body) (getf prim :return-type)) "
}

"))

(defun emit-all-prim-impls ()
  "Emit all prims to a temp buffer."
  (let ((counter -1))
    (dolist (p *prims*)
      (insert (tmpl "// Primitive " (incf counter) "\n"))
      (insert (emit-prim-impl p)))))

(defun emit-prim-encoding (idx argc)
  "Emit encoding for prim at IDX with ARGC."
  (tmpl "("idx" << 32) | ("argc " << 16) | PRIM_TAG"))

(defun emit-prim-enum ()
  "Emit enum naming all prims."
  ;; TODO: embed PRIM mask and call count in counter number
  (let ((counter -1))
    (insert (tmpl "
typedef enum {
" (mapcar (lambda (p) (tmpl "  " (getf p :prim-name) " = "
                            (emit-prim-encoding
                             (incf counter)
                             (length (getf p :args)))
                             ",\n")) *prims*) "
  PRIM_UNUSED = 0
} PrimitiveOperations;

"))))

(defun emit-prim-table ()
  "Emit primitive lookup table."
  (insert
   (tmpl "
PrimitiveFunction PrimLookupTable[] = {
"(mapcar (lambda (p) (tmpl "  &" (getf p :prim-name) "_impl,\n")) *prims*)"
  (void *)0
};
")))

(progn
  (clear-prims)
  (prim + PLUS ((a Fixnum) (b Fixnum)) Fixnum "a + b")
  (prim - MINUS ((a Fixnum) (b Fixnum)) Fixnum "a - b")
  (prim * TIMES ((a Fixnum) (b Fixnum)) Fixnum "a * b")
  (prim / DIVIDE ((a Fixnum) (b Fixnum)) Fixnum "a / b")
  (prim < LT  ((a Fixnum) (b Fixnum)) Bool "a < b")
  (prim > GT  ((a Fixnum) (b Fixnum)) Bool "a > b")
  (prim % MOD ((a Fixnum) (b Fixnum)) Fixnum "a % b")
  (prim cons CONS ((a any) (b any)) any "cons(vm, a, b)")
  (prim print PRINT ((a any)) any "primitive_print(a)")
  (setf *prims* (reverse *prims*)))

(with-current-buffer "foo"
  (delete-region (point-min) (point-max))
  (emit-prim-enum)
  (emit-all-prim-impls)
  (emit-prim-table))


(provide 'prims)
;;; prims.el ends here
