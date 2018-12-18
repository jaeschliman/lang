;; -*- lexical-binding: t -*-

;;; Code:
(require 'cl)

(defvar *prims* nil)

(defmacro prim (name prim-name arguments return-type body-expression)
  "Defines a primitive.

  NAME is the symbol the primitive will be installed as.
  PRIM-NAME is a unique cpp-friendly name used in generating the prim enum.
  ARGUMENTS describe the argument names and types.
  RETURN-TYPE specifies the primitive return type of the BODY-EXPRESSION."
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

(defun tmpl-repr (item)
  "The template representation of ITEM."
  (if (stringp item)
      item
    (if item (prin1-to-string item) "")))

(defun tmpl (&rest args)
  "Templates ARGS as a string."
  (apply 'concat (mapcar #'tmpl-repr (flatten args))))

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
  (tmpl "(("idx"ULL << 32) | ("argc "ULL << 16) | PRIM_TAG)"))

(defun emit-prim-enum ()
  "Emit enum naming all prims."
  ;; TODO: embed PRIM mask and call count in counter number
  (let ((counter -1))
    (insert (tmpl "
enum PrimitiveOperation : u64 {
" (mapcar (lambda (p) (tmpl "  " (getf p :prim-name) " = "
                            (emit-prim-encoding
                             (incf counter)
                             (length (getf p :args)))
                             ",\n")) *prims*) "
  PRIM_UNUSED = 0
};

"))))

(defun emit-prim-table ()
  "Emit primitive lookup table."
  (insert
   (tmpl "
PrimitiveFunction PrimLookupTable[] = {
"(mapcar (lambda (p) (tmpl "  &" (getf p :prim-name) "_impl,\n")) *prims*)"
  (PrimitiveFunction)(void *)0
};
")))

(defun emit-prim-registration (p)
  "String for registration of primitive P."
  (tmpl "  set_global(vm, \"" (getf p :name) "\", to(PrimOp, "(getf p :prim-name)"));\n" ))

(defun emit-prim-registration-function ()
  "Emits the function which registers the primitives under symbol names in the VM."
  (insert (tmpl "
void initialize_primitive_functions(VM *vm) {

" (mapcar 'emit-prim-registration *prims*)
"
}
")))

(progn
  (setf *prims* nil)

  (prim +     PLUS   ((a Fixnum) (b Fixnum)) Fixnum "a + b")
  (prim -     MINUS  ((a Fixnum) (b Fixnum)) Fixnum "a - b")
  (prim *     TIMES  ((a Fixnum) (b Fixnum)) Fixnum "a * b")
  (prim /     DIVIDE ((a Fixnum) (b Fixnum)) Fixnum "a / b")
  (prim <     LT     ((a Fixnum) (b Fixnum)) Bool   "a < b")
  (prim >     GT     ((a Fixnum) (b Fixnum)) Bool   "a > b")
  (prim %     MOD    ((a Fixnum) (b Fixnum)) Fixnum "a % b")
  (prim cons  CONS   ((a any) (b any))       any    "cons(vm, a, b)")
  (prim car   CAR    ((a any))               any    "car(vm, a)")
  (prim cdr   CDR    ((a any))               any    "cdr(vm, a)")
  (prim eq    EQ     ((a any) (b any))       Bool   "ptr_eq(a, b)")
  (prim print PRINT  ((a any))               any    "primitive_print(a)")

  (prim set-symbol-value SET_SYM_VAL ((a Symbol) (b any)) any "set_global(vm, objToPtr(a), b)")
  (prim print-stacktrace PRINT_STACK () any "vm_print_stack_trace(vm)")
  (prim debug-stacktrace DBG_STACK () any "vm_print_debug_stack_trace(vm)")

  (setf *prims* (reverse *prims*)))

;; TODO: emit directly to file (run in batch mode)
;; TODO: would be nice to have a prim -> name lookup table for printing as well.
(with-current-buffer "primop-generated.cpp"
  (delete-region (point-min) (point-max))
  (emit-prim-enum)
  (emit-all-prim-impls)
  (emit-prim-table)
  (emit-prim-registration-function))


(provide 'prims)
;;; prims.el ends here