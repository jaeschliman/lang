;; -*- lexical-binding: t -*-

;;; Code:
(require 'cl)

(defvar *prims* nil)
(defvar *prim-current* nil)

(defmacro prim (name prim-name arguments return-type body-expression)
  "Defines a primitive.

  NAME is the symbol the primitive will be installed as.
  PRIM-NAME is a unique cpp-friendly name used in generating the prim enum.
  ARGUMENTS describe the argument names and types.
  RETURN-TYPE specifies the primitive return type of the BODY-EXPRESSION."
  `(push
    (list :name ,(symbol-name name)
          :prim-name ,(concat "PRIM_" (symbol-name prim-name))
          :args ',arguments
          :index (1+ (length *prims*))
          :return-type ',return-type
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
    (tmpl "   VM_ARG(\"" (getf *prim-current* :name) "\"," type "," name ");
")))

(defun emit-varargs (name)
  "Emit varargs binding form for NAME."
  (tmpl "Ptr " name " = vm_get_stack_values_as_list(vm, argc);"))

(defun emit-prim-args (prim)
  "Emits argument binding forms for PRIM."
  (let ((args (getf prim :args)))
    (if (and args (symbolp args))
        (emit-varargs args)
      (mapcar 'emit-arg (reverse args)))))

(defun emit-body (body return-type)
  "Emit BODY and RETURN-TYPE."
  (if (eq return-type 'any)
      (tmpl " return " body ";")
    (tmpl "  return to("return-type",("body"));")))


(defun emit-prim-impl (prim)
  "Emits a primitive PRIM."
  (let ((*prim-current* prim))
  (tmpl "Ptr " (getf prim :prim-name) "_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
" (emit-prim-args prim) "
" (emit-body (getf prim :body) (getf prim :return-type)) "
}

")))

(defun emit-inline-body (body return-type)
  "Emit BODY and RETURN-TYPE."
  (if (eq return-type 'any)
      (tmpl " vm_push(vm, " body ");")
    (tmpl "  vm_push(vm, to("return-type",("body")));")))

(defun emit-prim-impl-inline (prim)
  "Emits PRIM inline for switch statement."
  (let ((*prim-current* prim)
        (return-type (getf prim :return-type))
        (body (getf prim :body)))
    (tmpl "
  case " (getf prim :index) ": {
" (emit-prim-args prim) "
   " (emit-inline-body body return-type) "
    break;
  }
")))

(defun emit-prim-giant-switch (prims)
  "Emits a giant switch statement function for PRIMS."
(tmpl "
inline Ptr giant_switch(VM *vm, u32 argc, u32 idx) {
  switch(idx) {
   " (mapcar 'emit-prim-impl-inline prims) "
  }
  return Nil;
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
  (tmpl "(("idx"ULL << 32) | ("argc "ULL << 16) | PrimOp_Tag)"))

(defun get-arg-count (p)
  "Argument count for prim P."
  (let ((args (getf p :args)))
    (if (symbolp args) 255 (length args))))

(defun emit-prim-enum ()
  "Emit enum naming all prims."
  ;; TODO: embed PRIM mask and call count in counter number
  (let ((counter 0))
    (insert (tmpl "
enum PrimitiveOperation : u64 {
  PRIM_APPLY = PrimOp_Tag,
" (mapcar (lambda (p) (tmpl "  " (getf p :prim-name) " = "
                            (emit-prim-encoding
                             (incf counter)
                             (get-arg-count p))
                             ",\n")) *prims*) "
  PRIM_UNUSED = 0
};

"))))

(defun emit-prim-table ()
  "Emit primitive lookup table."
  (insert
   (tmpl "
PrimitiveFunction PrimLookupTable[] = {
  (PrimitiveFunction)(void *)0, // apply
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
  set_global(vm, \"apply\", to(PrimOp,  PRIM_APPLY));

" (mapcar 'emit-prim-registration *prims*)
"
}
")))

(defun write-prims ()
  "Write the prims to the buffer."
  ;; TODO: emit directly to file (run in batch mode)
  ;; TODO: would be nice to have a prim -> name lookup table for printing as well.
  (with-current-buffer "primop-generated.cpp"
    (delete-region (point-min) (point-max))
    (emit-prim-enum)
    (emit-all-prim-impls)
    (emit-prim-table)
    (emit-prim-registration-function)
    (insert (emit-prim-giant-switch *prims*))
    (save-buffer)))

(progn
  (setf *prims* nil)

  ;; SPECIAL BUILTINS (handled specially in VM)

  ;; apply ;; 0
  (prim @send          SEND     unused any "unused") ;; 1
  (prim sleep-ms       SLEEP_MS unused any "unused") ;; 2
  (prim semaphore-wait SEM_WAIT unused any "unused") ;; 3
  (prim kill-thread    KILL_THD unused any "unused") ;; 4
  (prim -unused0       UNUS0    unused any "unused") ;; 5
  (prim -unused1       UNUS1    unused any "unused") ;; 6
  (prim -unused2       UNUS2    unused any "unused") ;; 7

  ;; NORMAL BUILTINS

  (prim compile-to-closure CMPC ((expr any)) any "compile_to_closure(vm, expr)")

  (prim class-of CLASSOF ((a any)) any "class_of(vm, a)")
  (prim create-class MKCLASS
        ((name any) (ivars Fixnum)) any "make_user_class(vm, name, ivars)")
  (prim class-set-method SETMETHOD
        ((a Standard) (sym any) (fn any)) any "class_set_method(vm, a, sym, fn)")
  (prim class-get-metadata CLSGETMETA
        ((a Standard) (key any)) any "class_get_metadata(a, key)")
  (prim class-set-metadata CLSSETMETA
        ((a Standard) (key any) (val any)) any "class_set_metadata(vm, a, key, val)")
  (prim instantiate-class MKINST
        ((klass Standard)) any "instantiate_user_class(vm, klass)")
  (prim instance-get-ivar IVAR_GET
        ((obj Standard) (idx Fixnum)) any "standard_object_get_ivar(obj, idx)")
  (prim instance-set-ivar IVAR_SET
        ((obj Standard) (idx Fixnum) (val any)) any
        "standard_object_set_ivar(obj, idx, val)")


  (prim class?        IS_CLSS ((a any)) Bool "is_class(a)")
  (prim bool?         IS_BOOL ((a any)) Bool "is(Bool, a)")
  (prim closure?      IS_CLS  ((a any)) Bool "is(Closure, a)")
  (prim primop?       IS_PRIM ((a any)) Bool "is(PrimOp, a)")
  (prim float?        IS_FLT  ((a any)) Bool "is(Float, a)")
  (prim fixnum?       IS_INT  ((a any)) Bool "is(Fixnum, a)")
  (prim pair?         PAIR_Q  ((a any)) Bool "is(cons, a)")
  (prim nil?          ISNIL   ((a any)) Bool "isNil(a)")
  (prim ht?           IS_HT   ((a any)) Bool "is(ht, a)")
  (prim array?        IS_ARY  ((a any)) Bool "is(Array, a)")
  (prim symbol?       SYM_Q   ((a any)) Bool "is(Symbol, a)")
  (prim char?         ISCHAR  ((a any)) Bool "is(Char, a)")
  (prim image?        ISIMAGE ((a any)) Bool "is(Image, a)")
  (prim point?        IS_PT   ((a any)) Bool "is(Point, a)")
  (prim continuation? IS_CONT ((a any)) Bool "is(cont, a)")
  (prim string?       STR_P   ((a any)) Bool "is(String, a)")
  (prim semaphore?    IS_SEM  ((a any)) Bool "is(semaphore, a)")
  (prim thread?       IS_THD  ((a any)) Bool "is(thread, a)")

  (prim +i    FIX_PLUS   ((a Fixnum) (b Fixnum)) Fixnum "a + b")
  (prim -i    FIX_MINUS  ((a Fixnum) (b Fixnum)) Fixnum "a - b")
  (prim *i    FIX_TIMES  ((a Fixnum) (b Fixnum)) Fixnum "a * b")
  (prim /i    FIX_DIVIDE ((a Fixnum) (b Fixnum)) Fixnum "a / b")
  (prim <i    FIX_LT     ((a Fixnum) (b Fixnum)) Bool   "a < b")
  (prim >i    FIX_GT     ((a Fixnum) (b Fixnum)) Bool   "a > b")
  (prim %i    FIX_MOD    ((a Fixnum) (b Fixnum)) Fixnum "a % b")

  (prim +f    FLT_PLUS   ((a Float) (b Float))   Float  "a + b")
  (prim -f    FLT_MINUS  ((a Float) (b Float))   Float  "a - b")
  (prim *f    FLT_TIMES  ((a Float) (b Float))   Float  "a * b")
  (prim /f    FLT_DIVIDE ((a Float) (b Float))   Float  "a / b")
  (prim <f    FLT_LT     ((a Float) (b Float))   Bool   "a < b")
  (prim >f    FLT_GT     ((a Float) (b Float))   Bool   "a > b")
  (prim %f    FLT_MOD    ((a Float) (b Float))   Float  "fmodf(a, b)")

  (prim i->f  FIX_TO_FLT ((a Fixnum))            Float  "(f32)a")
  (prim f->i  FLT_TO_FIX ((a Float))             Fixnum "(s64)a")

  (prim list  LIST       list                    any    "list")
  (prim cons  CONS       ((a any) (b any))       any    "cons(vm, a, b)")
  (prim car   CAR        ((a any))               any    "car(a)")
  (prim cdr   CDR        ((a any))               any    "cdr(a)")
  (prim eq    EQ         ((a any) (b any))       Bool   "ptr_eq(a, b)")
  (prim not   NOT        ((a any))               Bool   "a == False")
  (prim print PRINT      ((a any))               any    "primitive_print(a)")
  (prim nth   NTH        ((a any) (idx Fixnum))  any    "nth_or_nil(a, idx)")

  (prim make-array MKARY ((len Fixnum))                        any "make_zf_array(vm, len)")
  (prim aget       AGET  ((a PtrArray) (idx Fixnum))           any "aget(a, idx)")
  (prim aset       ASET  ((a PtrArray) (idx Fixnum) (val any)) any "aset(a, idx, val)")

  (prim make-ht   MK_HT     ()                             any "ht(vm)")
  (prim make-st   MK_ST     ()                             any "string_table(vm)")
  (prim ht-at     HT_AT     ((ht any) (key any))           any "ht_at(ht, key)")
  (prim ht-at-put HT_AT_PUT ((ht any) (key any) (val any)) any "ht_at_put(vm, ht, key, val)")

  (prim set-symbol-value SET_SYM_VAL ((a any) (b any)) any
        "set_symbol_value(vm, a, b)")
  (prim mark-symbol-as-special SET_SYM_SPECIAL ((a any)) any
        "mark_symbol_as_special(vm, a)")

  (prim gensym           GSYM        ()                   any  "make_symbol(vm, \"_gensym_\")")
  (prim intern           INTERN      ((a String))         any  "intern(vm, a)")

  (prim print-stacktrace PRINT_STACK () any "vm_print_stack_trace(vm)")
  (prim debug-stacktrace DBG_STACK   () any "vm_print_debug_stack_trace(vm)")

  (prim set-pixel  SETPXL  ((p Point))             any    "gfx_set_pixel(vm, p)")
  (prim point+     PTPLUS  ((a Point) (b Point))   Point  "a + b")
  (prim point-     PTMINUS ((a Point) (b Point))   Point  "a - b")
  (prim make-point MKPOINT ((a Fixnum) (b Fixnum)) Point  "(point){(s32)a, (s32)b}")
  (prim point-x    PTX     ((p Point))             Fixnum "(s64)p.x")
  (prim point-y    PTY     ((p Point))             Fixnum "(s64)p.y")
  (prim point-rotate PTROT ((p Point) (degrees Float)) Point "rotate_point(p, degrees)")

  (prim screen-fill-rect SFILLRCT ((a Point) (b Point) (color Fixnum)) any "gfx_screen_fill_rect(vm, a, b, color)")
  (prim blit-to-screen DRAWIMAGE ((img Image) (p Point) (scale Fixnum) (rot Fixnum)) any "gfx_blit_image_at(vm, img, p, scale, rot)")

  (prim fill-rect FILLRCT ((dst Image) (a Point) (b Point) (color Fixnum)) any
        "gfx_fill_rect(dst, a, b, color)")
  (prim clear-rect CLRRCT ((dst Image) (a Point) (b Point)) any
        "gfx_clear_rect(dst, a, b)")
  (prim blit BLT
        ((src Image) (dst Image) (at Point) (ul Point) (lr Point)
         (scale Float) (degrees_rotation Float))
        any
        "gfx_blit(src, dst, at, ul, lr, scale, degrees_rotation)")
  (prim blit-with-mask BLT_M
        ((src Image) (dst Image) (msk Image) (at Point)
         (src_ul Point) (src_lr Point) (src_scale Float) (src_rot Float)
         (msk_ul Point) (msk_lr Point) (msk_scale Float) (msk_rot Float))
        any
        "gfx_blit_image_with_mask(src, dst, msk, at,
  points_to_rect(src_ul, src_lr), src_scale, src_rot,
  points_to_rect(msk_ul, msk_lr), msk_scale, msk_rot
)")

  (prim blit-from-screen BLT_FR_SCRN
        ((dst Image) (at Point) (ul Point) (lr Point)
         (scale Float) (degrees_rotation Float))
        any
        "gfx_blit_from_screen(vm, dst, at, ul, lr, scale, degrees_rotation)")

  (prim load-image LOADIMAGE ((path String))         any "gfx_load_image(vm, path)")
  (prim make-image MKIMAGE   ((w Fixnum) (h Fixnum)) any "gfx_make_image(vm, w, h)")

  (prim image-width  IMG_W ((img Image)) Fixnum "image_width(img)")
  (prim image-height IMG_H ((img Image)) Fixnum "image_height(img)")

  (prim char-code-at CCA   ((str String) (idx Fixnum)) Fixnum "string_char_code_at(vm, str, idx)")
  (prim char-code    CC    ((ch Char))                 Fixnum "(s64)ch")
  (prim char-at      CH_AT ((str String) (idx Fixnum)) Char   "string_char_at(vm, str, idx)")
  (prim char-at-put  CH_ATP ((str String) (idx Fixnum) (ch Char)) any
        "string_set_char_at(vm, str, idx, ch)")
  (prim char-<       CH_LT ((a Char) (b Char))         Bool   "a < b")
  (prim char->       CH_GT ((a Char) (b Char))         Bool   "a > b")

  (prim char-by-name CHNM ((name String)) any "character_by_name(name)")

  (prim make-string MKSTR ((len Fixnum) (ch Char)) any "make_filled_string(vm, len, ch)")
  (prim string-length STRLEN ((str String)) Fixnum "string_length(str)")

  (prim set-stack-mark SSTKMARK ((m any)) any "vm_set_stack_mark(vm, m)")
  (prim snapshot-to-stack-mark PSTKMARK ((m any) (v any)) any "vm_abort_to_mark(vm, m, v)")
  (prim resume-stack-snapshot RSTKSNAP ((s any) (arg any)) any "vm_resume_stack_snapshot(vm, s, arg)")
  (prim continuation-value CONT_VAL ((a any)) any "cont_get_value(a)")
  (prim fork-continuation FORK ((priority any) (a any)) any "vm_schedule_cont(vm, a, priority, Nil)")

  (prim make-semaphore MK_SEM ((a Fixnum)) any "make_semaphore(vm, to(Fixnum, a))")
  (prim signal-semaphore SEM_SIG ((a any)) any "signal_semaphore(a)")

  (prim current-thread CURR_THD () any "vm->globals->current_thread")

  (prim slurp SLURP ((path String)) any "slurp(vm, path)")

  (setf *prims* (reverse *prims*))
  (write-prims))


(provide 'prims)
;;; prims.el ends here
