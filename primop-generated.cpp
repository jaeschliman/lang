
enum PrimitiveOperation : u64 {
  PRIM_PLUS = ((0ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MINUS = ((1ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_TIMES = ((2ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_DIVIDE = ((3ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_LT = ((4ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_GT = ((5ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MOD = ((6ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CONS = ((7ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CAR = ((8ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CDR = ((9ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_EQ = ((10ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_ISNIL = ((11ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_NOT = ((12ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PRINT = ((13ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_NTH = ((14ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SET_SYM_VAL = ((15ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PRINT_STACK = ((16ULL << 32) | (0ULL << 16) | PrimOp_Tag),
  PRIM_DBG_STACK = ((17ULL << 32) | (0ULL << 16) | PrimOp_Tag),
  PRIM_SETPXL = ((18ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FILLRCT = ((19ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_PTPLUS = ((20ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PTMINUS = ((21ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MKPOINT = ((22ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PTX = ((23ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PTY = ((24ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_DRAWIMAGE = ((25ULL << 32) | (4ULL << 16) | PrimOp_Tag),
  PRIM_LOADIMAGE = ((26ULL << 32) | (1ULL << 16) | PrimOp_Tag),

  PRIM_UNUSED = 0
};

// Primitive 0
Ptr PRIM_PLUS_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Fixnum,(a + b));
}

// Primitive 1
Ptr PRIM_MINUS_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Fixnum,(a - b));
}

// Primitive 2
Ptr PRIM_TIMES_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Fixnum,(a * b));
}

// Primitive 3
Ptr PRIM_DIVIDE_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Fixnum,(a / b));
}

// Primitive 4
Ptr PRIM_LT_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Bool,(a < b));
}

// Primitive 5
Ptr PRIM_GT_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Bool,(a > b));
}

// Primitive 6
Ptr PRIM_MOD_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Fixnum,(a % b));
}

// Primitive 7
Ptr PRIM_CONS_impl(VM *vm) {
   VM_ARG(any,b);
   VM_ARG(any,a);

 return cons(vm, a, b);
}

// Primitive 8
Ptr PRIM_CAR_impl(VM *vm) {
   VM_ARG(any,a);

 return car(a);
}

// Primitive 9
Ptr PRIM_CDR_impl(VM *vm) {
   VM_ARG(any,a);

 return cdr(a);
}

// Primitive 10
Ptr PRIM_EQ_impl(VM *vm) {
   VM_ARG(any,b);
   VM_ARG(any,a);

  return to(Bool,(ptr_eq(a, b)));
}

// Primitive 11
Ptr PRIM_ISNIL_impl(VM *vm) {
   VM_ARG(any,a);

  return to(Bool,(isNil(a)));
}

// Primitive 12
Ptr PRIM_NOT_impl(VM *vm) {
   VM_ARG(any,a);

  return to(Bool,(a == False));
}

// Primitive 13
Ptr PRIM_PRINT_impl(VM *vm) {
   VM_ARG(any,a);

 return primitive_print(a);
}

// Primitive 14
Ptr PRIM_NTH_impl(VM *vm) {
   VM_ARG(Fixnum,idx);
   VM_ARG(any,a);

 return nth_or_nil(a, idx);
}

// Primitive 15
Ptr PRIM_SET_SYM_VAL_impl(VM *vm) {
   VM_ARG(any,b);
   VM_ARG(Symbol,a);

 return set_global(vm, objToPtr(a), b);
}

// Primitive 16
Ptr PRIM_PRINT_STACK_impl(VM *vm) {

 return vm_print_stack_trace(vm);
}

// Primitive 17
Ptr PRIM_DBG_STACK_impl(VM *vm) {

 return vm_print_debug_stack_trace(vm);
}

// Primitive 18
Ptr PRIM_SETPXL_impl(VM *vm) {
   VM_ARG(Point,p);

 return gfx_set_pixel(vm, p);
}

// Primitive 19
Ptr PRIM_FILLRCT_impl(VM *vm) {
   VM_ARG(Fixnum,color);
   VM_ARG(Point,b);
   VM_ARG(Point,a);

 return gfx_fill_rect(vm, a, b, color);
}

// Primitive 20
Ptr PRIM_PTPLUS_impl(VM *vm) {
   VM_ARG(Point,b);
   VM_ARG(Point,a);

  return to(Point,(a + b));
}

// Primitive 21
Ptr PRIM_PTMINUS_impl(VM *vm) {
   VM_ARG(Point,b);
   VM_ARG(Point,a);

  return to(Point,(a - b));
}

// Primitive 22
Ptr PRIM_MKPOINT_impl(VM *vm) {
   VM_ARG(Fixnum,b);
   VM_ARG(Fixnum,a);

  return to(Point,((point){(s32)a, (s32)b}));
}

// Primitive 23
Ptr PRIM_PTX_impl(VM *vm) {
   VM_ARG(Point,p);

  return to(Fixnum,((s64)p.x));
}

// Primitive 24
Ptr PRIM_PTY_impl(VM *vm) {
   VM_ARG(Point,p);

  return to(Fixnum,((s64)p.y));
}

// Primitive 25
Ptr PRIM_DRAWIMAGE_impl(VM *vm) {
   VM_ARG(Fixnum,rot);
   VM_ARG(Fixnum,scale);
   VM_ARG(Point,p);
   VM_ARG(Image,img);

 return gfx_blit_image_at(vm, img, p, scale, rot);
}

// Primitive 26
Ptr PRIM_LOADIMAGE_impl(VM *vm) {
   VM_ARG(String,path);

 return load_image(vm, path);
}


PrimitiveFunction PrimLookupTable[] = {
  &PRIM_PLUS_impl,
  &PRIM_MINUS_impl,
  &PRIM_TIMES_impl,
  &PRIM_DIVIDE_impl,
  &PRIM_LT_impl,
  &PRIM_GT_impl,
  &PRIM_MOD_impl,
  &PRIM_CONS_impl,
  &PRIM_CAR_impl,
  &PRIM_CDR_impl,
  &PRIM_EQ_impl,
  &PRIM_ISNIL_impl,
  &PRIM_NOT_impl,
  &PRIM_PRINT_impl,
  &PRIM_NTH_impl,
  &PRIM_SET_SYM_VAL_impl,
  &PRIM_PRINT_STACK_impl,
  &PRIM_DBG_STACK_impl,
  &PRIM_SETPXL_impl,
  &PRIM_FILLRCT_impl,
  &PRIM_PTPLUS_impl,
  &PRIM_PTMINUS_impl,
  &PRIM_MKPOINT_impl,
  &PRIM_PTX_impl,
  &PRIM_PTY_impl,
  &PRIM_DRAWIMAGE_impl,
  &PRIM_LOADIMAGE_impl,

  (PrimitiveFunction)(void *)0
};

void initialize_primitive_functions(VM *vm) {

  set_global(vm, "+", to(PrimOp, PRIM_PLUS));
  set_global(vm, "-", to(PrimOp, PRIM_MINUS));
  set_global(vm, "*", to(PrimOp, PRIM_TIMES));
  set_global(vm, "/", to(PrimOp, PRIM_DIVIDE));
  set_global(vm, "<", to(PrimOp, PRIM_LT));
  set_global(vm, ">", to(PrimOp, PRIM_GT));
  set_global(vm, "%", to(PrimOp, PRIM_MOD));
  set_global(vm, "cons", to(PrimOp, PRIM_CONS));
  set_global(vm, "car", to(PrimOp, PRIM_CAR));
  set_global(vm, "cdr", to(PrimOp, PRIM_CDR));
  set_global(vm, "eq", to(PrimOp, PRIM_EQ));
  set_global(vm, "nil?", to(PrimOp, PRIM_ISNIL));
  set_global(vm, "not", to(PrimOp, PRIM_NOT));
  set_global(vm, "print", to(PrimOp, PRIM_PRINT));
  set_global(vm, "nth", to(PrimOp, PRIM_NTH));
  set_global(vm, "set-symbol-value", to(PrimOp, PRIM_SET_SYM_VAL));
  set_global(vm, "print-stacktrace", to(PrimOp, PRIM_PRINT_STACK));
  set_global(vm, "debug-stacktrace", to(PrimOp, PRIM_DBG_STACK));
  set_global(vm, "set-pixel", to(PrimOp, PRIM_SETPXL));
  set_global(vm, "fill-rect", to(PrimOp, PRIM_FILLRCT));
  set_global(vm, "point+", to(PrimOp, PRIM_PTPLUS));
  set_global(vm, "point-", to(PrimOp, PRIM_PTMINUS));
  set_global(vm, "make-point", to(PrimOp, PRIM_MKPOINT));
  set_global(vm, "point-x", to(PrimOp, PRIM_PTX));
  set_global(vm, "point-y", to(PrimOp, PRIM_PTY));
  set_global(vm, "blit-at", to(PrimOp, PRIM_DRAWIMAGE));
  set_global(vm, "load-image", to(PrimOp, PRIM_LOADIMAGE));

}
