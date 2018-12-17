
enum PrimitiveOperation : u64 {
  PRIM_PLUS = ((0ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_MINUS = ((1ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_TIMES = ((2ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_DIVIDE = ((3ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_LT = ((4ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_GT = ((5ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_MOD = ((6ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_CONS = ((7ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_CAR = ((8ULL << 32) | (1ULL << 16) | PRIM_TAG),
  PRIM_CDR = ((9ULL << 32) | (1ULL << 16) | PRIM_TAG),
  PRIM_EQ = ((10ULL << 32) | (2ULL << 16) | PRIM_TAG),
  PRIM_PRINT = ((11ULL << 32) | (1ULL << 16) | PRIM_TAG),

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

 return car(vm, a);
}

// Primitive 9
Ptr PRIM_CDR_impl(VM *vm) {
   VM_ARG(any,a);

 return cdr(vm, a);
}

// Primitive 10
Ptr PRIM_EQ_impl(VM *vm) {
   VM_ARG(any,b);
   VM_ARG(any,a);

  return to(Bool,(ptr_eq(a, b)));
}

// Primitive 11
Ptr PRIM_PRINT_impl(VM *vm) {
   VM_ARG(any,a);

 return primitive_print(a);
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
  &PRIM_PRINT_impl,

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
  set_global(vm, "print", to(PrimOp, PRIM_PRINT));

}
