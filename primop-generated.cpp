
enum PrimitiveOperation : u64 {
  PRIM_APPLY = PrimOp_Tag,
  PRIM_SEND = ((1ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_SLEEP_MS = ((2ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_SEM_WAIT = ((3ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_KILL_THD = ((4ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_UNUS0 = ((5ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_UNUS1 = ((6ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_UNUS2 = ((7ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_CMPC = ((8ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CLASSOF = ((9ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKCLASS = ((10ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SETMETHOD = ((11ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_CLSGETMETA = ((12ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CLSSETMETA = ((13ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_MKINST = ((14ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IVAR_GET = ((15ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_IVAR_SET = ((16ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_IS_CLSS = ((17ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_BOOL = ((18ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_CLS = ((19ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_PRIM = ((20ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_FLT = ((21ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_INT = ((22ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PAIR_Q = ((23ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_ISNIL = ((24ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_HT = ((25ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_ARY = ((26ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SYM_Q = ((27ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_ISCHAR = ((28ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_ISIMAGE = ((29ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_PT = ((30ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_CONT = ((31ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_STR_P = ((32ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_SEM = ((33ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_THD = ((34ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IS_PKG = ((35ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FIX_PLUS = ((36ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_MINUS = ((37ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_TIMES = ((38ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_DIVIDE = ((39ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_LT = ((40ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_GT = ((41ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_MOD = ((42ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_PLUS = ((43ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_MINUS = ((44ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_TIMES = ((45ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_DIVIDE = ((46ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_LT = ((47ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_GT = ((48ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_MOD = ((49ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_TO_FLT = ((50ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_TO_FIX = ((51ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_LIST = ((52ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_CONS = ((53ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CAR = ((54ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CDR = ((55ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_EQ = ((56ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_NOT = ((57ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PRINT = ((58ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_NTH = ((59ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MKARY = ((60ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_AGET = ((61ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_ASET = ((62ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_MK_HT = ((63ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_MK_ST = ((64ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_HT_AT = ((65ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_HT_AT_PUT = ((66ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_SET_SYM_VAL = ((67ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SET_SYM_SPECIAL = ((68ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PKG_EXT_SYM = ((69ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MK_USR_PKG = ((70ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_GSYM = ((71ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_INTERN = ((72ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PRINT_STACK = ((73ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_DBG_STACK = ((74ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_SETPXL = ((75ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PTPLUS = ((76ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PTMINUS = ((77ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MKPOINT = ((78ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PTX = ((79ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PTY = ((80ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PTROT = ((81ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SFILLRCT = ((82ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_DRAWIMAGE = ((83ULL << 32) | (4ULL << 16) | PrimOp_Tag),
  PRIM_FILLRCT = ((84ULL << 32) | (4ULL << 16) | PrimOp_Tag),
  PRIM_CLRRCT = ((85ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_BLT = ((86ULL << 32) | (7ULL << 16) | PrimOp_Tag),
  PRIM_BLT_M = ((87ULL << 32) | (12ULL << 16) | PrimOp_Tag),
  PRIM_BLT_FR_SCRN = ((88ULL << 32) | (6ULL << 16) | PrimOp_Tag),
  PRIM_LOADIMAGE = ((89ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKIMAGE = ((90ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_IMG_W = ((91ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IMG_H = ((92ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CCA = ((93ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CC = ((94ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CH_AT = ((95ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CH_ATP = ((96ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_CH_LT = ((97ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CH_GT = ((98ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CHNM = ((99ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKSTR = ((100ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_STRLEN = ((101ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SSTKMARK = ((102ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PSTKMARK = ((103ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_RSTKSNAP = ((104ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CONT_VAL = ((105ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FORK = ((106ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MK_SEM = ((107ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SEM_SIG = ((108ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CURR_THD = ((109ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_SLURP = ((110ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IM_SAV = ((111ULL << 32) | (1ULL << 16) | PrimOp_Tag),

  PRIM_UNUSED = 0
};

// Primitive 0
Ptr PRIM_SEND_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr unused = vm_get_stack_values_as_list(vm, argc);
 return unused;
}

// Primitive 1
Ptr PRIM_SLEEP_MS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr unused = vm_get_stack_values_as_list(vm, argc);
 return unused;
}

// Primitive 2
Ptr PRIM_SEM_WAIT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr unused = vm_get_stack_values_as_list(vm, argc);
 return unused;
}

// Primitive 3
Ptr PRIM_KILL_THD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr unused = vm_get_stack_values_as_list(vm, argc);
 return unused;
}

// Primitive 4
Ptr PRIM_UNUS0_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr unused = vm_get_stack_values_as_list(vm, argc);
 return unused;
}

// Primitive 5
Ptr PRIM_UNUS1_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr unused = vm_get_stack_values_as_list(vm, argc);
 return unused;
}

// Primitive 6
Ptr PRIM_UNUS2_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr unused = vm_get_stack_values_as_list(vm, argc);
 return unused;
}

// Primitive 7
Ptr PRIM_CMPC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("compile-to-closure",any,expr);

 return compile_to_closure(vm, expr);
}

// Primitive 8
Ptr PRIM_CLASSOF_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-of",any,a);

 return class_of(vm, a);
}

// Primitive 9
Ptr PRIM_MKCLASS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("create-class",Fixnum,ivars);
   VM_ARG("create-class",any,name);

 return make_user_class(vm, name, ivars);
}

// Primitive 10
Ptr PRIM_SETMETHOD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-set-method",any,fn);
   VM_ARG("class-set-method",any,sym);
   VM_ARG("class-set-method",Standard,a);

 return class_set_method(vm, a, sym, fn);
}

// Primitive 11
Ptr PRIM_CLSGETMETA_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-get-metadata",any,key);
   VM_ARG("class-get-metadata",Standard,a);

 return class_get_metadata(a, key);
}

// Primitive 12
Ptr PRIM_CLSSETMETA_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-set-metadata",any,val);
   VM_ARG("class-set-metadata",any,key);
   VM_ARG("class-set-metadata",Standard,a);

 return class_set_metadata(vm, a, key, val);
}

// Primitive 13
Ptr PRIM_MKINST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("instantiate-class",Standard,klass);

 return instantiate_user_class(vm, klass);
}

// Primitive 14
Ptr PRIM_IVAR_GET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("instance-get-ivar",Fixnum,idx);
   VM_ARG("instance-get-ivar",Standard,obj);

 return standard_object_get_ivar(obj, idx);
}

// Primitive 15
Ptr PRIM_IVAR_SET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("instance-set-ivar",any,val);
   VM_ARG("instance-set-ivar",Fixnum,idx);
   VM_ARG("instance-set-ivar",Standard,obj);

 return standard_object_set_ivar(obj, idx, val);
}

// Primitive 16
Ptr PRIM_IS_CLSS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class?",any,a);

  return to(Bool,(is_class(a)));
}

// Primitive 17
Ptr PRIM_IS_BOOL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("bool?",any,a);

  return to(Bool,(is(Bool, a)));
}

// Primitive 18
Ptr PRIM_IS_CLS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("closure?",any,a);

  return to(Bool,(is(Closure, a)));
}

// Primitive 19
Ptr PRIM_IS_PRIM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("primop?",any,a);

  return to(Bool,(is(PrimOp, a)));
}

// Primitive 20
Ptr PRIM_IS_FLT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("float?",any,a);

  return to(Bool,(is(Float, a)));
}

// Primitive 21
Ptr PRIM_IS_INT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("fixnum?",any,a);

  return to(Bool,(is(Fixnum, a)));
}

// Primitive 22
Ptr PRIM_PAIR_Q_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("pair?",any,a);

  return to(Bool,(is(cons, a)));
}

// Primitive 23
Ptr PRIM_ISNIL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("nil?",any,a);

  return to(Bool,(isNil(a)));
}

// Primitive 24
Ptr PRIM_IS_HT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ht?",any,a);

  return to(Bool,(is(ht, a)));
}

// Primitive 25
Ptr PRIM_IS_ARY_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("array?",any,a);

  return to(Bool,(is(Array, a)));
}

// Primitive 26
Ptr PRIM_SYM_Q_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("symbol?",any,a);

  return to(Bool,(is(Symbol, a)));
}

// Primitive 27
Ptr PRIM_ISCHAR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char?",any,a);

  return to(Bool,(is(Char, a)));
}

// Primitive 28
Ptr PRIM_ISIMAGE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("image?",any,a);

  return to(Bool,(is(Image, a)));
}

// Primitive 29
Ptr PRIM_IS_PT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point?",any,a);

  return to(Bool,(is(Point, a)));
}

// Primitive 30
Ptr PRIM_IS_CONT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("continuation?",any,a);

  return to(Bool,(is(cont, a)));
}

// Primitive 31
Ptr PRIM_STR_P_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("string?",any,a);

  return to(Bool,(is(String, a)));
}

// Primitive 32
Ptr PRIM_IS_SEM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("semaphore?",any,a);

  return to(Bool,(is(semaphore, a)));
}

// Primitive 33
Ptr PRIM_IS_THD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("thread?",any,a);

  return to(Bool,(is(thread, a)));
}

// Primitive 34
Ptr PRIM_IS_PKG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package?",any,a);

  return to(Bool,(is(package, a)));
}

// Primitive 35
Ptr PRIM_FIX_PLUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("+i",Fixnum,b);
   VM_ARG("+i",Fixnum,a);

  return to(Fixnum,(a + b));
}

// Primitive 36
Ptr PRIM_FIX_MINUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("-i",Fixnum,b);
   VM_ARG("-i",Fixnum,a);

  return to(Fixnum,(a - b));
}

// Primitive 37
Ptr PRIM_FIX_TIMES_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("*i",Fixnum,b);
   VM_ARG("*i",Fixnum,a);

  return to(Fixnum,(a * b));
}

// Primitive 38
Ptr PRIM_FIX_DIVIDE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("/i",Fixnum,b);
   VM_ARG("/i",Fixnum,a);

  return to(Fixnum,(a / b));
}

// Primitive 39
Ptr PRIM_FIX_LT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("<i",Fixnum,b);
   VM_ARG("<i",Fixnum,a);

  return to(Bool,(a < b));
}

// Primitive 40
Ptr PRIM_FIX_GT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG(">i",Fixnum,b);
   VM_ARG(">i",Fixnum,a);

  return to(Bool,(a > b));
}

// Primitive 41
Ptr PRIM_FIX_MOD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%i",Fixnum,b);
   VM_ARG("%i",Fixnum,a);

  return to(Fixnum,(a % b));
}

// Primitive 42
Ptr PRIM_FLT_PLUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("+f",Float,b);
   VM_ARG("+f",Float,a);

  return to(Float,(a + b));
}

// Primitive 43
Ptr PRIM_FLT_MINUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("-f",Float,b);
   VM_ARG("-f",Float,a);

  return to(Float,(a - b));
}

// Primitive 44
Ptr PRIM_FLT_TIMES_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("*f",Float,b);
   VM_ARG("*f",Float,a);

  return to(Float,(a * b));
}

// Primitive 45
Ptr PRIM_FLT_DIVIDE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("/f",Float,b);
   VM_ARG("/f",Float,a);

  return to(Float,(a / b));
}

// Primitive 46
Ptr PRIM_FLT_LT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("<f",Float,b);
   VM_ARG("<f",Float,a);

  return to(Bool,(a < b));
}

// Primitive 47
Ptr PRIM_FLT_GT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG(">f",Float,b);
   VM_ARG(">f",Float,a);

  return to(Bool,(a > b));
}

// Primitive 48
Ptr PRIM_FLT_MOD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%f",Float,b);
   VM_ARG("%f",Float,a);

  return to(Float,(fmodf(a, b)));
}

// Primitive 49
Ptr PRIM_FIX_TO_FLT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("i->f",Fixnum,a);

  return to(Float,((f32)a));
}

// Primitive 50
Ptr PRIM_FLT_TO_FIX_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("f->i",Float,a);

  return to(Fixnum,((s64)a));
}

// Primitive 51
Ptr PRIM_LIST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr list = vm_get_stack_values_as_list(vm, argc);
 return list;
}

// Primitive 52
Ptr PRIM_CONS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("cons",any,b);
   VM_ARG("cons",any,a);

 return cons(vm, a, b);
}

// Primitive 53
Ptr PRIM_CAR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("car",any,a);

 return car(a);
}

// Primitive 54
Ptr PRIM_CDR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("cdr",any,a);

 return cdr(a);
}

// Primitive 55
Ptr PRIM_EQ_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("eq",any,b);
   VM_ARG("eq",any,a);

  return to(Bool,(ptr_eq(a, b)));
}

// Primitive 56
Ptr PRIM_NOT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("not",any,a);

  return to(Bool,(a == False));
}

// Primitive 57
Ptr PRIM_PRINT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("print",any,a);

 return primitive_print(a);
}

// Primitive 58
Ptr PRIM_NTH_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("nth",Fixnum,idx);
   VM_ARG("nth",any,a);

 return nth_or_nil(a, idx);
}

// Primitive 59
Ptr PRIM_MKARY_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-array",Fixnum,len);

 return make_zf_array(vm, len);
}

// Primitive 60
Ptr PRIM_AGET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("aget",Fixnum,idx);
   VM_ARG("aget",PtrArray,a);

 return aget(a, idx);
}

// Primitive 61
Ptr PRIM_ASET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("aset",any,val);
   VM_ARG("aset",Fixnum,idx);
   VM_ARG("aset",PtrArray,a);

 return aset(a, idx, val);
}

// Primitive 62
Ptr PRIM_MK_HT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return ht(vm);
}

// Primitive 63
Ptr PRIM_MK_ST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return string_table(vm);
}

// Primitive 64
Ptr PRIM_HT_AT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ht-at",any,key);
   VM_ARG("ht-at",any,ht);

 return ht_at(ht, key);
}

// Primitive 65
Ptr PRIM_HT_AT_PUT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ht-at-put",any,val);
   VM_ARG("ht-at-put",any,key);
   VM_ARG("ht-at-put",any,ht);

 return ht_at_put(vm, ht, key, val);
}

// Primitive 66
Ptr PRIM_SET_SYM_VAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("set-symbol-value",any,b);
   VM_ARG("set-symbol-value",any,a);

 return set_symbol_value(vm, a, b);
}

// Primitive 67
Ptr PRIM_SET_SYM_SPECIAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("mark-symbol-as-special",any,a);

 return mark_symbol_as_special(vm, a);
}

// Primitive 68
Ptr PRIM_PKG_EXT_SYM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-extern-symbol",any,b);
   VM_ARG("package-extern-symbol",any,a);

 return package_extern_symbol(vm, a, b);
}

// Primitive 69
Ptr PRIM_MK_USR_PKG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-user-package",any,name);

 return make_user_package(vm, name);
}

// Primitive 70
Ptr PRIM_GSYM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return make_symbol(vm, "_gensym_");
}

// Primitive 71
Ptr PRIM_INTERN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("intern",any,pkg);
   VM_ARG("intern",String,a);

 return intern(vm, a, pkg);
}

// Primitive 72
Ptr PRIM_PRINT_STACK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return vm_print_stack_trace(vm);
}

// Primitive 73
Ptr PRIM_DBG_STACK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return vm_print_debug_stack_trace(vm);
}

// Primitive 74
Ptr PRIM_SETPXL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("set-pixel",Point,p);

 return gfx_set_pixel(vm, p);
}

// Primitive 75
Ptr PRIM_PTPLUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point+",Point,b);
   VM_ARG("point+",Point,a);

  return to(Point,(a + b));
}

// Primitive 76
Ptr PRIM_PTMINUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-",Point,b);
   VM_ARG("point-",Point,a);

  return to(Point,(a - b));
}

// Primitive 77
Ptr PRIM_MKPOINT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-point",Fixnum,b);
   VM_ARG("make-point",Fixnum,a);

  return to(Point,((point){(s32)a, (s32)b}));
}

// Primitive 78
Ptr PRIM_PTX_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-x",Point,p);

  return to(Fixnum,((s64)p.x));
}

// Primitive 79
Ptr PRIM_PTY_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-y",Point,p);

  return to(Fixnum,((s64)p.y));
}

// Primitive 80
Ptr PRIM_PTROT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-rotate",Float,degrees);
   VM_ARG("point-rotate",Point,p);

  return to(Point,(rotate_point(p, degrees)));
}

// Primitive 81
Ptr PRIM_SFILLRCT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("screen-fill-rect",Fixnum,color);
   VM_ARG("screen-fill-rect",Point,b);
   VM_ARG("screen-fill-rect",Point,a);

 return gfx_screen_fill_rect(vm, a, b, color);
}

// Primitive 82
Ptr PRIM_DRAWIMAGE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("blit-to-screen",Fixnum,rot);
   VM_ARG("blit-to-screen",Fixnum,scale);
   VM_ARG("blit-to-screen",Point,p);
   VM_ARG("blit-to-screen",Image,img);

 return gfx_blit_image_at(vm, img, p, scale, rot);
}

// Primitive 83
Ptr PRIM_FILLRCT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("fill-rect",Fixnum,color);
   VM_ARG("fill-rect",Point,b);
   VM_ARG("fill-rect",Point,a);
   VM_ARG("fill-rect",Image,dst);

 return gfx_fill_rect(dst, a, b, color);
}

// Primitive 84
Ptr PRIM_CLRRCT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("clear-rect",Point,b);
   VM_ARG("clear-rect",Point,a);
   VM_ARG("clear-rect",Image,dst);

 return gfx_clear_rect(dst, a, b);
}

// Primitive 85
Ptr PRIM_BLT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("blit",Float,degrees_rotation);
   VM_ARG("blit",Float,scale);
   VM_ARG("blit",Point,lr);
   VM_ARG("blit",Point,ul);
   VM_ARG("blit",Point,at);
   VM_ARG("blit",Image,dst);
   VM_ARG("blit",Image,src);

 return gfx_blit(src, dst, at, ul, lr, scale, degrees_rotation);
}

// Primitive 86
Ptr PRIM_BLT_M_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("blit-with-mask",Float,msk_rot);
   VM_ARG("blit-with-mask",Float,msk_scale);
   VM_ARG("blit-with-mask",Point,msk_lr);
   VM_ARG("blit-with-mask",Point,msk_ul);
   VM_ARG("blit-with-mask",Float,src_rot);
   VM_ARG("blit-with-mask",Float,src_scale);
   VM_ARG("blit-with-mask",Point,src_lr);
   VM_ARG("blit-with-mask",Point,src_ul);
   VM_ARG("blit-with-mask",Point,at);
   VM_ARG("blit-with-mask",Image,msk);
   VM_ARG("blit-with-mask",Image,dst);
   VM_ARG("blit-with-mask",Image,src);

 return gfx_blit_image_with_mask(src, dst, msk, at,
  points_to_rect(src_ul, src_lr), src_scale, src_rot,
  points_to_rect(msk_ul, msk_lr), msk_scale, msk_rot
);
}

// Primitive 87
Ptr PRIM_BLT_FR_SCRN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("blit-from-screen",Float,degrees_rotation);
   VM_ARG("blit-from-screen",Float,scale);
   VM_ARG("blit-from-screen",Point,lr);
   VM_ARG("blit-from-screen",Point,ul);
   VM_ARG("blit-from-screen",Point,at);
   VM_ARG("blit-from-screen",Image,dst);

 return gfx_blit_from_screen(vm, dst, at, ul, lr, scale, degrees_rotation);
}

// Primitive 88
Ptr PRIM_LOADIMAGE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("load-image",String,path);

 return gfx_load_image(vm, path);
}

// Primitive 89
Ptr PRIM_MKIMAGE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-image",Fixnum,h);
   VM_ARG("make-image",Fixnum,w);

 return gfx_make_image(vm, w, h);
}

// Primitive 90
Ptr PRIM_IMG_W_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("image-width",Image,img);

  return to(Fixnum,(image_width(img)));
}

// Primitive 91
Ptr PRIM_IMG_H_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("image-height",Image,img);

  return to(Fixnum,(image_height(img)));
}

// Primitive 92
Ptr PRIM_CCA_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-code-at",Fixnum,idx);
   VM_ARG("char-code-at",String,str);

  return to(Fixnum,(string_char_code_at(vm, str, idx)));
}

// Primitive 93
Ptr PRIM_CC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-code",Char,ch);

  return to(Fixnum,((s64)ch));
}

// Primitive 94
Ptr PRIM_CH_AT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-at",Fixnum,idx);
   VM_ARG("char-at",String,str);

  return to(Char,(string_char_at(vm, str, idx)));
}

// Primitive 95
Ptr PRIM_CH_ATP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-at-put",Char,ch);
   VM_ARG("char-at-put",Fixnum,idx);
   VM_ARG("char-at-put",String,str);

 return string_set_char_at(vm, str, idx, ch);
}

// Primitive 96
Ptr PRIM_CH_LT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-<",Char,b);
   VM_ARG("char-<",Char,a);

  return to(Bool,(a < b));
}

// Primitive 97
Ptr PRIM_CH_GT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char->",Char,b);
   VM_ARG("char->",Char,a);

  return to(Bool,(a > b));
}

// Primitive 98
Ptr PRIM_CHNM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-by-name",String,name);

 return character_by_name(name);
}

// Primitive 99
Ptr PRIM_MKSTR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-string",Char,ch);
   VM_ARG("make-string",Fixnum,len);

 return make_filled_string(vm, len, ch);
}

// Primitive 100
Ptr PRIM_STRLEN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("string-length",String,str);

  return to(Fixnum,(string_length(str)));
}

// Primitive 101
Ptr PRIM_SSTKMARK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("set-stack-mark",any,m);

 return vm_set_stack_mark(vm, m);
}

// Primitive 102
Ptr PRIM_PSTKMARK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("snapshot-to-stack-mark",any,v);
   VM_ARG("snapshot-to-stack-mark",any,m);

 return vm_abort_to_mark(vm, m, v);
}

// Primitive 103
Ptr PRIM_RSTKSNAP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("resume-stack-snapshot",any,arg);
   VM_ARG("resume-stack-snapshot",any,s);

 return vm_resume_stack_snapshot(vm, s, arg);
}

// Primitive 104
Ptr PRIM_CONT_VAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("continuation-value",any,a);

 return cont_get_value(a);
}

// Primitive 105
Ptr PRIM_FORK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("fork-continuation",any,a);
   VM_ARG("fork-continuation",any,priority);

 return vm_schedule_cont(vm, a, priority, Nil);
}

// Primitive 106
Ptr PRIM_MK_SEM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-semaphore",Fixnum,a);

 return make_semaphore(vm, to(Fixnum, a));
}

// Primitive 107
Ptr PRIM_SEM_SIG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("signal-semaphore",any,a);

 return signal_semaphore(a);
}

// Primitive 108
Ptr PRIM_CURR_THD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return vm->globals->current_thread;
}

// Primitive 109
Ptr PRIM_SLURP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("slurp",String,path);

 return slurp(vm, path);
}

// Primitive 110
Ptr PRIM_IM_SAV_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("save-snapshot",String,path);

 return im_snapshot_to_path(vm, path);
}


PrimitiveFunction PrimLookupTable[] = {
  (PrimitiveFunction)(void *)0, // apply
  &PRIM_SEND_impl,
  &PRIM_SLEEP_MS_impl,
  &PRIM_SEM_WAIT_impl,
  &PRIM_KILL_THD_impl,
  &PRIM_UNUS0_impl,
  &PRIM_UNUS1_impl,
  &PRIM_UNUS2_impl,
  &PRIM_CMPC_impl,
  &PRIM_CLASSOF_impl,
  &PRIM_MKCLASS_impl,
  &PRIM_SETMETHOD_impl,
  &PRIM_CLSGETMETA_impl,
  &PRIM_CLSSETMETA_impl,
  &PRIM_MKINST_impl,
  &PRIM_IVAR_GET_impl,
  &PRIM_IVAR_SET_impl,
  &PRIM_IS_CLSS_impl,
  &PRIM_IS_BOOL_impl,
  &PRIM_IS_CLS_impl,
  &PRIM_IS_PRIM_impl,
  &PRIM_IS_FLT_impl,
  &PRIM_IS_INT_impl,
  &PRIM_PAIR_Q_impl,
  &PRIM_ISNIL_impl,
  &PRIM_IS_HT_impl,
  &PRIM_IS_ARY_impl,
  &PRIM_SYM_Q_impl,
  &PRIM_ISCHAR_impl,
  &PRIM_ISIMAGE_impl,
  &PRIM_IS_PT_impl,
  &PRIM_IS_CONT_impl,
  &PRIM_STR_P_impl,
  &PRIM_IS_SEM_impl,
  &PRIM_IS_THD_impl,
  &PRIM_IS_PKG_impl,
  &PRIM_FIX_PLUS_impl,
  &PRIM_FIX_MINUS_impl,
  &PRIM_FIX_TIMES_impl,
  &PRIM_FIX_DIVIDE_impl,
  &PRIM_FIX_LT_impl,
  &PRIM_FIX_GT_impl,
  &PRIM_FIX_MOD_impl,
  &PRIM_FLT_PLUS_impl,
  &PRIM_FLT_MINUS_impl,
  &PRIM_FLT_TIMES_impl,
  &PRIM_FLT_DIVIDE_impl,
  &PRIM_FLT_LT_impl,
  &PRIM_FLT_GT_impl,
  &PRIM_FLT_MOD_impl,
  &PRIM_FIX_TO_FLT_impl,
  &PRIM_FLT_TO_FIX_impl,
  &PRIM_LIST_impl,
  &PRIM_CONS_impl,
  &PRIM_CAR_impl,
  &PRIM_CDR_impl,
  &PRIM_EQ_impl,
  &PRIM_NOT_impl,
  &PRIM_PRINT_impl,
  &PRIM_NTH_impl,
  &PRIM_MKARY_impl,
  &PRIM_AGET_impl,
  &PRIM_ASET_impl,
  &PRIM_MK_HT_impl,
  &PRIM_MK_ST_impl,
  &PRIM_HT_AT_impl,
  &PRIM_HT_AT_PUT_impl,
  &PRIM_SET_SYM_VAL_impl,
  &PRIM_SET_SYM_SPECIAL_impl,
  &PRIM_PKG_EXT_SYM_impl,
  &PRIM_MK_USR_PKG_impl,
  &PRIM_GSYM_impl,
  &PRIM_INTERN_impl,
  &PRIM_PRINT_STACK_impl,
  &PRIM_DBG_STACK_impl,
  &PRIM_SETPXL_impl,
  &PRIM_PTPLUS_impl,
  &PRIM_PTMINUS_impl,
  &PRIM_MKPOINT_impl,
  &PRIM_PTX_impl,
  &PRIM_PTY_impl,
  &PRIM_PTROT_impl,
  &PRIM_SFILLRCT_impl,
  &PRIM_DRAWIMAGE_impl,
  &PRIM_FILLRCT_impl,
  &PRIM_CLRRCT_impl,
  &PRIM_BLT_impl,
  &PRIM_BLT_M_impl,
  &PRIM_BLT_FR_SCRN_impl,
  &PRIM_LOADIMAGE_impl,
  &PRIM_MKIMAGE_impl,
  &PRIM_IMG_W_impl,
  &PRIM_IMG_H_impl,
  &PRIM_CCA_impl,
  &PRIM_CC_impl,
  &PRIM_CH_AT_impl,
  &PRIM_CH_ATP_impl,
  &PRIM_CH_LT_impl,
  &PRIM_CH_GT_impl,
  &PRIM_CHNM_impl,
  &PRIM_MKSTR_impl,
  &PRIM_STRLEN_impl,
  &PRIM_SSTKMARK_impl,
  &PRIM_PSTKMARK_impl,
  &PRIM_RSTKSNAP_impl,
  &PRIM_CONT_VAL_impl,
  &PRIM_FORK_impl,
  &PRIM_MK_SEM_impl,
  &PRIM_SEM_SIG_impl,
  &PRIM_CURR_THD_impl,
  &PRIM_SLURP_impl,
  &PRIM_IM_SAV_impl,

  (PrimitiveFunction)(void *)0
};

void initialize_primitive_functions(VM *vm) {
  set_global(vm, "apply", to(PrimOp,  PRIM_APPLY));

  set_global(vm, "@send", to(PrimOp, PRIM_SEND));
  set_global(vm, "sleep-ms", to(PrimOp, PRIM_SLEEP_MS));
  set_global(vm, "semaphore-wait", to(PrimOp, PRIM_SEM_WAIT));
  set_global(vm, "kill-thread", to(PrimOp, PRIM_KILL_THD));
  set_global(vm, "-unused0", to(PrimOp, PRIM_UNUS0));
  set_global(vm, "-unused1", to(PrimOp, PRIM_UNUS1));
  set_global(vm, "-unused2", to(PrimOp, PRIM_UNUS2));
  set_global(vm, "compile-to-closure", to(PrimOp, PRIM_CMPC));
  set_global(vm, "class-of", to(PrimOp, PRIM_CLASSOF));
  set_global(vm, "create-class", to(PrimOp, PRIM_MKCLASS));
  set_global(vm, "class-set-method", to(PrimOp, PRIM_SETMETHOD));
  set_global(vm, "class-get-metadata", to(PrimOp, PRIM_CLSGETMETA));
  set_global(vm, "class-set-metadata", to(PrimOp, PRIM_CLSSETMETA));
  set_global(vm, "instantiate-class", to(PrimOp, PRIM_MKINST));
  set_global(vm, "instance-get-ivar", to(PrimOp, PRIM_IVAR_GET));
  set_global(vm, "instance-set-ivar", to(PrimOp, PRIM_IVAR_SET));
  set_global(vm, "class?", to(PrimOp, PRIM_IS_CLSS));
  set_global(vm, "bool?", to(PrimOp, PRIM_IS_BOOL));
  set_global(vm, "closure?", to(PrimOp, PRIM_IS_CLS));
  set_global(vm, "primop?", to(PrimOp, PRIM_IS_PRIM));
  set_global(vm, "float?", to(PrimOp, PRIM_IS_FLT));
  set_global(vm, "fixnum?", to(PrimOp, PRIM_IS_INT));
  set_global(vm, "pair?", to(PrimOp, PRIM_PAIR_Q));
  set_global(vm, "nil?", to(PrimOp, PRIM_ISNIL));
  set_global(vm, "ht?", to(PrimOp, PRIM_IS_HT));
  set_global(vm, "array?", to(PrimOp, PRIM_IS_ARY));
  set_global(vm, "symbol?", to(PrimOp, PRIM_SYM_Q));
  set_global(vm, "char?", to(PrimOp, PRIM_ISCHAR));
  set_global(vm, "image?", to(PrimOp, PRIM_ISIMAGE));
  set_global(vm, "point?", to(PrimOp, PRIM_IS_PT));
  set_global(vm, "continuation?", to(PrimOp, PRIM_IS_CONT));
  set_global(vm, "string?", to(PrimOp, PRIM_STR_P));
  set_global(vm, "semaphore?", to(PrimOp, PRIM_IS_SEM));
  set_global(vm, "thread?", to(PrimOp, PRIM_IS_THD));
  set_global(vm, "package?", to(PrimOp, PRIM_IS_PKG));
  set_global(vm, "+i", to(PrimOp, PRIM_FIX_PLUS));
  set_global(vm, "-i", to(PrimOp, PRIM_FIX_MINUS));
  set_global(vm, "*i", to(PrimOp, PRIM_FIX_TIMES));
  set_global(vm, "/i", to(PrimOp, PRIM_FIX_DIVIDE));
  set_global(vm, "<i", to(PrimOp, PRIM_FIX_LT));
  set_global(vm, ">i", to(PrimOp, PRIM_FIX_GT));
  set_global(vm, "%i", to(PrimOp, PRIM_FIX_MOD));
  set_global(vm, "+f", to(PrimOp, PRIM_FLT_PLUS));
  set_global(vm, "-f", to(PrimOp, PRIM_FLT_MINUS));
  set_global(vm, "*f", to(PrimOp, PRIM_FLT_TIMES));
  set_global(vm, "/f", to(PrimOp, PRIM_FLT_DIVIDE));
  set_global(vm, "<f", to(PrimOp, PRIM_FLT_LT));
  set_global(vm, ">f", to(PrimOp, PRIM_FLT_GT));
  set_global(vm, "%f", to(PrimOp, PRIM_FLT_MOD));
  set_global(vm, "i->f", to(PrimOp, PRIM_FIX_TO_FLT));
  set_global(vm, "f->i", to(PrimOp, PRIM_FLT_TO_FIX));
  set_global(vm, "list", to(PrimOp, PRIM_LIST));
  set_global(vm, "cons", to(PrimOp, PRIM_CONS));
  set_global(vm, "car", to(PrimOp, PRIM_CAR));
  set_global(vm, "cdr", to(PrimOp, PRIM_CDR));
  set_global(vm, "eq", to(PrimOp, PRIM_EQ));
  set_global(vm, "not", to(PrimOp, PRIM_NOT));
  set_global(vm, "print", to(PrimOp, PRIM_PRINT));
  set_global(vm, "nth", to(PrimOp, PRIM_NTH));
  set_global(vm, "make-array", to(PrimOp, PRIM_MKARY));
  set_global(vm, "aget", to(PrimOp, PRIM_AGET));
  set_global(vm, "aset", to(PrimOp, PRIM_ASET));
  set_global(vm, "make-ht", to(PrimOp, PRIM_MK_HT));
  set_global(vm, "make-st", to(PrimOp, PRIM_MK_ST));
  set_global(vm, "ht-at", to(PrimOp, PRIM_HT_AT));
  set_global(vm, "ht-at-put", to(PrimOp, PRIM_HT_AT_PUT));
  set_global(vm, "set-symbol-value", to(PrimOp, PRIM_SET_SYM_VAL));
  set_global(vm, "mark-symbol-as-special", to(PrimOp, PRIM_SET_SYM_SPECIAL));
  set_global(vm, "package-extern-symbol", to(PrimOp, PRIM_PKG_EXT_SYM));
  set_global(vm, "make-user-package", to(PrimOp, PRIM_MK_USR_PKG));
  set_global(vm, "gensym", to(PrimOp, PRIM_GSYM));
  set_global(vm, "intern", to(PrimOp, PRIM_INTERN));
  set_global(vm, "print-stacktrace", to(PrimOp, PRIM_PRINT_STACK));
  set_global(vm, "debug-stacktrace", to(PrimOp, PRIM_DBG_STACK));
  set_global(vm, "set-pixel", to(PrimOp, PRIM_SETPXL));
  set_global(vm, "point+", to(PrimOp, PRIM_PTPLUS));
  set_global(vm, "point-", to(PrimOp, PRIM_PTMINUS));
  set_global(vm, "make-point", to(PrimOp, PRIM_MKPOINT));
  set_global(vm, "point-x", to(PrimOp, PRIM_PTX));
  set_global(vm, "point-y", to(PrimOp, PRIM_PTY));
  set_global(vm, "point-rotate", to(PrimOp, PRIM_PTROT));
  set_global(vm, "screen-fill-rect", to(PrimOp, PRIM_SFILLRCT));
  set_global(vm, "blit-to-screen", to(PrimOp, PRIM_DRAWIMAGE));
  set_global(vm, "fill-rect", to(PrimOp, PRIM_FILLRCT));
  set_global(vm, "clear-rect", to(PrimOp, PRIM_CLRRCT));
  set_global(vm, "blit", to(PrimOp, PRIM_BLT));
  set_global(vm, "blit-with-mask", to(PrimOp, PRIM_BLT_M));
  set_global(vm, "blit-from-screen", to(PrimOp, PRIM_BLT_FR_SCRN));
  set_global(vm, "load-image", to(PrimOp, PRIM_LOADIMAGE));
  set_global(vm, "make-image", to(PrimOp, PRIM_MKIMAGE));
  set_global(vm, "image-width", to(PrimOp, PRIM_IMG_W));
  set_global(vm, "image-height", to(PrimOp, PRIM_IMG_H));
  set_global(vm, "char-code-at", to(PrimOp, PRIM_CCA));
  set_global(vm, "char-code", to(PrimOp, PRIM_CC));
  set_global(vm, "char-at", to(PrimOp, PRIM_CH_AT));
  set_global(vm, "char-at-put", to(PrimOp, PRIM_CH_ATP));
  set_global(vm, "char-<", to(PrimOp, PRIM_CH_LT));
  set_global(vm, "char->", to(PrimOp, PRIM_CH_GT));
  set_global(vm, "char-by-name", to(PrimOp, PRIM_CHNM));
  set_global(vm, "make-string", to(PrimOp, PRIM_MKSTR));
  set_global(vm, "string-length", to(PrimOp, PRIM_STRLEN));
  set_global(vm, "set-stack-mark", to(PrimOp, PRIM_SSTKMARK));
  set_global(vm, "snapshot-to-stack-mark", to(PrimOp, PRIM_PSTKMARK));
  set_global(vm, "resume-stack-snapshot", to(PrimOp, PRIM_RSTKSNAP));
  set_global(vm, "continuation-value", to(PrimOp, PRIM_CONT_VAL));
  set_global(vm, "fork-continuation", to(PrimOp, PRIM_FORK));
  set_global(vm, "make-semaphore", to(PrimOp, PRIM_MK_SEM));
  set_global(vm, "signal-semaphore", to(PrimOp, PRIM_SEM_SIG));
  set_global(vm, "current-thread", to(PrimOp, PRIM_CURR_THD));
  set_global(vm, "slurp", to(PrimOp, PRIM_SLURP));
  set_global(vm, "save-snapshot", to(PrimOp, PRIM_IM_SAV));

}

inline Ptr giant_switch(VM *vm, u32 argc, u32 idx) {
  switch(idx) {
   
  case 1: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, unused);
    break;
  }

  case 2: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, unused);
    break;
  }

  case 3: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, unused);
    break;
  }

  case 4: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, unused);
    break;
  }

  case 5: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, unused);
    break;
  }

  case 6: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, unused);
    break;
  }

  case 7: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, unused);
    break;
  }

  case 8: {
   VM_ARG("compile-to-closure",any,expr);

    vm_push(vm, compile_to_closure(vm, expr));
    break;
  }

  case 9: {
   VM_ARG("class-of",any,a);

    vm_push(vm, class_of(vm, a));
    break;
  }

  case 10: {
   VM_ARG("create-class",Fixnum,ivars);
   VM_ARG("create-class",any,name);

    vm_push(vm, make_user_class(vm, name, ivars));
    break;
  }

  case 11: {
   VM_ARG("class-set-method",any,fn);
   VM_ARG("class-set-method",any,sym);
   VM_ARG("class-set-method",Standard,a);

    vm_push(vm, class_set_method(vm, a, sym, fn));
    break;
  }

  case 12: {
   VM_ARG("class-get-metadata",any,key);
   VM_ARG("class-get-metadata",Standard,a);

    vm_push(vm, class_get_metadata(a, key));
    break;
  }

  case 13: {
   VM_ARG("class-set-metadata",any,val);
   VM_ARG("class-set-metadata",any,key);
   VM_ARG("class-set-metadata",Standard,a);

    vm_push(vm, class_set_metadata(vm, a, key, val));
    break;
  }

  case 14: {
   VM_ARG("instantiate-class",Standard,klass);

    vm_push(vm, instantiate_user_class(vm, klass));
    break;
  }

  case 15: {
   VM_ARG("instance-get-ivar",Fixnum,idx);
   VM_ARG("instance-get-ivar",Standard,obj);

    vm_push(vm, standard_object_get_ivar(obj, idx));
    break;
  }

  case 16: {
   VM_ARG("instance-set-ivar",any,val);
   VM_ARG("instance-set-ivar",Fixnum,idx);
   VM_ARG("instance-set-ivar",Standard,obj);

    vm_push(vm, standard_object_set_ivar(obj, idx, val));
    break;
  }

  case 17: {
   VM_ARG("class?",any,a);

     vm_push(vm, to(Bool,(is_class(a))));
    break;
  }

  case 18: {
   VM_ARG("bool?",any,a);

     vm_push(vm, to(Bool,(is(Bool, a))));
    break;
  }

  case 19: {
   VM_ARG("closure?",any,a);

     vm_push(vm, to(Bool,(is(Closure, a))));
    break;
  }

  case 20: {
   VM_ARG("primop?",any,a);

     vm_push(vm, to(Bool,(is(PrimOp, a))));
    break;
  }

  case 21: {
   VM_ARG("float?",any,a);

     vm_push(vm, to(Bool,(is(Float, a))));
    break;
  }

  case 22: {
   VM_ARG("fixnum?",any,a);

     vm_push(vm, to(Bool,(is(Fixnum, a))));
    break;
  }

  case 23: {
   VM_ARG("pair?",any,a);

     vm_push(vm, to(Bool,(is(cons, a))));
    break;
  }

  case 24: {
   VM_ARG("nil?",any,a);

     vm_push(vm, to(Bool,(isNil(a))));
    break;
  }

  case 25: {
   VM_ARG("ht?",any,a);

     vm_push(vm, to(Bool,(is(ht, a))));
    break;
  }

  case 26: {
   VM_ARG("array?",any,a);

     vm_push(vm, to(Bool,(is(Array, a))));
    break;
  }

  case 27: {
   VM_ARG("symbol?",any,a);

     vm_push(vm, to(Bool,(is(Symbol, a))));
    break;
  }

  case 28: {
   VM_ARG("char?",any,a);

     vm_push(vm, to(Bool,(is(Char, a))));
    break;
  }

  case 29: {
   VM_ARG("image?",any,a);

     vm_push(vm, to(Bool,(is(Image, a))));
    break;
  }

  case 30: {
   VM_ARG("point?",any,a);

     vm_push(vm, to(Bool,(is(Point, a))));
    break;
  }

  case 31: {
   VM_ARG("continuation?",any,a);

     vm_push(vm, to(Bool,(is(cont, a))));
    break;
  }

  case 32: {
   VM_ARG("string?",any,a);

     vm_push(vm, to(Bool,(is(String, a))));
    break;
  }

  case 33: {
   VM_ARG("semaphore?",any,a);

     vm_push(vm, to(Bool,(is(semaphore, a))));
    break;
  }

  case 34: {
   VM_ARG("thread?",any,a);

     vm_push(vm, to(Bool,(is(thread, a))));
    break;
  }

  case 35: {
   VM_ARG("package?",any,a);

     vm_push(vm, to(Bool,(is(package, a))));
    break;
  }

  case 36: {
   VM_ARG("+i",Fixnum,b);
   VM_ARG("+i",Fixnum,a);

     vm_push(vm, to(Fixnum,(a + b)));
    break;
  }

  case 37: {
   VM_ARG("-i",Fixnum,b);
   VM_ARG("-i",Fixnum,a);

     vm_push(vm, to(Fixnum,(a - b)));
    break;
  }

  case 38: {
   VM_ARG("*i",Fixnum,b);
   VM_ARG("*i",Fixnum,a);

     vm_push(vm, to(Fixnum,(a * b)));
    break;
  }

  case 39: {
   VM_ARG("/i",Fixnum,b);
   VM_ARG("/i",Fixnum,a);

     vm_push(vm, to(Fixnum,(a / b)));
    break;
  }

  case 40: {
   VM_ARG("<i",Fixnum,b);
   VM_ARG("<i",Fixnum,a);

     vm_push(vm, to(Bool,(a < b)));
    break;
  }

  case 41: {
   VM_ARG(">i",Fixnum,b);
   VM_ARG(">i",Fixnum,a);

     vm_push(vm, to(Bool,(a > b)));
    break;
  }

  case 42: {
   VM_ARG("%i",Fixnum,b);
   VM_ARG("%i",Fixnum,a);

     vm_push(vm, to(Fixnum,(a % b)));
    break;
  }

  case 43: {
   VM_ARG("+f",Float,b);
   VM_ARG("+f",Float,a);

     vm_push(vm, to(Float,(a + b)));
    break;
  }

  case 44: {
   VM_ARG("-f",Float,b);
   VM_ARG("-f",Float,a);

     vm_push(vm, to(Float,(a - b)));
    break;
  }

  case 45: {
   VM_ARG("*f",Float,b);
   VM_ARG("*f",Float,a);

     vm_push(vm, to(Float,(a * b)));
    break;
  }

  case 46: {
   VM_ARG("/f",Float,b);
   VM_ARG("/f",Float,a);

     vm_push(vm, to(Float,(a / b)));
    break;
  }

  case 47: {
   VM_ARG("<f",Float,b);
   VM_ARG("<f",Float,a);

     vm_push(vm, to(Bool,(a < b)));
    break;
  }

  case 48: {
   VM_ARG(">f",Float,b);
   VM_ARG(">f",Float,a);

     vm_push(vm, to(Bool,(a > b)));
    break;
  }

  case 49: {
   VM_ARG("%f",Float,b);
   VM_ARG("%f",Float,a);

     vm_push(vm, to(Float,(fmodf(a, b))));
    break;
  }

  case 50: {
   VM_ARG("i->f",Fixnum,a);

     vm_push(vm, to(Float,((f32)a)));
    break;
  }

  case 51: {
   VM_ARG("f->i",Float,a);

     vm_push(vm, to(Fixnum,((s64)a)));
    break;
  }

  case 52: {
Ptr list = vm_get_stack_values_as_list(vm, argc);
    vm_push(vm, list);
    break;
  }

  case 53: {
   VM_ARG("cons",any,b);
   VM_ARG("cons",any,a);

    vm_push(vm, cons(vm, a, b));
    break;
  }

  case 54: {
   VM_ARG("car",any,a);

    vm_push(vm, car(a));
    break;
  }

  case 55: {
   VM_ARG("cdr",any,a);

    vm_push(vm, cdr(a));
    break;
  }

  case 56: {
   VM_ARG("eq",any,b);
   VM_ARG("eq",any,a);

     vm_push(vm, to(Bool,(ptr_eq(a, b))));
    break;
  }

  case 57: {
   VM_ARG("not",any,a);

     vm_push(vm, to(Bool,(a == False)));
    break;
  }

  case 58: {
   VM_ARG("print",any,a);

    vm_push(vm, primitive_print(a));
    break;
  }

  case 59: {
   VM_ARG("nth",Fixnum,idx);
   VM_ARG("nth",any,a);

    vm_push(vm, nth_or_nil(a, idx));
    break;
  }

  case 60: {
   VM_ARG("make-array",Fixnum,len);

    vm_push(vm, make_zf_array(vm, len));
    break;
  }

  case 61: {
   VM_ARG("aget",Fixnum,idx);
   VM_ARG("aget",PtrArray,a);

    vm_push(vm, aget(a, idx));
    break;
  }

  case 62: {
   VM_ARG("aset",any,val);
   VM_ARG("aset",Fixnum,idx);
   VM_ARG("aset",PtrArray,a);

    vm_push(vm, aset(a, idx, val));
    break;
  }

  case 63: {

    vm_push(vm, ht(vm));
    break;
  }

  case 64: {

    vm_push(vm, string_table(vm));
    break;
  }

  case 65: {
   VM_ARG("ht-at",any,key);
   VM_ARG("ht-at",any,ht);

    vm_push(vm, ht_at(ht, key));
    break;
  }

  case 66: {
   VM_ARG("ht-at-put",any,val);
   VM_ARG("ht-at-put",any,key);
   VM_ARG("ht-at-put",any,ht);

    vm_push(vm, ht_at_put(vm, ht, key, val));
    break;
  }

  case 67: {
   VM_ARG("set-symbol-value",any,b);
   VM_ARG("set-symbol-value",any,a);

    vm_push(vm, set_symbol_value(vm, a, b));
    break;
  }

  case 68: {
   VM_ARG("mark-symbol-as-special",any,a);

    vm_push(vm, mark_symbol_as_special(vm, a));
    break;
  }

  case 69: {
   VM_ARG("package-extern-symbol",any,b);
   VM_ARG("package-extern-symbol",any,a);

    vm_push(vm, package_extern_symbol(vm, a, b));
    break;
  }

  case 70: {
   VM_ARG("make-user-package",any,name);

    vm_push(vm, make_user_package(vm, name));
    break;
  }

  case 71: {

    vm_push(vm, make_symbol(vm, "_gensym_"));
    break;
  }

  case 72: {
   VM_ARG("intern",any,pkg);
   VM_ARG("intern",String,a);

    vm_push(vm, intern(vm, a, pkg));
    break;
  }

  case 73: {

    vm_push(vm, vm_print_stack_trace(vm));
    break;
  }

  case 74: {

    vm_push(vm, vm_print_debug_stack_trace(vm));
    break;
  }

  case 75: {
   VM_ARG("set-pixel",Point,p);

    vm_push(vm, gfx_set_pixel(vm, p));
    break;
  }

  case 76: {
   VM_ARG("point+",Point,b);
   VM_ARG("point+",Point,a);

     vm_push(vm, to(Point,(a + b)));
    break;
  }

  case 77: {
   VM_ARG("point-",Point,b);
   VM_ARG("point-",Point,a);

     vm_push(vm, to(Point,(a - b)));
    break;
  }

  case 78: {
   VM_ARG("make-point",Fixnum,b);
   VM_ARG("make-point",Fixnum,a);

     vm_push(vm, to(Point,((point){(s32)a, (s32)b})));
    break;
  }

  case 79: {
   VM_ARG("point-x",Point,p);

     vm_push(vm, to(Fixnum,((s64)p.x)));
    break;
  }

  case 80: {
   VM_ARG("point-y",Point,p);

     vm_push(vm, to(Fixnum,((s64)p.y)));
    break;
  }

  case 81: {
   VM_ARG("point-rotate",Float,degrees);
   VM_ARG("point-rotate",Point,p);

     vm_push(vm, to(Point,(rotate_point(p, degrees))));
    break;
  }

  case 82: {
   VM_ARG("screen-fill-rect",Fixnum,color);
   VM_ARG("screen-fill-rect",Point,b);
   VM_ARG("screen-fill-rect",Point,a);

    vm_push(vm, gfx_screen_fill_rect(vm, a, b, color));
    break;
  }

  case 83: {
   VM_ARG("blit-to-screen",Fixnum,rot);
   VM_ARG("blit-to-screen",Fixnum,scale);
   VM_ARG("blit-to-screen",Point,p);
   VM_ARG("blit-to-screen",Image,img);

    vm_push(vm, gfx_blit_image_at(vm, img, p, scale, rot));
    break;
  }

  case 84: {
   VM_ARG("fill-rect",Fixnum,color);
   VM_ARG("fill-rect",Point,b);
   VM_ARG("fill-rect",Point,a);
   VM_ARG("fill-rect",Image,dst);

    vm_push(vm, gfx_fill_rect(dst, a, b, color));
    break;
  }

  case 85: {
   VM_ARG("clear-rect",Point,b);
   VM_ARG("clear-rect",Point,a);
   VM_ARG("clear-rect",Image,dst);

    vm_push(vm, gfx_clear_rect(dst, a, b));
    break;
  }

  case 86: {
   VM_ARG("blit",Float,degrees_rotation);
   VM_ARG("blit",Float,scale);
   VM_ARG("blit",Point,lr);
   VM_ARG("blit",Point,ul);
   VM_ARG("blit",Point,at);
   VM_ARG("blit",Image,dst);
   VM_ARG("blit",Image,src);

    vm_push(vm, gfx_blit(src, dst, at, ul, lr, scale, degrees_rotation));
    break;
  }

  case 87: {
   VM_ARG("blit-with-mask",Float,msk_rot);
   VM_ARG("blit-with-mask",Float,msk_scale);
   VM_ARG("blit-with-mask",Point,msk_lr);
   VM_ARG("blit-with-mask",Point,msk_ul);
   VM_ARG("blit-with-mask",Float,src_rot);
   VM_ARG("blit-with-mask",Float,src_scale);
   VM_ARG("blit-with-mask",Point,src_lr);
   VM_ARG("blit-with-mask",Point,src_ul);
   VM_ARG("blit-with-mask",Point,at);
   VM_ARG("blit-with-mask",Image,msk);
   VM_ARG("blit-with-mask",Image,dst);
   VM_ARG("blit-with-mask",Image,src);

    vm_push(vm, gfx_blit_image_with_mask(src, dst, msk, at,
  points_to_rect(src_ul, src_lr), src_scale, src_rot,
  points_to_rect(msk_ul, msk_lr), msk_scale, msk_rot
));
    break;
  }

  case 88: {
   VM_ARG("blit-from-screen",Float,degrees_rotation);
   VM_ARG("blit-from-screen",Float,scale);
   VM_ARG("blit-from-screen",Point,lr);
   VM_ARG("blit-from-screen",Point,ul);
   VM_ARG("blit-from-screen",Point,at);
   VM_ARG("blit-from-screen",Image,dst);

    vm_push(vm, gfx_blit_from_screen(vm, dst, at, ul, lr, scale, degrees_rotation));
    break;
  }

  case 89: {
   VM_ARG("load-image",String,path);

    vm_push(vm, gfx_load_image(vm, path));
    break;
  }

  case 90: {
   VM_ARG("make-image",Fixnum,h);
   VM_ARG("make-image",Fixnum,w);

    vm_push(vm, gfx_make_image(vm, w, h));
    break;
  }

  case 91: {
   VM_ARG("image-width",Image,img);

     vm_push(vm, to(Fixnum,(image_width(img))));
    break;
  }

  case 92: {
   VM_ARG("image-height",Image,img);

     vm_push(vm, to(Fixnum,(image_height(img))));
    break;
  }

  case 93: {
   VM_ARG("char-code-at",Fixnum,idx);
   VM_ARG("char-code-at",String,str);

     vm_push(vm, to(Fixnum,(string_char_code_at(vm, str, idx))));
    break;
  }

  case 94: {
   VM_ARG("char-code",Char,ch);

     vm_push(vm, to(Fixnum,((s64)ch)));
    break;
  }

  case 95: {
   VM_ARG("char-at",Fixnum,idx);
   VM_ARG("char-at",String,str);

     vm_push(vm, to(Char,(string_char_at(vm, str, idx))));
    break;
  }

  case 96: {
   VM_ARG("char-at-put",Char,ch);
   VM_ARG("char-at-put",Fixnum,idx);
   VM_ARG("char-at-put",String,str);

    vm_push(vm, string_set_char_at(vm, str, idx, ch));
    break;
  }

  case 97: {
   VM_ARG("char-<",Char,b);
   VM_ARG("char-<",Char,a);

     vm_push(vm, to(Bool,(a < b)));
    break;
  }

  case 98: {
   VM_ARG("char->",Char,b);
   VM_ARG("char->",Char,a);

     vm_push(vm, to(Bool,(a > b)));
    break;
  }

  case 99: {
   VM_ARG("char-by-name",String,name);

    vm_push(vm, character_by_name(name));
    break;
  }

  case 100: {
   VM_ARG("make-string",Char,ch);
   VM_ARG("make-string",Fixnum,len);

    vm_push(vm, make_filled_string(vm, len, ch));
    break;
  }

  case 101: {
   VM_ARG("string-length",String,str);

     vm_push(vm, to(Fixnum,(string_length(str))));
    break;
  }

  case 102: {
   VM_ARG("set-stack-mark",any,m);

    vm_push(vm, vm_set_stack_mark(vm, m));
    break;
  }

  case 103: {
   VM_ARG("snapshot-to-stack-mark",any,v);
   VM_ARG("snapshot-to-stack-mark",any,m);

    vm_push(vm, vm_abort_to_mark(vm, m, v));
    break;
  }

  case 104: {
   VM_ARG("resume-stack-snapshot",any,arg);
   VM_ARG("resume-stack-snapshot",any,s);

    vm_push(vm, vm_resume_stack_snapshot(vm, s, arg));
    break;
  }

  case 105: {
   VM_ARG("continuation-value",any,a);

    vm_push(vm, cont_get_value(a));
    break;
  }

  case 106: {
   VM_ARG("fork-continuation",any,a);
   VM_ARG("fork-continuation",any,priority);

    vm_push(vm, vm_schedule_cont(vm, a, priority, Nil));
    break;
  }

  case 107: {
   VM_ARG("make-semaphore",Fixnum,a);

    vm_push(vm, make_semaphore(vm, to(Fixnum, a)));
    break;
  }

  case 108: {
   VM_ARG("signal-semaphore",any,a);

    vm_push(vm, signal_semaphore(a));
    break;
  }

  case 109: {

    vm_push(vm, vm->globals->current_thread);
    break;
  }

  case 110: {
   VM_ARG("slurp",String,path);

    vm_push(vm, slurp(vm, path));
    break;
  }

  case 111: {
   VM_ARG("save-snapshot",String,path);

    vm_push(vm, im_snapshot_to_path(vm, path));
    break;
  }

  }
  return Nil;
}
