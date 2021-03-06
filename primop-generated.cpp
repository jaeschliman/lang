
enum PrimitiveOperation : u64 {
  PRIM_APPLY = PrimOp_Tag,
  PRIM_SEND = ((1ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_SLEEP_MS = ((2ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_SEM_WAIT = ((3ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_KILL_THD = ((4ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_UNUS0 = ((5ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_UNUS1 = ((6ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_UNUS2 = ((7ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_ARGC = ((8ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_LDARG = ((9ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_OHIBITS = ((10ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_OLOBITS = ((11ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CMPC = ((12ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CLASSOF = ((13ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKCLASS = ((14ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SETMETHOD = ((15ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_GETMETHOD = ((16ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CLSGETMETA = ((17ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CLSSETMETA = ((18ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_CLSSETAPP = ((19ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MKINST = ((20ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IVAR_GET = ((21ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_IVAR_SET = ((22ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_IS_CLSS = ((23ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_BIG_NEG = ((24ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_BIG_TIMES = ((25ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_BIG_PLUS = ((26ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_BIG_MINUS = ((27ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_BIG_LT = ((28ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_BIG_GT = ((29ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_BIG_EQL = ((30ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_TO_BIG = ((31ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FIX_PLUS = ((32ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_MINUS = ((33ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_TIMES = ((34ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_DIVIDE = ((35ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_LT = ((36ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_GT = ((37ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_MOD = ((38ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_PLUS = ((39ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_MINUS = ((40ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_TIMES = ((41ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_DIVIDE = ((42ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_LT = ((43ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_GT = ((44ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_MOD = ((45ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FIX_TO_FLT = ((46ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_TO_FIX = ((47ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_LOG = ((48ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_FLOOR = ((49ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_CEIL = ((50ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_POW = ((51ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_REM = ((52ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_COS = ((53ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_SIN = ((54ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_TAN = ((55ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FLT_A2F = ((56ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_FLT_SRT = ((57ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_RAND = ((58ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_ASH = ((59ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_BAND = ((60ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_BOR = ((61ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_LIST = ((62ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_CONS = ((63ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CAR = ((64ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CDR = ((65ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_EQ = ((66ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_NOT = ((67ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PRINT = ((68ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_NTH = ((69ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_VEC = ((70ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_MKARY = ((71ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_AGET = ((72ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_ASET = ((73ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_ALEN = ((74ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKARYU16 = ((75ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_AGETU16 = ((76ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_ASETU16 = ((77ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_ALENU16 = ((78ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MK_HT = ((79ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_MK_ST = ((80ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_HT_AT = ((81ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_HT_AT_PUT = ((82ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_HT_HAS = ((83ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SYM_NAME = ((84ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SYM_PKG = ((85ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SYM_BND = ((86ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SET_SYM_VAL = ((87ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SYM_VAL = ((88ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SET_SYM_SPECIAL = ((89ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SYM_SPECIAL_P = ((90ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PKG_EXT_SYM = ((91ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PKG_IMP_SYM = ((92ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MK_USR_PKG = ((93ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MK_PKG = ((94ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PKG_NAME = ((95ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PKG_SUBP = ((96ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PKG_USELST = ((97ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PKG_SET_USELST = ((98ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PKG_EXTERNS = ((99ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PKG_META = ((100ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_GSYM = ((101ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_INTERN = ((102ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PRINT_STACK = ((103ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_DBG_STACK = ((104ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_SETPXL = ((105ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PTPLUS = ((106ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PTMINUS = ((107ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MKPOINT = ((108ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_PTX = ((109ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PTY = ((110ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PTROT = ((111ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_SFILLRCT = ((112ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_DRAWIMAGE = ((113ULL << 32) | (4ULL << 16) | PrimOp_Tag),
  PRIM_FILLRCT = ((114ULL << 32) | (4ULL << 16) | PrimOp_Tag),
  PRIM_CLRRCT = ((115ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_BLT = ((116ULL << 32) | (8ULL << 16) | PrimOp_Tag),
  PRIM_BLT_M = ((117ULL << 32) | (12ULL << 16) | PrimOp_Tag),
  PRIM_FIL_M = ((118ULL << 32) | (11ULL << 16) | PrimOp_Tag),
  PRIM_BLT_FR_SCRN = ((119ULL << 32) | (6ULL << 16) | PrimOp_Tag),
  PRIM_LOADIMAGE = ((120ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKIMAGE = ((121ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_IMG_W = ((122ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IMG_H = ((123ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CCA = ((124ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CCO = ((125ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_COC = ((126ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CH_AT = ((127ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CH_ATP = ((128ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_CH_LT = ((129ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CH_GT = ((130ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CH_W = ((131ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CHBYNM = ((132ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CHNM = ((133ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKSTR = ((134ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_STRBLEN = ((135ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_STRSBB = ((136ULL << 32) | (3ULL << 16) | PrimOp_Tag),
  PRIM_STR_EQUAL = ((137ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_STRCHCNT = ((138ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_STR_CHARY = ((139ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CHARY_STR = ((140ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SSTKMARK = ((141ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PSTKMARK = ((142ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_RSTKSNAP = ((143ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_RETMARK = ((144ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_CONT_VAL = ((145ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_FORK = ((146ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_MK_SEM = ((147ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_SEM_SIG = ((148ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_CURR_THD = ((149ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_THD_CNT = ((150ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_ALL_THDS = ((151ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_CAS_VEC = ((152ULL << 32) | (4ULL << 16) | PrimOp_Tag),
  PRIM_SLURP = ((153ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_OS_WSTR = ((154ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_OS_WCH = ((155ULL << 32) | (2ULL << 16) | PrimOp_Tag),
  PRIM_THD_DBG_INFO = ((156ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IM_SAV = ((157ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_IM_SAV_DIE = ((158ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_TIME_MS = ((159ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_CL_SRC_LOC = ((160ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_UPD_WIN = ((161ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_BLTQ = ((162ULL << 32) | (10ULL << 16) | PrimOp_Tag),
  PRIM_EXIT = ((163ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_PF = ((164ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_BCTOCLS = ((165ULL << 32) | (1ULL << 16) | PrimOp_Tag),
  PRIM_MKBTC = ((166ULL << 32) | (4ULL << 16) | PrimOp_Tag),
  PRIM_STKDPTH = ((167ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_STKDPTHB = ((168ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_CLRSTAT = ((169ULL << 32) | (255ULL << 16) | PrimOp_Tag),
  PRIM_PRNSTAT = ((170ULL << 32) | (255ULL << 16) | PrimOp_Tag),

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
Ptr PRIM_ARGC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

  return to(Fixnum,((s64)vm->curr_frame->argc));
}

// Primitive 8
Ptr PRIM_LDARG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%load-arg",Fixnum,it);

 return vm_load_arg(vm, it);
}

// Primitive 9
Ptr PRIM_OHIBITS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%obj-high-bits",Object,it);

  return to(Fixnum,(((u64) it >> 32)));
}

// Primitive 10
Ptr PRIM_OLOBITS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%obj-low-bits",Object,it);

  return to(Fixnum,(((u64) it & 0xFFFF)));
}

// Primitive 11
Ptr PRIM_CMPC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("compile-to-closure",any,expr);

 return compile_to_closure(vm, expr);
}

// Primitive 12
Ptr PRIM_CLASSOF_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-of",any,a);

 return class_of(vm, a);
}

// Primitive 13
Ptr PRIM_MKCLASS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("create-class",any,ivars);
   VM_ARG("create-class",any,name);

 return make_user_class(vm, name, ivars);
}

// Primitive 14
Ptr PRIM_SETMETHOD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-set-method",any,fn);
   VM_ARG("class-set-method",any,sym);
   VM_ARG("class-set-method",Standard,a);

 return class_set_method(vm, a, sym, fn);
}

// Primitive 15
Ptr PRIM_GETMETHOD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-get-method",any,sym);
   VM_ARG("class-get-method",Standard,a);

 return class_get_method(vm, a, sym);
}

// Primitive 16
Ptr PRIM_CLSGETMETA_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-get-metadata",any,key);
   VM_ARG("class-get-metadata",Standard,a);

 return class_get_metadata(a, key);
}

// Primitive 17
Ptr PRIM_CLSSETMETA_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-set-metadata",any,val);
   VM_ARG("class-set-metadata",any,key);
   VM_ARG("class-set-metadata",Standard,a);

 return class_set_metadata(vm, a, key, val);
}

// Primitive 18
Ptr PRIM_CLSSETAPP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class-set-applicator",any,fn);
   VM_ARG("class-set-applicator",Standard,a);

 return class_set_applicator(a, fn);
}

// Primitive 19
Ptr PRIM_MKINST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("instantiate-class",Standard,klass);

 return instantiate_user_class(vm, klass);
}

// Primitive 20
Ptr PRIM_IVAR_GET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("instance-get-ivar",Fixnum,idx);
   VM_ARG("instance-get-ivar",Standard,obj);

 return standard_object_get_ivar(obj, idx);
}

// Primitive 21
Ptr PRIM_IVAR_SET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("instance-set-ivar",any,val);
   VM_ARG("instance-set-ivar",Fixnum,idx);
   VM_ARG("instance-set-ivar",Standard,obj);

 return standard_object_set_ivar(obj, idx, val);
}

// Primitive 22
Ptr PRIM_IS_CLSS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("class?",any,a);

  return to(Bool,(is_class(a)));
}

// Primitive 23
Ptr PRIM_BIG_NEG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%negate-bignum",Bignum,a);

  return to(Bignum,(bignum_negate(vm, a)));
}

// Primitive 24
Ptr PRIM_BIG_TIMES_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("*b",Bignum,b);
   VM_ARG("*b",Bignum,a);

  return to(Bignum,(bignum_mul(vm, a, b)));
}

// Primitive 25
Ptr PRIM_BIG_PLUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("+b",Bignum,b);
   VM_ARG("+b",Bignum,a);

  return to(Bignum,(bignum_add(vm, a, b)));
}

// Primitive 26
Ptr PRIM_BIG_MINUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("-b",Bignum,b);
   VM_ARG("-b",Bignum,a);

  return to(Bignum,(bignum_sub(vm, a, b)));
}

// Primitive 27
Ptr PRIM_BIG_LT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("<b",Bignum,b);
   VM_ARG("<b",Bignum,a);

  return to(Bool,(bignum_lt(a, b)));
}

// Primitive 28
Ptr PRIM_BIG_GT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG(">b",Bignum,b);
   VM_ARG(">b",Bignum,a);

  return to(Bool,(bignum_gt(a,b)));
}

// Primitive 29
Ptr PRIM_BIG_EQL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("=b",Bignum,b);
   VM_ARG("=b",Bignum,a);

  return to(Bool,(bignum_eql(a, b)));
}

// Primitive 30
Ptr PRIM_FIX_TO_BIG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("i->b",Fixnum,a);

  return to(Bignum,(bignum_from_fixnum(vm, a)));
}

// Primitive 31
Ptr PRIM_FIX_PLUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("+i",Fixnum,b);
   VM_ARG("+i",Fixnum,a);

 return fixnum_add(vm, a, b);
}

// Primitive 32
Ptr PRIM_FIX_MINUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("-i",Fixnum,b);
   VM_ARG("-i",Fixnum,a);

 return fixnum_sub(vm, a, b);
}

// Primitive 33
Ptr PRIM_FIX_TIMES_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("*i",Fixnum,b);
   VM_ARG("*i",Fixnum,a);

 return fixnum_mul(vm, a, b);
}

// Primitive 34
Ptr PRIM_FIX_DIVIDE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("/i",Fixnum,b);
   VM_ARG("/i",Fixnum,a);

  return to(Fixnum,(a / b));
}

// Primitive 35
Ptr PRIM_FIX_LT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("<i",Fixnum,b);
   VM_ARG("<i",Fixnum,a);

  return to(Bool,(a < b));
}

// Primitive 36
Ptr PRIM_FIX_GT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG(">i",Fixnum,b);
   VM_ARG(">i",Fixnum,a);

  return to(Bool,(a > b));
}

// Primitive 37
Ptr PRIM_FIX_MOD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%i",Fixnum,b);
   VM_ARG("%i",Fixnum,a);

  return to(Fixnum,(a % b));
}

// Primitive 38
Ptr PRIM_FLT_PLUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("+f",Float,b);
   VM_ARG("+f",Float,a);

  return to(Float,(a + b));
}

// Primitive 39
Ptr PRIM_FLT_MINUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("-f",Float,b);
   VM_ARG("-f",Float,a);

  return to(Float,(a - b));
}

// Primitive 40
Ptr PRIM_FLT_TIMES_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("*f",Float,b);
   VM_ARG("*f",Float,a);

  return to(Float,(a * b));
}

// Primitive 41
Ptr PRIM_FLT_DIVIDE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("/f",Float,b);
   VM_ARG("/f",Float,a);

  return to(Float,(a / b));
}

// Primitive 42
Ptr PRIM_FLT_LT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("<f",Float,b);
   VM_ARG("<f",Float,a);

  return to(Bool,(a < b));
}

// Primitive 43
Ptr PRIM_FLT_GT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG(">f",Float,b);
   VM_ARG(">f",Float,a);

  return to(Bool,(a > b));
}

// Primitive 44
Ptr PRIM_FLT_MOD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%f",Float,b);
   VM_ARG("%f",Float,a);

  return to(Float,(fmodf(a, b)));
}

// Primitive 45
Ptr PRIM_FIX_TO_FLT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("i->f",Fixnum,a);

  return to(Float,((f32)a));
}

// Primitive 46
Ptr PRIM_FLT_TO_FIX_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("f->i",Float,a);

  return to(Fixnum,((s64)a));
}

// Primitive 47
Ptr PRIM_FLT_LOG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("logf",Float,n);
   VM_ARG("logf",Float,base);

  return to(Float,(log(n) / log(base)));
}

// Primitive 48
Ptr PRIM_FLT_FLOOR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("floorf",Float,n);

  return to(Float,(floorf(n)));
}

// Primitive 49
Ptr PRIM_FLT_CEIL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ceilf",Float,n);

  return to(Float,(ceilf(n)));
}

// Primitive 50
Ptr PRIM_FLT_POW_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("powf",Float,e);
   VM_ARG("powf",Float,n);

  return to(Float,(pow(n, e)));
}

// Primitive 51
Ptr PRIM_FLT_REM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("remf",Float,n);

  return to(Float,(fractional_part(n)));
}

// Primitive 52
Ptr PRIM_FLT_COS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("cosf",Float,n);

  return to(Float,(cos(n)));
}

// Primitive 53
Ptr PRIM_FLT_SIN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("sinf",Float,n);

  return to(Float,(sin(n)));
}

// Primitive 54
Ptr PRIM_FLT_TAN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("tanf",Float,n);

  return to(Float,(tan(n)));
}

// Primitive 55
Ptr PRIM_FLT_A2F_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("atan2f",Float,x);
   VM_ARG("atan2f",Float,y);

  return to(Float,(atan2f(y,x)));
}

// Primitive 56
Ptr PRIM_FLT_SRT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("sqrtf",Float,a);

  return to(Float,(sqrtf(a)));
}

// Primitive 57
Ptr PRIM_RAND_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("random",Fixnum,a);

  return to(Fixnum,(rand() % a));
}

// Primitive 58
Ptr PRIM_ASH_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ash",Fixnum,shift);
   VM_ARG("ash",Fixnum,n);

  return to(Fixnum,(shift < 0 ? n >> abs(shift) : n << shift));
}

// Primitive 59
Ptr PRIM_BAND_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("bit-and",Fixnum,b);
   VM_ARG("bit-and",Fixnum,a);

  return to(Fixnum,(a & b));
}

// Primitive 60
Ptr PRIM_BOR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("bit-or",Fixnum,b);
   VM_ARG("bit-or",Fixnum,a);

  return to(Fixnum,(a | b));
}

// Primitive 61
Ptr PRIM_LIST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr list = vm_get_stack_values_as_list(vm, argc);
 return list;
}

// Primitive 62
Ptr PRIM_CONS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("cons",any,b);
   VM_ARG("cons",any,a);

 return cons(vm, a, b);
}

// Primitive 63
Ptr PRIM_CAR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("car",any,a);

 return car(a);
}

// Primitive 64
Ptr PRIM_CDR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("cdr",any,a);

 return cdr(a);
}

// Primitive 65
Ptr PRIM_EQ_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("eq",any,b);
   VM_ARG("eq",any,a);

  return to(Bool,(ptr_eq(a, b)));
}

// Primitive 66
Ptr PRIM_NOT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("not",any,a);

  return to(Bool,(a == False));
}

// Primitive 67
Ptr PRIM_PRINT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%print",any,a);

 return primitive_print(a);
}

// Primitive 68
Ptr PRIM_NTH_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("nth",Fixnum,idx);
   VM_ARG("nth",any,a);

 return nth_or_nil(a, idx);
}

// Primitive 69
Ptr PRIM_VEC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
Ptr as = vm_get_stack_values_as_list(vm, argc);
 return make_vector_from_list(vm, as);
}

// Primitive 70
Ptr PRIM_MKARY_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-array",Fixnum,len);

 return make_zf_array(vm, len);
}

// Primitive 71
Ptr PRIM_AGET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("aget",Fixnum,idx);
   VM_ARG("aget",PtrArray,a);

 return aget(a, idx);
}

// Primitive 72
Ptr PRIM_ASET_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("aset",any,val);
   VM_ARG("aset",Fixnum,idx);
   VM_ARG("aset",PtrArray,a);

 return aset(a, idx, val);
}

// Primitive 73
Ptr PRIM_ALEN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("array-length",PtrArray,a);

  return to(Fixnum,(array_length(a)));
}

// Primitive 74
Ptr PRIM_MKARYU16_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-array-u16",Fixnum,len);

 return to(Ptr, alloc_u16ao(vm, len));
}

// Primitive 75
Ptr PRIM_AGETU16_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("aget-u16",Fixnum,idx);
   VM_ARG("aget-u16",U16Array,a);

  return to(Fixnum,(aget(a, idx)));
}

// Primitive 76
Ptr PRIM_ASETU16_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("aset-u16",Fixnum,v);
   VM_ARG("aset-u16",Fixnum,idx);
   VM_ARG("aset-u16",U16Array,a);

 return aset(a, idx, v);
}

// Primitive 77
Ptr PRIM_ALENU16_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("array-length-u16",U16Array,a);

  return to(Fixnum,(a->length));
}

// Primitive 78
Ptr PRIM_MK_HT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return ht(vm);
}

// Primitive 79
Ptr PRIM_MK_ST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return string_table(vm);
}

// Primitive 80
Ptr PRIM_HT_AT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ht-at",any,key);
   VM_ARG("ht-at",any,ht);

 return ht_at(ht, key);
}

// Primitive 81
Ptr PRIM_HT_AT_PUT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ht-at-put",any,val);
   VM_ARG("ht-at-put",any,key);
   VM_ARG("ht-at-put",any,ht);

 return ht_at_put(vm, ht, key, val);
}

// Primitive 82
Ptr PRIM_HT_HAS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("ht-contains?",any,key);
   VM_ARG("ht-contains?",any,ht);

  return to(Bool,(ht_contains(ht, key)));
}

// Primitive 83
Ptr PRIM_SYM_NAME_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("symbol-name",any,a);

 return Symbol_get_name(a);
}

// Primitive 84
Ptr PRIM_SYM_PKG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("symbol-package",any,a);

 return Symbol_get_package(a);
}

// Primitive 85
Ptr PRIM_SYM_BND_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("symbol-bound?",any,a);

  return to(Bool,(boundp(vm, a)));
}

// Primitive 86
Ptr PRIM_SET_SYM_VAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("set-symbol-value",any,b);
   VM_ARG("set-symbol-value",any,a);

 return set_symbol_value(vm, a, b);
}

// Primitive 87
Ptr PRIM_SYM_VAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("symbol-value",any,a);

 return get_symbol_value(vm, a);
}

// Primitive 88
Ptr PRIM_SET_SYM_SPECIAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("mark-symbol-as-special",any,a);

 return mark_symbol_as_special(vm, a);
}

// Primitive 89
Ptr PRIM_SYM_SPECIAL_P_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("special-symbol?",any,a);

  return to(Bool,(is_special_symbol(vm, a)));
}

// Primitive 90
Ptr PRIM_PKG_EXT_SYM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-extern-symbol",any,b);
   VM_ARG("package-extern-symbol",any,a);

 return package_extern_symbol(vm, a, b);
}

// Primitive 91
Ptr PRIM_PKG_IMP_SYM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-import-symbol",any,sym);
   VM_ARG("package-import-symbol",any,pkg);

 return package_import_symbol(vm, pkg, sym);
}

// Primitive 92
Ptr PRIM_MK_USR_PKG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-user-package",any,name);

 return make_user_package(vm, name);
}

// Primitive 93
Ptr PRIM_MK_PKG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%make-package",any,name);

 return make_basic_package(vm, name);
}

// Primitive 94
Ptr PRIM_PKG_NAME_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-name",any,a);

 return package_get_name(a);
}

// Primitive 95
Ptr PRIM_PKG_SUBP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-subpackages",any,a);

 return package_get_subpackages(a);
}

// Primitive 96
Ptr PRIM_PKG_USELST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-use-list",any,a);

 return package_get_use_list(a);
}

// Primitive 97
Ptr PRIM_PKG_SET_USELST_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-set-use-list",any,lst);
   VM_ARG("package-set-use-list",any,pkg);

 return (package_set_use_list(pkg,lst), Nil);
}

// Primitive 98
Ptr PRIM_PKG_EXTERNS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-exports",any,a);

 return package_get_exports(a);
}

// Primitive 99
Ptr PRIM_PKG_META_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("package-meta",any,a);

 return package_get_meta(a);
}

// Primitive 100
Ptr PRIM_GSYM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return make_symbol(vm, "_gensym_");
}

// Primitive 101
Ptr PRIM_INTERN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("intern",any,pkg);
   VM_ARG("intern",String,a);

 return intern(vm, a, pkg);
}

// Primitive 102
Ptr PRIM_PRINT_STACK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return vm_print_stack_trace(vm);
}

// Primitive 103
Ptr PRIM_DBG_STACK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return vm_print_debug_stack_trace(vm);
}

// Primitive 104
Ptr PRIM_SETPXL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("set-pixel",Point,p);

 return gfx_set_pixel(vm, p);
}

// Primitive 105
Ptr PRIM_PTPLUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point+",Point,b);
   VM_ARG("point+",Point,a);

  return to(Point,(a + b));
}

// Primitive 106
Ptr PRIM_PTMINUS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-",Point,b);
   VM_ARG("point-",Point,a);

  return to(Point,(a - b));
}

// Primitive 107
Ptr PRIM_MKPOINT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-point",Fixnum,b);
   VM_ARG("make-point",Fixnum,a);

  return to(Point,((point){(s32)a, (s32)b}));
}

// Primitive 108
Ptr PRIM_PTX_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-x",Point,p);

  return to(Fixnum,((s64)p.x));
}

// Primitive 109
Ptr PRIM_PTY_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-y",Point,p);

  return to(Fixnum,((s64)p.y));
}

// Primitive 110
Ptr PRIM_PTROT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("point-rotate",Float,degrees);
   VM_ARG("point-rotate",Point,p);

  return to(Point,(rotate_point(p, degrees)));
}

// Primitive 111
Ptr PRIM_SFILLRCT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("screen-fill-rect",Fixnum,color);
   VM_ARG("screen-fill-rect",Point,b);
   VM_ARG("screen-fill-rect",Point,a);

 return gfx_screen_fill_rect(vm, a, b, color);
}

// Primitive 112
Ptr PRIM_DRAWIMAGE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("blit-to-screen",Fixnum,rot);
   VM_ARG("blit-to-screen",Fixnum,scale);
   VM_ARG("blit-to-screen",Point,p);
   VM_ARG("blit-to-screen",Image,img);

 return gfx_blit_image_at(vm, img, p, scale, rot);
}

// Primitive 113
Ptr PRIM_FILLRCT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("fill-rect",Fixnum,color);
   VM_ARG("fill-rect",Point,b);
   VM_ARG("fill-rect",Point,a);
   VM_ARG("fill-rect",Image,dst);

 return gfx_fill_rect(dst, a, b, color);
}

// Primitive 114
Ptr PRIM_CLRRCT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("clear-rect",Point,b);
   VM_ARG("clear-rect",Point,a);
   VM_ARG("clear-rect",Image,dst);

 return gfx_clear_rect(dst, a, b);
}

// Primitive 115
Ptr PRIM_BLT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("blit",Fixnum,tint);
   VM_ARG("blit",Float,degrees_rotation);
   VM_ARG("blit",Float,scale);
   VM_ARG("blit",Point,lr);
   VM_ARG("blit",Point,ul);
   VM_ARG("blit",Point,at);
   VM_ARG("blit",Image,dst);
   VM_ARG("blit",Image,src);

 return gfx_blit(src, dst, at, ul, lr, scale, degrees_rotation, tint);
}

// Primitive 116
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

// Primitive 117
Ptr PRIM_FIL_M_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("fill-rect-with-mask",Float,msk_rot);
   VM_ARG("fill-rect-with-mask",Float,msk_scale);
   VM_ARG("fill-rect-with-mask",Point,msk_lr);
   VM_ARG("fill-rect-with-mask",Point,msk_ul);
   VM_ARG("fill-rect-with-mask",Float,src_rot);
   VM_ARG("fill-rect-with-mask",Float,src_scale);
   VM_ARG("fill-rect-with-mask",Point,src_lr);
   VM_ARG("fill-rect-with-mask",Point,src_ul);
   VM_ARG("fill-rect-with-mask",Image,msk);
   VM_ARG("fill-rect-with-mask",Image,dst);
   VM_ARG("fill-rect-with-mask",Fixnum,color);

 return gfx_fill_rect_with_mask(color, dst, msk,
  points_to_rect(src_ul, src_lr), src_scale, src_rot,
  points_to_rect(msk_ul, msk_lr), msk_scale, msk_rot
);
}

// Primitive 118
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

// Primitive 119
Ptr PRIM_LOADIMAGE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("load-image",String,path);

 return gfx_load_image(vm, path);
}

// Primitive 120
Ptr PRIM_MKIMAGE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-image",Fixnum,h);
   VM_ARG("make-image",Fixnum,w);

 return gfx_make_image(vm, w, h);
}

// Primitive 121
Ptr PRIM_IMG_W_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("image-width",Image,img);

  return to(Fixnum,(image_width(img)));
}

// Primitive 122
Ptr PRIM_IMG_H_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("image-height",Image,img);

  return to(Fixnum,(image_height(img)));
}

// Primitive 123
Ptr PRIM_CCA_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-code-at",Fixnum,idx);
   VM_ARG("char-code-at",String,str);

  return to(Fixnum,(string_char_code_at(vm, str, idx)));
}

// Primitive 124
Ptr PRIM_CCO_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-code",Char,ch);

  return to(Fixnum,(character_to_s64(ch)));
}

// Primitive 125
Ptr PRIM_COC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("code-char",Fixnum,code);

 return to(Char, (char)code);
}

// Primitive 126
Ptr PRIM_CH_AT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-at",Fixnum,idx);
   VM_ARG("char-at",String,str);

  return to(Char,(string_char_at(vm, str, idx)));
}

// Primitive 127
Ptr PRIM_CH_ATP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-at-put",Char,ch);
   VM_ARG("char-at-put",Fixnum,idx);
   VM_ARG("char-at-put",String,str);

 return string_set_char_at(vm, str, idx, ch);
}

// Primitive 128
Ptr PRIM_CH_LT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-<",Char,b);
   VM_ARG("char-<",Char,a);

  return to(Bool,(character_lt(a,b)));
}

// Primitive 129
Ptr PRIM_CH_GT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char->",Char,b);
   VM_ARG("char->",Char,a);

  return to(Bool,(character_gt(a,b)));
}

// Primitive 130
Ptr PRIM_CH_W_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-width",Char,a);

  return to(Fixnum,(character_byte_width(a)));
}

// Primitive 131
Ptr PRIM_CHBYNM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-by-name",String,name);

 return character_by_name(name);
}

// Primitive 132
Ptr PRIM_CHNM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-name",Char,a);

 return character_name(vm, a);
}

// Primitive 133
Ptr PRIM_MKSTR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-string",Char,ch);
   VM_ARG("make-string",Fixnum,len);

 return make_filled_string(vm, len, ch);
}

// Primitive 134
Ptr PRIM_STRBLEN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("string-byte-length",String,str);

  return to(Fixnum,(string_byte_length(str)));
}

// Primitive 135
Ptr PRIM_STRSBB_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("string-substr-bytes",Fixnum,b);
   VM_ARG("string-substr-bytes",Fixnum,a);
   VM_ARG("string-substr-bytes",String,str);

 return string_substr_byte_range(vm, str, a, b);
}

// Primitive 136
Ptr PRIM_STR_EQUAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("string-equal",String,b);
   VM_ARG("string-equal",String,a);

  return to(Bool,(string_equal(a,b)));
}

// Primitive 137
Ptr PRIM_STRCHCNT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("string-char-count",String,str);

  return to(Fixnum,(string_char_count(str)));
}

// Primitive 138
Ptr PRIM_STR_CHARY_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("string->char-array",String,str);

  return to(Array,(array_from_string(vm, str)));
}

// Primitive 139
Ptr PRIM_CHARY_STR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("char-array->string",Array,arr);

  return to(String,(string_from_array(vm, arr)));
}

// Primitive 140
Ptr PRIM_SSTKMARK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("set-stack-mark",any,m);

 return vm_set_stack_mark(vm, m);
}

// Primitive 141
Ptr PRIM_PSTKMARK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("snapshot-to-stack-mark",any,v);
   VM_ARG("snapshot-to-stack-mark",any,m);

 return vm_abort_to_mark(vm, m, v);
}

// Primitive 142
Ptr PRIM_RSTKSNAP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("resume-stack-snapshot",any,arg);
   VM_ARG("resume-stack-snapshot",any,s);

 return vm_resume_stack_snapshot(vm, s, arg);
}

// Primitive 143
Ptr PRIM_RETMARK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("return-from-mark",any,a);
   VM_ARG("return-from-mark",any,m);

 return vm_return_from_mark(vm, m, a);
}

// Primitive 144
Ptr PRIM_CONT_VAL_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("continuation-value",any,a);

 return cont_get_value(a);
}

// Primitive 145
Ptr PRIM_FORK_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("fork-thunk",any,a);
   VM_ARG("fork-thunk",any,priority);

 return vm_schedule_closure(vm, a, priority, Nil);
}

// Primitive 146
Ptr PRIM_MK_SEM_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-semaphore",any,a);

 return make_semaphore(vm, a);
}

// Primitive 147
Ptr PRIM_SEM_SIG_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("signal-semaphore",any,a);

 return signal_semaphore(a);
}

// Primitive 148
Ptr PRIM_CURR_THD_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return vm->curr_thd->thread;
}

// Primitive 149
Ptr PRIM_THD_CNT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

  return to(Fixnum,(vm->threads->count));
}

// Primitive 150
Ptr PRIM_ALL_THDS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return list_all_threads(vm);
}

// Primitive 151
Ptr PRIM_CAS_VEC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("cas-vector",any,replace);
   VM_ARG("cas-vector",any,expect);
   VM_ARG("cas-vector",Fixnum,idx);
   VM_ARG("cas-vector",any,vec);

  return to(Bool,(cas_vector(vec, idx, expect, replace)));
}

// Primitive 152
Ptr PRIM_SLURP_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("slurp",String,path);

 return slurp(vm, path);
}

// Primitive 153
Ptr PRIM_OS_WSTR_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%file-output-stream-write-string",String,str);
   VM_ARG("%file-output-stream-write-string",any,s);

  return to(Bool,(file_output_stream_write_string(s, str)));
}

// Primitive 154
Ptr PRIM_OS_WCH_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("%file-output-stream-write-char",Char,ch);
   VM_ARG("%file-output-stream-write-char",any,s);

  return to(Bool,(file_output_stream_write_char(s, ch)));
}

// Primitive 155
Ptr PRIM_THD_DBG_INFO_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("thread-get-debug-info",any,a);

 return thread_get_debug_info(vm, a);
}

// Primitive 156
Ptr PRIM_IM_SAV_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("save-snapshot",String,path);

 return im_snapshot_to_path(vm, path);
}

// Primitive 157
Ptr PRIM_IM_SAV_DIE_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("save-snapshot-and-exit",String,path);

 return im_snapshot_to_path_and_exit(vm, path);
}

// Primitive 158
Ptr PRIM_TIME_MS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

  return to(Fixnum,(current_time_ms()));
}

// Primitive 159
Ptr PRIM_CL_SRC_LOC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("closure-source-location",any,a);

 return get_source_location(a);
}

// Primitive 160
Ptr PRIM_UPD_WIN_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return update_display(vm);
}

// Primitive 161
Ptr PRIM_BLTQ_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("blitq",Point,dd);
   VM_ARG("blitq",Point,dc);
   VM_ARG("blitq",Point,db);
   VM_ARG("blitq",Point,da);
   VM_ARG("blitq",Point,sd);
   VM_ARG("blitq",Point,sc);
   VM_ARG("blitq",Point,sb);
   VM_ARG("blitq",Point,sa);
   VM_ARG("blitq",Image,d);
   VM_ARG("blitq",Image,s);

 return gfx_blit_image_into_quad(s,d, sa,sb,sc,sd,da,db,dc,dd);
}

// Primitive 162
Ptr PRIM_EXIT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("exit",Fixnum,status);

 return (exit(status), Nil);
}

// Primitive 163
Ptr PRIM_PF_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("prefetch",any,it);

 return prefetch(it);
}

// Primitive 164
Ptr PRIM_BCTOCLS_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("bytecode->closure",any,it);

 return make_closure(vm, it, Nil);
}

// Primitive 165
Ptr PRIM_MKBTC_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);
   VM_ARG("make-bytecode",PtrArray,literals);
   VM_ARG("make-bytecode",U16Array,code);
   VM_ARG("make-bytecode",any,name);
   VM_ARG("make-bytecode",Bool,varargs);

 return create_bytecode(vm, varargs, name, code, literals);
}

// Primitive 166
Ptr PRIM_STKDPTH_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

  return to(Fixnum,(vm->curr_thd->stack_depth));
}

// Primitive 167
Ptr PRIM_STKDPTHB_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

  return to(Fixnum,((vm->curr_thd->stack_start - vm->curr_thd->stack) * 8));
}

// Primitive 168
Ptr PRIM_CLRSTAT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return reset_stats_reporting(vm);
}

// Primitive 169
Ptr PRIM_PRNSTAT_impl(VM *vm, u32 argc) {
  maybe_unused(vm); maybe_unused(argc);

 return print_stats_reporting(vm);
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
  &PRIM_ARGC_impl,
  &PRIM_LDARG_impl,
  &PRIM_OHIBITS_impl,
  &PRIM_OLOBITS_impl,
  &PRIM_CMPC_impl,
  &PRIM_CLASSOF_impl,
  &PRIM_MKCLASS_impl,
  &PRIM_SETMETHOD_impl,
  &PRIM_GETMETHOD_impl,
  &PRIM_CLSGETMETA_impl,
  &PRIM_CLSSETMETA_impl,
  &PRIM_CLSSETAPP_impl,
  &PRIM_MKINST_impl,
  &PRIM_IVAR_GET_impl,
  &PRIM_IVAR_SET_impl,
  &PRIM_IS_CLSS_impl,
  &PRIM_BIG_NEG_impl,
  &PRIM_BIG_TIMES_impl,
  &PRIM_BIG_PLUS_impl,
  &PRIM_BIG_MINUS_impl,
  &PRIM_BIG_LT_impl,
  &PRIM_BIG_GT_impl,
  &PRIM_BIG_EQL_impl,
  &PRIM_FIX_TO_BIG_impl,
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
  &PRIM_FLT_LOG_impl,
  &PRIM_FLT_FLOOR_impl,
  &PRIM_FLT_CEIL_impl,
  &PRIM_FLT_POW_impl,
  &PRIM_FLT_REM_impl,
  &PRIM_FLT_COS_impl,
  &PRIM_FLT_SIN_impl,
  &PRIM_FLT_TAN_impl,
  &PRIM_FLT_A2F_impl,
  &PRIM_FLT_SRT_impl,
  &PRIM_RAND_impl,
  &PRIM_ASH_impl,
  &PRIM_BAND_impl,
  &PRIM_BOR_impl,
  &PRIM_LIST_impl,
  &PRIM_CONS_impl,
  &PRIM_CAR_impl,
  &PRIM_CDR_impl,
  &PRIM_EQ_impl,
  &PRIM_NOT_impl,
  &PRIM_PRINT_impl,
  &PRIM_NTH_impl,
  &PRIM_VEC_impl,
  &PRIM_MKARY_impl,
  &PRIM_AGET_impl,
  &PRIM_ASET_impl,
  &PRIM_ALEN_impl,
  &PRIM_MKARYU16_impl,
  &PRIM_AGETU16_impl,
  &PRIM_ASETU16_impl,
  &PRIM_ALENU16_impl,
  &PRIM_MK_HT_impl,
  &PRIM_MK_ST_impl,
  &PRIM_HT_AT_impl,
  &PRIM_HT_AT_PUT_impl,
  &PRIM_HT_HAS_impl,
  &PRIM_SYM_NAME_impl,
  &PRIM_SYM_PKG_impl,
  &PRIM_SYM_BND_impl,
  &PRIM_SET_SYM_VAL_impl,
  &PRIM_SYM_VAL_impl,
  &PRIM_SET_SYM_SPECIAL_impl,
  &PRIM_SYM_SPECIAL_P_impl,
  &PRIM_PKG_EXT_SYM_impl,
  &PRIM_PKG_IMP_SYM_impl,
  &PRIM_MK_USR_PKG_impl,
  &PRIM_MK_PKG_impl,
  &PRIM_PKG_NAME_impl,
  &PRIM_PKG_SUBP_impl,
  &PRIM_PKG_USELST_impl,
  &PRIM_PKG_SET_USELST_impl,
  &PRIM_PKG_EXTERNS_impl,
  &PRIM_PKG_META_impl,
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
  &PRIM_FIL_M_impl,
  &PRIM_BLT_FR_SCRN_impl,
  &PRIM_LOADIMAGE_impl,
  &PRIM_MKIMAGE_impl,
  &PRIM_IMG_W_impl,
  &PRIM_IMG_H_impl,
  &PRIM_CCA_impl,
  &PRIM_CCO_impl,
  &PRIM_COC_impl,
  &PRIM_CH_AT_impl,
  &PRIM_CH_ATP_impl,
  &PRIM_CH_LT_impl,
  &PRIM_CH_GT_impl,
  &PRIM_CH_W_impl,
  &PRIM_CHBYNM_impl,
  &PRIM_CHNM_impl,
  &PRIM_MKSTR_impl,
  &PRIM_STRBLEN_impl,
  &PRIM_STRSBB_impl,
  &PRIM_STR_EQUAL_impl,
  &PRIM_STRCHCNT_impl,
  &PRIM_STR_CHARY_impl,
  &PRIM_CHARY_STR_impl,
  &PRIM_SSTKMARK_impl,
  &PRIM_PSTKMARK_impl,
  &PRIM_RSTKSNAP_impl,
  &PRIM_RETMARK_impl,
  &PRIM_CONT_VAL_impl,
  &PRIM_FORK_impl,
  &PRIM_MK_SEM_impl,
  &PRIM_SEM_SIG_impl,
  &PRIM_CURR_THD_impl,
  &PRIM_THD_CNT_impl,
  &PRIM_ALL_THDS_impl,
  &PRIM_CAS_VEC_impl,
  &PRIM_SLURP_impl,
  &PRIM_OS_WSTR_impl,
  &PRIM_OS_WCH_impl,
  &PRIM_THD_DBG_INFO_impl,
  &PRIM_IM_SAV_impl,
  &PRIM_IM_SAV_DIE_impl,
  &PRIM_TIME_MS_impl,
  &PRIM_CL_SRC_LOC_impl,
  &PRIM_UPD_WIN_impl,
  &PRIM_BLTQ_impl,
  &PRIM_EXIT_impl,
  &PRIM_PF_impl,
  &PRIM_BCTOCLS_impl,
  &PRIM_MKBTC_impl,
  &PRIM_STKDPTH_impl,
  &PRIM_STKDPTHB_impl,
  &PRIM_CLRSTAT_impl,
  &PRIM_PRNSTAT_impl,

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
  set_global(vm, "%argument-count", to(PrimOp, PRIM_ARGC));
  set_global(vm, "%load-arg", to(PrimOp, PRIM_LDARG));
  set_global(vm, "%obj-high-bits", to(PrimOp, PRIM_OHIBITS));
  set_global(vm, "%obj-low-bits", to(PrimOp, PRIM_OLOBITS));
  set_global(vm, "compile-to-closure", to(PrimOp, PRIM_CMPC));
  set_global(vm, "class-of", to(PrimOp, PRIM_CLASSOF));
  set_global(vm, "create-class", to(PrimOp, PRIM_MKCLASS));
  set_global(vm, "class-set-method", to(PrimOp, PRIM_SETMETHOD));
  set_global(vm, "class-get-method", to(PrimOp, PRIM_GETMETHOD));
  set_global(vm, "class-get-metadata", to(PrimOp, PRIM_CLSGETMETA));
  set_global(vm, "class-set-metadata", to(PrimOp, PRIM_CLSSETMETA));
  set_global(vm, "class-set-applicator", to(PrimOp, PRIM_CLSSETAPP));
  set_global(vm, "instantiate-class", to(PrimOp, PRIM_MKINST));
  set_global(vm, "instance-get-ivar", to(PrimOp, PRIM_IVAR_GET));
  set_global(vm, "instance-set-ivar", to(PrimOp, PRIM_IVAR_SET));
  set_global(vm, "class?", to(PrimOp, PRIM_IS_CLSS));
  set_global(vm, "%negate-bignum", to(PrimOp, PRIM_BIG_NEG));
  set_global(vm, "*b", to(PrimOp, PRIM_BIG_TIMES));
  set_global(vm, "+b", to(PrimOp, PRIM_BIG_PLUS));
  set_global(vm, "-b", to(PrimOp, PRIM_BIG_MINUS));
  set_global(vm, "<b", to(PrimOp, PRIM_BIG_LT));
  set_global(vm, ">b", to(PrimOp, PRIM_BIG_GT));
  set_global(vm, "=b", to(PrimOp, PRIM_BIG_EQL));
  set_global(vm, "i->b", to(PrimOp, PRIM_FIX_TO_BIG));
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
  set_global(vm, "logf", to(PrimOp, PRIM_FLT_LOG));
  set_global(vm, "floorf", to(PrimOp, PRIM_FLT_FLOOR));
  set_global(vm, "ceilf", to(PrimOp, PRIM_FLT_CEIL));
  set_global(vm, "powf", to(PrimOp, PRIM_FLT_POW));
  set_global(vm, "remf", to(PrimOp, PRIM_FLT_REM));
  set_global(vm, "cosf", to(PrimOp, PRIM_FLT_COS));
  set_global(vm, "sinf", to(PrimOp, PRIM_FLT_SIN));
  set_global(vm, "tanf", to(PrimOp, PRIM_FLT_TAN));
  set_global(vm, "atan2f", to(PrimOp, PRIM_FLT_A2F));
  set_global(vm, "sqrtf", to(PrimOp, PRIM_FLT_SRT));
  set_global(vm, "random", to(PrimOp, PRIM_RAND));
  set_global(vm, "ash", to(PrimOp, PRIM_ASH));
  set_global(vm, "bit-and", to(PrimOp, PRIM_BAND));
  set_global(vm, "bit-or", to(PrimOp, PRIM_BOR));
  set_global(vm, "list", to(PrimOp, PRIM_LIST));
  set_global(vm, "cons", to(PrimOp, PRIM_CONS));
  set_global(vm, "car", to(PrimOp, PRIM_CAR));
  set_global(vm, "cdr", to(PrimOp, PRIM_CDR));
  set_global(vm, "eq", to(PrimOp, PRIM_EQ));
  set_global(vm, "not", to(PrimOp, PRIM_NOT));
  set_global(vm, "%print", to(PrimOp, PRIM_PRINT));
  set_global(vm, "nth", to(PrimOp, PRIM_NTH));
  set_global(vm, "vector", to(PrimOp, PRIM_VEC));
  set_global(vm, "make-array", to(PrimOp, PRIM_MKARY));
  set_global(vm, "aget", to(PrimOp, PRIM_AGET));
  set_global(vm, "aset", to(PrimOp, PRIM_ASET));
  set_global(vm, "array-length", to(PrimOp, PRIM_ALEN));
  set_global(vm, "make-array-u16", to(PrimOp, PRIM_MKARYU16));
  set_global(vm, "aget-u16", to(PrimOp, PRIM_AGETU16));
  set_global(vm, "aset-u16", to(PrimOp, PRIM_ASETU16));
  set_global(vm, "array-length-u16", to(PrimOp, PRIM_ALENU16));
  set_global(vm, "make-ht", to(PrimOp, PRIM_MK_HT));
  set_global(vm, "make-st", to(PrimOp, PRIM_MK_ST));
  set_global(vm, "ht-at", to(PrimOp, PRIM_HT_AT));
  set_global(vm, "ht-at-put", to(PrimOp, PRIM_HT_AT_PUT));
  set_global(vm, "ht-contains?", to(PrimOp, PRIM_HT_HAS));
  set_global(vm, "symbol-name", to(PrimOp, PRIM_SYM_NAME));
  set_global(vm, "symbol-package", to(PrimOp, PRIM_SYM_PKG));
  set_global(vm, "symbol-bound?", to(PrimOp, PRIM_SYM_BND));
  set_global(vm, "set-symbol-value", to(PrimOp, PRIM_SET_SYM_VAL));
  set_global(vm, "symbol-value", to(PrimOp, PRIM_SYM_VAL));
  set_global(vm, "mark-symbol-as-special", to(PrimOp, PRIM_SET_SYM_SPECIAL));
  set_global(vm, "special-symbol?", to(PrimOp, PRIM_SYM_SPECIAL_P));
  set_global(vm, "package-extern-symbol", to(PrimOp, PRIM_PKG_EXT_SYM));
  set_global(vm, "package-import-symbol", to(PrimOp, PRIM_PKG_IMP_SYM));
  set_global(vm, "make-user-package", to(PrimOp, PRIM_MK_USR_PKG));
  set_global(vm, "%make-package", to(PrimOp, PRIM_MK_PKG));
  set_global(vm, "package-name", to(PrimOp, PRIM_PKG_NAME));
  set_global(vm, "package-subpackages", to(PrimOp, PRIM_PKG_SUBP));
  set_global(vm, "package-use-list", to(PrimOp, PRIM_PKG_USELST));
  set_global(vm, "package-set-use-list", to(PrimOp, PRIM_PKG_SET_USELST));
  set_global(vm, "package-exports", to(PrimOp, PRIM_PKG_EXTERNS));
  set_global(vm, "package-meta", to(PrimOp, PRIM_PKG_META));
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
  set_global(vm, "fill-rect-with-mask", to(PrimOp, PRIM_FIL_M));
  set_global(vm, "blit-from-screen", to(PrimOp, PRIM_BLT_FR_SCRN));
  set_global(vm, "load-image", to(PrimOp, PRIM_LOADIMAGE));
  set_global(vm, "make-image", to(PrimOp, PRIM_MKIMAGE));
  set_global(vm, "image-width", to(PrimOp, PRIM_IMG_W));
  set_global(vm, "image-height", to(PrimOp, PRIM_IMG_H));
  set_global(vm, "char-code-at", to(PrimOp, PRIM_CCA));
  set_global(vm, "char-code", to(PrimOp, PRIM_CCO));
  set_global(vm, "code-char", to(PrimOp, PRIM_COC));
  set_global(vm, "char-at", to(PrimOp, PRIM_CH_AT));
  set_global(vm, "char-at-put", to(PrimOp, PRIM_CH_ATP));
  set_global(vm, "char-<", to(PrimOp, PRIM_CH_LT));
  set_global(vm, "char->", to(PrimOp, PRIM_CH_GT));
  set_global(vm, "char-width", to(PrimOp, PRIM_CH_W));
  set_global(vm, "char-by-name", to(PrimOp, PRIM_CHBYNM));
  set_global(vm, "char-name", to(PrimOp, PRIM_CHNM));
  set_global(vm, "make-string", to(PrimOp, PRIM_MKSTR));
  set_global(vm, "string-byte-length", to(PrimOp, PRIM_STRBLEN));
  set_global(vm, "string-substr-bytes", to(PrimOp, PRIM_STRSBB));
  set_global(vm, "string-equal", to(PrimOp, PRIM_STR_EQUAL));
  set_global(vm, "string-char-count", to(PrimOp, PRIM_STRCHCNT));
  set_global(vm, "string->char-array", to(PrimOp, PRIM_STR_CHARY));
  set_global(vm, "char-array->string", to(PrimOp, PRIM_CHARY_STR));
  set_global(vm, "set-stack-mark", to(PrimOp, PRIM_SSTKMARK));
  set_global(vm, "snapshot-to-stack-mark", to(PrimOp, PRIM_PSTKMARK));
  set_global(vm, "resume-stack-snapshot", to(PrimOp, PRIM_RSTKSNAP));
  set_global(vm, "return-from-mark", to(PrimOp, PRIM_RETMARK));
  set_global(vm, "continuation-value", to(PrimOp, PRIM_CONT_VAL));
  set_global(vm, "fork-thunk", to(PrimOp, PRIM_FORK));
  set_global(vm, "make-semaphore", to(PrimOp, PRIM_MK_SEM));
  set_global(vm, "signal-semaphore", to(PrimOp, PRIM_SEM_SIG));
  set_global(vm, "current-thread", to(PrimOp, PRIM_CURR_THD));
  set_global(vm, "thread-count", to(PrimOp, PRIM_THD_CNT));
  set_global(vm, "list-all-threads", to(PrimOp, PRIM_ALL_THDS));
  set_global(vm, "cas-vector", to(PrimOp, PRIM_CAS_VEC));
  set_global(vm, "slurp", to(PrimOp, PRIM_SLURP));
  set_global(vm, "%file-output-stream-write-string", to(PrimOp, PRIM_OS_WSTR));
  set_global(vm, "%file-output-stream-write-char", to(PrimOp, PRIM_OS_WCH));
  set_global(vm, "thread-get-debug-info", to(PrimOp, PRIM_THD_DBG_INFO));
  set_global(vm, "save-snapshot", to(PrimOp, PRIM_IM_SAV));
  set_global(vm, "save-snapshot-and-exit", to(PrimOp, PRIM_IM_SAV_DIE));
  set_global(vm, "current-time-ms", to(PrimOp, PRIM_TIME_MS));
  set_global(vm, "closure-source-location", to(PrimOp, PRIM_CL_SRC_LOC));
  set_global(vm, "update-display", to(PrimOp, PRIM_UPD_WIN));
  set_global(vm, "blitq", to(PrimOp, PRIM_BLTQ));
  set_global(vm, "exit", to(PrimOp, PRIM_EXIT));
  set_global(vm, "prefetch", to(PrimOp, PRIM_PF));
  set_global(vm, "bytecode->closure", to(PrimOp, PRIM_BCTOCLS));
  set_global(vm, "make-bytecode", to(PrimOp, PRIM_MKBTC));
  set_global(vm, "%stack-depth", to(PrimOp, PRIM_STKDPTH));
  set_global(vm, "%stack-depth-in-bytes", to(PrimOp, PRIM_STKDPTHB));
  set_global(vm, "%clear-stats", to(PrimOp, PRIM_CLRSTAT));
  set_global(vm, "%print-stats", to(PrimOp, PRIM_PRNSTAT));

}

inline Ptr giant_switch(VM *vm, u32 argc, u32 idx) {
  switch(idx) {
   
  case 1: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    return(unused);
    break;
  }

  case 2: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    return(unused);
    break;
  }

  case 3: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    return(unused);
    break;
  }

  case 4: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    return(unused);
    break;
  }

  case 5: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    return(unused);
    break;
  }

  case 6: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    return(unused);
    break;
  }

  case 7: {
Ptr unused = vm_get_stack_values_as_list(vm, argc);
    return(unused);
    break;
  }

  case 8: {

     return(to(Fixnum,((s64)vm->curr_frame->argc)));
    break;
  }

  case 9: {
   VM_ARG("%load-arg",Fixnum,it);

    return(vm_load_arg(vm, it));
    break;
  }

  case 10: {
   VM_ARG("%obj-high-bits",Object,it);

     return(to(Fixnum,(((u64) it >> 32))));
    break;
  }

  case 11: {
   VM_ARG("%obj-low-bits",Object,it);

     return(to(Fixnum,(((u64) it & 0xFFFF))));
    break;
  }

  case 12: {
   VM_ARG("compile-to-closure",any,expr);

    return(compile_to_closure(vm, expr));
    break;
  }

  case 13: {
   VM_ARG("class-of",any,a);

    return(class_of(vm, a));
    break;
  }

  case 14: {
   VM_ARG("create-class",any,ivars);
   VM_ARG("create-class",any,name);

    return(make_user_class(vm, name, ivars));
    break;
  }

  case 15: {
   VM_ARG("class-set-method",any,fn);
   VM_ARG("class-set-method",any,sym);
   VM_ARG("class-set-method",Standard,a);

    return(class_set_method(vm, a, sym, fn));
    break;
  }

  case 16: {
   VM_ARG("class-get-method",any,sym);
   VM_ARG("class-get-method",Standard,a);

    return(class_get_method(vm, a, sym));
    break;
  }

  case 17: {
   VM_ARG("class-get-metadata",any,key);
   VM_ARG("class-get-metadata",Standard,a);

    return(class_get_metadata(a, key));
    break;
  }

  case 18: {
   VM_ARG("class-set-metadata",any,val);
   VM_ARG("class-set-metadata",any,key);
   VM_ARG("class-set-metadata",Standard,a);

    return(class_set_metadata(vm, a, key, val));
    break;
  }

  case 19: {
   VM_ARG("class-set-applicator",any,fn);
   VM_ARG("class-set-applicator",Standard,a);

    return(class_set_applicator(a, fn));
    break;
  }

  case 20: {
   VM_ARG("instantiate-class",Standard,klass);

    return(instantiate_user_class(vm, klass));
    break;
  }

  case 21: {
   VM_ARG("instance-get-ivar",Fixnum,idx);
   VM_ARG("instance-get-ivar",Standard,obj);

    return(standard_object_get_ivar(obj, idx));
    break;
  }

  case 22: {
   VM_ARG("instance-set-ivar",any,val);
   VM_ARG("instance-set-ivar",Fixnum,idx);
   VM_ARG("instance-set-ivar",Standard,obj);

    return(standard_object_set_ivar(obj, idx, val));
    break;
  }

  case 23: {
   VM_ARG("class?",any,a);

     return(to(Bool,(is_class(a))));
    break;
  }

  case 24: {
   VM_ARG("%negate-bignum",Bignum,a);

     return(to(Bignum,(bignum_negate(vm, a))));
    break;
  }

  case 25: {
   VM_ARG("*b",Bignum,b);
   VM_ARG("*b",Bignum,a);

     return(to(Bignum,(bignum_mul(vm, a, b))));
    break;
  }

  case 26: {
   VM_ARG("+b",Bignum,b);
   VM_ARG("+b",Bignum,a);

     return(to(Bignum,(bignum_add(vm, a, b))));
    break;
  }

  case 27: {
   VM_ARG("-b",Bignum,b);
   VM_ARG("-b",Bignum,a);

     return(to(Bignum,(bignum_sub(vm, a, b))));
    break;
  }

  case 28: {
   VM_ARG("<b",Bignum,b);
   VM_ARG("<b",Bignum,a);

     return(to(Bool,(bignum_lt(a, b))));
    break;
  }

  case 29: {
   VM_ARG(">b",Bignum,b);
   VM_ARG(">b",Bignum,a);

     return(to(Bool,(bignum_gt(a,b))));
    break;
  }

  case 30: {
   VM_ARG("=b",Bignum,b);
   VM_ARG("=b",Bignum,a);

     return(to(Bool,(bignum_eql(a, b))));
    break;
  }

  case 31: {
   VM_ARG("i->b",Fixnum,a);

     return(to(Bignum,(bignum_from_fixnum(vm, a))));
    break;
  }

  case 32: {
   VM_ARG("+i",Fixnum,b);
   VM_ARG("+i",Fixnum,a);

    return(fixnum_add(vm, a, b));
    break;
  }

  case 33: {
   VM_ARG("-i",Fixnum,b);
   VM_ARG("-i",Fixnum,a);

    return(fixnum_sub(vm, a, b));
    break;
  }

  case 34: {
   VM_ARG("*i",Fixnum,b);
   VM_ARG("*i",Fixnum,a);

    return(fixnum_mul(vm, a, b));
    break;
  }

  case 35: {
   VM_ARG("/i",Fixnum,b);
   VM_ARG("/i",Fixnum,a);

     return(to(Fixnum,(a / b)));
    break;
  }

  case 36: {
   VM_ARG("<i",Fixnum,b);
   VM_ARG("<i",Fixnum,a);

     return(to(Bool,(a < b)));
    break;
  }

  case 37: {
   VM_ARG(">i",Fixnum,b);
   VM_ARG(">i",Fixnum,a);

     return(to(Bool,(a > b)));
    break;
  }

  case 38: {
   VM_ARG("%i",Fixnum,b);
   VM_ARG("%i",Fixnum,a);

     return(to(Fixnum,(a % b)));
    break;
  }

  case 39: {
   VM_ARG("+f",Float,b);
   VM_ARG("+f",Float,a);

     return(to(Float,(a + b)));
    break;
  }

  case 40: {
   VM_ARG("-f",Float,b);
   VM_ARG("-f",Float,a);

     return(to(Float,(a - b)));
    break;
  }

  case 41: {
   VM_ARG("*f",Float,b);
   VM_ARG("*f",Float,a);

     return(to(Float,(a * b)));
    break;
  }

  case 42: {
   VM_ARG("/f",Float,b);
   VM_ARG("/f",Float,a);

     return(to(Float,(a / b)));
    break;
  }

  case 43: {
   VM_ARG("<f",Float,b);
   VM_ARG("<f",Float,a);

     return(to(Bool,(a < b)));
    break;
  }

  case 44: {
   VM_ARG(">f",Float,b);
   VM_ARG(">f",Float,a);

     return(to(Bool,(a > b)));
    break;
  }

  case 45: {
   VM_ARG("%f",Float,b);
   VM_ARG("%f",Float,a);

     return(to(Float,(fmodf(a, b))));
    break;
  }

  case 46: {
   VM_ARG("i->f",Fixnum,a);

     return(to(Float,((f32)a)));
    break;
  }

  case 47: {
   VM_ARG("f->i",Float,a);

     return(to(Fixnum,((s64)a)));
    break;
  }

  case 48: {
   VM_ARG("logf",Float,n);
   VM_ARG("logf",Float,base);

     return(to(Float,(log(n) / log(base))));
    break;
  }

  case 49: {
   VM_ARG("floorf",Float,n);

     return(to(Float,(floorf(n))));
    break;
  }

  case 50: {
   VM_ARG("ceilf",Float,n);

     return(to(Float,(ceilf(n))));
    break;
  }

  case 51: {
   VM_ARG("powf",Float,e);
   VM_ARG("powf",Float,n);

     return(to(Float,(pow(n, e))));
    break;
  }

  case 52: {
   VM_ARG("remf",Float,n);

     return(to(Float,(fractional_part(n))));
    break;
  }

  case 53: {
   VM_ARG("cosf",Float,n);

     return(to(Float,(cos(n))));
    break;
  }

  case 54: {
   VM_ARG("sinf",Float,n);

     return(to(Float,(sin(n))));
    break;
  }

  case 55: {
   VM_ARG("tanf",Float,n);

     return(to(Float,(tan(n))));
    break;
  }

  case 56: {
   VM_ARG("atan2f",Float,x);
   VM_ARG("atan2f",Float,y);

     return(to(Float,(atan2f(y,x))));
    break;
  }

  case 57: {
   VM_ARG("sqrtf",Float,a);

     return(to(Float,(sqrtf(a))));
    break;
  }

  case 58: {
   VM_ARG("random",Fixnum,a);

     return(to(Fixnum,(rand() % a)));
    break;
  }

  case 59: {
   VM_ARG("ash",Fixnum,shift);
   VM_ARG("ash",Fixnum,n);

     return(to(Fixnum,(shift < 0 ? n >> abs(shift) : n << shift)));
    break;
  }

  case 60: {
   VM_ARG("bit-and",Fixnum,b);
   VM_ARG("bit-and",Fixnum,a);

     return(to(Fixnum,(a & b)));
    break;
  }

  case 61: {
   VM_ARG("bit-or",Fixnum,b);
   VM_ARG("bit-or",Fixnum,a);

     return(to(Fixnum,(a | b)));
    break;
  }

  case 62: {
Ptr list = vm_get_stack_values_as_list(vm, argc);
    return(list);
    break;
  }

  case 63: {
   VM_ARG("cons",any,b);
   VM_ARG("cons",any,a);

    return(cons(vm, a, b));
    break;
  }

  case 64: {
   VM_ARG("car",any,a);

    return(car(a));
    break;
  }

  case 65: {
   VM_ARG("cdr",any,a);

    return(cdr(a));
    break;
  }

  case 66: {
   VM_ARG("eq",any,b);
   VM_ARG("eq",any,a);

     return(to(Bool,(ptr_eq(a, b))));
    break;
  }

  case 67: {
   VM_ARG("not",any,a);

     return(to(Bool,(a == False)));
    break;
  }

  case 68: {
   VM_ARG("%print",any,a);

    return(primitive_print(a));
    break;
  }

  case 69: {
   VM_ARG("nth",Fixnum,idx);
   VM_ARG("nth",any,a);

    return(nth_or_nil(a, idx));
    break;
  }

  case 70: {
Ptr as = vm_get_stack_values_as_list(vm, argc);
    return(make_vector_from_list(vm, as));
    break;
  }

  case 71: {
   VM_ARG("make-array",Fixnum,len);

    return(make_zf_array(vm, len));
    break;
  }

  case 72: {
   VM_ARG("aget",Fixnum,idx);
   VM_ARG("aget",PtrArray,a);

    return(aget(a, idx));
    break;
  }

  case 73: {
   VM_ARG("aset",any,val);
   VM_ARG("aset",Fixnum,idx);
   VM_ARG("aset",PtrArray,a);

    return(aset(a, idx, val));
    break;
  }

  case 74: {
   VM_ARG("array-length",PtrArray,a);

     return(to(Fixnum,(array_length(a))));
    break;
  }

  case 75: {
   VM_ARG("make-array-u16",Fixnum,len);

    return(to(Ptr, alloc_u16ao(vm, len)));
    break;
  }

  case 76: {
   VM_ARG("aget-u16",Fixnum,idx);
   VM_ARG("aget-u16",U16Array,a);

     return(to(Fixnum,(aget(a, idx))));
    break;
  }

  case 77: {
   VM_ARG("aset-u16",Fixnum,v);
   VM_ARG("aset-u16",Fixnum,idx);
   VM_ARG("aset-u16",U16Array,a);

    return(aset(a, idx, v));
    break;
  }

  case 78: {
   VM_ARG("array-length-u16",U16Array,a);

     return(to(Fixnum,(a->length)));
    break;
  }

  case 79: {

    return(ht(vm));
    break;
  }

  case 80: {

    return(string_table(vm));
    break;
  }

  case 81: {
   VM_ARG("ht-at",any,key);
   VM_ARG("ht-at",any,ht);

    return(ht_at(ht, key));
    break;
  }

  case 82: {
   VM_ARG("ht-at-put",any,val);
   VM_ARG("ht-at-put",any,key);
   VM_ARG("ht-at-put",any,ht);

    return(ht_at_put(vm, ht, key, val));
    break;
  }

  case 83: {
   VM_ARG("ht-contains?",any,key);
   VM_ARG("ht-contains?",any,ht);

     return(to(Bool,(ht_contains(ht, key))));
    break;
  }

  case 84: {
   VM_ARG("symbol-name",any,a);

    return(Symbol_get_name(a));
    break;
  }

  case 85: {
   VM_ARG("symbol-package",any,a);

    return(Symbol_get_package(a));
    break;
  }

  case 86: {
   VM_ARG("symbol-bound?",any,a);

     return(to(Bool,(boundp(vm, a))));
    break;
  }

  case 87: {
   VM_ARG("set-symbol-value",any,b);
   VM_ARG("set-symbol-value",any,a);

    return(set_symbol_value(vm, a, b));
    break;
  }

  case 88: {
   VM_ARG("symbol-value",any,a);

    return(get_symbol_value(vm, a));
    break;
  }

  case 89: {
   VM_ARG("mark-symbol-as-special",any,a);

    return(mark_symbol_as_special(vm, a));
    break;
  }

  case 90: {
   VM_ARG("special-symbol?",any,a);

     return(to(Bool,(is_special_symbol(vm, a))));
    break;
  }

  case 91: {
   VM_ARG("package-extern-symbol",any,b);
   VM_ARG("package-extern-symbol",any,a);

    return(package_extern_symbol(vm, a, b));
    break;
  }

  case 92: {
   VM_ARG("package-import-symbol",any,sym);
   VM_ARG("package-import-symbol",any,pkg);

    return(package_import_symbol(vm, pkg, sym));
    break;
  }

  case 93: {
   VM_ARG("make-user-package",any,name);

    return(make_user_package(vm, name));
    break;
  }

  case 94: {
   VM_ARG("%make-package",any,name);

    return(make_basic_package(vm, name));
    break;
  }

  case 95: {
   VM_ARG("package-name",any,a);

    return(package_get_name(a));
    break;
  }

  case 96: {
   VM_ARG("package-subpackages",any,a);

    return(package_get_subpackages(a));
    break;
  }

  case 97: {
   VM_ARG("package-use-list",any,a);

    return(package_get_use_list(a));
    break;
  }

  case 98: {
   VM_ARG("package-set-use-list",any,lst);
   VM_ARG("package-set-use-list",any,pkg);

    return((package_set_use_list(pkg,lst), Nil));
    break;
  }

  case 99: {
   VM_ARG("package-exports",any,a);

    return(package_get_exports(a));
    break;
  }

  case 100: {
   VM_ARG("package-meta",any,a);

    return(package_get_meta(a));
    break;
  }

  case 101: {

    return(make_symbol(vm, "_gensym_"));
    break;
  }

  case 102: {
   VM_ARG("intern",any,pkg);
   VM_ARG("intern",String,a);

    return(intern(vm, a, pkg));
    break;
  }

  case 103: {

    return(vm_print_stack_trace(vm));
    break;
  }

  case 104: {

    return(vm_print_debug_stack_trace(vm));
    break;
  }

  case 105: {
   VM_ARG("set-pixel",Point,p);

    return(gfx_set_pixel(vm, p));
    break;
  }

  case 106: {
   VM_ARG("point+",Point,b);
   VM_ARG("point+",Point,a);

     return(to(Point,(a + b)));
    break;
  }

  case 107: {
   VM_ARG("point-",Point,b);
   VM_ARG("point-",Point,a);

     return(to(Point,(a - b)));
    break;
  }

  case 108: {
   VM_ARG("make-point",Fixnum,b);
   VM_ARG("make-point",Fixnum,a);

     return(to(Point,((point){(s32)a, (s32)b})));
    break;
  }

  case 109: {
   VM_ARG("point-x",Point,p);

     return(to(Fixnum,((s64)p.x)));
    break;
  }

  case 110: {
   VM_ARG("point-y",Point,p);

     return(to(Fixnum,((s64)p.y)));
    break;
  }

  case 111: {
   VM_ARG("point-rotate",Float,degrees);
   VM_ARG("point-rotate",Point,p);

     return(to(Point,(rotate_point(p, degrees))));
    break;
  }

  case 112: {
   VM_ARG("screen-fill-rect",Fixnum,color);
   VM_ARG("screen-fill-rect",Point,b);
   VM_ARG("screen-fill-rect",Point,a);

    return(gfx_screen_fill_rect(vm, a, b, color));
    break;
  }

  case 113: {
   VM_ARG("blit-to-screen",Fixnum,rot);
   VM_ARG("blit-to-screen",Fixnum,scale);
   VM_ARG("blit-to-screen",Point,p);
   VM_ARG("blit-to-screen",Image,img);

    return(gfx_blit_image_at(vm, img, p, scale, rot));
    break;
  }

  case 114: {
   VM_ARG("fill-rect",Fixnum,color);
   VM_ARG("fill-rect",Point,b);
   VM_ARG("fill-rect",Point,a);
   VM_ARG("fill-rect",Image,dst);

    return(gfx_fill_rect(dst, a, b, color));
    break;
  }

  case 115: {
   VM_ARG("clear-rect",Point,b);
   VM_ARG("clear-rect",Point,a);
   VM_ARG("clear-rect",Image,dst);

    return(gfx_clear_rect(dst, a, b));
    break;
  }

  case 116: {
   VM_ARG("blit",Fixnum,tint);
   VM_ARG("blit",Float,degrees_rotation);
   VM_ARG("blit",Float,scale);
   VM_ARG("blit",Point,lr);
   VM_ARG("blit",Point,ul);
   VM_ARG("blit",Point,at);
   VM_ARG("blit",Image,dst);
   VM_ARG("blit",Image,src);

    return(gfx_blit(src, dst, at, ul, lr, scale, degrees_rotation, tint));
    break;
  }

  case 117: {
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

    return(gfx_blit_image_with_mask(src, dst, msk, at,
  points_to_rect(src_ul, src_lr), src_scale, src_rot,
  points_to_rect(msk_ul, msk_lr), msk_scale, msk_rot
));
    break;
  }

  case 118: {
   VM_ARG("fill-rect-with-mask",Float,msk_rot);
   VM_ARG("fill-rect-with-mask",Float,msk_scale);
   VM_ARG("fill-rect-with-mask",Point,msk_lr);
   VM_ARG("fill-rect-with-mask",Point,msk_ul);
   VM_ARG("fill-rect-with-mask",Float,src_rot);
   VM_ARG("fill-rect-with-mask",Float,src_scale);
   VM_ARG("fill-rect-with-mask",Point,src_lr);
   VM_ARG("fill-rect-with-mask",Point,src_ul);
   VM_ARG("fill-rect-with-mask",Image,msk);
   VM_ARG("fill-rect-with-mask",Image,dst);
   VM_ARG("fill-rect-with-mask",Fixnum,color);

    return(gfx_fill_rect_with_mask(color, dst, msk,
  points_to_rect(src_ul, src_lr), src_scale, src_rot,
  points_to_rect(msk_ul, msk_lr), msk_scale, msk_rot
));
    break;
  }

  case 119: {
   VM_ARG("blit-from-screen",Float,degrees_rotation);
   VM_ARG("blit-from-screen",Float,scale);
   VM_ARG("blit-from-screen",Point,lr);
   VM_ARG("blit-from-screen",Point,ul);
   VM_ARG("blit-from-screen",Point,at);
   VM_ARG("blit-from-screen",Image,dst);

    return(gfx_blit_from_screen(vm, dst, at, ul, lr, scale, degrees_rotation));
    break;
  }

  case 120: {
   VM_ARG("load-image",String,path);

    return(gfx_load_image(vm, path));
    break;
  }

  case 121: {
   VM_ARG("make-image",Fixnum,h);
   VM_ARG("make-image",Fixnum,w);

    return(gfx_make_image(vm, w, h));
    break;
  }

  case 122: {
   VM_ARG("image-width",Image,img);

     return(to(Fixnum,(image_width(img))));
    break;
  }

  case 123: {
   VM_ARG("image-height",Image,img);

     return(to(Fixnum,(image_height(img))));
    break;
  }

  case 124: {
   VM_ARG("char-code-at",Fixnum,idx);
   VM_ARG("char-code-at",String,str);

     return(to(Fixnum,(string_char_code_at(vm, str, idx))));
    break;
  }

  case 125: {
   VM_ARG("char-code",Char,ch);

     return(to(Fixnum,(character_to_s64(ch))));
    break;
  }

  case 126: {
   VM_ARG("code-char",Fixnum,code);

    return(to(Char, (char)code));
    break;
  }

  case 127: {
   VM_ARG("char-at",Fixnum,idx);
   VM_ARG("char-at",String,str);

     return(to(Char,(string_char_at(vm, str, idx))));
    break;
  }

  case 128: {
   VM_ARG("char-at-put",Char,ch);
   VM_ARG("char-at-put",Fixnum,idx);
   VM_ARG("char-at-put",String,str);

    return(string_set_char_at(vm, str, idx, ch));
    break;
  }

  case 129: {
   VM_ARG("char-<",Char,b);
   VM_ARG("char-<",Char,a);

     return(to(Bool,(character_lt(a,b))));
    break;
  }

  case 130: {
   VM_ARG("char->",Char,b);
   VM_ARG("char->",Char,a);

     return(to(Bool,(character_gt(a,b))));
    break;
  }

  case 131: {
   VM_ARG("char-width",Char,a);

     return(to(Fixnum,(character_byte_width(a))));
    break;
  }

  case 132: {
   VM_ARG("char-by-name",String,name);

    return(character_by_name(name));
    break;
  }

  case 133: {
   VM_ARG("char-name",Char,a);

    return(character_name(vm, a));
    break;
  }

  case 134: {
   VM_ARG("make-string",Char,ch);
   VM_ARG("make-string",Fixnum,len);

    return(make_filled_string(vm, len, ch));
    break;
  }

  case 135: {
   VM_ARG("string-byte-length",String,str);

     return(to(Fixnum,(string_byte_length(str))));
    break;
  }

  case 136: {
   VM_ARG("string-substr-bytes",Fixnum,b);
   VM_ARG("string-substr-bytes",Fixnum,a);
   VM_ARG("string-substr-bytes",String,str);

    return(string_substr_byte_range(vm, str, a, b));
    break;
  }

  case 137: {
   VM_ARG("string-equal",String,b);
   VM_ARG("string-equal",String,a);

     return(to(Bool,(string_equal(a,b))));
    break;
  }

  case 138: {
   VM_ARG("string-char-count",String,str);

     return(to(Fixnum,(string_char_count(str))));
    break;
  }

  case 139: {
   VM_ARG("string->char-array",String,str);

     return(to(Array,(array_from_string(vm, str))));
    break;
  }

  case 140: {
   VM_ARG("char-array->string",Array,arr);

     return(to(String,(string_from_array(vm, arr))));
    break;
  }

  case 141: {
   VM_ARG("set-stack-mark",any,m);

    return(vm_set_stack_mark(vm, m));
    break;
  }

  case 142: {
   VM_ARG("snapshot-to-stack-mark",any,v);
   VM_ARG("snapshot-to-stack-mark",any,m);

    return(vm_abort_to_mark(vm, m, v));
    break;
  }

  case 143: {
   VM_ARG("resume-stack-snapshot",any,arg);
   VM_ARG("resume-stack-snapshot",any,s);

    return(vm_resume_stack_snapshot(vm, s, arg));
    break;
  }

  case 144: {
   VM_ARG("return-from-mark",any,a);
   VM_ARG("return-from-mark",any,m);

    return(vm_return_from_mark(vm, m, a));
    break;
  }

  case 145: {
   VM_ARG("continuation-value",any,a);

    return(cont_get_value(a));
    break;
  }

  case 146: {
   VM_ARG("fork-thunk",any,a);
   VM_ARG("fork-thunk",any,priority);

    return(vm_schedule_closure(vm, a, priority, Nil));
    break;
  }

  case 147: {
   VM_ARG("make-semaphore",any,a);

    return(make_semaphore(vm, a));
    break;
  }

  case 148: {
   VM_ARG("signal-semaphore",any,a);

    return(signal_semaphore(a));
    break;
  }

  case 149: {

    return(vm->curr_thd->thread);
    break;
  }

  case 150: {

     return(to(Fixnum,(vm->threads->count)));
    break;
  }

  case 151: {

    return(list_all_threads(vm));
    break;
  }

  case 152: {
   VM_ARG("cas-vector",any,replace);
   VM_ARG("cas-vector",any,expect);
   VM_ARG("cas-vector",Fixnum,idx);
   VM_ARG("cas-vector",any,vec);

     return(to(Bool,(cas_vector(vec, idx, expect, replace))));
    break;
  }

  case 153: {
   VM_ARG("slurp",String,path);

    return(slurp(vm, path));
    break;
  }

  case 154: {
   VM_ARG("%file-output-stream-write-string",String,str);
   VM_ARG("%file-output-stream-write-string",any,s);

     return(to(Bool,(file_output_stream_write_string(s, str))));
    break;
  }

  case 155: {
   VM_ARG("%file-output-stream-write-char",Char,ch);
   VM_ARG("%file-output-stream-write-char",any,s);

     return(to(Bool,(file_output_stream_write_char(s, ch))));
    break;
  }

  case 156: {
   VM_ARG("thread-get-debug-info",any,a);

    return(thread_get_debug_info(vm, a));
    break;
  }

  case 157: {
   VM_ARG("save-snapshot",String,path);

    return(im_snapshot_to_path(vm, path));
    break;
  }

  case 158: {
   VM_ARG("save-snapshot-and-exit",String,path);

    return(im_snapshot_to_path_and_exit(vm, path));
    break;
  }

  case 159: {

     return(to(Fixnum,(current_time_ms())));
    break;
  }

  case 160: {
   VM_ARG("closure-source-location",any,a);

    return(get_source_location(a));
    break;
  }

  case 161: {

    return(update_display(vm));
    break;
  }

  case 162: {
   VM_ARG("blitq",Point,dd);
   VM_ARG("blitq",Point,dc);
   VM_ARG("blitq",Point,db);
   VM_ARG("blitq",Point,da);
   VM_ARG("blitq",Point,sd);
   VM_ARG("blitq",Point,sc);
   VM_ARG("blitq",Point,sb);
   VM_ARG("blitq",Point,sa);
   VM_ARG("blitq",Image,d);
   VM_ARG("blitq",Image,s);

    return(gfx_blit_image_into_quad(s,d, sa,sb,sc,sd,da,db,dc,dd));
    break;
  }

  case 163: {
   VM_ARG("exit",Fixnum,status);

    return((exit(status), Nil));
    break;
  }

  case 164: {
   VM_ARG("prefetch",any,it);

    return(prefetch(it));
    break;
  }

  case 165: {
   VM_ARG("bytecode->closure",any,it);

    return(make_closure(vm, it, Nil));
    break;
  }

  case 166: {
   VM_ARG("make-bytecode",PtrArray,literals);
   VM_ARG("make-bytecode",U16Array,code);
   VM_ARG("make-bytecode",any,name);
   VM_ARG("make-bytecode",Bool,varargs);

    return(create_bytecode(vm, varargs, name, code, literals));
    break;
  }

  case 167: {

     return(to(Fixnum,(vm->curr_thd->stack_depth)));
    break;
  }

  case 168: {

     return(to(Fixnum,((vm->curr_thd->stack_start - vm->curr_thd->stack) * 8)));
    break;
  }

  case 169: {

    return(reset_stats_reporting(vm));
    break;
  }

  case 170: {

    return(print_stats_reporting(vm));
    break;
  }

  }
  return Nil;
}
