/*

 g++ main.cpp -Werror -std=c++14 && ./a.out

(setq flycheck-clang-language-standard "c++14")

DONE: stack traces
DONE: move allocations into vm-managed heap
DONE: lists (incl printing)
DONE: lisp reader
DONE: expression compiler
DONE: lambda compiler
DONE: lambdas + closures compiler
DONE: def or define or something
TODO: varargs
DONE: booleans
DONE: characters
DONE: obj_size function // heap size of object (can be 0 for imms)
DONE: obj_ptrs function // walk Ptr s of obj (use a lambda)
DONE: make stack frames objects
TODO: read string
TODO: tests! (something like an assert)
TODO: bounds checking for heap allocation
DONE: memory usage report function
DONE: if compiler
DONE: simple let
DONE: let + closures
TODO: move symbol table into heap (make it an object)
DONE: vm_map_stack_refs(VM *vm, PtrFn)
DONE: vm_map_reachable_refs(VM *vm) function
DONE: maybe replace 'ffi' with primitives array (simpler for dump/restore?)
DONE: code-generate the prims table. elisp?
DONE: remove the 'ffi' stuff
DONE: scan_heap(mem *start, mem*end) fn (linear scan of objects on the heap)
DONE: garbage collection (cheney?)
TODO: initial audit of requred gc_protect calls
DONE: gc_protect for ptrs
DONE: gc-safe list manipulation functions
DONE: gc-safe allocators / make- functions
DONE: growable ptr array
TODO: growable identity map
DONE: gc-safe reader
DONE: gc-safe ByteCodeBuilder
TODO: move VariableInfo & CompilerEnv into gc heap
TODO: gc-safe compiler
TODO: automatic garbage collection (need to use gc_protect)
TODO: move stack memory into vm-managed heap
TODO: continuations / exceptions / signals
TODO: dump/restore image
TODO: write macroexpander in the language itself
TODO: write reader in the language itself
TODO: sdl integration
TODO: single floats
TODO: U32 Array etc
TODO: more prim instrs

maybe have a stack of compilers? can push/pop...
have each compiler pass output to previous one in the stack

how to represent U32 and U64?

*/

#include <cassert>
#include <cstring>
#include <iostream>
#include <string>
#include <map>
#include <unordered_map>
#include <vector>
#include <tuple>
#include <set>
#include <fstream>
#include <sstream>
#include <functional>
#include "./macro_support.h"

using namespace std;

typedef unsigned int uint;
typedef uint64_t u64;
typedef int64_t s64;
typedef uint8_t u8;
typedef int8_t s8;

enum ObjectType : u64 {
  ByteCode_ObjectType,
  ByteArray_ObjectType,
  U64Array_ObjectType,
  PtrArray_ObjectType,
  Standard_ObjectType,
  StackFrame_ObjectType,
  BrokenHeart
};

// so we can write the is(...) macro
#define Fixnum_Mask 0b0000
#define Object_Mask 0b0001
#define Char_Mask   0b0011
#define Bool_Mask   0b0100
#define PrimOp_Mask 0b0101

struct Ptr {
  u64 value;
};


bool ptr_eq(Ptr a, Ptr b) {
  return a.value == b.value;
}

struct Header {
  ObjectType object_type;
  // u64 flags;
};

struct Object {
  Header header;
};

// TODO: rename this function
// @safe
inline Ptr objToPtr(Object *ref) {
  Ptr p;
  p.value = ((u64) ref) |  0b1;
  return p;
}

struct VM;

void gc_protect_reference(VM *vm, Object **);
void gc_unprotect_reference(VM *vm, Object **);

#define gc_protect(it) do {                     \
    auto _ref = (Object **)&it;                 \
    gc_protect_reference(vm, _ref);             \
  } while(0)
#define gc_unprotect(it) do {                   \
    auto _ref = (Object **)&it;                 \
    gc_unprotect_reference(vm, _ref);           \
  } while(0)


struct U64ArrayObject : Object {
  u64 length;
  u64 data[];
};

typedef enum {
  Array,
  Closure
} PAOType;

struct PtrArrayObject : Object {
  PAOType pao_type;
  uint length;
  Ptr  data[];
};

struct ByteCodeObject : Object {
  U64ArrayObject *code;
  PtrArrayObject *literals;
};

struct StackFrameObject : Object {
  Ptr* prev_stack;
  StackFrameObject* prev_frame;
  ByteCodeObject* prev_fn;
  u64 prev_pc;
  Ptr closed_over;
  u64 argc;
  u64 pad_count;
  Ptr argv[];
};

struct Globals;

struct VM {
  Ptr *stack;
  void* heap_mem;
  void* heap_end;
  void* alt_heap_mem;
  void* alt_heap_end;
  u64 allocation_count;
  u64 heap_size_in_bytes;
  StackFrameObject *frame;
  u64 pc;
  ByteCodeObject *bc;
  const char* error;
  Globals *globals;
  u64 gc_count;
  u64 gc_threshold_in_bytes;
  set<Object **>  *gc_protected;
};

typedef Ptr (*PrimitiveFunction)(VM*);
extern PrimitiveFunction PrimLookupTable[];

inline void *align_pointer(void *mem) {
  static_assert(sizeof(u64) == sizeof(void *), "right pointer size");
  auto bytes = (u64)mem;
  auto rounded = ((bytes >> 4) << 4);
  auto bump = rounded == bytes ? rounded : rounded + 16;
  assert(((u64)bump & 0b1111) == 0);
  return (void *)bump;
}

inline void *align_pointer_with_offset(void* mem, u64 bytes) {
  auto adjusted = (u8*)mem + bytes;
  return align_pointer(adjusted);
}

inline bool pointer_is_aligned(void* mem) {
  return ((u64)mem & 0b1111) == 0;
}

void * vm_alloc(VM *vm, u64 bytes) {
  auto result = (void *)vm->heap_end;
  assert(pointer_is_aligned(result));
  vm->heap_end = align_pointer_with_offset(vm->heap_end, bytes);
  assert(pointer_is_aligned(vm->heap_end));
  vm->allocation_count++;
  return result;
}

void report_memory_usage(VM *vm) {
  double byte_count = (u64)vm->heap_end - (u64)vm->heap_mem;
  cout << "Memory usage, MB : " << (byte_count / (1024 * 1024)) << endl;
}

VM *CURRENT_DEBUG_VM;

/* -------------------------------------------------- */

typedef enum {
  String,
  Symbol
} BAOType;

struct ByteArrayObject : Object {
  BAOType bao_type;
  uint length;
  char data[];
};

struct RawPointerObject : Object {
  void *pointer;
};

struct StandardObject : Object { // really more of a structure object
  StandardObject *klass;
  u64 ivar_count;
  Ptr ivars[];
};

#define EXTRACT_PTR_MASK 0xFFFFFFFFFFFFFFF0
#define TAG_MASK 0b1111
#define TAG_BITS 4
#define FIXNUM_TAG 0b0000
#define OBJECT_TAG 0b0001
#define CHAR_TAG   0b0011
#define BOOL_TAG   0b0100
#define PRIM_TAG   0b0101
// #define UNUSED_TAG 0b0110
// #define UNUSED_TAG 0b0111
// #define UNUSED_TAG 0b1000
// #define UNUSED_TAG 0b1001
// #define UNUSED_TAG 0b1010
// #define UNUSED_TAG 0b1011
// #define UNUSED_TAG 0b1100
// #define UNUSED_TAG 0b1101
// #define UNUSED_TAG 0b1110
// #define UNUSED_TAG 0b1111
// maybe have a pair of s30 ints as an imm?
// what about a float?

#define TRUE  ((Ptr){0b10100})
#define FALSE ((Ptr){0b00100})

// not so sure about this...
#define NIL objToPtr((Object *)0)

// @safe
Object *asObject(Ptr self) {
  return (Object *)(self.value & EXTRACT_PTR_MASK);
}

// @safe
inline bool isNonNilObject(Ptr it) {
  return it.value != 1 && ((it.value & TAG_MASK) == OBJECT_TAG);
}

#define type_test_name(type) is_##type##_Impl

// IS it of this type
#define is(type, it) type_test_name(type)(it)
// AS this type (like a primitive cast, or conversion) -- returns CPP type
#define as(type, it) as##type(it)
// TO the Ptr representing this type
#define to(type, it) to##type(it)

#define type_test(type, var) inline bool type_test_name(type)(Ptr var)

#define prim_type(type) type_test(type, it){     \
    return (it.value & TAG_MASK) == type##_Mask; \
  }

// @safe
#define object_type(type)                                               \
  type_test(type, it) {                                                 \
    return (isNonNilObject(it) &&                                       \
            (as(Object, it))->header.object_type == type##_ObjectType); \
  };                                                                    \
  inline type##Object * as##type(Ptr it) {                              \
    assert(is(type, it));                                               \
    return (type##Object *)as(Object, it);                              \
  }


// @safe
type_test(any, it) { return true; }

// @safe
inline Ptr asany(Ptr it) { return it; }

prim_type(Fixnum)
prim_type(Char)
prim_type(Bool)
prim_type(PrimOp)
type_test(Object, it) { return isNonNilObject(it); }
object_type(ByteCode)
object_type(ByteArray)
object_type(U64Array)
object_type(PtrArray)
object_type(Standard)
object_type(StackFrame)

#undef prim_type
#undef object_type

#define VM_ARG(type, name)                             \
  Ptr _##name = vm_pop(vm);                            \
  if (!is(type, _##name)) {                            \
    vm->error = " argument " #name " is not a " #type; \
    return NIL;                                        \
  }                                                    \
  auto name = as(type, _##name);

/* ---------------------------------------- */
// Ptr protection

struct GCPtr {
  Ptr ptr;
  Object *object;
  bool is_object;
  VM *vm;
};

#define protect_ptr(var, it)                    \
  GCPtr var;                                    \
  wrap_ptr(vm, &var, it)


inline void wrap_ptr(VM *vm, GCPtr* var, Ptr it)  {
  var->ptr = it;
  if (isNonNilObject(it)) {
    var->is_object = true;
    var->object = as(Object, it);
    var->vm = vm;
    gc_protect(var->object);
  } else {
    var->is_object = false;
  }
}

inline Ptr unwrap_ptr(GCPtr *it) {
  if (it->is_object) {
    auto vm = it->vm;
    gc_unprotect(it->object);
    it->ptr = objToPtr(it->object);
  }
  return it->ptr;
}

#define prot_ptr(it)   protect_ptr(safe__##it, it)
#define unprot_ptr(it) it = unwrap_ptr(&safe__##it)

#define protect_ptr_vector(var, count, vector)  \
  GCPtr var[count];                             \
  {                                             \
    u64 ___count = count;                       \
    for (u64 i = 0; i < ___count; i++) {        \
      wrap_ptr(vm, var + i, vector[i]);         \
    }                                           \
  }

#define std_vector_mem(vector) &(*vector->begin())

#define __prot_ptr(x) prot_ptr(x);
#define prot_ptrs(...) MAP(__prot_ptr, __VA_ARGS__)
#define __unprot_ptr(x) unprot_ptr(x);
#define unprot_ptrs(...) MAP(__unprot_ptr, __VA_ARGS__)

// would be nice to have a version with expr in trailing brackets.
#define call_with_ptrs(ptrs, expr)              \
  {                                             \
    prot_ptrs ptrs                              \
    expr;                                       \
    unprot_ptrs ptrs                            \
  }


/* ---------------------------------------- */

// @safe
inline s64 toS64(Ptr self) {
  return ((s64)self.value) >> TAG_BITS;
}

// @safe
inline Ptr s64ToPtr(s64 value) {
  // TODO: overflow check
  Ptr p;
  p.value = value << TAG_BITS;
  return p;
}

// @safe
inline Ptr toFixnum(s64 value) {
  return s64ToPtr(value);
}

// @safe
inline s64 asFixnum(Ptr self) {
  return ((s64)self.value) >> TAG_BITS;
}

// @safe
inline Ptr toPrimOp(u64 raw_value){
  return (Ptr){raw_value};
}

// TODO: convert this to type-test
// @safe
inline bool isNil(Ptr self) {
  return self.value == OBJECT_TAG;
}

// @safe
inline bool asBool(Ptr self) {
  return (self.value >> TAG_BITS) ? true : false;
}

// @safe
inline Ptr toBool(bool tf) {
  return tf ? TRUE : FALSE;
}

// @safe
char asChar(Ptr self) {
  return self.value >> TAG_BITS;
}

// @safe
Ptr charToPtr(char ch) {
  auto val = ((u64)ch << TAG_BITS)|CHAR_TAG;
  return (Ptr){val};
}

// @safe
U64ArrayObject *alloc_u64ao(VM *vm, uint len) {
  auto byte_count = sizeof(U64ArrayObject) + (len * sizeof(u64));
  U64ArrayObject* obj = (U64ArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = U64Array_ObjectType;
  obj->length = len;
  return obj;
}

// @safe
ByteCodeObject *alloc_bytecode(VM *vm) {
  auto byte_count = sizeof(ByteCodeObject);
  ByteCodeObject *obj = (ByteCodeObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteCode_ObjectType;
  return obj;
}

// @safe
ByteCodeObject *toBytecode(Ptr it) {
  assert(is(ByteCode, it));
  return (ByteCodeObject *)as(Object, it);
}

// @safe
ByteArrayObject *alloc_bao(VM *vm, BAOType ty, uint len) {
  auto byte_count = sizeof(ByteArrayObject) + len;
  ByteArrayObject* obj = (ByteArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteArray_ObjectType;
  obj->bao_type = ty;
  obj->length = len;
  return obj;
}

// @safe
type_test(Symbol, it){
  if (!is(ByteArray, it)) return false;
  auto bao = as(ByteArray, it);
  return bao->bao_type == Symbol;
}

// @safe
inline ByteArrayObject * asSymbol(Ptr it) {
  return as(ByteArray, it);
}

// @safe
PtrArrayObject *alloc_pao(VM *vm, PAOType ty, uint len) {
  auto byte_count = sizeof(PtrArrayObject) + (len * sizeof(Ptr));
  PtrArrayObject* obj = (PtrArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = PtrArray_ObjectType;
  obj->pao_type = ty;
  obj->length = len;
  return obj;
}

// FIXME: should this should be an AS not a TO
// @safe
PtrArrayObject *toPtrArrayObject(Ptr it) {
  assert(is(PtrArray,it));
  return (PtrArrayObject *)as(Object, it);
}

// @safe
type_test(Array, it) {
  return is(PtrArray, it) && (toPtrArrayObject(it))->pao_type == Array;
}

// @safe
StandardObject *alloc_standard_object(VM *vm, StandardObject *klass, u64 ivar_count) {
  gc_protect(klass);
  auto byte_count = (sizeof(StandardObject)) + ivar_count * (sizeof(Ptr));
  auto result = (StandardObject *)vm_alloc(vm, byte_count);
  result->header.object_type = Standard_ObjectType;
  result->klass = klass;
  result->ivar_count = ivar_count;
  gc_unprotect(klass);
  return result;
}

// @safe
Ptr standard_object_get_ivar(StandardObject *object, u64 idx) {
  assert(idx < object->ivar_count);
  return object->ivars[idx];
}

// @safe
Ptr standard_object_set_ivar(StandardObject *object, u64 idx, Ptr value) {
  assert(idx < object->ivar_count);
  return object->ivars[idx] = value;
}

// @safe
Ptr make_bytecode(VM *vm, u64 code_len) {
  auto bc = alloc_bytecode(vm);
  gc_protect(bc);
  auto code = alloc_u64ao(vm, code_len);
  gc_unprotect(bc);
  bc->code = code;
  return objToPtr(bc);
}

// @safe
Ptr make_string(VM *vm, const char* str) {
  ByteArrayObject *obj = alloc_bao(vm, String, strlen(str));
  const char *from = str;
  char *to = &(obj->data[0]);
  while(*from != 0) {
    *to = *from;
    to++; from++;
  }
  return objToPtr(obj);
}

// @safe
Ptr make_symbol(VM *vm, const char* str, u64 len) {
  ByteArrayObject *obj = alloc_bao(vm, Symbol, len);
  const char *from = str;
  char *to = &(obj->data[0]);
  while(len--) {
    *to = *from;
    to++; from++;
  }
  return objToPtr(obj);
}

// @safe
Ptr make_symbol(VM *vm, const char* str) {
  return make_symbol(vm, str, strlen(str));
}

// @safe
Ptr make_number(s64 value) { return s64ToPtr(value); }

// @safe
Ptr make_array(VM *vm, u64 len, Ptr objs[]) {
  protect_ptr_vector(safe_objs, len, objs);
  auto array = alloc_pao(vm, Array, len);
  for (u64 i = 0; i < len; i++) {
    array->data[i] = unwrap_ptr(safe_objs + i);
  }
  return objToPtr(array);
}

// @safe
Ptr make_zf_array(VM *vm, u64 len) {
  auto array = alloc_pao(vm, Array, len);
  auto zero = to(Fixnum, 0);
  for (u64 i = 0; i < len; i++) {
    array->data[i] = zero;
  }
  return objToPtr(array);
}

// @safe
Ptr array_get(Ptr array, u64 index) {
  auto a = toPtrArrayObject(array);
  assert(index < a->length);
  return a->data[index];
}

// @safe
void array_set(Ptr array, u64 index, Ptr value) {
  auto a = toPtrArrayObject(array);
  assert(index < a->length);
  a->data[index] = value;
}

// @safe
u64 array_capacity(Ptr array) {
  auto a = toPtrArrayObject(array);
  return a->length;
}

// using a 2 element array to hold count and buffer for now
// @safe
Ptr make_extensible_array(VM *vm) {
  auto used = to(Fixnum, 0);
  Ptr buffer = make_zf_array(vm, 4);
  return make_array(vm, 2, (Ptr[]){used, buffer});
}

// @safe
u64 extensible_array_capacity(Ptr array) {
  return as(PtrArray, array_get(array, 1))->length;
}

// @safe
u64 extensible_array_used(Ptr array) {
  return as(Fixnum, array_get(array, 0));
}

// @safe
void extensible_array_push(VM *vm, Ptr array, Ptr item) {
  auto used = extensible_array_used(array);
  auto cap = extensible_array_capacity(array);
  if (used + 1 >= cap) {
    auto new_cap = cap * 2;
    auto old_arr = array_get(array, 1);
    prot_ptrs(array, old_arr);
    auto new_arr = make_zf_array(vm, new_cap);
    unprot_ptrs(array, old_arr);
    for (u64 i = 0; i < cap; i++) {
      array_set(new_arr, i, array_get(old_arr, i));
    }
    array_set(array, 1, new_arr);
  }
  array_set(array_get(array, 1), used, item);
  array_set(array, 0, to(Fixnum, used + 1));
}

// @safe
Ptr *extensible_array_memory(Ptr array) {
  auto buff = as(PtrArray, array_get(array, 1));
  return buff->data;
}


// @safe
Ptr make_closure(VM *vm, Ptr code, Ptr env) {
  assert(is(ByteCode, code));
  assert(isNil(env) || is(PtrArray, env));
  prot_ptrs(code, env);
  auto it = alloc_pao(vm, Closure, 2);
  auto c = objToPtr(it);
  unprot_ptrs(code, env);
  array_set(c, 0, code);
  array_set(c, 1, env);
  return c;
}

// @safe
type_test(Closure, it) {
  return is(PtrArray, it) && (toPtrArrayObject(it))->pao_type == Closure;
}

// @safe
ByteCodeObject *closure_code(Ptr closure) {
  return as(ByteCode, array_get(closure, 0));
}

// @safe
Ptr closure_env(Ptr closure) {
  return array_get(closure, 1);
}

/* ---------------------------------------- */
// @safe

// size of object in bytes
// note that obj_size of stack frame does not take into account temporaries.

u64 obj_size(U64ArrayObject *it)  { return sizeof(U64ArrayObject) + it->length * 8;    }
u64 obj_size(ByteCodeObject *it)  { return sizeof(ByteCodeObject) + 0;                 }
u64 obj_size(ByteArrayObject *it) { return sizeof(ByteArrayObject) + it->length;       }
u64 obj_size(PtrArrayObject *it)  { return sizeof(PtrArrayObject) + it->length * 8;    }
u64 obj_size(StandardObject *it)  { return sizeof(StandardObject) + it->ivar_count * 8;}
// TODO: include padding
u64 obj_size(StackFrameObject*it) { return sizeof(StackFrameObject) + it->argc * 8;    }

auto size_of(Ptr it) {
  if (isNil(it) || !is(Object, it)) return (u64)0;
  if (is(U64Array, it))   return obj_size(as(U64Array, it));
  if (is(ByteCode, it))   return obj_size(as(ByteCode, it));
  if (is(ByteArray, it))  return obj_size(as(ByteArray, it));
  if (is(PtrArray, it))   return obj_size(as(PtrArray, it));
  if (is(Standard, it))   return obj_size(as(Standard, it));
  if (is(StackFrame, it)) return obj_size(as(StackFrame, it));
  cout << " unknown object type in object_size " << endl;
  assert(false);
}

/* ---------------------------------------- */
// walk object references
// NB. not gc safe

typedef std::function<void(Ptr)> PtrFn;

// NB: cannot track items currently on the stack from this function
//     unless we do a full stack scan.
void obj_refs(VM *vm, StackFrameObject *it, PtrFn fn) {
  for (u64 i = 0; i < it->argc; i++) {
    fn(it->argv[it->pad_count + i]);
  }
  fn(it->closed_over);
  if (it->prev_frame) fn(objToPtr(it->prev_frame));
}

void obj_refs(VM *vm, U64ArrayObject *it, PtrFn fn) { return; }
void obj_refs(VM *vm, ByteCodeObject *it, PtrFn fn) {
  fn(objToPtr(it->code));
  fn(objToPtr(it->literals));
}
void obj_refs(VM *vm, ByteArrayObject *it, PtrFn fn) { return; }
void obj_refs(VM *vm, PtrArrayObject *it, PtrFn fn) {
  for (u64 i = 0; i < it->length; i++) {
    fn(it->data[i]);
  }
}

void obj_refs(VM *vm, StandardObject *it, PtrFn fn) {
  fn(objToPtr(it->klass));
  for (u64 i = 0; i < it->ivar_count; i++) {
    fn(it->ivars[i]);
  }
}

void map_refs(VM *vm, Ptr it, PtrFn fn) {
  if (isNil(it) || !is(Object, it)) return;
  if (is(U64Array, it))   return obj_refs(vm, as(U64Array, it),   fn);
  if (is(ByteCode, it))   return obj_refs(vm, as(ByteCode, it),   fn);
  if (is(ByteArray, it))  return obj_refs(vm, as(ByteArray, it),  fn);
  if (is(PtrArray, it))   return obj_refs(vm, as(PtrArray, it),   fn);
  if (is(Standard, it))   return obj_refs(vm, as(Standard, it),   fn);
  if (is(StackFrame, it)) return obj_refs(vm, as(StackFrame, it), fn);
  cout << " unknown object type in map_refs" << endl;
  assert(false);
}

/* ---------------------------------------- */
// @safe @noalloc

enum {
  BaseClassName = 0,
  BaseClassIvarCount = 1,
  BaseClassDebugPrint = 2,
  BaseClassEnd = 3
};

typedef void(*DebugPrintFunction)(std::ostream &os, Ptr p);

#define DEBUG_PRINT_MAX 255
DebugPrintFunction DebugPrintTable[DEBUG_PRINT_MAX] = {0};
enum BuiltInDebugPrintIndex : u64 {
  DebugPrint_None,
  DebugPrint_Cons,
  DebugPrint_End
};

std::ostream &operator<<(std::ostream &os, Ptr p);

std::ostream &operator<<(std::ostream &os, Object *obj) {
  auto otype = obj->header.object_type;
  switch (otype) {
  case ByteArray_ObjectType: {
    const ByteArrayObject *vobj = (const ByteArrayObject*)(obj);
    switch(vobj->bao_type) {
    case String:
      os << "\"";
      for (uint i = 0; i < vobj->length; i++) {
        os << vobj->data[i];
      }
      os << "\"";
      return os;
    case Symbol:
      for (uint i = 0; i < vobj->length; i++) {
        os << vobj->data[i];
      }
      return os;
    }
  }
  case PtrArray_ObjectType: {
    const PtrArrayObject *vobj = (const PtrArrayObject*)(obj);
    os << "[";
    if (vobj->length > 0) {
      cout << vobj->data[0];
    }
    for (uint i = 1; i < vobj->length; i++) {
      cout << " " << vobj->data[i];
    }
    os << "]";
    return os;
  }
  case Standard_ObjectType: {
    auto sobj = (StandardObject *)obj;
    auto name = standard_object_get_ivar(sobj->klass, BaseClassName);
    auto pr_obj = standard_object_get_ivar(sobj->klass, BaseClassDebugPrint);
    if (is(Fixnum, pr_obj)) {
      auto idx = as(Fixnum, pr_obj);
      if (idx > 0 && idx < DEBUG_PRINT_MAX && DebugPrintTable[idx]) {
        auto fn = DebugPrintTable[idx];
        fn(os, objToPtr(obj));
        return os;
      }
    }
    cout << "#<A " << as(Object, name) << " " << (void*)obj << ">";
    return os;
  }
  case ByteCode_ObjectType: {
    cout << "#<ByteCode " << (void*)obj << ">";
    return os;
  }
  case U64Array_ObjectType: {
    auto len = ((const U64ArrayObject *)obj)->length;
    return os << "#<U64Array (" << len << ") "<< (void*)obj << ">";
  }
  case StackFrame_ObjectType: {
    auto len = ((const StackFrameObject *)obj)->argc;
    return os << "#<StackFrame (argc = " << len << ") "<< (void*)obj << ">";
  }
  case BrokenHeart: {
    return os << "#<GC Broken Heart>";
  }
  }
  return os << "don't know how to print object: " << otype << (void*)obj << endl;
}

std::ostream &operator<<(std::ostream &os, Ptr p) {
  if (isNil(p)) {
    return os << "nil";
  } else if (is(Object, p)) {
    return os << (as(Object, p));
  } else if (is(Fixnum, p)) {
    return os << (as(Fixnum, p));
  } else if (is(Bool, p)) {
    return os << (as(Bool, p) ? "#t" : "#f");
  } else if (is(Char, p)) {
    return os << "#\\" << as(Char, p);
  } else {
    return os << "don't know how to print ptr: " << (void *)p.value;
  }
}

// @safe @noalloc
void vm_dump_args(VM *vm) {
  auto f = vm->frame;
  auto c = f->argc;
  cout << " dumping args:" << endl;
  while(c--) {
    cout << "  argument: " << f->argv[f->pad_count + c] << endl;
  }
}

// @safe @noalloc
void _debug_walk(VM *vm, Ptr it, set<u64>*seen) {
  map_refs(vm, it, [&](Ptr p){
      if (seen->find(p.value) != seen->end()) return;
      seen->insert(p.value);
      cout << "    " << p << endl;
      _debug_walk(vm, p, seen);
    });
}

// @safe @noalloc
void debug_walk(VM *vm, Ptr it) {
  set<u64> seen;
  cout << "DEBUG WALK::" << endl;
  // TODO: print out `it` as well :P
  _debug_walk(vm, it, &seen);
  cout << "========================================" << endl << endl;
}

// @unsafe
auto vm_map_stack_refs(VM *vm, PtrFn fn) {
  StackFrameObject *fr = vm->frame;
  Ptr *stack = vm->stack;
  while (fr) {
    fn(fr->closed_over);
    auto pad = fr->pad_count;
    for (u64 i = 1; i <= fr->argc; i++) {
      auto arg = fr->argv[pad + (fr->argc - i)];
      fn(arg);
    }
    auto on_stack = (Ptr*)(void *)fr;
    while (on_stack > stack) {
      on_stack--;
      fn(*on_stack);
    }
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
  }
}

// @safe @noalloc
auto vm_print_stack_trace(VM *vm) {
  StackFrameObject *fr = vm->frame;
  Ptr *stack = vm->stack;
  cout << "PRINTING STACKTRACE:" << endl;
  while (fr) {
    cout << " FRAME: " << fr << endl;
    cout << "   closure: " << fr->closed_over << endl;
    cout << "   aligned by: " << fr->pad_count << endl;
    auto pad = fr->pad_count;
    for (u64 i = 1; i <= fr->argc; i++) {
      cout << "   arg " << (i - 1) << " = " << fr->argv[pad + (fr->argc - i)] << endl;
    }
    auto h =  (u64)fr - (u64)stack;
    cout << "   stack height (bytes): " << h << endl;
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
  }
  return NIL;
}

// @safe @noalloc
auto vm_print_debug_stack_trace(VM *vm) {
  StackFrameObject *fr = vm->frame;
  debug_walk(vm, objToPtr(fr));
  cout << "----------------------------------------" << endl;
  vm_map_stack_refs(vm, [&](Ptr it){
      cout << "    " << it << endl;
    });
  cout << "----------------------------------------" << endl;
  return NIL;
}

/* ---------------------------------------- */

// @safe
StandardObject *make_standard_object(VM *vm, StandardObject *klass, Ptr*ivars) {
  auto ivar_count_object = standard_object_get_ivar(klass, BaseClassIvarCount);
  assert(is(Fixnum, ivar_count_object));
  auto ivar_count = toS64(ivar_count_object);

  protect_ptr_vector(safe_ivars, ivar_count, ivars);

  auto result = alloc_standard_object(vm, klass, ivar_count);

  for (auto i = 0; i < ivar_count; i++) {
    standard_object_set_ivar(result, i, unwrap_ptr(safe_ivars + i));
  }

  return result;
}

/* ---------------------------------------- */

struct Globals {
  StandardObject *Base, *Cons, *Fixnum, *Symbol;
  unordered_map<string, Ptr> *symtab;
  Ptr env;
};

// @unsafe
auto vm_map_reachable_refs(VM *vm, PtrFn fn) {
  set<u64> seen;
  PtrFn recurse = [&](Ptr it) {
    if (!isNonNilObject(it)) return;
    if (seen.find(it.value) != seen.end()) return;
    seen.insert(it.value);
    fn(it);
    map_refs(vm, it, recurse);
  };
  vm_map_stack_refs(vm, recurse);
  recurse(vm->globals->env);
  for (auto pair : *vm->globals->symtab) {
    recurse(pair.second);
  }
  recurse(objToPtr(vm->globals->Base));
  recurse(objToPtr(vm->globals->Cons));
  recurse(objToPtr(vm->globals->Fixnum));
  recurse(objToPtr(vm->globals->Symbol));
}

// @safe @noalloc
void vm_count_reachable_refs(VM *vm) {
  u64 count = 0;
  vm_map_reachable_refs(vm, [&](Ptr it){
      count++;
    });
  cout << "  " << count << "  reachable objects." << endl;
}

// @unsafe
void scan_heap(void *start, void*end, PtrFn fn) {
  while(start < end) {
    assert(pointer_is_aligned(start));
    auto it = objToPtr((Object *)start);
    auto offset = size_of(it);
    if (offset == 0) {
      cout << " error while scanning heap." << endl;
      return;
    }
    fn(it);
    start = align_pointer_with_offset(start, offset);
  }
}

// @safe @noalloc
void vm_count_objects_on_heap(VM *vm) {
  u64 count = 0;
  scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it){
      count++;
    });
  cout << " counted " << count << " objects on heap. " << endl;
  cout << " allocation count is : " << vm->allocation_count << endl;
}

/* ---------------------------------------- */
/*             -- gc support --             */

void gc_prepare_vm(VM *vm) {
  auto start = vm->heap_mem;
  auto end = vm->heap_end;
  vm->heap_mem = vm->alt_heap_mem;
  vm->heap_end = vm->alt_heap_mem;
  vm->alt_heap_mem = start;
  vm->alt_heap_end = end;
}

bool gc_is_broken_heart(Object *obj) {
  return obj->header.object_type == BrokenHeart;
}

void gc_break_heart(Object *obj, Object *forwarding_address) {
  assert(!gc_is_broken_heart(obj));
  obj->header.object_type = BrokenHeart;
  auto ptr = (u64 *)obj;
  ptr++;
  *ptr = (u64)forwarding_address;
}

Object *gc_forwarding_address(Object *obj) {
  assert(gc_is_broken_heart(obj));
  auto ptr = (u64 *)obj;
  ptr++;
  return (Object *)*ptr;
}

auto gc_copy_object(VM *vm, Ptr it) {
  auto count = size_of(it);
  if (!count) return it;
  auto bytes = vm_alloc(vm, count);
  auto from  = as(Object, it);
  auto to    = (Object *)bytes;
  memcpy(to, from, count);
  return objToPtr(to);
}

Ptr gc_move_object(VM *vm, Object *obj) {
  assert(!gc_is_broken_heart(obj));
  auto ptr = objToPtr(obj);
  assert(size_of(ptr) > 0);
  auto new_ptr = gc_copy_object(vm, ptr);
  Object *addr = as(Object, new_ptr);
  gc_break_heart(obj, addr);
  return new_ptr;
}

void gc_update_ptr(VM *vm, Ptr *p) {
  auto ptr = *p;
  if (!isNonNilObject(ptr)) return;
  auto obj = as(Object, ptr);
  if (!gc_is_broken_heart(obj)) {
    gc_move_object(vm, obj);
  }
  auto new_addr = gc_forwarding_address(obj);
  auto new_ptr = objToPtr(new_addr);
  *p = new_ptr;
}

// gc update is called while scanning the heap of objects that have
// already been copied over.
// StackFrameObject is handled specially.
void gc_update(VM *vm, ByteCodeObject* it) {
  {
    Ptr p = objToPtr(it->code);
    gc_update_ptr(vm, &p);
    it->code = as(U64Array, p);
  }
  {
    Ptr p = objToPtr(it->literals);
    gc_update_ptr(vm, &p);
    it->literals = as(PtrArray, p);
  }
}

void gc_update(VM *vm, PtrArrayObject* it) {
  for (u64 i = 0; i < it->length; i++) {
    gc_update_ptr(vm, it->data + i);
  }
}

void gc_update(VM *vm, StandardObject* it) {
  {
    Ptr p = objToPtr(it->klass);
    gc_update_ptr(vm, &p);
    it->klass = as(Standard, p);
  }
  for (u64 i = 0; i < it->ivar_count; i++) {
    gc_update_ptr(vm, it->ivars + i);
  }
}

void gc_update_copied_object(VM *vm, Ptr it) {
  assert(is(Object, it));
  if (is(ByteCode, it)) return gc_update(vm, as(ByteCode, it));
  if (is(PtrArray, it)) return gc_update(vm, as(PtrArray, it));
  if (is(Standard, it)) return gc_update(vm, as(Standard, it));
}

void gc_update_stack(VM *vm) {
  StackFrameObject *fr = vm->frame;
  Ptr *stack = vm->stack;
  while (fr) {
    gc_update_ptr(vm, &fr->closed_over);
    auto pad = fr->pad_count;
    for (u64 i = 1; i <= fr->argc; i++) {
      auto arg = fr->argv[pad + (fr->argc - i)];
      gc_update_ptr(vm, &arg);
    }
    auto on_stack = (Ptr*)(void *)fr;
    while (on_stack > stack) {
      on_stack--;
      gc_update_ptr(vm, on_stack);
    }
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
  }
}

void gc_update_base_class(VM *vm, StandardObject **it) {
  Ptr p = objToPtr(*it);
  gc_update_ptr(vm, &p);
  *it = as(Standard, p);
}

void gc_update_globals(VM *vm) {
  gc_update_ptr(vm, &vm->globals->env);
  gc_update_base_class(vm, &vm->globals->Base);
  gc_update_base_class(vm, &vm->globals->Cons);
  gc_update_base_class(vm, &vm->globals->Fixnum);
  gc_update_base_class(vm, &vm->globals->Symbol);
}

void gc_copy_symtab(VM *vm) {
  auto old_symtab = vm->globals->symtab;
  auto new_symtab = new unordered_map<string, Ptr>;
  for (auto pair : *old_symtab) {
    gc_update_ptr(vm, &pair.second);
    new_symtab->insert(pair);
  }
  vm->globals->symtab = new_symtab;
  delete old_symtab;
}

void gc_update_protected_references(VM *vm) {
  for (Object **ref : *vm->gc_protected) {
    auto obj = *ref;
    auto ptr = objToPtr(obj);
    gc_update_ptr(vm, &ptr);
    auto new_obj = as(Object, ptr);
    *ref = new_obj;
  }
}

void gc_protect_reference(VM *vm, Object **ref){
  vm->gc_protected->insert(ref);
}
void gc_unprotect_reference(VM *vm, Object **ref){
  vm->gc_protected->erase(ref);
}

void gc(VM *vm) {
  // prepare_vm
  gc_prepare_vm(vm);
  // copy symtab
  gc_copy_symtab(vm);

  // set start ptr (everything on heap should be symbols right now)
  auto start = vm->heap_end;
  // update stack. (loop through stack and gc_copy_object on-stack and args etc.)
  gc_update_stack(vm);
  // update global refs
  gc_update_globals(vm);
  // update protected refs
  gc_update_protected_references(vm);

  auto end = vm->heap_end;
  // repeatedly update new allocations on the heap
  while (start < end) {
    scan_heap(start, end, [&](Ptr it){
        gc_update_copied_object(vm, it);
      });
    start = end;
    end = vm->heap_end;
  }
  // TODO: memset old heap for safety.
}


/* ---------------------------------------- */

// @safe
auto make_base_class(VM *vm, const char* name, u64 ivar_count) {
  auto defaultPrint = NIL;
  Ptr slots[] = {make_string(vm,name), make_number(ivar_count), defaultPrint};
  return make_standard_object(vm, vm->globals->Base, slots);
}

/* ---------------------------------------- */

// @safe
bool consp(VM *vm, Ptr p) {
  if (!is(Standard, p)) return false;
  auto obj = as(Standard, p);
  auto res = (void *)obj->klass == (void *)vm->globals->Cons;
  return res;
}

// @safe
Ptr car(VM *vm, Ptr p) {
  if (isNil(p)) return NIL;
  assert(consp(vm, p));
  return standard_object_get_ivar(as(Standard, p), 0);
}

// @safe
void set_car(VM *vm, Ptr cons, Ptr value) {
  assert(consp(vm, cons));
  standard_object_set_ivar(as(Standard, cons), 0, value);
}

// @safe
Ptr cdr(VM *vm, Ptr p) {
  if (isNil(p)) return NIL;
  assert(consp(vm, p));
  return standard_object_get_ivar(as(Standard, p), 1);
}

// @safe
void set_cdr(VM *vm, Ptr cons, Ptr value) {
  assert(consp(vm, cons));
  standard_object_set_ivar(as(Standard, cons), 1, value);
}

// @safe
Ptr nth_or_nil(VM *vm, Ptr p, u64 idx) {
  assert(idx >= 0);
  if (isNil(p)) return NIL;
  if (idx == 0) return car(vm, p);
  else return nth_or_nil(vm, cdr(vm, p), idx - 1);
}

// @safe
Ptr cons(VM *vm, Ptr car, Ptr cdr) {
  auto obj = make_standard_object(vm, vm->globals->Cons, (Ptr[]){car, cdr});
  auto res = objToPtr(obj);
  assert(consp(vm, res));
  return res;
}

// @safe
Ptr assoc(VM *vm, Ptr item, Ptr alist) {
  while (!isNil(alist)) {
    auto pair = car(vm, alist);
    if (ptr_eq(car(vm, pair), item)) return pair;
    alist = cdr(vm, alist);
  }
  return NIL;
}

// @safe
void set_assoc(VM *vm, Ptr *alistref, Ptr item, Ptr value) {
  auto existing = assoc(vm, item, *alistref);
  if (isNil(existing)) {
    auto pair = cons(vm, item, value);
    auto newalist = cons(vm, pair, *alistref);
    *alistref = newalist;
  } else {
    set_cdr(vm, existing, value);
  }
}

// @safe
Ptr make_list(VM *vm, u64 len, Ptr* ptrs) {
  if (len == 0) return NIL;
  // TODO: iterative solution
  return cons(vm, *ptrs, make_list(vm, len - 1, ptrs + 1));
}

// @safe
void debug_print_list(ostream &os, Ptr p) {
  VM *vm = CURRENT_DEBUG_VM;
  os << "(";
  auto a = car(vm, p);
  os << a;
  p = cdr(vm, p);
  while (!isNil(p)) {
    if (consp(vm, p)) {
      os << " " << car(vm, p);
      p = cdr(vm, p);
    } else {
      os << " . " << p;
      break;
    }
  }
  os << ")";
}

// @safe
void do_list(VM *vm, Ptr it, PtrFn cb) {
  while (!isNil(it)) {
    prot_ptr(it);
    cb(car(vm, it));
    unprot_ptr(it);
    it = cdr(vm, it);
  }
}

// @safe
u64 list_length(VM *vm, Ptr it) {
  u64 count = 0;
  do_list(vm, it, [&](Ptr p){ count++; });
  return count;
}

/* ---------------------------------------- */

// @unsafe
void initialize_classes(VM *vm)
{
  auto Base = alloc_standard_object(vm, 0, BaseClassEnd);
  Base->klass = Base;
  standard_object_set_ivar(Base, BaseClassName, make_string(vm, "Base"));
  standard_object_set_ivar(Base, BaseClassIvarCount, make_number(BaseClassEnd));
  auto g = vm->globals;
  g->Base = Base;
  g->Cons = make_base_class(vm, "Cons", 2);
  DebugPrintTable[DebugPrint_Cons] = &debug_print_list;
  standard_object_set_ivar(g->Cons, BaseClassDebugPrint,
                           to(Fixnum, DebugPrint_Cons));
  g->Fixnum = make_base_class(vm, "Fixnum", 0);
  g->Symbol = make_base_class(vm, "Symbol", 0);
}

// @safe
Ptr intern(VM *vm, const char* cstr, int len) {
  string name = string(cstr, len);
  auto tab = vm->globals->symtab;
  if (tab->find(name) == tab->end()) {
    auto sym = make_symbol(vm, cstr, len);
    // NB: symtab may have changed if make_symbol triggered a gc
    vm->globals->symtab->insert(make_pair(name, sym));
  }
  auto res = tab->find(name)->second;
  return res;
}

// @safe
Ptr intern(VM *vm, string name) {
  auto str = name.c_str();
  return intern(vm, str, strlen(str));
}

// @safe
Ptr set_global(VM *vm, Ptr sym, Ptr value) {
  assert(is(Symbol, sym));
  set_assoc(vm, &vm->globals->env, sym, value);
  return sym;
}

// @safe
Ptr set_global(VM *vm, const char* name, Ptr value) {
  return set_global(vm, intern(vm, name), value);
}

// @safe
Ptr get_global(VM *vm,  const char*name) {
  auto pair = assoc(vm, intern(vm, name), vm->globals->env);
  if (isNil(pair)) return pair;
  return cdr(vm, pair);
}

/* -------------------------------------------------- */
// @safe

#include "./chars.cpp"

auto is_digitchar(char ch) {
  return ch >= '0' && ch <= '9';
}
auto is_symchar(char ch) {
  u8 idx = ch;
  return character_table[idx] & character_sym_start;
}
auto is_symbodychar(char ch) {
  u8 idx = ch;
  u8 flags = character_sym_start | character_digit | character_sym_body;
  return character_table[idx] & flags;
}
auto is_parens(char ch) {
  u8 idx = ch;
  return character_table[idx] & character_bracket;
}
auto is_q(char ch) {
  return ch == '\'';
}
auto is_wschar(char ch) {
  u8 idx = ch;
  return !(character_table[idx]);
}

auto is_nlchar(char ch) {
  return ch == 10 || ch == 13;
}

/* -------------------------------------------------- */

// @safe
auto quote_form(VM *vm, Ptr it) {
  auto q = intern(vm, "quote");
  prot_ptr(q);
  auto res = cons(vm, it, NIL);
  unprot_ptr(q);
  return cons(vm, q, res);
}

// @safe
void eat_ws(const char **remaining, const char *end) {
  auto input = *remaining;
  while (input < end) {
    while(input < end && is_wschar(*input)) input++;
    if (*input == ';') { // eat comments
      while (input < end && !is_nlchar(*input)) input++;
    } else {
      break;
    }
  }
  *remaining = input;
}

// TODO: need to make all branches of read GC safe.
Ptr read(VM *vm, const char **remaining, const char *end, Ptr done);

// @safe
auto read_delimited_list(VM *vm, const char **remaining, const char *end, Ptr done, char delim) {
  auto input = *remaining;

  auto items = make_extensible_array(vm);

  while(input < end && *input != delim) {
    // would be nice to have some sort of with_protected() here...
    Ptr item;
    call_with_ptrs((items, done),
                   item = read(vm, &input, end, done));
    if(ptr_eq(item, done)) {
      vm->error = "unexpected end of input";
      return done;
    }
    call_with_ptrs((items, done),
                   extensible_array_push(vm, items, item));
    eat_ws(&input, end);
  }
  auto used = extensible_array_used(items);
  auto mem  = extensible_array_memory(items);
  auto res  = make_list(vm, used, mem);
  if (*input == delim) input++;
  *remaining = input;
  return res;
}

// @safe
Ptr read_character(VM *vm, const char **remaining, const char *end, Ptr done) {
  // TODO: would be nice read named characters
  auto input = *remaining;
  if (input >= end) { goto error; }
  input++;
  if (input >= end) { goto error; }
  goto ok;
 error: {
    *remaining = end;
    vm->error = "unexpected end of input";
    return done;
  }
 ok: {
    auto res = charToPtr(*input);
    input++;
    *remaining = input;
    return res;
  }
}

// @safe
Ptr read_bool(VM *vm, const char **remaining, const char *end, Ptr done) {
  auto input = *remaining;
  if (input >= end) { goto error; }
  goto ok;
 error: {
    *remaining = end;
    vm->error = "unexpected end of input";
    return done;
  }
 ok: {
    auto ch = *input;
    auto res = ch == 't' ? TRUE : FALSE; // TODO: error if not #f
    input++;
    *remaining = input;
    return res;
  }
}

// @safe
Ptr read(VM *vm, const char **remaining, const char *end, Ptr done) {
  const char *input = *remaining;
  while (input < end) {
    eat_ws(&input, end);
    if (input >= end) break;
    if (is_symchar(*input)) {
      // @safe
      const char* start = input;
      int len = 1;
      while(input < end && is_symbodychar(*(++input))) {
       len++;
      }
      auto result = intern(vm, start, len);
      *remaining = input;
      return result;
    } else if (*input == '\'') {
      input++;
      // @safe
      prot_ptr(done);
      auto result = quote_form(vm, read(vm, &input, end, done));
      unprot_ptr(done);
      *remaining = input;
      return result;
    } else if (*input == '(') {
      input++;
      // @safe
      prot_ptr(done);
      auto res = read_delimited_list(vm, &input, end, done, ')');
      unprot_ptr(done);
      *remaining = input;
      return res;
    } else if (*input == '#') {
      auto ch = *(++input);
      Ptr res;
      // @safe
      if (ch == '\\') {
        res = read_character(vm, &input, end, done);
      } else {
        res = read_bool(vm, &input, end, done);
      }
      *remaining = input;
      return res;
    } else if (is_digitchar(*input)) {
      u64 num = *input - '0';
      input++;
      // @safe
      while(input < end && is_digitchar(*input)) {
        num *= 10;
        num += *input - '0';
        input++;
      }
      *remaining = input;
      return s64ToPtr(num);
    }
    input++;
  }
  return done;
}

// @safe
Ptr read(VM *vm, const char* input) {
  auto len = strlen(input);
  return read(vm, &input, input+len, NIL);
}

// @safe
Ptr read_all(VM *vm, const char* input) {
  auto done = cons(vm, NIL, NIL);
  auto len = strlen(input);
 
  auto items = make_extensible_array(vm);
  auto end = input + len;
  Ptr item;
  call_with_ptrs((items, done),
                 item = read(vm, &input, end, done));
  while (input < end && !ptr_eq(item, done)) {
    assert(input < end);
    call_with_ptrs((items, done),
                   extensible_array_push(vm, items, item));
    call_with_ptrs((items, done),
                   item = read(vm, &input, end, done));
    assert(input <= end);
  }
  auto used = extensible_array_used(items);
  auto mem  = extensible_array_memory(items);
  auto res  = make_list(vm, used, mem);
  return res;
}

/* -------------------------------------------------- */

void vm_pop_stack_frame(VM* vm) {
  auto fr = vm->frame;
  if (!fr->prev_frame) {
    vm->error = "nowhere to return to";
    return;
  }
  vm->bc = fr->prev_fn;
  vm->pc = fr->prev_pc;
  vm->stack = fr->prev_stack + fr->argc;
  vm->frame = fr->prev_frame;

  // cout << "return stack frome to :" << vm->stack << endl;
}

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn, Ptr closed_over);

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn) {
  vm_push_stack_frame(vm, argc, fn, NIL);
};

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn, Ptr closed_over) {

  uint offset = (sizeof(StackFrameObject) / sizeof(u64));

  u64 *top = &((vm->stack - offset)->value);
  u64 padding = ((u64)top & TAG_MASK) ? 1 : 0;
  top -= padding;
  assert(((u64)top & TAG_MASK) == 0);

  StackFrameObject *new_frame = (StackFrameObject *)top;
  new_frame->header.object_type = StackFrame_ObjectType;
  new_frame->pad_count = padding;

  // cout << "pushing stack frame from: " << vm->stack << endl;
  // cout << "  argc = " << argc << endl;
  // cout << "  offset = " << offset << endl;
  // cout << "  top = " << top << endl;
  // cout << "  &ps = " << &new_frame->prev_stack << endl;
  // cout << "  &ps = " << &new_frame->prev_fn << endl;
  // cout << "  &ps = " << &new_frame->prev_pc << endl;
  // cout << "  &ps = " << &new_frame->argc << endl;

  new_frame->closed_over = closed_over;
  new_frame->prev_stack = vm->stack;
  new_frame->prev_frame = vm->frame;
  new_frame->prev_fn = vm->bc;
  new_frame->prev_pc = vm->pc;
  new_frame->argc = argc;
  vm->stack = (Ptr*)(void *)new_frame; // - 100; // STACK_PADDING
  vm->frame = new_frame;
  vm->bc = fn;
  vm->pc = 0;
}

typedef Ptr (*CCallFunction)(VM*);

enum OpCode {
  END = 0,
  RET = 1,
  PUSHLIT = 2,
  POP = 3,
  BR_IF_ZERO = 5,
  BR_IF_NOT_ZERO = 6,
  DUP = 7,
  CALL = 8,
  LOAD_ARG = 9,
  LOAD_GLOBAL = 10,
  LOAD_CLOSURE = 11,
  BUILD_CLOSURE = 12,
  PUSH_CLOSURE_ENV = 13,
  BR_IF_FALSE = 14,
  JUMP = 15,
  STACK_RESERVE = 16,
  LOAD_FRAME_RELATIVE = 17,
  STORE_FRAME_RELATIVE = 18,
  POP_CLOSURE_ENV = 20,
};

// @safe
void vm_push(VM* vm, Ptr value) {
  *(--vm->stack) = value;
}

// @safe
Ptr vm_pop(VM* vm) {
  return *(vm->stack++);
}

// @safe
auto vm_load_closure_value(VM *vm, u64 slot, u64 depth) {
  auto curr = vm->frame->closed_over;
  while (depth) {
    assert(!isNil(curr));
    curr = array_get(curr, 0);
    depth--;
  }
  assert(!isNil(curr));
  return array_get(curr, slot+1);
}

// @safe
inline u64 vm_curr_instr(VM *vm) {
  return vm->bc->code->data[vm->pc];
}

// @safe
inline u64 vm_adv_instr(VM *vm) {
  return vm->bc->code->data[++vm->pc];
}

// @safe
void vm_interp(VM* vm) {
  u64 instr;
  while ((instr = vm_curr_instr(vm))) {
    switch (instr){
    case STACK_RESERVE: {
      u64 count = vm_adv_instr(vm);
      while (count--) { vm_push(vm, NIL); }
      break;
    }
    case LOAD_FRAME_RELATIVE: {
      u64 idx = vm_adv_instr(vm);
      Ptr *stack_bottom = ((Ptr *)(void *)vm->frame) - 1;
      vm_push(vm, *(stack_bottom - idx));
      break;
    }
    case STORE_FRAME_RELATIVE: {
      u64 idx = vm_adv_instr(vm);
      Ptr it = vm_pop(vm);
      Ptr *stack_bottom = ((Ptr *)(void *)vm->frame) - 1;
      *(stack_bottom - idx) = it;
      break;
    }
    case POP:
      vm_pop(vm);
      break;
    case PUSHLIT: {
      u64 idx = vm_adv_instr(vm);
      Ptr it = vm->bc->literals->data[idx];
      vm_push(vm, it);
      break;
    }
    case LOAD_GLOBAL: {
      // assumes it comes after a pushlit of a cell in the env alist.
      auto it = vm_pop(vm);
      vm_push(vm, cdr(vm, it));
      break;
    }
    case LOAD_CLOSURE: {
      u64 slot  = vm_adv_instr(vm);
      u64 depth = vm_adv_instr(vm);
      auto it = vm_load_closure_value(vm, slot, depth);
      vm_push(vm, it);
      break;
    }
    case BUILD_CLOSURE: {
      auto lambda = vm_pop(vm);
      auto array = vm->frame->closed_over;
      auto closure = make_closure(vm, lambda, array);
      vm_push(vm, closure);
      break;
    }
    case PUSH_CLOSURE_ENV: {
      u64 count = vm_adv_instr(vm);
      auto array = objToPtr(alloc_pao(vm, Array, count + 1));
      array_set(array, 0, vm->frame->closed_over);
      while (count--) {
        auto it = vm_pop(vm);
        // cout << " setting closure val " << it << endl;
        array_set(array, count + 1, it);
      }
      vm->frame->closed_over = array;
      break;
    }
    case POP_CLOSURE_ENV: {
      auto curr = vm->frame->closed_over;
      if (isNil(curr)) {
        vm->error = "cannot pop null closure env ";
      } else {
        auto prev = array_get(curr, 0);
        vm->frame->closed_over = prev;
      }
      break;
    }
    case BR_IF_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = vm_adv_instr(vm);
      if ((u64)it.value == 0) {
        vm->pc = jump - 1; //-1 to acct for pc advancing
      }
      break;
    }
    case BR_IF_NOT_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = vm_adv_instr(vm);
      if ((u64)it.value != 0) {
        vm->pc = jump - 1; //-1 to acct for pc advancing
      }
      break;
    }
    case BR_IF_FALSE: {
      auto it = vm_pop(vm);
      u64 jump = vm_adv_instr(vm);
      if (ptr_eq(it, FALSE)) {
        vm->pc = jump - 1; //-1 to acct for pc advancing
      }
      break;
    }
    case JUMP: {
      u64 jump = vm_adv_instr(vm);
      vm->pc = jump - 1; //-1 to acct for pc advancing
      break;
    }
    case DUP: {
      auto it = vm_pop(vm);
      vm_push(vm, it);
      vm_push(vm, it);
      break;
    }
    case CALL: {
      u64 argc = vm_adv_instr(vm);
      auto fn = vm_pop(vm);
      if (is(PrimOp, fn)) {
        u64 v = fn.value;
        // TODO: validate argc against prim op
        // auto argc = (v >> 16) & 0xFF;
        auto idx  = (v >> 32) & 0xFFFF;
        // cout << " calling prim at idx: " << idx << " arg count = " << argc << endl;
        PrimitiveFunction fn = PrimLookupTable[idx];
        Ptr result = (*fn)(vm);
        vm_push(vm, result);
        break;
      }
      if (!is(Closure, fn)) {
        vm->error = "value is not a closure";
        break;
      }
      auto bc = closure_code(fn);
      auto env = closure_env(fn);
      // cout << "pushing code: " << bc << endl;
      // cout << "pushing env:  " << env << endl;
      vm_push_stack_frame(vm, argc, bc, env);
      vm->pc--; // or, could insert a NOOP at start of each fn... (or continue)
      break;
    }
    case RET: {
      auto it = vm_pop(vm);
      vm_pop_stack_frame(vm);
      vm_push(vm, it);
      // cout << "returning: " << it << endl;
      break;
    }
    case LOAD_ARG: {
      u64 idx = vm_adv_instr(vm);
      u64 argc = vm->frame->argc;
      u64 ofs  = vm->frame->pad_count;
      auto it = vm->frame->argv[ofs + (argc - (idx + 1))];
      vm_push(vm, it);
      // cout << " loading arg "<< idx << ": " << it << endl;
      // vm_dump_args(vm);
      break;
    }
    default:
      vm->error = "unexpected BC";
      return;
    }
    if (vm->error) return;
    ++vm->pc;
  }
}

typedef tuple<u64*, string> branch_entry;

// @safe
class ByteCodeBuilder {
private:
  VM* vm;
  u64* bc_mem;
  u64 bc_index;
  u64 lit_index;

  ByteCodeObject *bc;
  map<string, u64> *labelsMap; // label -> bc_index
  vector<branch_entry> *branchLocations; // tuple of label and &bc_mem
  u64 labelContextCounter;
  vector<u64> *labelContextStack;
  u64 labelContext;

  u64 *temp_count;
  vector<Ptr> *literals;

  ByteCodeBuilder* pushOp(u8 op) {
    return pushU64(op);
  }
  ByteCodeBuilder* pushU64(u64 it) {
    bc_mem[bc_index++] = it;
    return this;
  }
  u64* pushEmptyRef() {
    auto location = bc_mem + bc_index;
    pushU64(0);
    return location;
  }
  string labelify(const char * raw_name) {
    string name = raw_name;
    name += "____" + to_string(labelContext);
    return name;
  }
  ByteCodeBuilder* pushJumpLocation(const char* raw_name) {
    auto name = labelify(raw_name);
    auto location = pushEmptyRef();
    branchLocations->push_back(make_tuple(location,name));
    return this;
  }
  void fixupJumpLocations() {
    for (branch_entry it : *branchLocations) {
      auto loc = get<0>(it);
      auto lbl = get<1>(it);
      auto tgt = (*labelsMap)[lbl];
      *loc = tgt;
    }
  }
  void finalizeByteCode() {
    auto literal_count = literals->size();
    auto literal_mem = std_vector_mem(literals);

    protect_ptr_vector(safe_literals, literal_count, literal_mem);
    auto array = alloc_pao(vm, Array, literal_count);
    for (u64 i = 0; i < literal_count; i++) {
      array->data[i] = unwrap_ptr(safe_literals + i);
    }

    gc_protect(array);
    bc = as(ByteCode, make_bytecode(vm, bc_index + 1));
    gc_unprotect(array);

    bc->literals = array;
    for (u64 i = 0; i < bc_index; i++) {
      bc->code->data[i] = bc_mem[i];
    }
    free(bc_mem);
    delete literals;
    bc_mem = 0;
  }
public:
  ByteCodeBuilder(VM* vm) {
    this->vm = vm;
    bc_index = 0;
    lit_index = 0;
    bc_mem = (u64 *)calloc(1024, 1); // TODO: realloc when needed
    labelsMap = new map<string, u64>;
    branchLocations = new vector<branch_entry>;
    labelContextCounter = 0;
    labelContext = labelContextCounter;
    labelContextStack = new vector<u64>;
    literals = new vector<Ptr>;

    pushOp(STACK_RESERVE);
    temp_count = &bc_mem[bc_index];
    pushU64(0);
    assert(*temp_count == 0);
  }
  u64 reserveTemps(u64 count) {
    auto start= *temp_count;
    *temp_count += count;
    return start;
  }
  auto pushLit(Ptr literal) {
    // TODO: deduplicate them
    literals->push_back(literal);
    pushOp(PUSHLIT);
    pushOp(lit_index);
    lit_index++;
    return this;
  }
  auto loadFrameRel(u64 idx) {
    pushOp(LOAD_FRAME_RELATIVE);
    pushU64(idx);
    return this;
  }
  auto storeFrameRel(u64 idx) {
    pushOp(STORE_FRAME_RELATIVE);
    pushU64(idx);
    return this;
  }
  auto pushLabelContext() {
    labelContextStack->push_back(labelContext);
    labelContext = ++labelContextCounter;
    return this;
  }
  auto popLabelContext() {
    labelContext = labelContextStack->at(labelContextStack->size() - 1);
    labelContextStack->pop_back();
    return this;
  }
  auto dup() {
    pushOp(DUP);
    return this;
  }
  auto label(const char *name) {
    string key = labelify(name);
    (*labelsMap)[key] = bc_index;
    return this;
  }
  auto branchIfZero(const char *name) {
    pushOp(BR_IF_ZERO);
    pushJumpLocation(name);
    return this;
  }
  auto branchIfNotZero(const char *name) {
    pushOp(BR_IF_NOT_ZERO);
    pushJumpLocation(name);
    return this;
  }
  auto branchIfFalse(const char *name) {
    pushOp(BR_IF_FALSE);
    pushJumpLocation(name);
    return this;
  }
  auto jump(const char *name) {
    pushOp(JUMP);
    pushJumpLocation(name);
    return this;
  }
  auto call(u64 argc) {
    pushOp(CALL);
    pushU64(argc);
    return this;
  }
  auto pop(){
    pushOp(POP);
    return this;
  }
  auto ret() {
    pushOp(RET);
    return this;
  }
  auto loadArg(u64 index) {
    pushOp(LOAD_ARG);
    pushU64(index);
    return this;
  }
  auto loadGlobal(Ptr sym) {
    if (!is(Symbol, sym)) {
      cout << " ERROR: " << sym << " is not a symbol.";
      assert(false);
    }
    auto pair = assoc(vm, sym, vm->globals->env);
    if (!consp(vm, pair)) {
      cout << " ERROR: " << sym << " is not defined in the global environment.";
      assert(false);
    }
    pushLit(pair);
    pushOp(LOAD_GLOBAL);
    return this;
  }
  auto loadGlobal(const char *name) {
    auto sym = intern(vm, name);
    return loadGlobal(sym);
  }
  auto loadClosure(u64 slot, u64 depth) {
    pushOp(LOAD_CLOSURE);
    pushU64(slot);
    pushU64(depth);
    return this;
  }
  auto buildClosure() {
    pushOp(BUILD_CLOSURE);
  }
  auto pushClosureEnv(u64 count) {
    pushOp(PUSH_CLOSURE_ENV);
    pushU64(count);
  }
  auto popClosureEnv() {
    pushOp(POP_CLOSURE_ENV);
  }
  auto call(u64 argc, const char *name) {
    loadGlobal(name);
    call(argc);
    return this;
  }
  ByteCodeObject *build() {
    pushOp(END);
    fixupJumpLocations();
    finalizeByteCode();
    return bc;
  }
};

/* -------------------------------------------------- */

enum VariableScope {
  VariableScope_Global,
  VariableScope_Argument,
  VariableScope_Closure,
  VariableScope_Let,
};

enum CompilerEnvType {
  CompilerEnvType_Unknown,
  CompilerEnvType_Lambda,
  CompilerEnvType_Let
};

struct VariableInfo {
  VariableScope scope;
  u64 argument_index;
  u64 closure_index;
};

struct VariableBinding {
  u64 binding_depth;
  VariableInfo *info;
};

// TODO: would be nicer to represent this in the VM itself.
struct CompilerEnv {
  CompilerEnv *prev;
  // @gc
  unordered_map<u64, VariableInfo> *info;  // symbol -> VariableInfo
  // @gc
  unordered_map<u64, CompilerEnv*> *sub_envs; // symbol -> CompilerEnv *
  // @gc ?
  vector<u64> *closed_over; // [symbol]
  bool has_closure;
  CompilerEnvType type;
  CompilerEnv(CompilerEnv * parent_env) {
    prev = parent_env;
    info = new unordered_map<u64, VariableInfo>();
    sub_envs = new unordered_map<u64, CompilerEnv*>();
    closed_over = new vector<u64>();
    has_closure = false;
    type = CompilerEnvType_Unknown;
  }
  ~CompilerEnv() {
    delete info;
    delete closed_over;
    for (auto pair : *sub_envs) {
      delete pair.second;
    }
    delete sub_envs;
  }
};


// @gc
auto compiler_env_get_subenv(CompilerEnv *env, Ptr it) {
  auto key = it.value;
  if (env->sub_envs->find(key) == env->sub_envs->end()) {
    auto created = new CompilerEnv(env);
    env->sub_envs->insert(make_pair(key, created));
  }
  return env->sub_envs->find(key)->second;
}

VariableInfo GLOBAL_INFO = (VariableInfo){ VariableScope_Global, 0, 0};

// @gc
VariableBinding compiler_env_binding(CompilerEnv *env, Ptr sym) {
  if (!env) return (VariableBinding){0, &GLOBAL_INFO};
  auto existing = env->info->find(sym.value);
  if (existing == env->info->end()) {
    auto outer = compiler_env_binding(env->prev, sym);
    auto from_lambda = env->prev && env->prev->type == CompilerEnvType_Lambda;
    if (outer.info->scope == VariableScope_Argument && from_lambda) {
      cout << "  ERROR: variable should have been marked for closure: " << sym << endl;
      assert(false);
    }
    auto depth = outer.binding_depth + 1;
    auto info  = outer.info;
    return (VariableBinding){depth, info};
  }
  return (VariableBinding){0, &existing->second};
}

void emit_expr(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* env);

// @safe
void emit_call(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* env) {
  auto fn = car(vm, it);
  auto args = cdr(vm, it);
  auto argc = 0;
  prot_ptr(fn);
  while (!isNil(args)) {
    assert(consp(vm, args));
    argc++;
    call_with_ptrs((args),
                   emit_expr(vm, builder, car(vm, args), env));
    args = cdr(vm, args);
  }
  unprot_ptr(fn);
  emit_expr(vm, builder, fn, env);
  builder->call(argc);
}

// @safe
void emit_lambda_body(VM *vm, ByteCodeBuilder *builder, Ptr body, CompilerEnv *env) {
  if (isNil(body)) {
    builder->pushLit(NIL);
    return;
  }
  assert(consp(vm, body));
  while(!isNil(body)) {
    auto expr = car(vm, body);
    call_with_ptrs((body),
                   emit_expr(vm, builder, expr, env));
    body = cdr(vm, body);
    if (!isNil(body)) builder->pop();
  }
}

// @safe
auto emit_flat_lambda(VM *vm, Ptr it, CompilerEnv *env) {
  it = cdr(vm, it);
  auto builder = new ByteCodeBuilder(vm);
  auto body = cdr(vm, it);
  emit_lambda_body(vm, builder, body, env);
  builder->ret();
  auto bc = objToPtr(builder->build());
  return make_closure(vm, bc, NIL);
}

void emit_lambda(VM *vm, ByteCodeBuilder *p_builder, Ptr it, CompilerEnv* p_env) {
  // @gc
  CompilerEnv *env = compiler_env_get_subenv(p_env, it);
  auto has_closure = env->has_closure;
  if (has_closure) {
    auto closed_count = env->closed_over->size();
    auto builder = new ByteCodeBuilder(vm);
    // cout << " closing over " << closed_count << " arguments." << endl;
    // cout << "   form  = " << it << endl;
    for (auto raw: *env->closed_over) {
      Ptr ptr = {raw};
      auto binding = compiler_env_binding(env, ptr);
      // cout << "  closing over: " << ptr  << " idx: " << info.argument_index << endl;
      builder->loadArg(binding.info->argument_index);
    }
    builder->pushClosureEnv(closed_count);
    auto body = cdr(vm, cdr(vm, it));
    // @gc
    emit_lambda_body(vm, builder, body, env);
    builder->ret();
    p_builder->pushLit(objToPtr(builder->build()));
    p_builder->buildClosure();
  } else {
    // @safe
    auto closure = emit_flat_lambda(vm, it, env);
    p_builder->pushLit(closure);
  }
}

// @gc
void emit_let(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* p_env) {
  CompilerEnv *env = compiler_env_get_subenv(p_env, it);
  auto vars = nth_or_nil(vm, it, 1);
  auto count = list_length(vm, vars);
  auto start_index = builder->reserveTemps(count);

  do_list(vm, vars, [&](Ptr lst){
      auto sym = nth_or_nil(vm, lst, 0);
      auto expr = nth_or_nil(vm, lst, 1);
      assert(is(Symbol, sym));
      auto binding = compiler_env_binding(env, sym);
      auto idx = binding.info->argument_index + start_index;
      binding.info->argument_index = idx;

      emit_expr(vm, builder, expr, p_env);
      builder->storeFrameRel(idx);
    });

  auto has_closure = env->has_closure;
  if (has_closure) {
    auto closed_count = env->closed_over->size();
    for (auto raw: *env->closed_over) {
      Ptr ptr = {raw};
      auto binding = compiler_env_binding(env, ptr);
      // cout << "  closing over: " << ptr  << " idx: " << info.argument_index << endl;
      builder->loadFrameRel(binding.info->argument_index);
    }
    builder->pushClosureEnv(closed_count);
  }

  auto body = cdr(vm, cdr(vm, it));
  builder->pushLit(NIL); // LAZY
  do_list(vm, body, [&](Ptr expr){
      builder->pop();
      emit_expr(vm, builder, expr, env);
    });

  if (has_closure) {
    builder->popClosureEnv();
  }
}


// @gc
void emit_if(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* env) {
  auto test = nth_or_nil(vm, it, 1);
  auto _thn = nth_or_nil(vm, it, 2);
  auto _els = nth_or_nil(vm, it, 3);
  builder->pushLabelContext();
  emit_expr(vm, builder, test, env);
  builder->branchIfFalse("else");
  emit_expr(vm, builder, _thn, env);
  builder->jump("endif")->label("else");
  emit_expr(vm, builder, _els, env);
  builder->label("endif");
  builder->popLabelContext();
}

// @gc
void emit_expr(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* env) {
  if (is(Symbol, it)) {
    auto binding = compiler_env_binding(env, it);
    auto info = binding.info;
    if (info->scope == VariableScope_Global) {
      builder->loadGlobal(it);
    } else if (info->scope == VariableScope_Argument) {
      builder->loadArg(info->argument_index);
    } else if (info->scope == VariableScope_Closure) {
      auto index = info->closure_index;
      auto depth = binding.binding_depth;
      // cout << " closure scope, " << index << " " << depth << endl;
      builder->loadClosure(index, depth);
    } else if (info->scope == VariableScope_Let) {
      // cout << " Loading FR REL: " << info->argument_index << endl;
      builder->loadFrameRel(info->argument_index); // LAZY reusing argument_index
    } else {
      assert(false);
    }
  } else if (consp(vm, it)) {
    auto fst = car(vm, it);
    if (is(Symbol, fst)) {
      auto _if = intern(vm, "if");
      auto quote = intern(vm, "quote");
      auto lambda = intern(vm, "lambda");
      auto let = intern(vm, "let");
      if (ptr_eq(lambda, fst)) {
        emit_lambda(vm, builder, it, env);
        return;
      } else if (ptr_eq(quote, fst)) {
        auto item = car(vm, cdr(vm, it));
        builder->pushLit(item);
        return;
      } else if (ptr_eq(_if, fst)) {
        emit_if(vm, builder, it, env);
        return;
      } else if (ptr_eq(let, fst)) {
        emit_let(vm, builder, it, env);
        return;
      }
    }
    emit_call(vm, builder, it, env);
  } else {
    builder->pushLit(it);
  }
}

// returns true if variable was closed over, false otherwise
// @gc
bool mark_variable_for_closure
(VM *vm, Ptr sym, CompilerEnv *env, u64 level, bool saw_lambda)
{
  if (!env) return false; // global scope
  auto existing = env->info->find(sym.value);

  // symbol is bound at this scope.
  if (existing != env->info->end()) {
    // symbol was found in its enclosing scope, do nothing
    if (level == 0) return false;
    // there was no lambda in the lower scopes, so do nothing.
    if (!saw_lambda) return false;

    // otherwise, was found in an outer scope, and we need to create a closure.
    auto info = &existing->second;
    if (info->scope == VariableScope_Closure) return true;
    info->scope = VariableScope_Closure;
    info->closure_index = env->closed_over->size();
    env->closed_over->push_back(sym.value);
    env->has_closure = true;
    return true;

  } else {
    if (env->type == CompilerEnvType_Lambda) saw_lambda = true;
    auto closed = mark_variable_for_closure(vm, sym, env->prev, level + 1, saw_lambda);
    if (closed) env->has_closure = true;
    return closed;
  }
}

void mark_closed_over_variables(VM *vm, Ptr it, CompilerEnv* env);

// @gc
void mark_lambda_closed_over_variables(VM *vm, Ptr it, CompilerEnv *p_env) {
  CompilerEnv *env = compiler_env_get_subenv(p_env, it);
  env->type = CompilerEnvType_Lambda;

  it = cdr(vm, it);
  auto args = car(vm, it);
  u64 idx = 0;
  while (!isNil(args)) {
    auto arg = car(vm, args);
    assert(is(Symbol, arg));
    auto info = (VariableInfo){VariableScope_Argument, idx++, 0};
    env->info->insert(make_pair(arg.value, info));
    args = cdr(vm, args);
  }
  auto body = cdr(vm, it);
  if (isNil(body)) return;
  assert(consp(vm, body));
  while(!isNil(body)) {
    auto expr = car(vm, body);
    mark_closed_over_variables(vm, expr, env);
    body = cdr(vm, body);
  }
}

// @gc
void mark_let_closed_over_variables(VM *vm, Ptr it, CompilerEnv* p_env) {
  auto env = compiler_env_get_subenv(p_env, it);
  env->type = CompilerEnvType_Let;

  auto vars = nth_or_nil(vm, it, 1);

  u64 idx = 0;
  do_list(vm, vars, [&](Ptr lst){
      auto sym = nth_or_nil(vm, lst, 0);
      auto expr = nth_or_nil(vm, lst, 1);
      assert(is(Symbol, sym));

      // idx is altered in the emit phase to account for surrounding lets
      auto info = (VariableInfo){VariableScope_Let, idx, 0};
      env->info->insert(make_pair(sym.value, info));

      mark_closed_over_variables(vm, expr, p_env);
      idx++;
    });

  auto body = cdr(vm, cdr(vm, it));
  do_list(vm, body, [&](Ptr expr) {
      mark_closed_over_variables(vm, expr, env);
    });
}

// @gc
void mark_closed_over_variables(VM *vm, Ptr it, CompilerEnv* env) {
  if (is(Symbol, it)) {
    mark_variable_for_closure(vm, it, env, 0, false);
  } else if (consp(vm, it)) {
    auto fst = car(vm, it);
    if (is(Symbol, fst) && ptr_eq(intern(vm, "lambda"), fst)) {
      mark_lambda_closed_over_variables(vm, it, env);
    } else if (is(Symbol, fst) && ptr_eq(intern(vm, "quote"), fst)) {
      // do nothing
    } else if (is(Symbol, fst) && ptr_eq(intern(vm, "if"), fst)) {
      auto test = nth_or_nil(vm, it, 1);
      auto _thn = nth_or_nil(vm, it, 2);
      auto _els = nth_or_nil(vm, it, 3);
      mark_closed_over_variables(vm, test, env);
      mark_closed_over_variables(vm, _thn, env);
      mark_closed_over_variables(vm, _els, env);
    } else if (is(Symbol, fst) && ptr_eq(intern(vm, "let"), fst)) {
      mark_let_closed_over_variables(vm, it, env);
    } else {
      while(!isNil(it)) {
        mark_closed_over_variables(vm, car(vm, it), env);
        it = cdr(vm, it);
      }
    }
  }
}

// @gc
auto compile_toplevel_expression(VM *vm, Ptr it) {
  auto env = new CompilerEnv(nullptr);
  auto builder = new ByteCodeBuilder(vm);
  mark_closed_over_variables(vm, it, env);
  emit_expr(vm, builder, it, env);
  delete env;
  return builder->build();
}

// @gc -- continue audit from here

/* -------------------------------------------------- */

Ptr primitive_print(Ptr a) { cout << a << endl; return a; }

#include "./primop-generated.cpp"

/* -------------------------------------------------- */

void run_string(const char* str) {
  VM *vm;
  vm = (VM *)calloc(sizeof(VM), 1);

  auto count = 1024 * 100;
  Ptr *stack_mem = (Ptr *)calloc(sizeof(Ptr), count);
  vm->stack = stack_mem + (count - 1);

  auto heap_size_in_mb = 50;
  auto heap_size_in_bytes = heap_size_in_mb * 1024 * 1024;
  auto heap_mem = calloc(heap_size_in_bytes, 1);
  vm->heap_mem = heap_mem;
  vm->heap_end = heap_mem;
  auto alt_heap_mem = calloc(heap_size_in_bytes, 1);
  vm->alt_heap_mem = alt_heap_mem;
  vm->alt_heap_end = alt_heap_mem;

  vm->heap_size_in_bytes = heap_size_in_bytes;
  vm->allocation_count = 0;
  vm->gc_count = 0;
  vm->gc_threshold_in_bytes = 1 * 1024 * 1024;

  vm->gc_protected = new set<Object **>;

  vm->frame = 0;
  vm->error = 0;

  vm->globals = (Globals *)calloc(sizeof(Globals), 1);
  vm->globals->symtab = new unordered_map<string, Ptr>;
  vm->globals->env = NIL;
  initialize_classes(vm);
  initialize_primitive_functions(vm);

  // purely for debug printing. would be nice to get rid of this
  CURRENT_DEBUG_VM = vm;

  // so we have a root frame
  auto bc = (new ByteCodeBuilder(vm))->build();
  vm_push_stack_frame(vm, 0, bc);
  gc_protect(bc);

  auto exprs = read_all(vm, str);
  auto raw_exprs = as(Object, exprs);

  while (!isNil(exprs)) {
    gc_protect(raw_exprs);

    auto expr = car(vm, exprs);
    auto bc = compile_toplevel_expression(vm, expr);

    gc_protect(bc);
    vm_push_stack_frame(vm, 0, bc);

    vm_interp(vm);

    vm_pop_stack_frame(vm);
    gc_unprotect(bc);

    gc(vm);

    if (vm->error) {
      puts("VM ERROR: ");
      puts(vm->error);
      return;
    }

    // it would really be nice if there was an easier way to do this...
    gc_unprotect(raw_exprs);
    exprs = objToPtr(raw_exprs);
    exprs = cdr(vm, exprs);
    raw_exprs = as(Object, exprs);
    gc_protect(raw_exprs);
  }

  gc_unprotect(bc);
  gc_unprotect(raw_exprs);

  vm_count_objects_on_heap(vm);
  vm_count_reachable_refs(vm);
  cout << " running gc..." << endl;
  gc(vm);
  vm_count_objects_on_heap(vm);
  vm_count_reachable_refs(vm);
  report_memory_usage(vm);

  // TODO: clean up
}

const char *read_file_contents(string path) {
  ifstream istream(path);
  stringstream buffer;
  buffer << istream.rdbuf();
  auto str = buffer.str();
  // TODO: does this require freeing later?
  auto cstr = str.c_str();
  auto len = strlen(cstr) + 1;
  auto mem = (char *)calloc(len, 1);
  strcpy(mem, (char *)cstr);
  return (const char *)mem;
}

auto run_file(string path) {
  auto contents = read_file_contents(path);
  run_string(contents);
  // TODO: free contents
}

/* ---------------------------------------- */

int main() {
  run_file("./hello.lisp");
  return 0;
}
