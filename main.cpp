/*

(setq flycheck-clang-language-standard "c++14")
(setq flycheck-clang-include-path '("~/Library/Frameworks"))

DONE: stack traces
DONE: move allocations into vm-managed heap
DONE: lists (incl printing)
DONE: lisp reader
DONE: expression compiler
DONE: lambda compiler
DONE: lambdas + closures compiler
DONE: def or define or something
TODO: varargs
TODO: quasiquotation
DONE: booleans
DONE: characters
DONE: obj_size function // heap size of object (can be 0 for imms)
DONE: obj_ptrs function // walk Ptr s of obj (use a lambda)
DONE: make stack frames objects
TODO: read string
TODO: tests! (something like an assert)
DONE: bounds checking for heap allocation
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
DONE: initial audit of requred gc_protect calls
DONE: gc_protect for ptrs
DONE: gc-safe list manipulation functions
DONE: gc-safe allocators / make- functions
DONE: growable ptr array
DONE: growable identity map
DONE: gc-safe reader
DONE: gc-safe BCBuilder
DONE: move varinfo & CompilerEnv into gc heap
DONE: gc-safe compiler
DONE: automatic garbage collection (need to use gc_protect)
DONE: identity hash function
DONE: proper imap
DONE: make cons a defstruct
TODO: move stack memory into vm-managed heap
TODO: continuations / exceptions / signals
TODO: dump/restore image
TODO: write macroexpander in the language itself
TODO: write reader in the language itself
DONE: basic sdl integration
TODO: single floats
TODO: U32 Array etc
TODO: more prim instrs
TODO: maybe expose bytecode prims as special forms? %push %call etc...
TODO: growable heap
TODO: looping special form (while?)
DONE: repl
TODO: basic notebook
TODO: notebook w/ event reactor
DONE: 'load file' vs run file
TODO: rewrite the codegen in the lang itself 
TODO: 'apply'
DONE: basic 'event reactor' runloop -- start with keycodes
TODO: optimize global calls for more arities
DONE: point functions
TODO: real 'built-in-classes' i.e. Cons Fixnum etc
TODO: basic message definition, send facility
DONE: image objects
DONE: rotation + scale BitBlt like thing
TODO: maybe store image data off-heap?

maybe have a stack of compilers? can push/pop...
have each compiler pass output to previous one in the stack

how to represent U32 and U64?

how will we pass callbacks through to the VM?  e.g. if I want to map
values of a ht?  safe to 'just' push a stack frame?  but how do we
yield control to the vm, and get it back?

RE: storing image data off-heap: could have a 'byte-blob' object type.
after a gc, scan the old heap for byte-blobs that are not broken hearts,
and free the mem. it's a tradeoff, two heap scans, but less copying.
could become important as there will eventually be many images

*/

#include <SDL2/SDL.h>
#include <SDL2_image/SDL_image.h>
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
#include <chrono>
#include <thread>
#include <bitset>
#include "./stacktrace.h"
#include "./macro_support.h"

#define GC_DEBUG 0

#define unused(x) (void)(x)

using namespace std;

typedef unsigned int uint;
typedef uint64_t u64;
typedef uint32_t u32;
typedef int32_t s32;
typedef int64_t s64;
typedef uint8_t u8;
typedef int8_t s8;

#define EXTRACT_PTR_MASK 0xFFFFFFFFFFFFFFF0
#define TAG_MASK 0b1111
#define TAG_BITS 4

#define Fixnum_Tag 0b0000
#define Object_Tag 0b0001
#define Char_Tag   0b0011
#define Bool_Tag   0b0100
#define PrimOp_Tag 0b0101
#define Point_Tag  0b0110
// #define UNUSED_TAG 0b0111
// #define UNUSED_TAG 0b1000
// #define UNUSED_TAG 0b1001
// #define UNUSED_TAG 0b1010
// #define UNUSED_TAG 0b1011
// #define UNUSED_TAG 0b1100
// #define UNUSED_TAG 0b1101
// #define UNUSED_TAG 0b1110
// #define UNUSED_TAG 0b1111
// what about a float?

struct Ptr { u64 value; };

#define True  ((Ptr){0b10100})
#define False ((Ptr){0b00100})
// do we really need nil?
#define Nil   ((Ptr){0b0001})

std::ostream &operator<<(std::ostream &os, Ptr p);

inline bool operator == (Ptr a, Ptr b) {
  return a.value == b.value;
}

bool ptr_eq(Ptr a, Ptr b) {
  return a.value == b.value;
}

enum ObjectType : u32 {
  ByteCode_ObjectType,
  ByteArray_ObjectType,
  U64Array_ObjectType,
  PtrArray_ObjectType,
  Standard_ObjectType,
  StackFrame_ObjectType,
  BrokenHeart
};

struct Header {
  ObjectType object_type;
  u32 hashcode;
  u64 flags;
};

struct Object {
  Header header;
};

// TODO: rename this function
inline Ptr objToPtr(Object *ref) {
  Ptr p;
  p.value = ((u64) ref) |  0b1;
  return p;
}


/* ---------------------------------------- */
//          GC protection macros

struct VM;
inline void gc_protect_ptr(VM *vm, Ptr *ref);
inline void gc_unprotect_ptr(VM *vm, Ptr *ref);
inline void gc_protect_ptr_vector(VM *vm, Ptr *ref, u64 count);
inline void gc_unprotect_ptr_vector(VM *vm, Ptr *ref);
inline void gc_protect_reference(VM *vm, Object **);
inline void gc_unprotect_reference(VM *vm, Object **);

#define prot_ptr(it) gc_protect_ptr(vm, &it)
#define unprot_ptr(it) gc_unprotect_ptr(vm, &it)

#define protect_ptr_vector(vector, count)  gc_protect_ptr_vector(vm, vector, count)

#define unprotect_ptr_vector(vector)  gc_unprotect_ptr_vector(vm, vector)

#define std_vector_mem(vector) &(*vector->begin())

#define __prot_ptr(x) prot_ptr(x);
#define prot_ptrs(...) MAP(__prot_ptr, __VA_ARGS__)
#define __unprot_ptr(x) unprot_ptr(x);
#define unprot_ptrs(...) MAP(__unprot_ptr, __VA_ARGS__)

#define gc_protect(it) do {                     \
    auto _ref = (Object **)&it;                 \
    gc_protect_reference(vm, _ref);             \
  } while(0)
#define gc_unprotect(it) do {                   \
    auto _ref = (Object **)&it;                 \
    gc_unprotect_reference(vm, _ref);           \
  } while(0)

/* ---------------------------------------- */

struct U64ArrayObject : Object {
  u64 length;
  u64 data[];
};

typedef enum {
  Array,
  Closure,
  Struct
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
  u64 instruction_count;
  Globals *globals;
  bool gc_disabled;
  bool in_gc;
  u64 gc_count;
  u64 gc_threshold_in_bytes;
  u64 gc_compacted_size_in_bytes;
  u64 allocation_high_watermark;
  unordered_map<Object **, u64> *gc_protected;
  unordered_map<Ptr *, u64> *gc_protected_ptrs;
  unordered_map<Ptr *, u64> *gc_protected_ptr_vectors;
  SDL_Surface *surface;
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

inline u64 vm_heap_used(VM *vm) {
  return (u64)vm->heap_end - (u64)vm->heap_mem;
}

#define _ostream_prefix(x) << x
#define die(...) do {                           \
    print_stacktrace();                         \
    cerr MAP(_ostream_prefix, __VA_ARGS__) << endl; \
    exit(1);                                    \
  } while(0)

#define dbg(...) cerr MAP(_ostream_prefix, __VA_ARGS__) << endl

void gc(VM *vm);

void * vm_alloc(VM *vm, u64 bytes) {
  auto preserved_heap_end = vm->heap_end;
  auto result = (void *)vm->heap_end;
  assert(pointer_is_aligned(result));
  vm->heap_end = align_pointer_with_offset(vm->heap_end, bytes);
  assert(pointer_is_aligned(vm->heap_end));
  vm->allocation_count++;

  u64 used_byte_count = vm_heap_used(vm);

  u64 threshold = 1 * 1024 * 1024 * 0.5;

  u64 last_byte_count = vm->gc_compacted_size_in_bytes;
  u64 limit = min(threshold + last_byte_count, vm->heap_size_in_bytes);

  if (used_byte_count > limit && !vm->gc_disabled && !vm->in_gc) {
      vm->heap_end = preserved_heap_end;
      gc(vm);
      if (vm_heap_used(vm) + bytes + 16 > vm->heap_size_in_bytes) {
        die("heap exhausted after gc");
      }
      vm->gc_disabled = true;
      auto result = vm_alloc(vm, bytes);
      vm->gc_disabled = false;
      return result;
  }

  if (used_byte_count > vm->heap_size_in_bytes) {
    die("heap exhausted");
  }

  if (used_byte_count > vm->allocation_high_watermark) {
    vm->allocation_high_watermark = used_byte_count;
  }

  bzero(result, bytes);
  return result;
}

void report_memory_usage(VM *vm) {
  double byte_count = (u64)vm->heap_end - (u64)vm->heap_mem;
  double max_byte_count = vm->allocation_high_watermark;
  cerr << " heap used MB: " << (byte_count / (1024 * 1024));
  cerr << " max heap used MB: " << (max_byte_count / (1024 * 1024)) << endl;
}

/* -------------------------------------------------- */

typedef enum {
  String,
  Symbol,
  Image
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

#define _type_test_name(type) is_##type##__Impl
#define _ptr_creation_name(type) to_##type##_Ptr__Impl
#define _ptr_conversion_name(type) as##type##__Impl

// IS Ptr it of this type Ptr -> bool
#define is(type, it) _type_test_name(type)(it)
// AS this type (like a primitive cast, or conversion) Ptr -> raw type
#define as(type, it) _ptr_conversion_name(type)(it)
// TO the Ptr representing this type raw type -> Ptr
#define to(type, it) _ptr_creation_name(type)(it)

#define type_test(type, var) inline bool _type_test_name(type)(Ptr var)
#define create_ptr_for(type, var) inline Ptr _ptr_creation_name(type)(var)
#define unwrap_ptr_for(type, var) inline auto _ptr_conversion_name(type)(Ptr var)

#define prim_type(type) type_test(type, it){    \
    return (it.value & TAG_MASK) == type##_Tag; \
  }

unwrap_ptr_for(Object, self) {
  return (Object *)(self.value & EXTRACT_PTR_MASK);
}

type_test(NonNilObject, it) {
  return it.value != 1 && ((it.value & TAG_MASK) == Object_Tag);
}

type_test(any, it) { unused(it); return true; }
unwrap_ptr_for(any, it) { return it; }

#define object_type(type)                                               \
  type_test(type, it) {                                                 \
    return (is(NonNilObject, it) &&                                     \
            (as(Object, it))->header.object_type == type##_ObjectType); \
  };                                                                    \
  unwrap_ptr_for(type, it) {                                            \
    assert(is(type, it));                                               \
    return (type##Object *)as(Object, it);                              \
  }


prim_type(Fixnum)
create_ptr_for(Fixnum, s64 value) {
  // TODO: overflow check
  Ptr p;
  p.value = value << TAG_BITS;
  return p;
}
unwrap_ptr_for(Fixnum, it) {
  return (s64)(((s64)it.value) >> TAG_BITS);
}

prim_type(Char)
create_ptr_for(Char, char ch) {
  // TODO wide char support? (there's room in the Ptr)
  auto val = ((u64)ch << TAG_BITS)|Char_Tag;
  return (Ptr){val};
}
unwrap_ptr_for(Char, it) {
  return (char)(it.value >> TAG_BITS);
}

prim_type(Bool)
create_ptr_for(Bool, bool tf) {
  return tf ? True : False;
}
unwrap_ptr_for(Bool, it){
  return (it.value >> TAG_BITS) ? true : false;
}

prim_type(PrimOp)
create_ptr_for(PrimOp, u64 raw_value) {
  return (Ptr){raw_value};
}

struct point { s32 x, y; };
point operator +(point a, point b) { return (point){a.x + b.x, a.y + b.y}; }
point operator -(point a, point b) { return (point){a.x - b.x, a.y - b.y}; }

// @cleanup -- there's gotta be a way to do this with fewer instructions
prim_type(Point)
create_ptr_for(Point, point p) {
  // cout << " p.x = " << bitset<32>(p.x) << " p.y =" << bitset<32>(p.y) << endl;
  auto mask      = (1ULL << 30) - 1;
  auto high_mask = mask << 34;
  auto low_mask  = mask << 4;
  auto x_sign    = p.x < 0 ? (1ULL << 63) : 0;
  u64 x_comp     = (((u64)p.x << 34) & high_mask) | x_sign;
  auto y_sign    = p.y < 0 ? (1ULL << 33) : 0;
  u64 y_comp     = (((u64)p.y << 4) & low_mask) | y_sign;
  u64 value      = x_comp | y_comp | Point_Tag;
  return (Ptr){value};
}
unwrap_ptr_for(Point, it) {
  point p;
  auto mask = (1ULL << 30) - 1;
  u64 xbit = it.value & (1ULL << 63) ? 0b11 : 0b00;
  u64 ybit = it.value & (1ULL << 33) ? 0b11 : 0b00;
  // cout << " x < 0 ? " << xbit << "   y < 0 ? " << ybit << endl;
  auto val = it.value >> 4;
  u32 y = (val & mask) | (ybit << 30);
  u32 x = ((val >> 30) & mask) | (xbit << 30);
  p.x = (s32)x;
  p.y = (s32)y;
  // cout << " p.x = " << bitset<32>(p.x) << " p.y =" << bitset<32>(p.y) << endl;
  return p;
}


type_test(Object, it) { return is(NonNilObject, it); }
object_type(ByteCode)
object_type(ByteArray)
object_type(U64Array)
object_type(PtrArray)
object_type(Standard)
object_type(StackFrame)

type_test(BrokenHeart, it) {
  return is(Object, it) && as(Object, it)->header.object_type == BrokenHeart;
}

#undef prim_type
#undef object_type

#define VM_ARG(type, name)                             \
  Ptr _##name = vm_pop(vm);                            \
  if (!is(type, _##name)) {                            \
    vm->error = " argument " #name " is not a " #type; \
    return Nil;                                        \
  }                                                    \
  auto name = as(type, _##name);

/* ---------------------------------------- */

// TODO: convert this to type-test
inline bool isNil(Ptr self) {
  return self.value == Object_Tag;
}

U64ArrayObject *alloc_u64ao(VM *vm, uint len) {
  auto byte_count = sizeof(U64ArrayObject) + (len * sizeof(u64));
  U64ArrayObject* obj = (U64ArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = U64Array_ObjectType;
  obj->length = len;
  return obj;
}

ByteCodeObject *alloc_bytecode(VM *vm) {
  auto byte_count = sizeof(ByteCodeObject);
  ByteCodeObject *obj = (ByteCodeObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteCode_ObjectType;
  return obj;
}

ByteCodeObject *toBytecode(Ptr it) {
  assert(is(ByteCode, it));
  return (ByteCodeObject *)as(Object, it);
}

ByteArrayObject *alloc_bao(VM *vm, BAOType ty, uint len) {
  auto byte_count = sizeof(ByteArrayObject) + len;
  ByteArrayObject* obj = (ByteArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteArray_ObjectType;
  obj->bao_type = ty;
  obj->length = len;
  return obj;
}

type_test(Symbol, it) {
  if (!is(ByteArray, it)) return false;
  auto bao = as(ByteArray, it);
  return bao->bao_type == Symbol;
}
unwrap_ptr_for(Symbol, it) {
  return as(ByteArray, it);
}

type_test(String, it) {
  if (!is(ByteArray, it)) return false;
  auto bao = as(ByteArray, it);
  return bao->bao_type == String;
}
unwrap_ptr_for(String, it) {
  return as(ByteArray, it);
}

type_test(Image, it) {
  if (!is(ByteArray, it)) return false;
  auto bao = as(ByteArray, it);
  return bao->bao_type == Image;
}
unwrap_ptr_for(Image, it) {
  return as(ByteArray, it);
}

u32 image_width(ByteArrayObject *it) {
  assert(it->bao_type == Image);
  return *(u32*)it->data;
}
u32 image_height(ByteArrayObject *it) {
  assert(it->bao_type == Image);
  return *(((u32*)it->data)+1);
}

u8* image_data(ByteArrayObject *it) {
  assert(it->bao_type == Image);
  return (u8*)(((u32*)it->data)+2);
}

ByteArrayObject *alloc_image(VM *vm, u32 w, u32 h) {
  auto byte_count = (w * h * 4) + 8;
  // die("byte count is: ", byte_count);
  auto result = alloc_bao(vm, Image, byte_count);
  auto u32s = (u32*)result->data;
  u32s[0] = w; u32s[1] = h;
  return result;
}

Ptr load_image_from_path(VM *vm, string path) {
  auto surface = IMG_Load(path.c_str());
  if (!surface) die("could not load image: ", IMG_GetError());
  auto w = surface->w; auto h = surface->h;
  auto img = alloc_image(vm, w, h);
  auto mem = (u32*)image_data(img);
  for (auto y = 0; y < h; y++) {
    auto stride =  y * surface->pitch;
    for (auto x = 0; x < w; x++) {
      u32 pixel = *(u32 *)((u8*)surface->pixels + stride + x * 4);
      mem[y * w + x] = pixel;
    }
  }
  SDL_FreeSurface(surface);
  return objToPtr(img);
}

Ptr load_image(VM *vm, ByteArrayObject *path) {
  if (!is(String, objToPtr(path))) die("load_image takes a string");
  auto str = string(path->data, path->length);
  return load_image_from_path(vm, str);
}

PtrArrayObject *alloc_pao(VM *vm, PAOType ty, uint len) {
  auto byte_count = sizeof(PtrArrayObject) + (len * sizeof(Ptr));
  PtrArrayObject* obj = (PtrArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = PtrArray_ObjectType;
  obj->pao_type = ty;
  obj->length = len;
  return obj;
}

type_test(Array, it) {
  return is(PtrArray, it) && (as(PtrArray, it))->pao_type == Array;
}
unwrap_ptr_for(Array, it) {
  return as(ByteArray, it);
}

type_test(Struct, it) {
  return is(PtrArray, it) && (as(PtrArray, it))->pao_type == Struct;
}
unwrap_ptr_for(Struct, it) {
  return as(ByteArray, it);
}

StandardObject *alloc_standard_object(VM *vm, StandardObject *klass, u64 ivar_count) {
  auto byte_count = (sizeof(StandardObject)) + ivar_count * (sizeof(Ptr));
  gc_protect(klass);
  auto result = (StandardObject *)vm_alloc(vm, byte_count);
  gc_unprotect(klass);
  result->header.object_type = Standard_ObjectType;
  result->klass = klass;
  result->ivar_count = ivar_count;
  return result;
}

Ptr standard_object_get_ivar(StandardObject *object, u64 idx) {
  assert(idx < object->ivar_count);
  return object->ivars[idx];
}

Ptr standard_object_set_ivar(StandardObject *object, u64 idx, Ptr value) {
  assert(idx < object->ivar_count);
  return object->ivars[idx] = value;
}

Ptr make_bytecode(VM *vm, u64 code_len) {
  auto bc = alloc_bytecode(vm);
  gc_protect(bc);
  auto code = alloc_u64ao(vm, code_len);
  gc_unprotect(bc);
  bc->code = code;
  return objToPtr(bc);
}

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

Ptr make_string_with_end(VM *vm, const char* str, const char* end) {
  // die(" make string with end: ", end - str);
  ByteArrayObject *obj = alloc_bao(vm, String, end - str);
  const char *from = str;
  char *to = &(obj->data[0]);
  while(from < end) {
    *to = *from;
    to++; from++;
  }
  return objToPtr(obj);
}

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

Ptr make_symbol(VM *vm, const char* str) {
  return make_symbol(vm, str, strlen(str));
}

Ptr make_number(s64 value) { return to(Fixnum, value); }

Ptr make_zf_array(VM *vm, u64 len) {
  auto array = alloc_pao(vm, Array, len); // alloc is zf
  return objToPtr(array);
}

Ptr make_zf_struct(VM *vm, u64 len, Ptr tag) { prot_ptr(tag);
  auto array = alloc_pao(vm, Struct, len + 1); // alloc is zf
  array->data[0] = tag;
  unprot_ptr(tag);
  return objToPtr(array);
}

Ptr array_get(Ptr array, u64 index) {
  auto a = as(PtrArray, array);
  assert(index < a->length);
  return a->data[index];
}

void array_set(Ptr array, u64 index, Ptr value) {
  auto a = as(PtrArray, array);
  assert(index < a->length);
  a->data[index] = value;
}

u64 array_capacity(Ptr array) {
  auto a = as(PtrArray, array);
  return a->length;
}

// using a 2 element array to hold count and buffer for now -- could be a defstruct
Ptr make_xarray_with_capacity_and_used(VM *vm, u64 cap, u64 used) {
  assert(used <= cap);
  Ptr buffer = make_zf_array(vm, cap); prot_ptr(buffer);
  Ptr result = make_zf_array(vm, 2);
  array_set(result, 0, to(Fixnum,used));
  array_set(result, 1, buffer);
  unprot_ptr(buffer);
  return result;
}

Ptr make_xarray(VM *vm) {
  return make_xarray_with_capacity_and_used(vm, 4, 0);
}

u64 xarray_capacity(Ptr array) {
  return as(PtrArray, array_get(array, 1))->length;
}

u64 xarray_used(Ptr array) {
  return as(Fixnum, array_get(array, 0));
}

void xarray_push(VM *vm, Ptr array, Ptr item) {
  auto used = xarray_used(array);
  auto cap = xarray_capacity(array);
  if (used + 1 >= cap) {                 prot_ptrs(array, item);
    auto new_cap = cap * 2;
    auto old_arr = array_get(array, 1);  prot_ptr(old_arr);
    auto new_arr = make_zf_array(vm, new_cap);
    for (u64 i = 0; i < cap; i++) { // @speed could do a memcpy here
      array_set(new_arr, i, array_get(old_arr, i));
    }
    array_set(array, 1, new_arr);
    unprot_ptrs(array, item, old_arr);
  }
  array_set(array_get(array, 1), used, item);
  array_set(array, 0, to(Fixnum, used + 1));
}

Ptr *xarray_memory(Ptr array) {
  auto buff = as(PtrArray, array_get(array, 1));
  return buff->data;
}

Ptr xarray_at(Ptr array, u64 idx) {
  assert(idx < xarray_used(array));
  return xarray_memory(array)[idx];
}

/* ---------------------------------------- */
//          basic hashing support
//
// use adapted djb2 for strings, random for everything else
u32 djb2(unsigned char *str, u64 len) {
  u32 hash = 5381;
  u64 i = 0;

  while (i++ < len)
    hash = ((hash << 5) + hash) + (*str++); /* hash * 33 + c */

  return hash;
}

// mindlessly cribbed from https://gist.github.com/badboy/6267743
// we'll see if it works for now...
u32 hash6432shift(u64 key)
{
  key = (~key) + (key << 18); // key = (key << 18) - key - 1;
  key = key ^ (key >> 31);
  key = key * 21; // key = (key + (key << 2)) + (key << 4);
  key = key ^ (key >> 11);
  key = key + (key << 6);
  key = key ^ (key >> 22);
  return (u32 )key;
}

u32 hash_code(Ptr it) {
  if (!is(Object, it)) return hash6432shift(it.value);
  auto obj = as(Object, it);
  if (!obj->header.hashcode) {
    if (is(String, it)) {
      auto str = as(String, it);
      obj->header.hashcode = djb2((u8*)str->data, str->length);
    } else {
      obj->header.hashcode = hash6432shift(it.value);
    }
  }
  return obj->header.hashcode;
} 


/* ---------------------------------------- */
// defstruct macro
// primitive 'structures' based on arrays
// could eventually have something like CCL's gvectors
// would then support is(..) etc.
// would be nice to have parameter typechecking as well, but, well...

#define _def_struct_arg(x) Ptr x
#define _def_struct_args(...) MAP_WITH_COMMAS(_def_struct_arg, __VA_ARGS__)
#define _def_struct_set_arg(slot, name, idx) name##_set_##slot(result, slot);

#define _define_structure_maker(name, tag, ...)                         \
  Ptr alloc_##name(VM *vm) {                                            \
    auto len = PP_NARG(__VA_ARGS__);                                    \
    auto res = make_zf_struct(vm, len, to(Fixnum, tag));                \
    return res;                                                         \
  }                                                                     \
  Ptr make_##name(VM *vm, _def_struct_args(__VA_ARGS__)) {              \
    prot_ptrs(__VA_ARGS__);                                             \
    auto result = alloc_##name(vm);                                     \
    unprot_ptrs(__VA_ARGS__);                                           \
    MAP_WITH_ARG_AND_INDEX(_def_struct_set_arg, name, __VA_ARGS__ );    \
    return result;                                                      \
  }

#define _define_structure_type_test(name, tag)          \
  type_test(name, it) {                                 \
    return is(Struct, it) &&                            \
      ptr_eq(to(Fixnum, tag), array_get(it, 0));        \
  }

#define _define_structure_accessors(slot, name, idx)    \
  Ptr name##_get_##slot(Ptr obj) {                      \
    assert(is(name, obj));                              \
    return array_get(obj, idx + 1);                     \
  }                                                     \
  void name##_set_##slot(Ptr obj, Ptr value) {          \
    assert(is(name, obj));                              \
    array_set(obj, idx + 1, value);                     \
  }

#define defstruct(name, tag, ...)                                       \
  _define_structure_type_test(name, tag);                               \
  MAP_WITH_ARG_AND_INDEX(_define_structure_accessors, name, __VA_ARGS__); \
  _define_structure_maker(name, tag, __VA_ARGS__);

enum StructTag : s64 {
  StructTag_Cons,
  StructTag_VarInfo,
  StructTag_CompilerEnv,
  StructTag_HashTable,
  StructTag_End
};

/* ---------------------------------------- */

defstruct(cons, StructTag_Cons, car, cdr);
defstruct(ht, StructTag_HashTable, array);

/* ---------------------------------------- */

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

type_test(Closure, it) {
  return is(PtrArray, it) && (as(PtrArray, it))->pao_type == Closure;
}

ByteCodeObject *closure_code(Ptr closure) {
  return as(ByteCode, array_get(closure, 0));
}

Ptr closure_env(Ptr closure) {
  return array_get(closure, 1);
}

/* ---------------------------------------- */

// size of object in bytes
// note that obj_size of stack frame does not take into account temporaries.

u64 obj_size(U64ArrayObject *it)  { return sizeof(U64ArrayObject) + it->length * 8;    }
u64 obj_size(ByteCodeObject *)    { return sizeof(ByteCodeObject) + 0;                 }
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
  unused(vm);
  for (u64 i = 0; i < it->argc; i++) {
    fn(it->argv[it->pad_count + i]);
  }
  fn(it->closed_over);
  if (it->prev_frame) fn(objToPtr(it->prev_frame));
}

void obj_refs(VM *vm, ByteCodeObject *it, PtrFn fn) {
  unused(vm);
  fn(objToPtr(it->code));
  fn(objToPtr(it->literals));
}
void obj_refs(VM *vm, PtrArrayObject *it, PtrFn fn) {
  unused(vm);
  for (u64 i = 0; i < it->length; i++) {
    fn(it->data[i]);
  }
}

void obj_refs(VM *vm, StandardObject *it, PtrFn fn) {
  unused(vm);
  fn(objToPtr(it->klass));
  for (u64 i = 0; i < it->ivar_count; i++) {
    fn(it->ivars[i]);
  }
}

void map_refs(VM *vm, Ptr it, PtrFn fn) {
  if (isNil(it) || !is(Object, it)) return;
  if (is(U64Array, it))   return; // no refs
  if (is(ByteArray, it))  return; // no refs
  if (is(ByteCode, it))   return obj_refs(vm, as(ByteCode, it),   fn);
  if (is(PtrArray, it))   return obj_refs(vm, as(PtrArray, it),   fn);
  if (is(Standard, it))   return obj_refs(vm, as(Standard, it),   fn);
  if (is(StackFrame, it)) return obj_refs(vm, as(StackFrame, it), fn);
  cout << " unknown object type in map_refs" << endl;
  assert(false);
}

/* ---------------------------------------- */

enum {
  BaseClassName = 0,
  BaseClassIvarCount = 1,
  BaseClassDebugPrint = 2,
  BaseClassEnd = 3
};

typedef void(*DebugPrintFunction)(std::ostream &os, Ptr p);

DebugPrintFunction StructPrintTable[StructTag_End] = {0};

// @deprecated
#define DEBUG_PRINT_MAX 255
DebugPrintFunction DebugPrintTable[DEBUG_PRINT_MAX] = {0};
enum BuiltInDebugPrintIndex : u64 {
  DebugPrint_None,
  DebugPrint_Cons,
  DebugPrint_End
};


std::ostream &operator<<(std::ostream &os, Object *obj) {
  auto otype = obj->header.object_type;
  switch (otype) {
  case ByteArray_ObjectType: {
    ByteArrayObject *vobj = (ByteArrayObject*)(obj);
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
    case Image: {
      auto w = image_width(vobj); auto h = image_height(vobj);
      return os << "#<Image w=" << w << " h=" << h << " " << (void*)vobj << ">";
    }
    }
  }
  case PtrArray_ObjectType: {
    const PtrArrayObject *vobj = (const PtrArrayObject*)(obj);
    switch (vobj->pao_type) {
    case Struct: {
      auto index = as(Fixnum, vobj->data[0]);
      assert(index >= 0 && index < StructTag_End);
      auto fn = StructPrintTable[index];
      if (fn) { fn(os, objToPtr(obj)); }
      else { os << "#<Struct[" << index << "] " << (void *)obj << ">" ;}
      return os;
    }
    case Array:
    case Closure:
      os << "[";
      if (vobj->length > 0) {
        cout << vobj->data[0];
      }
      for (uint i = 1; i < vobj->length; i++) {
        cout << " " << vobj->data[i];
      }
      os << "]";
    }
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
  } else if (is(Point, p)) {
    auto pt = as(Point, p);
    return os << pt.x << "@" << pt.y;
  } else {
    return os << "don't know how to print ptr: " << (void *)p.value;
  }
}

void vm_dump_args(VM *vm) {
  auto f = vm->frame;
  auto c = f->argc;
  cout << " dumping args:" << endl;
  while(c--) {
    cout << "  argument: " << f->argv[f->pad_count + c] << endl;
  }
}

void _debug_walk(VM *vm, Ptr it, set<u64>*seen) {
  map_refs(vm, it, [&](Ptr p){
      if (seen->find(p.value) != seen->end()) return;
      seen->insert(p.value);
      cout << "    " << p << endl;
      _debug_walk(vm, p, seen);
    });
}

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
  ByteCodeObject *bytecode = vm->bc;
  while (fr) {
    fn(fr->closed_over);
    auto pad = fr->pad_count;
    for (u64 i = 1; i <= fr->argc; i++) {
      auto arg = fr->argv[pad + (fr->argc - i)];
      fn(arg);
    }
    fn(objToPtr(bytecode));
    auto on_stack = (Ptr*)(void *)fr;
    while (on_stack > stack) {
      on_stack--;
      fn(*on_stack);
    }
    bytecode = fr->prev_fn;
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
  }
}

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
  return Nil;
}

auto vm_print_debug_stack_trace(VM *vm) {
  StackFrameObject *fr = vm->frame;
  debug_walk(vm, objToPtr(fr));
  cout << "----------------------------------------" << endl;
  vm_map_stack_refs(vm, [&](Ptr it){
      cout << "    " << it << endl;
    });
  cout << "----------------------------------------" << endl;
  return Nil;
}

/* ---------------------------------------- */

StandardObject *make_standard_object(VM *vm, StandardObject *klass, Ptr*ivars) {
  auto ivar_count_object = standard_object_get_ivar(klass, BaseClassIvarCount);
  assert(is(Fixnum, ivar_count_object));
  auto ivar_count = as(Fixnum, ivar_count_object);
  protect_ptr_vector(ivars, ivar_count);

  auto result = alloc_standard_object(vm, klass, ivar_count);

  for (auto i = 0; i < ivar_count; i++) {
    standard_object_set_ivar(result, i, ivars[i]);
  }

  unprotect_ptr_vector(ivars);
  return result;
}

/* ---------------------------------------- */

struct Globals {
  StandardObject *Base, *Cons, *Fixnum, *Symbol;
  unordered_map<string, Ptr> *symtab;
  Ptr env;
  Ptr call1;
};

// @unsafe
auto vm_map_reachable_refs(VM *vm, PtrFn fn) {
  set<u64> seen;
  PtrFn recurse = [&](Ptr it) {
    if (!is(NonNilObject, it)) return;
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
  recurse(vm->globals->call1);
  recurse(objToPtr(vm->globals->Base));
  recurse(objToPtr(vm->globals->Cons));
  recurse(objToPtr(vm->globals->Fixnum));
  recurse(objToPtr(vm->globals->Symbol));
}

void vm_count_reachable_refs(VM *vm) {
  u64 count = 0;
  vm_map_reachable_refs(vm, [&](Ptr it){ unused(it); count++; });
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

void vm_count_objects_on_heap(VM *vm) {
  u64 count = 0;
  scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it){ unused(it); count++; });
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
  if (!is(NonNilObject, ptr)) return;
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
  if (it->code) {
    Ptr p = objToPtr(it->code);
    gc_update_ptr(vm, &p);
    it->code = as(U64Array, p);
  }
  if (it->literals) {
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
  if (it->klass){
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
  ByteCodeObject **bytecode = &vm->bc;
  while (fr) {
    gc_update_ptr(vm, &fr->closed_over);
    auto pad = fr->pad_count;
    for (u64 i = 1; i <= fr->argc; i++) {
      auto offs = pad + (fr->argc - i);
      gc_update_ptr(vm, fr->argv + offs);
    }
    {
      Ptr bc = objToPtr(*bytecode);
      gc_update_ptr(vm, &bc);
      *bytecode = as(ByteCode, bc);
    }
    auto on_stack = (Ptr*)(void *)fr;
    while (on_stack > stack) {
      on_stack--;
      gc_update_ptr(vm, on_stack);
    }
    stack = &fr->argv[fr->argc + pad];
    bytecode = &fr->prev_fn;
    fr = fr->prev_frame;
  }
}

void gc_update_base_class(VM *vm, StandardObject **it) {
  Ptr p = objToPtr(*it);
  gc_update_ptr(vm, &p);
  *it = as(Standard, p);
}

void gc_update_globals(VM *vm) {
  gc_update_ptr(vm, &vm->globals->call1);
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
  for (auto pair : *vm->gc_protected) {
    Object **ref = pair.first;
    auto obj = *ref;
    auto ptr = objToPtr(obj);
    gc_update_ptr(vm, &ptr);
    auto new_obj = as(Object, ptr);
    *ref = new_obj;
  }
  for (auto pair : *vm->gc_protected_ptrs) {
    gc_update_ptr(vm, pair.first);
  }
  for (auto pair : *vm->gc_protected_ptr_vectors) {
    auto start = pair.first;
    auto count = pair.second;
    while (count--) {
      gc_update_ptr(vm, start + count);
    }
  }
}

inline void gc_protect_reference(VM *vm, Object **ref){
  auto map = vm->gc_protected;
  auto found = map->find(ref);
  if (found == map->end()) {
    vm->gc_protected->insert(make_pair(ref, 1));
  } else {
    found->second++; 
  }
}
inline void gc_unprotect_reference(VM *vm, Object **ref){
  auto map = vm->gc_protected;
  auto found = map->find(ref);
  if (found == map->end()) {
    die("tried to protect a non-protected reference");
  } else {
    found->second--;
    if (found->second == 0) {
      map->erase(ref);
    }
  }
}

inline void gc_protect_ptr(VM *vm, Ptr *ref){
  auto map = vm->gc_protected_ptrs;
  auto found = map->find(ref);
  if (found == map->end()) {
    vm->gc_protected_ptrs->insert(make_pair(ref, 1));
  } else {
    found->second++; 
  }
}

inline void gc_unprotect_ptr(VM *vm, Ptr *ref){
  auto map = vm->gc_protected_ptrs;
  auto found = map->find(ref);
  if (found == map->end()) {
    die("tried to unprotect a non-protected reference.");
  } else {
    found->second--;
    if (found->second == 0) {
      map->erase(ref);
    }
  }
}

inline void gc_protect_ptr_vector(VM *vm, Ptr *ref, u64 count){
  auto map = vm->gc_protected_ptr_vectors;
  auto found = map->find(ref);
  if (found == map->end()) {
    map->insert(make_pair(ref, count));
  } else {
    if (found->second != count) {
      die("bad ptr vector protection");
    }
  }
}

inline void gc_unprotect_ptr_vector(VM *vm, Ptr *ref){
  vm->gc_protected_ptr_vectors->erase(ref);
}

void gc(VM *vm) {
  vm->in_gc = true;
  vm->gc_count++;

  if (GC_DEBUG) {
    vm_map_reachable_refs(vm, [&](Ptr it){
        if (is(BrokenHeart, it)) {
          die("found broken heart before gc");
        }
      });
  }

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

  vm->gc_compacted_size_in_bytes = (u64)vm->heap_end - (u64)vm->heap_mem;

  if (GC_DEBUG) {
    vm_map_reachable_refs(vm, [&](Ptr it){
        if (is(BrokenHeart, it)) {
          die("found broken heart after gc");
        }
      });
  }

  // cerr << " protected Object count : " << vm->gc_protected->size() << endl;
  // cerr << " protected ptr    count : " << vm->gc_protected_ptrs->size() << endl;

  vm->in_gc = false;
}


/* ---------------------------------------- */

auto make_base_class(VM *vm, const char* name, u64 ivar_count) {
  auto defaultPrint = Nil;
  Ptr slots[] = {make_string(vm,name), make_number(ivar_count), defaultPrint};
  return make_standard_object(vm, vm->globals->Base, slots);
}

/* ---------------------------------------- */

inline bool consp(Ptr it) {
  return is(cons, it);
}

inline Ptr car(Ptr it) {
  if (isNil(it)) return Nil;
  return cons_get_car(it);
}

inline void set_car(Ptr cons, Ptr value) {
  cons_set_car(cons, value);
}

inline Ptr cdr(Ptr it) {
  if (isNil(it)) return Nil;
  return cons_get_cdr(it);
}

inline void set_cdr(Ptr it, Ptr value) {
  cons_set_cdr(it, value);
}

Ptr nth_or_nil(Ptr it, u64 idx) {
  assert(idx >= 0);
  if (isNil(it)) return Nil;
  if (idx == 0) return car(it);
  else return nth_or_nil(cdr(it), idx - 1);
}

inline Ptr cons(VM *vm, Ptr car, Ptr cdr) {
  return make_cons(vm, car, cdr);
}

Ptr assoc(Ptr item, Ptr alist) {
  while (!isNil(alist)) {
    auto pair = car(alist);
    if (ptr_eq(car(pair), item)) return pair;
    alist = cdr(alist);
  }
  return Nil;
}

void set_assoc(VM *vm, Ptr *alistref, Ptr item, Ptr value) {
  auto existing = assoc(item, *alistref);
  if (isNil(existing)) {
    auto old = *alistref; prot_ptr(old);
    auto pair = cons(vm, item, value);
    unprot_ptr(old);
    auto newalist = cons(vm, pair, old);
    *alistref = newalist;
  } else {
    set_cdr(existing, value);
  }
}

Ptr make_list(VM *vm, u64 len, Ptr* ptrs) {
  if (len == 0) return Nil;
  // TODO: iterative solution
  return cons(vm, *ptrs, make_list(vm, len - 1, ptrs + 1));
}

void debug_print_list(ostream &os, Ptr p) {
  os << "(" << car(p);
  p = cdr(p);
  while (!isNil(p)) {
    if (consp(p)) {
      os << " " << car(p);
      p = cdr(p);
    } else {
      os << " . " << p;
      break;
    }
  }
  os << ")";
}

void do_list(VM *vm, Ptr it, PtrFn cb) { prot_ptr(it);
  while (!isNil(it)) {
    cb(car(it));
    it = cdr(it);
  }
  unprot_ptr(it);
}

u64 list_length(VM *vm, Ptr it) {
  u64 count = 0;
  do_list(vm, it, [&](Ptr p){ unused(p); count++; });
  return count;
}
/* ---------------------------------------- */

Ptr ht(VM *vm) {
  return make_ht(vm, make_xarray_with_capacity_and_used(vm, 64, 64));
}

Ptr ht_at(Ptr ht, Ptr key) {
  auto array = ht_get_array(ht);
  auto used  = xarray_used(array);
  auto mem   = xarray_memory(array);
  auto hash  = hash_code(key);
  auto idx   = hash % used;
  assert(idx < used); //:P
  if (!mem[idx].value) return Nil;
  auto cons = mem[idx];
  while (!isNil(cons)) {
    auto pair = car(cons);
    if (car(pair) == key ) return cdr(pair);
    cons = cdr(cons);
  }
  return Nil;
}

// TODO: grow table when it gets too full
void ht_at_put(VM *vm, Ptr ht, Ptr key, Ptr value) {  prot_ptrs(key, value);
  auto array = ht_get_array(ht);                      prot_ptr(array);
  auto used  = xarray_used(array);
  auto mem   = xarray_memory(array);
  auto hash  = hash_code(key);
  auto idx   = hash % used;
  assert(idx < used); //:P
  if (!mem[idx].value) { // no entry
    auto list = cons(vm, cons(vm, key, value), Nil);
    auto mem  = xarray_memory(array); // mem may have moved
    mem[idx] = list;
    unprot_ptrs(key, value, array);
    return;
  } 
  // collision
  auto entry = mem[idx];
  while (!isNil(entry)) { // existing entries
    auto pair = car(entry);
    if (car(pair) == key) {
      set_cdr(pair, value);
      unprot_ptrs(key, value, array); // found
      return;
    }
    entry = cdr(entry);
  }
  // not found
  auto list = cons(vm, cons(vm, key, value), mem[idx]);
  {
    auto mem = xarray_memory(array); // mem may have moved
    mem[idx] = list;
  }
  unprot_ptrs(key, value, array);
  return ;
}


/* ---------------------------------------- */

void initialize_struct_printers() {
  StructPrintTable[StructTag_Cons] = &debug_print_list;
}

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

Ptr intern(VM *vm, const char* cstr, int len) {
  string name = string(cstr, len);
  auto tab = vm->globals->symtab;
  if (tab->find(name) == tab->end()) {
    auto sym = make_symbol(vm, cstr, len);
    // NB: symtab may have changed if make_symbol triggered a gc
    vm->globals->symtab->insert(make_pair(name, sym));
  }
  auto res = vm->globals->symtab->find(name)->second;
  return res;
}

Ptr intern(VM *vm, string name) {
  auto str = name.c_str();
  return intern(vm, str, strlen(str));
}

Ptr set_global(VM *vm, Ptr sym, Ptr value) {
  assert(is(Symbol, sym));
  set_assoc(vm, &vm->globals->env, sym, value);
  return sym;
}

Ptr set_global(VM *vm, const char* name, Ptr value) {
  return set_global(vm, intern(vm, name), value);
}

Ptr get_global(VM *vm,  const char*name) {
  auto pair = assoc(intern(vm, name), vm->globals->env);
  if (isNil(pair)) return pair;
  return cdr(pair);
}

/* -------------------------------------------------- */

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

auto quote_form(VM *vm, Ptr it) {
  auto q = intern(vm, "quote");
  prot_ptr(q);
  auto res = cons(vm, it, Nil);
  unprot_ptr(q);
  return cons(vm, q, res);
}

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

Ptr read(VM *vm, const char **remaining, const char *end, Ptr done);

auto read_delimited_list(VM *vm, const char **remaining, const char *end, Ptr done, char delim) {
  prot_ptrs(done);
  auto input = *remaining;
  auto items = make_xarray(vm); prot_ptrs(items);

  while(input < end && *input != delim) {
    auto item = read(vm, &input, end, done);
    if(ptr_eq(item, done)) {
      vm->error = "unexpected end of input";
      unprot_ptrs(items, done);
      return done;
    }
    xarray_push(vm, items, item);
    eat_ws(&input, end);
  }
  auto used = xarray_used(items);
  auto mem  = xarray_memory(items);
  auto res  = make_list(vm, used, mem);
  if (*input == delim) input++;
  *remaining = input;
  unprot_ptrs(items, done);
  return res;
}

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
    auto res = to(Char, *input);
    input++;
    *remaining = input;
    return res;
  }
}

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
    auto res = ch == 't' ? True : False; // TODO: error if not #f
    input++;
    *remaining = input;
    return res;
  }
}

s64 read_int(VM *vm, const char **remaining, const char *end) {
  auto input = *remaining;
  s64 res = 0;
  if (input >= end) { goto error; }
  if (*input == '0') {
    input++;
    if (input >= end) return 0;
    if (*input == 'x') input++; // start reading hex
    if (input >= end) goto error;
    goto hex;
  }
  if (*input == '-') {
    input++;
    if (input >= end || !is_digitchar(*input)) { goto error; }
    res = -1 * (*input - '0');
  }
  goto ok;
 error: {
    *remaining = end;
    vm->error = "invalid integer";
    return -1;
  }
 hex: {
    while (input < end && (is_digitchar(*input) || (*input >= 'a' && *input <= 'f'))) {
      res *= 16;
      if (is_digitchar(*input)) {
        res += *input - '0';
      } else {
        res += (*input - 'a') + 10;
      }
      input++;
    }
    *remaining = input;
    return res;
 }
 ok: {
    while (input < end && is_digitchar(*input)) {
      res *= 10; res += *input - '0';
      input++;
    }
      *remaining = input;
      return res;
    }
}

Ptr read_string(VM *vm, const char **remaining, const char *end, Ptr done) {
  auto input = *remaining;
  auto start = input;
  while (*input != '"') {
    if (input >= end) {
      vm->error = "read_string: unexpected end of input";
      *remaining = input;
      return done;
    }
    input++; //TODO: handle backslash quote etc
  }
  *remaining = input;
  return make_string_with_end(vm, start, input);
}

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
    } else if (is_digitchar(*input) || *input == '-') {
      // @safe
      s64 num = read_int(vm, &input, end);
      Ptr res;
      if (input < end && *input == '@') { // read point
        input++;
        s64 num2 = read_int(vm, &input, end);
        auto pt = (point){ (s32)num, (s32)num2};
        res = to(Point, pt);
      } else {
        res = to(Fixnum, num);
      }
      *remaining = input;
      return res;
    } else if (*input == '"') {
      input++;
      auto res = read_string(vm, &input, end, done);
      input++;
      *remaining = input;
      return res;
    }
    input++;
  }
  return done;
}

Ptr read(VM *vm, const char* input) {
  auto len = strlen(input);
  return read(vm, &input, input+len, Nil);
}

Ptr read_all(VM *vm, const char* input) {
  auto done = cons(vm, Nil, Nil);            prot_ptrs(done);
  auto len = strlen(input);

  auto items = make_xarray(vm);              prot_ptrs(items);
  auto end   = input + len;
  auto item  = read(vm, &input, end, done);  prot_ptrs(item);
  while (input <= end && !ptr_eq(item, done)) {
    xarray_push(vm, items, item);
    item = read(vm, &input, end, done);
    assert(input <= end);
  }
  auto used = xarray_used(items);
  auto mem  = xarray_memory(items);
  auto res  = make_list(vm, used, mem);
  unprot_ptrs(done, items, item);
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

  // cout << "return stack frame to :" << vm->stack << endl;
}

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn, Ptr closed_over);

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn) {
  vm_push_stack_frame(vm, argc, fn, Nil);
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
  BR_IF_False = 14,
  JUMP = 15,
  STACK_RESERVE = 16,
  LOAD_FRAME_RELATIVE = 17,
  STORE_FRAME_RELATIVE = 18,
  POP_CLOSURE_ENV = 20,
};

void vm_push(VM* vm, Ptr value) {
  *(--vm->stack) = value;
}

Ptr vm_pop(VM* vm) {
  return *(vm->stack++);
}

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

inline u64 vm_curr_instr(VM *vm) {
  return vm->bc->code->data[vm->pc];
}

inline u64 vm_adv_instr(VM *vm) {
  return vm->bc->code->data[++vm->pc];
}

void vm_interp(VM* vm) {
  u64 instr;
  while ((instr = vm_curr_instr(vm))) {
    vm->instruction_count++;
    switch (instr){
    case STACK_RESERVE: {
      u64 count = vm_adv_instr(vm);
      while (count--) { vm_push(vm, Nil); }
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
      vm_push(vm, cdr(it));
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
    case BR_IF_False: {
      auto it = vm_pop(vm);
      u64 jump = vm_adv_instr(vm);
      if (ptr_eq(it, False)) {
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

class BCBuilder {
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
  Object *literals; // xarray[any]

  BCBuilder* pushOp(u8 op) {
    return pushU64(op);
  }
  BCBuilder* pushU64(u64 it) {
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
  BCBuilder* pushJumpLocation(const char* raw_name) {
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
    PtrArrayObject *array;
    {
      auto literal_count = xarray_used(objToPtr(this->literals));
      array = alloc_pao(vm, Array, literal_count);
      auto literal_mem = xarray_memory(objToPtr(this->literals));
      for (u64 i = 0; i < literal_count; i++) {
        array->data[i] = literal_mem[i];
      }
      gc_unprotect(this->literals);
      literals = 0;
    }

    gc_protect(array);
    bc = as(ByteCode, make_bytecode(vm, bc_index + 1));
    gc_unprotect(array);

    bc->literals = array;
    for (u64 i = 0; i < bc_index; i++) {
      bc->code->data[i] = bc_mem[i];
    }
    free(bc_mem);
    bc_mem = 0;
  }
public:
  BCBuilder(VM* vm) {
    this->vm = vm;

    bc_index            = 0;
    lit_index           = 0;
    bc_mem              = (u64 *)calloc(1024, 1); // TODO: realloc when needed
    labelsMap           = new map<string, u64>;
    branchLocations     = new vector<branch_entry>;
    labelContextCounter = 0;
    labelContext        = labelContextCounter;
    labelContextStack   = new vector<u64>;

    this->literals = as(Object, make_xarray(vm));
    gc_protect(this->literals);

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
    xarray_push(vm, objToPtr(this->literals), literal);
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
    pushOp(BR_IF_False);
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
      die(sym, " is not a symbol");
    }
    auto pair = assoc(sym, vm->globals->env);
    if (!consp(pair)) {
      die(sym, " is not defined in the global environment.");
    }
    pushLit(pair);
    pushOp(LOAD_GLOBAL);
    return this;
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
  ByteCodeObject *build() {
    pushOp(END);
    fixupJumpLocations();
    finalizeByteCode();
    return bc;
  }
};

/* -------------------------------------------------- */
// @deprecated
// just a wrapper for ht

Ptr make_imap(VM *vm) {
  return ht(vm);
}

bool imap_has(Ptr map, Ptr key) {
  return !isNil(ht_at(map, key));
}

Ptr imap_get(Ptr map, Ptr key) {
  return ht_at(map, key);
}

void imap_set(VM *vm, Ptr map, Ptr key, Ptr value) {
  return ht_at_put(vm, map, key, value);
}


/* -------------------------------------------------- */

#define VariableScope_Global   (Ptr){0 << TAG_BITS}
#define VariableScope_Argument (Ptr){1 << TAG_BITS}
#define VariableScope_Closure  (Ptr){2 << TAG_BITS}
#define VariableScope_Let      (Ptr){3 << TAG_BITS}

#define CompilerEnvType_Unknown (Ptr){0 << TAG_BITS}
#define CompilerEnvType_Lambda  (Ptr){1 << TAG_BITS}
#define CompilerEnvType_Let     (Ptr){2 << TAG_BITS}

defstruct(varinfo, StructTag_VarInfo,
          scope,          // VariableScope
          argument_index, // Fixnum
          closure_index); // Fixnum

struct VariableBinding {
  u64 binding_depth;
  Ptr variable_info;
};

defstruct(cenv, StructTag_CompilerEnv,
          prev,        // cenv
          info,        // imap[symbol -> varinfo]
          closed_over, // xarray[symbol]
          sub_envs,    // imap[symbol -> cenv]
          has_closure, // bool
          type);       // CompilerEnvType

Ptr cenv(VM *vm, Ptr prev) {
  prot_ptr(prev);
  auto info        = make_imap(vm);   prot_ptr(info);
  auto closed_over = make_xarray(vm); prot_ptr(closed_over);
  auto sub_envs    = make_imap(vm);   prot_ptr(sub_envs);
  auto has_closure = False;
  auto type = CompilerEnvType_Unknown;
  unprot_ptrs(prev, info, closed_over, sub_envs);
  return make_cenv(vm, prev, info, closed_over, sub_envs, has_closure, type);
}

bool cenv_has_subenv_for(Ptr cenv, Ptr key) {
  return imap_has(cenv_get_sub_envs(cenv), key);
}

Ptr cenv_get_subenv_for(Ptr cenv, Ptr key) {
  return imap_get(cenv_get_sub_envs(cenv), key);
}

void cenv_set_subenv_for(VM *vm, Ptr cenv, Ptr key, Ptr subenv) {
  imap_set(vm, cenv_get_sub_envs(cenv), key, subenv);
}

bool cenv_is_lambda(Ptr cenv) {
  if (isNil(cenv)) return false;
  return cenv_get_type(cenv) == CompilerEnvType_Lambda;
}

bool cenv_is_let(Ptr cenv) {
  if (isNil(cenv)) return false;
  return cenv_get_type(cenv) == CompilerEnvType_Let;
}

bool cenv_has_closure(Ptr cenv) {
  return as(Bool, cenv_get_has_closure(cenv));
}

auto compiler_env_get_subenv(VM *vm, Ptr env, Ptr it) { 
  if (!cenv_has_subenv_for(env, it)) {  prot_ptrs(env, it);
    auto created = cenv(vm, env);       prot_ptr(created);
    cenv_set_subenv_for(vm, env, it, created);
    unprot_ptrs(created, env, it);
    return created;
  }
  return cenv_get_subenv_for(env, it);
}

auto global_env_binding(VM *vm) {
  auto info = make_varinfo(vm, VariableScope_Global,
                           to(Fixnum,0), to(Fixnum, 0));
  return (VariableBinding){0, info}; 
}

VariableBinding compiler_env_binding(VM *vm, Ptr env, Ptr sym) {
  if (isNil(env)) return global_env_binding(vm);

  if (!imap_has(cenv_get_info(env), sym)) {
    prot_ptrs(sym, env);
    VariableBinding outer = compiler_env_binding(vm, cenv_get_prev(env), sym);
    unprot_ptrs(sym, env)
    auto from_lambda = cenv_is_lambda(cenv_get_prev(env));
    auto in_let = cenv_is_let(env);
    auto scope = varinfo_get_scope(outer.variable_info);
    // this is a pretty crude check...
    if (scope == VariableScope_Argument && from_lambda && !in_let) {
      die("variable should have been marked for closure: ", sym);
    }
    auto depth = outer.binding_depth + 1;
    auto info  = outer.variable_info;
    return (VariableBinding){depth, info};
  }
  return (VariableBinding){0, imap_get(cenv_get_info(env), sym)};
}

void emit_expr(VM *vm, BCBuilder *builder, Ptr it, Ptr env);

void emit_call(VM *vm, BCBuilder *builder, Ptr it, Ptr env) {
  prot_ptr(env);
  auto fn   = car(it); prot_ptr(fn);
  auto args = cdr(it); prot_ptr(args);
  auto argc = 0;
  do_list(vm, args, [&](Ptr arg){
      argc++;
      emit_expr(vm, builder, arg, env);
    });
  emit_expr(vm, builder, fn, env);
  builder->call(argc);
  unprot_ptrs(env, fn, args);
}

void emit_lambda_body(VM *vm, BCBuilder *builder, Ptr body, Ptr env) {
  if (isNil(body)) {
    builder->pushLit(Nil);
    return;
  }
  assert(consp(body));
  prot_ptrs(env, body);
  builder->pushLit(Nil);
  emit_expr(vm, builder, car(body), env);
  do_list(vm, cdr(body), [&](Ptr expr){
      builder->pop();
      emit_expr(vm, builder, expr, env);
    });
  unprot_ptrs(env, body);
}

auto emit_flat_lambda(VM *vm, Ptr it, Ptr env) {
  it = cdr(it);
  prot_ptrs(it, env);
  auto builder = new BCBuilder(vm);
  auto body = cdr(it);
  emit_lambda_body(vm, builder, body, env);
  builder->ret();
  auto bc = objToPtr(builder->build());
  unprot_ptrs(it, env);
  return make_closure(vm, bc, Nil);
}

void emit_lambda(VM *vm, BCBuilder *parent, Ptr it, Ptr p_env) {  prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);              prot_ptr(env);
  auto has_closure = cenv_has_closure(env);
  if (has_closure) {
    auto closed = cenv_get_closed_over(env);                      prot_ptrs(closed);
    auto closed_count = xarray_used(closed);

    // @safe
    auto builder = new BCBuilder(vm);
    for (u64 i = 0; i < closed_count; i++) {
      auto ptr     = xarray_at(closed, i);
      auto binding = compiler_env_binding(vm, env, ptr);
      auto index   = as(Fixnum, varinfo_get_argument_index(binding.variable_info));
      builder->loadArg(index);
    }
    unprot_ptrs(closed);

    // @safe
    builder->pushClosureEnv(closed_count);
    auto body = cdr(cdr(it));
    emit_lambda_body(vm, builder, body, env);
    builder->ret();
    parent->pushLit(objToPtr(builder->build()));
    parent->buildClosure();
  } else {
    // @safe
    auto closure = emit_flat_lambda(vm, it, env);
    parent->pushLit(closure);
  }
  unprot_ptrs(it, p_env, env);
}

void emit_let (VM *vm, BCBuilder *builder, Ptr it, Ptr p_env) {  prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);             prot_ptr(env);

  // @safe
  {
    auto vars        = nth_or_nil(it, 1);
    auto count       = list_length(vm, vars);
    auto start_index = builder->reserveTemps(count);

    do_list(vm, vars, [&](Ptr lst){
        auto sym = nth_or_nil(lst, 0);
        assert(is(Symbol, sym));
        auto expr = nth_or_nil(lst, 1);
        auto binding = compiler_env_binding(vm, env, sym);

        auto argidx  = varinfo_get_argument_index(binding.variable_info);
        auto idx     = as(Fixnum, argidx) + start_index;
        varinfo_set_argument_index(binding.variable_info, to(Fixnum, idx));

        emit_expr(vm, builder, expr, p_env);

        builder->storeFrameRel(idx);
      });
  }

  auto has_closure = cenv_has_closure(env);

  // @safe
  if (has_closure) {
    auto closed_over = cenv_get_closed_over(env);                prot_ptr(closed_over);
    auto closed_count = xarray_used(closed_over);

    for (u64 i = 0; i < closed_count; i++) {
      Ptr var = xarray_at(closed_over, i);
      auto binding = compiler_env_binding(vm, env, var);

      auto argidx  = varinfo_get_argument_index(binding.variable_info);
      builder->loadFrameRel(as(Fixnum, argidx));
    }
    builder->pushClosureEnv(closed_count);
    unprot_ptrs(closed_over);
  }

  // @safe
  {
    auto body = cdr(cdr(it));                            prot_ptr(body);
    builder->pushLit(Nil);
    do_list(vm, body, [&](Ptr expr){
        builder->pop();
        emit_expr(vm, builder, expr, env);
      });
    unprot_ptr(body);
  }

  if (has_closure) {
    builder->popClosureEnv();
  }

  unprot_ptrs(it, p_env, env);
}


void emit_if(VM *vm, BCBuilder *builder, Ptr it, Ptr env) {
  auto test = nth_or_nil(it, 1);
  auto _thn = nth_or_nil(it, 2);
  auto _els = nth_or_nil(it, 3);
  prot_ptrs(env, test, _thn, _els);

  builder->pushLabelContext();
  emit_expr(vm, builder, test, env);
  builder->branchIfFalse("else");
  emit_expr(vm, builder, _thn, env);
  builder->jump("endif")->label("else");
  emit_expr(vm, builder, _els, env);
  builder->label("endif");
  builder->popLabelContext();

  unprot_ptrs(env, test, _thn, _els);
}

void emit_expr(VM *vm, BCBuilder *builder, Ptr it, Ptr env) {
  if (is(Symbol, it)) { /*                                  */ prot_ptrs(it, env);
    auto binding        = compiler_env_binding(vm, env, it);
    auto info           = binding.variable_info;
    auto scope          = varinfo_get_scope(info);
    auto argument_index = varinfo_get_argument_index(info);
    auto closure_index  = varinfo_get_closure_index(info);
    if (scope == VariableScope_Global) {
      builder->loadGlobal(it);
    } else if (scope == VariableScope_Argument) {
      builder->loadArg(as(Fixnum, argument_index));
    } else if (scope == VariableScope_Closure) {
      auto index = as(Fixnum, closure_index);
      auto depth = binding.binding_depth;
      builder->loadClosure(index, depth);
    } else if (scope == VariableScope_Let) {
      // LAZY reusing argument_index
      builder->loadFrameRel(as(Fixnum, argument_index));
    } else {
      cout << "unexpected variable scope: " << scope << endl;
      assert(false);
    }
    unprot_ptrs(it, env);
  } else if (consp(it)) { // @safe
    auto fst = car(it);
    if (is(Symbol, fst)) {                                     prot_ptrs(it, env, fst);
      auto _if    = intern(vm, "if");                          prot_ptr(_if);
      auto quote  = intern(vm, "quote");                       prot_ptr(quote);
      auto lambda = intern(vm, "lambda");                      prot_ptr(lambda);
      auto let    = intern(vm, "let");
      unprot_ptrs(_if, quote, lambda, it, env, fst);
      if (ptr_eq(lambda, fst)) {
        emit_lambda(vm, builder, it, env);
        return;
      } else if (ptr_eq(quote, fst)) {
        auto item = car(cdr(it));
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
bool mark_variable_for_closure (VM *vm, Ptr sym, Ptr env, u64 level, bool saw_lambda) {
  if (isNil(env)) return false; // global scope
  auto var_map = cenv_get_info(env);
  auto exists = imap_has(var_map, sym);

  // symbol is bound at this scope.
  if (exists) {
    // symbol was found in its enclosing scope, do nothing
    if (level == 0) return false;
    // there was no lambda in the lower scopes, so do nothing.
    if (!saw_lambda) return false;

    // otherwise, was found in an outer scope, and we need to create a closure.
    auto info = imap_get(var_map, sym);
    auto scope = varinfo_get_scope(info);
    if (scope == VariableScope_Closure) return true;

    prot_ptrs(sym, env, var_map);
    varinfo_set_scope(info,  VariableScope_Closure);
    auto closed_over = cenv_get_closed_over(env);
    auto index = xarray_used(closed_over);
    varinfo_set_closure_index(info, to(Fixnum, index));
    xarray_push(vm, closed_over, sym);
    cenv_set_has_closure(env, True);
    unprot_ptrs(sym, env, var_map);

    return true;

  } else {
    prot_ptr(env);
    if (cenv_is_lambda(env)) saw_lambda = true;
    auto prev = cenv_get_prev(env);
    auto closed = mark_variable_for_closure(vm, sym, prev, level + 1, saw_lambda);
    if (closed) cenv_set_has_closure(env, True);
    unprot_ptr(env);
    return closed;
  }
}

void mark_closed_over_variables(VM *vm, Ptr it, Ptr env);

void
mark_lambda_closed_over_variables(VM *vm, Ptr it, Ptr p_env) {  prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);            prot_ptr(env);
  cenv_set_type(env, CompilerEnvType_Lambda);
  assert(cenv_is_lambda(env));

  it = cdr(it);
  auto zero = to(Fixnum, 0);
  u64 idx = 0;

  // @safe
  {
    auto args    = car(it);                                 prot_ptr(args);
    auto var_map = cenv_get_info(env);                          prot_ptr(var_map);
    do_list(vm, args, [&](Ptr arg){                             prot_ptrs(arg);
        assert(is(Symbol, arg));
        auto index = to(Fixnum, idx++);
        auto info  = make_varinfo(vm, VariableScope_Argument, index, zero);
        imap_set(vm, var_map, arg, info);
        unprot_ptrs(arg);
      });
    unprot_ptrs(args, var_map);
  }

  // @safe
  auto body = cdr(it);
  if (!isNil(body)) {
    assert(consp(body));
    do_list(vm, body, [&](Ptr expr){
        mark_closed_over_variables(vm, expr, env);
      });
  }

  unprot_ptrs(it, p_env, env);
}

void mark_let_closed_over_variables(VM *vm, Ptr it, Ptr p_env) {  prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);              prot_ptr(env);
  cenv_set_type(env, CompilerEnvType_Let);

  // @safe
  auto vars = nth_or_nil(it, 1);
  u64 idx   = 0;
  auto zero = to(Fixnum, 0);
  do_list(vm, vars, [&](Ptr lst){
      auto sym  = nth_or_nil(lst, 0);                             prot_ptr(sym);
      auto expr = nth_or_nil(lst, 1);                             prot_ptr(expr);
      assert(is(Symbol, sym));

      // idx is altered in the emit phase to account for surrounding lets
      auto index = to(Fixnum, idx);
      auto scope = VariableScope_Let;
      auto info  = make_varinfo(vm, scope, index, zero);          prot_ptrs(info);
      imap_set(vm, cenv_get_info(env), sym, info);
      mark_closed_over_variables(vm, expr, p_env);
      idx++;
      unprot_ptrs(sym, expr, info)
    });

  // @safe
  {
    auto body = cdr(cdr(it));
    do_list(vm, body, [&](Ptr expr) {
        mark_closed_over_variables(vm, expr, env);
      });
  }
  unprot_ptrs(it, p_env, env);
}

void mark_closed_over_variables(VM *vm, Ptr it, Ptr env) {  prot_ptrs(it, env);
  if (is(Symbol, it)) {
    mark_variable_for_closure(vm, it, env, 0, false);
  } else if (consp(it)) {
    auto fst    = car(it);                                  prot_ptr(fst);
    auto _if    = intern(vm, "if");                         prot_ptr(_if);
    auto quote  = intern(vm, "quote");                      prot_ptr(quote);
    auto lambda = intern(vm, "lambda");                     prot_ptr(lambda);
    auto let    = intern(vm, "let");
    auto is_sym = is(Symbol, fst);
    unprot_ptrs(fst, _if, quote, lambda);
    if (is_sym && ptr_eq(lambda, fst)) {
      mark_lambda_closed_over_variables(vm, it, env);
    } else if (is_sym && ptr_eq(quote, fst)) {
      // do nothing
    } else if (is_sym && ptr_eq(_if, fst)) {
      auto test = nth_or_nil(it, 1);                        prot_ptr(test);
      auto _thn = nth_or_nil(it, 2);                        prot_ptr(_thn);
      auto _els = nth_or_nil(it, 3);                        prot_ptr(_els);
      mark_closed_over_variables(vm, test, env);
      mark_closed_over_variables(vm, _thn, env);
      mark_closed_over_variables(vm, _els, env);
      unprot_ptrs(test, _thn, _els);
    } else if (is_sym && ptr_eq(let, fst)) {
      mark_let_closed_over_variables(vm, it, env);
    } else {
      do_list(vm, it, [&](Ptr expr){
          mark_closed_over_variables(vm, expr, env);
        });
    }
  }
  unprot_ptrs(it, env);
}

auto compile_toplevel_expression(VM *vm, Ptr it) { prot_ptr(it);
  auto env = cenv(vm, Nil);                        prot_ptr(env);
  auto builder = new BCBuilder(vm);
  mark_closed_over_variables(vm, it, env);
  emit_expr(vm, builder, it, env);
  auto result = builder->build();
  unprot_ptrs(it, env);
  return result;
}

/* -------------------------------------------------- */

Ptr primitive_print(Ptr a) { cout << a << endl; return a; }

Ptr gfx_set_pixel(VM *vm, point p) { // assumes 4 bytes pp
  auto surface = vm->surface;
  if (surface) {
    if (p.x < 0 || p.x >= surface->w) return Nil;
    if (p.y < 0 || p.y >= surface->h) return Nil;
    u32 pixel = 0;
    u8 *target_pixel = (u8 *)surface->pixels + p.y * surface->pitch + p.x * 4;
    *(u32 *)target_pixel = pixel;
  }
  return Nil;
}

Ptr gfx_fill_rect(VM *vm, point a, point b, s64 color) {
  auto surface = vm->surface;
  if (surface) {
    SDL_Rect r;
    r.x = a.x; r.y = a.y;
    r.w = b.x - a.x; r.h = b.y - a.y;
    SDL_FillRect(surface, &r, (u32)color);
  }
  return Nil;
}

#define _deg_to_rad(x) (x) * (M_PI / 180.0)

// largely from http://www.drdobbs.com/architecture-and-design/fast-bitmap-rotation-and-scaling/184416337
// and some mention from alan kay in a video about how it works
// scale and rotation are s64s becuase we don't have floats in the VM yet :P
Ptr gfx_blit_image_at(VM *vm, ByteArrayObject* img, point p, s64 scale100, s64 deg_rot) {
  if (!is(Image, objToPtr(img))) die("gfx_blit_image: not an image");

  auto surface = vm->surface;
  auto out     = surface->pixels;
  auto mem     = (u32 *)image_data(img);

  float scale = scale100 / 100.0f;
  float source_step = 1.0f / scale;

  float angle = _deg_to_rad((float)(deg_rot % 360));

  s32 sx = p.x;
  s32 sy = p.y;

  u32 scr_w = vm->surface->w,   scr_h = vm->surface->h;
  u32 img_w = image_width(img), img_h = image_height(img);

  float rvx = cosf(angle);
  float rvy = sinf(angle);

  float du_col = rvx, dv_col = rvy; 
  float du_row = -rvy, dv_row = rvx;

  float cx = img_w * 0.5f, cy = img_h * 0.5f;
  float dcx = scr_w * 0.5f, dcy = scr_h * 0.5f;

  // set the initial position by rotating the destination surface
  // I don't get why this works yet :P
  float src_x = cx - (dcx * du_row + dcy * dv_row);
  float src_y = cy - (dcy * dv_col + dcx * du_col);

  float u = src_x, v = src_y;

  float row_u = src_x, row_v = src_y;

  float iscale = 1.0 / scale;
  u32 offsx = p.x + img_w * scale * 0.5;
  u32 offsy = p.y + img_h * scale * 0.5;

  for (u32 y = 0; y < img_h; y++) {
    u = row_u; v = row_v; 
    auto dest_row = (offsy + y) * surface->pitch;

    for (u32 x = 0; x < img_w; x++) {

      if (u >= 0.0f && v >= 0.0f && u <= (float)img_w && v <= (float)img_h
          && offsx + x < scr_w && offsy + y < scr_h
          ) {

        u32 sx = floorf(u), sy = floorf(v);

        u8* under = ((u8*)out + dest_row + (offsx + x) * 4);
        u8* over  = (u8*)(mem + sy * img_w + sx);
        
        float alpha  = over[3] / 255.0f;
        float ialpha = 1.0 - alpha; 
        under[0] = over[0] * alpha + under[0] * ialpha;
        under[1] = over[1] * alpha + under[1] * ialpha;
        under[2] = over[2] * alpha + under[2] * ialpha;

      }

      u += du_col * source_step; v += dv_col * source_step;
    }
    row_u += du_row * source_step; row_v += dv_row * source_step;
  }

  return Nil;
}

Ptr gfx_blit_image(VM *vm, ByteArrayObject* img) {
  return gfx_blit_image_at(vm, img, (point){0, 0}, 100, 0);
}

#include "./primop-generated.cpp"

/* -------------------------------------------------- */

VM *vm_create() {
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

  vm->gc_protected = new unordered_map<Object **, u64>;
  vm->gc_protected_ptrs = new unordered_map<Ptr *, u64>;
  vm->gc_protected_ptr_vectors = new unordered_map<Ptr *, u64>;
  vm->gc_protected->reserve(100);
  vm->gc_protected_ptrs->reserve(100);
  vm->gc_protected_ptr_vectors->reserve(100);

  vm->in_gc = false;

  vm->frame = 0;
  vm->error = 0;

  vm->globals = (Globals *)calloc(sizeof(Globals), 1);
  vm->globals->symtab = new unordered_map<string, Ptr>;
  vm->globals->env = Nil;

  initialize_classes(vm);
  initialize_primitive_functions(vm);

  // so we have a root frame
  auto bc = (new BCBuilder(vm))->build();
  vm_push_stack_frame(vm, 0, bc);

  return vm;
}

Ptr eval(VM *vm, Ptr expr) {
  auto bc = compile_toplevel_expression(vm, expr);

  vm_push_stack_frame(vm, 0, bc);

  vm_interp(vm);
  Ptr result = vm_pop(vm);

  vm_pop_stack_frame(vm);

  if (vm->error) {
    cerr << "VM ERROR: " << vm->error << endl;
    exit(3);
  }
  return result;
}

Ptr run_string(VM *vm, const char *str) {
  auto exprs = read_all(vm, str);
  Ptr result = Nil;
  do_list(vm, exprs, [&](Ptr expr){
      result = eval(vm, expr);
    });
  return result;
}

void start_up_and_run_string(const char* str, bool soak) {
  VM *vm = vm_create();

  auto exprs = read_all(vm, str);
  auto kept_head = cons(vm, Nil, exprs); prot_ptr(kept_head);
  do {

    do_list(vm, cdr(kept_head), [&](Ptr expr){
        eval(vm, expr);
      });

    if (soak) {
      vm_count_objects_on_heap(vm);
      vm_count_reachable_refs(vm);
      cerr << " executed " << vm->instruction_count << " instructions." << endl;
      cerr << " gc count: " << vm->gc_count;
      report_memory_usage(vm);
      // this_thread::sleep_for(chrono::milliseconds(10));
      // this_thread::sleep_for(chrono::milliseconds(100));
    }

  } while(soak);

  unprot_ptr(kept_head);
  
  // TODO: clean up
}

void start_up_and_run_repl() {
  VM *vm = vm_create();
  while (true) {
    cout << "lang>";
    string input;
    getline(cin, input);
    auto result = run_string(vm, input.c_str());
    cout << result << endl;
  }
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

auto run_file(string path, bool soak_test) {
  auto contents = read_file_contents(path);
  start_up_and_run_string(contents, soak_test);
  // TODO: free contents
}

void load_file(VM *vm, const char *path) {
  run_string(vm, read_file_contents(path));
}

ByteCodeObject *build_call(VM *vm, Ptr symbol, u64 argc, Ptr argv[]) {
  prot_ptr(symbol); protect_ptr_vector(argv, argc);
  auto builder = new BCBuilder(vm);
  for (u64 i = 0; i < argc; i++) {
    builder->pushLit(argv[i]);
  }
  builder->loadGlobal(symbol);
  builder->call(argc);
  auto result = builder->build();
  unprot_ptr(symbol); unprotect_ptr_vector(argv);
  return result;
}

// use with care. assumes bc is laid out properly for patching.
void patch_bytecode_for_call(VM *vm, ByteCodeObject *bc, Ptr symbol, u64 argc, Ptr argv[]) {
    auto lits = objToPtr(bc->literals);
    for (u64 i = 0; i < argc; i++) {
      array_set(lits, i, argv[i]);
    }
    auto pair = assoc(symbol, vm->globals->env);
    if (isNil(pair)) { die("bad vm_call_global"); }
    array_set(lits, argc, pair);
}


/* should only be used as an 'entry' into the VM */
/* IOW, we don't want two of these on the stack, they will interfere */
void vm_call_global(VM *vm, Ptr symbol, u64 argc, Ptr argv[]) {
  prot_ptr(symbol); protect_ptr_vector(argv, argc);

  ByteCodeObject *bc;

  if (argc == 1) { // TODO: bc patches for more arities
    if (!is(ByteCode, vm->globals->call1)) {
      bc = build_call(vm, symbol, argc, argv);
      vm->globals->call1 = objToPtr(bc);
    } else {
      bc = as(ByteCode, vm->globals->call1);
    }
    patch_bytecode_for_call(vm, bc, symbol, argc, argv);
  } else {
    bc = build_call(vm, symbol, argc, argv);
  }

  vm_push_stack_frame(vm, 0, bc);
  vm_interp(vm);
  vm_pop_stack_frame(vm);

  unprot_ptr(symbol); unprotect_ptr_vector(argv);
}


void start_up_and_run_event_loop(const char *path) {
  auto vm = vm_create();
  load_file(vm, path);

  SDL_Window *window;

  auto x     = SDL_WINDOWPOS_CENTERED;
  auto y     = SDL_WINDOWPOS_CENTERED;
  auto w     = 640;
  auto h     = 480;
  auto title = "my window";

  SDL_Init(SDL_INIT_VIDEO);
  window = SDL_CreateWindow(title, x, y, w, h, SDL_WINDOW_SHOWN);  
  if(!window) die("could not create window");
  vm->surface = SDL_GetWindowSurface(window);
  if (!vm->surface) die("could not create surface");

  {
    //Initialize PNG loading
    int imgFlags = IMG_INIT_PNG;
    if(!(IMG_Init(imgFlags) & imgFlags)) {
      die("SDL_image could not initialize! SDL_image Error: ", IMG_GetError());
    } 
  }

  {
    auto fmt = vm->surface->format;
    SDL_FillRect(vm->surface, NULL, SDL_MapRGB(fmt, 255, 255, 255));
    auto W = to(Fixnum, w);
    auto H = to(Fixnum, h);
    vm_call_global(vm, intern(vm, "onshow"), 2, (Ptr[]){W, H});
    SDL_UpdateWindowSurface(window);
  }

  bool running = true;
  SDL_Event event;
  while (running) {
    if (SDL_WaitEvent(&event)) { //or SDL_PollEvent for continual updates
      switch (event.type) {
      case SDL_QUIT: running = false; break;
      case SDL_KEYDOWN : {
        auto key = event.key.keysym.scancode;
        Ptr num = to(Fixnum, key);
        vm_call_global(vm, intern(vm, "onkey"), 1, (Ptr[]){num});
        break;
      }
      case SDL_MOUSEMOTION: {
        auto x = event.motion.x;
        auto y = event.motion.y;
        auto p = (point){x, y};
        auto pt = to(Point, p);
        if (event.motion.state & SDL_BUTTON_LMASK) {
          vm_call_global(vm, intern(vm, "onmousedrag"), 1, (Ptr[]){pt});
        } else {
          vm_call_global(vm, intern(vm, "onmousemove"), 1, (Ptr[]){pt});
        }
        break;
      }
      case SDL_MOUSEBUTTONDOWN: {
        auto x = event.button.x;
        auto y = event.button.y;
        auto p = (point){x, y};
        auto pt = to(Point, p);
        vm_call_global(vm, intern(vm, "onmousedown"), 1, (Ptr[]){pt});
        break;
      }
      }
    }
    SDL_UpdateWindowSurface(window);
    // SDL_Delay(10);
  }

  SDL_DestroyWindow(window);
  cerr << " executed " << vm->instruction_count << " instructions." << endl;
  cerr << " gc count: " << vm->gc_count;
  report_memory_usage(vm);
  SDL_Quit();
}

/* ---------------------------------------- */

const char *require_argv_file(int argc, const char** argv) {
  if (argc > 1) {
    return argv[1];
  } else {
    cerr << "must provide a file to run" << endl;
    exit(1);
  }
}

int main(int argc, const char** argv) {

  initialize_struct_printers();

  const char *invoked = argv[0];
  const char *curr = argv[0];

  while (*curr) {
    if (*curr == '/') invoked = curr + 1;
    curr++;
  }

  // pretty hacky way of avoiding checking flags, I'll admit...
  if (strcmp(invoked, "run-file") == 0) { 
    auto file = require_argv_file(argc, argv);
    run_file(file, false);
  } else if (strcmp(invoked, "soak") == 0){
    auto file = require_argv_file(argc, argv);
    run_file(file, true);
  } else if (strcmp(invoked, "repl") == 0){
    start_up_and_run_repl();
  } else if (strcmp(invoked, "events") == 0){
    auto file = require_argv_file(argc, argv);
    start_up_and_run_event_loop(file);
  } else {
    cerr << " unrecognized invocation: " << invoked << endl;
    return 2;
  }
  return 0;
}
