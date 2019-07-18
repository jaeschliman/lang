/*

(setq flycheck-clang-language-standard "c++14")
(setq flycheck-clang-include-path '("/Users/jsn/Library/Frameworks"))
(setq flycheck-clang-args '("-F" "/Users/jsn/Library/Frameworks"))

*/

// #define NDEBUG

#include <locale.h>
#include <SDL2/SDL.h>
#include <SDL2_image/SDL_image.h>
#include <sys/stat.h>
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
#include <unistd.h> 
#include <stdio.h> 
#include <sys/socket.h> 
#include <stdlib.h> 
#include <netinet/in.h>
#include <fcntl.h>
#include <math.h>
#include "./stacktrace.h"
#include "./macro_support.h"


using std::string;

#define GC_DEBUG 0
#define PRIM_USE_GIANT_SWITCH 1
#define INCLUDE_REPL 1
#define DEBUG_IMAGE_SNAPSHOTS 0
// with it on:  216533389 / 15 / 60 = ~240,600 instr / frame
//              187983512 / 13.048 / 60 = ~240,117
// with it off: 220863576 / 16.57 / 60 = ~222,150 instr / frame
//              181965286 / 12.79 / 60 = ~237,119
// 2.5Ghz chip. 2500000000 / 1 / 60 = ~41,700,000 ops / frame!
// so roughly 170 ops / bc via wall clock.
// with it on, and NDEBUG: 179763496 / 12.201 / 60 = 245,550 ops/frame
// on a different workload (meta-1), saw over 400,000 instr / frame
// on meta1-compiler, with the GC tuned up a bit: 1276176013 / 47 / 60 = ~450,000

#define unused(x) (void)(x)
#define maybe_unused(x) (void)(x)
#define KNOWN(symbol) vm->globals->known._##symbol
#define _deg_to_rad(x) (x) * (M_PI / 180.0f)

typedef unsigned int uint;
typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;
typedef uint16_t u16;
typedef int16_t s16;
typedef uint8_t u8;
typedef int8_t s8;
typedef float f32;
typedef double f64;

// the address object references in images begin at.
// we can't reduce them down to 0 as they would be indistinguishable from Nil
#define SYSTEM_HEAP_IMAGE_OFFSET 512;

#define SYSTEM_ROOT_PACKAGE_KEY FIXNUM(0)
#define SYSTEM_CURRENT_THREAD_KEY FIXNUM(1)
#define SYSTEM_OTHER_THREADS_KEY FIXNUM(2)
#define SYSTEM_BUILTIN_CLASSES_KEY FIXNUM(3)

s64 current_time_ms() {
  auto now = std::chrono::system_clock::now();
  auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch());
  return ms.count();
}

f32 fractional_part(f32 n) {
  f32 unused = 0;
  return modf(n, &unused);
}

#define EXTRACT_PTR_MASK 0xFFFFFFFFFFFFFFF0
#define TAG_MASK 0b1111
#define TAG_BITS 4

enum PtrTag {
  Fixnum_Tag = 0b0000,
  Object_Tag = 0b0001,
  Char_Tag   = 0b0011,
  Bool_Tag   = 0b0100,
  PrimOp_Tag = 0b0101,
  Point_Tag  = 0b0110,
  Float_Tag  = 0b0111
// #define UNUSED_TAG 0b1000
// #define UNUSED_TAG 0b1001
// #define UNUSED_TAG 0b1010
// #define UNUSED_TAG 0b1011
// #define UNUSED_TAG 0b1100
// #define UNUSED_TAG 0b1101
// #define UNUSED_TAG 0b1110
// #define UNUSED_TAG 0b1111
};

struct Ptr { u64 value; };

#define True  ((Ptr){0b10100})
#define False ((Ptr){0b00100})
// do we really need nil?
#define Nil   ((Ptr){0b0001})
#define FIXNUM(x) ((Ptr){ (x) << TAG_BITS })

// ----------------------------------------
// queue for threads

struct ptrq_node { Ptr val;  ptrq_node *next;  };
struct ptrq { ptrq_node *front, *back; ptrq_node *free_list; };

ptrq_node *ptrq_get_node(ptrq *q) {
  if (q->free_list) {
    auto res = q->free_list;
    q->free_list = q->free_list->next;
    return res;
  }
  return (ptrq_node *)calloc(sizeof(ptrq_node), 1);
}

void ptrq_push(ptrq *q, ptrq_node *n) {
  n->next = 0;
  if (!q->front) {
    q->front = q->back = n;
  } else {
    q->back->next = n;
    q->back = n;
  }
}

void ptrq_push(ptrq *q, Ptr p) {
  auto n = ptrq_get_node(q);
  n->val = p;
  ptrq_push(q, n);
}

ptrq_node *ptrq_pop(ptrq *q) {
  if (q->front) {
    auto res = q->front;
    if (q->front == q->back) {
      q->front = q->back = 0;
    } else {
      q->front = q->front->next;
    }
    res->next = 0;
    return res;
  }
  return 0;
}

void ptrq_remove_all(ptrq *q) {
  while (auto n = ptrq_pop(q)) {
    n->next = q->free_list;
    q->free_list = n;
    n->val = Nil;
  }
}

void ptrq_remove_next(ptrq *q, ptrq_node *p) {
  if (!p) { // first item
    auto n = ptrq_pop(q);
    if (n) {
      n->next = q->free_list;
      q->free_list = n;
      n->val = Nil;
    }
    return;
  }
  auto n = p->next;
  assert(n);
  if (n == q->back) q->back = p;
  p->next = n->next;
  
  n->next = q->free_list;
  q->free_list = n;
  n->val = Nil;
}

void ptrq_remove_ptr(ptrq *q, Ptr ptr) {
  ptrq_node *p = 0;
  auto n = q->front;
  while (n) {
    if (n->val.value == ptr.value) {
      ptrq_remove_next(q, p);
      return;
    }
    p = n;
    n = n->next;
  }
}

// ----------------------------------------

std::ostream &operator<<(std::ostream &os, Ptr p);

inline bool operator == (Ptr a, Ptr b) {
  return a.value == b.value;
}
inline bool operator != (Ptr a, Ptr b) {
  return a.value != b.value;
}


// TODO: delete this
bool ptr_eq(Ptr a, Ptr b) {
  return a.value == b.value;
}

// ----------------------------------------

enum ObjectType : u8 {
  Broken_ObjectType = 0,
  ByteCode_ObjectType,
  ByteArray_ObjectType,
  U64Array_ObjectType,
  PtrArray_ObjectType,
  Standard_ObjectType,
  StackFrame_ObjectType,
  BrokenHeart
};


// -- lookup table for 127 builtins, (saves at least a u64 per.)
//    everything else uses second u64 = class. need a standard layout,
//    for non-compact? { header, class, u64 length, u8* mem } where mem is reinterpreted
//    maybe...
//    for now just stuff builtins as all compact... can revisit
struct Header {
  ObjectType object_type;       // 8
  u8         custom_class;      // 8
  u16        flags;             // 16
  u32        hashcode;          // 32
};

struct Object {
  Header header;
};

enum BuiltinClassIndex : u8 {
#define declare_class(name) BuiltinClassIndex_##name
#define X(...)  MAP_WITH_COMMAS(declare_class, __VA_ARGS__)
#include "./builtin-classes.include0"
  ,
#include "./builtin-classes.include1"
  ,
  BuiltinClassIndexEnd
#undef X
#undef declare_class
};

void object_set_custom_class(Object *obj, BuiltinClassIndex idx) {
  assert(idx >= 0);
  u8 uidx = idx;
  obj->header.custom_class = (1 << 7) | uidx;
}
bool object_has_custom_class(Object *obj) {
  return obj->header.custom_class >> 7;
}

#define set_obj_tag(obj,name) object_set_custom_class(obj, BuiltinClassIndex_##name)


// TODO: rename this function
inline Ptr objToPtr(Object *ref) {
  return (Ptr){ ((u64) ref) | 0b1 };
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
  bool is_varargs;
};

struct StackFrameObject : Object {
  Ptr* prev_stack;
  StackFrameObject* prev_frame;
  ByteCodeObject* prev_fn;
  u64 prev_pc;
  Ptr closed_over;
  Ptr special_variables;
  u64 special_count;
  Ptr mark;
  u64 preserved_argc; // used for snapshots;
  u64 argc;
  u64 pad_count; // must be 0 or 1
  Ptr argv[]; // MUST be trailing element
};

struct Globals;

struct point { s32 x, y; }; // really it is s30
struct rect { s64 x, y, width, height; };

inline rect points_to_rect(point upper_left, point lower_right) {
  return (rect){ upper_left.x, upper_left.y,
      lower_right.x - upper_left.x,
      lower_right.y - upper_left.y
  };
}

struct blit_surface {
  u8 *mem; s64 pitch, width, height;
};

struct VM {
  Ptr system_dictionary;
  Ptr *stack;
  Ptr *stack_end;
  s64 stack_depth;
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
  std::unordered_map<Object **, u64> *gc_protected;
  std::unordered_map<Ptr *, u64> *gc_protected_ptrs;
  std::unordered_map<Ptr *, u64> *gc_protected_ptr_vectors;
  blit_surface *surface;
  bool screen_dirty;
  ptrq *threads;
  bool suspended;
};

/* ---------------------------------------- */

typedef Ptr (*PrimitiveFunction)(VM*, u32 argc);
extern PrimitiveFunction PrimLookupTable[];

inline void *align_pointer(void *mem) {
  static_assert(sizeof(u64) == sizeof(void *), "right pointer size");
  auto bytes = (u64)mem;
  if (! (bytes & 0b1111)) return (void *)bytes;
  return (void *)((bytes & (~0b1111)) + 16);
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
#define die(...) do {                                         \
    print_stacktrace(stderr, 128);                            \
    std::cerr MAP(_ostream_prefix, __VA_ARGS__) << std::endl; \
    assert(false);                                            \
  } while(0)

#define dbg(...) std::cerr MAP(_ostream_prefix, __VA_ARGS__) << std::endl

void gc(VM *vm);

void * vm_alloc(VM *vm, u64 bytes) {
  auto preserved_heap_end = vm->heap_end;
  auto result = (void *)vm->heap_end;
  assert(pointer_is_aligned(result));
  vm->heap_end = align_pointer_with_offset(vm->heap_end, bytes);
  assert(pointer_is_aligned(vm->heap_end));
  vm->allocation_count++;

  u64 used_byte_count = vm_heap_used(vm);

  u64 threshold = vm->gc_threshold_in_bytes;

  u64 last_byte_count = vm->gc_compacted_size_in_bytes;
  u64 limit = std::min(threshold + last_byte_count, vm->heap_size_in_bytes);

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
  std::cerr << " heap used MB: " << (byte_count / (1024 * 1024));
  std::cerr << " max heap used MB: " << (max_byte_count / (1024 * 1024)) << std::endl;
}

/* -------------------------------------------------- */

typedef enum {
  String,
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

#define StandardObjectFlag_IsClass 0b1

#define _type_test_name(type) is_##type##__Impl
#define _ptr_creation_name(type) to_##type##_Ptr__Impl
#define _ptr_conversion_name(type) as##type##__Impl

// IS Ptr it of this type Ptr -> bool
#define is(type, it) _type_test_name(type)(it)
// AS this type (like a primitive cast, or conversion) Ptr -> raw type
#define as(type, it)   _ptr_conversion_name(type)(it)
#define from(type, it) _ptr_conversion_name(type)(it)
// TO the Ptr representing this type raw type -> Ptr
#define to(type, it) _ptr_creation_name(type)(it)

#define type_test(type, var) inline bool _type_test_name(type)(Ptr var)
#define create_ptr_for(type, var) inline Ptr _ptr_creation_name(type)(var)
#define unwrap_ptr_for(type, var) inline auto _ptr_conversion_name(type)(Ptr var)

#define prim_type(type) type_test(type, it){    \
    return (it.value & TAG_MASK) == type##_Tag; \
  }

unwrap_ptr_for(void, self) {
  return (void *)(self.value & EXTRACT_PTR_MASK);
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
   if (! is(type, it)) { die("unwrap of ", #type, " got ", it); } \
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

struct character { u32 code_point; };

inline u8 character_byte_at(character c, u8 idx) {
  return (u8)(c.code_point >> ((3 - idx) * 8));
}

inline s64 utf8_byte_width_for_char(u8 byte) {
  if ((byte & 0b10000000) == 0) return 1;
  if ((byte & 0b00100000) == 0) return 2;
  if ((byte & 0b00010000) == 0) return 3;
  if ((byte & 0b00001000) == 0) return 4;
  return -1;
}

s64 character_byte_width(character c) {
  auto byte = character_byte_at(c, 0);
  auto result = utf8_byte_width_for_char(byte);
  return result;
}

inline bool character_eq(character a, character b) {
  return a.code_point == b.code_point;
}

inline u32 character_to_u32(character a) {
  return character_byte_at(a, 0) |
    (character_byte_at(a, 1) << 8) |
    (character_byte_at(a, 2) << 16) |
    (character_byte_at(a, 3) << 24);
}

inline s64 character_to_s64(character a) {
  s64 result = character_to_u32(a);
  return result;
}

// FIXME is this the proper ordering?
inline bool character_lt(character a, character b) {
  return character_to_u32(a) < character_to_u32(b);
}
// FIXME is this the proper ordering?
inline bool character_gt(character a, character b) {
  return character_to_u32(a) > character_to_u32(b);
}

char *character_as_c_string(character c, char*result) {
  auto count = character_byte_width(c);
  for (auto i = 0; i < count; i++) {
    result[i] = character_byte_at(c, i);
  }
  // ((u32 *)result)[0] = c.code_point; // this may not actually work
  return result;
}

prim_type(Char)
create_ptr_for(Char, char ch) {
  auto val = ((u64)ch << 56) | Char_Tag;
  return (Ptr){val};
}
create_ptr_for(Char, character ch) {
  auto val = ((u64)ch.code_point << 32) | Char_Tag;
  return (Ptr){val};
}
unwrap_ptr_for(Char, it) {
  return (character){(u32)(it.value >> 32 )};
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

point operator +(point a, point b) { return (point){a.x + b.x, a.y + b.y}; }
point operator -(point a, point b) { return (point){a.x - b.x, a.y - b.y}; }

point rotate_point(point p, f32 degrees) {
  f32 angle = _deg_to_rad(fmod(degrees, 360.0));
  f32 c = cosf(angle);
  f32 s = sinf(angle);
  f32 x = p.x * c - p.y * s;
  f32 y = p.x * s + p.y * c;
  return (point){(s32)x, (s32)y};
}

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

prim_type(Float)
create_ptr_for(Float, f32 f) {
  u64 bits = 0;
  bits = *(u32*)&f;
  bits = bits << 32;
  return (Ptr){ bits | Float_Tag };
}
unwrap_ptr_for(Float, it) {
  u32 bits = it.value >> 32;
  f32 res = *(f32 *)&bits;
  return res;
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

#define VM_ARG(fname, type, name)                                       \
  Ptr _##name = vm_pop(vm);                                             \
  if (!is(type, _##name)) {                                             \
    vm->error = "in: " fname " argument " #name " is not a " #type;     \
    return Nil;                                                         \
  }                                                                     \
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
  set_obj_tag(obj, U64Array);
  obj->length = len;
  return obj;
}

ByteCodeObject *alloc_bytecode(VM *vm) {
  auto byte_count = sizeof(ByteCodeObject);
  ByteCodeObject *obj = (ByteCodeObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteCode_ObjectType;
  set_obj_tag(obj, ByteCode);
  return obj;
}

ByteArrayObject *alloc_bao(VM *vm, BAOType ty, uint len) {
  auto byte_count = sizeof(ByteArrayObject) + len;
  ByteArrayObject* obj = (ByteArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteArray_ObjectType;
  obj->bao_type = ty;
  obj->length = len;
  return obj;
}

type_test(String, it) {
  if (!is(ByteArray, it)) return false;
  auto bao = as(ByteArray, it);
  return bao->bao_type == String;
}
create_ptr_for(String, ByteArrayObject *it) {
  return objToPtr(it);
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
  auto result = alloc_bao(vm, Image, byte_count);
  set_obj_tag(result, Image);
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
  for (auto y = 0; y < h; y++) { // @speed could be copied by row instead of pixel
    auto stride =  y * surface->pitch;
    for (auto x = 0; x < w; x++) {
      u32 pixel = *(u32 *)((u8*)surface->pixels + stride + x * 4);
      mem[y * w + x] = pixel;
    }
  }
  SDL_FreeSurface(surface);
  return objToPtr(img);
}

Ptr gfx_load_image(VM *vm, ByteArrayObject *path) {
  if (!is(String, objToPtr(path))) die("load_image takes a string");
  auto str = string(path->data, path->length);
  auto result = load_image_from_path(vm, str);
  return result;
}
Ptr gfx_make_image(VM *vm, s64 w, s64 h) {
  return objToPtr(alloc_image(vm, w, h));
}

blit_surface image_blit_surface(ByteArrayObject *img) {
  if (!is(Image, objToPtr(img))) die("image_blit_surface requires an image");
  auto w = image_width(img), h = image_height(img);
  auto mem = image_data(img);
  auto pitch = w * 4;
  return (blit_surface){ mem, pitch, w, h};
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
create_ptr_for(Array, PtrArrayObject *it) {
  return objToPtr(it);
}
unwrap_ptr_for(Array, it) {
  return as(PtrArray, it);
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
  set_obj_tag(obj, String);
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
  set_obj_tag(obj, String);
  const char *from = str;
  char *to = &(obj->data[0]);
  while(from < end) {
    *to = *from;
    to++; from++;
  }
  return objToPtr(obj);
}

char string_byte_at(VM *vm, ByteArrayObject *str, s64 index) {
  if (index >= str->length) {
    vm->error = "string index out of range";
    return -1;
  }
  return (char)(str->data[index]);
}

character string_char_at(VM *vm, ByteArrayObject *str, s64 index) {
  if (index >= str->length) {
    vm->error = "string index out of range";
    return (character){0};
  }
  u32 code_point = 0;
  u8 *data = (u8 *)str->data + index;
  auto width = utf8_byte_width_for_char(*data);
  switch(width) {
  case 1: {
    code_point |= *data << 24;
    break;
  }
  case 2: {
    code_point |= *data << 24; data++;
    code_point |= *data << 16;
    break;
  }
  case 3: {
    code_point |= *data << 24; data++;
    code_point |= *data << 16; data++;
    code_point |= *data << 8;
    break;
  }
  case 4: {
    code_point |= *data << 24; data++;
    code_point |= *data << 16; data++;
    code_point |= *data << 8; data++;
    code_point |= *data;
    break;
  }
  }
  return (character){code_point};
}

s64 string_char_code_at(VM *vm, ByteArrayObject *str, s64 index) {
  auto ch = string_char_at(vm, str, index);
  return character_to_s64(ch);
}

Ptr string_set_char_at(VM *vm, ByteArrayObject *str, s64 index, character ch) {
  auto width = character_byte_width(ch);
  if (index > str->length + width) {
    vm->error = "string index out of range";
  } else {
    for (auto i = 0; i < width; i++) {
      str->data[index + i] = character_byte_at(ch, i);
    }
  }
  return Nil;
}

Ptr make_filled_string(VM *vm, s64 count, character ch) {
  auto width = character_byte_width(ch);
  auto size = count * width;
  auto s = alloc_bao(vm, String, size);
  // memset(s->data, ch, size);
  // TODO this is slow
  for (auto i = 0; i < size; i+=width) {
    string_set_char_at(vm, s, i, ch);
  }
  return objToPtr(s);
}

s64 string_byte_length(ByteArrayObject *str) {
  return str->length;
}

s64 string_char_count(ByteArrayObject *str){
  auto count = 0;
  auto i = 0;
  while (i < str->length) {
    u8 byte = str->data[i];
    count++;
    i += utf8_byte_width_for_char(byte);
  }
  return count;
}

inline Ptr make_number(s64 value) { return to(Fixnum, value); }

Ptr make_zf_array(VM *vm, u64 len) {
  auto array = alloc_pao(vm, Array, len); // alloc is zf
  set_obj_tag(array, Array);
  return objToPtr(array);
}


Ptr make_zf_struct(VM *vm, u64 len, Ptr tag) { prot_ptr(tag);
  auto array = alloc_pao(vm, Struct, len + 1); // alloc is zf
  array->data[0] = tag;
  unprot_ptr(tag);
  return objToPtr(array);
}

inline s64 array_length(PtrArrayObject *it) {
  return it->length;
}

Ptr array_get(Ptr array, u64 index) {
  auto a = as(PtrArray, array);
  assert(index < a->length);
  return a->data[index];
}

Ptr aget(PtrArrayObject *a, u64 index) {
  assert(index < a->length);
  return a->data[index];
}

void array_set(Ptr array, u64 index, Ptr value) {
  auto a = as(PtrArray, array);
  assert(index < a->length);
  a->data[index] = value;
}

Ptr aset(PtrArrayObject *a, u64 index, Ptr value) {
  assert(index < a->length);
  a->data[index] = value;
  return Nil;
}

u64 array_capacity(Ptr array) {
  auto a = as(PtrArray, array);
  return a->length;
}

PtrArrayObject *array_from_string(VM *vm, ByteArrayObject *str) { gc_protect(str);
  auto len = string_char_count(str); 
  auto result = make_zf_array(vm, len);
  u64 read = 0;
  auto write = 0;
  while (write < len) {
    auto ch = string_char_at(vm, str, read);
    array_set(result, write, to(Char, ch));
    read += character_byte_width(ch);
    write++;
  }
  gc_unprotect(str);
  return as(PtrArray, result);
}

ByteArrayObject *string_from_array(VM *vm, PtrArrayObject *chars) { gc_protect(chars);
  auto size = 0;
  {
    auto i = 0;
    while (i < chars->length) {
      size += character_byte_width(from(Char, chars->data[i]));
      i++;
    }
  }
  auto result = alloc_bao(vm, String, size);
  set_obj_tag(result, String);
  {
    auto array = objToPtr(chars);
    auto write = 0;
    auto read = 0;
    while (read < chars->length) {
      auto ch = from(Char, array_get(array, read));
      string_set_char_at(vm, result, write, ch);
      write += character_byte_width(ch);
      read++;
    }
  }
  gc_unprotect(chars);
  return result;
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

s64 xarray_index_of(Ptr array, Ptr item) {
  auto used = xarray_used(array);
  auto mem = xarray_memory(array);
  for (u64 i = 0; i < used; i++) {
    if (mem[i] == item) return i;
  }
  return -1;
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
  if (obj->header.hashcode == 0) {
    if (is(String, it)) {
      auto str = as(String, it);
      obj->header.hashcode = djb2((u8*)str->data, str->length);
    } else {
      obj->header.hashcode = hash6432shift(it.value) | 1;
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
    auto res = make_zf_struct(vm, len, to(Fixnum, StructTag_##tag));    \
    set_obj_tag(as(Object, res), tag);                                  \
    return res;                                                         \
  }                                                                     \
  Ptr make_##name(VM *vm, _def_struct_args(__VA_ARGS__)) {              \
    prot_ptrs(__VA_ARGS__);                                             \
    auto result = alloc_##name(vm);                                     \
    MAP_WITH_ARG_AND_INDEX(_def_struct_set_arg, name, __VA_ARGS__ );    \
    unprot_ptrs(__VA_ARGS__);                                           \
    return result;                                                      \
  }

#define _define_structure_type_test(name, tag)                  \
  type_test(name, it) {                                         \
    return is(Struct, it) &&                                    \
      ptr_eq(to(Fixnum, StructTag_##tag), array_get(it, 0));    \
  }

#define _define_structure_accessors(slot, name, idx)         \
  Ptr name##_get_##slot(Ptr obj) {                           \
    if (! is(name, obj)) { die("GOT ", #name, #slot, obj); } \
                                                             \
    assert(is(name, obj));                                   \
    return array_get(obj, idx + 1);                          \
  }                                                          \
  void name##_set_##slot(Ptr obj, Ptr value) {               \
    assert(is(name, obj));                                   \
    array_set(obj, idx + 1, value);                          \
  }

#define defstruct(name, tag, ...)                                       \
  _define_structure_type_test(name, tag);                               \
  MAP_WITH_ARG_AND_INDEX(_define_structure_accessors, name, __VA_ARGS__); \
  _define_structure_maker(name, tag, __VA_ARGS__);

enum StructTag : s64 {
  StructTag_Cons,
  StructTag_Symbol,
  StructTag_Package,
  StructTag_VarInfo,
  StructTag_CompilerEnv,
  StructTag_HashTable,
  StructTag_Continuation,
  StructTag_Thread,
  StructTag_Semaphore,
  StructTag_FileOutputStream,
  StructTag_End
};

StructTag struct_get_tag(Ptr it) {
  assert(is(Struct, it));
  return (StructTag)as(Fixnum, array_get(it, 0));
}

/* ---------------------------------------- */

defstruct(cons, Cons, car, cdr);
defstruct(Symbol, Symbol, name, package, value, flags, meta);
defstruct(package, Package, name, symtab, exports, use_list);
defstruct(ht, HashTable, array, dedupe_strings, count);
defstruct(cont, Continuation,
          stack_top,
          program_counter,
          bytecode,
          stack,
          value);
defstruct(thread, Thread,
          continuation,
          status,
          semaphore,
          wake_after,
          priority,
          local_bindings);
defstruct(semaphore, Semaphore,
          count);
defstruct(file_output_stream, FileOutputStream, fd);

auto THREAD_STATUS_RUNNING  = FIXNUM(0);
auto THREAD_STATUS_WAITING  = FIXNUM(1);
auto THREAD_STATUS_DEAD     = FIXNUM(2);
auto THREAD_STATUS_SLEEPING = FIXNUM(3);
auto THREAD_STATUS_SEM_WAIT = FIXNUM(4);

auto THREAD_PRIORITY_NORMAL  = FIXNUM(0);
auto THREAD_PRIORITY_HIGHEST = FIXNUM(100);


/* ---------------------------------------- */

auto SYMBOL_FLAG_BOUNDP = 0b1;
auto SYMBOL_FLAG_SPECIAL = 0b10;

Ptr make_symbol(VM *vm, const char* str, u64 len) {
  auto name = make_string_with_end(vm, str, str + len);
  auto package = Nil;
  return make_Symbol(vm, name, package, Nil, FIXNUM(0), Nil);
}

Ptr make_symbol(VM *vm, const char* str) {
  return make_symbol(vm, str, strlen(str));
}

Ptr make_closure(VM *vm, Ptr code, Ptr env) { prot_ptrs(code, env);
  assert(is(ByteCode, code));
  assert(isNil(env) || is(PtrArray, env));
  auto it = alloc_pao(vm, Closure, 2);
  set_obj_tag(it, Closure);
  auto c = objToPtr(it);
  array_set(c, 0, code);
  array_set(c, 1, env);
  unprot_ptrs(code, env);
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

bool file_output_stream_write_string(Ptr os, ByteArrayObject *str) {
  auto fd = from(Fixnum, file_output_stream_get_fd(os));
  assert(fd == 1 || fd == 2); // only support stdout & stderr right now
  auto file = fd == 1 ? stdout : stderr;
  auto wrote = fwrite(str->data, 1, str->length, file);
  return wrote == str->length;
}
bool file_output_stream_write_char(Ptr os, character ch) {
  auto fd = from(Fixnum, file_output_stream_get_fd(os));
  assert(fd == 1 || fd == 2); // only support stdout & stderr right now
  auto file = fd == 1 ? stdout : stderr;
  auto count = character_byte_width(ch);
  char buf[5] = {0};
  auto str = character_as_c_string(ch, (char *)&buf);
  auto wrote = fwrite(str, 1, count, file);
  return wrote == count;
}

/* ---------------------------------------- */

// size of object in bytes
// note that obj_size of stack frame does not take into account temporaries.

u64 obj_size(U64ArrayObject *it)  { return sizeof(U64ArrayObject) + it->length * 8;    }
u64 obj_size(ByteCodeObject *)    { return sizeof(ByteCodeObject) + 0;                 }
u64 obj_size(ByteArrayObject *it) { return sizeof(ByteArrayObject) + it->length;       }
u64 obj_size(PtrArrayObject *it)  { return sizeof(PtrArrayObject) + it->length * 8;    }
u64 obj_size(StandardObject *it)  { return sizeof(StandardObject) + it->ivar_count * 8;}
u64 obj_size(StackFrameObject*it) {
  return sizeof(StackFrameObject) + (it->argc + it->pad_count) * 8;
}

Object *gc_forwarding_address(Object *obj);

auto size_of(Ptr it) {
  if (isNil(it) || !is(Object, it)) return (u64)0;
  if (is(U64Array, it))   return obj_size(as(U64Array, it));
  if (is(ByteCode, it))   return obj_size(as(ByteCode, it));
  if (is(ByteArray, it))  return obj_size(as(ByteArray, it));
  if (is(PtrArray, it))   return obj_size(as(PtrArray, it));
  if (is(Standard, it))   return obj_size(as(Standard, it));
  if (is(StackFrame, it)) return obj_size(as(StackFrame, it));

  if (is(BrokenHeart, it)) {
    auto ref = gc_forwarding_address(as(Object, it));
    dbg("broken heart in size_of, ", it);
    dbg("forwarding to: ", objToPtr(ref));
  }
  die("unexpected object type in size_of: ", (as(Object, it))->header.object_type);
}
/* ---------------------------------------- */
// bang on object references
// NB. not gc safe. not anything safe.

typedef std::function<Ptr(Ptr)> BangPtrFn;
typedef std::function<void(Ptr)> PtrFn;

void bang_refs(StackFrameObject *it, BangPtrFn fn) {
  for (u64 i = 0; i < it->argc; i++) {
     it->argv[it->pad_count + i] = fn(it->argv[it->pad_count + i]);
  }
  it->special_variables = fn(it->special_variables);
  it->closed_over = fn(it->closed_over);
  it->mark = fn(it->mark);
  if (it->prev_fn) {
    it->prev_fn = (ByteCodeObject *)as(void, fn(objToPtr(it->prev_fn)));  
  }
  if (it->prev_frame) {
    it->prev_frame = (StackFrameObject *)as(void, fn(objToPtr(it->prev_frame)));
  }
}

void bang_refs(ByteCodeObject *it, BangPtrFn fn) {
  it->code = (U64ArrayObject *)as(void, fn(objToPtr(it->code)));
  it->literals = (PtrArrayObject *)as(void, fn(objToPtr(it->literals)));
}
void bang_refs(PtrArrayObject *it, BangPtrFn fn) {
  for (u64 i = 0; i < it->length; i++) {
    it->data[i] = fn(it->data[i]);
  }
}

void bang_refs(StandardObject *it, BangPtrFn fn) {
  it->klass = (StandardObject *)as(void, fn(objToPtr(it->klass)));
  for (u64 i = 0; i < it->ivar_count; i++) {
    it->ivars[i] = fn(it->ivars[i]);
  }
}

void scan_heap(void *start, void *end, PtrFn fn);

void bang_heap(void *start, void *end, BangPtrFn fn) {
  scan_heap(start, end, [&](Ptr it) {
      if (isNil(it) || !is(Object, it)) return;
      if (is(U64Array, it))   return; // no refs
      if (is(ByteArray, it))  return; // no refs
      if (is(ByteCode, it))   return bang_refs(as(ByteCode, it),   fn);
      if (is(PtrArray, it))   return bang_refs(as(PtrArray, it),   fn);
      if (is(Standard, it))   return bang_refs(as(Standard, it),   fn);
      if (is(StackFrame, it)) return bang_refs(as(StackFrame, it), fn);
      die("unkown object type in bang heap ", it);
    });
}

/* ---------------------------------------- */
// walk object references
// NB. not gc safe

// NB: cannot track items currently on the stack from this function
//     unless we do a full stack scan.
void obj_refs(StackFrameObject *it, PtrFn fn) {
  for (u64 i = 0; i < it->argc; i++) {
    fn(it->argv[it->pad_count + i]);
  }
  fn(it->special_variables);
  fn(it->closed_over);
  fn(it->mark);
  if (it->prev_fn) fn(objToPtr(it->prev_fn));
  if (it->prev_frame) fn(objToPtr(it->prev_frame));
}

void obj_refs(ByteCodeObject *it, PtrFn fn) {
  fn(objToPtr(it->code));
  fn(objToPtr(it->literals));
}
void obj_refs(PtrArrayObject *it, PtrFn fn) {
  for (u64 i = 0; i < it->length; i++) {
    fn(it->data[i]);
  }
}

void obj_refs(StandardObject *it, PtrFn fn) {
  fn(objToPtr(it->klass));
  for (u64 i = 0; i < it->ivar_count; i++) {
    fn(it->ivars[i]);
  }
}

void map_refs(Ptr it, PtrFn fn) {
  if (isNil(it) || !is(Object, it)) return;
  if (is(U64Array, it))   return; // no refs
  if (is(ByteArray, it))  return; // no refs
  if (is(ByteCode, it))   return obj_refs(as(ByteCode, it),   fn);
  if (is(PtrArray, it))   return obj_refs(as(PtrArray, it),   fn);
  if (is(Standard, it))   return obj_refs(as(Standard, it),   fn);
  if (is(StackFrame, it)) return obj_refs(as(StackFrame, it), fn);
#if GC_DEBUG
  if (is(BrokenHeart, it)) {
    dbg("broken heart in map_refs");
    return;
  }
#endif
  die("unknown object type in map_refs: " , it);
}

/* ---------------------------------------- */

enum {
  BaseClassName       = 0,
  BaseClassIvarCount  = 1,
  BaseClassMethodDict = 2,
  BaseClassMetadata   = 3,
  BaseClassApplicator = 4,
  BaseClassEnd        = 5
};

typedef void(*DebugPrintFunction)(std::ostream &os, Ptr p);

DebugPrintFunction StructPrintTable[StructTag_End] = {0};

std::unordered_map<string, char> character_codes_by_name;
std::unordered_map<char, string> character_names_by_code;

void initialize_character_names() {
#define X(code, name) character_codes_by_name[name] = code;
#include "./character-names.include"
#undef X
#define X(code, name) character_names_by_code[code] = name;
#include "./character-names.include"
#undef X
}

Ptr character_by_name(ByteArrayObject *str) {
  auto lookup = string((char *)&str->data, str->length);
  if (character_codes_by_name.find(lookup) == character_codes_by_name.end()) {
    return Nil;
  } else {
    return to(Char, character_codes_by_name[lookup]);
  }
}

bool is_object_class(StandardObject *obj);

std::ostream &operator<<(std::ostream &os, Object *obj) {
  auto otype = obj->header.object_type;
  switch (otype) {
  case Broken_ObjectType: {
    return os << "#<Broken Object Header>";
  }
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
        std::cout << vobj->data[0];
      }
      for (uint i = 1; i < vobj->length; i++) {
        std::cout << " " << vobj->data[i];
      }
      os << "]";
    }
    return os;
  }
  case Standard_ObjectType: {
    auto sobj = (StandardObject *)obj;
    auto klass = sobj->klass;
    auto is_class = is_object_class(sobj);
    if (is_class) {
      auto name = standard_object_get_ivar(sobj, BaseClassName);
      std::cout << "#<Class " << as(Object, name) << " " << (void*)obj << ">";
    } else {
      auto name = standard_object_get_ivar(klass, BaseClassName);
      std::cout << "#<A " << as(Object, name) << " " << (void*)obj << ">";
    }
    return os;
  }
  case ByteCode_ObjectType: {
    std::cout << "#<ByteCode " << (void*)obj << ">";
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
  return os << "don't know how to print object: " << otype << (void*)obj << std::endl;
}

std::ostream &operator<<(std::ostream &os, Ptr it) {
  PtrTag tag = (PtrTag)(it.value & TAG_MASK);
  switch (tag) {
  case Fixnum_Tag: return os << as(Fixnum, it);
  case Object_Tag: {
    if (isNil(it)) return os << "nil";
    return os << as(Object, it);
  }
  case Char_Tag  : {
    auto ch = as(Char, it);
    if (character_byte_width(ch) == 1) {
      auto byte = character_byte_at(ch, 0);
      return os << "#\\" << character_names_by_code[byte];
    } else {
      char buf[5] = {0};
      auto str = character_as_c_string(ch, (char *)&buf);
      os << "#\\" << str;
      return os;
    }
  }
  case Bool_Tag  : return os << (as(Bool, it) ? "#t" : "#f");
  case PrimOp_Tag: return os << "#<PrimOp " << (it.value >> 32) << ">";
  case Point_Tag : {
    auto pt = as(Point, it);
    return os << pt.x << "@" << pt.y;
  }
  case Float_Tag : return os << std::showpoint << as(Float, it);
  }
}

void vm_dump_args(VM *vm) {
  auto f = vm->frame;
  auto c = f->argc;
  dbg(" dumping args: ", c, " pad count =", f->pad_count, " pac =", f->preserved_argc);
  while(c--) {
    dbg("  argument: ", f->argv[f->pad_count + c]);
  }
}

void _debug_walk(VM *vm, Ptr it, std::set<u64>*seen) {
  map_refs(it, [&](Ptr p){
      if (seen->find(p.value) != seen->end()) return;
      seen->insert(p.value);
      std::cout << "    " << p << std::endl;
      _debug_walk(vm, p, seen);
    });
}

void debug_walk(VM *vm, Ptr it) {
  std::set<u64> seen;
  std::cout << "DEBUG WALK::" << std::endl;
  // TODO: print out `it` as well :P
  _debug_walk(vm, it, &seen);
  std::cout << "========================================" << std::endl << std::endl;
}

// @unsafe
auto vm_map_stack_refs(VM *vm, PtrFn fn) {
  StackFrameObject *fr = vm->frame;
  Ptr *stack = vm->stack;
  ByteCodeObject *bytecode = vm->bc;
  while (fr) {
    fn(fr->special_variables);
    fn(fr->closed_over);
    fn(fr->mark);
    auto pad = fr->pad_count;
    for (u64 i = 0; i < fr->argc; i++) {
      auto arg = fr->argv[pad + i];
      fn(arg);
    }
    if (bytecode) fn(objToPtr(bytecode));
    auto on_stack = (Ptr*)(void *)fr; // go back 'up' the stack to get current args
    while (on_stack > stack) {
      on_stack--;
      fn(*on_stack);
    }
    bytecode = fr->prev_fn;
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
  }
}

auto vm_print_stack_trace(VM *vm) { return Nil;
  StackFrameObject *fr = vm->frame;
  Ptr *stack = vm->stack;
  dbg("PRINTING STACKTRACE: ", vm->stack_depth);
  s64 count = 0;
  while (fr) {
    assert(isNil(fr->closed_over) || is(PtrArray,fr->closed_over));
    dbg("FRAME: ", fr, count++);
    dbg("  closure:", fr->closed_over);
    dbg("  mark:", fr->mark);
    dbg("  aligned by: ", fr->pad_count);
    auto pad = fr->pad_count;
    for (u64 i = 1; i <= fr->argc; i++) {
      dbg("    arg", (i - 1), " = ", fr->argv[pad + (fr->argc - i)]);
    }
    auto h = (u64)fr - (u64)stack;
    dbg("  stack height (bytes):", h);
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
  }
  return Nil;
};

void vm_debug_print_stack_top(VM *vm) {
  auto bottom = vm->stack;
  auto top = (Ptr *)(void *)vm->frame;
  dbg(" printing stack top:");
  for (auto it = top - 1; it >= bottom; it--) {
    dbg("     : ", *it);
  }
  dbg(" done.");
}

void vm_debug_print_stackframe_args(VM *vm, StackFrameObject *fr) {
  maybe_unused(vm);
  dbg(" printing stack frame args: arg=", fr->argc, " pac =", fr->preserved_argc);
    for (u64 i = 0; i < fr->argc; i++) {
      auto it = fr->argv[i + fr->pad_count];
      dbg("     : ", it);
    }
  dbg(" done.");
}



Ptr vm_print_debug_stack_trace(VM *vm) {
  StackFrameObject *fr = vm->frame;
  debug_walk(vm, objToPtr(fr));
  std::cerr << "----------------------------------------" << std::endl;
  vm_map_stack_refs(vm, [&](Ptr it){
      std::cerr << "    " << it << std::endl;
    });
  std::cerr << "----------------------------------------" << std::endl;
  return Nil;
}

/* ---------------------------------------- */

StandardObject *make_standard_object(VM *vm, StandardObject *klass, Ptr*ivars) {
  auto ivar_count_object = standard_object_get_ivar(klass, BaseClassIvarCount);
  assert(is(Fixnum, ivar_count_object));
  auto ivar_count = as(Fixnum, ivar_count_object);

  if (ivars) {
    protect_ptr_vector(ivars, ivar_count);
  }

  auto result = alloc_standard_object(vm, klass, ivar_count);

  if (ivars) {
    for (auto i = 0; i < ivar_count; i++) {
      standard_object_set_ivar(result, i, ivars[i]);
    }
    unprotect_ptr_vector(ivars);
  }

  return result;
}

/* ---------------------------------------- */


struct Globals {
  struct {

#define make_class(name) *_##name
#define X(...) MAP_WITH_COMMAS(make_class, __VA_ARGS__)
    StandardObject
#include "./primitive-classes.include"
    ;
#undef X
#undef make_class

    StandardObject *builtins[127];

  } classes;
  Ptr root_package;
  Ptr call1;
  struct {
    Ptr _lambda, _quote, _if, _let, _fixnum, _cons, _string, _array, _character, _boolean, _quasiquote, _unquote, _unquote_splicing, _compiler, _set_bang, _exception, _run_string, _with_special_binding, _XpackageX;
  } known;

  Ptr current_thread;
};

/* ---------------------------------------- */
// @cleanup this function shows that the object model is too complex IMO

Ptr class_of(VM *vm, Ptr it) {
#define builtin(name) objToPtr(vm->globals->classes._##name)
#define builtin_case(type, name) case type##_Tag: return builtin(name)
  PtrTag ptr_tag = (PtrTag)(it.value & TAG_MASK);
  switch (ptr_tag) {
    builtin_case(Fixnum, Fixnum);
    builtin_case(Char, Character);
    builtin_case(Bool, Bool);
    builtin_case(PrimOp, PrimOp);
    builtin_case(Point, Point);
    builtin_case(Float, Float);
  case Object_Tag: {
    if (is(Object, it)) { // could be Nil
      auto obj = as(Object, it);
      if (obj->header.custom_class) {
        u8 idx     = obj->header.custom_class & 0b01111111;
        auto klass = vm->globals->classes.builtins[idx];
        return objToPtr(klass);
      } else {
        return objToPtr(as(Standard, it)->klass);
      }
    } else {
      return builtin(Null);
    }
  }
  }
#undef builtin_case
#undef builtin
}

/* ---------------------------------------- */
auto _debug_heap_walk_refs(void * start, void *end, PtrFn fn) {
  scan_heap(start, end, [&](Ptr it) {
      return map_refs(it, fn);
    });
}

auto _debug_heap_report(void *start, void *end, s64 delta = 0) {
  auto count = 0;
  auto ref_count = 0;
  auto fn = [&](Ptr it) {
    if (it == Nil || !is(Object, it)) return;
    auto ptr = as(Object, it);
    auto ptr_val = (u64)ptr;
    auto where = ptr_val + delta;
    ref_count++;
    assert((void*)where >= start && (void*)where < end);
  };
  scan_heap(start, end, [&](Ptr it) {
      count++;
      return map_refs(it, fn);
    });
  dbg(count, " objects on heap, ", ref_count, " in-heap references ");
}

// @unsafe
auto vm_map_reachable_refs(VM *vm, PtrFn fn) {
  std::set<u64> seen;
  PtrFn recurse = [&](Ptr it) {
    if (!is(NonNilObject, it)) return;
    if (seen.find(it.value) != seen.end()) return;
    seen.insert(it.value);
    fn(it);
    map_refs(it, recurse);
  };
  vm_map_stack_refs(vm, recurse);
  recurse(vm->globals->root_package);
  recurse(vm->globals->call1);

#define handle_class(name) recurse(objToPtr(vm->globals->classes._##name));
#define X(...) MAP(handle_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X
#undef handle_class

  for (auto i = 0; i < BuiltinClassIndexEnd; i++) {
    recurse(objToPtr(vm->globals->classes.builtins[i]));
  }

}

void vm_count_reachable_refs(VM *vm) {
  u64 count = 0;
  vm_map_reachable_refs(vm, [&](Ptr it){ unused(it); count++; });
  std::cout << "  " << count << "  reachable objects." << std::endl;
}

// @unsafe
void scan_heap(void *start, void *end, PtrFn fn) {
  while(start < end) {
    assert(pointer_is_aligned(start));
    // we know it is an object because primitive values are stored inline in fields
    auto it = objToPtr((Object *)start);
    auto offset = size_of(it);
    if (offset == 0) {
      die("error while scanning heap");
    }
    fn(it);
    start = align_pointer_with_offset(start, offset);
  }
}

void vm_count_objects_on_heap(VM *vm) {
  u64 count = 0;
  scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it){ unused(it); count++; });
  std::cout << " counted " << count << " objects on heap. " << std::endl;
  std::cout << " allocation count is : " << vm->allocation_count << std::endl;
}


/* ---------------------------------------- */
/*             -- gc support --             */

void gc_prepare_vm(VM *vm) {
  // swap the heaps
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
  ((u64*)obj)[1] = (u64)forwarding_address;
}

Object *gc_forwarding_address(Object *obj) {
  assert(gc_is_broken_heart(obj));
  auto ptr = ((u64 *)obj) + 1;
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

bool gc_ptr_is_in_to_space(VM *vm, Object *ptr) {
  return ptr >= vm->heap_mem && ptr < vm->heap_end;
}

void gc_update_ptr(VM *vm, Ptr *p) {
  auto ptr = *p;
  if (!is(NonNilObject, ptr)) return;
  auto obj = as(Object, ptr);
  // assert(!gc_ptr_is_in_to_space(vm, obj));
  if (!gc_is_broken_heart(obj)) {
    gc_move_object(vm, obj);
  }
  auto new_addr = gc_forwarding_address(obj);
  // assert(gc_ptr_is_in_to_space(vm, new_addr));
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

void gc_update(VM *vm, StackFrameObject *it) {
  for (u64 i = 0; i < it->argc; i++) {
    gc_update_ptr(vm, it->argv + it->pad_count + i);
  }
  gc_update_ptr(vm, &it->special_variables);
  gc_update_ptr(vm, &it->closed_over);
  gc_update_ptr(vm, &it->mark);
  if (it->prev_frame) {
    Ptr p = objToPtr(it->prev_frame);
    gc_update_ptr(vm, &p);
    it->prev_frame = as(StackFrame, p);
  }
  if (it->prev_fn) {
    Ptr p = objToPtr(it->prev_fn);
    gc_update_ptr(vm, &p);
    it->prev_fn = as(ByteCode, p);
  }
}

void gc_update_copied_object(VM *vm, Ptr it) {
  assert(is(Object, it));
  if (is(ByteCode, it)) return gc_update(vm, as(ByteCode, it));
  if (is(PtrArray, it)) return gc_update(vm, as(PtrArray, it));
  if (is(Standard, it)) return gc_update(vm, as(Standard, it));
  if (is(StackFrame, it)) return gc_update(vm, as(StackFrame, it));
}

void gc_update_stack(VM *vm) {
  StackFrameObject *fr = vm->frame;
  Ptr *stack = vm->stack;
  ByteCodeObject **bytecode = &vm->bc;
  u64 count = 0;
  while (fr) {
    gc_update_ptr(vm, &fr->special_variables);
    gc_update_ptr(vm, &fr->closed_over);
    gc_update_ptr(vm, &fr->mark);
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
    count++;
  }
}

void gc_update_base_class(VM *vm, StandardObject **it) {
  Ptr p = objToPtr(*it);
  gc_update_ptr(vm, &p);
  *it = as(Standard, p);
}


#define handle_class(name) gc_update_base_class(vm, &vm->globals->classes._##name);
#define update_sym(n) gc_update_ptr(vm, &vm->globals->known._##n); 
#define update_symbols(...) MAP(update_sym, __VA_ARGS__)

void gc_update_globals(VM *vm) {
  gc_update_ptr(vm, &vm->globals->call1);
  gc_update_ptr(vm, &vm->globals->root_package);
  gc_update_ptr(vm, &vm->globals->current_thread);

#define X(...) MAP(handle_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X

  for (auto i = 0; i < BuiltinClassIndexEnd; i++) {
    gc_update_base_class(vm, vm->globals->classes.builtins + i);
  }

  update_symbols(lambda, quote, let, if, fixnum, cons, string);
  update_symbols(array, character, boolean, quasiquote, unquote, unquote_splicing);
  update_symbols(compiler, set_bang, exception, run_string, with_special_binding);
  update_symbols(XpackageX);

}

#undef update_sym
#undef update_symbols
#undef handle_class

void gc_copy_threads(VM *vm) {
  auto n = vm->threads->front;
  while(n) {
    gc_update_ptr(vm, &n->val);
    n = n->next;
  }
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
    s64  count = pair.second;
    for (s64 i = 0; i < count; i++) {
      gc_update_ptr(vm, start + i);
    }
  }
}

Ptr im_offset_ptr(Ptr it, s64 delta) {
  if (it == Nil || !is(Object, it)) return it;
  auto result_value = (it.value & EXTRACT_PTR_MASK) + delta;
  auto result = (Ptr){result_value | Object_Tag};
  #if 0
  assert((result.value & EXTRACT_PTR_MASK) - delta == (it.value & EXTRACT_PTR_MASK));
  if (is(U64Array, it))   assert(is(U64Array, result));
  if (is(ByteArray, it))  assert(is(ByteArray,result));
  if (is(ByteCode, it))   assert(is(ByteCode, result));
  if (is(PtrArray, it))   assert(is(PtrArray, result));
  if (is(Standard, it))   assert(is(Standard, result));
  if (is(StackFrame, it)) assert(is(StackFrame, result));
  #endif
  return result;
}

void im_offset_protected_references(VM *vm, s64 delta) {
  for (auto pair : *vm->gc_protected) {
    Object **ref = pair.first;
    auto obj = *ref;
    auto ptr = im_offset_ptr(objToPtr(obj), delta);
    auto new_obj = as(Object, ptr);
    *ref = new_obj;
  }
  for (auto pair : *vm->gc_protected_ptrs) {
    *pair.first = im_offset_ptr(*pair.first, delta);
  }
  for (auto pair : *vm->gc_protected_ptr_vectors) {
    auto start = pair.first;
    s64  count = pair.second;
    for (s64 i = 0; i < count; i++) {
      start[i] = im_offset_ptr(start[i], delta);
    }
  }
}

inline void gc_protect_reference(VM *vm, Object **ref){
  auto map = vm->gc_protected;
  auto found = map->find(ref);
  if (found == map->end()) {
    vm->gc_protected->insert(std::make_pair(ref, 1));
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
    vm->gc_protected_ptrs->insert(std::make_pair(ref, 1));
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
    map->insert(std::make_pair(ref, count));
  } else {
    if (found->second != count) {
      die("bad ptr vector protection");
    }
  }
}

inline void gc_unprotect_ptr_vector(VM *vm, Ptr *ref){
  vm->gc_protected_ptr_vectors->erase(ref);
}

void im_move_heap(VM *vm);

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

  // set start ptr
  auto start = vm->heap_end;

  // system dictionary should always be first item on the heap
  gc_update_ptr(vm, &vm->system_dictionary);

  // update stack. (loop through stack and gc_copy_object on-stack and args etc.)
  gc_update_stack(vm);
  // update global refs
  gc_update_globals(vm);
  // update protected refs
  gc_update_protected_references(vm);
  // copy threads
  gc_copy_threads(vm);

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
    auto frame_count = 0;
    vm_map_reachable_refs(vm, [&](Ptr it){
        if (is(BrokenHeart, it)) {
          die("found broken heart after gc");
        }
        if (is(StackFrame, it)) {
          frame_count++;
        }
      });
    dbg(frame_count, " stack frames in the heap after GC");
  }


  memset(vm->alt_heap_mem, 0, vm->heap_size_in_bytes);
  // cerr << " protected Object count : " << vm->gc_protected->size() << endl;
  // cerr << " protected ptr    count : " << vm->gc_protected_ptrs->size() << endl;

  vm->in_gc = false;

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

Ptr make_list(VM *vm, u64 len, Ptr* ptrs) { protect_ptr_vector(ptrs, len);
  Ptr result = Nil;                         prot_ptr(result);
  u64 i = len;
  while (i--) {
    result = cons(vm, ptrs[i], result);
  }
  unprot_ptr(result);
  unprotect_ptr_vector(ptrs);
  return result;
}

Ptr make_list_rev(VM *vm, u64 len, Ptr* ptrs) { protect_ptr_vector(ptrs, len);
  Ptr result = Nil;                             prot_ptr(result);
  for (u64 i = 0; i < len; i++) {
    result = cons(vm, ptrs[i], result);
  }
  unprot_ptr(result);
  unprotect_ptr_vector(ptrs);
  return result;
}

void debug_print_list(std::ostream &os, Ptr p) {
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

void debug_print_symbol(std::ostream &os, Ptr p) {
  auto s = as(String, Symbol_get_name(p));
  for (uint i = 0; i < s->length; i++) {
    os << s->data[i];
  }
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
  return make_ht(vm, make_xarray_with_capacity_and_used(vm, 64, 64), False, FIXNUM(0));
}
Ptr string_table(VM *vm) {
  return make_ht(vm, make_xarray_with_capacity_and_used(vm, 64, 64), True, FIXNUM(0));
}

Ptr ht_at(Ptr ht, Ptr key) {
  auto strs  = ht_get_dedupe_strings(ht) == True && is(String, key); 
  auto array = ht_get_array(ht);
  auto used  = xarray_used(array);
  auto mem   = xarray_memory(array);
  auto hash  = hash_code(key);
  auto idx   = hash % used;
  assert(idx < used); //:P
  if (!mem[idx].value) return Nil;
  auto cons = mem[idx];

  if (strs) {
    while (!isNil(cons)) {
      auto pair = car(cons);
      auto it = car(pair);
      if (is(String, it) && hash_code(it) == hash) return cdr(pair);
      else if (it == key) return cdr(pair);
      cons = cdr(cons);
    }
  } else {
    while (!isNil(cons)) {
      auto pair = car(cons);
      if (car(pair) == key) return cdr(pair);
      cons = cdr(cons);
    }
  }
  return Nil;
}

Ptr ht_listed_entries(VM *vm, Ptr ht) { prot_ptr(ht);
  auto result = Nil;
  auto array = ht_get_array(ht);        prot_ptr(array); 
  auto count = xarray_used(array);
  for (auto i = 0; i < count; i++) {
    auto it = xarray_at(array, i);
    if (it.value) {
      result = cons(vm, it, result);
    }
  }
  unprot_ptrs(ht, array);
  return result;
}

Ptr ht_at_put(VM *vm, Ptr ht, Ptr key, Ptr value);

void ht_grow(VM *vm, Ptr ht) {              prot_ptr(ht);
  auto entries = ht_listed_entries(vm, ht); prot_ptr(entries);
  auto array = ht_get_array(ht); 
  auto new_size = xarray_capacity(array) * 2;
  auto new_storage = make_xarray_with_capacity_and_used(vm, new_size, new_size);
  ht_set_array(ht, new_storage);
  do_list(vm, entries, [&](Ptr assoc_list) {
      do_list(vm, assoc_list, [&](Ptr pair) {
          ht_at_put(vm, ht, car(pair), cdr(pair));
        });
    });
  unprot_ptrs(ht, entries);
}

// TODO: grow table when it gets too full
Ptr ht_at_put(VM *vm, Ptr ht, Ptr key, Ptr value) { prot_ptrs(key, value);
  auto count = from(Fixnum, ht_get_count(ht));
  auto strs  = ht_get_dedupe_strings(ht) == True && is(String, key); 
  auto array = ht_get_array(ht); 
  auto load_factor = 0.8; // just a guess, can be tweaked

  u64 new_size = count + 1;
  if (new_size >= xarray_capacity(array) * load_factor) {
    prot_ptr(ht);
    ht_grow(vm, ht);
    unprot_ptrs(ht, key, value);
    return ht_at_put(vm, ht, key, value);
  }

  prot_ptr(array);

  auto used  = xarray_used(array);
  auto mem   = xarray_memory(array);
  auto hash  = hash_code(key);
  auto idx   = hash % used;

  assert(idx < used); //:P
  if (!mem[idx].value) { // no entry
    prot_ptr(ht);
    auto list = cons(vm, cons(vm, key, value), Nil);
    auto mem  = xarray_memory(array); // mem may have moved
    mem[idx] = list;
    unprot_ptrs(ht, key, value, array);
    ht_set_count(ht, to(Fixnum, count + 1));
    return Nil;
  }

  // collision
  auto entry = mem[idx];
  if (strs) {
    while (!isNil(entry)) { // existing entries
      auto pair = car(entry);
      auto it = car(pair);
      if ((is(String, it) && hash_code(it) == hash) || it == key) { // found 
        set_cdr(pair, value);
        unprot_ptrs(key, value, array);
        return Nil;
      }
      entry = cdr(entry);
    }
  } else {
    while (!isNil(entry)) { // existing entries
      auto pair = car(entry);
      if (car(pair) == key) { // found
        set_cdr(pair, value);
        unprot_ptrs(key, value, array);
        return Nil;
      }
      entry = cdr(entry);
    }
  }

  // not found
  prot_ptr(ht);
  auto list = cons(vm, cons(vm, key, value), mem[idx]);
  {
    auto mem = xarray_memory(array); // mem may have moved
    mem[idx] = list;
  }
  unprot_ptrs(ht, key, value, array);
  ht_set_count(ht, to(Fixnum, count + 1));
  return Nil;
}


/* ---------------------------------------- */


void initialize_struct_printers() {
  StructPrintTable[StructTag_Cons] = &debug_print_list;
  StructPrintTable[StructTag_Symbol] = &debug_print_symbol;
}

/* ---------------------------------------- */
Ptr get_symbol_value(VM *vm, Ptr sym);

Ptr package_lookup_string(Ptr pkg, Ptr str) {
  auto exist = ht_at(package_get_symtab(pkg), str);
  auto curr  = package_get_use_list(pkg);
  while (exist == Nil && curr != Nil) {
    auto pkg = car(curr);
    auto found = ht_at(package_get_symtab(pkg), str);
    if (found != Nil) {
      exist = ht_at(package_get_exports(pkg), found);
    }
    curr = cdr(curr);
  }
  return exist;
}

inline Ptr get_current_package(VM *vm) {
  return get_symbol_value(vm, KNOWN(XpackageX));
}

Ptr package_extern_symbol(VM *vm, Ptr pkg, Ptr sym) {
  ht_at_put(vm, package_get_exports(pkg), sym, sym);
  return Nil;
}

Ptr make_user_package(VM *vm, Ptr name) {                   prot_ptr(name);
  auto symtab = string_table(vm);                           prot_ptr(symtab);
  auto exports = ht(vm);                                    prot_ptr(exports);
  auto use_list = cons(vm, vm->globals->root_package, Nil); prot_ptr(use_list);
  auto res = make_package(vm, name, symtab, exports, use_list);
  unprot_ptrs(name, symtab, exports, use_list);
  return res;
}

Ptr intern(VM *vm, const char* cstr, int len, Ptr pkg) {
  auto name = make_string_with_end(vm, cstr, cstr+len);
  if (pkg == Nil) return make_Symbol(vm, name, Nil, Nil, FIXNUM(0), Nil);
  auto tab = package_get_symtab(pkg); 
  auto exist = package_lookup_string(pkg, name);
  if (exist == Nil) {                                         prot_ptrs(tab, name);
    exist = make_Symbol(vm, name, vm->globals->root_package,
                        Nil, FIXNUM(0), Nil);
    prot_ptr(exist);
    ht_at_put(vm, tab, name, exist); 
    unprot_ptrs(tab, name, exist);
  }
  return exist;
}

Ptr intern(VM *vm, ByteArrayObject *str, Ptr pkg) {
  return intern(vm, str->data, str->length, pkg);
}

Ptr intern(VM *vm, string name, Ptr pkg) {
  auto str = name.c_str();
  return intern(vm, str, strlen(str), pkg);
}

Ptr root_intern(VM *vm, string name) {
  return intern(vm, name, vm->globals->root_package);
}

bool is_special_symbol(VM *vm, Ptr sym) {
  maybe_unused(vm);
  return from(Fixnum, Symbol_get_flags(sym)) & SYMBOL_FLAG_SPECIAL;
}

Ptr mark_symbol_as_special(VM *vm, Ptr sym) {
  maybe_unused(vm);
  auto flags = from(Fixnum, Symbol_get_flags(sym)) | SYMBOL_FLAG_SPECIAL;
  Symbol_set_flags(sym, to(Fixnum, flags));
  return Nil;
}

// @unsafe
#define _init_sym(n) globals->known._##n = root_intern(vm, #n);
#define _init_symbols(...) MAP(_init_sym, __VA_ARGS__)

void initialize_known_symbols(VM *vm) {

  auto globals = vm->globals;
  _init_symbols(lambda, quote, let, if, fixnum, cons, string, array, character, boolean, quasiquote, unquote, compiler, exception);
  globals->known._unquote_splicing = root_intern(vm, "unquote-splicing");
  globals->known._set_bang = root_intern(vm, "set!");
  globals->known._run_string = root_intern(vm, "run-string");
  globals->known._with_special_binding = root_intern(vm, "with-special-binding");
  globals->known._XpackageX = root_intern(vm, "*package*");

}
#undef _init_sym
#undef _init_symbols

/* ---------------------------------------- */

bool is_object_class(StandardObject *obj) {
  return obj->header.flags & StandardObjectFlag_IsClass;
}

Ptr mark_object_as_class(StandardObject *obj) {
  obj->header.flags = obj->header.flags | StandardObjectFlag_IsClass;
  return Nil;
}

// @unsafe
auto make_base_class(VM *vm, const char* name) {
  Ptr slots[] = {make_string(vm,name), make_number(0), ht(vm), ht(vm), Nil};
  auto base = vm->globals->classes.builtins[BuiltinClassIndex_Base];
  auto result = make_standard_object(vm, base, slots);
  mark_object_as_class(result);
  return result;
}

// @unsafe
void initialize_classes(VM *vm)
{
  auto Base = alloc_standard_object(vm, 0, BaseClassEnd);
  Base->klass = Base;
  standard_object_set_ivar(Base, BaseClassName, make_string(vm, "Base"));
  standard_object_set_ivar(Base, BaseClassIvarCount, make_number(BaseClassEnd));
  standard_object_set_ivar(Base, BaseClassMethodDict, ht(vm));
  standard_object_set_ivar(Base, BaseClassMetadata, ht(vm));
  standard_object_set_ivar(Base, BaseClassApplicator, Nil);
  mark_object_as_class(Base);
  vm->globals->classes.builtins[BuiltinClassIndex_Base] = Base;

#define make_class(name) if (!builtin(name)) builtin(name) = make_base_class(vm, #name);
#define builtin(name) vm->globals->classes.builtins[BuiltinClassIndex_##name]
#define X(...) MAP(make_class, __VA_ARGS__)
#include "./builtin-classes.include0"
#include "./builtin-classes.include1"
#undef X
#undef builtin
#define builtin(name) vm->globals->classes._##name
#define X(...) MAP(make_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X
#undef builtin
#undef make_class
#undef builtin

}

// TODO: won't handle changes in number of builtins
Ptr _built_in_classes_as_array(VM *vm) {
  auto result = make_xarray(vm);
  for (auto i = 0; i < BuiltinClassIndexEnd; i++) {
    auto it = objToPtr(vm->globals->classes.builtins[i]); 
    xarray_push(vm, result, it);
  }
#define save_class(name) xarray_push(vm, result, objToPtr(builtin(name)));
#define builtin(name) vm->globals->classes._##name
#define X(...) MAP(save_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X
#undef builtin
#undef save_class
  return result;
}

void _built_in_classes_restore_from_xarray(VM *vm, Ptr it) {
  for (auto i = 0; i < BuiltinClassIndexEnd; i++) {
    vm->globals->classes.builtins[i] = as(Standard, xarray_at(it, i));
  }
  int idx = BuiltinClassIndexEnd;
#define save_class(name) { builtin(name) = as(Standard, xarray_at(it, idx)); idx++;}
#define builtin(name) vm->globals->classes._##name
#define X(...) MAP(save_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X
#undef builtin
#undef save_class
}





bool is_class(Ptr obj) {
  if (is(Standard, obj)) return is_object_class(as(Standard, obj));
  return false;
}


Ptr make_user_class(VM *vm, Ptr name, s64 ivar) { prot_ptrs(name);
  auto ivar_ct = to(Fixnum, ivar);
  auto method_dict = ht(vm);                      prot_ptr(method_dict);
  auto metadata = ht(vm);
  auto superclass = vm->globals->classes.builtins[BuiltinClassIndex_Base];
  Ptr slots[] = {name, ivar_ct, method_dict, metadata, Nil};
  auto result = make_standard_object(vm, superclass, slots);
  unprot_ptrs(name, method_dict);
  mark_object_as_class(result);
  return objToPtr(result);
}

Ptr instantiate_user_class(VM *vm, StandardObject *klass) {
  return objToPtr(make_standard_object(vm, klass, 0));
}

Ptr class_get_metadata(StandardObject *klass, Ptr key) {
  auto meta = standard_object_get_ivar(klass, BaseClassMetadata);
  return ht_at(meta, key);
}

Ptr class_set_metadata(VM *vm, StandardObject *klass, Ptr key, Ptr value) {
  auto meta = standard_object_get_ivar(klass, BaseClassMetadata);
  ht_at_put(vm, meta, key, value);
  return Nil;
}

Ptr class_set_applicator(StandardObject *klass, Ptr fn) {
  standard_object_set_ivar(klass, BaseClassApplicator, fn);
  return Nil;
}

Ptr set_global(VM *vm, Ptr sym, Ptr value) {
  maybe_unused(vm);
  assert(is(Symbol, sym));
  Symbol_set_value(sym, value);
  auto flags = from(Fixnum, Symbol_get_flags(sym)) | SYMBOL_FLAG_BOUNDP;
  Symbol_set_flags(sym, to(Fixnum, flags));
  return sym;
}

Ptr set_global(VM *vm, const char* name, Ptr value) { prot_ptr(value);
  auto result = set_global(vm, root_intern(vm, name), value);
  unprot_ptr(value);
  return result;
}

bool boundp(VM *vm, Ptr sym) {
  maybe_unused(vm);
  return as(Fixnum, Symbol_get_flags(sym)) & SYMBOL_FLAG_BOUNDP;
}

Ptr get_global(VM *vm,  Ptr sym) {
  if (boundp(vm, sym)) return Symbol_get_value(sym);
  vm->error = "symbol is unbound";
  return Nil;
}

Ptr get_global(VM *vm,  const char*name) {
  return get_global(vm, root_intern(vm, name));
}

inline Ptr get_special_binding(VM *vm, Ptr sym) {
  return assoc(sym, vm->frame->special_variables);
}

Ptr set_special(VM *vm, Ptr sym, Ptr value) {
  auto pair = get_special_binding(vm, sym);
  if (pair == Nil) set_global(vm, sym, value);
  else set_cdr(pair, value);
  return sym;
}

Ptr get_special(VM *vm, Ptr sym) {
  auto pair = get_special_binding(vm, sym);
  if (pair == Nil) return get_global(vm, sym);
  else return cdr(pair);
}

Ptr set_symbol_value(VM *vm, Ptr sym, Ptr value) {
  assert(is(Symbol, sym));
  if (is_special_symbol(vm, sym)) return set_special(vm, sym, value); 
  else return set_global(vm, sym, value);
}

Ptr get_symbol_value(VM *vm, Ptr sym) {
  assert(is(Symbol, sym));
  if (is_special_symbol(vm, sym)) return get_special(vm, sym); 
  else return get_global(vm, sym);
}

inline void _thread_local_binding_push(VM *vm, Ptr sym, Ptr val) {
  auto assoc = cons(vm, sym, val);
  auto exist = vm->frame->special_variables;
  auto update = cons(vm, assoc, exist);
  vm->frame->special_variables = update;
  vm->frame->special_count++;
}

inline void _thread_local_binding_pop(VM *vm) {
  auto exist = vm->frame->special_variables;
  auto update = cdr(exist);
  vm->frame->special_variables = update;
  vm->frame->special_count--;
}

/* -------------------------------------------------- */

void initialize_global_variables(VM *vm) {
  set_symbol_value(vm, KNOWN(XpackageX), vm->globals->root_package);
  mark_symbol_as_special(vm, KNOWN(XpackageX));
  auto _stdout = root_intern(vm, "*standard-output*");
  set_symbol_value(vm, _stdout, make_file_output_stream(vm, FIXNUM(1)));
  mark_symbol_as_special(vm, _stdout);
  auto _stderr = root_intern(vm, "*standard-error*");
  set_symbol_value(vm, _stderr, make_file_output_stream(vm, FIXNUM(2)));
  mark_symbol_as_special(vm, _stderr);
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
auto is_alphachar(char ch) {
  u8 idx = ch;
  return character_table[idx] & character_alpha;
}
auto is_nlchar(char ch) {
  return ch == 10 || ch == 13;
}

/* -------------------------------------------------- */

auto quote_form(VM *vm, Ptr it) {
  auto cdr = cons(vm, it, Nil);
  return cons(vm, KNOWN(quote), cdr);
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
  auto items = make_xarray(vm);  prot_ptrs(items);

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
  // N.B. need to unprotect items as we are taking reference to its
  //      and protecting within make_list 
  auto mem  = xarray_memory(items); unprot_ptrs(items);
  auto res  = make_list(vm, used, mem);
  if (*input == delim) input++;
  *remaining = input;
  unprot_ptrs(done);
  return res;
}

Ptr read_character(VM *vm, const char **remaining, const char *end, Ptr done) {
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
    auto start = input;
    if (*input <= 32 || *input > 126) { // TODO: handle backslash
      vm->error = "unrecognized character";
      *remaining = input;
      return done;
    }
    input++;
    if (input >= end) goto error;
    while (input < end && is_alphachar(*input)) {
      input++;
    }
    Ptr res = done;
    auto lookup = string(start, input-start);
    if (character_codes_by_name.find(lookup) == character_codes_by_name.end()) {
      vm->error = "unrecognized character";
    } else {
      res = to(Char, character_codes_by_name[lookup]);
    }
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
    input++;
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
    auto sign = res < 0 ? -1 : 1;
    while (input < end && is_digitchar(*input)) {
      res *= 10;
      res += (*input - '0') * sign;
      input++;
    }
    *remaining = input;
    return res;
  }
}

bool reader_scan_for_float(const char *input, const char *end) {
  while (input + 1 < end && is_digitchar(*input)) {
    if (*(input + 1) == '.') return true;
    input++;
  }
  return false;
}

// TODO: this is probably losing precision all over the place.
f32 read_float(VM *vm, const char **remaining, const char *end) {
  auto *input = *remaining;
  s64 res = 0, sign = 1.0;
  if (*input == '-') { sign = -1; input++; }
  while (input < end && is_digitchar(*input)) {
    res *= 10; res += *input - '0';
    input++;
  }
  s64 divisor = 1;
  input++; // skip dot
  while (input < end && is_digitchar(*input)) {
    res *= 10; res += *input - '0';
    input++; divisor *= 10;
  }
  double mult = 1.0;
  if (input < end && *input == 'e') {  // handle exponent
    input++;
    if (input >= end) {
      vm->error = "invalid float syntax.";
      *remaining = input;
      return -1;
    }
    bool invert = *input == '-';
    input++;
    if (input >= end) {
      vm->error = "invalid float syntax.";
      *remaining = input;
      return -1;
    }
    auto exp = read_int(vm, &input, end);
    mult = pow(10.0, exp);
    if (invert) mult = 1.0 / mult;
  }
  *remaining = input;
  double as_signed = res * sign;
  double divided   = as_signed / (double)divisor;
  double result = divided * mult;
  return (f32)result;
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
  auto pkg = get_current_package(vm);
  while (input < end) {
    eat_ws(&input, end);
    if (input >= end) break;
    if (is_digitchar(*input) || (*input == '-' && is_digitchar(*(input + 1)))) {
      auto scan_from = *input == '-' ? input + 1 : input;
      if (reader_scan_for_float(scan_from, end)) {
        auto res = to(Float, read_float(vm, &input, end));
        *remaining = input;
        return res;
      } else {
        // read int or point
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
      }
    } else if (is_symchar(*input)) {
      const char* start = input;
      int len = 1;
      while(input < end && is_symbodychar(*(++input))) {
       len++;
      }
      auto result = intern(vm, start, len, pkg);
      *remaining = input;
      return result;
    } else if (*input == '\'') {
      input++;
      auto result = quote_form(vm, read(vm, &input, end, done));
      *remaining = input;
      return result;
    } else if (*input == '(') {
      input++;
      auto res = read_delimited_list(vm, &input, end, done, ')');
      *remaining = input;
      return res;
    } else if (*input == '#') {
      auto ch = *(++input);
      Ptr res;
      if (ch == '\\') {
        res = read_character(vm, &input, end, done);
      } else {
        res = read_bool(vm, &input, end, done);
      }
      *remaining = input;
      return res;
    } else if (*input == '"') {
      input++;
      auto res = read_string(vm, &input, end, done);
      input++;
      *remaining = input;
      return res;
    } else if (*input == '`') {
      input++;
      auto it     = read(vm, &input, end, done); //FIXME: check for `done`
      auto result = cons(vm, KNOWN(quasiquote), cons(vm, it, Nil));
      *remaining = input;
      return result;
    } else if (*input == ',') {
      input++;
      auto prefix = KNOWN(unquote); prot_ptrs(prefix, done);
      if (*input == '@') {
        input++;
        if (input >= end) {
          vm->error = "unexpected end of input";
          unprot_ptrs(prefix, done);
          *remaining = input;
          return done;
        }
        prefix = KNOWN(unquote_splicing);
      }
      auto it = read(vm, &input, end, done);
      if (it == done) {
        *remaining = input;
        unprot_ptrs(prefix, done);
        return it; }
      auto result = cons(vm, prefix, cons(vm, it, Nil));
      unprot_ptrs(prefix, done);
      *remaining = input;
      return result;
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
Ptr vm_pop(VM *vm);
void vm_push(VM *vm, Ptr it);

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
  vm->stack_depth--;

  // dbg("--- popped stack frame.");
  // vm_dump_args(vm);

}

void _vm_reset_stack_from_root_frame(VM *vm) {
  auto fr = vm->frame;
  vm->bc = 0;
  vm->pc = 0;
  vm->stack = fr->prev_stack + fr->argc;
  vm->frame = 0;
  vm->stack_depth--;
}

void vm_prepare_for_tail_call(VM *vm, s64 argc) {
  // argc + 1 to account for the function, which is also on the stack.
  Ptr args[argc + 1];
  for (auto i = 0; i < argc + 1; i++) { //@speed could just 'move' these
    args[i] = vm_pop(vm);
  }
  vm_pop_stack_frame(vm);
  for (auto i = argc; i >= 0; i--) {
    vm_push(vm, args[i]);
  }
}

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn, Ptr closed_over);

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn) {
  vm_push_stack_frame(vm, argc, fn, Nil);
};

#define STACK_PADDING 0ULL

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn, Ptr closed_over) {

  uint offset = (sizeof(StackFrameObject) / sizeof(u64));

  u64 *top = &((vm->stack - offset)->value);
  u64 padding = ((u64)top & TAG_MASK) ? 1 + STACK_PADDING : STACK_PADDING;
  top -= padding;
  assert(((u64)top & TAG_MASK) == 0);

  if ((Ptr *)top < vm->stack_end) die("stack overflow.");

  StackFrameObject *new_frame = (StackFrameObject *)top;
  new_frame->header.object_type = StackFrame_ObjectType;
  set_obj_tag(new_frame, StackFrame);
  new_frame->pad_count = padding;

  new_frame->closed_over = closed_over;
  new_frame->mark = Nil;
  new_frame->prev_stack = vm->stack;
  new_frame->prev_frame = vm->frame;
  new_frame->prev_fn = vm->bc;
  new_frame->prev_pc = vm->pc;
  new_frame->argc = argc;
  if (vm->frame) {
    new_frame->special_variables = vm->frame->special_variables;
  } else {
    new_frame->special_variables = thread_get_local_bindings(vm->globals->current_thread);
  }
  new_frame->special_count = 0;
  vm->stack = (Ptr*)(void *)new_frame; // - 100; // STACK_PADDING
  vm->frame = new_frame;
  vm->bc = fn;
  vm->pc = 0;
  vm->stack_depth++;
}


Ptr vm_set_stack_mark(VM *vm, Ptr mark) {
  vm->frame->mark = mark;
  return Nil;
}

// count of items beyond argv until next frame
s64 vm_stack_frame_additional_item_count(StackFrameObject *fr) {
  if (!fr->prev_frame) return 0;
  Ptr *end_of_argv = fr->argv + fr->pad_count + fr->argc;
  Ptr *pf = (Ptr *)(void *)fr->prev_frame;
  auto result = pf - end_of_argv;
  if (result > 1000) die("unexpected added count:", result, pf, end_of_argv);
  return result;
}

typedef std::function<bool(StackFrameObject*)> StackPred;

void _debug_validate_stack_frame(StackFrameObject *fr){
  while (fr) {
    auto f = objToPtr(fr);
    if (!is(StackFrame, f)) die("expecting stack frame got: ", f);
    fr = fr->prev_frame;
  }
}

void _debug_validate_stack(VM *vm) {
  StackFrameObject *fr = vm->frame;
  _debug_validate_stack_frame(fr);
}

void _debug_validate_thread(Ptr thread) {
  assert(is(thread, thread));
  auto cont = thread_get_continuation(thread);
  assert(is(cont, cont));
  auto stack = cont_get_stack(cont);
  assert(is(StackFrame, stack));
  _debug_validate_stack_frame(as(StackFrame, stack));
}

void _assert_ptr_in_heap(void *start, void *end, Ptr it) {
  if (it == Nil || ! is(Object, it)) return;
  auto addr = as(Object, it);
  assert(addr >= start && addr < end);
}

void _debug_validate_thread_in_heap(void *start, void *end, Ptr thread){
  _assert_ptr_in_heap(start, end, thread);
  assert(is(thread, thread));
  auto cont = thread_get_continuation(thread);
  assert(is(cont, cont));
  _assert_ptr_in_heap(start, end, cont);
  auto stack = cont_get_stack(cont);
  assert(is(StackFrame, stack));
  _assert_ptr_in_heap(start, end, stack);
  auto fr = as(StackFrame, stack);
  auto depth = 0;
  while (fr) {
    assert(fr >= start && fr < end);
    fr = fr->prev_frame;
    depth++;
  }
}

Ptr vm_snapshot_stack_with_predicate(VM *vm, StackPred fn) {
  StackFrameObject *fr = vm->frame;
  Ptr result  = Nil;                               prot_ptr(result);
  result = alloc_cont(vm);
  cont_set_program_counter(result, to(Fixnum, vm->pc));
  cont_set_bytecode(result, objToPtr(vm->bc));
  {
    Ptr *stack = vm->stack;
    auto on_stack = (Ptr*)(void *)vm->frame; // go back 'up' the stack to get current args
    auto count = on_stack - stack;
    auto objs  = make_zf_array(vm, count);
    for (auto i = 0; i < count; i++) {
      array_set(objs, i, stack[i]);
      // dbg("saving stack object: ", stack[i]);
    }
    cont_set_stack_top(result, objs);
  }

  Ptr top_fr  = Nil;                               prot_ptr(top_fr);
  Ptr prev_fr = Nil;                               prot_ptr(prev_fr);

  while (fr && !fn(fr)) {
    #if DEBUG_IMAGE_SNAPSHOTS
    {
     auto f = objToPtr(fr);
     assert(is(StackFrame, f));
    }
    #endif
    auto is_root_frame = fr->prev_frame && fn(fr->prev_frame);

    auto added = vm_stack_frame_additional_item_count(fr);
    if (is_root_frame) added = 0; // we don't need the additional previous stack
    auto byte_count = obj_size(fr) + added * 8;

    auto nf = (StackFrameObject *)vm_alloc(vm, byte_count);
    memcpy(nf, fr, byte_count);

    // this is a bit of a hack...
    nf->argc = fr->argc + added;
    nf->preserved_argc = fr->argc;
    nf->prev_stack = 0;
    nf->prev_frame = 0;

    if (is_root_frame) {
      nf->prev_fn = 0;
    }

    // if there was a frame above us, point it at us
    if (prev_fr != Nil) {
      auto pf = as(StackFrame, prev_fr);
      pf->prev_frame = nf; 
    }

    prev_fr = objToPtr(nf);
    if (top_fr == Nil) top_fr = prev_fr;
    fr = fr->prev_frame;
  }

  cont_set_stack(result, top_fr);
  unprot_ptrs(result, top_fr, prev_fr);
  return result;
}

Ptr vm_snapshot_stack_to_mark(VM *vm, Ptr mark) { prot_ptr(mark);
  auto result = vm_snapshot_stack_with_predicate(vm,[&](StackFrameObject *fr){
      return ptr_eq(fr->mark, mark); });
  unprot_ptr(mark);
  return result;
}

void vm_unwind_to_mark(VM *vm, Ptr mark) {
  while (vm->frame && !ptr_eq(vm->frame->mark, mark)) {
    vm_pop_stack_frame(vm); 
    if (vm->error) return;
  }
}

void vm_unwind_to_predicate(VM *vm, StackPred fn) {
  while (vm->frame && !fn(vm->frame)) {
    vm_pop_stack_frame(vm); 
    if (vm->error) return;
  }
}

Ptr vm_abort_to_mark(VM *vm, Ptr mark, Ptr value) { prot_ptrs(mark, value);
  Ptr cont = vm_snapshot_stack_to_mark(vm, mark);
  cont_set_value(cont, value);
  vm_unwind_to_mark(vm, mark);
  unprot_ptrs(mark, value);
  return cont;
}

bool vm_handle_error(VM *vm) {
  Ptr ex = make_string(vm, vm->error); // TODO: signal better errors than just strings
  vm->error = 0;
  vm_unwind_to_mark(vm, KNOWN(exception));
  vm_push(vm, ex);
  if (!vm->frame->prev_frame) {
    vm_pop(vm);
    dbg("killing thread: ", vm->globals->current_thread, " due to uncaught exception ", ex);
    thread_set_status(vm->globals->current_thread, THREAD_STATUS_DEAD);
    return false;
  }
  return true;
}

Ptr rebase_alist(VM *vm, Ptr lst, Ptr root, s64 count) {
  if (!count) return root;
  prot_ptrs(lst, root);
  auto pair = car(lst);
  auto copy = cons(vm, car(pair), cdr(pair));
  auto prev = rebase_alist(vm, cdr(lst), root, count - 1);
  auto result = cons(vm, copy, prev);
  unprot_ptrs(lst, root);
  return result;
}

void _vm_restore_special_variables_snapshot(VM *vm, StackFrameObject *base_frame) {
  if (!base_frame || !base_frame->prev_frame) return;
  std::vector<StackFrameObject *> frames;
  auto fr = vm->frame;
  while (fr && fr != base_frame) {
    frames.push_back(fr);
    fr = fr->prev_frame;
  }
  for (s64 i = frames.size() - 1; i >= 0; i--) {
    auto fr = frames.at(i);
    fr->special_variables = rebase_alist(vm,
                                         fr->special_variables,
                                         fr->prev_frame->special_variables,
                                         fr->special_count);
  }
}

void _vm_copy_stack_snapshot_args_to_stack(VM *vm, StackFrameObject *fr) {
  for (s64 i = fr->argc - 1; i >= 0; i--) {
    auto it = fr->argv[i + fr->pad_count];
    vm_push(vm, it);
  }
}

void _vm_restore_stack_snapshot(VM *vm, StackFrameObject *fr) {
  if (!fr) return;
  #if DEBUG_IMAGE_SNAPSHOTS
  {
    Ptr it = objToPtr(fr);
    assert(is(StackFrame, it));
  }
  #endif

  // first restore previous frame
  _vm_restore_stack_snapshot(vm, fr->prev_frame);

  //restore previous frame stack top
  _vm_copy_stack_snapshot_args_to_stack(vm, fr);

  // alloc and align new frame;
  auto top = vm->stack - (sizeof(StackFrameObject) / sizeof(u64));
  auto was_aligned = true;
  if (!pointer_is_aligned(top)) {
    top -= 1;
    was_aligned = false;
  }
  assert(pointer_is_aligned(top));

  auto new_frame = (StackFrameObject *)(void *)top;
  memcpy(new_frame, fr, sizeof(StackFrameObject));

  new_frame->prev_stack = vm->stack;
  new_frame->pad_count = was_aligned ? 0 : 1;
  new_frame->argc = new_frame->preserved_argc;

  // hook in the tail frame
  if (!new_frame->prev_frame) {
    new_frame->prev_fn = vm->bc;
    new_frame->prev_pc = vm->pc;
  } else {
    // do nothing
  }

  #if DEBUG_IMAGE_SNAPSHOTS
  {
    Ptr it = objToPtr(vm->frame);
    assert(is(StackFrame, it));
  }
  #endif

  new_frame->prev_frame = vm->frame;

  // update the stack etc
  vm->stack = (Ptr *)(void *)new_frame;
  vm->frame = new_frame;
  vm->stack_depth++;

}

void vm_restore_stack_snapshot(VM *vm, Ptr cont) {
  Ptr extra_args = cont_get_stack_top(cont);
  Ptr frame      = cont_get_stack(cont);
  Ptr pc         = cont_get_program_counter(cont);
  Ptr bc         = cont_get_bytecode(cont);

  auto base_frame = vm->frame;

  // restore frames
  _vm_restore_stack_snapshot(vm, as(StackFrame, frame));

  // retore stack top
  auto args = as(PtrArray, extra_args);

  if (args->length > 0) {
    for (s64 i = args->length - 1; i >= 0; i--) {
      vm_push(vm, args->data[i]);
    }
  }

  // restore bc and pc
  vm->bc = as(ByteCode, bc);
  vm->pc = as(Fixnum, pc);

  // restore special variables (must go last as it may cons)
  _vm_restore_special_variables_snapshot(vm, base_frame);
}

Ptr vm_resume_stack_snapshot(VM *vm, Ptr cont, Ptr arg) { prot_ptr(arg);
  vm_restore_stack_snapshot(vm, cont);
  unprot_ptr(arg);
  return arg;
}

Ptr signal_semaphore(Ptr a) {
  assert(is(semaphore, a));
  auto ct = as(Fixnum, semaphore_get_count(a));
  semaphore_set_count(a, to(Fixnum, ct + 1));
  return Nil;
}

/* returns time in ms, or -1 or -2 */
/* -1 = no threads found */
/* -2 = all threads in sem_wait */
s64 _vm_threads_get_minimum_sleep_time(VM *vm) {
  auto curr = vm->threads->front;
  if (!curr) return -1; // -1 signals we should terminate -- no threads
  auto now = current_time_ms();
  auto best = -2; // -2 signals we are waiting for a signal (or deadlocked)
  while (curr) {
    auto thread = curr->val;
    auto status = thread_get_status(thread);
    if (status == THREAD_STATUS_WAITING) return 0;
    if (status == THREAD_STATUS_SLEEPING) {
      auto wake_after = thread_get_wake_after(thread);
      s64 delta = as(Fixnum, wake_after) - now;
      if (delta > 0) {
        if (best < 0) best = delta;
        else best = best < delta ? best : delta;
      } else {
        return 0;
      }
    }
    curr = curr->next;
  }
  return best;
}

// FIXME: doesn't yet respect thread priority
Ptr _vm_maybe_get_next_available_thread(VM *vm) {
  if (vm->threads->front) {
    s64 now = current_time_ms();
    ptrq_node *p = 0;
    auto n = vm->threads->front;
    while (n) {
      auto thread = n->val;
      auto status = thread_get_status(thread);
      if (status == THREAD_STATUS_WAITING) {
        // dbg("found waiting thread");
        ptrq_remove_next(vm->threads, p);
        return thread;
      } else if (status == THREAD_STATUS_SLEEPING) {
        auto wake_after = thread_get_wake_after(thread);
        s64 delta = as(Fixnum, wake_after) - now;
        // if (delta > 0) dbg("thread still sleeping for ", delta, " ms", thread);
        if (delta <= 0) {
          // dbg("waking sleeping thread");
          ptrq_remove_next(vm->threads, p);
          return thread;
        }
      } else if (status == THREAD_STATUS_SEM_WAIT) {
        auto sem = thread_get_semaphore(thread);
        auto count = as(Fixnum, semaphore_get_count(sem));
        if (count > 0) {
          // dbg("waking on semaphore, count was: ", count);
          semaphore_set_count(sem, to(Fixnum, count - 1));
          ptrq_remove_next(vm->threads, p);
          return thread;
        }
      } else if (status == THREAD_STATUS_RUNNING) {
        // should not get here
        dbg("somehow wound up with running thread in scheduled?");
      }
      p = n;
      n = n->next;
    }
  }
  return Nil;
}

Ptr _list_rev(Ptr lst) {
  if (lst == Nil) return Nil;
  auto prev = lst;
  auto curr = cdr(lst);
  set_cdr(prev, Nil);
  while (curr != Nil) {
    auto next = cdr(curr);
    set_cdr(curr, prev);
    prev = curr;
    curr = next;
  }
  return prev;
}

Ptr _background_threads_as_list(VM *vm) {
  auto result = Nil; prot_ptr(result);
  auto n = vm->threads->front;
  while (n) {
    result = cons(vm, n->val, result);
    n = n->next;
  }
  unprot_ptr(result);
  return _list_rev(result);
}

void _vm_unwind_to_root_frame(VM *vm) {
  auto predicate = [&](StackFrameObject *fr){
    return fr->prev_frame == 0;
  };
  vm_unwind_to_predicate(vm, predicate);
}

Ptr _vm_thread_suspend(VM *vm) {
  if (vm->frame->prev_frame == 0) return Nil;
  auto predicate = [&](StackFrameObject *fr){
    return fr->prev_frame == 0;
  };
  auto cont = vm_snapshot_stack_with_predicate(vm, predicate);
  thread_set_continuation(vm->globals->current_thread, cont);
  thread_set_status(vm->globals->current_thread, THREAD_STATUS_WAITING);
  vm_unwind_to_predicate(vm, predicate);
  return vm->globals->current_thread;
}

void _vm_thread_resume(VM *vm, Ptr thread) { prot_ptr(thread);
  // FIXME: we should be able to unwind to root here, but it breaks things at the moment
  auto cont = thread_get_continuation(thread);
  vm_restore_stack_snapshot(vm, cont);
  vm->globals->current_thread = thread;
  thread_set_status(thread, THREAD_STATUS_RUNNING);
  unprot_ptr(thread);
}

// N.B. when used outside the interpreter loop, you must
//      manually advance the PC before entering vm_interp
bool vm_maybe_start_next_thread(VM *vm) {
  auto next = _vm_maybe_get_next_available_thread(vm);
  if (is(thread, next)) {
    _vm_thread_resume(vm, next);
    // dbg("did resume next thread...", next);
    return true;
  }
  return false;
}

void vm_add_thread_to_background_set(VM *vm, Ptr thread){
  assert(is(thread, thread));
  assert(!(thread == vm->globals->current_thread));
  if (!ptr_eq(thread_get_status(thread), THREAD_STATUS_SLEEPING) &&
      !ptr_eq(thread_get_status(thread), THREAD_STATUS_WAITING) &&
      !ptr_eq(thread_get_status(thread), THREAD_STATUS_SEM_WAIT)
      ) {
    dbg(" !!!! ensuring thread status !!!");
    thread_set_status(thread, THREAD_STATUS_WAITING);
  }
  // dbg("adding background thread");
  ptrq_push(vm->threads, thread);
}

void vm_suspend_current_thread(VM *vm) {
  Ptr curr = _vm_thread_suspend(vm);
  vm->globals->current_thread = Nil; // XXX careful!
  if (is(thread,curr)) vm_add_thread_to_background_set(vm, curr);
  vm->suspended = true;
}

bool vm_sleep_current_thread(VM *vm, s64 ms) {
  Ptr curr = _vm_thread_suspend(vm);
  vm->globals->current_thread = Nil; // XXX careful!
  if (is(thread,curr)) {
    // dbg("putting thread to sleep, count is now: ", vm->threads->size());
    thread_set_status(curr, THREAD_STATUS_SLEEPING);
    s64 wake_after = current_time_ms() + ms;
    thread_set_wake_after(curr, to(Fixnum, wake_after));
    vm_add_thread_to_background_set(vm, curr);
    // dbg("put thread to sleep, count is now: ", vm->threads->size());
    // dbg("slept to thread: ", curr);
  }
  auto result = vm_maybe_start_next_thread(vm);
  return result;
}

bool vm_sem_wait_current_thread(VM *vm, Ptr semaphore) { prot_ptr(semaphore);
  Ptr curr = _vm_thread_suspend(vm);
  vm->globals->current_thread = Nil; // XXX careful!
  if (is(thread,curr)) {
    thread_set_status(curr, THREAD_STATUS_SEM_WAIT);
    thread_set_semaphore(curr, semaphore);
    vm_add_thread_to_background_set(vm, curr);
  }
  auto result = vm_maybe_start_next_thread(vm);
  unprot_ptr(semaphore);
  return result;
}

bool vm_swap_threads(VM *vm) {
  auto next = _vm_maybe_get_next_available_thread(vm);
  if (is(thread, next)) { prot_ptr(next);
    assert(!ptr_eq(next, vm->globals->current_thread));
    Ptr curr = _vm_thread_suspend(vm);
    vm->globals->current_thread = Nil; // XXX careful!
    if (is(thread, curr)) vm_add_thread_to_background_set(vm, curr);
    _vm_thread_resume(vm, next);
    unprot_ptr(next);
    return true;
  }
  return false;
}

Ptr vm_schedule_cont(VM *vm, Ptr cont, Ptr priority, Ptr bindings) {
  assert(is(cont, cont));
  auto thread = make_thread(vm, cont,
                            THREAD_STATUS_WAITING,
                            Nil,
                            FIXNUM(0),
                            priority,
                            bindings);
  vm_add_thread_to_background_set(vm, thread);
  return Nil;
}

/* ---------------------------------------- */
/*        image save / restore support      */

void vm_init_from_heap_snapshot(VM *vm);
ByteCodeObject *make_empty_bytecode(VM *vm);

void _im_prepare_vm_for_snapshot(VM *vm) {

  // write the root package
  ht_at_put(vm, vm->system_dictionary,
            SYSTEM_ROOT_PACKAGE_KEY, vm->globals->root_package);

  // copy the waiting threads into the system dictionary 
  auto threads = _background_threads_as_list(vm);
  ht_at_put(vm, vm->system_dictionary, SYSTEM_OTHER_THREADS_KEY, threads);

  // suspend the current thread and store it in the system dictionary
  auto main = _vm_thread_suspend(vm);
  ht_at_put(vm, vm->system_dictionary, SYSTEM_CURRENT_THREAD_KEY, main);

  auto classes = _built_in_classes_as_array(vm);
  ht_at_put(vm, vm->system_dictionary, SYSTEM_BUILTIN_CLASSES_KEY, classes);

  gc(vm);

  #if DEBUG_IMAGE_SNAPSHOTS
  {
    scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it) {
        if (is(thread, it)) {
          _debug_validate_thread_in_heap(vm->heap_mem, vm->heap_end, it);
        }
      });
  }
  #endif
}

// stress test to ensure the save/restore functionality works
void im_move_heap(VM *vm) {

  _im_prepare_vm_for_snapshot(vm);

  // allocate new heap
  auto new_heap = calloc(vm->heap_size_in_bytes, 1);

  // copy in the old heap
  auto new_heap_end = new_heap;
  {
    auto used = vm_heap_used(vm);
    // could be used, but is something missing?
    memcpy(new_heap, vm->heap_mem, vm->heap_size_in_bytes);
    new_heap_end = (u8*)new_heap + used;
    assert(((u64)new_heap_end - (u64)new_heap) ==
           ((u64)vm->heap_end - (u64)vm->heap_mem));
  }

  #if DEBUG_IMAGE_SNAPSHOTS
  {
    auto old_count = 0;
    auto new_count = 0;
    scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it) { old_count++; });
    scan_heap(new_heap,     new_heap_end, [&](Ptr it) { new_count++; });
    assert(old_count == new_count);
    dbg("scanned ", new_count, " references");
  }
  #endif

  // scan new heap updating ptrs with delta from old to new heap
  {
    auto prev = (u64)vm->heap_mem;
    auto next = (u64)new_heap;
    s64 delta = next - prev;
    assert(prev + delta == next);
    bang_heap(new_heap, new_heap_end, [&](Ptr it) {
        auto res = im_offset_ptr(it, delta);
        #if DEBUG_IMAGE_SNAPSHOTS
        if (res != Nil && is(Object, res)) {
          auto addr = as(Object, res);
          assert(addr >= new_heap && addr < new_heap_end);
        }
        #endif
        return res;
      });

    // need to update all the protected pointers as well.
    im_offset_protected_references(vm, delta);
  }

  #if DEBUG_IMAGE_SNAPSHOTS
  {
    scan_heap(new_heap, new_heap_end, [&](Ptr it) {
        if (is(thread, it)) {
          _debug_validate_thread_in_heap(new_heap, new_heap_end, it);
        }
      });
  }
  #endif

  // swap in the new heap and new heap_end
  // free the old heap
  {
    auto used = vm_heap_used(vm); 
    memset(vm->heap_mem, 0, used);
    free(vm->heap_mem);
    vm->heap_mem = new_heap;
    vm->heap_end = new_heap_end;
  }

  // re-initialize vm from heap
  vm_init_from_heap_snapshot(vm);

  #if DEBUG_IMAGE_SNAPSHOTS
  {

    auto found = ht_at(vm->system_dictionary, SYSTEM_CURRENT_THREAD_KEY);
    assert(found == main);
    assert(main == vm->globals->current_thread);
  }
  _debug_validate_thread(main);
  _debug_validate_thread(vm->globals->current_thread);
  #endif

  // resume main thread
  _vm_unwind_to_root_frame(vm);
  _vm_reset_stack_from_root_frame(vm);
  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->frame->mark = KNOWN(exception);
  _debug_validate_stack(vm);

  vm->error = 0;
  vm->suspended = false;
  _vm_thread_resume(vm, vm->globals->current_thread);
}

Ptr im_snapshot_to_path(VM *vm, const char *path){
  _im_prepare_vm_for_snapshot(vm);

  // allocate new heap
  auto new_heap = calloc(vm->heap_size_in_bytes, 1);

  // copy in the old heap
  auto new_heap_end = new_heap;
  {
    auto used = vm_heap_used(vm);
    // could be used, but is something missing?
    memcpy(new_heap, vm->heap_mem, vm->heap_size_in_bytes);
    new_heap_end = (u8*)new_heap + used;
    assert(((u64)new_heap_end - (u64)new_heap) ==
           ((u64)vm->heap_end - (u64)vm->heap_mem));
  }

  dbg("moving references");
  // move all references down to start at zero
  {
    s64 delta = (u64)vm->heap_mem * -1 + SYSTEM_HEAP_IMAGE_OFFSET;
    bang_heap(new_heap, new_heap_end, [&](Ptr it) {
        return im_offset_ptr(it, delta);
      });

    // FIXME: not-quite-getting the delta right for validation yet
    // s64 validation_delta = delta + (u64)new_heap;
    // _debug_heap_report(new_heap, new_heap_end, validation_delta);
  }

  dbg("writing file...");
  // write it out
  {
    auto size = (u64)new_heap_end - (u64)new_heap;
    FILE *out = fopen(path, "wb");
    if(out != NULL) {
      auto data = (char*)new_heap;
      s64 to_go = size;
      while(to_go > 0) {
        const size_t wrote = fwrite(data, 1, to_go, out);
        if(wrote == 0) break;
        to_go -= wrote;
        data += wrote;
        dbg("to go bytes: ", to_go, " wrote: ", wrote);
      }
      fclose(out);
    }
  }
  dbg("file written");

  free(new_heap);

  // resume main thread
  _vm_unwind_to_root_frame(vm);
  _vm_reset_stack_from_root_frame(vm);
  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->frame->mark = KNOWN(exception);
  _debug_validate_stack(vm);

  vm->error = 0;
  vm->suspended = false;
  _vm_thread_resume(vm, vm->globals->current_thread);

  return True;
}


const char *bao_to_c_string(ByteArrayObject *bao);

Ptr im_snapshot_to_path(VM *vm, ByteArrayObject* path) {
  auto c_path = bao_to_c_string(path);
  auto result = im_snapshot_to_path(vm, c_path);
  free((char *)c_path);
  return result;
}


typedef Ptr (*CCallFunction)(VM*);

enum OpCode : u8 {
  END                  = 0,
  RET                  ,
  PUSHLIT              ,
  POP                  ,
  BR_IF_ZERO           ,
  BR_IF_NOT_ZERO       ,
  DUP                  ,
  CALL                 ,
  TAIL_CALL            ,
  LOAD_ARG             ,
  LOAD_GLOBAL          ,
  LOAD_SPECIAL         ,
  LOAD_CLOSURE         ,
  STORE_CLOSURE        ,
  BUILD_CLOSURE        ,
  PUSH_CLOSURE_ENV     ,
  BR_IF_False          ,
  JUMP                 ,
  STACK_RESERVE        ,
  LOAD_FRAME_RELATIVE  ,
  STORE_FRAME_RELATIVE ,
  POP_CLOSURE_ENV      ,
  PUSH_SPECIAL_BINDING ,
  POP_SPECIAL_BINDING  ,
};

inline void vm_push(VM* vm, Ptr value) {
  *(--vm->stack) = value;
}

inline void vm_stack_reserve_n(VM* vm, u64 n){
  vm->stack -= n;
  memset(vm->stack, 0, n * 8);
}

inline Ptr vm_pop(VM* vm) {
  return *(vm->stack++);
}

inline void vm_stack_pop_n(VM *vm, u64 n) {
  vm->stack += n;
}

inline Ptr vm_stack_ref(VM *vm, u32 distance) {
  return vm->stack[distance];
}

inline Ptr vm_load_arg(VM *vm, u32 idx) {
  u64 argc = vm->frame->argc;
  u64 ofs  = vm->frame->pad_count;
  return vm->frame->argv[ofs + (argc - (idx + 1))];
}

// N.B. must not double-prot the ptrs on the stack to avoid double-copy
Ptr vm_get_stack_values_as_list(VM *vm, u32 count) { //@varargs
  Ptr result = Nil; prot_ptr(result);
  Ptr *ptrs = vm->stack; // unprotected to avoid double-copy
  for (u64 i = 0; i < count; i++) {
    result = cons(vm, ptrs[i], result);
  }
  vm_stack_pop_n(vm, count);
  unprot_ptr(result);
  return result;
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

void vm_store_closure_value(VM *vm, u64 slot, u64 depth, Ptr value) {
  auto curr = vm->frame->closed_over;
  while (depth) {
    assert(!isNil(curr));
    curr = array_get(curr, 0);
    depth--;
  }
  assert(!isNil(curr));
  array_set(curr, slot+1, value);
}

inline u8 instr_code(u64 bc) {
  return ((u8*)&bc)[0];
}
inline u8 instr_data(u64 bc) {
  return ((u32*)&bc)[1];
}

inline u64 build_instr(u8 op, u32 data) {
  u64 res = 0;
  ((u8*)&res)[0] = op;
  ((u32*)&res)[1] = data;
  return res;
}

Ptr applicator_for_object(VM *vm, Ptr it) {
  auto klass = class_of(vm, it);
  return standard_object_get_ivar(as(Standard, klass), BaseClassApplicator);
}

// @speed this will be hideously slow. need bytecode level support
inline void vm_interp_prepare_for_send(VM *vm, u32 argc) {
  // TODO: arity check and errors
  auto message = vm_stack_ref(vm, argc - 1);
  auto self    = vm_stack_ref(vm, argc - 2);
  auto klass   = class_of(vm, self);
  auto dict    = standard_object_get_ivar(as(Standard, klass), BaseClassMethodDict);
  auto fn      = ht_at(dict, message);
  if (isNil(fn)) die("could not send message: ", message);
  vm_push(vm, fn);
}

// @incomplete need to confirm that klass is actually a class.
Ptr class_set_method(VM *vm, StandardObject *klass, Ptr sym, Ptr callable) {
  auto dict = standard_object_get_ivar(klass, BaseClassMethodDict);
  ht_at_put(vm, dict, sym, callable);
  return Nil;
}

inline Ptr giant_switch(VM *vm, u32 argc, u32 idx);
bool vm_handle_error(VM *vm);

// @speed would be nice to have less indirection here,
//        but need to integrate with stack push/pop and gc both
#define vm_curr_instr(vm) vm->bc->code->data[vm->pc]
#define vm_adv_instr(vm) vm->bc->code->data[++vm->pc]

typedef struct {
  s64 thread_switch_instr_budget, total_execution_instr_budget;
  bool block_for_initial_thread;
} interp_params;

//absurdly low for testing
#define CTX_SWITCH 1000
#define RUN_QUICK 10000
#define RUN_AWHILE 1000000
#define RUN_INDEFINITELY 0
auto INTERP_PARAMS_MAIN_EXECUTION = (interp_params){CTX_SWITCH,RUN_INDEFINITELY, false};
auto INTERP_PARAMS_MAIN_EVENT_HANDLER = (interp_params){CTX_SWITCH,RUN_AWHILE, false};
auto INTERP_PARAMS_EVAL = (interp_params){CTX_SWITCH,RUN_INDEFINITELY,true};

void vm_interp(VM* vm, interp_params params) {
  auto counter = 0;
  auto init_thread = vm->globals->current_thread; prot_ptr(init_thread);
  u64 instr; u8 code; u32 data;
  s64 ctx_switch_budget, spent_instructions;
  ctx_switch_budget = params.thread_switch_instr_budget;
  spent_instructions = 0;

 restart_interp:

  vm->suspended = false;

  while ((instr = vm_curr_instr(vm))) {
    vm->instruction_count++;
    spent_instructions++;
    ctx_switch_budget--;
    code = instr_code(instr);
    data = instr_data(instr);

    switch (code){
    case STACK_RESERVE: {
      u64 count = vm_adv_instr(vm);
      vm_stack_reserve_n(vm, count);
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
      u32 idx = data;
      Ptr it = vm->bc->literals->data[idx];
      vm_push(vm, it);
      break;
    }
    case LOAD_GLOBAL: {
      // assumes it comes after a pushlit of a symbol
      *vm->stack = get_global(vm, *vm->stack);
      break;
    }
    case LOAD_CLOSURE: {
      u64 slot  = vm_adv_instr(vm);
      u64 depth = vm_adv_instr(vm);
      auto it = vm_load_closure_value(vm, slot, depth);
      vm_push(vm, it);
      break;
    }
    case STORE_CLOSURE: {
      u64 slot  = vm_adv_instr(vm);
      u64 depth = vm_adv_instr(vm);
      auto value = vm_pop(vm);
      vm_store_closure_value(vm, slot, depth, value);
      vm_push(vm, value);
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
      while (count--) { //@speed
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
    case TAIL_CALL: {
      u32 argc = data;
      vm_prepare_for_tail_call(vm, argc);
      // fallthrough
    }
    case CALL: {
      u32 argc = data;
      reenter_call:
      auto fn = vm_pop(vm);
      if (is(PrimOp, fn)) {
        u64 v = fn.value;
        // TODO: validate argc against prim op
        // auto argc = (v >> 16) & 0xFF;
        auto idx  = (v >> 32) & 0xFFFF;
        auto is_builtin = !(idx & ~0b111);

        if (is_builtin) {
          if (idx == 0) { // APPLY
            // TODO: multi-arity apply
            auto args = vm_pop(vm);
            auto to_apply = vm_pop(vm);
            if (!is(cons, args)) {
              vm->error = "bad call to apply. args is not a list.";
              break;
            }
            u32 count = 0;
            do_list(vm, args, [&](Ptr arg){ vm_push(vm, arg); count++; });
            vm_push(vm, to_apply);
            argc = count;
            goto reenter_call;
          } else if (idx == 1) { // SEND
            dbg("preparing to send... ", idx);
            vm_interp_prepare_for_send(vm, argc);
            argc--;
            goto reenter_call;
          } else if (idx == 2) { // SLEEP
            auto ms = vm_pop(vm); 
            vm_push(vm, Nil);
            // dbg("sleeping for ", as(Fixnum, ms));
            if (!vm_sleep_current_thread(vm, as(Fixnum, ms))) {
              // if we failed to resume another thread, we are suspended
              vm->suspended = true;
              goto exit;
            } else {
              // do nothing, we have another thread ready to go
            }
            break;
          } else if (idx == 3) { // SEM_WAIT
            auto semaphore = vm_pop(vm);
            vm_push(vm, Nil);
            auto ct = as(Fixnum, semaphore_get_count(semaphore));
            // dbg("in semaphore-wait, count is: ", ct);
            if (ct > 0) {
              semaphore_set_count(semaphore, to(Fixnum, ct - 1));
            } else {
              vm->pc++;
              if (!vm_sem_wait_current_thread(vm, semaphore)) {
                // if we failed to resume another thread, we are suspended
                vm->suspended = true;
                goto exit;
              } else {
                // do nothing, we have another thread ready to go
              }
            }
            break;
          } else if (idx == 4) { // KILL_THD
            auto thread = vm_pop(vm);
            vm_push(vm, Nil);
            thread_set_status(thread, THREAD_STATUS_DEAD);
            if (thread == vm->globals->current_thread) {
              thread_set_status(thread, THREAD_STATUS_DEAD);
              auto exclusive = params.block_for_initial_thread;
              // FIXME: the check here should not be needed
              if (!exclusive) _vm_unwind_to_root_frame(vm);
              if (exclusive || !vm_maybe_start_next_thread(vm)) {
                vm->suspended = true;
                goto exit;
              }
            } else {
              ptrq_remove_ptr(vm->threads, thread);
            }
            break;
          }
        }

        // cerr << " calling prim at idx: " << idx << " arg count = " << argc << endl;
#if PRIM_USE_GIANT_SWITCH
        giant_switch(vm, argc, idx);
#else
        PrimitiveFunction fn = PrimLookupTable[idx];
        Ptr result = (*fn)(vm, argc);
        vm_push(vm, result);
#endif
        break;
      }
      if (!is(Closure, fn)) {
        if (fn == Nil) {
          vm->error = "value is not a closure";
        } else {
          {
            // rotate fn into first position
            vm_push(vm, fn);
            for (auto n = 0; n < argc; n++) {
              vm->stack[n] = vm->stack[n+1];
            }
            vm->stack[argc] = fn;
          }
          argc++;
          vm_push(vm, applicator_for_object(vm, fn));
          goto reenter_call;
        }

#if GC_DEBUG
        if (is(BrokenHeart, fn)) {
          auto other = objToPtr(gc_forwarding_address(as(Object, fn)));
          dbg("attempted to call broken heart, fwd:", other);
        }
#endif
        break;
      }
      auto bc = closure_code(fn);
      if (bc->is_varargs) {
        prot_ptrs(fn);
        vm_push(vm, vm_get_stack_values_as_list(vm, argc));
        unprot_ptrs(fn);
        auto bc  = closure_code(fn); // bc may have moved
        auto env = closure_env(fn);
        vm_push_stack_frame(vm, 1, bc, env);
      } else {
        auto env = closure_env(fn);
        vm_push_stack_frame(vm, argc, bc, env);
      }

      vm->pc--; // or, could insert a NOOP at start of each fn... (or continue)
      // PC is now -1

      // we choose the end of a CALL as our safe point for ctx switching
      if (params.total_execution_instr_budget &&
          spent_instructions >= params.total_execution_instr_budget) {
        // dbg("suspending execution");
        vm_suspend_current_thread(vm);
        if (vm->error) { dbg("error suspending: ", vm->error); }
        // dbg("suspended.");
        return;
      }
      if (params.thread_switch_instr_budget && ctx_switch_budget <= 0) {
        if (vm_swap_threads(vm)) {
          // dbg("swapped threads.");
          #if DEBUG_IMAGE_SNAPSHOTS
          if (vm->gc_count > 1 && counter % 30 == 0) {
           im_move_heap(vm);
          }
          #endif
          counter++;
        } else {
          // dbg("did not swap threads. ", vm->threads->size());
        }
        ctx_switch_budget = params.thread_switch_instr_budget;
      }

      break;
    }
    case RET: {
      auto it = vm_pop(vm);
      vm_pop_stack_frame(vm);
      vm_push(vm, it);
      break;
    }
    case LOAD_ARG: {
      u64 idx = data;
      auto it = vm_load_arg(vm, idx);
      vm_push(vm, it);
      // cout << " loading arg "<< idx << ": " << it << endl;
      // vm_dump_args(vm);
      break;
    }
    case PUSH_SPECIAL_BINDING: {
      auto val = vm_pop(vm);
      auto var = vm_pop(vm);
      _thread_local_binding_push(vm, var, val);
      break;
    }
    case POP_SPECIAL_BINDING: {
      _thread_local_binding_pop(vm);
      break;
    }
    case LOAD_SPECIAL: {
      // assumes it comes after a pushlit of a special symbol
      *vm->stack = get_special(vm, *vm->stack);
      break;
    }
    default:
      dbg("instr = ", instr, " code = ", (int)code, " data = ", data);
      dbg("bc = ", vm->bc);
      dbg("mk = ", vm->frame->mark);
      vm->error = "unexpected BC";
      print_stacktrace();
      exit(1);
      return;
    }

    if (vm->error) {
      if (!vm_handle_error(vm)) {
        vm->suspended = true;
        // vm->globals->current_thread = Nil;
        goto exit;
      }
    }

    ++vm->pc;
  }

 exit:

  // should ONLY be used by our EVAL, (not userspace one)
  // which we want to block in the strange case
  // that somone sleeps at the toplevel...
  if (params.block_for_initial_thread) {
    if(!vm->suspended &&
       init_thread == vm->globals->current_thread) {
      // the initial thread ran its course
      unprot_ptr(init_thread);
      return;
    } else if (vm->suspended &&
               thread_get_status(init_thread) == THREAD_STATUS_DEAD) {
      // the initial thread was killed
      unprot_ptr(init_thread);
      return;
    } else {

    retry_starting_next_thread:
      bool restarting = vm_maybe_start_next_thread(vm);
      if (restarting) {
        // dbg("restarting interpreter loop, remaining other threads: ", vm->threads->size());
        ctx_switch_budget = params.thread_switch_instr_budget;
        ++vm->pc;
        goto restart_interp;
      } else {
        auto ms = _vm_threads_get_minimum_sleep_time(vm);
        if (ms < 0) {
          // dbg("giving up with ret code ", ms);
          unprot_ptr(init_thread);
          return;
        } else {
          // dbg("waiting for ", ms, " ms");
        }
        usleep(ms * 1000);
        // dbg("will retry starting next thread...");
        // dbg("init_thread status = ", thread_get_status(init_thread));
        goto retry_starting_next_thread;
      }
      goto restart_interp;
    }
  }

  if (vm->suspended) {
    unprot_ptr(init_thread);
    return;
  }

  // if exclusive
  if (!params.thread_switch_instr_budget && !params.total_execution_instr_budget) {
    unprot_ptr(init_thread);
    return;
  }

  bool restarting = vm_maybe_start_next_thread(vm);
  if (restarting) {
    // dbg("restarting interpreter loop, remaining other threads: ", vm->threads->size());
    ctx_switch_budget = params.thread_switch_instr_budget;
    ++vm->pc;
    goto restart_interp;
  }

  unprot_ptr(init_thread);
}
#undef vm_curr_instr
#undef vm_adv_instr


typedef std::tuple<u64*, string> branch_entry;

class BCBuilder {
private:
  VM* vm;
  u64* bc_mem;
  u32 bc_index;
  u64 bc_capacity;
  u32 lit_index;
  bool is_varargs;

  ByteCodeObject *bc;
  std::map<string, u64> *labelsMap; // label -> bc_index
  std::vector<branch_entry> *branchLocations; // tuple of label and &bc_mem
  u64 labelContextCounter;
  std::vector<u64> *labelContextStack;
  u64 labelContext;

  u64 *temp_count;
  Object *literals; // xarray[any]

  void _reserveInstruction() {
    if (bc_index - 1 >= bc_capacity) {
      bc_capacity *= 2;
      bc_mem = (u64 *)realloc(bc_mem, bc_capacity * 8);
    }
  }
  BCBuilder* pushPair(u8 op, u32 data) {
    return pushU64(build_instr(op, data));
  }
  BCBuilder* pushOp(u8 op) {
    return pushU64(build_instr(op, 0));
  }
  BCBuilder* pushU64(u64 it) {
    _reserveInstruction();
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
    name += "____" + std::to_string(labelContext);
    return name;
  }
  BCBuilder* pushJumpLocation(const char* raw_name) {
    auto name = labelify(raw_name);
    auto location = pushEmptyRef();
    branchLocations->push_back(make_tuple(location,name));
    return this;
  }
  void fixupJumpLocations() {
    auto skip_stack = *temp_count == 0;
    auto offset = skip_stack ? -2 : 0;
    for (branch_entry it : *branchLocations) {
      auto loc = std::get<0>(it);
      auto lbl = std::get<1>(it);
      auto tgt = (*labelsMap)[lbl] + offset;
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

    auto skip_stack = *temp_count == 0;
    auto count = bc_index + (skip_stack ? -1 : 1);

    gc_protect(array);
    bc = as(ByteCode, make_bytecode(vm, count));
    gc_unprotect(array);

    auto start_index = skip_stack ? 2 : 0;
    bc->literals = array;
    for (u64 i = start_index; i < bc_index; i++) {
      bc->code->data[i - start_index] = bc_mem[i];
    }
  }
public:
  BCBuilder(VM* vm) {
    this->vm = vm;

    bc_index            = 0;
    lit_index           = 0;
    bc_capacity         = 1024;
    bc_mem              = (u64 *)calloc(bc_capacity, 8);
    labelsMap           = new std::map<string, u64>;
    branchLocations     = new std::vector<branch_entry>;
    labelContextCounter = 0;
    labelContext        = labelContextCounter;
    labelContextStack   = new std::vector<u64>;
    is_varargs          = false;

    // cleaned up in finalizeByteCode
    this->literals = as(Object, make_xarray(vm));
    gc_protect(this->literals);

    pushOp(STACK_RESERVE);
    temp_count = &bc_mem[bc_index];
    pushU64(0);
    assert(*temp_count == 0);
  }
  ~BCBuilder() {
    delete labelsMap;
    delete branchLocations;
    delete labelContextStack;
    free(bc_mem);
  }
  u64 reserveTemps(u64 count) {
    auto start = *temp_count;
    *temp_count += count;
    return start;
  }
  auto isVarargs() {
    is_varargs = true;
    return this;
  }
  auto pushLit(Ptr literal) {
    auto literals = objToPtr(this->literals);
    auto idx = xarray_index_of(literals, literal);
    if (idx == -1) {
      xarray_push(vm, literals, literal);
      pushPair(PUSHLIT, lit_index);
      lit_index++;
    } else {
      pushPair(PUSHLIT, idx);
    }
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
  auto call(u32 argc) {
    pushPair(CALL, argc);
    return this;
  }
  auto tailCall(u32 argc) {
    pushPair(TAIL_CALL, argc);
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
  auto loadArg(u32 index) {
    pushPair(LOAD_ARG, index);
    return this;
  }
  auto loadGlobal(Ptr sym) {
    if (!is(Symbol, sym)) {
      die(sym, " is not a symbol");
    }
    // TODO: this may be a little harsh, but it helps catch typos...
    if (!boundp(vm, sym)) { 
      die(sym, " is not defined in the global environment.");
    }
    pushLit(sym);
    pushOp(LOAD_GLOBAL);
    return this;
  }
  auto loadSpecial(Ptr sym) {
    if (!is(Symbol, sym)) {
      die(sym, " is not a special symbol");
    }
    pushLit(sym);
    pushOp(LOAD_SPECIAL);
    return this;
  }
  auto pushSpecialBinding() {
    pushOp(PUSH_SPECIAL_BINDING);
  }
  auto popSpecialBinding() {
    pushOp(POP_SPECIAL_BINDING);
  }
  auto loadClosure(u64 slot, u64 depth) {
    pushOp(LOAD_CLOSURE);
    pushU64(slot);
    pushU64(depth);
    return this;
  }
  auto storeClosure(u64 slot, u64 depth) {
    pushOp(STORE_CLOSURE);
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
    pushOp(END); // XXX off by one again
    pushOp(END);
    fixupJumpLocations();
    finalizeByteCode();
    bc->is_varargs = is_varargs;
    return bc;
  }
};

ByteCodeObject *make_empty_bytecode(VM *vm){
  auto builder = new BCBuilder(vm);
  auto result = builder->build();
  delete builder;
  return result;
}


/* ---------------------------------------------------*/

void vm_run_until_completion(VM *vm) {
  // dbg("running remaining threads");

  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->frame->mark = KNOWN(exception);

  while (vm->threads->front) {
    auto success = vm_maybe_start_next_thread(vm);
    if (success) {
      // dbg("resuming with thread count: ", vm->threads->size());
      vm->pc++;
      vm_interp(vm, INTERP_PARAMS_MAIN_EXECUTION);

      // if (vm->suspended) {
      //   dbg("suspended with thread count: ", vm->threads->size());
      // } else {
      //   dbg("returned normally with thread count: ", vm->threads->size());
      // }
      // re-add root frame
      auto bc = make_empty_bytecode(vm);
      vm_push_stack_frame(vm, 0, bc, Nil);
      vm->frame->mark = KNOWN(exception);

    } else {
      // TODO: some kind of event loop will be more appropriate in the long run
      auto ms = _vm_threads_get_minimum_sleep_time(vm);
      if (ms < 0) {
        // dbg("run completed, return code was", ms);
        return;
      }
      usleep(ms * 1000);
    }
  }
  // dbg("all threads finished: ", (bool)(vm->threads->front == 0));
}


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

Ptr imap_set(VM *vm, Ptr map, Ptr key, Ptr value) {
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

defstruct(varinfo, VarInfo,
          scope,          // VariableScope
          argument_index, // Fixnum
          closure_index); // Fixnum

struct VariableBinding {
  u64 binding_depth;
  Ptr variable_info;
};

defstruct(cenv, CompilerEnv,
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

auto global_env_binding(VM *vm, u64 depth) {
  auto info = make_varinfo(vm, VariableScope_Global,
                           to(Fixnum,0), to(Fixnum, 0));
  return (VariableBinding){depth, info};
}

VariableBinding _compiler_env_binding(VM *vm, Ptr env, Ptr sym, u64 depth) {
  if (isNil(env)) return global_env_binding(vm, depth + 1);

  if (imap_has(cenv_get_info(env), sym)) {
    return (VariableBinding){depth, imap_get(cenv_get_info(env), sym) };
  }

  prot_ptrs(sym, env);
  VariableBinding outer = _compiler_env_binding(vm, cenv_get_prev(env), sym, depth + 1);
  unprot_ptrs(sym, env);
  {
    auto from_lambda = cenv_is_lambda(cenv_get_prev(env));
    auto in_let = cenv_is_let(env);
    auto scope = varinfo_get_scope(outer.variable_info);
    // this is a pretty crude check...
    if (scope == VariableScope_Argument && from_lambda && !in_let) {
      die("variable should have been marked for closure: ", sym);
    }
  }
  return outer;
}
VariableBinding compiler_env_binding(VM *vm, Ptr env, Ptr sym) {
  return _compiler_env_binding(vm, env, sym, 0);
}

void emit_expr(VM *vm, BCBuilder *builder, Ptr it, Ptr env, bool tail);

void emit_call(VM *vm, BCBuilder *builder, Ptr it, Ptr env, bool tail) {
  prot_ptr(env);
  auto fn   = car(it); prot_ptr(fn);
  auto args = cdr(it); prot_ptr(args);
  auto argc = 0;
  do_list(vm, args, [&](Ptr arg){
      argc++;
      emit_expr(vm, builder, arg, env, false);
    });
  emit_expr(vm, builder, fn, env, false);
  if (tail) {
    builder->tailCall(argc);
  } else {
    builder->call(argc);
  }
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
  s64 count = list_length(vm, body);
  auto idx = 0;
  do_list(vm, body, [&](Ptr expr){
      builder->pop();
      auto in_tail = (++idx) == count;
      emit_expr(vm, builder, expr, env, in_tail);
    });
  unprot_ptrs(env, body);
}

auto emit_flat_lambda(VM *vm, Ptr it, Ptr env) {
  prot_ptrs(it, env);
  auto builder = new BCBuilder(vm);
  if (is(Symbol, car(cdr(it)))) builder->isVarargs();
  auto body = cdr(cdr(it));
  emit_lambda_body(vm, builder, body, env);
  builder->ret();
  auto bc = objToPtr(builder->build());
  delete builder;
  unprot_ptrs(it, env);
  return make_closure(vm, bc, Nil);
}

void emit_lambda(VM *vm, BCBuilder *parent, Ptr it, Ptr p_env) {  prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);              prot_ptr(env);
  auto has_closure = cenv_has_closure(env);
  if (has_closure) {
    auto closed = cenv_get_closed_over(env);                      prot_ptrs(closed);
    auto closed_count = xarray_used(closed);


    auto builder = new BCBuilder(vm);

    if (is(Symbol, car(cdr(it)))) builder->isVarargs();

    for (u64 i = 0; i < closed_count; i++) {
      auto ptr     = xarray_at(closed, i);
      auto binding = compiler_env_binding(vm, env, ptr);
      auto index   = as(Fixnum, varinfo_get_argument_index(binding.variable_info));
      builder->loadArg(index);
    }
    unprot_ptrs(closed);

    builder->pushClosureEnv(closed_count);
    auto body = cdr(cdr(it));
    emit_lambda_body(vm, builder, body, env);
    builder->ret();
    parent->pushLit(objToPtr(builder->build()));
    delete builder;
    parent->buildClosure();
  } else {
    auto closure = emit_flat_lambda(vm, it, env);
    parent->pushLit(closure);
  }
  unprot_ptrs(it, p_env, env);
}

void emit_let
(VM *vm, BCBuilder *builder, Ptr it, Ptr p_env, bool tail) { prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);         prot_ptr(env);
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

        emit_expr(vm, builder, expr, p_env, false);

        builder->storeFrameRel(idx);
      });
  }

  auto has_closure = cenv_has_closure(env);

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

  {
    auto body = cdr(cdr(it));                            prot_ptr(body);
    s64 count = list_length(vm, body);
    auto idx = 0;
    builder->pushLit(Nil);
    do_list(vm, body, [&](Ptr expr){
        builder->pop();
        auto in_tail = (++idx) == count ? tail : false;
        emit_expr(vm, builder, expr, env, in_tail);
      });
    unprot_ptr(body);
  }

  if (has_closure) {
    builder->popClosureEnv(); // TODO: vet this w/ TCO
  }

  unprot_ptrs(it, p_env, env);
}


void emit_if(VM *vm, BCBuilder *builder, Ptr it, Ptr env, bool tail) {
  auto test = nth_or_nil(it, 1);
  auto _thn = nth_or_nil(it, 2);
  auto _els = nth_or_nil(it, 3);
  prot_ptrs(env, test, _thn, _els);

  builder->pushLabelContext();
  emit_expr(vm, builder, test, env, false);
  builder->branchIfFalse("else");
  emit_expr(vm, builder, _thn, env, tail);
  builder->jump("endif")->label("else");
  emit_expr(vm, builder, _els, env, tail);
  builder->label("endif");
  builder->popLabelContext();

  unprot_ptrs(env, test, _thn, _els);
}

void emit_set_bang(VM *vm, BCBuilder *builder, Ptr it, Ptr env) { prot_ptrs(it, env);
  auto sym = nth_or_nil(it, 1); prot_ptr(sym);
  auto expr = nth_or_nil(it, 2); prot_ptr(expr);

  emit_expr(vm, builder, expr, env, false);
  builder->dup(); // return the value

  auto binding        = compiler_env_binding(vm, env, sym);
  auto info           = binding.variable_info;
  auto scope          = varinfo_get_scope(info);
  auto argument_index = varinfo_get_argument_index(info);
  auto closure_index  = varinfo_get_closure_index(info);

  if (scope == VariableScope_Global) {
    die(sym, " set! to global NYS ");
  } else if (scope == VariableScope_Argument) {
    die(sym, " set! to argument NYS");
  } else if (scope == VariableScope_Closure) {
    auto index = as(Fixnum, closure_index);
    auto depth = binding.binding_depth;
    builder->storeClosure(index, depth);
  } else if (scope == VariableScope_Let) {
    // LAZY reusing argument_index
    builder->storeFrameRel(as(Fixnum, argument_index));
  } else {
    std::cout << "unexpected variable scope: " << scope << std::endl;
    assert(false);
  }
  unprot_ptrs(it, env, sym, expr);
}

void emit_with_special_binding(VM *vm, BCBuilder *builder, Ptr it, Ptr env) {
  auto sym       = car(cdr(it));
  auto val_expr  = car(cdr(cdr(it)));
  auto body_form = car(cdr(cdr(cdr(it))));
  builder->pushLit(sym);
  emit_expr(vm, builder, val_expr, env, false);
  builder->pushSpecialBinding();
  emit_expr(vm, builder, body_form, env, false);
  builder->popSpecialBinding();
}

void emit_expr(VM *vm, BCBuilder *builder, Ptr it, Ptr env, bool tail) {
  if (is(Symbol, it)) { /*                                  */ prot_ptrs(it, env);
    auto binding        = compiler_env_binding(vm, env, it);
    auto info           = binding.variable_info;
    auto scope          = varinfo_get_scope(info);
    auto argument_index = varinfo_get_argument_index(info);
    auto closure_index  = varinfo_get_closure_index(info);
    if (scope == VariableScope_Global) {
      // TODO: eventually have a VariableScope_Special instead
      if (is_special_symbol(vm, it)) builder->loadSpecial(it);
      else builder->loadGlobal(it);
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
      die("unexpected variable scope: ", scope);
    }
    unprot_ptrs(it, env);
  } else if (consp(it)) { // @safe
    auto fst = car(it);
    if (is(Symbol, fst)) {
      if (ptr_eq(KNOWN(lambda), fst)) {
        emit_lambda(vm, builder, it, env);
        return;
      } else if (ptr_eq(KNOWN(quote), fst)) {
        auto item = car(cdr(it));
        builder->pushLit(item);
        return;
      } else if (ptr_eq(KNOWN(if), fst)) {
        emit_if(vm, builder, it, env, tail);
        return;
      } else if (ptr_eq(KNOWN(let), fst)) {
        emit_let(vm, builder, it, env, tail);
        return;
      } else if (ptr_eq(KNOWN(set_bang), fst)) {
        emit_set_bang(vm, builder, it, env);
        return;
      } else if (ptr_eq(KNOWN(with_special_binding), fst)) {
        emit_with_special_binding(vm, builder, it, env);
        return;
      }
    }
    emit_call(vm, builder, it, env, tail);
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

    // otherwise, was found in an outer scope, or closed over elsewhere,
    // and we need to create a closure.
    auto info = imap_get(var_map, sym);
    auto scope = varinfo_get_scope(info);

    // already marked.
    if (scope == VariableScope_Closure) return true;

    // there was no lambda in the lower scopes, and the var has not
    // been marked for closure, so do nothing.
    if (!saw_lambda) return false;

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
    auto args    = car(it);                                     prot_ptr(args);
    auto var_map = cenv_get_info(env);                          prot_ptr(var_map);

    auto mark_arg = [&](Ptr arg){                               prot_ptrs(arg);
      assert(is(Symbol, arg));
      auto index = to(Fixnum, idx++);
      auto info  = make_varinfo(vm, VariableScope_Argument, index, zero);
      imap_set(vm, var_map, arg, info);
      unprot_ptrs(arg);
    };

    if (is(Symbol, args)) mark_arg(args);
    else                  do_list(vm, args, mark_arg);

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
    auto fst    = car(it);
    auto is_sym = is(Symbol, fst);
    if (is_sym && ptr_eq(KNOWN(lambda), fst)) {
      mark_lambda_closed_over_variables(vm, it, env);
    } else if (is_sym && ptr_eq(KNOWN(quote), fst)) {
      // do nothing
    } else if (is_sym && ptr_eq(KNOWN(if), fst)) {
      auto test = nth_or_nil(it, 1);                        prot_ptr(test);
      auto _thn = nth_or_nil(it, 2);                        prot_ptr(_thn);
      auto _els = nth_or_nil(it, 3);                        prot_ptr(_els);
      mark_closed_over_variables(vm, test, env);
      mark_closed_over_variables(vm, _thn, env);
      mark_closed_over_variables(vm, _els, env);
      unprot_ptrs(test, _thn, _els);
    } else if (is_sym && ptr_eq(KNOWN(let), fst)) {
      mark_let_closed_over_variables(vm, it, env);
    } else if (is_sym && ptr_eq(KNOWN(set_bang), fst)) {
      auto var = nth_or_nil(it, 1);                         prot_ptr(var);
      auto expr = nth_or_nil(it, 2);                        prot_ptr(expr);
      mark_closed_over_variables(vm, var, env);
      mark_closed_over_variables(vm, expr, env);
      unprot_ptrs(var, expr);
    } else if (is_sym && ptr_eq(KNOWN(with_special_binding), fst)) {
      auto val = nth_or_nil(it, 2);                         prot_ptr(val);
      auto expr = nth_or_nil(it, 3);                        prot_ptr(expr);
      mark_closed_over_variables(vm, val, env);
      mark_closed_over_variables(vm, expr, env);
      unprot_ptrs(val, expr);
    } else {
      do_list(vm, it, [&](Ptr expr){
          mark_closed_over_variables(vm, expr, env);
        });
    }
  }
  unprot_ptrs(it, env);
}

auto _compile_toplevel_expression(VM *vm, Ptr it, bool ret) { prot_ptr(it);
  auto env = cenv(vm, Nil);                                   prot_ptr(env);
  auto builder = new BCBuilder(vm);
  mark_closed_over_variables(vm, it, env);
  emit_expr(vm, builder, it, env, false);
  if (ret) builder->ret();
  auto result = builder->build();
  delete builder;
  unprot_ptrs(it, env);
  return result;
}

auto compile_toplevel_expression(VM *vm, Ptr it) {
  return _compile_toplevel_expression(vm, it, false);
}

/* -------------------------------------------------- */

Ptr primitive_print(Ptr a) { std::cout << a << std::endl; return a; }

Ptr gfx_set_pixel(VM *vm, point p) { // assumes 4 bytes pp
  auto surface = vm->surface;
  if (surface) {
    if (p.x < 0 || p.x >= surface->width) return Nil;
    if (p.y < 0 || p.y >= surface->height) return Nil;
    u32 pixel = 0;
    u8 *target_pixel = surface->mem + p.y * surface->pitch + p.x * 4;
    *(u32 *)target_pixel = pixel;
    vm->screen_dirty = true;
  }
  return Nil;
}

void _gfx_fill_rect(blit_surface *dst, point a, point b, s64 color) {
  u32 pixel = color > 0 ? color : 0L;
  u8* over = (u8*)&pixel;
  auto alpha = over[3];
  s64 max_x = std::min((s64)b.x, dst->width);
  s64 max_y = std::min((s64)b.y, dst->height);
  if (alpha == 255) {
    for (s64 y = a.y; y < max_y; y++) {
      for (s64 x = a.x; x < max_x; x++) {
        auto idx = y * dst->pitch + x * 4;
        auto mem = dst->mem + idx;
        mem[0] = over[0];
        mem[1] = over[1];
        mem[2] = over[2];
        mem[3] = 255;
      }
    }
  } else {
    for (s64 y = a.y; y < max_y; y++) {
      for (s64 x = a.x; x < max_x; x++) {
        auto idx = y * dst->pitch + x * 4;
        auto under = dst->mem + idx;
        // aA + (1-a)B = a(A-B)+B
        under[0] = ((over[0] - under[0]) * alpha /  255)  + under[0];
        under[1] = ((over[1] - under[1]) * alpha /  255)  + under[1];
        under[2] = ((over[2] - under[2]) * alpha /  255)  + under[2];
        u8 ualpha = under[3];
        u8 calpha = alpha + ualpha;
        under[3] = calpha < alpha ? 255 : calpha;
      }
    }
  }
}

Ptr gfx_screen_fill_rect(VM *vm, point a, point b, s64 color) {
  if (vm->surface) {
    _gfx_fill_rect(vm->surface, a, b, color);
    vm->screen_dirty = true;
  }
  return Nil;
}
Ptr gfx_fill_rect(ByteArrayObject *dst_image, point a, point b, s64 color) {
  auto dst = image_blit_surface(dst_image);
  _gfx_fill_rect(&dst, a, b, color);
  return Nil;
}

Ptr gfx_clear_rect(ByteArrayObject *dst_image, point a, point b) {
  auto dst = image_blit_surface(dst_image);
  _gfx_fill_rect(&dst, a, b, -1);
  return Nil;
}

struct blit_sampler {
  blit_surface *src;
  f32 scale, iscale, angle,
    du_col, dv_col,
    du_row, dv_row,
    u, v, row_u, row_v,
    src_x, src_y;
  s32 min_x, min_y, max_x, max_y;
};

// largely from http://www.drdobbs.com/architecture-and-design/fast-bitmap-rotation-and-scaling/184416337
// and some mention from alan kay in a video about how it works
inline void blit_sampler_init(blit_sampler *s, blit_surface *src,
                              f32 scale, f32 deg_rot, rect *from) {
  s->src = src;
  s->scale = scale;
  s->iscale = 1.0f / scale;

  f32 angle = _deg_to_rad((f32)(fmodf(deg_rot , 360.0f)));
  s->angle = angle;

  f32 rvx = cosf(angle);
  f32 rvy = sinf(angle);

  s->du_col = rvx;  s->dv_col = rvy;
  s->du_row = -rvy; s->dv_row = rvx;

  f32 cx = from->x + from->width  * 0.5f;
  f32 cy = from->y + from->height * 0.5f;

  // I'm starting to get what these are for but not 100% yet.
  f32 dcx = from->width  * 0.5f;
  f32 dcy = from->height * 0.5f;

  // set the initial position by rotating the 'destination' surface
  // I don't quite get why this works yet :P
  s->src_x = cx - (dcy * s->du_row + dcx * s->dv_row);
  s->src_y = cy - (dcx * s->dv_col + dcy * s->du_col);

  s->u = s->src_x;
  s->v = s->src_y;

  s->row_u = s->src_x;
  s->row_v = s->src_y;

  // source sample range
  s->min_x = std::max(from->x, 0LL);
  s->max_x = std::min(from->x + from->width,  src->width);
  s->min_y = std::max(from->y, 0LL);
  s->max_y = std::min(from->y + from->height, src->height);
}

inline void blit_sampler_start_row(blit_sampler *s) {
  s->u = s->row_u; s->v = s->row_v;
}

inline bool blit_sampler_sample(blit_sampler *s, u8**out) {
  s32 sx = floorf(s->u), sy = floorf(s->v);
  if (sx >= s->min_x && sx < s->max_x &&
      sy >= s->min_y && sy < s->max_y) {
    auto src = s->src;
    *out = src->mem + sy * src->pitch + sx * 4;
    return true;
  } else {
    return false;
  }
}

inline void blit_sampler_step_col(blit_sampler *s) {
  s->u += s->du_col * s->iscale;
  s->v += s->dv_col * s->iscale;
}
inline void blit_sampler_step_row(blit_sampler *s) {
  s->row_u += s->du_row * s->iscale;
  s->row_v += s->dv_row * s->iscale;
}


#define DEBUG_FILL 0
Ptr gfx_blit_image(blit_surface *src, blit_surface *dst,
                   rect *from,
                   point at,
                   f32 scale,
                   f32 deg_rot) {

  u32 scan_width; u32 scan_height;
  // TODO: @speed properly calculate scan width and height (rotate rect and get bounds)
  {
    f32 sw = from->width  * scale;
    f32 sh = from->height * scale;
    scan_width = scan_height = (u32)floorf(sqrtf(sw * sw + sh * sh));
  }

  s32 right  = std::min((s32)scan_width, (s32)dst->width - at.x);
  s32 bottom = std::min((s32)scan_height, (s32)dst->height - at.y);

  blit_sampler bs_src;
  blit_sampler_init(&bs_src, src, scale, deg_rot, from);

  for (s32 y = 0; y < bottom; y++) {

    blit_sampler_start_row(&bs_src);
    auto dest_row = (at.y + y) * dst->pitch;

    for (s32 x = 0; x < right; x++) {
      u8 *over;

      // it would be great if there were a way to do fewer checks here.
      if (at.x + x >= 0L && at.y + y >= 0L &&
          blit_sampler_sample(&bs_src, &over)) {

        u8* under = dst->mem + dest_row + (at.x + x) * 4;
        u8 alpha   = over[3];

        // aA + (1-a)B = a(A-B)+B
        under[0] = ((over[0] - under[0]) * alpha /  255)  + under[0];
        under[1] = ((over[1] - under[1]) * alpha /  255)  + under[1];
        under[2] = ((over[2] - under[2]) * alpha /  255)  + under[2];
        u8 ualpha = under[3];
        u8 calpha = alpha + ualpha;
        under[3] = calpha < alpha ? 255 : calpha;
      }
      #if DEBUG_FILL
      else if ( offsx + x < dst->width && offsy + y < dst->height ) {
        u8* under = (dst->mem + dest_row + (offsx + x) * 4);
        under[0] = 0xff;
        under[1] = under[2] = 0;
      }
      #endif

      blit_sampler_step_col(&bs_src);
    }

    blit_sampler_step_row(&bs_src);
  }

  return Nil;
}

Ptr _gfx_blit_image_with_mask(blit_surface *src, blit_surface *dst, blit_surface *msk,
                             point at,
                             rect *from, f32 scale, f32 deg_rot,
                             rect *m_from, f32 m_scale, f32 m_deg_rot
                             ) {

  u32 scan_width; u32 scan_height;
  // TODO: @speed properly calculate scan width and height (rotate rect and get bounds)
  {
    f32 sw = from->width  * scale;
    f32 sh = from->height * scale;
    scan_width = scan_height = (u32)floorf(sqrtf(sw * sw + sh * sh));
  }

  s32 right  = std::min((s32)scan_width, (s32)dst->width - at.x);
  s32 bottom = std::min((s32)scan_height, (s32)dst->height - at.y);

  blit_sampler bs_src;
  blit_sampler_init(&bs_src, src, scale, deg_rot, from);
  blit_sampler bs_msk;
  blit_sampler_init(&bs_msk, msk, m_scale, m_deg_rot, m_from);

  for (s32 y = 0; y < bottom; y++) {

    blit_sampler_start_row(&bs_src);
    blit_sampler_start_row(&bs_msk);
    auto dest_row = (at.y + y) * dst->pitch;

    for (s32 x = 0; x < right; x++) {
      u8 *over;
      u8 *mask;

      // it would be great if there were a way to do fewer checks here.
      if (at.x + x >= 0L && at.y + y >= 0L &&
          blit_sampler_sample(&bs_msk, &mask) &&
          blit_sampler_sample(&bs_src, &over)
          ) {

        u8* under = dst->mem + dest_row + (at.x + x) * 4;
        u8 alpha  = over[3] * mask[0] / 255;

        // aA + (1-a)B = a(A-B)+B
        under[0] = ((over[0] - under[0]) * alpha /  255)  + under[0];
        under[1] = ((over[1] - under[1]) * alpha /  255)  + under[1];
        under[2] = ((over[2] - under[2]) * alpha /  255)  + under[2];
        u8 ualpha = under[3];
        u8 calpha = alpha + ualpha;
        under[3] = calpha < alpha ? 255 : calpha;
      }
      #if DEBUG_FILL
      else if ( offsx + x < dst->width && offsy + y < dst->height ) {
        u8* under = (dst->mem + dest_row + (offsx + x) * 4);
        under[0] = 0xff;
        under[1] = under[2] = 0;
      }
      #endif

      blit_sampler_step_col(&bs_src);
      blit_sampler_step_col(&bs_msk);
    }

    blit_sampler_step_row(&bs_src);
    blit_sampler_step_row(&bs_msk);
  }

  return Nil;
}

Ptr gfx_blit_image_with_mask(ByteArrayObject *src_img,
                             ByteArrayObject *dst_img,
                             ByteArrayObject *msk_img,
                             point at,
                             rect from, f32 scale, f32 deg_rot,
                             rect m_from, f32 m_scale, f32 m_deg_rot
                             ) {
  blit_surface src = image_blit_surface(src_img);
  blit_surface dst = image_blit_surface(dst_img);
  blit_surface msk = image_blit_surface(msk_img);
  return _gfx_blit_image_with_mask(&src, &dst, &msk, at, &from, scale, deg_rot, &m_from, m_scale, m_deg_rot);
};

Ptr gfx_blit_image_at(VM *vm, ByteArrayObject* img, point p, s64 scale100, s64 deg_rot) {
  if (!is(Image, objToPtr(img))) die("gfx_blit_image: not an image");
  auto src  = image_blit_surface(img);
  auto dst = vm->surface;
  vm->screen_dirty = true;

  auto from = (rect){ 0, 0, src.width, src.height };
  return gfx_blit_image(&src, dst, &from, p, scale100/100.0f, deg_rot * 1.0f);
}

Ptr gfx_blit(ByteArrayObject *source_image, ByteArrayObject *dest_image,
             point dst_location,
             point src_upper_left,
             point src_lower_right,
             f32 scale,
             f32 degrees_rotation) {
  if (!is(Image, objToPtr(source_image))) die("gfx_blit_image: not an image");
  if (!is(Image, objToPtr(dest_image)))   die("gfx_blit_image: not an image");
  auto src = image_blit_surface(source_image);
  auto dst = image_blit_surface(dest_image);
  auto from = points_to_rect(src_upper_left, src_lower_right);
  return gfx_blit_image(&src, &dst, &from, dst_location, scale, degrees_rotation);
}

// TODO: it would be nice to represent the screen as just an image,
//       but not sure how to do that without excessive copying.
Ptr gfx_blit_from_screen(VM *vm, ByteArrayObject *dest_image,
                         point dst_location,
                         point src_upper_left,
                         point src_lower_right,
                         f32 scale,
                         f32 degrees_rotation) {
  if (!is(Image, objToPtr(dest_image)))   die("gfx_blit_image: not an image");
  auto dst = image_blit_surface(dest_image);
  auto from = points_to_rect(src_upper_left, src_lower_right);
  return gfx_blit_image(vm->surface, &dst, &from, dst_location, scale, degrees_rotation);
}

/* -------------------------------------------------- */

Ptr compile_to_closure(VM *vm, Ptr expr) {
  auto bc = objToPtr(_compile_toplevel_expression(vm, expr, true));
  auto closure = make_closure(vm, bc, Nil);
  return closure;
}

const char *read_file_contents(string path) {
  std::ifstream istream(path);
  std::stringstream buffer;
  buffer << istream.rdbuf();
  auto str = buffer.str();
  // TODO: does this require freeing later?
  auto cstr = str.c_str();
  auto len = strlen(cstr) + 1;
  auto mem = (char *)calloc(len, 1);
  strcpy(mem, (char *)cstr);
  return (const char *)mem;
}

const char *bao_to_c_string(ByteArrayObject *bao) {
  auto len = bao->length + 1;
  auto mem = (char *)calloc(len, 1);
  memcpy(mem, &bao->data, len - 1);
  return mem;
}

Ptr slurp(VM *vm, ByteArrayObject* path) {
  auto c_path = bao_to_c_string(path);
  auto c_str = read_file_contents(c_path);
  free((char *)c_path);
  auto result = make_string(vm, c_str);
  free((char *)c_str);
  return result;
}

#include "./primop-generated.cpp"

/* -------------------------------------------------- */

void load_file(VM *vm, const char* path);
void _debug_assert_in_heap(VM *vm, Ptr p) {
  if (p == Nil || ! is(Object, p)) return;
  auto it = as(Object, p);
  assert(it >= vm->heap_mem && it < vm->heap_end);
}

void vm_init_from_heap_snapshot(VM *vm) {
  // set system dictionary
  vm->system_dictionary = objToPtr((Object *)(vm->heap_mem));
  _debug_assert_in_heap(vm, vm->system_dictionary);
  dbg("loaded system dictionary: ", vm->system_dictionary);
  assert(is(ht, vm->system_dictionary));

  // set root package
  vm->globals->root_package = ht_at(vm->system_dictionary, SYSTEM_ROOT_PACKAGE_KEY);
  _debug_assert_in_heap(vm, vm->globals->root_package);
  assert(is(package, vm->globals->root_package));

  // set main thread
  vm->globals->current_thread = ht_at(vm->system_dictionary, SYSTEM_CURRENT_THREAD_KEY);
  // set built in classes
  {
    auto xarray = ht_at(vm->system_dictionary, SYSTEM_BUILTIN_CLASSES_KEY);
    _built_in_classes_restore_from_xarray(vm, xarray);
  }

  assert(is(thread, vm->globals->current_thread));
  _debug_assert_in_heap(vm, vm->globals->current_thread);
  _debug_validate_thread_in_heap(vm->heap_mem, vm->heap_end,
                                 vm->globals->current_thread);
  _debug_validate_thread(vm->globals->current_thread);

  // clear out old threads, add new threads
  ptrq_remove_all(vm->threads);
  assert(vm->threads->front == vm->threads->back);
  assert(!vm->threads->front);

  auto thread_list = ht_at(vm->system_dictionary, SYSTEM_OTHER_THREADS_KEY);
  auto count = 0;
  do_list(vm, thread_list, [&](Ptr thread) {
      ptrq_push(vm->threads, thread);
      count++;
    });
  dbg("restored ", count, " threads + main = ", vm->globals->current_thread);

  // re-init known symbols
  initialize_known_symbols(vm);

}

void vm_init_for_blank_startup(VM *vm) {
  vm->gc_disabled = true;

  vm->system_dictionary = ht(vm); // should be the first allocation

  vm->globals->root_package = make_package(vm,
                                           make_string(vm, "lang"),
                                           string_table(vm),
                                           ht(vm),
                                           Nil);

  vm->globals->current_thread = make_thread(vm, Nil,
                                            THREAD_STATUS_RUNNING,
                                            Nil,
                                            FIXNUM(0),
                                            THREAD_PRIORITY_NORMAL,
                                            Nil);

  ht_at_put(vm, vm->system_dictionary,
            SYSTEM_ROOT_PACKAGE_KEY, vm->globals->root_package);

  initialize_known_symbols(vm);
  initialize_classes(vm);
  initialize_primitive_functions(vm);
  initialize_global_variables(vm);

  vm->gc_disabled = false;

  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->frame->mark = KNOWN(exception);

  // load the stdlib
  load_file(vm, "./boot/built-in-classes.lisp");
  load_file(vm, "./boot/0.lisp");
  load_file(vm, "./boot/generic-functions.lisp");
  load_file(vm, "./boot/list.lisp");
  load_file(vm, "./boot/string.lisp");
  load_file(vm, "./boot/string-output-stream.lisp");
  load_file(vm, "./boot/interaction-support.lisp");
  load_file(vm, "./boot/exports.lisp");
}

VM *_vm_create() {
  VM *vm;
  vm = (VM *)calloc(sizeof(VM), 1);

  auto count = 1024 * 1000;
  Ptr *stack_mem = (Ptr *)calloc(sizeof(Ptr), count);
  vm->stack = stack_mem + (count - 1);
  vm->stack_end = stack_mem + 1024; // padding

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
  vm->gc_threshold_in_bytes = 15.0 * 1024 * 1024;

  vm->gc_protected = new std::unordered_map<Object **, u64>;
  vm->gc_protected_ptrs = new std::unordered_map<Ptr *, u64>;
  vm->gc_protected_ptr_vectors = new std::unordered_map<Ptr *, u64>;
  vm->gc_protected->reserve(100);
  vm->gc_protected_ptrs->reserve(100);
  vm->gc_protected_ptr_vectors->reserve(100);

  vm->in_gc = false;

  vm->threads = (ptrq *)calloc(sizeof(ptrq), 1);
  vm->suspended = false;

  vm->frame = 0;
  vm->error = 0;

  vm->globals = (Globals *)calloc(sizeof(Globals), 1);

  return vm;
}

VM *vm_create() {
  VM *vm = _vm_create();
  vm_init_for_blank_startup(vm);
  return vm;
}

VM *vm_create_from_image(const char *path) {
  VM *vm = _vm_create();

  // read the image into heap memory
  {
    struct stat info;
    stat(path, &info);
    auto size = info.st_size;
    FILE *in = fopen(path, "rb");
    assert(in != NULL);
    auto data = (char*)vm->heap_mem;
    auto amt_read = fread(data, 1, size, in);
    assert(amt_read == size);
    fclose(in);
    vm->heap_end = (void *)((u64)vm->heap_mem + size);
    dbg("read size: ", (1.0 * size) / (1024 * 1024));
  }

  // fixup the pointers
  {
    s64 delta = (u64)vm->heap_mem - SYSTEM_HEAP_IMAGE_OFFSET;
    bang_heap(vm->heap_mem, vm->heap_end, [&](Ptr it) {
        return im_offset_ptr(it, delta);
      });
  }

  _debug_heap_report(vm->heap_mem, vm->heap_end);

  {
    scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it) {
        if (it == Nil || !is(Object, it)) return;
        auto where = as(Object, it);
        assert(vm->heap_mem <= where && vm->heap_end > where);
      });
  }

  vm_init_from_heap_snapshot(vm);

  // resume main thread

  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->frame->mark = KNOWN(exception);
  _debug_validate_stack(vm);

  vm->error = 0;
  vm->suspended = false;

  _vm_thread_resume(vm, vm->globals->current_thread);

  return vm;
}

Ptr vm_call_global(VM *vm, Ptr symbol, u64 argc, Ptr argv[]);

Ptr compile_toplevel_expression_with_hooks(VM *vm, Ptr expr) {
  if (boundp(vm, KNOWN(compiler))) {
    Ptr new_expr = vm_call_global(vm, KNOWN(compiler), 1, (Ptr[]){expr});
    return make_closure(vm, objToPtr(compile_toplevel_expression(vm, new_expr)), Nil);
  } else {
    return make_closure(vm, objToPtr(compile_toplevel_expression(vm, expr)), Nil);
  }
}

Ptr eval(VM *vm, Ptr expr) { // N.B. should /not/ be exposed to userspace
  auto closure = compile_toplevel_expression_with_hooks(vm, expr);

  auto bc = closure_code(closure);
  auto env = closure_env(closure);

  vm_push_stack_frame(vm, 0, bc, env);

  vm_interp(vm, INTERP_PARAMS_EVAL);
  Ptr result = Nil;

  if (vm->suspended) {
    // do nothing
    // dbg("in eval, vm was suspended..."); 
  } else {
    result = vm_pop(vm);
    if (vm->frame->prev_frame) {
      vm_pop_stack_frame(vm);
    } else {
      // we got here via some sort of error, like a throw
      // XXX
      // this is really hacky... it will become less important
      // once more execution is handled by the meta runner
      // but depends on internals of the shift/reset implementation
      // of try/catch way too closely
      if (is(cont, result)) {
        result = cont_get_value(result);
        if (is(Closure, result)) {
          result = array_get(closure_env(result), 1);
          dbg("Uncaught Exception: ", result);
        } else {
          dbg("failed to unwrap exception: ", result);
          exit(1);
        }
      }
      // TODO: should be a repr of the result
      vm->error = "uncaught at top level";
      vm_handle_error(vm);
    }
  }

  if (vm->error) {
    dbg("VM ERROR", vm->error);
    exit(1);
  }
  return result;
}

Ptr run_string(VM *vm, const char *str) {
  Ptr result = Nil;                        prot_ptr(result);
  auto done  = cons(vm, Nil, Nil);         prot_ptr(done);
  auto start = str;
  auto end = str + strlen(str) - 1;
  const char *curr = start;
  while (curr < end) {
    auto form = read(vm, &curr, end, done);
    result = eval(vm, form);
  }
  unprot_ptrs(result, done);
  return result;
}

Ptr run_string_with_hooks(VM *vm, const char *str) {
  if (boundp(vm, KNOWN(run_string))) {
    auto string = make_string(vm, str);
    return vm_call_global(vm, KNOWN(run_string), 1, (Ptr[]){string});
  } else {
    return run_string(vm, str);
  }
}

// ----------------------------------------
// simple socket support

struct socket_connection {
  int server_fd, client_fd;
  char *string;
  sockaddr_in *address;
};

void socket_connection_init(socket_connection *conn) {
  #define PORT 8080
  #define BUF_SIZE 5120

  int server_fd, new_socket; 
  int addrlen = sizeof(struct sockaddr_in); 
  struct sockaddr_in *address = (struct sockaddr_in *)calloc(addrlen, 1);; 
  int opt = 1; 

  {
    // Creating socket file descriptor 
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) { 
      perror("socket failed"); 
      exit(EXIT_FAILURE); 
    } 
    // Make socket nonblocking
    if(fcntl(server_fd, F_SETFL, fcntl(server_fd, F_GETFL, 0) | O_NONBLOCK) == -1) {
      perror("calling fcntl");
      exit(EXIT_FAILURE);
    } 

    // Forcefully attaching socket to the port
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt))) { 
      perror("setsockopt"); 
      exit(EXIT_FAILURE); 
    } 
    address->sin_family = AF_INET; 
    address->sin_addr.s_addr = INADDR_ANY; 
    address->sin_port = htons(PORT); 
        
    // Forcefully attaching socket to the port 
    if (bind(server_fd, (const struct sockaddr*)address, (socklen_t)addrlen) < 0) { 
      perror("bind failed"); 
      exit(EXIT_FAILURE); 
    } 
    if (listen(server_fd, 3) < 0) { 
      perror("listen"); 
      exit(EXIT_FAILURE); 
    } 
    new_socket = 0;
  }

  conn->server_fd = server_fd;
  conn->client_fd = 0;
  conn->string    = (char *)calloc(BUF_SIZE, 1);
  conn->address   = address;

}

char *socket_connection_read(socket_connection *conn) {
  int new_socket = conn->client_fd;
  int server_fd  = conn->server_fd;
  char *buffer   = conn->string;
  auto address = conn->address;
  int addrlen = sizeof(struct sockaddr_in); 

  if (new_socket <= 0) {
    new_socket = accept(server_fd, (struct sockaddr *)&address, (socklen_t*)&addrlen);
    if (new_socket > 0) {
      // dbg("socket connected");
      conn->client_fd = new_socket;
    } 
  }

  if (new_socket > 0) {
    // TODO: proper communication protocol here
    int valread = read(new_socket, buffer, BUF_SIZE - 1);
    if (valread > 0) {
      conn->string = (char *)calloc(BUF_SIZE, 1);
      return buffer;
    }
  }

  return 0;
}

#undef PORT
#undef BUF_SIZE

// ----------------------------------------

void start_up_and_run_repl() {
  VM *vm = vm_create();

  // set stdin to nonblocking
  int flags = fcntl(0, F_GETFL, 0);
  fcntl(0, F_SETFL, flags | O_NONBLOCK);

  // start listening for connections from emacs
  socket_connection conn;
  socket_connection_init(&conn);

  std::cout << "lang>";
  while (true) {
    bool got_input = false;

    string input;
    getline(std::cin, input);
    if (input.size() > 0) {
      got_input = true;
      auto result = run_string_with_hooks(vm, input.c_str());
      std::cout << result << std::endl;
    }
    std::cin.clear();

    auto remote_input = socket_connection_read(&conn);
    if (remote_input) {
      got_input = true;
      std::cout << std::endl;
      auto result = run_string_with_hooks(vm, remote_input);
      std::cout << result << std::endl;
      free(remote_input);
    }

    if (!got_input) {
      sleep(1); // TODO: read from stdin/socket connection with timeout instead
      continue;
    }
    std::cout << "lang>";
  }

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
  delete builder;
  unprot_ptr(symbol); unprotect_ptr_vector(argv);
  return result;
}

// use with care. assumes bc is laid out properly for patching.
void patch_bytecode_for_call(VM *vm, ByteCodeObject *bc, Ptr symbol, u64 argc, Ptr argv[]) {
    auto lits = objToPtr(bc->literals);
    for (u64 i = 0; i < argc; i++) {
      array_set(lits, i, argv[i]);
    }
    if (!boundp(vm, symbol)) { die("bad vm_call_global"); }
    array_set(lits, argc, symbol);
}


/* should only be used as an 'entry' into the VM */
/* IOW, we don't want two of these on the stack, they will interfere */
Ptr vm_call_global(VM *vm, Ptr symbol, u64 argc, Ptr argv[]) {
  auto bc = build_call(vm, symbol, argc, argv);

  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->frame->mark = KNOWN(exception);

  vm_interp(vm, INTERP_PARAMS_EVAL);

  auto result = Nil;
  if (vm->suspended) {
    // do nothing
  } else {
    result = vm_pop(vm);
    vm_pop_stack_frame(vm);
  }
  return result;
}

void vm_poke_event(VM *vm, Ptr event_list_name, Ptr semaphore_name, Ptr event_name, Ptr event_data) {
  prot_ptrs(semaphore_name, event_list_name);
  auto event = cons(vm, event_name, event_data);
  set_global(vm, event_list_name, cons(vm, event, get_global(vm, event_list_name)));
  signal_semaphore(get_global(vm, semaphore_name));
  unprot_ptrs(semaphore_name, event_list_name);
}

void run_event_loop_with_display(VM *vm, int w, int h, bool from_image = false) {
  SDL_Window *window;

  auto onshow = root_intern(vm, "onshow"); prot_ptr(onshow);
  auto event_ready_semaphore = root_intern(vm, "event-ready-semaphore");
  prot_ptr(event_ready_semaphore);
  auto pending_events = root_intern(vm, "pending-events"); prot_ptr(pending_events);

  auto x     = SDL_WINDOWPOS_CENTERED;
  auto y     = SDL_WINDOWPOS_CENTERED;
  auto title = "amber";

  // allow highdpi not actually working on my macbook...
  // SDL_WINDOW_FULLSCREEN has insane results (but they /are/ hi dpi)
  auto winopts = SDL_WINDOW_SHOWN; // | SDL_WINDOW_ALLOW_HIGHDPI | SDL_WINDOW_FULLSCREEN_DESKTOP;

  SDL_Init(SDL_INIT_VIDEO);
  window = SDL_CreateWindow(title, x, y, w, h, winopts);
  if(!window) die("could not create window");

  auto window_surface = SDL_GetWindowSurface(window);
  auto window_context = (blit_surface){
    (u8*)window_surface->pixels,
    (s64)window_surface->pitch,
    (s64)window_surface->w,
    (s64)window_surface->h
  };

  vm->surface = &window_context;
  if (!vm->surface) die("could not create surface");
  vm->screen_dirty = false;

  {
    //Initialize PNG loading
    int imgFlags = IMG_INIT_PNG;
    if(!(IMG_Init(imgFlags) & imgFlags)) {
      die("SDL_image could not initialize! SDL_image Error: ", IMG_GetError());
    }
  }

  if(!from_image) {
    auto fmt = window_surface->format;
    SDL_FillRect(window_surface, NULL, SDL_MapRGBA(fmt, 255, 255, 255, 255));
    auto param = to(Point, ((point){(s32)w, (s32)h}));
    vm_poke_event(vm, pending_events, event_ready_semaphore,
                  onshow, param);
    SDL_UpdateWindowSurface(window);
  }

  bool running = true;
  SDL_Event event;

  auto onkey       = root_intern(vm, "onkey");       prot_ptr(onkey);
  auto onmousedrag = root_intern(vm, "onmousedrag"); prot_ptr(onmousedrag);
  auto onmousemove = root_intern(vm, "onmousemove"); prot_ptr(onmousemove);
  auto onmousedown = root_intern(vm, "onmousedown"); prot_ptr(onmousedown);
  auto onframe     = root_intern(vm, "onframe");     prot_ptr(onframe);

  auto as_main_event = INTERP_PARAMS_MAIN_EVENT_HANDLER;

  #if INCLUDE_REPL
  socket_connection conn;
  socket_connection_init(&conn);
  #endif

  auto wait_timeout_ms = 0;

  while (running) {

    #if INCLUDE_REPL
    {
      char *content = socket_connection_read(&conn);
      if (content) {
        run_string_with_hooks(vm, content);
        free(content);
      }
    }
    #endif

    auto has_run_first_cycle = false;

    while (SDL_WaitEventTimeout(&event, wait_timeout_ms)) { 
      switch (event.type) {
      case SDL_QUIT: running = false; break;
      case SDL_KEYDOWN : {
        auto key = event.key.keysym.scancode;
        Ptr num = to(Fixnum, key);
        vm_poke_event(vm, pending_events, event_ready_semaphore, onkey, num);
        break;
      }
      case SDL_MOUSEMOTION: {
        auto x = event.motion.x;
        auto y = event.motion.y;
        auto p = (point){x, y};
        auto pt = to(Point, p);
        if (event.motion.state & SDL_BUTTON_LMASK) {
          vm_poke_event(vm, pending_events, event_ready_semaphore, onmousedrag, pt);
        } else {
          vm_poke_event(vm, pending_events, event_ready_semaphore, onmousemove, pt);
        }
        SDL_FlushEvent(SDL_MOUSEMOTION);
        break;
      }
      case SDL_MOUSEBUTTONDOWN: {
        auto x = event.button.x;
        auto y = event.button.y;
        auto p = (point){x, y};
        auto pt = to(Point, p);
        vm_poke_event(vm, pending_events, event_ready_semaphore, onmousedown, pt);
        break;
      }
      }
      if (vm->error) {
        dbg(vm->error); // TODO: vm_handle_error
        break;
      }
      wait_timeout_ms = 0;
    }

    {
      if (from_image && !has_run_first_cycle) {
        has_run_first_cycle = true;
        //just resume the main thread
        vm_interp(vm, as_main_event);
      } else {
        auto can_run = vm_maybe_start_next_thread(vm);
        if (can_run) {
          // thread resumption usually happens in the interpreter loop,
          // which means the pc is advanced. here we have to do it manually.
          vm->pc++;
          vm_interp(vm, as_main_event);
        }
      }
      if (vm->screen_dirty) SDL_UpdateWindowSurface(window);
      vm->screen_dirty = false;
      auto sleep_time = _vm_threads_get_minimum_sleep_time(vm);
      if (sleep_time == -1) {
        running = false;
        break;
      } else if (sleep_time == -2) {
        sleep_time = 1000;
      }
      wait_timeout_ms = sleep_time;
    }
  }

  unprot_ptrs(onkey, onmousedrag, onmousemove, onmousedown, onframe);

  SDL_DestroyWindow(window);
  std::cerr << " executed " << vm->instruction_count << " instructions." << std::endl;
  std::cerr << " gc count: " << vm->gc_count;
  report_memory_usage(vm);
  SDL_Quit();
}

void run_file_with_optional_display(const char * path) {
  VM *vm = vm_create();
  auto str = read_file_contents(path);

  auto done  = cons(vm, Nil, Nil);         prot_ptr(done);
  auto start = str;
  auto end = str + strlen(str) - 1;
  const char *curr = start;
  while (curr < end) {
    auto form = read(vm, &curr, end, done);
    eval(vm, form);
    if (vm->globals->current_thread != Nil &&
        thread_get_status(vm->globals->current_thread) == THREAD_STATUS_DEAD) {
      vm->globals->current_thread = Nil;
      break;
    }
  }
  unprot_ptrs(done);

  auto wants_display = get_global(vm, root_intern(vm, "wants-display"));
  if (is(Point, wants_display)) {
    auto p = as(Point, wants_display);
    run_event_loop_with_display(vm, p.x, p.y);
  } else {
    vm_run_until_completion(vm);
  }
}

void start_up_and_run_image(const char* path) {
  VM *vm = vm_create_from_image(path);

  vm->pc++;

  auto wants_display = get_global(vm, root_intern(vm, "wants-display"));
  if (is(Point, wants_display)) {
    auto p = as(Point, wants_display);
    run_event_loop_with_display(vm, p.x, p.y, true);
  } else {
    vm_interp(vm, INTERP_PARAMS_EVAL);
    vm_run_until_completion(vm);
  }

}

/* ---------------------------------------- */

const char *require_argv_file(int argc, const char** argv) {
  if (argc > 1) {
    return argv[1];
  } else {
    std::cerr << "must provide a file to run" << std::endl;
    exit(1);
  }
}

int main(int argc, const char** argv) {
  setlocale(LC_ALL, "en_US.utf8");

  initialize_struct_printers();
  initialize_character_names();

  const char *invoked = argv[0];
  const char *curr = argv[0];

  while (*curr) {
    if (*curr == '/') invoked = curr + 1;
    curr++;
  }

  // pretty hacky way of avoiding checking flags, I'll admit...
  if (strcmp(invoked, "amber") == 0) {
    auto file = require_argv_file(argc, argv);
    run_file_with_optional_display(file);
  } else if (strcmp(invoked, "img") == 0){
    auto file = require_argv_file(argc, argv);
    start_up_and_run_image(file);
  } else if (strcmp(invoked, "repl") == 0){
    start_up_and_run_repl();
  } else {
    std::cerr << " unrecognized invocation: " << invoked << std::endl;
    return 2;
  }
  return 0;
}
