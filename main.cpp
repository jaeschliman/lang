/*

(setq flycheck-clang-language-standard "c++14")
(setq flycheck-clang-include-path '("/Users/jsn/Library/Frameworks"))
(setq flycheck-clang-args '("-F" "/Users/jsn/Library/Frameworks"))

*/

// #define NDEBUG

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
#define PRIM_USE_GIANT_SWITCH 1
// with it on:  216533389 / 15 / 60 = ~240,600 instr / frame
//              187983512 / 13.048 / 60 = ~240,117
// with it off: 220863576 / 16.57 / 60 = ~222,150 instr / frame
//              181965286 / 12.79 / 60 = ~237,119
// 2.5Ghz chip. 2500000000 / 1 / 60 = ~41,700,000 ops / frame!
// so roughly 170 ops / bc via wall clock.
// with it on, and NDEBUG: 179763496 / 12.201 / 60 = 245,550 ops/frame

#define unused(x) (void)(x)
#define maybe_unused(x) (void)(x)
#define KNOWN(symbol) vm->globals->known._##symbol
#define _deg_to_rad(x) (x) * (M_PI / 180.0f)

using namespace std;

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

std::ostream &operator<<(std::ostream &os, Ptr p);

inline bool operator == (Ptr a, Ptr b) {
  return a.value == b.value;
}

bool ptr_eq(Ptr a, Ptr b) {
  return a.value == b.value;
}

enum ObjectType : u8 {
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
  u16        flags;             // 16 -- currently unused
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
  u64 argc;
  u64 pad_count;
  Ptr argv[];
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
  blit_surface *surface;
};

/* ---------------------------------------- */

typedef Ptr (*PrimitiveFunction)(VM*, u32 argc);
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

  u64 threshold = vm->gc_threshold_in_bytes;

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

Ptr gfx_load_image(VM *vm, ByteArrayObject *path) {
  if (!is(String, objToPtr(path))) die("load_image takes a string");
  auto str = string(path->data, path->length);
  return load_image_from_path(vm, str);
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

s64 string_char_code_at(VM *vm, ByteArrayObject *str, s64 index) {
  if (index >= str->length) {
    vm->error = "string index out of range";
    return -1;
  }
  return str->data[index];
}

char string_char_at(VM *vm, ByteArrayObject *str, s64 index) {
  if (index >= str->length) {
    vm->error = "string index out of range";
    return -1;
  }
  return (char)(str->data[index]);
}

s64 string_length(ByteArrayObject *str) {
  return str->length;
}

Ptr make_symbol(VM *vm, const char* str, u64 len) {
  ByteArrayObject *obj = alloc_bao(vm, Symbol, len);
  set_obj_tag(obj, Symbol);
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
  set_obj_tag(array, Array);
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
  StructTag_InputStream,
  StructTag_End
};

StructTag struct_get_tag(Ptr it) {
  assert(is(Struct, it));
  return (StructTag)as(Fixnum, array_get(it, 0));
}

/* ---------------------------------------- */

defstruct(cons, Cons, car, cdr);
defstruct(ht, HashTable, array);
defstruct(istream, InputStream, string, index);

/* ---------------------------------------- */

Ptr make_closure(VM *vm, Ptr code, Ptr env) {
  assert(is(ByteCode, code));
  assert(isNil(env) || is(PtrArray, env));
  prot_ptrs(code, env);
  auto it = alloc_pao(vm, Closure, 2);
  set_obj_tag(it, Closure);
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
  die("unexpected object type in size_of");
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
  BaseClassName       = 0,
  BaseClassIvarCount  = 1, //TODO: we don't actually need this for Base Classes
  BaseClassMethodDict = 2,
  BaseClassEnd        = 3
};

typedef void(*DebugPrintFunction)(std::ostream &os, Ptr p);

DebugPrintFunction StructPrintTable[StructTag_End] = {0};

unordered_map<string, char> character_codes_by_name;
unordered_map<char, string> character_names_by_code;

void initialize_character_names() {
#define X(code, name) character_codes_by_name[name] = code;
#include "./character-names.include"
#undef X
#define X(code, name) character_names_by_code[code] = name;
#include "./character-names.include"
#undef X
}

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

std::ostream &operator<<(std::ostream &os, Ptr it) {
  PtrTag tag = (PtrTag)(it.value & TAG_MASK);
  switch (tag) {
  case Fixnum_Tag: return os << as(Fixnum, it);
  case Object_Tag: {
    if (isNil(it)) return os << "nil";
    return os << as(Object, it);
  }
  case Char_Tag  : return os << "#\\" << character_names_by_code[as(Char, it)];
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

Ptr vm_print_debug_stack_trace(VM *vm) {
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
  unordered_map<string, Ptr> *symtab;
  Ptr env; // the global environment (currently an alist)
  Ptr call1;
  struct {
    Ptr _lambda, _quote, _if, _let, _fixnum, _cons, _string, _array, _character, _boolean, _quasiquote, _unquote, _unquote_splicing, _compiler;
  } known;
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


#define handle_class(name) gc_update_base_class(vm, &vm->globals->classes._##name);
#define update_sym(n) gc_update_ptr(vm, &vm->globals->known._##n);
#define update_symbols(...) MAP(update_sym, __VA_ARGS__)

void gc_update_globals(VM *vm) {
  gc_update_ptr(vm, &vm->globals->call1);
  gc_update_ptr(vm, &vm->globals->env);

#define X(...) MAP(handle_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X

  for (auto i = 0; i < BuiltinClassIndexEnd; i++) {
    gc_update_base_class(vm, vm->globals->classes.builtins + i);
  }

  update_symbols(lambda, quote, let, if, fixnum, cons, string,
                 array, character, boolean, quasiquote, unquote, unquote_splicing,
                 compiler);
}

#undef update_sym
#undef update_symbols
#undef handle_class

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
    s64  count = pair.second;
    for (s64 i = 0; i < count; i++) {
      gc_update_ptr(vm, start + i);
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
Ptr ht_at_put(VM *vm, Ptr ht, Ptr key, Ptr value) { prot_ptrs(key, value);
  auto array = ht_get_array(ht);                    prot_ptr(array);
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
    return Nil;
  }
  // collision
  auto entry = mem[idx];
  while (!isNil(entry)) { // existing entries
    auto pair = car(entry);
    if (car(pair) == key) { // found
      set_cdr(pair, value);
      unprot_ptrs(key, value, array);
      return Nil;
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
  return Nil;
}


/* ---------------------------------------- */

void initialize_struct_printers() {
  StructPrintTable[StructTag_Cons] = &debug_print_list;
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

// @unsafe
#define _init_sym(n) globals->known._##n = intern(vm, #n);
#define _init_symbols(...) MAP(_init_sym, __VA_ARGS__)

void initialize_known_symbols(VM *vm) {

  auto globals = vm->globals;
  _init_symbols(lambda, quote, let, if, fixnum, cons, string, array, character, boolean, quasiquote, unquote, compiler);
  globals->known._unquote_splicing = intern(vm, "unquote-splicing");

}
#undef _init_sym
#undef _init_symbols

/* ---------------------------------------- */
// @unsafe
auto make_base_class(VM *vm, const char* name) {
  Ptr slots[] = {make_string(vm,name), make_number(0), ht(vm)};
  auto base = vm->globals->classes.builtins[BuiltinClassIndex_Base];
  return make_standard_object(vm, base, slots);
}

// @unsafe
void initialize_classes(VM *vm)
{
  auto Base = alloc_standard_object(vm, 0, BaseClassEnd);
  Base->klass = Base;
  standard_object_set_ivar(Base, BaseClassName, make_string(vm, "Base"));
  standard_object_set_ivar(Base, BaseClassIvarCount, make_number(BaseClassEnd));
  standard_object_set_ivar(Base, BaseClassMethodDict, ht(vm));
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

Ptr set_global(VM *vm, Ptr sym, Ptr value) {
  assert(is(Symbol, sym));
  set_assoc(vm, &vm->globals->env, sym, value);
  return sym;
}

Ptr set_global(VM *vm, const char* name, Ptr value) { prot_ptr(value);
  auto result = set_global(vm, intern(vm, name), value);
  unprot_ptr(value);
  return result;
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
auto is_alphachar(char ch) {
  u8 idx = ch;
  return character_table[idx] & character_alpha;
}
auto is_nlchar(char ch) {
  return ch == 10 || ch == 13;
}

/* -------------------------------------------------- */

auto quote_form(VM *vm, Ptr it) {
  return cons(vm, KNOWN(quote), cons(vm, it, Nil));
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
      auto result = intern(vm, start, len);
      *remaining = input;
      return result;
    } else if (*input == '\'') {
      input++;
      auto result = quote_form(vm, read(vm, &input, end, done));
      *remaining = input;
      return result;
    } else if (*input == '(') {
      input++;
      prot_ptr(done);
      auto res = read_delimited_list(vm, &input, end, done, ')');
      unprot_ptr(done);
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
      auto it     = read(vm, &input, end, done);
      auto result = cons(vm, KNOWN(quasiquote), cons(vm, it, Nil));
      *remaining = input;
      return result;
    } else if (*input == ',') {
      input++;
      auto prefix = KNOWN(unquote);
      if (*input == '@') {
        input++;
        if (input >= end) {
          vm->error = "unexpected end of input";
          *remaining = input;
          return done;
        }
        prefix = KNOWN(unquote_splicing);
      }
      auto it = read(vm, &input, end, done);
      if (it == done) { *remaining = input; return it; }
      auto result = cons(vm, prefix, cons(vm, it, Nil));
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

Ptr make_istream_from_string(VM *vm, const char *input) {
  auto str = make_string(vm, input); prot_ptr(str);
  auto idx = to(Fixnum, 0);
  auto result = make_istream(vm, str, idx);
  unprot_ptrs(str);
  return result;
}

bool istream_at_end(Ptr s) {
  auto used  = as(Fixnum, istream_get_index(s));
  auto avail = string_length(as(String, istream_get_string(s)));
  return used >= avail - 1;
}

Ptr istream_next(VM *vm, Ptr s) {
  if (istream_at_end(s)) return Nil;
  auto used = as(Fixnum, istream_get_index(s));
  auto str  = as(String, istream_get_string(s));
  istream_set_index(s, to(Fixnum, 1+used));
  return to(Char, string_char_code_at(vm, str, used));
}

// sigh... this is pretty ugly, but it'll have to do for now...
// @speed all this copying is horrrrrrible
Ptr read_from_istream(VM *vm, Ptr s) { prot_ptr(s);
  auto _str  = as(String, istream_get_string(s));
  auto len   = _str->length;
  auto used  = as(Fixnum, istream_get_index(s));
  auto str   = (string(_str->data + used, _str->length)).c_str();
  auto start = str;
  auto end   = str + (len - used);
  auto done  = cons(vm, Nil, Nil);     prot_ptr(done);
  auto input = start;
  auto result = read(vm, &input, end, done);
  used += input - start;
  istream_set_index(s, to(Fixnum, used));
  unprot_ptrs(s, done);
  return result;
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
  set_obj_tag(new_frame, StackFrame);
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

enum OpCode : u8 {
  END                  = 0,
  RET                  = 1,
  PUSHLIT              = 2,
  POP                  = 3,
  BR_IF_ZERO           = 5,
  BR_IF_NOT_ZERO       = 6,
  DUP                  = 7,
  CALL                 = 8,
  LOAD_ARG             = 9,
  LOAD_GLOBAL          = 10,
  LOAD_CLOSURE         = 11,
  BUILD_CLOSURE        = 12,
  PUSH_CLOSURE_ENV     = 13,
  BR_IF_False          = 14,
  JUMP                 = 15,
  STACK_RESERVE        = 16,
  LOAD_FRAME_RELATIVE  = 17,
  STORE_FRAME_RELATIVE = 18,
  POP_CLOSURE_ENV      = 20,
};

inline void vm_push(VM* vm, Ptr value) {
  *(--vm->stack) = value;
}

inline void vm_stack_reserve_n(VM* vm, u64 n){
  vm->stack -= n;
  memset(vm->stack, 0, n);
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
Ptr class_set_method(VM *vm, StandardObject *klass, ByteArrayObject* sym, Ptr callable) {
  auto dict = standard_object_get_ivar(klass, BaseClassMethodDict);
  ht_at_put(vm, dict, objToPtr(sym), callable);
  return Nil;
}

inline Ptr giant_switch(VM *vm, u32 argc, u32 idx);

#define vm_curr_instr(vm) currfn[vm->pc]
#define vm_adv_instr(vm) currfn[++vm->pc]
void vm_interp(VM* vm) {
  u64 instr; u8 code; u32 data; auto currfn = vm->bc->code->data;
  while ((instr = vm_curr_instr(vm))) {
    vm->instruction_count++;
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
      // assumes it comes after a pushlit of a cell in the env alist.
      *vm->stack = cdr(*vm->stack);
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
    case CALL: {
      u32 argc = data;
      reenter_call:
      auto fn = vm_pop(vm);
      if (is(PrimOp, fn)) {
        u64 v = fn.value;
        // TODO: validate argc against prim op
        // auto argc = (v >> 16) & 0xFF;
        auto idx  = (v >> 32) & 0xFFFF;

        // @speed it is horrible to have two if checks here for every call.
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
          vm_interp_prepare_for_send(vm, argc);
          argc--;
          goto reenter_call;
        }

        // cout << " calling prim at idx: " << idx << " arg count = " << argc << endl;
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
        vm->error = "value is not a closure";
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
      currfn = vm->bc->code->data;
      vm->pc--; // or, could insert a NOOP at start of each fn... (or continue)
      break;
    }
    case RET: {
      auto it = vm_pop(vm);
      vm_pop_stack_frame(vm);
      currfn = vm->bc->code->data;
      vm_push(vm, it);
      // cout << "returning: " << it << endl;
      break;
    }
    case LOAD_ARG: {
      u64 idx = data;
      u64 argc = vm->frame->argc;
      u64 ofs  = vm->frame->pad_count;
      auto it = vm->frame->argv[ofs + (argc - (idx + 1))];
      vm_push(vm, it);
      // cout << " loading arg "<< idx << ": " << it << endl;
      // vm_dump_args(vm);
      break;
    }
    default:
      dbg("instr = ", instr, " code = ", (int)code, " data = ", data);
      vm->error = "unexpected BC";
      return;
    }
    if (vm->error) {
      vm_print_debug_stack_trace(vm);
      return;
    };
    ++vm->pc;
  }
}
#undef vm_curr_instr
#undef vm_adv_instr

typedef tuple<u64*, string> branch_entry;

class BCBuilder {
private:
  VM* vm;
  u64* bc_mem;
  u32 bc_index;
  u64 bc_capacity;
  u32 lit_index;
  bool is_varargs;

  ByteCodeObject *bc;
  map<string, u64> *labelsMap; // label -> bc_index
  vector<branch_entry> *branchLocations; // tuple of label and &bc_mem
  u64 labelContextCounter;
  vector<u64> *labelContextStack;
  u64 labelContext;

  u64 *temp_count;
  Object *literals; // xarray[any]

  void _reserveInstruction() {
    if (bc_index - 1 >= bc_capacity) {
      bc_capacity *= 2;
      bc_mem = (u64 *)realloc(bc_mem, bc_capacity);
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
    bc_capacity         = 1024;
    bc_mem              = (u64 *)calloc(bc_capacity, 1);
    labelsMap           = new map<string, u64>;
    branchLocations     = new vector<branch_entry>;
    labelContextCounter = 0;
    labelContext        = labelContextCounter;
    labelContextStack   = new vector<u64>;
    is_varargs          = false;

    this->literals = as(Object, make_xarray(vm));
    gc_protect(this->literals);

    pushOp(STACK_RESERVE);
    temp_count = &bc_mem[bc_index];
    pushU64(0);
    assert(*temp_count == 0);
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
    bc->is_varargs = is_varargs;
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
  prot_ptrs(it, env);
  auto builder = new BCBuilder(vm);
  if (is(Symbol, car(cdr(it)))) builder->isVarargs();
  auto body = cdr(cdr(it));
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
    parent->buildClosure();
  } else {
    auto closure = emit_flat_lambda(vm, it, env);
    parent->pushLit(closure);
  }
  unprot_ptrs(it, p_env, env);
}

void emit_let (VM *vm, BCBuilder *builder, Ptr it, Ptr p_env) {  prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);             prot_ptr(env);

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
    if (is(Symbol, fst)) {
      if (ptr_eq(KNOWN(lambda), fst)) {
        emit_lambda(vm, builder, it, env);
        return;
      } else if (ptr_eq(KNOWN(quote), fst)) {
        auto item = car(cdr(it));
        builder->pushLit(item);
        return;
      } else if (ptr_eq(KNOWN(if), fst)) {
        emit_if(vm, builder, it, env);
        return;
      } else if (ptr_eq(KNOWN(let), fst)) {
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
  emit_expr(vm, builder, it, env);
  if (ret) builder->ret();
  auto result = builder->build();
  unprot_ptrs(it, env);
  return result;
}

auto compile_toplevel_expression(VM *vm, Ptr it) {
  return _compile_toplevel_expression(vm, it, false);
}

/* -------------------------------------------------- */

Ptr primitive_print(Ptr a) { cout << a << endl; return a; }

Ptr gfx_set_pixel(VM *vm, point p) { // assumes 4 bytes pp
  auto surface = vm->surface;
  if (surface) {
    if (p.x < 0 || p.x >= surface->width) return Nil;
    if (p.y < 0 || p.y >= surface->height) return Nil;
    u32 pixel = 0;
    u8 *target_pixel = surface->mem + p.y * surface->pitch + p.x * 4;
    *(u32 *)target_pixel = pixel;
  }
  return Nil;
}

void _gfx_fill_rect(blit_surface *dst, point a, point b, s64 color) {
  u32 pixel = color > 0 ? color : 0L;
  u8* components = (u8*)&pixel;
  s64 max_x = min((s64)b.x, dst->width);
  s64 max_y = min((s64)b.y, dst->height);
  for (s64 y = a.y; y < max_y; y++) {
    for (s64 x = a.x; x < max_x; x++) {
      auto idx = y * dst->pitch + x * 4;
      auto mem = dst->mem + idx;
      mem[0] = components[0];
      mem[1] = components[1];
      mem[2] = components[2];
      mem[3] = components[3];
    }
  }
}

Ptr gfx_screen_fill_rect(VM *vm, point a, point b, s64 color) {
  if (vm->surface) { _gfx_fill_rect(vm->surface, a, b, color); }
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
  s->min_x = max(from->x, 0LL); s->max_x = min(from->x + from->width,  src->width);
  s->min_y = max(from->y, 0LL); s->max_y = min(from->y + from->height, src->height);
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

  s32 right  = min((s32)scan_width, (s32)dst->width - at.x);
  s32 bottom = min((s32)scan_height, (s32)dst->height - at.y);

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

  s32 right  = min((s32)scan_width, (s32)dst->width - at.x);
  s32 bottom = min((s32)scan_height, (s32)dst->height - at.y);

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
  auto bc = objToPtr(_compile_toplevel_expression(vm, expr, true)); prot_ptr(bc);
  auto closure = make_closure(vm, bc, Nil);
  unprot_ptrs(bc);
  return closure;
}

#include "./primop-generated.cpp"

/* -------------------------------------------------- */

void load_file(VM *vm, const char* path);

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
  vm->gc_threshold_in_bytes = 0.5 * 1024 * 1024;

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

  vm->gc_disabled = true;

  initialize_known_symbols(vm);
  initialize_classes(vm);
  initialize_primitive_functions(vm);

  vm->gc_disabled = false;

  // so we have a root frame
  auto bc = (new BCBuilder(vm))->build();
  vm_push_stack_frame(vm, 0, bc);

  // load the stdlib
  load_file(vm, "./boot.lisp");
  return vm;
}

Ptr vm_call_global(VM *vm, Ptr symbol, u64 argc, Ptr argv[]);
bool boundp(VM*, Ptr);

Ptr compile_toplevel_expression_with_hooks(VM *vm, Ptr expr) {
  if (boundp(vm, KNOWN(compiler))) {
    Ptr new_expr = vm_call_global(vm, KNOWN(compiler), 1, (Ptr[]){expr});
    return make_closure(vm, objToPtr(compile_toplevel_expression(vm, new_expr)), Nil);
  } else {
    return make_closure(vm, objToPtr(compile_toplevel_expression(vm, expr)), Nil);
  }
}

Ptr eval(VM *vm, Ptr expr) { // N.B. should /not/ be exposed
  auto closure = compile_toplevel_expression_with_hooks(vm, expr);

  auto bc = closure_code(closure);
  auto env = closure_env(closure);

  vm_push_stack_frame(vm, 0, bc, env);

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

  do {

    auto istream = make_istream_from_string(vm, str); prot_ptr(istream);

    while (!istream_at_end(istream)) {
      auto read = read_from_istream(vm, istream);
      eval(vm, read);
    }

    unprot_ptrs(istream);

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

bool boundp(VM *vm, Ptr sym) {
  auto pair = assoc(sym, vm->globals->env);
  return consp(pair);
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
Ptr vm_call_global(VM *vm, Ptr symbol, u64 argc, Ptr argv[]) {
  prot_ptr(symbol); protect_ptr_vector(argv, argc);

  ByteCodeObject *bc;

  // TODO: bc patches for more arities
  //       NB: will require special method for reserving room for literals,
  //           as they are now de-duplicated
  if (argc == 1) {
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
  auto result = vm_pop(vm);
  vm_pop_stack_frame(vm);

  unprot_ptr(symbol); unprotect_ptr_vector(argv);
  return result;
}


void start_up_and_run_event_loop(const char *path) {
  auto vm = vm_create();
  load_file(vm, path);

  SDL_Window *window;

  auto x     = SDL_WINDOWPOS_CENTERED;
  auto y     = SDL_WINDOWPOS_CENTERED;
  auto w     = 1280;
  auto h     = 800; // To acct for title bar
  auto title = "my window";

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

  {
    //Initialize PNG loading
    int imgFlags = IMG_INIT_PNG;
    if(!(IMG_Init(imgFlags) & imgFlags)) {
      die("SDL_image could not initialize! SDL_image Error: ", IMG_GetError());
    }
  }

  {
    auto fmt = window_surface->format;
    SDL_FillRect(window_surface, NULL, SDL_MapRGBA(fmt, 255, 255, 255, 255));
    auto W = to(Fixnum, w);
    auto H = to(Fixnum, h);
    vm_call_global(vm, intern(vm, "onshow"), 2, (Ptr[]){W, H});
    SDL_UpdateWindowSurface(window);
  }

  bool running = true;
  SDL_Event event;

  auto onkey       = intern(vm, "onkey");       prot_ptr(onkey);
  auto onmousedrag = intern(vm, "onmousedrag"); prot_ptr(onmousedrag);
  auto onmousemove = intern(vm, "onmousemove"); prot_ptr(onmousemove);
  auto onmousedown = intern(vm, "onmousedown"); prot_ptr(onmousedown);

  while (running) {
    if (SDL_WaitEvent(&event)) { //or SDL_PollEvent for continual updates
      switch (event.type) {
      case SDL_QUIT: running = false; break;
      case SDL_KEYDOWN : {
        auto key = event.key.keysym.scancode;
        Ptr num = to(Fixnum, key);
        vm_call_global(vm, onkey, 1, (Ptr[]){num});
        break;
      }
      case SDL_MOUSEMOTION: {
        auto x = event.motion.x;
        auto y = event.motion.y;
        auto p = (point){x, y};
        auto pt = to(Point, p);
        if (event.motion.state & SDL_BUTTON_LMASK) {
          vm_call_global(vm, onmousedrag, 1, (Ptr[]){pt});
        } else {
          vm_call_global(vm, onmousemove, 1, (Ptr[]){pt});
        }
        SDL_FlushEvent(SDL_MOUSEMOTION);
        break;
      }
      case SDL_MOUSEBUTTONDOWN: {
        auto x = event.button.x;
        auto y = event.button.y;
        auto p = (point){x, y};
        auto pt = to(Point, p);
        vm_call_global(vm, onmousedown, 1, (Ptr[]){pt});
        break;
      }
      }
      if (vm->error) {
        dbg(vm->error);
        break;
      }
    }
    SDL_UpdateWindowSurface(window);
    // SDL_Delay(10);
  }

  unprot_ptrs(onkey, onmousedrag, onmousemove, onmousedown);

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
  initialize_character_names();

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
