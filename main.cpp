/*

(progn
  (setq flycheck-clang-language-standard "c++14")
  (setq flycheck-clang-include-path '("/Users/jsn/Library/Frameworks"))
  (setq flycheck-clang-args '("-F" "/Users/jsn/Library/Frameworks")))

*/

// #define NDEBUG

#include <xmmintrin.h>
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

#define likely(x)   __builtin_expect((x),1)
#define unlikely(x) __builtin_expect((x),0)

#define _ostream_prefix(x) << x
#define dbg(...) std::cerr MAP(_ostream_prefix, __VA_ARGS__) << std::endl

struct run_info {
  int argc; const char** argv;
};

using std::string;

#define GC_DEBUG 0
#define PRIM_USE_GIANT_SWITCH 0
#define INCLUDE_REPL 0
#define DEBUG_IMAGE_SNAPSHOTS 0
#define STATS 0
#define UNCHECKED_UNWRAP 1
#define BA_STATIC_MEM 0

#if UNCHECKED_UNWRAP
  #define check(arg)
#else
  #define check(arg) assert(arg)
#endif

#define MOST_POSITIVE_FIXNUM 576460752303423487
#define MOST_NEGATIVE_FIXNUM -576460752303423487

/*
 latest perf reports on bouncers-2.
  
 GIANT_SWITCH OFF:
 executed 1443278943 instructions over 29.156 seconds.
 average of 49501953 ops per second, including sleep. 
 gc count: 12
 GIANT_SWITCH ON:
 executed 1611884478 instructions over 31.383 seconds.
 average of 51361707 ops per second, including sleep. 
 gc count: 14
 GIANT_SWITCH ON + NDEBUG:
 executed 5182630820 instructions over 78.84 seconds.
 average of 65736058 ops per second, including sleep. 
 gc count: 43
 */

#define unused(x) (void)(x)
#define maybe_unused(x) (void)(x)
#define KNOWN(symbol) vm->globals->known._##symbol
#define _deg_to_rad(x) (x) * (M_PI / 180.0f)

typedef unsigned int uint;
typedef __uint128_t u128;
typedef __int128_t s128;
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

#define SYSTEM_LANG_PACKAGE_KEY FIXNUM(0)
#define SYSTEM_CURRENT_THREAD_KEY FIXNUM(1)
#define SYSTEM_OTHER_THREADS_KEY FIXNUM(2)
#define SYSTEM_BUILTIN_CLASSES_KEY FIXNUM(3)
#define SYSTEM_CURRENT_COLOR_KEY FIXNUM(4)

// info that lives at the start of a snapshot image
struct image_header {
  u64 heap_size;
  u64 static_size;
  u64 static_memory_allocation_count;
};

#if STATS

struct stats {
  u64 total_string_bytes_allocated;
  u64 total_cons_bytes_allocated;
  u64 total_object_bytes_allocated;
  u64 total_closure_bytes_allocated;
  u64 total_bytecode_bytes_allocated;
  u64 flat_lambda_count;
  u64 closure_lambda_count;
};

void report_stats(stats *s) {
  dbg("total string bytes allocated: ", s->total_string_bytes_allocated);
  dbg("total cons bytes allocated: ", s->total_cons_bytes_allocated);
  dbg("total object bytes allocated (including slot vectors): ", s->total_object_bytes_allocated);
  dbg("total closure bytes: ", s->total_closure_bytes_allocated);
  dbg("total bytecode bytes: ", s->total_bytecode_bytes_allocated);
  dbg("emitted ", s->flat_lambda_count, " flat lambdas");
  dbg("emitted ", s->closure_lambda_count, " closure lambdas");
}

#endif

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

struct thread_ctx;
thread_ctx *make_thread_ctx();
void reset_thread_ctx(thread_ctx *ctx);
void thread_ctx_set_thread(thread_ctx *ctx, Ptr thread);
Ptr thread_ctx_get_thread(thread_ctx *ctx);
void _print_debug_stacktrace(thread_ctx *thd);

struct thdq_node { thread_ctx *val;  thdq_node *next; };
struct thdq { thdq_node *front, *back; thdq_node *free_list;  s64 count; };

thdq_node *thdq_get_node(thdq *q) {
  if (q->free_list) {
    auto res = q->free_list;
    q->free_list = q->free_list->next;
    return res;
  }
  auto result = (thdq_node *)calloc(sizeof(thdq_node), 1);
  return result;
}

void thdq_push(thdq *q, thdq_node *n) {
  q->count++;
  n->next = 0;
  if (!q->front) {
    q->front = q->back = n;
  } else {
    q->back->next = n;
    q->back = n;
  }
}

void thdq_insert_at_index(thdq *q, thdq_node *n, s64 idx) {
  // std::cerr << "inserting at: " << idx << std::endl;
  thdq_node *p = 0;
  thdq_node *curr = q->front;
  if (!curr) return thdq_push(q, n);
  if (idx >= q->count) return thdq_push(q, n);
  while (curr && idx >= 0) {
    if (!idx) {
      q->count++;
      if (!p) {
        n->next = q->front;
        q->front = n;
      } else {
        n->next = p->next;
        p->next = n;
      }
      return;
    }
    idx--;
    p = curr;
    curr = curr->next;
  }
  // should not happen
  // assert(false);
  thdq_push(q, n);
}

void thdq_insert_at_index(thdq *q, thread_ctx *ctx, s64 idx) {
  auto n = thdq_get_node(q);
  n->val = ctx;
  thdq_insert_at_index(q, n, idx);
}

void thdq_push_ptr(thdq *q, Ptr p) {
  auto n = thdq_get_node(q);
  n->val = make_thread_ctx();
  thread_ctx_set_thread(n->val, p);
  thdq_push(q, n);
}

void thdq_push(thdq *q, thread_ctx *p) {
  auto n = thdq_get_node(q);
  n->val = p;
  thdq_push(q, n);
}

thdq_node *thdq_pop(thdq *q) {
  if (q->front) {
    q->count--;
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

void thdq_remove_all(thdq *q) {
  q->count = 0;
  while (auto n = thdq_pop(q)) {
    n->next = q->free_list;
    q->free_list = n;
    free(n->val);
    n->val = 0;
  }
}

void thdq_remove_next(thdq *q, thdq_node *p) {
  if (!p) { // first item
    auto n = thdq_pop(q);
    if (n) {
      n->next = q->free_list;
      q->free_list = n;
      n->val = 0;
    }
    return;
  }
  q->count--;
  auto n = p->next;
  check(n);
  if (n == q->back) q->back = p;
  p->next = n->next;
  
  n->next = q->free_list;
  q->free_list = n;
  n->val = 0;
}

void thdq_remove_ptr(thdq *q, Ptr ptr) {
  thdq_node *p = 0;
  auto n = q->front;
  while (n) {
    if (thread_ctx_get_thread(n->val).value == ptr.value) {
      // FIXME: audit as we leak val here
      thdq_remove_next(q, p);
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
  U32Array_ObjectType,
  U16Array_ObjectType,
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
  check(idx >= 0);
  u8 uidx = idx;
  obj->header.custom_class = (1 << 7) | uidx;
}
bool object_has_custom_class(Object *obj) {
  return obj->header.custom_class >> 7;
}

#define set_obj_tag(obj,name) object_set_custom_class(obj, BuiltinClassIndex_##name)


struct VM;
// XXX want to get rid of this (don't want any global VM refs)
VM *DEBUG_VM;

/* ---------------------------------------- */
//          GC protection macros

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

struct U32ArrayObject : Object {
  u64 length;
  u32 data[];
};

struct U16ArrayObject : Object {
  u64 length;
  u16 data[];
};


enum PAOType : u32 {
  Array,
  Closure,
  Struct
};

struct PtrArrayObject : Object {
  PAOType pao_type;
  u32 length;
  Ptr data[];
};

struct ByteCodeObject : Object {
  U16ArrayObject *code;
  PtrArrayObject *literals;
  Ptr name;
  bool is_varargs;
};

struct StackFrameObject : Object {
  Ptr* prev_stack;
  StackFrameObject* prev_frame;
  ByteCodeObject* bc;
  s64 prev_pc;
  Ptr closed_over;
  Ptr special_variables;
  u64 special_count;
  Ptr mark;
  u64 preserved_argc; // used for snapshots;
  s64 argc;
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
std::ostream &operator<<(std::ostream &os, point p) {
  return os << "{" << p.x << "@" << p.y << "}";
}

struct thread_ctx {
  void *stack_mem;
  s64 curr_size;
  Ptr *stack;
  Ptr *stack_start;
  Ptr *stack_end;
  s64 stack_depth;
  StackFrameObject *frame;
  ByteCodeObject *bc;
  Ptr thread;
  bool has_run;
};

thread_ctx *make_thread_ctx() {
  auto result = (thread_ctx *)calloc(sizeof(thread_ctx), 1);
  auto count = 256;
  auto curr_size = count * sizeof(Ptr);
  void *stack_mem = calloc(curr_size, 1);
  Ptr *ptr_mem = (Ptr *)stack_mem;
  result->stack_mem = stack_mem;
  result->curr_size = curr_size;
  result->stack = ptr_mem + (count - 1);
  result->stack_start = result->stack;
  result->stack_end = ptr_mem; // + 1024; // padding
  result->has_run = false;
  return result;
}

// reset the stack of this ctx
void reset_thread_ctx(thread_ctx *ctx) {
  ctx->stack = ctx->stack_start;
}

void thread_ctx_set_thread(thread_ctx *ctx, Ptr thread) {
  ctx->thread = thread;
}
Ptr thread_ctx_get_thread(thread_ctx *ctx) {
  return ctx->thread;
}

bool pointer_is_aligned(void* mem);

struct _copy_state { Ptr *stack; StackFrameObject *frame; };

struct blit_surface {
  u8 *mem; s64 pitch, width, height;
};

struct static_memory {
  static_memory *next;
  u32 flags;
  u32 byte_count;
  u8 bytes[];
};

struct VM {
  s64 pc;
  u16 *curr_code;
  thread_ctx* curr_thd;
  StackFrameObject *curr_frame;
  thdq *threads;
  Ptr *curr_lits;
  Ptr system_dictionary;
  u16 current_color; // actual 'color' stored in top two bits
  static_memory *static_mem;
  u64 static_mem_size;
  u64 static_mem_allocation_count;
  void* heap_mem;
  void* heap_end;
  void* alt_heap_mem;
  void* alt_heap_end;
  void *collection_limit;
  u64 allocation_count;
  u64 heap_size_in_bytes;
  const char* error;
  u64 instruction_count;
  Globals *globals;
  bool gc_disabled;
  bool in_gc;
  u64 gc_count;
  u64 gc_threshold_in_bytes;
  u64 gc_compacted_size_in_bytes;
  u64 allocation_high_watermark;
  // std::unordered_map<Object **, u64> *gc_protected;
  std::vector<u64> *gc_protected_vec;
  // std::unordered_map<Ptr *, u64> *gc_protected_ptrs;
  std::vector<Ptr *> *gc_protected_ptrs_vec;
  std::unordered_map<Ptr *, u64> *gc_protected_ptr_vectors;
  blit_surface *surface;
  SDL_Window *window;
  bool screen_dirty;
  bool suspended;
  s64 start_time_ms;
  #if STATS
  stats *stats;
  #endif
};

// ----------------------------------------
// growable stack

// cf __vm_restore_stack_snapshot
_copy_state _copy_frame_to_new_stack(StackFrameObject *fr, Ptr *input_stack, Ptr*a,Ptr*b) {
  if (!fr) return (_copy_state){input_stack, 0};

  auto state = _copy_frame_to_new_stack(fr->prev_frame, input_stack, a, b);
  auto stack = state.stack;
  auto prev_frame = state.frame;

  if (fr->prev_frame) {
    s64 count =  (Ptr *)(void*)fr->prev_frame - (Ptr *)(void*)fr->prev_stack;
    for (s64 i = count - 1; i >= 0; i--) {
      auto it = fr->argv[i + fr->pad_count];
      *(--stack) = it;
    }
  }

  // alloc and align new frame;
  auto top = stack - (sizeof(StackFrameObject) / sizeof(u64));
  auto was_aligned = true;
  if (!pointer_is_aligned(top)) {
    top -= 1;
    was_aligned = false;
  }
  check(pointer_is_aligned(top));

  auto new_frame = (StackFrameObject *)(void *)top;
  memcpy(new_frame, fr, sizeof(StackFrameObject));

  new_frame->pad_count = was_aligned ? 0 : 1;
  new_frame->prev_stack = stack;
  new_frame->prev_frame = prev_frame;

  check(!prev_frame || ((void*)prev_frame <= (void*)a && (void*)prev_frame >= (void*)b));
  auto new_stack = (Ptr *)(void *)new_frame;

  return (_copy_state){ new_stack, new_frame};
}

_copy_state _copy_thread_stack_to_new_stack(thread_ctx *ctx, Ptr* input_stack, Ptr* a, Ptr* b) {
  auto state = _copy_frame_to_new_stack(ctx->frame, input_stack, a, b);
  auto stack = state.stack;

  auto on_stack = (Ptr*)(void *)ctx->frame; // go back 'up' the stack to get current args
  s64 count = on_stack - ctx->stack;
  for (auto i = count - 1; i >= 0; i--) {
    *(--stack) = ctx->stack[i];
  }
  return (_copy_state){stack, state.frame};
}

void vm_refresh_frame_state(VM *);

void grow_thread_ctx(VM *vm, thread_ctx *ctx) {
  auto new_curr_size   = ctx->curr_size * 2;
  std::cerr << ("growing thread stack: ") << new_curr_size << std::endl;
  // _print_debug_stacktrace(ctx);
  if (new_curr_size > 1000000) { puts("it's all over folks"); exit(1); }
  auto new_count       = new_curr_size / sizeof(Ptr);
  auto new_stack_mem   = calloc(new_curr_size, 1);
  check(pointer_is_aligned(new_stack_mem));
  auto new_ptr_mem     = (Ptr *)new_stack_mem;
  auto new_stack       = new_ptr_mem + (new_count - 1);
  auto new_stack_start = new_stack;
  auto new_stack_end   = new_ptr_mem;
  check(ctx->stack >= ctx->stack_end);
  check(new_stack > new_stack_end);
  check(new_stack <= new_stack_start);

  if (vm->curr_thd && vm->curr_thd->frame) { vm->curr_thd->frame->prev_pc = vm->pc; }
  auto state = _copy_thread_stack_to_new_stack(ctx, new_stack, new_stack_start, new_stack_end);
  new_stack = state.stack;
  auto new_frame = state.frame;
  free(ctx->stack_mem);
  check(ctx->stack_start - ctx->stack == new_stack_start - new_stack);

  ctx->stack_mem   = new_stack_mem;
  ctx->curr_size   = new_curr_size;
  ctx->stack       = new_stack;
  ctx->stack_start = new_stack_start;
  ctx->stack_end   = new_stack_end;
  ctx->frame       = new_frame;
  check(ctx->stack <= ctx->stack_start && ctx->stack >= ctx->stack_end);

  vm_refresh_frame_state(vm);
}

inline void thread_ctx_ensure_bytes(VM *vm,thread_ctx *ctx, s64 byte_count) {
  if (unlikely(ctx->stack - (byte_count / sizeof(Ptr)) <= ctx->stack_end)) {
    grow_thread_ctx(vm, ctx);
  } 
}



// ----------------------------------------
// static memory

static_memory *alloc_static_memory(VM *vm, u64 byte_count) {
  auto size = sizeof(static_memory) + byte_count;
  auto it = (static_memory *)calloc(size, 1);
  it->byte_count = byte_count;
  it->next = vm->static_mem;
  vm->static_mem = it;
  vm->static_mem_allocation_count++;
  vm->static_mem_size += size;
  return it;
}

// ----------------------------------------
// stats reporting

Ptr reset_stats_reporting(VM *vm) {
  maybe_unused(vm);
  #if STATS
  memset(vm->stats, 0, sizeof(stats));
  #endif
  return Nil;
}

Ptr print_stats_reporting(VM *vm) {
  maybe_unused(vm);
  #if STATS
  report_stats(vm->stats);
  #else
  dbg("must build with STATS enabled to print stats");
  #endif
  return Nil;
}

// ----------------------------------------

inline void unsafe_vm_refresh_frame_state(VM *vm){
  auto thd = vm->curr_thd;
  auto bc = thd->bc;
  vm->curr_frame = thd->frame;
  vm->curr_code  = bc->code->data ;
  vm->curr_lits  = bc->literals->data ;
  vm->pc = thd->frame->prev_pc ;
}

inline void vm_refresh_frame_state(VM *vm){
  auto thd = vm->curr_thd;
  if (thd) {
    auto bc = thd->bc;
    vm->curr_frame = thd->frame;
    vm->curr_code  = bc ? bc->code->data : 0;
    vm->curr_lits =  bc ? bc->literals->data : 0;
    vm->pc = thd->frame ? thd->frame->prev_pc : 0;
  } else {
    vm->curr_frame = 0;
    vm->curr_code  = 0;
    vm->curr_lits  = 0;
    vm->pc = 0;
  }
}

inline void vm_set_curr_thread(VM *vm, thread_ctx* thd) {
  if (thd != vm->curr_thd && vm->curr_thd && vm->curr_thd && vm->curr_thd->frame) {
    vm->curr_thd->frame->prev_pc = vm->pc;
  }
  vm->curr_thd = thd;
  vm_refresh_frame_state(vm);
}

Ptr update_display(VM *vm) {
  if (vm->window) SDL_UpdateWindowSurface(vm->window);
  vm->screen_dirty = false;
  return Nil;
}

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

#define die(...) do {                                         \
    print_stacktrace(stderr, 128);                            \
    std::cerr MAP(_ostream_prefix, __VA_ARGS__) << std::endl; \
    assert(false);                                            \
  } while(0)

void gc(VM *vm);

void * vm_alloc(VM *vm, u64 bytes) {
  auto result = (void *)vm->heap_end;

  #if DEBUG_GC
  assert(pointer_is_aligned(result));
  assert(pointer_is_aligned(vm->heap_end));
  #endif 

  auto new_heap_end = align_pointer_with_offset(vm->heap_end, bytes);

  auto past_end = new_heap_end > vm->collection_limit;

  if (unlikely(past_end && !vm->gc_disabled && !vm->in_gc)) {
      gc(vm);
      if (vm_heap_used(vm) + bytes + 16 > vm->heap_size_in_bytes) {
        die("heap exhausted after gc");
      }
      vm->gc_disabled = true;
      auto result = vm_alloc(vm, bytes);
      vm->gc_disabled = false;
      if (vm_heap_used(vm) > vm->heap_size_in_bytes) { die("heap exhausted"); }
      auto object = (Object *)result;
      object->header.flags = vm->current_color;
      return result;
  }

  vm->heap_end = new_heap_end;
  vm->allocation_count++;

  auto object = (Object *)result;
  object->header.flags = vm->current_color;

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
  Image,
  Bignum
} BAOType;

struct ByteArrayObject : Object {
  BAOType bao_type;
#if BA_STATIC_MEM
  static_memory *mem;
#else
  uint length;
  char data[];
#endif
};

inline u64 ba_length(ByteArrayObject *o) {
#if BA_STATIC_MEM
  return o->mem->byte_count;
#else
  return o->length;
#endif
}

inline char* ba_data(ByteArrayObject *o) {
#if BA_STATIC_MEM
  return (char *)o->mem->bytes;
#else
  return o->data;
#endif
}

inline u8* ba_mem(ByteArrayObject *o) {
#if BA_STATIC_MEM
  return (u8 *)o->mem->bytes;
#else
  return (u8*)o->data;
#endif
}

struct RawPointerObject : Object {
  void *pointer;
};

struct StandardObject : Object { 
  StandardObject *klass;
  PtrArrayObject *slots;
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

inline Ptr _ptr_creation_name(Ptr)(Object *it){
  return (Ptr){ ((u64) it) | 0b1 };
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

#if UNCHECKED_UNWRAP
#define object_type(type)                                               \
  type_test(type, it) {                                                 \
    return (is(NonNilObject, it) &&                                     \
            (as(Object, it))->header.object_type == type##_ObjectType); \
  };                                                                    \
  unwrap_ptr_for(type, it) {                                            \
    return (type##Object *)as(Object, it);                              \
  }
#else
#define object_type(type)                                               \
  type_test(type, it) {                                                 \
    return (is(NonNilObject, it) &&                                     \
            (as(Object, it))->header.object_type == type##_ObjectType); \
  };                                                                    \
  unwrap_ptr_for(type, it) {                                            \
   if (! is(type, it)) { die("unwrap of ", #type, " got ", it); }       \
    assert(is(type, it));                                               \
    return (type##Object *)as(Object, it);                              \
  }
#endif



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
  return (u8)(c.code_point >> (idx * 8));
}

inline s64 utf8_byte_width_for_char(u8 byte) {
  if (likely((byte & 0b10000000) == 0)) return 1;
  if ((byte & 0b00100000) == 0) return 2;
  if ((byte & 0b00010000) == 0) return 3;
  if ((byte & 0b00001000) == 0) return 4;
  return -1;
}

inline s64 character_byte_width(character c) {
  auto byte = character_byte_at(c, 0);
  auto result = utf8_byte_width_for_char(byte);
  return result;
}

inline bool character_eq(character a, character b) {
  return a.code_point == b.code_point;
}

inline u32 character_to_u32(character a) {
  return a.code_point;
}

inline s64 character_to_s64(character a) {
  s64 result = a.code_point;
  return result;
}

// FIXME is this the proper ordering?
inline bool character_lt(character a, character b) {
  return a.code_point < b.code_point;
}
// FIXME is this the proper ordering?
inline bool character_gt(character a, character b) {
  return a.code_point > b.code_point;
}

char *character_as_c_string(character c, char*result) {
  auto count = character_byte_width(c);
  for (auto i = 0; i < count; i++) {
    result[i] = character_byte_at(c, i);
  }
  return result;
}

prim_type(Char)
create_ptr_for(Char, char ch) {
  auto val = ((u64)ch << 32) | Char_Tag;
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

inline point operator +(point a, point b) { return (point){a.x + b.x, a.y + b.y}; }
inline point operator -(point a, point b) { return (point){a.x - b.x, a.y - b.y}; }
inline point operator *(point a, float s) { return (point){(s32)(a.x * s), (s32)(a.y * s)}; }

point rotate_point(point p, f32 degrees) {
  f32 angle = _deg_to_rad(fmod(degrees, 360.0));
  f32 c = cosf(angle);
  f32 s = sinf(angle);
  f32 x = p.x * c - p.y * s;
  f32 y = p.x * s + p.y * c;
  return (point){(s32)x, (s32)y};
}

prim_type(Point)
create_ptr_for(Point, point p) {
  // cout << " p.x = " << bitset<32>(p.x) << " p.y =" << bitset<32>(p.y) << endl;
  const auto mask      = (1ULL << 30) - 1;
  const auto high_mask = mask << 34;
  const auto low_mask  = mask << 4;

  u64 x_comp = (((u64)p.x) << 34) & high_mask;
  u64 y_comp = (((u64)p.y) << 4) & low_mask;

  u64 value = x_comp | y_comp | Point_Tag;
  return (Ptr){value};
}
unwrap_ptr_for(Point, it) {
  point p;
  auto mask = (1ULL << 30) - 1;
  u64 xbit = it.value & (1ULL << 63) ? 0b11 << 30 : 0b00 << 30;
  u64 ybit = it.value & (1ULL << 33) ? 0b11 << 30 : 0b00 << 30;
  // cout << " x < 0 ? " << xbit << "   y < 0 ? " << ybit << endl;
  auto val = it.value >> 4;
  u32 y = (val & mask) | (ybit);
  u32 x = ((val >> 30) & mask) | (xbit);
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
object_type(U32Array)
object_type(U16Array)
object_type(U64Array)
object_type(PtrArray)
object_type(Standard)
object_type(StackFrame)

type_test(BrokenHeart, it) {
  return is(Object, it) && as(Object, it)->header.object_type == BrokenHeart;
}

#undef prim_type
#undef object_type

inline Ptr prefetch(Ptr p) {
  if (is(NonNilObject, p)) {
    _mm_prefetch((char *)as(Object, p), _MM_HINT_T0);
  }
  return p;
}

#define VM_ARG(fname, type, name)                                       \
  Ptr _##name = vm_pop(vm);                                             \
  if (unlikely(!is(type, _##name))) {                                   \
    vm->error = "in: " fname " argument " #name " is not a " #type;     \
    return Nil;                                                         \
  }                                                                     \
  auto name = as(type, _##name);

/* ---------------------------------------- */

// TODO: convert this to type-test
inline bool isNil(Ptr self) {
  return self.value == Object_Tag;
}

U16ArrayObject *alloc_u16ao(VM *vm, uint len) {
  auto byte_count = sizeof(U16ArrayObject) + (len * sizeof(u16));
  U16ArrayObject* obj = (U16ArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = U16Array_ObjectType;
  set_obj_tag(obj, U16Array);
  obj->length = len;
  return obj;
}

U32ArrayObject *alloc_u32ao(VM *vm, uint len) {
  auto byte_count = sizeof(U32ArrayObject) + (len * sizeof(u32));
  U32ArrayObject* obj = (U32ArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = U32Array_ObjectType;
  set_obj_tag(obj, U32Array);
  obj->length = len;
  return obj;
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
#if BA_STATIC_MEM
  auto byte_count = sizeof(ByteArrayObject);
  ByteArrayObject* obj = (ByteArrayObject *)vm_alloc(vm, byte_count);
  auto mem = alloc_static_memory(vm, len);
  obj->mem = mem;
#else 
  auto byte_count = sizeof(ByteArrayObject) + len;
  ByteArrayObject* obj = (ByteArrayObject *)vm_alloc(vm, byte_count);
  obj->length = len;
#endif
  obj->header.object_type = ByteArray_ObjectType;
  obj->bao_type = ty;
  return obj;
}

type_test(String, it) {
  if (!is(ByteArray, it)) return false;
  auto bao = as(ByteArray, it);
  return bao->bao_type == String;
}
create_ptr_for(String, ByteArrayObject *it) {
  return to(Ptr, it);
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

type_test(Bignum, it) {
  if (!is(ByteArray, it)) return false;
  auto bao = as(ByteArray, it);
  return bao->bao_type == Bignum;
}
create_ptr_for(Bignum, ByteArrayObject *it) {
  return to(Ptr, it);
}
unwrap_ptr_for(Bignum, it) {
  return as(ByteArray, it);
}

u32 image_width(ByteArrayObject *it) {
  assert(it->bao_type == Image);
  return *(u32*)ba_data(it);
}
u32 image_height(ByteArrayObject *it) {
  assert(it->bao_type == Image);
  return *(((u32*)ba_data(it))+1);
}

u8* image_data(ByteArrayObject *it) {
  check(it->bao_type == Image);
  return (u8*)(((u32*)ba_data(it))+2);
}

ByteArrayObject *alloc_image(VM *vm, u32 w, u32 h) {
  auto byte_count = (w * h * 4) + 8;
  auto result = alloc_bao(vm, Image, byte_count);
  set_obj_tag(result, Image);
  auto u32s = (u32*)ba_data(result);
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
  return to(Ptr, img);
}

Ptr gfx_load_image(VM *vm, ByteArrayObject *path) {
  if (!is(String, to(Ptr, path))) die("load_image takes a string");
  auto str = string(ba_data(path), ba_length(path));
  auto result = load_image_from_path(vm, str);
  return result;
}
Ptr gfx_make_image(VM *vm, s64 w, s64 h) {
  return to(Ptr, alloc_image(vm, w, h));
}

blit_surface image_blit_surface(ByteArrayObject *img) {
  if (!is(Image, to(Ptr, img))) die("image_blit_surface requires an image");
  auto w = image_width(img), h = image_height(img);
  auto mem = image_data(img);
  auto pitch = w * 4;
  return (blit_surface){ mem, pitch, w, h};
}

blit_surface dummy_blit_surface(s64 w, s64 h) {
  return (blit_surface){ NULL, w, w, h};
}

PtrArrayObject *alloc_pao(VM *vm, PAOType ty, uint len) {
  auto byte_count = sizeof(PtrArrayObject) + (len * sizeof(Ptr));
  PtrArrayObject* obj = (PtrArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = PtrArray_ObjectType;
  obj->pao_type = ty;
  obj->length = len;
  return obj;
}

inline Ptr alloc_closure_env(VM *vm, uint count) {
  #if STATS
  vm->stats->total_closure_bytes_allocated += sizeof(PtrArrayObject) + (count + 1) * 8;
  #endif
  auto array = to(Ptr, alloc_pao(vm, Array, count + 1));
  return array;
}

type_test(Array, it) {
  return is(PtrArray, it) && (as(PtrArray, it))->pao_type == Array;
}
create_ptr_for(Array, PtrArrayObject *it) {
  return to(Ptr, it);
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


Ptr make_zf_array(VM *vm, u64 len);

StandardObject *alloc_standard_object(VM *vm, StandardObject *klass, u64 ivar_count) {
  gc_protect(klass);
  auto slot_vector = make_zf_array(vm, ivar_count); prot_ptr(slot_vector);
  auto byte_count = sizeof(StandardObject);
  #if STATS
  vm->stats->total_object_bytes_allocated += sizeof(PtrArrayObject) + ivar_count * 8;
  vm->stats->total_object_bytes_allocated += sizeof(StandardObject);
  #endif
  auto result = (StandardObject *)vm_alloc(vm, byte_count);
  gc_unprotect(klass);
  unprot_ptr(slot_vector);
  result->header.object_type = Standard_ObjectType;
  result->klass = klass;
  result->slots = as(PtrArray, slot_vector);
  return result;
}

Ptr standard_object_get_ivar(StandardObject *object, u64 idx) {
  // TODO: trigger VM error on OOB
  check(idx < object->slots->length);
  return object->slots->data[idx];
}

Ptr standard_object_set_ivar(StandardObject *object, u64 idx, Ptr value) {
  // TODO: trigger VM error on OOB
  check(idx < object->slots->length);
  return object->slots->data[idx] = value;
}

Ptr make_bytecode(VM *vm, u64 code_len) {
  #if STATS
  vm->stats->total_bytecode_bytes_allocated += sizeof(ByteCodeObject) +
    sizeof(U16ArrayObject) + code_len * 2;
  #endif
  auto bc = alloc_bytecode(vm);
  gc_protect(bc);
  auto code = alloc_u16ao(vm, code_len);
  gc_unprotect(bc);
  bc->code = code;
  return to(Ptr, bc);
}

Ptr create_bytecode(VM* vm, bool varargs, Ptr name, U16ArrayObject *code, PtrArrayObject *literals) {
  // TODO: stats
  prot_ptr(name);
  gc_protect(code); gc_protect(literals);
  auto bc = alloc_bytecode(vm);
  bc->is_varargs = varargs;
  bc->name       = name;
  bc->code       = code;
  bc->literals   = literals;
  gc_unprotect(literals);
  gc_unprotect(code);
  unprot_ptr(name);
  return to(Ptr, bc);
}
  
inline ByteArrayObject *alloc_bignum(VM *vm, s64 len) {
  #if STATS
  vm->stats->total_string_bytes_allocated += len;
  #endif
  ByteArrayObject *obj = alloc_bao(vm, Bignum, len);
  set_obj_tag(obj, Bignum);
  return obj;
}

inline ByteArrayObject *alloc_string(VM *vm, s64 len) {
  #if STATS
  vm->stats->total_string_bytes_allocated += len;
  #endif
  ByteArrayObject *obj = alloc_bao(vm, String, len);
  set_obj_tag(obj, String);
  return obj;
}

Ptr make_string(VM *vm, const char* str) {
  ByteArrayObject *obj = alloc_string(vm, strlen(str));
  const char *from = str;
  char *to = ba_data(obj);
  while(*from != 0) {
    *to = *from;
    to++; from++;
  }
  return to(Ptr, obj);
}

Ptr make_string_with_end(VM *vm, const char* str, const char* end) {
  // die(" make string with end: ", end - str);
  ByteArrayObject *obj = alloc_string(vm, end - str);
  const char *from = str;
  char *to = ba_data(obj);
  while(from < end) {
    *to = *from;
    to++; from++;
  }
  return to(Ptr, obj);
}

char string_byte_at(VM *vm, ByteArrayObject *str, s64 index) {
  if (index >= ba_length(str)) {
    vm->error = "string index out of range";
    return -1;
  }
  return (char)(ba_data(str)[index]);
}

character string_char_at(VM *vm, ByteArrayObject *str, s64 index) {
  if (unlikely(index >= ba_length(str))) {
    vm->error = "string index out of range";
    return (character){0};
  }
  u8 *data = (u8 *)(ba_data(str)) + index;
  u8 byte = *data;

  auto width = utf8_byte_width_for_char(byte);
  u32 code_point = byte;
  data++;
  switch(width) {
  case 1: {
    return (character){code_point};
  }
  case 2: {
    code_point |= *data << 8;
    break;
  }
  case 3: {
    code_point |= *data << 8; data++;
    code_point |= *data << 16;
    break;
  }
  case 4: {
    code_point |= *data << 8; data++;
    code_point |= *data << 16; data++;
    code_point |= *data << 24;
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
  if (index > ba_length(str) + width) {
    vm->error = "string index out of range";
  } else {
    for (auto i = 0; i < width; i++) {
      ba_data(str)[index + i] = character_byte_at(ch, i);
    }
  }
  return Nil;
}

Ptr make_filled_string(VM *vm, s64 count, character ch) {
  auto width = character_byte_width(ch);
  auto size = count * width;
  auto s = alloc_string(vm, size);
  // memset(s->data, ch, size);
  // TODO this is slow
  for (auto i = 0; i < size; i+=width) {
    string_set_char_at(vm, s, i, ch);
  }
  return to(Ptr, s);
}

Ptr string_substr_byte_range(VM *vm, ByteArrayObject *str, s64 start, s64 end) {
  gc_protect(str);
  auto len = end - start;
  // @speed should just allocate inline here instead
  auto result = make_filled_string(vm, len, from(Char, to(Char, '0')));
  const char *read = ba_data(str) + start;
  auto write = from(String, result);
  for (auto i = 0; i < len; i++) {
    ba_data(write)[i] = read[i];
  }
  gc_unprotect(str);
  return result;
}


inline s64 string_byte_length(ByteArrayObject *str) {
  return ba_length(str);
}

s64 string_char_count(ByteArrayObject *str){
  auto count = 0;
  auto i = 0;
  while (i < ba_length(str)) {
    u8 byte = ba_data(str)[i];
    count++;
    i += utf8_byte_width_for_char(byte);
  }
  return count;
}

u32 hash_code(Ptr it);
bool string_equal(ByteArrayObject *a, ByteArrayObject*b) {
  if (hash_code(to(Ptr, a)) != hash_code(to(Ptr, b))) return false;
  if (ba_length(a) != ba_length(b)) return false;
  return memcmp(ba_data(a), ba_data(b), ba_length(a)) == 0;
}

inline Ptr make_number(s64 value) { return to(Fixnum, value); }

Ptr make_zf_array(VM *vm, u64 len) {
  auto array = alloc_pao(vm, Array, len); // alloc is zf
  set_obj_tag(array, Array);
  return to(Ptr, array);
}


Ptr make_zf_struct(VM *vm, u64 len, Ptr tag) { prot_ptr(tag);
  auto array = alloc_pao(vm, Struct, len + 1); // alloc is zf
  array->data[0] = tag;
  unprot_ptr(tag);
  return to(Ptr, array);
}

inline s64 array_length(PtrArrayObject *it) {
  return it->length;
}

inline Ptr array_get(Ptr array, u64 index) {
  auto a = as(PtrArray, array);
  check(index < a->length);
  return a->data[index];
}

inline Ptr aget(PtrArrayObject *a, u64 index) {
  assert(index < a->length);
  return a->data[index];
}

inline u16 aget(U16ArrayObject *a, u64 index) {
  assert(index < a->length);
  return a->data[index];
}

inline void array_set(Ptr array, u64 index, Ptr value) {
  auto a = as(PtrArray, array);
  check(index < a->length);
  a->data[index] = value;
}

inline Ptr aset(PtrArrayObject *a, u64 index, Ptr value) {
  assert(index < a->length);
  a->data[index] = value;
  return Nil;
}

inline Ptr aset(U16ArrayObject *a, u64 index, s64 value) {
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
  auto result = alloc_string(vm, size);
  {
    auto array = to(Ptr, chars);
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
  check(used <= cap);
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

inline Ptr *xarray_memory(Ptr array) {
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
  check(idx < xarray_used(array));
  return xarray_memory(array)[idx];
}

// ----------------------------------------
// -- math stuff --

Ptr make_bignum(VM *vm, s64 byte_count, u8 bytes[]) {
  {
    auto neg = ((s8 *)bytes)[0] < 0;
    auto max = byte_count - 2;
    auto trunc = 0;
    if (neg) {
      for (auto i = 0; i >= max; i++) {
        if (bytes[i] == 0xff && ((s8*)bytes)[i + 1] < 0) {
          trunc++;
        } else {
          break;
        }
      }
    } else {
      for (auto i = 0; i >= max; i++) {
        if (bytes[i] == 0 && ((s8*)bytes)[i + 1] >= 0) {
          trunc++;
        } else {
          break;
        }
      }
    }
    byte_count -= trunc;
    bytes += trunc;
  }
  auto result = alloc_bignum(vm, byte_count);
  auto mem = ba_mem(result);
  for (auto i = 0; i < byte_count; i++) { mem[i] = bytes[i]; }
  return to(Ptr, result);
}

inline bool is_bignum_negative(ByteArrayObject *b) {
  return ((s8 *)ba_data(b))[0] < 0;
}

Ptr s128_to_bignum(VM *vm, s128 r) {
  union {
    u128 whole;
    u64 qwords[2];
    u8 bytes[16];
  } bits;

  bits.whole = (u128)r;
  auto tmp = bits.qwords[1];
  bits.qwords[1] = (u64)htonll(bits.qwords[0]);
  bits.qwords[0] = (u64)htonll(tmp);
  return make_bignum(vm, 16, bits.bytes);
}

inline Ptr fixnum_mul(VM *vm, s64 a, s64 b) {
  s128 r = (s128)a * (s128)b;
  if (likely(r <= MOST_POSITIVE_FIXNUM && r >= MOST_NEGATIVE_FIXNUM)) { return to(Fixnum, r); }
  return s128_to_bignum(vm, r);
}

inline Ptr fixnum_add(VM *vm, s64 a, s64 b) {
  s128 r = (s128)a + (s128)b;
  if (likely(r <= MOST_POSITIVE_FIXNUM && r >= MOST_NEGATIVE_FIXNUM)) { return to(Fixnum, r); }
  return s128_to_bignum(vm, r);
}

inline Ptr fixnum_sub(VM *vm, s64 a, s64 b) {
  s128 r = (s128)a -(s128)b;
  if (likely(r <= MOST_POSITIVE_FIXNUM && r >= MOST_NEGATIVE_FIXNUM)) { return to(Fixnum, r); }
  return s128_to_bignum(vm, r);
}

typedef std::vector<u8> lh;
lh *longhand_add(lh *a, lh *b) {
  int carry = 0;
  auto result = new lh;
  auto idx = 0;
  lh *bigger, *smaller;
  if (a->size() > b->size()) {bigger = a; smaller = b;}
  else {bigger = b; smaller = a; }
  while (idx < smaller->size()) {
    auto n = a->at(idx) + b->at(idx) + carry;
    carry = n / 10;
    result->push_back(n % 10);
    idx++;
  }
  while (idx < bigger->size()) {
    auto n = bigger->at(idx) + carry;
    carry = n / 10;
    result->push_back(n % 10);
    idx++;
  }
  if (carry) { result->push_back(carry); }
  return result;
}

lh *longhand_mul_digit(s64 places, s64 digit, lh *a) {
  auto result = new lh;
  while (places--) { result->push_back(0); }
  int carry = 0;
  auto idx = 0;
  while (idx < a->size()) {
    auto n = a->at(idx) * digit + carry;
    carry = n / 10;
    result->push_back(n % 10);
    idx++;
  }
  if (carry) { result->push_back(carry); }
  return result;
}

lh *longhand_mul(lh*a, lh *b) {
  lh *result = 0;
  auto idx = 0;
  while (idx < a->size()) {
    auto digit = a->at(idx);
    auto one = longhand_mul_digit(idx, digit, b);
    if (!result) { result = one; }
    else {
      auto new_result = longhand_add(result, one);
      delete result;
      result = new_result;
    }
    idx++;
  }
  return result;
}

// FIXME: we drop the sign in this conversion, no need really.
lh *bignum_to_longhand(ByteArrayObject *ba) {
  bool neg = is_bignum_negative(ba);
  lh *pw2 = new lh;
  pw2->push_back(1);
  auto mem = ba_mem(ba);
  auto idx = ba_length(ba);
  auto res = new lh;
  res->push_back(neg ? 1 : 0);
  while (idx--) {
    auto byte = neg ? ~(mem[idx]) : mem[idx];
    auto bit = 8;
    while (bit--) {
      auto isset = byte & 0b1;
      if (isset) {
        auto new_res = longhand_add(res, pw2);
        delete res;
        res = new_res;
      }
      auto new_pw2 = longhand_mul_digit(0, 2, pw2);
      delete pw2; pw2 = new_pw2;
      byte = byte >> 1;
    }
  }
  delete pw2;
  return res;
}

ByteArrayObject *bignum_from_fixnum(VM *vm, s64 n) {
  union {
    s64 num;
    u8  bytes[4];
  } bits;
  bits.num = htonl(n);
  return as(Bignum, make_bignum(vm, 4, bits.bytes));
}

ByteArrayObject *bignum_copy(VM *vm, ByteArrayObject *n) {
  return as(Bignum, make_bignum(vm, ba_length(n), ba_mem(n)));
}

ByteArrayObject *bignum_add(VM *vm, ByteArrayObject *a, ByteArrayObject *b);

void bignum_negate_in_place(ByteArrayObject *r) {
  auto len = ba_length(r);
  auto mem = ba_mem(r);
  int carry = 1;
  int idx = len;
  while (idx--) {
    u8 n = ~mem[idx];
    s16 sum = n + carry;
    mem[idx] = (u8)(sum % 256);
    carry = sum / 256;
  }
}

ByteArrayObject *bignum_negate(VM *vm, ByteArrayObject *n) {
  auto r = bignum_copy(vm, n);
  bignum_negate_in_place(r);
  return r;
}

ByteArrayObject *bignum_add(VM *vm, ByteArrayObject *a, ByteArrayObject *b) {
  auto a_len = ba_length(a), b_len = ba_length(b);

  ByteArrayObject *bigger, *smaller;
  if (a_len < b_len) { bigger = b; smaller = a;}
  else { bigger = a; smaller = b;}
  auto bigger_mem = ba_mem(bigger);
  auto smaller_mem = ba_mem(smaller);

  auto a_mem = ba_mem(a), b_mem = ba_mem(b);
  auto smaller_neg = ((s8 *)smaller_mem)[0] < 0;

  auto min_len = std::min(a_len, b_len);
  auto max_len = std::max(a_len, b_len);
  auto c_len = max_len + 1;
  u8 c_mem[c_len];
  memset(c_mem, 0, c_len);

  u16 carry = 0;
  s32 idx = 0;
  while (idx++ < min_len) {
    u16 sum = a_mem[a_len - idx] + b_mem[b_len - idx] + carry;
    c_mem[c_len - idx] = sum % 256;
    carry = sum / 256;
  }
  idx--;
  while (idx++ < max_len) {
    auto n = bigger_mem[max_len - idx];
    // sign extend the smaller number if needed
    u16 sum = smaller_neg ? n + 0xff + carry : n + carry;
    c_mem[c_len - idx] = sum % 256;
    carry = sum / 256;
  }
  idx--;

  //TODO: makes me uncomofortable to drop the carry, but it seems to be what works

  auto prev_byte = ((s8 *)c_mem)[c_len - idx];

  if (prev_byte < 0) {
    // could also set top byte to 0xFF, but may as well drop it
    auto result = make_bignum(vm, c_len - 1, &(c_mem[1]));
    return as(Bignum, result);
  }

  // TODO: truncate empty bytes
  auto result = make_bignum(vm, c_len, c_mem);
  return as(Bignum, result);
}

ByteArrayObject *bignum_sub(VM *vm, ByteArrayObject *a, ByteArrayObject *b) {
  gc_protect(b);
  bignum_negate_in_place(b);
  auto r = bignum_add(vm, a, b);
  bignum_negate_in_place(b);
  gc_unprotect(b);
  return r;
}

inline void _bignum_add_number_and_carry(std::vector<u8> &num, u32 k, u32 to_add) {
  auto len = num.size();
  auto inner_sum = num[len - k] + to_add;
  num[len - k] = inner_sum % 256;
  auto inner_carry = inner_sum / 256;
  while (inner_carry) {
    k++;
    inner_sum = num[len - k] + inner_carry;
    num[len - k] = inner_sum % 256;
    inner_carry = inner_sum / 256;
  }
}

ByteArrayObject *bignum_mul(VM *vm, ByteArrayObject *a, ByteArrayObject *b) {
  auto a_neg = is_bignum_negative(a), b_neg = is_bignum_negative(b);
  if (a_neg) bignum_negate_in_place(a);
  if (b_neg) bignum_negate_in_place(b);

  auto a_mem = ba_mem(a), b_mem = ba_mem(b);
  auto a_len = ba_length(a), b_len = ba_length(b);
  auto len = a_len + b_len;

  std::vector<u8>num(len);
  for (auto i = 0; i < len; i++) { num[i] = 0; }

  auto offs = 0;
  for (auto i = 1; i <= a_len; i++) {
    auto n = a_mem[a_len - i];
    auto carry = 0;
    for (auto j = 1; j <= b_len; j++) {
      auto sum = n * b_mem[b_len - j] + carry;
      carry = sum / 256;

      auto k = j + offs;
      auto to_add = (sum % 256);
      _bignum_add_number_and_carry(num, k, to_add);
    }
    if (carry) {
      auto k = offs + b_len + 1;
      _bignum_add_number_and_carry(num, k, carry);
    }
    offs++;
  }

  if (a_neg) bignum_negate_in_place(a);
  if (b_neg) bignum_negate_in_place(b);

  auto neg = (a_neg || b_neg) && !(a_neg && b_neg);
  auto result = as(Bignum, make_bignum(vm, len, num.data()));
  if (neg) { bignum_negate_in_place(result); }

  return result;
}

s32 bignum_cmp(ByteArrayObject *a, ByteArrayObject *b) {
  auto a_neg = is_bignum_negative(a), b_neg = is_bignum_negative(b);
  auto inverted = false;

  if (a_neg && b_neg) inverted = true;
  else if (a_neg) return -1;
  else if (b_neg) return 1;

  auto a_len = ba_length(a), b_len = ba_length(b);

  if (a_len < b_len) return inverted ? 1 : -1;
  if (a_len > b_len) return inverted ? -1 : 1;

  auto a_mem = ba_mem(a), b_mem = ba_mem(b);
  if (inverted) {
    for (auto i = 0; i < a_len; i++) {
      auto a_byte = ~a_mem[i], b_byte = ~b_mem[i];
      if (a_byte > b_byte) return -1;
      if (a_byte < b_byte) return 1;
    }
  } else {
    for (auto i = 0; i < a_len; i++) {
      auto a_byte = a_mem[i], b_byte = b_mem[i];
      if (a_byte < b_byte) return -1;
      if (a_byte > b_byte) return 1;
    }
  }
  return 0;
}

bool bignum_lt(ByteArrayObject *a, ByteArrayObject *b) {
  return bignum_cmp(a, b) == -1;
}
bool bignum_gt(ByteArrayObject *a, ByteArrayObject *b) {
  return bignum_cmp(a, b) == 1;
}
bool bignum_eql(ByteArrayObject *a, ByteArrayObject *b) {
  return bignum_cmp(a, b) == 0;
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
      obj->header.hashcode = djb2((u8*)ba_data(str), ba_length(str));
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

#if UNCHECKED_UNWRAP
#define _define_structure_accessors(slot, name, idx)         \
  inline Ptr name##_get_##slot(Ptr obj) {                    \
    return as(PtrArray, obj)->data[idx + 1];                 \
  }                                                          \
  inline void name##_set_##slot(Ptr obj, Ptr value) {        \
    as(PtrArray, obj)->data[idx + 1] = value;                \
  }
#else
void _print_debug_stacktrace(thread_ctx *);
#define _define_structure_accessors(slot, name, idx)         \
  Ptr name##_get_##slot(Ptr obj) {                           \
    if (! is(name, obj)) { \
      _print_debug_stacktrace(DEBUG_VM->curr_thd);           \
      die("GOT ", #name, #slot, obj); }                      \
                                                             \
    assert(is(name, obj));                                   \
    return array_get(obj, idx + 1);                          \
  }                                                          \
  void name##_set_##slot(Ptr obj, Ptr value) {               \
    assert(is(name, obj));                                   \
    array_set(obj, idx + 1, value);                          \
  }
#endif

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
  check(is(Struct, it));
  return (StructTag)as(Fixnum, array_get(it, 0));
}

/* ---------------------------------------- */

defstruct(cons, Cons, car, cdr);
defstruct(Symbol, Symbol, name, package, value, flags, meta);
defstruct(package, Package, name, symtab, exports, use_list, subpackages, meta);
defstruct(ht, HashTable, array, dedupe_strings, count);
defstruct(cont, Continuation,
          stack_top,
          stack,
          value);
defstruct(thread, Thread,
          thunk,
          status,
          semaphore,
          wake_after,
          priority,
          local_bindings,
          suspension);

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

inline Ptr make_closure(VM *vm, Ptr code, Ptr env) { prot_ptrs(code, env);
  // assert(is(ByteCode, code));
  // assert(isNil(env) || is(PtrArray, env));
  auto it = alloc_pao(vm, Closure, 2);
  set_obj_tag(it, Closure);
  #if STATS
  vm->stats->total_closure_bytes_allocated += sizeof(PtrArrayObject) + 2 * 8;
  #endif
  it->data[0] = code;
  it->data[1] = env;
  unprot_ptrs(code, env);
  return to(Ptr, it);
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
  auto wrote = fwrite(ba_data(str), 1, ba_length(str), file);
  return wrote == ba_length(str);
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

u64 obj_size(U16ArrayObject *it)  { return sizeof(U16ArrayObject) + it->length * 2;    }
u64 obj_size(U32ArrayObject *it)  { return sizeof(U32ArrayObject) + it->length * 4;    }
u64 obj_size(U64ArrayObject *it)  { return sizeof(U64ArrayObject) + it->length * 8;    }
u64 obj_size(ByteCodeObject *)    { return sizeof(ByteCodeObject) + 0;                 }
#if BA_STATIC_MEM
u64 obj_size(ByteArrayObject *it) { return sizeof(ByteArrayObject);                    }
#else
u64 obj_size(ByteArrayObject *it) { return sizeof(ByteArrayObject) + it->length;       }
#endif
u64 obj_size(PtrArrayObject *it)  { return sizeof(PtrArrayObject) + it->length * 8;    }
u64 obj_size(StandardObject *)    { return sizeof(StandardObject);                     }
u64 obj_size(StackFrameObject*it) {
  return sizeof(StackFrameObject) + (it->argc + it->pad_count) * 8;
}

Object *gc_forwarding_address(Object *obj);

auto size_of(Ptr it) {
  if (isNil(it) || !is(Object, it)) return (u64)0;
  // TODO: switch off of header object type
  if (is(U16Array, it))   return obj_size(as(U16Array, it));
  if (is(U32Array, it))   return obj_size(as(U32Array, it));
  if (is(U64Array, it))   return obj_size(as(U64Array, it));
  if (is(ByteCode, it))   return obj_size(as(ByteCode, it));
  if (is(ByteArray, it))  return obj_size(as(ByteArray, it));
  if (is(PtrArray, it))   return obj_size(as(PtrArray, it));
  if (is(Standard, it))   return obj_size(as(Standard, it));
  if (is(StackFrame, it)) return obj_size(as(StackFrame, it));

  if (is(BrokenHeart, it)) {
    auto ref = gc_forwarding_address(as(Object, it));
    dbg("broken heart in size_of, ", it);
    dbg("forwarding to: ", to(Ptr, ref));
  }
  die("unexpected object type in size_of: ", (as(Object, it))->header.object_type);
  exit(1);
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
  it->bc = (ByteCodeObject *)as(void, fn(to(Ptr, it->bc)));
  if (it->prev_frame) {
    it->prev_frame = (StackFrameObject *)as(void, fn(to(Ptr, it->prev_frame)));
  }
}

void bang_refs(ByteCodeObject *it, BangPtrFn fn) {
  it->code = (U16ArrayObject *)as(void, fn(to(Ptr, it->code)));
  it->literals = (PtrArrayObject *)as(void, fn(to(Ptr, it->literals)));
  it->name = fn(it->name);
}
void bang_refs(PtrArrayObject *it, BangPtrFn fn) {
  for (u64 i = 0; i < it->length; i++) {
    it->data[i] = fn(it->data[i]);
  }
}

void bang_refs(StandardObject *it, BangPtrFn fn) {
  it->klass = (StandardObject *)as(void, fn(to(Ptr, it->klass)));
  it->slots = (PtrArrayObject *)as(void, fn(to(Ptr, it->slots)));
}

void scan_heap(void *start, void *end, PtrFn fn);

void bang_heap(void *start, void *end, BangPtrFn fn) {
  scan_heap(start, end, [&](Ptr it) {
      if (isNil(it) || !is(Object, it)) return;
      // TODO: switch off header object type
      if (is(U16Array, it))   return; // no refs
      if (is(U32Array, it))   return; // no refs
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
  fn(to(Ptr, it->bc));
  if (it->prev_frame) fn(to(Ptr, it->prev_frame));
}

void obj_refs(ByteCodeObject *it, PtrFn fn) {
  fn(to(Ptr, it->code));
  fn(to(Ptr, it->literals));
  fn(it->name);
}
void obj_refs(PtrArrayObject *it, PtrFn fn) {
  for (u64 i = 0; i < it->length; i++) {
    fn(it->data[i]);
  }
}

void obj_refs(StandardObject *it, PtrFn fn) {
  fn(to(Ptr, it->klass));
  fn(to(Ptr, it->slots));
}

void map_refs(Ptr it, PtrFn fn) {
  if (isNil(it) || !is(Object, it)) return;
  // TODO: switch off object header type
  if (is(U16Array, it))   return; // no refs
  if (is(U32Array, it))   return; // no refs
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
  BaseClassIvarNames  = 5,
  BaseClassEnd        = 6
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
  auto lookup = string((char *)ba_data(str), ba_length(str));
  if (character_codes_by_name.find(lookup) == character_codes_by_name.end()) {
    return Nil;
  } else {
    return to(Char, character_codes_by_name[lookup]);
  }
}

Ptr character_name(VM *vm, character ch) {
  if (character_byte_width(ch) == 1) {
    auto byte = character_byte_at(ch, 0);
    return make_string(vm, character_names_by_code[byte].c_str());
  } else {
    char buf[5] = {0};
    auto str = character_as_c_string(ch, (char *)&buf);
    return make_string(vm, str);
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
    case String: {
      os << "\"";
      auto len = ba_length(vobj);
      auto mem = ba_data(vobj);
      for (uint i = 0; i < len; i++) {
        os << mem[i];
      }
      os << "\"";
      return os;
    }
    case Image: {
      auto w = image_width(vobj); auto h = image_height(vobj);
      return os << "#<Image w=" << w << " h=" << h << " " << (void*)vobj << ">";
    }
    case Bignum: {
      auto neg = is_bignum_negative(vobj);
      if (neg) os << "-";
      auto nums = bignum_to_longhand(vobj);
      for (auto it = nums->rbegin(); it != nums->rend(); it++) {
        os << (u16)*it;
      }
      delete nums;
      return os;
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
      if (fn) { fn(os, to(Ptr, obj)); }
      else { os << "#<Struct[" << index << "] " << (void *)obj << ">" ;}
      return os;
    }
    case Closure: { return os << "#<Closure " << (void *)obj << ">" ;}
    case Array:
      os << "[";
      if (vobj->length > 0) {
        os << vobj->data[0];
      }
      for (uint i = 1; i < vobj->length; i++) {
        os << " " << vobj->data[i];
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
      os << "#<Class " << as(Object, name) << " " << (void*)obj << ">";
    } else {
      auto name = standard_object_get_ivar(klass, BaseClassName);
      os << "#<A " << as(Object, name) << " " << (void*)obj << ">";
    }
    return os;
  }
  case ByteCode_ObjectType: {
    auto bc = (ByteCodeObject *)obj;
    os << "#<ByteCode " << bc->name << " " << (void*)obj << ">";
    return os;
  }
  case U16Array_ObjectType: {
    auto len = ((const U16ArrayObject *)obj)->length;
    return os << "#<U16Array (" << len << ") "<< (void*)obj << ">";
  }
  case U32Array_ObjectType: {
    auto len = ((const U32ArrayObject *)obj)->length;
    return os << "#<U32Array (" << len << ") "<< (void*)obj << ">";
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
  auto f = vm->curr_thd->frame;
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
auto vm_map_thread_ctx_refs(VM *vm, thread_ctx *ctx, PtrFn fn) {
  unused(vm);
  fn(ctx->thread);
  fn(to(Ptr, ctx->bc));
  StackFrameObject *fr = ctx->frame;
  Ptr *stack = ctx->stack;
  while (fr) {
    fn(fr->special_variables);
    fn(fr->closed_over);
    fn(fr->mark);
    auto pad = fr->pad_count;
    for (u64 i = 0; i < fr->argc; i++) {
      auto arg = fr->argv[pad + i];
      fn(arg);
    }
    if (fr->bc) fn(to(Ptr, fr->bc));
    auto on_stack = (Ptr*)(void *)fr; // go back 'up' the stack to get current args
    while (on_stack > stack) {
      on_stack--;
      fn(*on_stack);
    }
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
  }
}
auto vm_map_stack_refs(VM *vm, PtrFn fn) {
  vm_map_thread_ctx_refs(vm, vm->curr_thd, fn);
  auto n = vm->threads->front;
  while(n) {
    vm_map_thread_ctx_refs(vm, n->val, fn);
    n = n->next;
  }
}

auto vm_print_stack_trace(VM *vm) { return Nil;
  StackFrameObject *fr = vm->curr_thd->frame;
  Ptr *stack = vm->curr_thd->stack;
  dbg("PRINTING STACKTRACE: ", vm->curr_thd->stack_depth);
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
  auto bottom = vm->curr_thd->stack;
  auto top = (Ptr *)(void *)vm->curr_thd->frame;
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
  StackFrameObject *fr = vm->curr_thd->frame;
  debug_walk(vm, to(Ptr, fr));
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
  Ptr lang_package;
  Ptr call1;
  struct {
    Ptr _lambda, _quote, _if, _let, _fixnum, _cons, _string, _array, _character, _boolean, _quasiquote, _unquote, _unquote_splicing, _compiler, _set_bang, _exception, _run_string, _with_special_binding, _XpackageX, _Xsource_locationX;
  } known;

};

/* ---------------------------------------- */
// @cleanup this function shows that the object model is too complex IMO

Ptr class_of(VM *vm, Ptr it) {
#define builtin(name) to(Ptr, vm->globals->classes._##name)
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
    if (it == Nil) return builtin(Null);
    auto obj = as(Object, it);
    if (obj->header.custom_class) {
      u8 idx     = obj->header.custom_class & 0b01111111;
      auto klass = vm->globals->classes.builtins[idx];
      return to(Ptr, klass);
    } else {
      return to(Ptr, as(Standard, it)->klass);
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
    ref_count++;
    #ifndef NDEBUG
    auto ptr = as(Object, it);
    auto ptr_val = (u64)ptr;
    auto where = ptr_val + delta;
    assert((void*)where >= start && (void*)where < end);
    #endif
  };
  scan_heap(start, end, [&](Ptr it) {
      count++;
      return map_refs(it, fn);
    });
  dbg(count, " objects on heap, ", ref_count, " in-heap references ");
}

#define COLOR_MASK 0b1100000000000000

u16 vm_advance_color(VM *vm) {
  vm->current_color = ((((vm->current_color >> 14) & 0b11) + 1) << 14) & COLOR_MASK;
  // dbg("set current color: ", std::bitset<16>(vm->current_color));
  return vm->current_color;
}

bool vm_is_object_marked(VM *vm, Ptr it, u16 color) {
  unused(vm);
  if (!is(NonNilObject,it)) return true;
  auto obj = as(Object, it);
  return (obj->header.flags & COLOR_MASK) == color;
} 

void vm_mark_object(VM *vm, Ptr it, u16 color) {
  unused(vm);
  if (!is(NonNilObject,it)) return;
  auto obj = as(Object, it);
  obj->header.flags &= ~COLOR_MASK;
  obj->header.flags |= color;
}

// @unsafe
auto vm_map_reachable_refs(VM *vm, PtrFn fn) {
  std::vector<Ptr> *q = new std::vector<Ptr>;
  auto grey  = vm_advance_color(vm);
  auto black = vm_advance_color(vm);

  PtrFn enqueue = [&](Ptr it) {
    if (vm_is_object_marked(vm, it, grey) || vm_is_object_marked(vm, it, black)) return;
    vm_mark_object(vm, it, grey);
    q->push_back(it);
  };

  PtrFn recurse = [&](Ptr it) {
    if (vm_is_object_marked(vm, it, black)) return;
    vm_mark_object(vm, it, black);
    fn(it);
    map_refs(it, enqueue);
  };

  vm_map_stack_refs(vm, recurse);
  recurse(vm->globals->lang_package);
  recurse(vm->globals->call1);

#define handle_class(name) recurse(to(Ptr, vm->globals->classes._##name));
#define X(...) MAP(handle_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X
#undef handle_class

  for (auto i = 0; i < BuiltinClassIndexEnd; i++) {
    recurse(to(Ptr, vm->globals->classes.builtins[i]));
  }

  recurse(vm->system_dictionary);

  while (q->size()) {
    auto it = q->at(q->size() - 1);
    q->pop_back();
    recurse(it);
  }

  delete q;
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
    auto it = to(Ptr, (Object *)start);
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
  if (vm->curr_thd && vm->curr_thd->frame) {
    vm->curr_thd->frame->prev_pc = vm->pc;
  }
}

bool gc_is_broken_heart(Object *obj) {
  return obj->header.object_type == BrokenHeart;
}

void gc_break_heart(Object *obj, Object *forwarding_address) {
  check(!gc_is_broken_heart(obj));
  obj->header.object_type = BrokenHeart;
  ((u64*)obj)[1] = (u64)forwarding_address;
}

Object *gc_forwarding_address(Object *obj) {
  check(gc_is_broken_heart(obj));
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
  return to(Ptr, to);
}

Ptr gc_move_object(VM *vm, Object *obj) {
  check(!gc_is_broken_heart(obj));
  auto ptr = to(Ptr, obj);
  check(size_of(ptr) > 0);
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
  check(!gc_ptr_is_in_to_space(vm, obj));
  if (!gc_is_broken_heart(obj)) {
    gc_move_object(vm, obj);
  }
  auto new_addr = gc_forwarding_address(obj);
  // assert(gc_ptr_is_in_to_space(vm, new_addr));
  auto new_ptr = to(Ptr, new_addr);
  *p = new_ptr;
}

// gc update is called while scanning the heap of objects that have
// already been copied over.
// StackFrameObject is handled specially.
void gc_update(VM *vm, ByteCodeObject* it) {
  if (it->code) {
    Ptr p = to(Ptr, it->code);
    gc_update_ptr(vm, &p);
    it->code = as(U16Array, p);
  }
  if (it->literals) {
    Ptr p = to(Ptr, it->literals);
    gc_update_ptr(vm, &p);
    it->literals = as(PtrArray, p);
  }
  gc_update_ptr(vm, &it->name);
}

void gc_update(VM *vm, PtrArrayObject* it) {
  for (u64 i = 0; i < it->length; i++) {
    gc_update_ptr(vm, it->data + i);
  }
}

void gc_update(VM *vm, StandardObject* it) {
  if (it->klass){
    Ptr p = to(Ptr, it->klass);
    gc_update_ptr(vm, &p);
    it->klass = as(Standard, p);
  }
  if (it->slots){
    Ptr p = to(Ptr, it->slots);
    gc_update_ptr(vm, &p);
    it->slots = as(PtrArray, p);
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
    Ptr p = to(Ptr, it->prev_frame);
    gc_update_ptr(vm, &p);
    it->prev_frame = as(StackFrame, p);
  }
  if (it->bc) {
    Ptr p = to(Ptr, it->bc);
    gc_update_ptr(vm, &p);
    it->bc = as(ByteCode, p);
  }
}

void gc_update_copied_object(VM *vm, Ptr it) {
  check(is(Object, it));
  if (is(ByteCode, it)) return gc_update(vm, as(ByteCode, it));
  if (is(PtrArray, it)) return gc_update(vm, as(PtrArray, it));
  if (is(Standard, it)) return gc_update(vm, as(Standard, it));
  if (is(StackFrame, it)) return gc_update(vm, as(StackFrame, it));
}

void gc_update_thread_ctx(VM *vm, thread_ctx* thd) {
  StackFrameObject *fr = thd->frame;
  Ptr *stack           = thd->stack;

  if (thd->bc) {
    ByteCodeObject **bytecode = &thd->bc;
    Ptr bc = to(Ptr, *bytecode);
    gc_update_ptr(vm, &bc);
    *bytecode = as(ByteCode, bc);
  }

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
      Ptr bc = to(Ptr, fr->bc);
      gc_update_ptr(vm, &bc);
      fr->bc = as(ByteCode, bc);
    }
    auto on_stack = (Ptr*)(void *)fr;
    while (on_stack > stack) {
      on_stack--;
      gc_update_ptr(vm, on_stack);
    }
    stack = &fr->argv[fr->argc + pad];
    fr = fr->prev_frame;
    count++;
  }
}

void gc_update_stack(VM *vm) {
  gc_update_thread_ctx(vm, vm->curr_thd);
  auto n = vm->threads->front;
  while(n) {
    gc_update_thread_ctx(vm, n->val);
    n = n->next;
  }
}

void gc_update_base_class(VM *vm, StandardObject **it) {
  Ptr p = to(Ptr, *it);
  gc_update_ptr(vm, &p);
  *it = as(Standard, p);
}


#define handle_class(name) gc_update_base_class(vm, &vm->globals->classes._##name);
#define update_sym(n) gc_update_ptr(vm, &vm->globals->known._##n); 
#define update_symbols(...) MAP(update_sym, __VA_ARGS__)

void gc_update_globals(VM *vm) {
  gc_update_ptr(vm, &vm->globals->call1);
  gc_update_ptr(vm, &vm->globals->lang_package);

#define X(...) MAP(handle_class, __VA_ARGS__)
#include "./primitive-classes.include"
#undef X

  for (auto i = 0; i < BuiltinClassIndexEnd; i++) {
    gc_update_base_class(vm, vm->globals->classes.builtins + i);
  }

  update_symbols(lambda, quote, let, if, fixnum, cons, string);
  update_symbols(array, character, boolean, quasiquote, unquote, unquote_splicing);
  update_symbols(compiler, set_bang, exception, run_string, with_special_binding);
  update_symbols(XpackageX, Xsource_locationX);

}

#undef update_sym
#undef update_symbols
#undef handle_class

void gc_copy_threads(VM *vm) {
  gc_update_ptr(vm, &vm->curr_thd->thread);
  auto n = vm->threads->front;
  while(n) {
    gc_update_ptr(vm, &n->val->thread);
    n = n->next;
  }
}

void gc_update_protected_references(VM *vm) {
  /*
  for (auto pair : *vm->gc_protected) {
    Object **ref = pair.first;
    auto obj = *ref;
    auto ptr = to(Ptr, obj);
    gc_update_ptr(vm, &ptr);
    auto new_obj = as(Object, ptr);
    *ref = new_obj;
    } */
  {
    auto prot = vm->gc_protected_vec;
    auto dat = prot->data();
    for (auto i = 0; i < prot->size(); i += 2) {
      Object **ref = (Object **)dat[i];
      if (!ref) continue;
      auto obj = *ref;
      auto ptr = to(Ptr, obj);
      gc_update_ptr(vm, &ptr);
      auto new_obj = as(Object, ptr);
      *ref = new_obj;
    }
  }
  {
    auto prot = vm->gc_protected_ptrs_vec;
    auto dat = prot->data();
    s64 cnt = prot->size();
    for (auto i = 0; i < cnt; i++) {
      if (dat[i]) { gc_update_ptr(vm, dat[i]); }
    }
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
  {
    auto prot = vm->gc_protected_vec;
    auto dat = prot->data();
    s64 cnt = prot->size();
    for (auto i = 0; i < cnt; i+=2) {
      Object **ref = (Object **)dat[i];
      auto obj = *ref;
      auto ptr = im_offset_ptr(to(Ptr, obj), delta);
      auto new_obj = as(Object, ptr);
      *ref = new_obj;
    }
  }

  {
    auto prot = vm->gc_protected_ptrs_vec;
    auto dat = prot->data();
    s64 cnt = prot->size();
    for (auto i = 0; i < cnt; i++) {
      if (dat[i]) {
        *dat[i] = im_offset_ptr(*dat[i], delta);
      }
    }
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
  auto prot = vm->gc_protected_vec;
  auto dat = prot->data();
  auto empty = -1;
  for (auto i = 0; i < prot->size(); i += 2) {
    Object **o = (Object **)dat[i];
    if (ref == o) {
      dat[i+1]++;
      return;
    }
    if (!o) empty = i;
  }
  if (empty > -1) {
    dat[empty] = (u64)ref;
    dat[empty+1] = 1;
    return;
  }
  prot->push_back((u64)ref);
  prot->push_back(1);
}

inline void gc_unprotect_reference(VM *vm, Object **ref){
  auto prot = vm->gc_protected_vec;
  auto dat = prot->data();
  for (auto i = 0; i < prot->size(); i += 2) {
    if (dat[i] == (u64)ref) {
      if (dat[i+1] == 1) dat[i] = 0;
      else dat[i+1]--;
      return;
    }
  }
  die("tried to unprotect a non-protected reference");
}

inline void gc_protect_ptr(VM *vm, Ptr *ref){
  auto prot = vm->gc_protected_ptrs_vec;
  auto dat = prot->data();
  s64 cnt = prot->size();
  for (s64 i = cnt - 1; i >= 0; i--) {
    if (!dat[i]) {
      dat[i] = ref;
      return;
    }
  }
  prot->push_back(ref);
}

inline void gc_unprotect_ptr(VM *vm, Ptr *ref){
  auto prot = vm->gc_protected_ptrs_vec;
  auto dat = prot->data();
  s64 cnt = prot->size();
  for (auto i = cnt - 1; i >= 0; i--) {
    if (dat[i] == ref) {
      dat[i] = (Ptr *)0;
      return;
    }
  }
  die("tried to unprotect a non-protected reference.");
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

void _vm_update_collection_limit(VM *vm);

void gc_static_mem(VM *vm) {

    #if BA_STATIC_MEM
    vm_map_reachable_refs(vm, [&](Ptr it){
         if (!is(ByteArray, it)) return;
         auto ba = as(ByteArray, it);
         ba->mem->flags = 1;
      });
    #endif

    auto count = 0;
    auto size = 0;
    static_memory **prev = &vm->static_mem;
    auto curr = vm->static_mem;
    while (curr) {
      if (curr->flags) {
        size += sizeof(static_memory) + curr->byte_count;
        count++;
        curr->flags = 0;
        *prev = curr;
        prev = &curr->next;
        curr = curr->next;
      } else {
        auto next = curr->next;
        free(curr);
        curr = next;
      }
    }
    *prev = 0;

    vm->static_mem_allocation_count = count;
    vm->static_mem_size = size;
}

void gc(VM *vm) {
  dbg("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");
  vm->in_gc = true;
  vm->gc_count++;

  auto byte_count = vm_heap_used(vm);
  if (byte_count > vm->allocation_high_watermark) {
    vm->allocation_high_watermark = byte_count;
  }


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
  _vm_update_collection_limit(vm);

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


  memset(vm->alt_heap_mem, 0, byte_count);
  // dbg(" protected Object count : ", vm->gc_protected->size());
  // dbg(" protected ptr    count : ", vm->gc_protected_ptrs->size());

  vm_refresh_frame_state(vm);

  gc_static_mem(vm);

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
  #if STATS
  vm->stats->total_cons_bytes_allocated += (sizeof(PtrArrayObject) + 3 * 8);
  #endif
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
  auto len = ba_length(s); 
  auto mem = ba_data(s);
  for (uint i = 0; i < len; i++) {
    os << mem[i];
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

Ptr make_vector_from_list(VM *vm, Ptr lst) { prot_ptr(lst);
  auto result = make_zf_array(vm, list_length(vm, lst));
  auto idx = 0;
  do_list(vm, lst, [&](Ptr it){ array_set(result, idx, it); idx++; });
  unprot_ptr(lst);
  return result;
}


/* ---------------------------------------- */

Ptr ht(VM *vm) {
  return make_ht(vm, make_xarray_with_capacity_and_used(vm, 16, 16), False, FIXNUM(0));
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
  check(idx < used); //:P
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

  check(idx < used); //:P
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
  auto pair = cons(vm, key, value);
  auto list = cons(vm, pair, xarray_memory(array)[idx]);
  xarray_memory(array)[idx] = list;
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
  auto use_list = cons(vm, vm->globals->lang_package, Nil); prot_ptr(use_list);
  auto subpackages = string_table(vm);                      prot_ptr(subpackages);
  auto res = make_package(vm, name, symtab, exports, use_list, subpackages, ht(vm));
  unprot_ptrs(name, symtab, exports, use_list, subpackages);
  return res;
}

Ptr make_basic_package(VM *vm, Ptr name) {                  prot_ptr(name);
  auto symtab = string_table(vm);                           prot_ptr(symtab);
  auto exports = ht(vm);                                    prot_ptr(exports);
  auto subpackages = string_table(vm);                      prot_ptr(subpackages);
  auto res = make_package(vm, name, symtab, exports, Nil, subpackages, ht(vm));
  unprot_ptrs(name, symtab, exports, subpackages);
  return res;
}

Ptr intern(VM *vm, const char* cstr, int len, Ptr pkg) {
  auto name = make_string_with_end(vm, cstr, cstr+len);
  if (pkg == Nil) return make_Symbol(vm, name, Nil, Nil, FIXNUM(0), Nil);
  auto tab = package_get_symtab(pkg); 
  auto exist = package_lookup_string(pkg, name);
  if (exist == Nil) {                                         prot_ptrs(tab, name);
    exist = make_Symbol(vm, name, pkg, Nil, FIXNUM(0), Nil);
    prot_ptr(exist);
    ht_at_put(vm, tab, name, exist); 
    unprot_ptrs(tab, name, exist);
  }
  return exist;
}

Ptr intern(VM *vm, ByteArrayObject *str, Ptr pkg) {
  return intern(vm, ba_data(str), ba_length(str), pkg);
}

Ptr intern(VM *vm, string name, Ptr pkg) {
  auto str = name.c_str();
  return intern(vm, str, strlen(str), pkg);
}

Ptr root_intern(VM *vm, string name) {
  return intern(vm, name, vm->globals->lang_package);
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
  _init_symbols(quote, if, fixnum, cons, string, array, character, boolean, quasiquote, unquote, compiler, exception);
  globals->known._let = root_intern(vm, "%let");
  globals->known._lambda = root_intern(vm, "%nlambda");
  globals->known._unquote_splicing = root_intern(vm, "unquote-splicing");
  globals->known._set_bang = root_intern(vm, "set!");
  globals->known._run_string = root_intern(vm, "run-string");
  globals->known._with_special_binding = root_intern(vm, "with-special-binding");
  globals->known._XpackageX = root_intern(vm, "*package*");
  globals->known._Xsource_locationX = root_intern(vm, "*source-location*");

}
#undef _init_sym
#undef _init_symbols

/* ---------------------------------------- */

bool is_object_class(StandardObject *obj) {
  return obj->header.flags & StandardObjectFlag_IsClass;
}

Ptr mark_object_as_class(StandardObject *obj) {
  obj->header.flags |= StandardObjectFlag_IsClass;
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
  standard_object_set_ivar(Base, BaseClassIvarNames, Nil); // TODO: initialize in boot
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
    auto it = to(Ptr, vm->globals->classes.builtins[i]); 
    xarray_push(vm, result, it);
  }
#define save_class(name) xarray_push(vm, result, to(Ptr, builtin(name)));
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


Ptr make_user_class(VM *vm, Ptr name, Ptr ivar_names) { prot_ptrs(name, ivar_names);
  auto ivar_names_vector = make_vector_from_list(vm, ivar_names); prot_ptr(ivar_names_vector);
  auto ivar_ct = to(Fixnum, list_length(vm, ivar_names));
  auto method_dict = ht(vm);                      prot_ptr(method_dict);
  auto metadata = ht(vm);
  auto superclass = vm->globals->classes.builtins[BuiltinClassIndex_Base];
  Ptr slots[] = {name, ivar_ct, method_dict, metadata, Nil, ivar_names_vector};
  auto result = make_standard_object(vm, superclass, slots);
  unprot_ptrs(name, method_dict, ivar_names, ivar_names_vector);
  mark_object_as_class(result);
  return to(Ptr, result);
}

Ptr instantiate_user_class(VM *vm, StandardObject *klass) {
  return to(Ptr, make_standard_object(vm, klass, 0));
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
  check(is(Symbol, sym));
  Symbol_set_value(sym, value);
  auto flags = from(Fixnum, Symbol_get_flags(sym)) | SYMBOL_FLAG_BOUNDP;
  Symbol_set_flags(sym, to(Fixnum, flags));
  return sym;
}

inline Ptr set_global(VM *vm, const char* name, Ptr value) { prot_ptr(value);
  auto result = set_global(vm, root_intern(vm, name), value);
  unprot_ptr(value);
  return result;
}

inline bool boundp(VM *vm, Ptr sym) {
  maybe_unused(vm);
  return Symbol_get_flags(sym).value & (SYMBOL_FLAG_BOUNDP << 4);
}

inline Ptr get_global(VM *vm,  Ptr sym) {
  if (likely(boundp(vm, sym))) return Symbol_get_value(sym);
  dbg("unbound global: ", sym);
  vm->error = "symbol is unbound";
  return Nil;
}

inline Ptr get_global(VM *vm,  const char*name) {
  return get_global(vm, root_intern(vm, name));
}

inline Ptr get_special_binding(VM *vm, Ptr sym) {
  // when we compile initial bytecode the frame may not be ready
  // may make sense to move this check there rather than take the hit on every lookup
  if (!vm->curr_frame) return Nil;
  return assoc(sym, vm->curr_frame->special_variables);
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
  check(is(Symbol, sym));
  if (is_special_symbol(vm, sym)) return set_special(vm, sym, value); 
  else return set_global(vm, sym, value);
}

Ptr get_symbol_value(VM *vm, Ptr sym) {
  check(is(Symbol, sym));
  if (is_special_symbol(vm, sym)) return get_special(vm, sym); 
  else return get_global(vm, sym);
}

inline void _thread_local_binding_push(VM *vm, Ptr sym, Ptr val) {
  auto fr = vm->curr_frame;
  auto assoc = cons(vm, sym, val);
  auto exist = fr->special_variables;
  auto update = cons(vm, assoc, exist);
  fr->special_variables = update;
  fr->special_count++;
}

inline void _thread_local_binding_pop(VM *vm) {
  auto fr = vm->curr_frame;
  auto exist = fr->special_variables;
  auto update = cdr(exist);
  fr->special_variables = update;
  fr->special_count--;
}

/* -------------------------------------------------- */

void initialize_global_variables(VM *vm) {
  set_symbol_value(vm, KNOWN(XpackageX), vm->globals->lang_package);
  mark_symbol_as_special(vm, KNOWN(XpackageX));

  auto _stdout = root_intern(vm, "*standard-output*");
  set_symbol_value(vm, _stdout, make_file_output_stream(vm, FIXNUM(1)));
  mark_symbol_as_special(vm, _stdout);

  auto _stderr = root_intern(vm, "*standard-error*");
  set_symbol_value(vm, _stderr, make_file_output_stream(vm, FIXNUM(2)));
  mark_symbol_as_special(vm, _stderr);

  set_symbol_value(vm, KNOWN(Xsource_locationX), Nil);
  mark_symbol_as_special(vm, KNOWN(Xsource_locationX));
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
    check(input <= end);
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

inline void vm_adv_pc(VM *vm) {
  vm->pc++;
}

void vm_pop_stack_frame(VM* vm) {
  auto thd = vm->curr_thd;
  auto fr = thd->frame;
  if (unlikely(!fr->prev_frame)) {
    vm->error = "nowhere to return to";
    return;
  }
  // assert(vm->curr_thread->stack <= vm->curr_thread->stack_start &&
  //        vm->curr_thread->stack >= vm->curr_thread->stack_end);
  thd->stack = fr->prev_stack + fr->argc;
  thd->frame = fr->prev_frame;
  // assert(vm->curr_thread->stack <= vm->curr_thread->stack_start &&
  //        vm->curr_thread->stack >= vm->curr_thread->stack_end);
  thd->bc = thd->frame->bc;
  thd->stack_depth--;
  // dbg("--- popped stack frame.");
  // vm_dump_args(vm);
  vm_refresh_frame_state(vm);
}

void _vm_reset_stack_from_root_frame(VM *vm) {
  auto thd = vm->curr_thd;
  auto fr = thd->frame;
  thd->bc = 0;
  thd->stack = fr->prev_stack + fr->argc;
  thd->frame = 0;
  thd->stack_depth--;
  vm_refresh_frame_state(vm);
}

void vm_prepare_for_tail_call(VM *vm, s64 argc) {
  // argc + 1 to account for the function, which is also on the stack.
  auto count = argc + 1;
  Ptr *args = vm->curr_thd->stack;
  vm_pop_stack_frame(vm);
  memmove(vm->curr_thd->stack - count, args, count * 8);
  vm->curr_thd->stack -= count;
}

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn, Ptr closed_over);

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn) {
  vm_push_stack_frame(vm, argc, fn, Nil);
};

#define STACK_PADDING 0ULL

void vm_push_stack_frame(VM* vm, u64 argc, ByteCodeObject*fn, Ptr closed_over) {

  auto thd = vm->curr_thd;

  if (thd->frame) { thd->frame->prev_pc = vm->pc; }

  uint offset = (sizeof(StackFrameObject) / sizeof(u64));

  u64 *top = &((thd->stack - offset)->value);
  // ensure new stack frame is properly aligned
  u64 padding = ((u64)top & TAG_MASK) ? 1 + STACK_PADDING : STACK_PADDING;
  top -= padding;
  check(((u64)top & TAG_MASK) == 0);

  if (unlikely((Ptr *)top < thd->stack_end)) {
    #ifndef NDEBUG
    auto ct = thd->curr_size;
    maybe_unused(ct);
    grow_thread_ctx(vm, thd);
    check(ct * 2 == vm->curr_thd->curr_size);
    check(thd->stack_start - thd->stack_end > (ct / sizeof(Ptr)));
    vm_push_stack_frame(vm, argc, fn, closed_over);
    #else
    grow_thread_ctx(vm, thd);
    vm_push_stack_frame(vm, argc, fn, closed_over);
    #endif
    return;
  }

  StackFrameObject *new_frame = (StackFrameObject *)top;
  new_frame->header.object_type = StackFrame_ObjectType;
  set_obj_tag(new_frame, StackFrame);
  new_frame->pad_count = padding;

  new_frame->closed_over = closed_over;
  new_frame->mark = Nil;
  new_frame->bc = fn;
  new_frame->prev_stack = thd->stack;
  new_frame->prev_frame = thd->frame;
  new_frame->prev_pc = 0;
  new_frame->argc = argc;
  if (thd->frame) {
    new_frame->special_variables = thd->frame->special_variables;
  } else {
    new_frame->special_variables = thread_get_local_bindings(thd->thread);
  }
  new_frame->special_count = 0;
  thd->stack = (Ptr*)(void *)new_frame;
  thd->frame = new_frame;
  thd->bc = fn;
  thd->stack_depth++;

  unsafe_vm_refresh_frame_state(vm);
}


Ptr vm_set_stack_mark(VM *vm, Ptr mark) {
  vm->curr_frame->mark = mark;
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
  auto count = 0;
  while (fr) {
    auto f = to(Ptr, fr);
    if (!is(StackFrame, f)) die("expecting stack frame got: ", f);
    fr = fr->prev_frame;
    count++;
  }
}

void _debug_validate_stack_frame(StackFrameObject *fr, thread_ctx *ctx){
  auto count = 0;
  while (fr) {
    auto f = to(Ptr, fr);
    if (!is(StackFrame, f)) die("expecting stack frame got: ", f);
    assert((void*)fr >= (void*)ctx->stack_start && (void*)fr <= (void*)ctx->stack_end);
    assert(fr->argc < 100 && fr->argc >= 0);
    fr = fr->prev_frame;
    count++;
  }
}

void _debug_validate_stack(thread_ctx *thd) {
  if (!thd->frame) return; // when thread has not yet started
  assert((void *)thd->stack <= (void*)thd->frame);
  assert((void *)thd->frame <= (void *)thd->stack_start);
  assert((void*)thd->frame >= (void *)thd->stack_end);
  assert(thd->stack <= thd->stack_start);
  assert(thd->stack >= thd->stack_end);
  StackFrameObject *fr = thd->frame;
  _debug_validate_stack_frame(fr);
}

void _debug_validate_stack(VM *vm) {
  _debug_validate_stack(vm->curr_thd);
}

void _debug_validate_background_threads(VM *vm) {
  auto count = 0;
  auto curr = vm->threads->front;
  while(curr) {
    if (curr->val->frame) _debug_validate_stack(curr->val);
    curr = curr->next;
    count++;
  }
}

void _assert_ptr_in_heap(void *start, void *end, Ptr it) {
  #ifndef NDEBUG
  if (it == Nil || ! is(Object, it)) return;
  auto addr = as(Object, it);
  assert(addr >= start && addr < end);
  #endif
}

Ptr snapshot_thread_ctx_with_predicate(VM *vm, thread_ctx *thd, StackPred fn) {
  StackFrameObject *fr = thd->frame;
  Ptr result  = Nil;                               prot_ptr(result);
  result = alloc_cont(vm);
  {
    Ptr *stack = thd->stack;
    auto on_stack = (Ptr*)(void *)thd->frame; // go back 'up' the stack to get current args
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
     auto f = to(Ptr, fr);
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

    // if there was a frame above us, point it at us
    if (prev_fr != Nil) {
      auto pf = as(StackFrame, prev_fr);
      pf->prev_frame = nf; 
    }

    prev_fr = to(Ptr, nf);
    check(is(StackFrame, prev_fr));
    if (top_fr == Nil) top_fr = prev_fr;
    fr = fr->prev_frame;
  }

  cont_set_stack(result, top_fr);
  unprot_ptrs(result, top_fr, prev_fr);
  return result;
}

Ptr vm_snapshot_stack_with_predicate(VM *vm, StackPred fn) {
  auto thd = vm->curr_thd;
  thd->frame->prev_pc = vm->pc;
  return snapshot_thread_ctx_with_predicate(vm, thd, fn);
}

Ptr vm_snapshot_stack_to_mark(VM *vm, Ptr mark) { prot_ptr(mark);
  auto result = vm_snapshot_stack_with_predicate(vm,[&](StackFrameObject *fr){
      return ptr_eq(fr->mark, mark); });
  unprot_ptr(mark);
  return result;
}

void vm_unwind_to_mark(VM *vm, Ptr mark) {
  while (vm->curr_frame && !ptr_eq(vm->curr_frame->mark, mark)) {
    vm_pop_stack_frame(vm); 
    if (vm->error) return;
  }
}

void vm_unwind_to_predicate(VM *vm, StackPred fn) {
  while (vm->curr_frame && !fn(vm->curr_frame)) {
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

Ptr vm_return_from_mark(VM *vm, Ptr mark, Ptr value) {
  vm_unwind_to_mark(vm, mark);
  vm_pop_stack_frame(vm);
  return value;
}

bool vm_handle_error(VM *vm) {
  _print_debug_stacktrace(vm->curr_thd);
  Ptr ex = make_string(vm, vm->error); // TODO: signal better errors than just strings
  vm->error = 0;
  vm_unwind_to_mark(vm, KNOWN(exception));
  vm_push(vm, ex);
  if (!vm->curr_frame->prev_frame) {
    vm_pop(vm);
    dbg("killing thread: ", vm->curr_thd->thread, " due to uncaught exception ", ex);
    thread_set_status(vm->curr_thd->thread, THREAD_STATUS_DEAD);
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
  auto fr = vm->curr_thd->frame;
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

void __vm_restore_stack_snapshot(VM *vm, StackFrameObject *fr) {
  if (!fr) return;
  #if DEBUG_IMAGE_SNAPSHOTS
  {
    Ptr it = to(Ptr, fr);
    assert(is(StackFrame, it));
  }
  #endif

  auto thd = vm->curr_thd;

  // first restore previous frame
  __vm_restore_stack_snapshot(vm, fr->prev_frame);

  //restore previous frame stack top
  _vm_copy_stack_snapshot_args_to_stack(vm, fr);

  thread_ctx_ensure_bytes(vm, thd, sizeof(StackFrameObject) + sizeof(Ptr));
  // alloc and align new frame;
  auto top = thd->stack - (sizeof(StackFrameObject) / sizeof(u64));
  auto was_aligned = true;
  if (!pointer_is_aligned(top)) {
    top -= 1;
    was_aligned = false;
  }
  check(pointer_is_aligned(top));

  auto new_frame = (StackFrameObject *)(void *)top;
  memcpy(new_frame, fr, sizeof(StackFrameObject));

  new_frame->prev_stack = thd->stack;
  new_frame->pad_count = was_aligned ? 0 : 1;
  new_frame->argc = new_frame->preserved_argc;

  #if DEBUG_IMAGE_SNAPSHOTS
  {
    Ptr it = to(Ptr, thd->frame);
    assert(is(StackFrame, it));
  }
  #endif

  new_frame->prev_frame = thd->frame;

  // update the stack etc
  thd->stack = (Ptr *)(void *)new_frame;
  thd->frame = new_frame;
  thd->stack_depth++;

}

void _vm_restore_stack_snapshot(VM *vm, StackFrameObject *fr) {
  __vm_restore_stack_snapshot(vm, fr);
  thread_ctx_ensure_bytes(vm, vm->curr_thd, sizeof(Ptr));
  vm->curr_thd->stack--;
}

StackFrameObject *_vm_get_nth_frame(VM *vm, s64 n) {
  auto curr = vm->curr_frame;
  while (curr && n > 0) {
    curr = curr->prev_frame;
    n--;
  }
  return curr;
}


void vm_restore_stack_snapshot(VM *vm, Ptr cont) {
  Ptr extra_args = cont_get_stack_top(cont);
  Ptr frame      = cont_get_stack(cont);
  auto bc        = as(StackFrame, frame)->bc;

  auto thd = vm->curr_thd;
  auto stack_depth = thd->stack_depth;

  // restore frames
  _vm_restore_stack_snapshot(vm, as(StackFrame, frame));

  // retore stack top
  auto args = as(PtrArray, extra_args);

  if (args->length > 0) {
    for (s64 i = args->length - 1; i >= 0; i--) {
      vm_push(vm, args->data[i]);
    }
  }

  thd->bc = bc;

  // restore special variables (must go last as it may cons)
  auto base_frame = _vm_get_nth_frame(vm, thd->stack_depth - stack_depth);
  _vm_restore_special_variables_snapshot(vm, base_frame);

  vm_refresh_frame_state(vm);
}

Ptr vm_resume_stack_snapshot(VM *vm, Ptr cont, Ptr arg) { prot_ptr(arg);
  vm_restore_stack_snapshot(vm, cont);
  unprot_ptr(arg);
  return arg;
}

Ptr signal_semaphore(Ptr a) {
  check(is(semaphore, a));
  auto it = semaphore_get_count(a);
  if (is(Fixnum, it)) {
    auto ct = as(Fixnum, it);
    semaphore_set_count(a, to(Fixnum, ct + 1));
  } else {
    semaphore_set_count(a, True);
  }
  return Nil;
}

bool acquire_semaphore(Ptr a) {
  check(is(semaphore, a));
  auto it = semaphore_get_count(a);
  if (is(Fixnum, it)) {
    auto ct = as(Fixnum, it);
    if (ct > 0) {
      semaphore_set_count(a, to(Fixnum, ct - 1));
      return true;
    }
  } else if (it == True) {
    semaphore_set_count(a, False);
    return true;
  }
  return false;
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
    auto thread = curr->val->thread;
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
thread_ctx *_vm_maybe_get_next_available_thread(VM *vm) {
  auto threads = vm->threads;
  if (threads->front) {
    s64 now = 0; //current_time_ms();
    thdq_node *p = 0;
    auto n = threads->front;
    while (n) {
      auto thd = n->val;
      auto thread = thd->thread;
      auto status = thread_get_status(thread);
      if (status == THREAD_STATUS_WAITING) {
        // dbg("found waiting thread");
        thdq_remove_next(threads, p);
        return thd;
      } else if (status == THREAD_STATUS_SLEEPING) {
        if (!now) now = current_time_ms();
        auto wake_after = thread_get_wake_after(thread);
        s64 delta = as(Fixnum, wake_after) - now;
        // if (delta > 0) dbg("thread still sleeping for ", delta, " ms", thread);
        if (delta <= 0) {
          // dbg("waking sleeping thread");
          thdq_remove_next(threads, p);
          return thd;
        }
      } else if (status == THREAD_STATUS_SEM_WAIT) {
        auto sem = thread_get_semaphore(thread);
        if (acquire_semaphore(sem)) {
          thdq_remove_next(threads, p);
          return thd;
        }
      } else if (status == THREAD_STATUS_RUNNING) {
        // should not get here
        dbg("somehow wound up with running thread in scheduled?");
      }
      p = n;
      n = n->next;
    }
  }
  return 0;
}


Ptr list_all_threads(VM *vm) {
  Ptr result = Nil; prot_ptr(result);
  auto n = vm->threads->front;
  while (n) {
    result = cons(vm, n->val->thread, result);
    n = n->next;
  }
  result = cons(vm, vm->curr_thd->thread, result);
  unprot_ptr(result);
  return result;
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

void thread_ctx_suspend_in_continuation(VM *vm, thread_ctx *ctx);

Ptr _suspended_background_threads_as_list(VM *vm) {
  auto result = Nil; prot_ptr(result);
  auto n = vm->threads->front;
  while (n) {
    thread_ctx_suspend_in_continuation(vm, n->val);
    result = cons(vm, n->val->thread, result);
    n = n->next;
  }
  unprot_ptr(result);
  return _list_rev(result);
}

Ptr _background_threads_as_list(VM *vm) {
  auto result = Nil; prot_ptr(result);
  auto n = vm->threads->front;
  while (n) {
    result = cons(vm, n->val->thread, result);
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

thread_ctx *_vm_thread_suspend(VM *vm) {
  auto frame = vm->curr_thd->frame;
  if (frame) { frame->prev_pc = vm->pc; }
  thread_set_status(vm->curr_thd->thread, THREAD_STATUS_WAITING);
  return vm->curr_thd;
}

void thread_ctx_suspend_in_continuation(VM *vm, thread_ctx *ctx) {
  if (ctx->frame->prev_frame == 0) return;
  auto predicate = [&](StackFrameObject *fr){
    return fr->prev_frame == 0;
  };
  auto cont = snapshot_thread_ctx_with_predicate(vm, ctx, predicate);
  thread_set_suspension(ctx->thread, cont);
  thread_set_status(ctx->thread, THREAD_STATUS_WAITING);
  ctx->has_run = false;
}

ByteCodeObject *make_empty_bytecode(VM *vm);

void _vm_thread_resume(VM *vm, thread_ctx* thd) {
  vm_set_curr_thread(vm, thd);
  auto thread = thd->thread;
  thread_set_status(thread, THREAD_STATUS_RUNNING);
  if (thd->has_run) return;
  thd->has_run = true;
  auto thunk = thread_get_thunk(thread);
  auto cont  = thread_get_suspension(thread);
  if (thunk != Nil || cont != Nil) {
    if (is(Closure, thunk)) {
      thread_set_thunk(thread, Nil);
      prot_ptrs(thread, thunk);
      _vm_unwind_to_root_frame(vm);
      if (!vm->curr_frame) {
        auto bc = make_empty_bytecode(vm);
        vm_push_stack_frame(vm, 0, bc, Nil);
        vm->curr_frame->mark = KNOWN(exception);
      }
      auto bc = closure_code(thunk);
      auto env = closure_env(thunk);
      vm_push_stack_frame(vm, 0, bc, env);
      vm->pc--;
      unprot_ptrs(thread, thunk);
    } else if (is(cont, cont)) {
      thread_set_suspension(thread, Nil);
      prot_ptrs(thread, cont);
      _vm_unwind_to_root_frame(vm);
      if (!vm->curr_frame) {
        auto bc = make_empty_bytecode(vm);
        vm_push_stack_frame(vm, 0, bc, Nil);
        vm->curr_frame->mark = KNOWN(exception);
      }
      vm_restore_stack_snapshot(vm, cont);
      unprot_ptrs(thread, cont);
    } else {
      assert(false);
    }
  } else {
      assert(false);
  }
}

// N.B. when used outside the interpreter loop, you must
//      manually advance the PC before entering vm_interp
bool vm_maybe_start_next_thread(VM *vm) {
  auto next = _vm_maybe_get_next_available_thread(vm);
  if (next) {
    _vm_thread_resume(vm, next);
    // dbg("did resume next thread...", next);
    return true;
  }
  return false;
}

void vm_add_thread_to_background_set(VM *vm, thread_ctx *thd){
  auto thread = thd->thread;
  check(is(thread, thread));
  if (vm->curr_thd) assert(!(thread == vm->curr_thd->thread));
  if (!ptr_eq(thread_get_status(thread), THREAD_STATUS_SLEEPING) &&
      !ptr_eq(thread_get_status(thread), THREAD_STATUS_WAITING) &&
      !ptr_eq(thread_get_status(thread), THREAD_STATUS_SEM_WAIT)
      ) {
    dbg(" !!!! ensuring thread status !!!");
    thread_set_status(thread, THREAD_STATUS_WAITING);
  }
  // dbg("adding background thread");
  auto q = vm->threads;
  if (q->count < 50) return thdq_push(q, thd);
  auto pri = from(Fixnum, thread_get_priority(thread));
  if (pri <= 0) return thdq_push(q, thd);
  s64 idx = q->count / pri;
  if (idx < 1) idx = 1;
  // dbg("scheduling at ", idx, " of ", q->count);
  thdq_insert_at_index(q, thd, idx);
}

void vm_suspend_current_thread(VM *vm) {
  auto curr = _vm_thread_suspend(vm);
  vm_set_curr_thread(vm, 0); // XXX careful!
  if (curr) vm_add_thread_to_background_set(vm, curr);
  vm->suspended = true;
}

bool vm_sleep_current_thread(VM *vm, s64 ms) {
  auto curr = _vm_thread_suspend(vm);
  vm_set_curr_thread(vm, 0); // XXX careful!
  if (curr) {
    auto thread = curr->thread;
    // dbg("putting thread to sleep, count is now: ", vm->threads->size());
    thread_set_status(thread, THREAD_STATUS_SLEEPING);
    s64 wake_after = current_time_ms() + ms;
    thread_set_wake_after(thread, to(Fixnum, wake_after));
    vm_add_thread_to_background_set(vm, curr);
    // dbg("put thread to sleep, count is now: ", vm->threads->size());
    // dbg("slept to thread: ", curr);
  }
  auto result = vm_maybe_start_next_thread(vm);
  return result;
}

bool vm_sem_wait_current_thread(VM *vm, Ptr semaphore) { prot_ptr(semaphore);
  auto curr = _vm_thread_suspend(vm);
  vm_set_curr_thread(vm, 0); // XXX careful!
  if (curr) {
    auto thread = curr->thread;
    thread_set_status(thread, THREAD_STATUS_SEM_WAIT);
    thread_set_semaphore(thread, semaphore);
    vm_add_thread_to_background_set(vm, curr);
  }
  auto result = vm_maybe_start_next_thread(vm);
  unprot_ptr(semaphore);
  return result;
}

bool vm_swap_threads(VM *vm) {
  auto next = _vm_maybe_get_next_available_thread(vm);
  if (next) {
    // assert(vm->curr_thd != next);
    auto curr = _vm_thread_suspend(vm);
    // vm_set_curr_thread(vm, 0); // XXX careful!
    vm->curr_thd = 0;
    if (curr) vm_add_thread_to_background_set(vm, curr);
    _vm_thread_resume(vm, next);
    return true;
  }
  return false;
}

Ptr vm_schedule_closure(VM *vm, Ptr closure, Ptr priority, Ptr bindings) {
  assert(is(Closure, closure));

  auto thread = make_thread(vm,
                            closure,
                            THREAD_STATUS_WAITING,
                            Nil,
                            FIXNUM(0),
                            priority,
                            bindings,
                            Nil);

  auto ctx = make_thread_ctx();
  ctx->thread = thread;
  vm_add_thread_to_background_set(vm, ctx);
  return Nil;
}

/* ---------------------------------------- */
/*        image save / restore support      */

void vm_init_from_heap_snapshot(VM *vm);
ByteCodeObject *make_empty_bytecode(VM *vm);

void _im_prepare_vm_for_snapshot(VM *vm) {

  // write the root package
  ht_at_put(vm, vm->system_dictionary,
            SYSTEM_LANG_PACKAGE_KEY, vm->globals->lang_package);

  // copy the waiting threads into the system dictionary 
  auto threads = _suspended_background_threads_as_list(vm);
  ht_at_put(vm, vm->system_dictionary, SYSTEM_OTHER_THREADS_KEY, threads);

  // suspend the current thread and store it in the system dictionary
  vm->curr_thd->frame->prev_pc = vm->pc;
  thread_ctx_suspend_in_continuation(vm, vm->curr_thd);
  auto main = vm->curr_thd->thread;
  ht_at_put(vm, vm->system_dictionary, SYSTEM_CURRENT_THREAD_KEY, main);

  auto classes = _built_in_classes_as_array(vm);
  ht_at_put(vm, vm->system_dictionary, SYSTEM_BUILTIN_CLASSES_KEY, classes);

  // allocate space for the current object color (before gc)
  ht_at_put(vm, vm->system_dictionary, SYSTEM_CURRENT_COLOR_KEY, FIXNUM(0));

  gc(vm);

  // save the current object color (after gc)
  ht_at_put(vm, vm->system_dictionary, SYSTEM_CURRENT_COLOR_KEY, to(Fixnum, vm->current_color));


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
  _debug_validate_thread(vm->curr_thd->thread);
  #endif

  // resume main thread
  _vm_unwind_to_root_frame(vm);
  _vm_reset_stack_from_root_frame(vm);
  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->curr_thd->frame->mark = KNOWN(exception);
  _debug_validate_stack(vm);

  vm->error = 0;
  vm->suspended = false;
  _vm_thread_resume(vm, vm->curr_thd);
}

Ptr im_snapshot_to_path(VM *vm, const char *path){
  _im_prepare_vm_for_snapshot(vm);

  // allocate new heap
  auto used_bytes = vm_heap_used(vm);
  auto new_heap = calloc(used_bytes, 1);

  // copy in the old heap
  auto new_heap_end = new_heap;
  {
    std::memmove(new_heap, vm->heap_mem, used_bytes);
    new_heap_end = (u8*)new_heap + used_bytes;
    // assert(((u64)new_heap_end - (u64)new_heap) ==
    //        ((u64)vm->heap_end - (u64)vm->heap_mem));
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
    image_header header;
    auto heap_size = (u64)new_heap_end - (u64)new_heap;

    header.heap_size = heap_size;
    header.static_size = vm->static_mem_size;
    header.static_memory_allocation_count = vm->static_mem_allocation_count;

    FILE *out = fopen(path, "wb");
    if(out != NULL) {
      //write the header
      {
        auto data = (char*)&header;
        fwrite(data, 1, sizeof(image_header), out);
        dbg("wrote header");
      }
      //write the heap
      {
        auto data = (char*)new_heap;
        s64 to_go = header.heap_size;
        while(to_go > 0) {
          const size_t wrote = fwrite(data, 1, to_go, out);
          if(wrote == 0) break;
          to_go -= wrote;
          data += wrote;
          dbg("to go bytes: ", to_go, " wrote: ", wrote);
        }
      }
      //write the static memory
      {
        auto curr = vm->static_mem;
        while (curr) {
          auto next = curr->next;
          curr->next = curr; // store this block's pointer in itself to reference on rehydrate
          auto size = sizeof(static_memory) + curr->byte_count;
          #ifdef NDEBUG
          fwrite(curr, 1, size, out);
          #else
          auto wrote = fwrite(curr, 1, size, out);
          assert(wrote == size);
          #endif
          curr->next = next;
          curr = next;
        }
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
  vm->curr_thd->frame->mark = KNOWN(exception);
  _debug_validate_stack(vm);

  vm->error = 0;
  vm->suspended = false;
  _vm_thread_resume(vm, vm->curr_thd);

  return True;
}


const char *bao_to_c_string(ByteArrayObject *bao);

Ptr im_snapshot_to_path(VM *vm, ByteArrayObject* path) {
  auto c_path = bao_to_c_string(path);
  auto result = im_snapshot_to_path(vm, c_path);
  free((char *)c_path);
  return result;
}

Ptr im_snapshot_to_path_and_exit(VM *vm, ByteArrayObject *path) {
  im_snapshot_to_path(vm, path);

  auto run_time = (current_time_ms() - vm->start_time_ms) / 1000.0;
  s64 ops_per_second = vm->instruction_count / run_time;
  dbg(" executed ", vm->instruction_count, " instructions over ", run_time, " seconds.");
  dbg(" average of ", ops_per_second, " ops per second, including sleep. ");
  dbg(" gc count: ", vm->gc_count);

  #if STATS
  report_stats(vm->stats);
  #endif
  exit(0);
  return Nil;
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
  SWAP                 ,
  CALL                 ,
  TAIL_CALL            ,
  LOAD_ARG             ,
  STORE_ARG            ,
  LOAD_GLOBAL          ,
  LOAD_SPECIAL         ,
  LOAD_CLOSURE         ,
  STORE_CLOSURE        ,
  BUILD_CLOSURE        ,
  SAVE_CLOSURE_ENV     ,
  RESTORE_CLOSURE_ENV  ,
  PUSH_CLOSURE_ENV     ,
  BR_IF_False          ,
  JUMP                 ,
  PUSH_JUMP            ,
  POP_JUMP             ,
  STACK_RESERVE        ,
  LOAD_FRAME_RELATIVE  ,
  STORE_FRAME_RELATIVE ,
  POP_CLOSURE_ENV      ,
  PUSH_SPECIAL_BINDING ,
  POP_SPECIAL_BINDING  ,
  LOAD_GLOBAL_AT_IDX   ,
};

inline void vm_push(VM* vm, Ptr value) {
  auto thd = vm->curr_thd;
  if (unlikely(thd->stack - 1 <= thd->stack_end)) {
    grow_thread_ctx(vm,  thd);
  }
  *(--thd->stack) = value;
}

inline void vm_stack_reserve_n(VM* vm, u64 n){
  auto thd = vm->curr_thd;
  thread_ctx_ensure_bytes(vm, thd, sizeof(Ptr) * n);
  thd->stack -= n;
  memset(thd->stack, 0, n * 8);
}

inline Ptr vm_pop(VM* vm) {
  return *(vm->curr_thd->stack++);
}

inline void vm_stack_pop_n(VM *vm, u64 n) {
  vm->curr_thd->stack += n;
}

inline Ptr vm_stack_ref(VM *vm, u32 distance) {
  return vm->curr_thd->stack[distance];
}

inline Ptr vm_load_arg(VM *vm, u8 idx) {
  auto fr = vm->curr_frame;
  u64 argc = fr->argc;
  u64 ofs  = fr->pad_count;
  return fr->argv[ofs + (argc - (idx + 1))];
}

inline void vm_store_arg(VM *vm, u32 idx, Ptr it) {
  auto fr = vm->curr_frame;
  u64 argc = fr->argc;
  u64 ofs  = fr->pad_count;
  fr->argv[ofs + (argc - (idx + 1))] = it;
}

// N.B. must not double-prot the ptrs on the stack to avoid double-copy
Ptr vm_get_stack_values_as_list(VM *vm, u32 count) { //@varargs
  Ptr result = Nil; prot_ptr(result);
  Ptr *ptrs = vm->curr_thd->stack; // unprotected to avoid double-copy
  for (u64 i = 0; i < count; i++) {
    // faster than calling `cons` as we incur less gc_protect overhead
    auto c = alloc_cons(vm);
    cons_set_car(c, ptrs[i]);
    cons_set_cdr(c, result);
    result = c;
  }
  vm_stack_pop_n(vm, count);
  unprot_ptr(result);
  return result;
}

inline auto vm_load_closure_value(VM *vm, u64 slot, u64 depth) {
  auto curr = vm->curr_frame->closed_over;
  while (depth) {
    // if (isNil(curr)) _print_debug_stacktrace(vm->curr_thd);
    check(!isNil(curr));
    curr = array_get(curr, 0);
    depth--;
  }
  // if (isNil(curr)) _print_debug_stacktrace(vm->curr_thd);
  check(!isNil(curr));
  return array_get(curr, slot+1);
}

inline void vm_store_closure_value(VM *vm, u64 slot, u64 depth, Ptr value) {
  auto curr = vm->curr_frame->closed_over;
  while (depth) {
    check(!isNil(curr));
    curr = array_get(curr, 0);
    depth--;
  }
  check(!isNil(curr));
  array_set(curr, slot+1, value);
}

inline u8 instr_code(u16 bc) {
  return ((u8*)&bc)[0];
}
inline u8 instr_data(u16 bc) {
  return ((u8*)&bc)[1];
}

inline u16 build_instr(u8 op, u16 data) {
  u16 res = 0;
  ((u8*)&res)[0] = op;
  ((u8*)&res)[1] = data;
  return res;
}

Ptr applicator_for_object(VM *vm, Ptr it) {
  auto klass = class_of(vm, it);
  return standard_object_get_ivar(as(Standard, klass), BaseClassApplicator);
}

// @speed this will be hideously slow. need bytecode level support
inline void vm_interp_prepare_for_send(VM *vm, s32 argc) {
  // TODO: arity check and errors
  auto message = vm_stack_ref(vm, argc - 1);
  auto self    = vm_stack_ref(vm, argc - 2);
  auto klass   = class_of(vm, self);
  auto dict    = standard_object_get_ivar(as(Standard, klass), BaseClassMethodDict);
  auto fn      = ht_at(dict, message);
  if (isNil(fn)) die("could not send message: ", message);
  // shift args back @speed could just memmove these
  if (argc > 0) {
    Ptr args[argc];
    for (auto i = argc - 1; i >= 0; i--) {
      args[i] = vm_pop(vm);
    }
    for (auto i = 1; i < argc; i++) {
      vm_push(vm, args[i]);
    }
  } else {
    vm_pop(vm);
  }
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

// @speed need to have less indirection here,
//        but also need to integrate with stack push/pop and gc both
#define vm_curr_instr(vm) vm->curr_code[vm->pc]
#define vm_adv_instr(vm) vm->curr_code[++(vm->pc)]

typedef struct {
  s64 thread_switch_instr_budget, total_execution_instr_budget;
  bool block_for_initial_thread;
} interp_params;

//absurdly low for testing
#define CTX_SWITCH 10000
#define RUN_QUICK 10000
#define RUN_AWHILE 1000000
#define RUN_INDEFINITELY 0
auto INTERP_PARAMS_MAIN_EXECUTION = (interp_params){CTX_SWITCH,RUN_INDEFINITELY, false};
auto INTERP_PARAMS_MAIN_EVENT_HANDLER = (interp_params){CTX_SWITCH,RUN_AWHILE, false};
auto INTERP_PARAMS_EVAL = (interp_params){CTX_SWITCH,RUN_INDEFINITELY,true};

void vm_interp(VM* vm, interp_params params) {
  auto counter = 0;
  auto init_thread = vm->curr_thd->thread; prot_ptr(init_thread);
  u16 instr; u8 code; u8 data;
  s64 ctx_switch_budget, spent_instructions;
  ctx_switch_budget = params.thread_switch_instr_budget;
  spent_instructions = 0;

  if (!vm->start_time_ms) vm->start_time_ms = current_time_ms();

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
      u32 idx = data;
      if (idx == 255) idx = vm_adv_instr(vm);
      Ptr *stack_bottom = ((Ptr *)(void *)vm->curr_frame) - 1;
      check(stack_bottom - idx >= vm->curr_thd->stack);
      vm_push(vm, *(stack_bottom - idx));
      break;
    }
    case STORE_FRAME_RELATIVE: {
      u32 idx = data;
      if (idx == 255) idx = vm_adv_instr(vm);
      Ptr it = vm_pop(vm);
      Ptr *stack_bottom = ((Ptr *)(void *)vm->curr_frame) - 1;
      *(stack_bottom - idx) = it;
      break;
    }
    case POP:
      vm_pop(vm);
      break;
    case PUSHLIT: {
      u32 idx = data;
      if (idx == 255) idx = vm_adv_instr(vm);
      Ptr it = vm->curr_lits[idx];
      vm_push(vm, it);
      break;
    }
    case LOAD_GLOBAL: {
      // assumes it comes after a pushlit of a symbol
      *vm->curr_thd->stack = get_global(vm, *vm->curr_thd->stack);
      break;
    }
    case LOAD_GLOBAL_AT_IDX: {
      auto idx = data;
      if (idx == 255) idx = vm_adv_instr(vm);
      Ptr it = vm->curr_lits[idx];
      vm_push(vm, Symbol_get_value(it));
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
      break;
    }
    case BUILD_CLOSURE: {
      auto lambda = vm_pop(vm);
      auto array = vm->curr_frame->closed_over;
      auto closure = make_closure(vm, lambda, array);
      vm_push(vm, closure);
      break;
    }
    case SAVE_CLOSURE_ENV: {
      s64 count = vm_adv_instr(vm);
      // dbg("SAVE: ", count);
      auto top = vm->curr_frame->closed_over;
      vm_push(vm, top);
      while (count--) {
        vm->curr_frame->closed_over = array_get(vm->curr_frame->closed_over, 0);
      }
      break;
    }
    case RESTORE_CLOSURE_ENV: {
      vm->curr_frame->closed_over = vm_pop(vm);
      // dbg("RESTORE: ", vm->curr_frame->closed_over);
      break;
    }
    case PUSH_CLOSURE_ENV: {
      u64 count = vm_adv_instr(vm);
      auto array = alloc_closure_env(vm, count);
      auto a = as(PtrArray, array);
      auto d = a->data;
      // array_set(array, 0, vm->curr_frame->closed_over);
      d[0] = vm->curr_frame->closed_over;
      while (count--) { //@speed
        auto it = vm_pop(vm);
        // cout << " setting closure val " << it << endl;
        d[count + 1] = it;
        // array_set(array, count + 1, it);
      }
      vm->curr_frame->closed_over = array;
      break;
    }
    case POP_CLOSURE_ENV: {
      auto curr = vm->curr_frame->closed_over;
      if (isNil(curr)) {
        vm->error = "cannot pop null closure env ";
      } else {
        auto prev = array_get(curr, 0);
        vm->curr_frame->closed_over = prev;
      }
      break;
    }
    case BR_IF_ZERO: {
      auto it = vm_pop(vm);
      s64 jump = vm_adv_instr(vm);
      if ((u64)it.value == 0) {
        vm->pc = jump - 1; //-1 to acct for pc advancing
      }
      break;
    }
    case BR_IF_NOT_ZERO: {
      auto it = vm_pop(vm);
      s64 jump = vm_adv_instr(vm);
      if ((u64)it.value != 0) {
        vm->pc = jump - 1; //-1 to acct for pc advancing
      }
      break;
    }
    case BR_IF_False: {
      auto it = vm_pop(vm);
      s64 jump = vm_adv_instr(vm);
      if (ptr_eq(it, False)) {
        vm->pc = jump - 1; //-1 to acct for pc advancing
      }
      break;
    }
    case JUMP: {
      s64 jump = vm_adv_instr(vm);
      vm->pc = jump - 1; //-1 to acct for pc advancing
      break;
    }
    case PUSH_JUMP: {
      s64 jump = vm_adv_instr(vm);
      // dbg("PUSH_JUMP: ", jump);
      vm_push(vm,to(Fixnum, jump));
      break;
    }
    case POP_JUMP: {
      s64 jump = from(Fixnum, vm_pop(vm));
      // dbg("POP JUMP: ", jump);
      vm->pc = jump - 1; //-1 to acct for pc advancing
      break;
    }
    case DUP: {
      auto it = vm_pop(vm);
      vm_push(vm, it);
      vm_push(vm, it);
      break;
    }
    case SWAP: {
      auto a = vm_pop(vm);
      auto b = vm_pop(vm);
      vm_push(vm, a);
      vm_push(vm, b);
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
          switch(idx) {
          case 0: { // APPLY
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
          }
          case 1: { // SEND
            // dbg("preparing to send... ", idx);
            vm_interp_prepare_for_send(vm, argc);
            argc--;
            goto reenter_call;
          }
          case 2: { // SLEEP
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
          }
          case 3:{ // SEM_WAIT
            auto semaphore = vm_pop(vm);
            vm_push(vm, Nil);
            auto status = semaphore_get_count(semaphore);
            auto available = false;
            if (is(Bool, status)) {
              if (status == True) {
                available = true;
                semaphore_set_count(semaphore, False);
              }
            } else {
              auto ct = as(Fixnum, status);
              // dbg("in semaphore-wait, count is: ", ct);
              if (ct > 0) {
                available = true;
                semaphore_set_count(semaphore, to(Fixnum, ct - 1));
              }
            } 
            if (!available) {
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
          }
          case 4: { // KILL_THD
            auto thread = vm_pop(vm);
            vm_push(vm, Nil);
            thread_set_status(thread, THREAD_STATUS_DEAD);
            if (thread == vm->curr_thd->thread) {
              auto exclusive = params.block_for_initial_thread;
              // FIXME: the check here should not be needed
              // if (!exclusive) _vm_unwind_to_root_frame(vm);
              if (exclusive || !vm_maybe_start_next_thread(vm)) {
                vm->suspended = true;
                goto exit;
              } else {
              }
            } else {
              // FIXME: leak
              thdq_remove_ptr(vm->threads, thread);
            }
            break;
          }
          }

          break; // from CALL
        } // end special built-ins

        // cerr << " calling prim at idx: " << idx << " arg count = " << argc << endl;
#if PRIM_USE_GIANT_SWITCH
        vm_push(vm, giant_switch(vm, argc, idx));
#else
        PrimitiveFunction fn = PrimLookupTable[idx];
        Ptr result = (*fn)(vm, argc);
        vm_push(vm, result);
#endif
        break; // from CALL
      }

      if (!is(Closure, fn)) {
        if (fn == Nil) {
          vm->error = "value is not a closure";
        } else {
          {
            // rotate fn into first position
            vm_push(vm, fn);
            for (auto n = 0; n < argc; n++) {
              vm->curr_thd->stack[n] = vm->curr_thd->stack[n+1];
            }
            vm->curr_thd->stack[argc] = fn;
          }
          argc++;
          vm_push(vm, applicator_for_object(vm, fn));
          goto reenter_call;
        }

#if GC_DEBUG
        if (is(BrokenHeart, fn)) {
          auto other = to(Ptr, gc_forwarding_address(as(Object, fn)));
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
      if (params.thread_switch_instr_budget && ctx_switch_budget <= 0) {
        // check if we need to poll for events
        if (params.total_execution_instr_budget &&
            spent_instructions >= params.total_execution_instr_budget) {
          // dbg("suspending execution");
          vm_suspend_current_thread(vm);
          if (vm->error) { dbg("error suspending: ", vm->error); }
          // dbg("suspended.");
          unprot_ptr(init_thread);
          return;
        }

        if (vm_swap_threads(vm)) {
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
      u8 idx = data;
      auto it = vm_load_arg(vm, idx);
      vm_push(vm, it);
      // cout << " loading arg "<< idx << ": " << it << endl;
      // vm_dump_args(vm);
      break;
    }
    case STORE_ARG: {
      u64 idx = data;
      auto it = vm_pop(vm);
      vm_store_arg(vm, idx, it);
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
      *vm->curr_thd->stack = get_special(vm, *vm->curr_thd->stack);
      break;
    }
    default:
      dbg("instr = ", instr, " code = ", (int)code, " data = ", data);
      dbg("bc = ", vm->curr_thd->bc);
      dbg("mk = ", vm->curr_thd->frame->mark);
      vm->error = "unexpected BC";
      print_stacktrace();
      exit(1);
      return;
    }

    if (vm->error) {
      if (!vm_handle_error(vm)) {
        vm->suspended = true;
        // vm->curr_thread->thread = Nil;
        goto exit;
      }
    }

    // assert(vm->curr_thread->stack <= vm->curr_thread->stack_start &&
    //        vm->curr_thread->stack >= vm->curr_thread->stack_end);
    // assert(vm->curr_thread->frame->argc < 100 && vm->curr_thread->frame->argc >= 0);
    // _debug_validate_stack(vm);
    // _debug_validate_background_threads(vm);

    ++vm->pc;
    // if (vm->curr_thd && vm->curr_thd->frame) { vm->curr_thd->frame->prev_pc = vm->pc; }
  }

 exit:

  // should ONLY be used by our EVAL, (not userspace one)
  // which we want to block in the strange case
  // that somone sleeps at the toplevel...
  if (params.block_for_initial_thread) {
    if(!vm->suspended &&
       init_thread == vm->curr_thd->thread) {
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


typedef std::tuple<u16*, string> branch_entry;

class BCBuilder {
private:
  VM* vm;
  u16* bc_mem;
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

  u16 *temp_count;
  Object *literals; // xarray[any]
  Ptr name;

  void _reserveInstruction() {
    if (bc_index - 1 >= bc_capacity) {
      bc_capacity *= 2;
      bc_mem = (u16 *)realloc(bc_mem, bc_capacity * sizeof(u16));
    }
  }
  // FIXME: audit all uses of pushPair
  BCBuilder* pushPair(u8 op, u16 data) {
    if (data >= 255) {
      pushU16(build_instr(op, 255));
      return pushU16(data);
    }
    return pushU16(build_instr(op, data));
  }
  BCBuilder* pushOp(u8 op) {
    return pushU16(build_instr(op, 0));
  }
  BCBuilder* pushU16(u16 it) {
    _reserveInstruction();
    bc_mem[bc_index++] = it;
    return this;
  }
  u16* pushEmptyRef() {
    auto location = bc_mem + bc_index;
    pushU16(0);
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
    // save source location as final slot in literals array
    {
      auto val = get_symbol_value(vm, KNOWN(Xsource_locationX));
      xarray_push(vm, to(Ptr, this->literals), val);
    }

    PtrArrayObject *array;
    {
      auto literal_count = xarray_used(to(Ptr, this->literals));
      array = alloc_pao(vm, Array, literal_count);
      auto literal_mem = xarray_memory(to(Ptr, this->literals));
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

    bc->name = this->name;
    unprot_ptr(this->name);
  }
public:
  BCBuilder(VM* vm) {
    this->vm = vm;

    bc_index            = 0;
    lit_index           = 0;
    bc_capacity         = 1024;
    bc_mem              = (u16 *)calloc(bc_capacity, sizeof(u16));
    labelsMap           = new std::map<string, u64>;
    branchLocations     = new std::vector<branch_entry>;
    labelContextCounter = 0;
    labelContext        = labelContextCounter;
    labelContextStack   = new std::vector<u64>;
    is_varargs          = false;

    // cleaned up in finalizeByteCode
    this->literals = as(Object, make_xarray(vm));
    gc_protect(this->literals);
    this->name = Nil;
    prot_ptr(this->name);

    pushOp(STACK_RESERVE);
    temp_count = &bc_mem[bc_index];
    pushU16(0);
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
  auto setName(Ptr name){
    this->name = name;
    return this;
  }
  auto isVarargs() {
    is_varargs = true;
    return this;
  }
  auto pushLit(Ptr literal) {
    auto literals = to(Ptr, this->literals);
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
  auto loadFrameRel(u16 idx) {
    pushPair(LOAD_FRAME_RELATIVE, idx);
    return this;
  }
  auto storeFrameRel(u16 idx) {
    pushPair(STORE_FRAME_RELATIVE, idx);
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
  auto call(u16 argc) {
    pushPair(CALL, argc);
    return this;
  }
  auto tailCall(u16 argc) {
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
  auto loadArg(u16 index) {
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
  auto loadClosure(u16 slot, u16 depth) {
    pushOp(LOAD_CLOSURE);
    pushU16(slot);
    pushU16(depth);
    return this;
  }
  auto storeClosure(u16 slot, u16 depth) {
    pushOp(STORE_CLOSURE);
    pushU16(slot);
    pushU16(depth);
    return this;
  }
  auto buildClosure() {
    pushOp(BUILD_CLOSURE);
  }
  auto pushClosureEnv(u16 count) {
    pushOp(PUSH_CLOSURE_ENV);
    pushU16(count);
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

ByteCodeObject *make_empty_bytecode(VM *vm){
  auto builder = new BCBuilder(vm);
  auto result = builder->build();
  delete builder;
  return result;
}


/* ---------------------------------------------------*/

void run_event_loop_with_display(VM *vm, int w, int h, bool from_image);

void vm_run_until_completion(VM *vm) {

  auto wants_display_sym = root_intern(vm, "wants-display"); prot_ptr(wants_display_sym);

  while (vm->threads->front) {

    {
      auto wants_display = get_global(vm, wants_display_sym);
      // launch display if requested
      if (is(Point, wants_display)) {
        auto p = as(Point, wants_display);
        run_event_loop_with_display(vm, p.x, p.y, false);
        break;
      } else {
        // do nothing
      }
    }

    auto success = vm_maybe_start_next_thread(vm);
    if (success) {
      vm->pc++;
      vm_interp(vm, INTERP_PARAMS_MAIN_EXECUTION);
    } else {
      // TODO: some kind of event loop will be more appropriate in the long run
      auto ms = _vm_threads_get_minimum_sleep_time(vm);
      if (ms < 0) {
        return;
      }
      usleep(ms * 1000);
    }

  }

  unprot_ptr(wants_display_sym);

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

/* -------- thread introspection -------------------- */

inline Ptr _stack_frame_load_arg(StackFrameObject *fr, u32 idx) {
  u64 argc = fr->argc;
  u64 ofs  = fr->pad_count;
  return fr->argv[ofs + (argc - (idx + 1))];
}

Ptr _stack_frame_get_args(VM *vm, StackFrameObject *fr) {
  auto result = Nil; prot_ptr(result);
  for (auto i = ((s32)fr->argc) - 1; i >= 0; i--) {
    result = cons(vm, _stack_frame_load_arg(fr, i), result);
  }
  unprot_ptr(result);
  return result;
}

Ptr get_source_location(Ptr it) {
  if (is(Closure, it)) {
    auto bc = closure_code(it);
    return bc->literals->data[bc->literals->length - 1];
  }
  return Nil;
}

Ptr _current_thread_get_debug_info(VM *vm){
  auto result = Nil;                         prot_ptr(result);
  auto fr = vm->curr_frame;                  gc_protect(fr);

  while (fr) {
    auto args = _stack_frame_get_args(vm, fr);
    auto frame = cons(vm, fr->bc->name, args);
    result = cons(vm, frame, result);
    fr = fr->prev_frame;
  }
  result = _list_rev(result);

  gc_unprotect(fr);
  unprot_ptr(result);
  return result;
}


Ptr thread_get_debug_info(VM *vm, Ptr thread) {
  if (thread == vm->curr_thd->thread) {
    return _current_thread_get_debug_info(vm);
  }
  return Nil;
}

void _print_debug_frame(StackFrameObject *fr) {
  std::cerr << "   " << fr->bc->name;
  for (auto i = 0; i < ((s32)fr->argc); i++) {
    std::cerr << " " << _stack_frame_load_arg(fr, i);
  }
  std::cerr << std::endl;
}
void _print_debug_stacktrace(thread_ctx *thd) {
  // auto thread = thd->thread;
  auto fr = thd->frame;
  dbg("userspace stacktrace:");
  while (fr) {
    _print_debug_frame(fr);
    fr = fr->prev_frame;
  }
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
  auto parent = cenv_get_prev(cenv);
  return as(Bool, cenv_get_has_closure(cenv)) || (parent != Nil && cenv_has_closure(parent));
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
  #if STATS
  vm->stats->flat_lambda_count++;
  #endif
  prot_ptrs(it, env);
  Ptr bc;
  {
    auto builder = BCBuilder(vm);
    auto name = car(cdr(it));
    builder.setName(name);
    auto arglist = car(cdr(cdr(it)));
    if (is(Symbol, arglist)) builder.isVarargs();
    auto body = cdr(cdr(cdr(it)));
    emit_lambda_body(vm, &builder, body, env);
    builder.ret();
    bc = to(Ptr, builder.build());
  }
  unprot_ptrs(it, env);
  return make_closure(vm, bc, Nil);
}

void emit_lambda(VM *vm, BCBuilder *parent, Ptr it, Ptr p_env) {  prot_ptrs(it, p_env);
  auto env = compiler_env_get_subenv(vm, p_env, it);              prot_ptr(env);
  auto has_closure = cenv_has_closure(env);
  if (has_closure) {
    #if STATS
    vm->stats->closure_lambda_count++;
    #endif

    auto closed = cenv_get_closed_over(env);                      prot_ptrs(closed);
    auto closed_count = xarray_used(closed);

    {
      auto builder = BCBuilder(vm);

      auto name = car(cdr(it));
      builder.setName(name);
      auto arglist = car(cdr(cdr(it)));
      if (is(Symbol, arglist)) builder.isVarargs();

      for (u64 i = 0; i < closed_count; i++) {
        auto ptr     = xarray_at(closed, i);
        auto binding = compiler_env_binding(vm, env, ptr);
        auto index   = as(Fixnum, varinfo_get_argument_index(binding.variable_info));
        builder.loadArg(index);
      }
      unprot_ptrs(closed);

      builder.pushClosureEnv(closed_count);
      auto body = cdr(cdr(cdr(it)));
      emit_lambda_body(vm, &builder, body, env);
      builder.ret();
      parent->pushLit(to(Ptr, builder.build()));
    }
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

void emit_with_special_binding(VM *vm, BCBuilder *builder, Ptr it, Ptr env) { prot_ptrs(it, env);
  auto sym       = car(cdr(it));
  builder->pushLit(sym);
  auto val_expr  = car(cdr(cdr(it)));
  emit_expr(vm, builder, val_expr, env, false);
  builder->pushSpecialBinding();
  auto body_form = car(cdr(cdr(cdr(it))));
  emit_expr(vm, builder, body_form, env, false);
  builder->popSpecialBinding();
  unprot_ptrs(it, env);
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

  it = cdr(cdr(it));
  auto zero = to(Fixnum, 0);
  u64 idx = 0;

  // @safe
  {
    auto args    = car(it);                                     prot_ptr(args);
    auto var_map = cenv_get_info(env);                          prot_ptr(var_map);

    auto mark_arg = [&](Ptr arg){                               prot_ptrs(arg);
      if (!is(Symbol, arg)) { dbg(it); dbg(arg); };
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
  s64 min_x = std::max((s64)a.x, (s64)0);
  s64 min_y = std::max((s64)a.y, (s64)0);
  s64 max_x = std::min((s64)b.x, dst->width);
  s64 max_y = std::min((s64)b.y, dst->height);
  if (alpha == 255) {
    for (s64 y = min_y; y < max_y; y++) {
      for (s64 x = min_x; x < max_x; x++) {
        auto idx = y * dst->pitch + x * 4;
        auto mem = dst->mem + idx;
        mem[0] = over[0];
        mem[1] = over[1];
        mem[2] = over[2];
        mem[3] = 255;
      }
    }
  } else {
    for (s64 y = min_y; y < max_y; y++) {
      for (s64 x = min_x; x < max_x; x++) {
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
                   f32 deg_rot,
                   u32 tint) {

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

  f32 tint_a, tint_r, tint_g, tint_b;
  {
    u8* tints = (u8*)&tint;
    tint_r = tints[0] / 255.0;
    tint_g = tints[1] / 255.0;
    tint_b = tints[2] / 255.0;
    tint_a = tints[3] / 255.0;
  }

  for (s32 y = 0; y < bottom; y++) {

    blit_sampler_start_row(&bs_src);
    auto dest_row = (at.y + y) * dst->pitch;

    for (s32 x = 0; x < right; x++) {
      u8 *over;

      // it would be great if there were a way to do fewer checks here.
      if (at.x + x >= 0L && at.y + y >= 0L &&
          blit_sampler_sample(&bs_src, &over)) {

        u8* under = dst->mem + dest_row + (at.x + x) * 4;
        u8 alpha  = over[3];

        f32 malpha = (alpha / 255.0) * tint_a;

        // aA + (1-a)B = a(A-B)+B
        under[0] = (((over[0] * tint_r) - under[0]) * malpha)  + under[0];
        under[1] = (((over[1] * tint_g) - under[1]) * malpha)  + under[1];
        under[2] = (((over[2] * tint_b) - under[2]) * malpha)  + under[2];
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

Ptr _gfx_fill_rect_with_mask(u32 color, blit_surface *dst, blit_surface *msk,
                             rect *from, f32 scale, f32 deg_rot,
                             rect *m_from, f32 m_scale, f32 m_deg_rot
                             ) {

  auto at_y = from->y, at_x = from->x;
  
  u32 scan_width; u32 scan_height;
  // TODO: @speed properly calculate scan width and height (rotate rect and get bounds)
  {
    f32 sw = from->width  * scale;
    f32 sh = from->height * scale;
    scan_width = scan_height = (u32)floorf(sqrtf(sw * sw + sh * sh));
  }

  s32 right  = std::min((s32)scan_width, (s32)(dst->width - at_x));
  s32 bottom = std::min((s32)scan_height, (s32)(dst->height - at_y));

  blit_sampler bs_msk;
  blit_sampler_init(&bs_msk, msk, m_scale, m_deg_rot, m_from);

  for (s32 y = 0; y < bottom; y++) {

    blit_sampler_start_row(&bs_msk);
    auto dest_row = (at_y + y) * dst->pitch;

    for (s32 x = 0; x < right; x++) {
      u8 *over = (u8*)(u32*)&color;
      u8 *mask;

      // it would be great if there were a way to do fewer checks here.
      if (at_x + x >= 0L && at_y + y >= 0L &&
          blit_sampler_sample(&bs_msk, &mask)) {

        u8* under = dst->mem + dest_row + (at_x + x) * 4;
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

      blit_sampler_step_col(&bs_msk);
    }

    blit_sampler_step_row(&bs_msk);
  }

  return Nil;
}

Ptr gfx_fill_rect_with_mask(u32 color,
                             ByteArrayObject *dst_img,
                             ByteArrayObject *msk_img,
                             rect from, f32 scale, f32 deg_rot,
                             rect m_from, f32 m_scale, f32 m_deg_rot
                             ) {
  blit_surface dst = image_blit_surface(dst_img);
  blit_surface msk = image_blit_surface(msk_img);
  return _gfx_fill_rect_with_mask(color, &dst, &msk, &from, scale, deg_rot, &m_from, m_scale, m_deg_rot);
};


Ptr gfx_blit_image_at(VM *vm, ByteArrayObject* img, point p, s64 scale100, s64 deg_rot) {
  if (!vm->surface) return Nil;
  if (!is(Image, to(Ptr, img))) die("gfx_blit_image: not an image");
  auto src  = image_blit_surface(img);
  auto dst = vm->surface;
  vm->screen_dirty = true;

  auto from = (rect){ 0, 0, src.width, src.height };
  return gfx_blit_image(&src, dst, &from, p, scale100/100.0f, deg_rot * 1.0f, 0xffffffff);
}

Ptr gfx_blit(ByteArrayObject *source_image, ByteArrayObject *dest_image,
             point dst_location,
             point src_upper_left,
             point src_lower_right,
             f32 scale,
             f32 degrees_rotation,
             u32 tint) {
  if (!is(Image, to(Ptr, source_image))) die("gfx_blit_image: not an image");
  if (!is(Image, to(Ptr, dest_image)))   die("gfx_blit_image: not an image");
  auto src = image_blit_surface(source_image);
  auto dst = image_blit_surface(dest_image);
  auto from = points_to_rect(src_upper_left, src_lower_right);
  return gfx_blit_image(&src, &dst, &from, dst_location, scale, degrees_rotation, tint);
}

// TODO: it would be nice to represent the screen as just an image,
//       but not sure how to do that without excessive copying.
Ptr gfx_blit_from_screen(VM *vm, ByteArrayObject *dest_image,
                         point dst_location,
                         point src_upper_left,
                         point src_lower_right,
                         f32 scale,
                         f32 degrees_rotation) {
  if (!is(Image, to(Ptr, dest_image)))   die("gfx_blit_image: not an image");
  auto dst = image_blit_surface(dest_image);
  auto from = points_to_rect(src_upper_left, src_lower_right);
  return gfx_blit_image(vm->surface, &dst, &from, dst_location, scale, degrees_rotation, 0xffffffff);
}

/* 
   a b
   c d
*/

f32 lerp_angle(f32 amt, f32 a, f32 b) {
  if (a >= 0 && b >= 0) return a * (1.0 - amt) + amt * b;
  if (a <= 0 && b <= 0) return a * (1.0 - amt) + amt * b;
  if (a <= 0 && b >= 0) {
    auto mid = lerp_angle(amt, 0, a * -1 + b);
    return mid + a;
  }
  if (a >= 0 && b < 0) {
    auto mid = lerp_angle(amt, b * -1 + a, 0);
    return mid + b;
  }
  auto mid = lerp_angle(amt, a+b, 0);
  return mid - b;
}

f32 angle_between_points(point a, point b) {
  return atan2f(b.y - a.y, b.x - a.x);
}

point lerp_points(f32 amt, point a, point b) {
  s32 x = amt * a.x + (1.0 - amt) * b.x;
  s32 y = amt * a.y + (1.0 - amt) * b.y;
  return (point){x, y};
}

s64 point_distance(point pa, point pb) {
  auto a = pb.x - pa.x;
  auto b = pb.y - pa.y;
  return sqrtf(a*a + b*b);
}

struct quad_scan_state {
  point a, b, c, d;

  f32 ldx, ldy, rdx, rdy;
  f32 lscale, rscale;
  f32 clx, cly, crx, cry;
  f32 cx, cy;
  f32 step_count;
  f32 width_remaining;

  f32 dx, dy;
  s64 x, y;
};

void quad_scan_state_init(quad_scan_state *q, 
                          point d_a, point d_b, point d_c, point d_d) {


  q->a = d_a; q->b = d_b; q->c = d_c; q->d = d_d;

  if (q->a.x == q->b.x) q->b.x++;

  {
    f32 dleft       = point_distance(q->c , q->a);
    f32 dright      = point_distance(q->d , q->b);
    f32 left_angle  = angle_between_points(q->a, q->c);
    f32 right_angle = angle_between_points(q->b, q->d);
    q->ldx = cosf(left_angle) * 0.5; q->ldy = sinf(left_angle) * 0.5;
    q->rdx = cosf(right_angle) * 0.5; q->rdy = sinf(right_angle) * 0.5;

    if (dleft > dright) {
      q->lscale = 1.0;
      q->rscale = dright / dleft; 
      q->step_count = dleft * 2.0;
    } else {
      q->lscale = dleft / dright;
      q->rscale = 1.0;
      q->step_count = dright * 2.0;
    }

    q->clx = q->a.x; q->cly = q->a.y;
    q->crx = q->b.x; q->cry = q->b.y;
  }
}

void quad_scan_state_init_reading(quad_scan_state *q, 
                                  point d_a, point d_b, point d_c, point d_d,
                                  quad_scan_state *w) {

  quad_scan_state_init(q, d_a, d_b, d_c, d_d);
  q->lscale *= q->step_count / w->step_count;
  q->rscale *= q->step_count / w->step_count;
  q->step_count = w->step_count;
}

void quad_scan_state_start_row(quad_scan_state *q) {
  q->clx += q->ldx * q->lscale;
  q->cly += q->ldy * q->lscale;
  q->crx += q->rdx * q->rscale;
  q->cry += q->rdy * q->rscale;
  f32 a = q->cry - q->cly;
  f32 b = q->crx - q->clx;
  f32 angle = atan2f(a, b);
  f32 dist = sqrtf(a*a + b*b);
  q->dx = cosf(angle) * 0.5;
  q->dy = sinf(angle) * 0.5;
  q->cx = q->clx;
  q->cy = q->cly;
  q->width_remaining = dist * 2.0;
}

void quad_scan_state_start_row_reading(quad_scan_state *q, quad_scan_state *w) {
  quad_scan_state_start_row(q);
  q->dx *= q->width_remaining / w->width_remaining;
  q->dy *= q->width_remaining / w->width_remaining;
  q->width_remaining = w->width_remaining;
}

bool quad_scan_state_start_col(quad_scan_state *q) {
  q->width_remaining--;
  q->x = (s64)roundf(q->cx); q->y = (s64)roundf(q->cy);
  q->cx += q->dx; q->cy += q->dy;
  return q->width_remaining > 0.9999999999;
}

void _gfx_blit_image_into_quad(blit_surface *src, blit_surface *dst,
                               quad_scan_state *read, quad_scan_state *write){

  auto line_count = write->step_count;

  for (auto line = 0; line < line_count; line++) {
    quad_scan_state_start_row(write);
    quad_scan_state_start_row_reading(read, write);

    if (write->dx != 0 && read->dx != 0) {

      while (quad_scan_state_start_col(write)) {
        quad_scan_state_start_col(read);

        auto x = write->x; auto y = write->y;
        // @speed some way to eliminate this many checks per pixel
        if (x >= 0 && x < dst->width && y >= 0 && y < dst->height
            && read->x >= 0 && read->x < src->width && read->y >= 0 && read->x <= src->height) {

          auto src_row = read->y * src->pitch;
          auto dest_row = write->y * dst->pitch;

          u8* under = dst->mem + dest_row + x * 4;
          u8* over = src->mem + src_row + read->x * 4;

          u8 alpha  = over[3];

          // aA + (1-a)B = a(A-B)+B
          under[0] = ((over[0] - under[0]) * alpha /  255)  + under[0];
          under[1] = ((over[1] - under[1]) * alpha /  255)  + under[1];
          under[2] = ((over[2] - under[2]) * alpha /  255)  + under[2];
          u8 ualpha = under[3];
          u8 calpha = alpha + ualpha;
          under[3] = calpha < alpha ? 255 : calpha;
          // under[0] = under[1] = under[2] = l * 255;
          // under[3] = 255;
        }
      }
    }
  }
}

Ptr gfx_blit_image_into_quad(ByteArrayObject *src, ByteArrayObject *dst,
                             point s_a, point s_b, point s_c, point s_d,
                             point d_a, point d_b, point d_c, point d_d
                             ) {

  quad_scan_state write, read;
  quad_scan_state_init(&write, d_a, d_b, d_c, d_d);
  quad_scan_state_init_reading(&read, s_a, s_b, s_c, s_d, &write);

  auto src_s = image_blit_surface(src);
  auto dst_s = image_blit_surface(dst);
  _gfx_blit_image_into_quad(&src_s, &dst_s, &read, &write);
  
  return Nil;
}


/* -------------------------------------------------- */

Ptr compile_to_closure(VM *vm, Ptr expr) {
  auto bc = to(Ptr, _compile_toplevel_expression(vm, expr, true));
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
  auto len = ba_length(bao) + 1;
  auto mem = (char *)calloc(len, 1);
  memcpy(mem, ba_data(bao), len - 1);
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

void _vm_poke_arguments(VM *vm, run_info info) {
  auto args = Nil; prot_ptr(args);
  for (auto i = info.argc - 1; i >= 0; i--) {
    args = cons(vm, make_string(vm, info.argv[i]), args);
  }
  set_global(vm, root_intern(vm, "*command-line-args*"), args);
  unprot_ptr(args);
}

void load_file(VM *vm, const char* path);
void _debug_assert_in_heap(VM *vm, Ptr p) {
  if (p == Nil || ! is(Object, p)) return;
  #ifndef NDEBUG
  auto it = as(Object, p);
  assert(it >= vm->heap_mem && it < vm->heap_end);
  #endif
}

void vm_init_from_heap_snapshot(VM *vm) {
  vm->gc_disabled = true;
  _vm_update_collection_limit(vm);

  // set system dictionary
  vm->system_dictionary = to(Ptr, (Object *)(vm->heap_mem));
  _debug_assert_in_heap(vm, vm->system_dictionary);
  dbg("loaded system dictionary: ", vm->system_dictionary);
  assert(is(ht, vm->system_dictionary));

  // set current color
  vm->current_color = from(Fixnum, ht_at(vm->system_dictionary, SYSTEM_CURRENT_COLOR_KEY));

  // set root package
  vm->globals->lang_package = ht_at(vm->system_dictionary, SYSTEM_LANG_PACKAGE_KEY);
  _debug_assert_in_heap(vm, vm->globals->lang_package);
  assert(is(package, vm->globals->lang_package));

  // set main thread
  vm->curr_thd->thread = ht_at(vm->system_dictionary, SYSTEM_CURRENT_THREAD_KEY);
  // set built in classes
  {
    auto xarray = ht_at(vm->system_dictionary, SYSTEM_BUILTIN_CLASSES_KEY);
    _built_in_classes_restore_from_xarray(vm, xarray);
  }

  assert(is(thread, vm->curr_thd->thread));
  _debug_assert_in_heap(vm, vm->curr_thd->thread);

  // clear out old threads, add new threads
  thdq_remove_all(vm->threads);
  assert(vm->threads->front == vm->threads->back);
  assert(!vm->threads->front);

  auto thread_list = ht_at(vm->system_dictionary, SYSTEM_OTHER_THREADS_KEY);
  auto count = 0;
  do_list(vm, thread_list, [&](Ptr thread) {
      thdq_push_ptr(vm->threads, thread);
      count++;
    });
  dbg("restored ", count, " threads + main = ", vm->curr_thd->thread);

  // re-init known symbols
  initialize_known_symbols(vm);
  vm->gc_disabled = false;
}

void vm_init_for_blank_startup(VM *vm, run_info info) {
  vm->gc_disabled = true;

  vm->system_dictionary = ht(vm); // should be the first allocation

  vm->globals->lang_package = make_package(vm,
                                           make_string(vm, "lang"),
                                           string_table(vm),
                                           ht(vm),
                                           Nil,
                                           string_table(vm),
                                           ht(vm));

  vm->curr_thd->thread = make_thread(vm,
                                     Nil,
                                     THREAD_STATUS_RUNNING,
                                     Nil,
                                     FIXNUM(0),
                                     THREAD_PRIORITY_NORMAL,
                                     Nil,
                                     Nil);

  ht_at_put(vm, vm->system_dictionary,
            SYSTEM_LANG_PACKAGE_KEY, vm->globals->lang_package);

  initialize_known_symbols(vm);
  initialize_classes(vm);
  initialize_primitive_functions(vm);
  initialize_global_variables(vm);

  vm->gc_disabled = false;

  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->curr_thd->frame->mark = KNOWN(exception);

  // load the stdlib
  load_file(vm, "./boot/built-in-classes.lisp");
  load_file(vm, "./boot/0.lisp");
  load_file(vm, "./boot/0-package.lisp");
  load_file(vm, "./meta-reader/0-compiler.lisp");
  load_file(vm, "./meta-reader/1-lisp-handwritten.lisp");
  load_file(vm, "./meta-reader/1-meta-handwritten.lisp");
  load_file(vm, "./meta-reader/2-bootstrap.lisp");
  load_file(vm, "./boot/1.lisp");

  _vm_poke_arguments(vm, info);
}

inline void _vm_update_collection_limit(VM *vm) {
  vm->collection_limit = std::min((u8 *)vm->heap_end + vm->gc_threshold_in_bytes,
                                  (u8 *)vm->heap_mem + vm->heap_size_in_bytes);
}

VM *_vm_create() {
  VM *vm;
  vm = (VM *)calloc(sizeof(VM), 1);
  DEBUG_VM = vm;

  #if STATS
  vm->stats = (stats *)calloc(sizeof(stats), 1);
  #endif

  vm->curr_thd = make_thread_ctx();

  auto heap_size_in_mb = 512;
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
  vm->gc_threshold_in_bytes = heap_size_in_bytes * 0.5;

  // vm->gc_protected = new std::unordered_map<Object **, u64>;
  vm->gc_protected_vec = new std::vector<u64>;
  vm->gc_protected_ptrs_vec = new std::vector<Ptr *>;
  // vm->gc_protected_ptrs = new std::unordered_map<Ptr *, u64>;
  vm->gc_protected_ptr_vectors = new std::unordered_map<Ptr *, u64>;
  // vm->gc_protected->reserve(100);
  // vm->gc_protected_ptrs->reserve(100);
  vm->gc_protected_vec->reserve(256);
  vm->gc_protected_ptrs_vec->reserve(256);
  vm->gc_protected_ptr_vectors->reserve(100);

  _vm_update_collection_limit(vm);

  vm->in_gc = false;

  vm->threads = (thdq *)calloc(sizeof(thdq), 1);
  vm->suspended = false;

  vm->curr_thd->frame = 0;
  vm->error = 0;

  vm->globals = (Globals *)calloc(sizeof(Globals), 1);

  return vm;
}

VM *vm_create(run_info info) {
  VM *vm = _vm_create();
  vm_init_for_blank_startup(vm, info);
  return vm;
}

VM *vm_create_from_image(const char *path, run_info info) {
  VM *vm = _vm_create();

  static_memory *smem = 0;

  // read the image into heap memory
  {
    FILE *in = fopen(path, "rb");
    assert(in != NULL);

    image_header header;
    // read the header
    {
      fread((char *)&header, 1, sizeof(image_header), in);
    }
    // read the heap
    {
      auto data = (char*)vm->heap_mem;
      auto amt_read = fread(data, 1, header.heap_size, in);
      assert(amt_read == header.heap_size);
      if (amt_read != header.heap_size) {
        dbg("failed to read full heap image.");
        exit(1);
      }
    }
    vm->heap_end = (void *)((u64)vm->heap_mem + header.heap_size);
    dbg("read heap size: ", (1.0 * header.heap_size) / (1024 * 1024));

    // read the static memory
    vm->static_mem_allocation_count = header.static_memory_allocation_count;
    {
      smem = (static_memory *)calloc(header.static_size, 1);
      auto amt_read = fread(smem, 1, header.static_size, in);
      assert(amt_read == header.static_size);
      if (amt_read != header.static_size) {
        dbg("failed to read full static memory.");
        exit(1);
      }
    }
    dbg("read static size : ", (1.0 * header.static_size) / (1024 * 1024));

    fclose(in);

  }

  // fixup the pointers
  {
    s64 delta = (u64)vm->heap_mem - SYSTEM_HEAP_IMAGE_OFFSET;
    bang_heap(vm->heap_mem, vm->heap_end, [&](Ptr it) {
        return im_offset_ptr(it, delta);
      });
  }

  _debug_heap_report(vm->heap_mem, vm->heap_end);

  // assert heap integrity
  {
    scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it) {
        if (it == Nil || !is(Object, it)) return;
        #ifndef NDEBUG
        auto where = as(Object, it);
        assert(vm->heap_mem <= where && vm->heap_end > where);
        #endif 
      });
  }

  // set up static memory
  {
    std::map<static_memory*, static_memory*>fixups;

    static_memory *src = smem;
    auto count = vm->static_mem_allocation_count;
    dbg("fixing up ", count, " allocations");
    while (count--) {
      auto size = sizeof(static_memory) + src->byte_count;
      auto dst = (static_memory *)calloc(size, 1);
      fixups[src->next] = dst;
      memcpy(dst, src, size);
      dst->next = vm->static_mem;
      vm->static_mem = dst;
      src = (static_memory*)(((u8*)src) + size);
    }

    scan_heap(vm->heap_mem, vm->heap_end, [&](Ptr it) {
        if (it == Nil || !is(ByteArray, it)) return;
        #if BA_STATIC_MEM
        auto ba = as(ByteArray, it);
        auto mem = fixups[ba->mem];
        assert(mem);
        ba->mem = mem;
        #endif
      });
  }

  vm_init_from_heap_snapshot(vm);

  // resume main thread

  // so we have a root frame
  auto bc = make_empty_bytecode(vm);
  vm_push_stack_frame(vm, 0, bc, Nil);
  vm->curr_frame->mark = KNOWN(exception);
  _debug_validate_stack(vm);

  vm->error = 0;
  vm->suspended = false;

  _vm_thread_resume(vm, vm->curr_thd);
  _debug_validate_stack(vm);

  _vm_poke_arguments(vm, info);

  free(smem);
  return vm;
}

Ptr vm_call_global(VM *vm, Ptr symbol, u64 argc, Ptr argv[]);

Ptr compile_toplevel_expression_with_hooks(VM *vm, Ptr expr) {
  if (boundp(vm, KNOWN(compiler))) {
    Ptr new_expr = vm_call_global(vm, KNOWN(compiler), 1, (Ptr[]){expr});
    return make_closure(vm, to(Ptr, compile_toplevel_expression(vm, new_expr)), Nil);
  } else {
    return make_closure(vm, to(Ptr, compile_toplevel_expression(vm, expr)), Nil);
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
    if (vm->curr_frame->prev_frame) {
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

void start_up_and_run_repl(run_info info) {
  VM *vm = vm_create(info);

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
  dbg("loading.... ", path);
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
    auto lits = to(Ptr, bc->literals);
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
  vm->curr_frame->mark = KNOWN(exception);

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
  vm->window = window;

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
  auto onmouseup   = root_intern(vm, "onmouseup");   prot_ptr(onmouseup);
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
        char key = event.key.keysym.sym;
        Ptr num = to(Char, key);
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
      case SDL_MOUSEBUTTONUP: {
        auto x = event.button.x;
        auto y = event.button.y;
        auto p = (point){x, y};
        auto pt = to(Point, p);
        vm_poke_event(vm, pending_events, event_ready_semaphore, onmouseup, pt);
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
  auto run_time = (current_time_ms() - vm->start_time_ms) / 1000.0;
  s64 ops_per_second = vm->instruction_count / run_time;
  dbg(" executed ", vm->instruction_count, " instructions over ", run_time, " seconds.");
  dbg(" average of ", ops_per_second, " ops per second, including sleep. ");
  dbg(" gc count: ", vm->gc_count);
  report_memory_usage(vm);
  #if STATS
  report_stats(vm->stats);
  #endif

  SDL_Quit();
}

void run_file_with_optional_display(const char * path, run_info info) {
  VM *vm = vm_create(info);
  auto str = read_file_contents(path);

  auto done  = cons(vm, Nil, Nil);         prot_ptr(done);
  auto start = str;
  auto end = str + strlen(str) - 1;
  const char *curr = start;
  while (curr < end) {
    auto form = read(vm, &curr, end, done);
    eval(vm, form);
    if (vm->curr_thd->thread != Nil &&
        thread_get_status(vm->curr_thd->thread) == THREAD_STATUS_DEAD) {
      vm->curr_thd->thread = Nil;
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

void start_up_and_run_image(const char* path, run_info info) {
  VM *vm = vm_create_from_image(path, info);

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

  run_info info;
  info.argc = argc;
  info.argv = argv;

  // pretty hacky way of avoiding checking flags, I'll admit...
  if (strcmp(invoked, "boot") == 0) {
    auto file = require_argv_file(argc, argv);
    run_file_with_optional_display(file, info);
  } else if (strcmp(invoked, "img") == 0){
    auto file = require_argv_file(argc, argv);
    start_up_and_run_image(file, info);
  } else if (strcmp(invoked, "repl") == 0){
    start_up_and_run_repl(info);
  } else {
    std::cerr << " unrecognized invocation: " << invoked << std::endl;
    return 2;
  }
  return 0;
}
