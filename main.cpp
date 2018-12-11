/*

 g++ main.cpp -Werror -std=c++14 && ./a.out
 
(setq flycheck-clang-language-standard "c++14")

TODO: stack traces
DONE: move allocations into vm-managed heap
DONE: lists (incl printing)
DONE: lisp reader
DONE: expression compiler
DONE: lambda compiler
TODO: lambdas + closures compiler
TODO: if compiler
TODO: move stack memory into vm-managed heap
TODO: garbage collection
TODO: continuations / exceptions / signals
TODO: dump/restore image

*/

#include <cassert>
#include <cstring>
#include <iostream>
#include <string>
#include <map>
#include <unordered_map>
#include <vector>
#include <tuple>
#include <fstream>
#include <sstream>

using namespace std;

typedef unsigned int uint;
typedef uint64_t u64;
typedef int64_t s64;
typedef uint8_t u8;
typedef int8_t s8;

enum ObjectType : u64 {
  ByteCode_ObjectType,
  RawPointer_ObjectType,
  ByteArray_ObjectType,
  U64Array_ObjectType,
  PtrArray_ObjectType,
  StdObject_ObjectType
};

struct Header {
  ObjectType object_type;
};
  
struct Object {
  Header header;
};

struct U64ArrayObject : Object {
  u64 length;
  u64 data[];
};

struct Ptr {
  u64 value;
};

bool ptr_eq(Ptr a, Ptr b) {
  return a.value == b.value;
}

struct ByteCode : Object {
  U64ArrayObject *code;
  Ptr literals[1024];
};

struct Frame {
  Ptr* prev_stack;
  Frame* prev_frame;
  ByteCode* prev_fn;
  u64* prev_pc;
  u64 argc;
  Ptr argv[];
};

struct Globals;

struct VM {
  Ptr *stack;
  void* heap_mem;
  void* heap_end;
  u64 heap_size_in_bytes;
  Frame *frame;
  u64 *pc;
  ByteCode *bc;
  const char* error;
  Globals *globals;
};

void * vm_alloc(VM *vm, u64 bytes) {
  auto result = (void *)vm->heap_end;
  static_assert(sizeof(u64) == sizeof(void *), "right pointer size");
  assert(((u64)result & 0b1111) == 0);
  auto rounded = ((bytes >> 4) << 4);
  u64 bump = rounded == bytes ? rounded : rounded + 16;
  vm->heap_end = ((u8*)vm->heap_end) + bump;
  assert(((u64)vm->heap_end & 0b1111) == 0);
  // cout << " alloc: bytes = " << bytes << " bump = " << bump << " end = " << vm->heap_end << endl;
  return result;
}

VM *CURRENT_DEBUG_VM;

/* -------------------------------------------------- */

typedef enum {
  String,
  Symbol
} BAOType;

typedef enum {
  Array
} PAOType;

struct ByteArrayObject : Object {
  BAOType bao_type;
  uint length;
  char data[];
};

struct PtrArrayObject : Object {
  PAOType pao_type;
  uint length;
  Ptr  data[];
};

struct RawPointerObject : Object {
  void *pointer;
};

struct StandardObject : Object {
  StandardObject *klass;
  u64 ivar_count;
  Ptr ivars[];
};

#define EXTRACT_PTR_MASK 0xFFFFFFFFFFFFFFF0
#define TAG_MASK 0b111
#define TAG_BITS 3
#define FIXNUM_TAG 0b000
#define OBJECT_TAG 0b001
#define NIL toPtr((Object *)0)

bool isFixnum(Ptr self) {
  return (self.value & TAG_MASK) == FIXNUM_TAG;
}
bool isObject(Ptr self) {
  return (self.value & TAG_MASK) == OBJECT_TAG;
}
bool isNil(Ptr self) {
  return (self.value == OBJECT_TAG);
}
Object *toObject(Ptr self) {
  return (Object *)(self.value & EXTRACT_PTR_MASK);
}
s64 toS64(Ptr self) {
  return ((s64)self.value) >> TAG_BITS;
}

Ptr toPtr(Object *ref) {
  Ptr p;
  p.value = ((u64) ref) |  0b1;
  return p;
}

Ptr toPtr(s64 value) {
  // TODO: overflow checking
  Ptr p;
  p.value = value << TAG_BITS;
  return p;
}

bool isBytecode(Ptr it) {
  return isObject(it) && toObject(it)->header.object_type == ByteCode_ObjectType;
}

U64ArrayObject *alloc_u64ao(VM *vm, uint len) {
  auto byte_count = sizeof(U64ArrayObject) + (len * sizeof(u64));
  U64ArrayObject* obj = (U64ArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteArray_ObjectType;
  obj->length = len;
  return obj;
}

ByteCode *alloc_bytecode(VM *vm) {
  auto byte_count = sizeof(ByteCode);
  ByteCode *obj = (ByteCode *)vm_alloc(vm, byte_count);
  obj->header.object_type = ByteCode_ObjectType;
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

bool isByteArrayObject(Ptr it) {
  return isObject(it) && toObject(it)->header.object_type == ByteArray_ObjectType;
}

bool isSymbol(Ptr it) {
  if (!isByteArrayObject(it)) return false;
  auto bao = (ByteArrayObject *)toObject(it);
  return bao->bao_type == Symbol;
}

PtrArrayObject *alloc_pao(VM *vm, PAOType ty, uint len) {
  auto byte_count = sizeof(PtrArrayObject) + (len * sizeof(Ptr));
  PtrArrayObject* obj = (PtrArrayObject *)vm_alloc(vm, byte_count);
  obj->header.object_type = PtrArray_ObjectType;
  obj->pao_type = ty;
  obj->length = len;
  return obj;
}

StandardObject *alloc_standard_object(VM *vm, StandardObject *klass, u64 ivar_count) {
  auto byte_count = (sizeof(StandardObject)) + ivar_count * (sizeof(Ptr));
  auto result = (StandardObject *)vm_alloc(vm, byte_count);
  result->header.object_type = StdObject_ObjectType;
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
  auto code = alloc_u64ao(vm, code_len);
  bc->code = code;
  return toPtr(bc);
}

Ptr make_string(VM *vm, const char* str) {
  ByteArrayObject *obj = alloc_bao(vm, String, strlen(str));
  const char *from = str;
  char *to = &(obj->data[0]);
  while(*from != 0) {
    *to = *from;
    to++; from++;
  }
  return toPtr(obj);
}

Ptr make_symbol(VM *vm, const char* str, u64 len) {
  ByteArrayObject *obj = alloc_bao(vm, Symbol, len);
  const char *from = str;
  char *to = &(obj->data[0]);
  while(len--) {
    *to = *from;
    to++; from++;
  }
  return toPtr(obj);
}

Ptr make_symbol(VM *vm, const char* str) {
  return make_symbol(vm, str, strlen(str));
}

Ptr make_number(s64 value) { return toPtr(value); }

Ptr make_raw_pointer(VM *vm, void* ptr) {
  RawPointerObject *obj = (RawPointerObject *)vm_alloc(vm, sizeof RawPointer_ObjectType);
  obj->header.object_type = RawPointer_ObjectType;
  obj->pointer = ptr;
  return toPtr(obj);
}

Ptr make_array(VM *vm, u64 len, Ptr objs[]) {
  auto array = alloc_pao(vm, Array, len);
  for (u64 i = 0; i < len; i++) {
    array->data[i] = objs[i];
  }
  return toPtr(array);
}


/* ---------------------------------------- */

enum {
  BaseClassName = 0,
  BaseClassIvarCount = 1,
  BaseClassDebugPrint = 2,
  BaseClassEnd = 3
};

typedef void(*DebugPrintFunction)(std::ostream &os, Ptr p);

std::ostream &operator<<(std::ostream &os, Ptr p);

std::ostream &operator<<(std::ostream &os, Object *obj) { 
  auto otype = obj->header.object_type;
  if (otype == ByteArray_ObjectType) {
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
  } else if (otype == PtrArray_ObjectType) {
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
  } else if (otype == StdObject_ObjectType) {
    auto sobj = (StandardObject *)obj;
    auto name = standard_object_get_ivar(sobj->klass, BaseClassName);
    auto pr_obj = standard_object_get_ivar(sobj->klass, BaseClassDebugPrint);
    if (isObject(pr_obj)) {
      Object *vobj = toObject(pr_obj);
      if (vobj->header.object_type == RawPointer_ObjectType) {
        auto rp  = (RawPointerObject *)vobj;
        auto fn = (DebugPrintFunction)rp->pointer;
        fn(os, toPtr(obj));
        return os;
      }
    }
    cout << "#<A " << toObject(name) << " " << (void*)obj << ">";
    return os;
  } else if (otype == ByteCode_ObjectType) {
    cout << "#<ByteCode " << (void*)obj << ">";
    return os;
  }
  return os << "don't know how to print object: " << otype << (void*)obj << endl;
}

std::ostream &operator<<(std::ostream &os, Ptr p) { 
  if (isNil(p)) {
    return os << "nil";
  } else if (isObject(p)) {
    return os << (toObject(p));
  } else if (isFixnum(p)) {
    return os << (toS64(p));
  } else {
    return os << "don't know how to print ptr.";
  }
}


/* ---------------------------------------- */

StandardObject *make_standard_object(VM *vm, StandardObject *klass, Ptr*ivars) {
  auto ivar_count_object = standard_object_get_ivar(klass, BaseClassIvarCount);
  assert(isFixnum(ivar_count_object));
  auto ivar_count = toS64(ivar_count_object);
  auto result = alloc_standard_object(vm, klass, ivar_count);
  for (auto i = 0; i < ivar_count; i++) {
    standard_object_set_ivar(result, i, ivars[i]);
  }
  return result;
}

bool isStdObj(Ptr p) {
  if (isNil(p)) return false;
  return isObject(p) && (toObject(p)->header.object_type == StdObject_ObjectType);
}

/* ---------------------------------------- */


typedef void *(*compiled)();

//  void *my_arg_grabber() {
//    asm(
//        "addq %rsi, %rdi\n"
//        "movq %rdi, %rax\n"
//        "popq %rbp\n"
//        "ret\n"
//        );
//  
//    // to silence the warnings
//    return 0;
//  }
//  
//  void my_arg_setter(u64 a, u64 b) {
//    //  asm("" :: "rsi"(a), "rdi"(b));
//  }

/* ---------------------------------------- */

struct Globals {
  StandardObject *Base, *Cons, *Fixnum, *Symbol;
  unordered_map<string, Ptr> *symtab;
  Ptr env;
};

auto make_base_class(VM *vm, const char* name, u64 ivar_count) {
  Ptr slots[] = {make_string(vm,name), make_number(ivar_count), toPtr((u64)0)};
  return make_standard_object(vm, vm->globals->Base, slots);
}

/* ---------------------------------------- */

bool consp(VM *vm, Ptr p) {
  if (!isStdObj(p)) return false;
  auto obj = (StandardObject *)toObject(p);
  auto res = (void *)obj->klass == (void *)vm->globals->Cons;
  return res;
}

Ptr car(VM *vm, Ptr p) {
  if (isNil(p)) return NIL;
  assert(consp(vm, p));
  return standard_object_get_ivar((StandardObject *)toObject(p), 0);
}

void set_car(VM *vm, Ptr cons, Ptr value) {
  assert(consp(vm, cons));
  standard_object_set_ivar((StandardObject *)toObject(cons), 0, value);
}

Ptr cdr(VM *vm, Ptr p) {
  if (isNil(p)) return NIL;
  assert(consp(vm, p));
  return standard_object_get_ivar((StandardObject *)toObject(p), 1);
}

void set_cdr(VM *vm, Ptr cons, Ptr value) {
  assert(consp(vm, cons));
  standard_object_set_ivar((StandardObject *)toObject(cons), 1, value);
}

Ptr cons(VM *vm, Ptr car, Ptr cdr) {
  auto obj = make_standard_object(vm, vm->globals->Cons, (Ptr[]){car, cdr});
  auto res = toPtr(obj);
  assert(consp(vm, res));
  return res;
}

Ptr assoc(VM *vm, Ptr item, Ptr alist) {
  while (!isNil(alist)) {
    auto pair = car(vm, alist);
    if (ptr_eq(car(vm, pair), item)) return pair;
    alist = cdr(vm, alist);
  }
  return NIL;
}

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

Ptr make_list(VM *vm, u64 len, Ptr* ptrs) {
  if (len == 0) return NIL;
  // TODO: iterative solution
  return cons(vm, *ptrs, make_list(vm, len - 1, ptrs + 1));
}

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


/* ---------------------------------------- */

void initialize_classes(VM *vm)
{
  auto Base = alloc_standard_object(vm, 0, BaseClassEnd);
  Base->klass = Base;
  standard_object_set_ivar(Base, BaseClassName, make_string(vm, "Base"));
  standard_object_set_ivar(Base, BaseClassIvarCount, make_number(BaseClassEnd));
  auto g = vm->globals;
  g->Base = Base;
  g->Cons = make_base_class(vm, "Cons", 2);
  standard_object_set_ivar(g->Cons, BaseClassDebugPrint,
                           make_raw_pointer(vm, (void*)&debug_print_list));
  g->Fixnum = make_base_class(vm, "Fixnum", 0);
  g->Symbol = make_base_class(vm, "Symbol", 0);
}

Ptr intern(VM *vm, const char* cstr, int len) {
  string name = string(cstr, len);
  auto tab = vm->globals->symtab;
  if (tab->find(name) == tab->end()) {
    auto sym = make_symbol(vm, cstr, len);
    tab->insert(make_pair(name, sym));   
  }
  auto res = tab->find(name)->second;
  return res;
}

Ptr intern(VM *vm, string name) {
  auto str = name.c_str();
  return intern(vm, str, strlen(str));
}

void set_global(VM *vm, const char* name, Ptr value) {
  set_assoc(vm, &vm->globals->env, intern(vm, name), value);
}

/* -------------------------------------------------- */

auto is_symchar(char ch) {
  return ch >= 'a' && ch <= 'z';
}
auto is_digitchar(char ch) {
  return ch >= '0' && ch <= '9';
}
auto is_symbodychar(char ch) {
  return is_symchar(ch) || is_digitchar(ch);
}
auto is_parens(char ch) {
  return ch == '(' || ch == ')';
}
auto is_q(char ch) {
  return ch == '\'';
}
auto is_wschar(char ch) {
  return !(is_symchar(ch) || is_digitchar(ch) || is_parens(ch) || is_q(ch));
}

auto quote_form(VM *vm, Ptr it) {
  auto res = cons(vm, it, NIL);
  auto q = intern(vm, "quote");
  return cons(vm, q, res);
}

Ptr read(VM *vm, const char **remaining, const char *end, Ptr done) {
  const char *input = *remaining;
  while (input < end) {
    while(input < end && is_wschar(*input)) input++;
    if (input >= end) break;
    if (is_symchar(*input)) {
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
      while(input < end && is_wschar(*input)) input++;
      vector<Ptr> items;
      while(input < end && *input != ')') {
        auto item = read(vm, &input, end, done);
        items.push_back(item);
        while(input < end && is_wschar(*input)) input++;
      }
      auto res = make_list(vm, items.size(), &items[0]);
      if (*input == ')') input++;
      *remaining = input;
      return res;
    } else if (is_digitchar(*input)) {
      u64 num = *input - '0';
      input++;
      while(input < end && is_digitchar(*input)) {
        num *= 10;
        num += *input - '0';
        input++;
      }
      *remaining = input;
      return toPtr(num);
    }
    input++;
  }
  return done;
}

Ptr read(VM *vm, const char* input) {
  auto len = strlen(input);
  return read(vm, &input, input+len, NIL);
}

Ptr read_all(VM *vm, const char* input) {
  auto done = cons(vm, NIL, NIL);
  auto len = strlen(input);
  vector<Ptr> items;
  auto end = input + len;
  auto item = read(vm, &input, end, done);
  while (input < end && !ptr_eq(item, done)) {
    assert(input < end);
    items.push_back(item);
    item = read(vm, &input, end, done);
    assert(input <= end);
  }
  auto res = make_list(vm, items.size(), &items[0]);
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

void vm_push_stack_frame(VM* vm, u64 argc, ByteCode*fn) {

  uint offset = (sizeof(Frame) / sizeof(u64));
  u64 *top = &((vm->stack - offset)->value);
  Frame *new_frame = (Frame *)top;

  // cout << "pushing stack frame from: " << vm->stack << endl;
  // cout << "  argc = " << argc << endl;
  // cout << "  offset = " << offset << endl;
  // cout << "  top = " << top << endl;
  // cout << "  &ps = " << &new_frame->prev_stack << endl;
  // cout << "  &ps = " << &new_frame->prev_fn << endl;
  // cout << "  &ps = " << &new_frame->prev_pc << endl;
  // cout << "  &ps = " << &new_frame->argc << endl;

  new_frame->prev_stack = vm->stack;
  new_frame->prev_frame = vm->frame;
  new_frame->prev_fn = vm->bc;
  new_frame->prev_pc = vm->pc;
  new_frame->argc = argc;
  vm->stack = (Ptr*)(void *)new_frame; // - 100; // STACK_PADDING
  vm->frame = new_frame;
  vm->bc = fn;
  vm->pc = fn->code->data;
}

typedef Ptr (*CCallFunction)(VM*);

enum OpCode {
  END = 0,
  RET = 1,
  PUSHLIT = 2,
  POP = 3,
  FFI_CALL = 4,
  BR_IF_ZERO = 5,
  BR_IF_NOT_ZERO = 6,
  DUP = 7,
  CALL = 8,
  LOAD_ARG = 9,
  LOAD_GLOBAL = 10
};

void vm_push(VM* vm, Ptr value) {
  *(--vm->stack) = value;
}

Ptr vm_pop(VM* vm) {
  return *(vm->stack++);
}

void vm_ffi_call(VM* vm) {
  Ptr ptr = vm_pop(vm);
  // cout << "ffi calling: " << ptr << endl;
  if (!isObject(ptr)) {
    vm->error = "integer is not a pointer";
    return;
  }
  Object *top = toObject(ptr);
  if (top->header.object_type != RawPointer_ObjectType) {
    vm->error = "not a pointer";
    return;
  }
  RawPointerObject *po = (RawPointerObject *)top;
  CCallFunction fn = (CCallFunction)(po->pointer);
  Ptr result = (*fn)(vm);
  // cout << " ffi call returned: " << result << endl;
  vm_push(vm, result);
}

void vm_interp(VM* vm) {
  u64 instr;
  while ((instr = *vm->pc)) {
    switch (instr){
    case POP:
      vm_pop(vm);
      break;
    case PUSHLIT: {
      u64 idx = *(++vm->pc);
      Ptr it = vm->bc->literals[idx];
      vm_push(vm, it);
      break;
    }
    case LOAD_GLOBAL: {
      // assumes it comes after a pushlit of a cell in the env alist.
      auto it = vm_pop(vm);
      vm_push(vm, cdr(vm, it));
      break;
    }
    case FFI_CALL:
      vm_ffi_call(vm);
      if (vm->error) return;
      break;
    case BR_IF_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = *(++vm->pc);
      if ((u64)it.value == 0) {
        vm->pc = vm->bc->code->data + (jump - 1); //-1 to acct for pc advancing
      } 
      break;
    }
    case BR_IF_NOT_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = *(++vm->pc);
      if ((u64)it.value != 0) {
        vm->pc = vm->bc->code->data + (jump - 1); //-1 to acct for pc advancing
      } 
      break;
    }
    case DUP: {
      auto it = vm_pop(vm);
      vm_push(vm, it);
      vm_push(vm, it);
      break;
    }
    case CALL: {
      u64 argc = *(++vm->pc);
      auto fn = vm_pop(vm);
      if (!isBytecode(fn)) {
        vm->error = "value is not bytecode";
        break;
      }
      auto bc = (ByteCode *)toObject(fn);
      vm_push_stack_frame(vm, argc, bc);
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
      u64 idx = *(++vm->pc);
      u64 argc = vm->frame->argc;
      auto it = vm->frame->argv[argc - (idx + 1)];
      vm_push(vm, it);
      // cout << "loading arg "<< idx << ": " << it << endl;
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

class ByteCodeBuilder {
private:
  VM* vm;
  u64* bc_mem;
  u64 bc_index;
  u64 lit_index;
  ByteCode *bc;
  map<string, u64> *labelsMap;
  vector<branch_entry> *branchLocations;

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
  ByteCodeBuilder* pushJumpLocation(const char* name) {
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
  u64 currentAddress() {
    return bc_index;
  }
public:
  ByteCodeBuilder(VM* vm) {
    this->vm = vm;
    bc_index = 0;
    lit_index = 0;
    bc = (ByteCode *)toObject(make_bytecode(vm, 1024));
    bc_mem = bc->code->data;
    labelsMap = new map<string, u64>;
    branchLocations = new vector<branch_entry>;
  }
  auto dup() {
    pushOp(DUP);
    return this;
  }
  auto pushLit(Ptr literal) {
    bc->literals[lit_index] = literal;
    pushOp(PUSHLIT);
    pushOp(lit_index);
    lit_index++;
    return this;
  }
  auto FFICall(CCallFunction fn) {
    return this
      ->pushLit(make_raw_pointer(vm, (void *)fn))
      ->pushOp(FFI_CALL);
  }
  auto label(const char *name) {
    string key = name;
    (*labelsMap)[key] = currentAddress();
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
  auto call(u64 argc) {
    pushOp(CALL);
    pushU64(argc);
    return this;
  }
  auto call(u64 argc, ByteCode* bc) {
    pushLit(toPtr(bc));
    call(argc);
    return this;
  }
  auto selfcall(u64 argc) {
    pushLit(toPtr(bc));
    call(argc);
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
    if (!isSymbol(sym)) {
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
  auto call(u64 argc, const char *name) {
    loadGlobal(name);
    call(argc);
    return this;
  }
  ByteCode *build() {
    pushOp(END);
    fixupJumpLocations();
    return bc;
  }
};

/* -------------------------------------------------- */

enum VariableScope {
  VariableScope_Global,
  VariableScope_Argument
};

struct VariableInfo {
  VariableScope scope;
  u64 index;
};

struct CompilerEnv {
  CompilerEnv *prev;
  unordered_map<u64, VariableInfo> info;
};

auto compiler_env_info(CompilerEnv *env, Ptr sym) {
  if (!env) return (VariableInfo){ VariableScope_Global, 0 };
  auto existing = env->info.find(sym.value);
  if (existing == env->info.end()) {
    auto outer = compiler_env_info(env->prev, sym);
    if (outer.scope == VariableScope_Argument) {
      cout << "  ERROR: closed over variables not yet implemented: " << sym << endl;
      assert(false);
    }
    return outer;
  }
  return existing->second;
}

void emit_expr(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* env);

void emit_call(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* env) {
  auto fn = car(vm, it);
  auto args = cdr(vm, it);
  auto argc = 0;
  while (!isNil(args)) {
    assert(consp(vm, args));
    argc++;
    emit_expr(vm, builder, car(vm, args), env);
    args = cdr(vm, args);
  }
  emit_expr(vm, builder, fn, env);
  builder->call(argc);
}

void emit_lambda(VM *vm, ByteCodeBuilder *p_builder, Ptr it, CompilerEnv* p_env) {
  CompilerEnv env;
  env.prev = p_env;
  it = cdr(vm, it);
  auto args = car(vm, it);
  u64 idx = 0;
  while (!isNil(args)) {
    auto arg = car(vm, args);
    assert(isSymbol(arg));
    env.info[arg.value] = (VariableInfo){VariableScope_Argument, idx++};
    args = cdr(vm, args);
  }
  auto builder = new ByteCodeBuilder(vm);
  auto body = cdr(vm, it);
  if (isNil(body)) {
    builder->pushLit(NIL);
    return;
  }
  assert(consp(vm, body));
  while(!isNil(body)) {
    auto expr = car(vm, body);
    emit_expr(vm, builder, expr, &env);
    body = cdr(vm, body);
  }
  builder->ret();
  p_builder->pushLit(toPtr(builder->build()));
}

void emit_expr(VM *vm, ByteCodeBuilder *builder, Ptr it, CompilerEnv* env) {
  if (isSymbol(it)) {
    auto info = compiler_env_info(env, it);
    if (info.scope == VariableScope_Global) {
      builder->loadGlobal(it);
    } else if (info.scope == VariableScope_Argument) {
      builder->loadArg(info.index);
    } else {
      assert(false);
    }
  } else if (consp(vm, it)) {
    auto fst = car(vm, it);
    if (isSymbol(fst)) {
      auto quote = intern(vm, "quote");
      auto lambda = intern(vm, "lambda");
      if (ptr_eq(lambda, fst)) {
        emit_lambda(vm, builder, it, env);
        return;
      } else if (ptr_eq(quote, fst)) {
        auto item = car(vm, cdr(vm, it));
        builder->pushLit(item);
        return;
      }
    }
    emit_call(vm, builder, it, env);
  } else {
    builder->pushLit(it);
  }
}

auto compile_toplevel_expression(VM *vm, Ptr it) {
  auto builder = new ByteCodeBuilder(vm);
  emit_expr(vm, builder, it, nullptr);
  return builder->build();
}

/* -------------------------------------------------- */

Ptr print_object(VM *vm) {
  Ptr object = vm_pop(vm);
  cout << object << endl;
  return  object;
}

Ptr decrement_object(VM *vm) {
  Ptr object = vm_pop(vm);
  if (!isFixnum(object)) {
    vm->error = "argument is not a fixnum";
    return object;
  }
  s64 n = toS64(object);
  return toPtr(n - 1);
}

Ptr mul_objects(VM *vm) {
  Ptr objectA = vm_pop(vm);
  if (!isFixnum(objectA)) {
    vm->error = "argument A is not a fixnum";
    return objectA;
  }
  Ptr objectB = vm_pop(vm);
  if (!isFixnum(objectB)) {
    vm->error = "argument B is not a fixnum";
    return objectB;
  }
  s64 a = toS64(objectA);
  s64 b = toS64(objectB);
  return toPtr(a * b);
}

void initialize_global_environment(VM *vm) {

  auto print = (new ByteCodeBuilder(vm))
    ->loadArg(0)
    ->FFICall(&print_object)
    ->ret()
    ->build();

  set_global(vm, "print", toPtr(print));
  
  auto mul = (new ByteCodeBuilder(vm))
    ->loadArg(0)
    ->loadArg(1)
    ->FFICall(&mul_objects)
    ->ret()
    ->build();

  set_global(vm, "mul", toPtr(mul));

}

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
  vm->heap_size_in_bytes = heap_size_in_bytes;

  vm->frame = 0;
  vm->error = 0;

  vm->globals = (Globals *)calloc(sizeof(Globals), 1);
  vm->globals->symtab = new unordered_map<string, Ptr>;
  vm->globals->env = NIL;
  initialize_classes(vm);
  initialize_global_environment(vm);

  // purely for debug printing
  CURRENT_DEBUG_VM = vm;

  auto exprs = read_all(vm, str);

  while (!isNil(exprs)) {
    auto expr = car(vm, exprs);
    auto bc = compile_toplevel_expression(vm, expr);

    vm_push_stack_frame(vm, 0, bc);
    vm->frame->prev_frame = 0;
    vm->frame->argc = 0;

    vm_interp(vm);
  
    if (vm->error) {
      puts("VM ERROR: ");
      puts(vm->error);
      return;
    }
    exprs = cdr(vm, exprs);
  }

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

