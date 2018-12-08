/*

 g++ main.cpp -Werror -std=c++14 && ./a.out
 
(setq flycheck-clang-language-standard "c++14")

*/

#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <tuple>

using namespace std;

typedef unsigned int uint;
typedef uint64_t u64;
typedef int64_t s64;
typedef uint8_t u8;
typedef int8_t s8;

typedef enum {
  Simple,
  RawPointer,
  ByteArray,
  PtrArray,
} ObjectType;

typedef enum {
  String,
  Symbol
} BAOType;

typedef enum {
  Class,
  Cons
} PAOType;

struct Header {
  ObjectType object_type;
  union {
    BAOType bao_type;
    PAOType pao_type;
  };
};
  
struct Object {
  Header header;
};

struct ByteArrayObject : Object {
  uint length;
  char data[];
};

#define EXTRACT_PTR_MASK 0xFFFFFFFFFFFFFFF0
#define TAG_MASK 0b111
#define TAG_BITS 3

typedef enum {
  Fixnum,
  Pointer
} PtrType;

struct Ptr {
  u64 value;
};

struct PtrArrayObject : Object {
  uint length;
  Ptr  data[];
};

struct RawPointerObject : Object {
  void *pointer;
};

bool isFixnum(Ptr self) {
  return (self.value & TAG_MASK) == 0;
}
bool isObject(Ptr self) {
  return (self.value & TAG_MASK) == 0b1;
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

ByteArrayObject *alloc_bao(BAOType ty, uint len) {
  auto byte_count = sizeof(Header) + (sizeof(uint)) + len;
  ByteArrayObject* obj = (ByteArrayObject *)malloc(byte_count);
  obj->header.object_type = ByteArray;
  obj->header.bao_type = ty;
  obj->length = len;
  return obj;
}

void free_bao(ByteArrayObject *obj) {
  free(obj);
}

PtrArrayObject *alloc_pao(PAOType ty, uint len) {
  auto byte_count = sizeof(Header) + (sizeof(uint)) + (len * sizeof(Ptr));
  PtrArrayObject* obj = (PtrArrayObject *)malloc(byte_count);
  obj->header.object_type = ByteArray;
  obj->header.pao_type = ty;
  obj->length = len;
  return obj;
}

void free_pao(PtrArrayObject *obj) {
  free(obj);
}

Ptr make_string(const char* str) {
  ByteArrayObject *obj = alloc_bao(String, strlen(str));
  const char *from = str;
  char *to = &(obj->data[0]);
  while(*from != 0) {
    *to = *from;
    to++; from++;
  }
  return toPtr(obj);
}

Ptr make_symbol(const char* str) {
  ByteArrayObject *obj = alloc_bao(Symbol, strlen(str));
  const char *from = str;
  char *to = &(obj->data[0]);
  while(*from != 0) {
    *to = *from;
    to++; from++;
  }
  return toPtr(obj);
}

Ptr make_number(s64 value) { return toPtr(value); }

Ptr make_raw_pointer(void* ptr) {
  RawPointerObject *obj = (RawPointerObject *)malloc(sizeof RawPointer);
  obj->header.object_type = RawPointer;
  obj->pointer = ptr;
  return toPtr(obj);
}

std::ostream &operator<<(std::ostream &os, Object *obj) { 
  auto otype = obj->header.object_type;
  if (otype == ByteArray) {
    const ByteArrayObject *vobj = (const ByteArrayObject*)(obj);
    switch(vobj->header.bao_type) {
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
  } else if (otype == PtrArray) {
    const PtrArrayObject *vobj = (const PtrArrayObject*)(obj);
    switch(vobj->header.pao_type) {
    case Class:
      return os << "#<A Class>";
    case Cons:
      return os << "#<A Cons>";
    }
  }
  return os << "don't know how to print object.";
}

std::ostream &operator<<(std::ostream &os, Ptr p) { 
  if (isObject(p)) {
    return os << (toObject(p));
  } else if (isFixnum(p)) {
    return os << (toS64(p));
  } else {
    return os << "don't know how to print ptr.";
  }
}

typedef void *(*compiled)();

void *my_arg_grabber() {
  asm(
      "addq %rsi, %rdi\n"
      "movq %rdi, %rax\n"
      "popq %rbp\n"
      "ret\n"
      );

  // to silence the warnings
  return 0;
}

void my_arg_setter(u64 a, u64 b) {
  //  asm("" :: "rsi"(a), "rdi"(b));
}

/* ---------------------------------------- */

struct ByteCode {
  u64 *code;
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


struct VM {
  Ptr *stack;
  Frame *frame;
  u64 *pc;
  ByteCode *bc;
  const char* error;
};

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
  vm->pc = fn->code;
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
  LOAD_ARG = 9
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
  if (top->header.object_type != RawPointer) {
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
    case FFI_CALL:
      vm_ffi_call(vm);
      if (vm->error) return;
      break;
    case BR_IF_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = *(++vm->pc);
      if ((u64)it.value == 0) {
        vm->pc = vm->bc->code + (jump - 1); //-1 to acct for pc advancing
      } 
      break;
    }
    case BR_IF_NOT_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = *(++vm->pc);
      if ((u64)it.value != 0) {
        vm->pc = vm->bc->code + (jump - 1); //-1 to acct for pc advancing
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
      ByteCode *next = (ByteCode *)(*(++vm->pc));
      vm_push_stack_frame(vm, argc, next);
      vm->pc--; // or, could insert a NOOP at start of each fn...
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
    bc->code[bc_index++] = it;
    return this;
  }
  u64* pushEmptyRef() {
    auto location = bc->code + bc_index;
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
  ByteCodeBuilder() {
    bc_index = 0;
    lit_index = 0;
    bc_mem = (u64 *)malloc(1024 * sizeof(u64));
    bc = new ByteCode();
    bc->code = bc_mem;
    labelsMap = new map<string, u64>;
    branchLocations = new vector<branch_entry>;
  }
  ByteCodeBuilder* dup() {
    pushOp(DUP);
    return this;
  }
  ByteCodeBuilder* pushLit(Ptr literal) {
    bc->literals[lit_index] = literal;
    pushOp(PUSHLIT);
    pushOp(lit_index);
    lit_index++;
    return this;
  }
  ByteCodeBuilder* FFICall(CCallFunction fn) {
    return this
      ->pushLit(make_raw_pointer((void *)fn))
      ->pushOp(FFI_CALL);
  }
  ByteCodeBuilder* label(const char *name) {
    string key = name;
    (*labelsMap)[key] = currentAddress();
    return this;
  }
  ByteCodeBuilder* branchIfZero(const char *name) {
    pushOp(BR_IF_ZERO);
    pushJumpLocation(name);
    return this;
  }
  ByteCodeBuilder* branchIfNotZero(const char *name) {
    pushOp(BR_IF_NOT_ZERO);
    pushJumpLocation(name);
    return this;
  }
  ByteCodeBuilder* call(u64 argc, ByteCode* bc) {
    pushOp(CALL);
    pushU64(argc);
    u64 it = (u64) bc;
    pushU64(it);
    return this;
  }
  ByteCodeBuilder* selfcall(u64 argc) {
    pushOp(CALL);
    pushU64(argc);
    pushU64((u64)bc);
    return this;
  }
  ByteCodeBuilder* pop(){
    pushOp(POP);
    return this;
  }
  ByteCodeBuilder* ret() {
    pushOp(RET);
    return this;
  }
  ByteCodeBuilder* loadArg(u64 index) {
    pushOp(LOAD_ARG);
    pushU64(index);
    return this;
  }
  ByteCode *build() {
    pushOp(END);
    fixupJumpLocations();
    return bc;
  }
};

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

void check() {
  VM vm;

  auto count = 1024;
  Ptr *stack_mem = (Ptr *)malloc(count * sizeof(Ptr));
  vm.stack = stack_mem + (count - 1);
  vm.frame = 0;

  auto returnHelloWorld = (new ByteCodeBuilder())
    ->pushLit(make_string("hello, world"))
    ->ret()
    ->build();

  auto dec = (new ByteCodeBuilder())
    ->loadArg(0)
    ->FFICall(&decrement_object)
    ->ret()
    ->build();

  auto print = (new ByteCodeBuilder())
    ->loadArg(0)
    ->FFICall(&print_object)
    ->ret()
    ->build();

  auto mul = (new ByteCodeBuilder())
    ->loadArg(0)
    ->loadArg(1)
    ->FFICall(&mul_objects)
    ->ret()
    ->build();

  auto bc = (new ByteCodeBuilder())
    ->pushLit(make_number(3))
    ->label("loop_start")
    ->call(0, returnHelloWorld)
    ->call(1, print)
    ->pop()
    ->call(1, dec)
    ->dup()
    ->branchIfNotZero("loop_start")
    ->pop()
    ->pushLit(make_number(43))
    ->call(1, dec)
    ->call(1, print)
    ->pushLit(make_number(0))
    ->branchIfZero("exit")
    ->pushLit(make_string("skip me"))
    ->call(1, print)
    ->label("exit")
    ->pushLit(make_number(10))
    ->pushLit(make_number(20))
    ->call(2, mul)
    ->call(1, print)
    ->pushLit(make_string("done!"))
    ->call(1, print)
    ->build();


  vm_push_stack_frame(&vm, 0, bc);
  vm.frame->prev_frame = 0;
  vm.frame->argc = 0;

  vm_interp(&vm);
  
  free(stack_mem);
  if (vm.error) {
    puts(vm.error);
  } else {
    puts("no error");
  }
}




/* ---------------------------------------- */

int main() {
  /*
  cout << make_string("hello, world");
  cout << make_symbol("nil");
  cout << toPtr(42);
  cout << toPtr(-42);
  cout << "\n";
  compiled fn = &my_arg_grabber;
  my_arg_setter(45, -3);
  cout << ((s64)((*fn)()));
  puts("\n");
  */
  check();
  return 0;
}

