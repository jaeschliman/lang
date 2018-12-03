/*

 g++ main.cpp -Werror -std=c++11 && ./a.out

*/

#include <iostream>
#include <string>
#include <map>

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

struct Frame {
  Ptr* prev_stack;
  Frame* prev_frame;
  u64 argc;
  Ptr argv[];
};


struct VM {
  Ptr *stack;
  Frame *frame;
  const char* error;
};

void vm_pop_stack_frame(VM* vm) {
  auto nf = vm->frame->prev_frame;
  if (!nf) {
    vm->error = "nowhere to return to";
    return;
  }
  vm->stack = nf->prev_stack;
  vm->frame = nf->prev_frame;
}

void vm_push_stack_frame(VM* vm, u64 argc) {
  u64 *top = &vm->stack->value;
  uint offset = (sizeof(Frame) / sizeof(u64));
  top -= offset;
  Frame *new_frame = (Frame *)top;
  new_frame->prev_frame = vm->frame;
  new_frame->prev_stack = vm->stack;
  new_frame->argc = argc;
  vm->stack = (Ptr*)(void *)new_frame;
  vm->frame = new_frame;
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
  DUP = 7
};

struct ByteCode {
  u64 *code;
  Ptr literals[1024];
};

void vm_push(VM* vm, Ptr value) {
  *(++vm->stack) = value;
}

Ptr vm_pop(VM* vm) {
  return *(vm->stack--);
}

void vm_ffi_call(VM* vm) {
  Ptr ptr = vm_pop(vm);
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
  vm_push(vm, result);
}


void vm_interp(VM* vm, ByteCode* bc) {
  u64 *curr = bc->code;
  u64 instr;
  while ((instr = *curr)) {
    switch (instr){
    case POP:
      vm_pop(vm);
      break;
    case PUSHLIT: {
      u64 idx = *(++curr);
      Ptr it = bc->literals[idx];
      vm_push(vm, it);
      break;
    }
    case FFI_CALL:
      vm_ffi_call(vm);
      if (vm->error) return;
      break;
    case BR_IF_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = *(++curr);
      if ((u64)it.value == 0) {
        curr = bc->code + (jump - 1); //-1 to acct for pc advancing
      } 
      break;
    }
    case BR_IF_NOT_ZERO: {
      auto it = vm_pop(vm);
      u64 jump = *(++curr);
      if ((u64)it.value != 0) {
        curr = bc->code + (jump - 1); //-1 to acct for pc advancing
      } 
      break;
    }
    case DUP: {
      auto it = vm_pop(vm);
      vm_push(vm, it);
      vm_push(vm, it);
      break;
    }
    default:
      vm->error = "unexpected BC";
      return;
    }
    if (vm->error) return;
    ++curr;
  }
}


class ByteCodeBuilder {
private:
  u64* bc_mem;
  u64 bc_index;
  u64 lit_index;
  ByteCode *bc;
  map<string, u64> *labelsMap;
  ByteCodeBuilder* pushOp(u8 op) {
    return pushU64(op);
  }
  ByteCodeBuilder* pushU64(u64 it) {
    bc->code[bc_index++] = it;
    return this;
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
    string key = name;
    auto addr = (*labelsMap)[key];
    pushOp(BR_IF_ZERO);
    pushU64(addr);
    return this;
  }
  ByteCodeBuilder* branchIfNotZero(const char *name) {
    string key = name;
    auto addr = (*labelsMap)[key];
    pushOp(BR_IF_NOT_ZERO);
    pushU64(addr);
    return this;
  }
  ByteCodeBuilder* pop(){
    pushOp(POP);
    return this;
  }
  ByteCode *build() {
    pushOp(END);
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

void check() {
  VM vm;

  Ptr *stack_mem = (Ptr *)malloc(1024 * sizeof(Ptr));
  vm.stack = stack_mem;
  vm.frame = 0;

  vm_push_stack_frame(&vm, 0);

  auto bc = (new ByteCodeBuilder())
    ->pushLit(make_number(3))
    ->label("loop_start")
    ->pushLit(make_string("hello, world"))
    ->FFICall(&print_object)
    ->pop()
    ->FFICall(&decrement_object)
    // ->FFICall(&print_object)
    ->dup()
    ->branchIfNotZero("loop_start")
    ->pop()
    ->build();

  vm_interp(&vm, bc);
  
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

