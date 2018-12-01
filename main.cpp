/*

 g++ main.cpp -Werror -std=c++11 && ./a.out

*/

#include <iostream>
#include <string>

using namespace std;

typedef unsigned int uint;
typedef uint64_t u64;
typedef int64_t s64;
typedef uint8_t u8;

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

Ptr print_object(Ptr *stack) {
  Ptr object = *stack;
  cout << object << endl;
  return  object;
}

typedef Ptr (*CCallFunction)(Ptr*);

struct VM {
  Ptr *stack;
  const char* error;
};

enum OpCode {
  END = 0,
  RET = 1,
  PUSHLIT = 2,
  POP = 3,
  FFI_CALL = 4,
  CALL0 = 5,
  CALL1 = 6,
  CALL2 = 7,
};

struct ByteCode {
  u8 *bytes;
  Ptr literals[128];
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
  Ptr result = (*fn)(vm->stack);
  vm_push(vm, result);
}


void vm_interp(VM* vm, ByteCode* bc) {
  u8 *curr = bc->bytes;
  u8 instr;
  while ((instr = *curr)) {
    switch (instr){
    case PUSHLIT: {
      u8 idx = *(++curr);
      Ptr it = bc->literals[idx];
      vm_push(vm, it);
      break;
    }
    case FFI_CALL:
      vm_ffi_call(vm);
      if (vm->error) return;
      break;
    default:
      vm->error = "unexpected BC";
      return;
    }
    ++curr;
  }
}

void check() {
  VM vm;
  ByteCode bc;

  Ptr *stack_mem = (Ptr *)malloc(1024 * sizeof(Ptr));
  vm.stack = stack_mem;

  u8* bc_mem = (u8 *)malloc(1024);

  {
    u8 *curr = bc_mem;
#define B(l) *curr++ = l
    B(PUSHLIT); B(0);
    B(PUSHLIT); B(1);
    B(FFI_CALL);
    B(END);
#undef B
  }

  bc.bytes = bc_mem;
  bc.literals[0] = make_string("hello, world");
  bc.literals[1] = make_raw_pointer((void*)&print_object);

  {
    vm_interp(&vm, &bc);
  }
  
  free(stack_mem);
  free(bc_mem);
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

