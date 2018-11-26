/*

 g++ main.cpp -Werror -std=c++11 && ./a.out

*/

#include <iostream>
#include <string>

using namespace std;

typedef unsigned int uint;
typedef uint64_t u64;
typedef int64_t s64;

typedef enum {
  Simple,
  ByteArray,
  PtrArray
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
  ByteArrayObject* obj = (ByteArrayObject *)malloc(sizeof(Header) + (sizeof(uint)) + len);
  obj->header.object_type = ByteArray;
  obj->header.bao_type = ty;
  obj->length = len;
  return obj;
}

void free_bao(ByteArrayObject *obj) {
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


int main() {
  // cout << make_string("hello, world");
  // cout << make_symbol("nil");
  // cout << toPtr(42);
  cout << toPtr(-42);
  cout << "\n";
  return 0;
}

