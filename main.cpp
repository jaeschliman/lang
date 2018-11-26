#include <iostream>
#include <string>

using namespace std;

typedef unsigned int uint;
typedef uint64_t u64;
typedef int64_t s64;

typedef enum {
  Simple,
  VariableLength,
} ObjectType;

typedef enum {
  Class,
  Cons,
  String,
  Symbol
} VLType;

struct Header {
  ObjectType object_type;
  VLType type;
};
  
struct Object {
  Header header;
};

struct VLObject : Object {
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

bool isFixnum(Ptr self) {
  return (self.value & TAG_MASK) == 0;
}
bool isReference(Ptr self) {
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


VLObject *alloc_vlo(VLType ty, uint len) {
  VLObject* obj = (VLObject *)malloc(sizeof(Header) + (sizeof(uint)) + len);
  obj->header.object_type = VariableLength;
  obj->header.type = ty;
  obj->length = len;
  return obj;
}

void free_vlo(VLObject *obj) {
  free(obj);
}

Ptr make_string(const char* str) {
  VLObject *obj = alloc_vlo(String, strlen(str));
  const char *from = str;
  char *to = &(obj->data[0]);
  while(*from != 0) {
    *to = *from;
    to++; from++;
  }
  return toPtr(obj);
}

Ptr make_symbol(const char* str) {
  VLObject *obj = alloc_vlo(Symbol, strlen(str));
  const char *from = str;
  char *to = &(obj->data[0]);
  while(*from != 0) {
    *to = *from;
    to++; from++;
  }
  return toPtr(obj);
}

std::ostream &operator<<(std::ostream &os, Object *obj) { 
  if (obj->header.object_type == VariableLength) {
    const VLObject *vobj = (const VLObject*)(obj);
    switch(vobj->header.type) {
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
    case Class:
      return os << "#<A Class>";
    case Cons:
      return os << "#<A Cons>";
    }
  }
  return os << "don't know how to print object.";
}

std::ostream &operator<<(std::ostream &os, Ptr p) { 
  if (isReference(p)) {
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

