#include "main.cpp"

void check() {
  VM *vm;
  vm = (VM *)malloc(sizeof(VM));

  auto count = 1024 * 100;
  Ptr *stack_mem = (Ptr *)malloc(count * sizeof(Ptr));
  vm->stack = stack_mem + (count - 1);

  auto heap_size_in_mb = 50;
  auto heap_size_in_bytes = heap_size_in_mb * 1024 * 1024;
  auto heap_mem = malloc(heap_size_in_bytes);
  memset(heap_mem, 0, heap_size_in_bytes);
  vm->heap_mem = heap_mem;
  vm->heap_end = heap_mem;
  vm->heap_size_in_bytes = heap_size_in_bytes;

  vm->frame = 0;

  vm->globals = (Globals *)malloc(sizeof(Globals));
  vm->globals->symtab = new unordered_map<string, Ptr>;
  vm->globals->env = NIL;
  initialize_classes(vm);
  initialize_global_environment(vm);

  CURRENT_DEBUG_VM = vm;

  set_global(vm, "test-symbol", make_string(vm, "test-value"));

  auto returnHelloWorld = (new ByteCodeBuilder(vm))
    ->pushLit(make_string(vm, "hello, world"))
    ->ret()
    ->build();

  auto dec = (new ByteCodeBuilder(vm))
    ->loadArg(0)
    ->FFICall(&decrement_object)
    ->ret()
    ->build();

  auto factorial = (new ByteCodeBuilder(vm))
    ->loadArg(0)
    ->branchIfZero("return1")
    ->loadArg(0)
    ->call(1, dec)
    ->dup()
    ->branchIfZero("return1")
    ->selfcall(1)
    ->loadArg(0)
    ->call(2, "mul")
    ->ret()
    ->label("return1")
    ->pushLit(make_number(1))
    ->ret()
    ->build();

  auto tests = (new ByteCodeBuilder(vm))
    ->pushLit(make_number(3))
    ->label("loop_start")
    ->call(0, returnHelloWorld)
    ->call(1, "print")
    ->pop()
    ->call(1, dec)
    ->dup()
    ->branchIfNotZero("loop_start")
    ->pop()
    ->pushLit(make_number(43))
    ->call(1, dec)
    ->call(1, "print")
    ->pushLit(make_number(0))
    ->branchIfZero("exit")
    ->pushLit(make_string(vm, "skip me"))
    ->call(1, "print")
    ->label("exit")
    ->pushLit(make_number(10))
    ->pushLit(make_number(20))
    ->call(2, "mul")
    ->call(1, "print")
    ->pushLit(make_number(10))
    ->call(1, factorial)
    ->call(1, "print")
    ->loadGlobal("test-symbol")
    ->call(1, "print")
    ->pushLit(make_string(vm, "done!"))
    ->call(1, "print")
    ->ret()
    ->build();

  set_global(vm, "tests", toPtr(tests));

  assert(ptr_eq(intern(vm, "nil"), intern(vm, "nil")));
  cout << " nil is: " << intern(vm, "nil") << endl;
  assert(ptr_eq(intern(vm, "nil"),
                read(vm, " nil ")));
  assert(ptr_eq(intern(vm, "nil"),
                read(vm, " nil")));

  {
    const char *input = "(hello world)";
    cout << read(vm, input) << endl;
    cout << read(vm, input) << endl;
  }

  {
    const char *input = "(lambda (x) x)";
    cout << read(vm, input) << endl;
  }

  {
    cout << read(vm, "5") << endl;
    cout << read(vm, "15") << endl;
    cout << read(vm, "150") << endl;
    cout << read(vm, "(1 2 3 4 5 a b c d e)") << endl;
  }

  // auto test_expr = read(vm, "(print (mul 255 10))");
  // auto test_expr = read(vm, "(print ((lambda (x) x) 5))");
  // auto test_expr = read(vm, "(print ((lambda (x y) (mul x y)) 5 4))");
  auto test_expr = read(vm, "(print ((lambda (x y) y) 5 4))");
  auto test_bc   = compile_toplevel_expression(vm, test_expr);

  vm_push_stack_frame(vm, 0, test_bc);
  vm->frame->prev_frame = 0;
  vm->frame->argc = 0;

  vm_interp(vm);
  
  if (vm->error) {
    puts(vm->error);
  } else {
    puts("no error");
  }

  free(stack_mem);
}

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
  check();
  // run_string("(print ((lambda (x y) y) 5 4)) (print 5)");

*/
