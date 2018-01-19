This documents potentially non-obvious behavior (or desired behavior in desired implementation) of calypso.

## A.init != A()

```
// test.h:
struct A{
  int x=42;
  float y=1.5;
  double z;
  int* u;
  A(){
    static int counter=0;
    // can call things at runtime
    x++;
    u=new int(10);
  }
  ~A(){
    delete u;
  }
};

// main.d:
import(C++) _.A;

struct B{
  A a;
}

class C : A{
}

void main(){
  A a1; // no ctor called, just A.init which is known at compile time
  auto a2=A.init; // ditto
  enum a3=A.init // ditto
  
  static assert(A.init.x==42); // not 0
  static assert(A.init.y==1.5); // not float.NaN
  static assert(A.init.z==double.NaN);  // because no initializer in `A.z`
  static assert(A.init.u==0);
  
  // `auto a=expr;` is identical to `A a=expr;` when typeof(expr)=A
  
  auto a4=A(); // calls ctor A::A()
  assert(a4.x==42+1);
  
  A* a5=new A(); // calls ctor A::A() (allocates using GC)

  A a6=void; // uninitialized (to whatever garbage on the stack)
  
  // likewise with DCXX B:
  B b1; // no ctor called, same with B.init
  auto b2=B();  // calls B's ctor, which in turn calls A::A()
  
  // 
}
```

## memory allocation: D vs C++ new, D vs C++ delete, D destroy
```
A a0; // allocates on the stack using compile time value A.init;
A a1=A1(); // allocates on the stack using A::A()
A* a1=new A(); // allocate on D GC (GC's responsability to delete)
A* a2=A.new(); // allocate on heap using C++::new (leaks without A.delete())
// delete a1;// delete was deprecated in standard D
a1.destroy; // no suprise, calls A::~A() as it would with `struct D{~this(){}} D d; d.destroy;`

a2.delete(); // 
a2.destroy; // TODO
```

## tricky point: A::~A() called without A::A() called
It matches D's behavior for structs (which cannot have parameterless `this()` ctor).
Is that the best behavior for C++ structs?
```
void main(){
  A a; // A::A() not called
  // upon exiting scope, A::~A() is called
}
```

## traits
```
static assert(__traits(is_cxx_struct, A));  // doesn't care whether A is a C++ struct or class (almost same)
static assert(__traits(is_dcxx_struct, B)); // for D struct embedding a is_cxx_struct or is_dcxx_struct struct
static assert(__traits(is_dcxx_class, C)); // for D class inheriting a is_cxx_struct or is_dcxx_struct or is_dcxx_class
```

