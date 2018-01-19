This documents potentially non-obvious behavior (or desired behavior in desired implementation) of calypso.

## A.init != A()

```
struct Inner{
  Inner(){}
  ~Inner(){}
};

// test.h:
struct A{
  int x=42;
  float y=1.5;
  double z;
  int* u;
  Inner inner;
  A(){
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
  enum a1=A.init; // // no ctor called, just A.init which is known at compile time
  auto a2=A.init; // ditto
  A a3=A.init; // ditto: `auto a=expr;` is identical to `A a=expr;` when typeof(expr)=A

  static assert(A.init.x==42); // not 0
  static assert(A.init.y==1.5); // not float.NaN
  static assert(A.init.z==double.NaN);  // because no initializer in `A.z`
  static assert(A.init.u==0);
  
  auto a4=A(); // calls ctor A::A()
  assert(a4.x==42+1);
  assert(a4.u);
  
  A* a5=new A(); // calls ctor A::A() (allocates using GC)

  A a6=void; // uninitialized (to whatever garbage on the stack)
  
  // likewise with B that embeds A:
  enum b1=B.init; // no ctor called
  auto b2=B();  // calls B's ctor, hence also A::A()

  A a7; // Should be identical to `A a7=A();`
  /+
  This one is controversial but probably the least worst option.
  If `A a7;` meant `A a7=A.init;`, it would give rise to surprising behavior, different from what one would expect using C++ type A; the constructor would not be called but the destructor A::~A() would be, leading to potential memory corruption if `~A()` deallocates things allocated in `A::A()`.
  
  The drawback of that is that existing D code makes no difference between `D d=D.init; D d=D(); D d;` ; so if we use existing D code with a C++ type `A` it could lead to different behavior depending on which variant the code uses. It could also lead to compile errors if an expression is expected to be known at CT, depending on which variant is used.
  +/
}
```

## memory allocation: D vs C++ new, D vs C++ delete, D destroy
```
A a0=A.init; // allocates on the stack using compile time value A.init;
A a1=A1(); // allocates on the stack using A::A()
A* a1=new A(); // allocate on D GC (GC's responsability to delete)
A* a2=A.new(); // allocate on heap using C++::new (leaks without A.delete())
// delete a1;// delete was deprecated in standard D
a1.destroy; // no suprise, calls A::~A() as it would with `struct D{~this(){}} D d; d.destroy;` but doesn't deallocate A.sizeof bytes holding A itself

a2.delete(); // calls C++::delete(a2) and sets a2 to null.
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

## when is C++ move assignment and move constructor used
TODO

## traits
```
static assert(__traits(is_cxx_struct, A));  // doesn't care whether A is a C++ struct or class (almost same)
static assert(__traits(is_dcxx_struct, B)); // for D struct embedding a is_cxx_struct or is_dcxx_struct struct
static assert(__traits(is_dcxx_class, C)); // for D class inheriting a is_cxx_struct or is_dcxx_struct or is_dcxx_class
```

