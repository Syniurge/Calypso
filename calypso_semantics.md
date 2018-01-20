This documents potentially non-obvious behavior (or desired behavior in desired implementation) of calypso.

## A.init != A()

```C++
// fun.h
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
```

```d
// main.d:
import(C++) _.A;

struct B{
  A a;
}

class C : A{
}

/+
NOTE: in C++:
A* aa=new(buf)A(); // calls Inner::Inner() then A::A(...); let's call the whole thing A.__ctor
a->~A(); // calls A::~A() then Inner::~Inner(); let's call the whole thing A.__dtor

TODO: CHECK whether it should be __dtor or __xdtor
+/

void main(){
  enum a1=A.init; // // A.__ctor not called (nor copy ctor or move ctor), just A.init which is known at compile time
  auto a2=A.init; // ditto
  A a3=A.init; // ditto: `auto a=expr;` is identical to `A a=expr;` when typeof(expr)=A; in particular, should be callable at CTFE.

  static assert(A.init.x==42); // not 0
  static assert(A.init.y==1.5); // not float.NaN
  static assert(A.init.z==double.NaN);  // because no initializer in `A.z`
  static assert(A.init.u==0);
  
  auto a4=A(); // calls A.__ctor
  assert(a4.x==42+1);
  assert(a4.u);
  
  A* a5=new A(); // allocates A.sizeof using GC then calls a5.__ctor()

  A a6=void; // uninitialized stack allocation (to whatever garbage on the stack)
  
  // likewise with B that embeds A:
  enum b1=B.init; // no __ctor called
  auto b2=B();  // calls B.__ctor which calls A.__ctor

  A a7; // Should be identical to `A a7=A();`

  /+
  NOTE:
  This one is controversial but probably the least worst option.
  If `A a7;` meant `A a7=A.init;`, it would give rise to surprising behavior, different from what one would expect using C++ type A; A.__ctor would not be called but the destructor A.__dtor would be, leading to potential memory corruption if `A.__dtor` deallocates things allocated in `A.__ctor`.
  
  The drawback of that is that existing D code makes no difference between `D d=D.init; D d=D(); D d;` ; so if we use existing D code with a C++ type `A` it could lead to different behavior depending on which variant the code uses. It could also lead to compile errors if an expression is expected to be known at CT, depending on which variant is used.
  +/
}
```

## memory allocation: D vs C++ new, D vs C++ delete, D destroy, C++ new/delete vs malloc/free
```d
A a0=A.init; // allocates on the stack using compile time value A.init;
A a1=A1(); // allocates on the stack then calls A.__ctor
A* a2=new A(); // allocate A.sizeof on D GC (and calls a2.__ctor); GC will call a2.__dtor and deallocate A.sizeof (if/when collection happens) so nothing to do in user code

// defined in cpp.memory; we need both ways to allow both ways to interface with C++ libraries that expect allocation was done with malloc vs new
A* a3=cppNew!A(); // allocate on heap using C++::new (leaks without cppDelete(a3))
A* a4=cppMalloc!A();  // ditto (also calls A.__ctor) but allocation part is done with malloc; (leaks without cppFree(a3))

a1.destroy; // calls A.__dtor as it would with `struct D{~this(){}}; D d; d.destroy;` and memcpy A.init in a1; NOTE: when a1 goes out of scope, it'll call `A.__dtor` again, potentially causing memory corruption (but same situation with standard D structs today)

// TODO: how to force deallocation on a2? `destroy(*a2)` could lead to doubly calling A.__dtor when GC collects a2
a3.delete(); // calls C++ `delete a3;` and sets `a3 = null;`

// for destroy on struct pointer, not yet clear, see http://forum.dlang.org/thread/uplymqtaxubgkxwzacrz@forum.dlang.org Calling destroy on struct pointer; should be consistent with how D treats struct pointers
```

## question: is it a problem that A.__dtor gets called on a stack allocated C++ struct intialized via A.init without A.__ctor? could we use a special value (eg A.init or 0) to check whether we can skip calling A.__dtor?
The problem doesn't appear (easily) in C++ code because there's no (non-contrived) equivalent of `A a=A.init;`
```d
void main(){
  A a=A.init; // A.__ctor not called
  // upon exiting scope, should A.__dtor get called? NOTE: for D structs, it gets called.
}
```

### Option 1: `A.__dtor` gets called
* advantage:
consistent with D behavior for structs
in typical cases, calling __dtor on a struct that was initialized to compile time value `A.init` should be harmless (eg, pointers would typically be 0. In this case, calling `destroy` repeatedly should also be harmless.

* disadvantage:
could cause memory corruption in corner cases, eg:
```
int x_global;
struct A {
    // either of these cause issues
    int* x0=(int*)0xbaadf00d;
    int* x1=&x_global;

    A(){
        x = new int;
    }

    ~A() {
        delete x; // UB if `A()` wasn't called
    }
};
```

NOTE: if we make `A.init` be 0, this problem goes away, but that's not consistent with `.init` for D structs; this is a bigger even problem for a D struct that embeds `A`, eg: `struct B2{A a; int a=1;}` ; making `B.init is 0` true is surprising.

NOTE: another option is to force `A.init` (and embeddings D structs) to have pointers set to non-zero values, eg in these case, `static assert(A.init.x0 is 0)`

### Option 2: `A.__dtor` doesn't get called if `a is A.init` (via `memcmp`)
advantage:
avoids calling `__dtor` if no `__ctor` was called (these are matching 1:1 except by "accident" when `A.__ctor` was called but is equal to `A.init` during deallocation time
allows calling `destroy(a)` multiple times safely (as in D), where a.destroy would mean: `memcp(&a, A.init)`

disadvantage:
not consistent with D behavior for structs
extra call to `memcmp` (only in case of stack deallocation or in more cases?)
could that skip legitimate calls to `A.__dtor` if `a` just happens to be `A.init` by "accident" ?

## when is C++ move assignment and move constructor used
should behave the same as in C++:
When lvalue reference is cast in rvalue reference with std::move or other.

// TODO: show examples to cover most cases; specify which calls can be optimized out depending on optimization levels

## traits
```d
static assert(__traits(is_cxx_struct, A));  // doesn't care whether A is a C++ struct or class (almost same)
static assert(__traits(is_dcxx_struct, B)); // for D struct embedding a is_cxx_struct or is_dcxx_struct struct
static assert(__traits(is_dcxx_class, C)); // for D class inheriting a is_cxx_struct or is_dcxx_struct or is_dcxx_class
```

