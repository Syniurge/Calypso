This documents potentially non-obvious behavior (or desired behavior in desired implementation) of calypso.

## semantics for A.init, A()

```
// test.h:
struct A{
  int x=42;
  float y=1.5;
  double z;
  A(){
    static int counter=0;
    // can call things at runtime
    x++;
  }
};

// main.d:
import(C++) _.A;


struct B{
  A a;
}

void main(){
  A a1; // no ctor called, just A.init which is known at compile time
  auto a2=A.init; // ditto
  enum a3=A.init // ditto
  
  static assert(A.init.x==42); // not 0
  static assert(A.init.y==1.5); // not float.NaN
  static assert(A.init.z==double.NaN);  // because no initializer in `A.z`
  
  // `auto a=expr;` is identical to `A a=expr;` when typeof(expr)=A
  
  auto a4=A(); // calls ctor A::A()
  assert(a4.x==42+1);

  A a5=void; // uninitialized (to whatever garbage on the stack)
  
  // likewise with DCXX B:
  B b1; // no ctor called, same with B.init
  auto b2=B();  // calls B's ctor, which in turn calls A::A()
}
```
