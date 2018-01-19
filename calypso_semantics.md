## semantics for A.init, A()

```
// test.h:
struct A{
  A(){}
};

// main.d:
import(C++) _.A;


struct B{
  A a;
}

void main(){
  A a1; // no ctor called, just A.init
  auto a2=A.init; // ditto
  enum a3=A.init // ditto
  
  // `auto a=expr;` is identical to `A a=expr;` when typeof(expr)=A
  
  auto a4=A(); // calls ctor A::A()
  A a5=void; // uninitialized (to whatever garbage on the stack)
  
  // likewise with DCXX B:
  B b1; // no ctor called, same with B.init
  auto b2=B();  // calls B's ctor, which in turn calls A::A()
}
```
