# Implementation Notes

<a name="1"></a>
## 1. Overview

The implementation of this Lisp is largely based on that of L2Lisp, 
[ver. 7.2 (l2lisp-in-python)](https://github.com/nukata/l2lisp-in-python) and later.
It features:

 - A sort of subset of Emacs Lisp, but being Lisp-1 with lexical scoping
 - Very few built-ins -- even defun is defined in the `prelude` as follows:
   ```Lisp
   (defmacro defun (name args &rest body)
     `(progn (setq ,name (lambda ,args ,@body))
             ',name))
   ```
 - Tail call optimization, which also implies tail recursion optimization ([§4](#4))
 - Automatic avoidance of free symbol capture in macro expansion ([§5](#5))


<a name="2"></a>
## 2. Inner Representations of Lisp Expressions


The table below summarizes the inner representations of expressions to be read by the Lisp interpreter.
Bulit-in types of Dart also represent their values in Lisp if possible.

The `Sym` class is, however, user-defined to represent Lisp symbols because the bulit-in `Symbol`
type of Dart represents only identifiers that are allowed by the Dart language.
Some symbols such as *lambda* and *cond* are represented by `Keyword` 
in order to be treated specially as expression keywords.
A list of Lisp is represented as a linked list of instances of the `Cell` class,
which is defined to represent *cons* cells.
An instance of `Cell` consists of two members, named `car` and `cdr` respectively by tradition.
These classes are discussed in the next section ([§3](#3)).


|  Lisp Expression                 | Inner Representation               |
|:---------------------------------|:-----------------------------------|
| numbers *1*, *2.3*               | `num` (`int` & `double`)           |
| strings *"abc"*, *"hello!\n"*    | `String`                           |
| *t*                              | `true`                             |
| *nil*                            | `null`                             |
| symbols *x*, *+*                 | `Sym` (user-defined)               |
| keywords *lambda*, *cond*        | `Keyword` (extends `Sym`)          |
| lists *(x 1 "2")*, *(y . 3)*     | `Cell` (user-defined)              |


The table below summarizes the inner representations of evaluated lambda expressions and other functions.
All of them are instances of user-defined classes.
Note that they are **not** the results of **applications** of such expressions.

A lambda expression is a list represented by `Cell` when it is read by the interpreter.
When it is evaluated as a Lisp expression, an instance of `Closure` will be constructed as the result.
In the construction of `Closure`, each lambda expression nested in the body of the lambda expression
will be replaced by an instance of `Lambda`;
each occurrence of formal parameter there will be replaced by an instance of `Arg`;
and each macro and each quasi-quotation there will be expanded.
In short, it is **compiled** to resolve the parameters, macros, etc. which occur in its body.


|  Lisp Expression                                 | Compiled Inner Representation               |
|:-------------------------------------------------|:--------------------------------------------|
| lambda expression *(lambda (...) ...)*           | `Closure` (has `Cell` as its environment)   |
| nested lambda expresions *(lambda (...) ...)*    | `Lambda` (has no environment)               |
| macro expressions *(macro (...) ...)*            | `Macro` (has no environment)                |
| built-in functions *car*, *cons*, *+*, ...       | `BuiltInFunc`                               |


You can see string representations of instances of these classes in your interactive Lisp session as follows.
When you define a function with *(defun ...)* in Lisp, 
the function body will be compiled to an instance of `Closure`.
In a similar way, when you define a macro with *(defmacro ...)*, 
the macro body will be compiled to an instance of `Macro`.

```
> (lambda (x) (+ x 1))
#<closure:1:nil:((+ #0:0:x 1))>
> (lambda (x) (lambda (y) (+ x y)))
#<closure:1:nil:(#<lambda:1:((+ #1:0:x #0:0:y))>)>
> (macro (x) `(+ ,x 1))
#<macro:1:((list '+ #0:0:x 1))>
> car
#<car:1>
> equal
#<closure:2:nil:((cond ((atom #0:0:x) (eql #0:0:x #0:1:y)) ((atom #0:1:y) nil) (
(equal (car #0:0:x) (car #0:1:y)) (equal (cdr #0:0:x) (cdr #0:1:y)))))>
> if
#<macro:-3:((cons 'cond (cons (list #0:0:test #0:1:then) (cond (#0:2:else (list
(cons t #0:2:else)))))))>
>
```


Look into the bodies of the compiled lambda expressions in the above example and you see every formal
parameter replaced with some `Arg` instance of the string representation such as *#0:0:x*.
Each `Arg` instance holds a static nesting level of the corresponding function call and an offset value
within the call-frame in order to implement static scoping.
For example, an `Arg` instance of the string representation *#1:0:x* represents a formal parameter of 
nesting level 1, offset 0, and symbolic name *x*.

A call-frame of a Lisp function is implemented with a list (an instance of the built-in `List` type of Dart) 
which contains the actual arguments.
An environment is implemented with a Lisp list (a instance of user-defined `Cell`) which contains call-frames.

------------------------------

**Note:**
Most parameters are likely to be at level 0 and then refer to the head of the environment, i.e. a *car* of 
a Lisp list, as their call-frame.
And each element of the frame can be accessed in constant time since the frame is a `List` of Dart.
After all, given an environment, most parameters are likely to be accessed in constant time.

------------------------------

Apart from the environments described above, the global environment which holds functions and macros defined 
globally, for example, *car*, *cons*, *+*, *let*, and *if*, is implemented as the `globals` member of
`Map<Sym, Object>` type within the `Interp` class which represents the core of the interpreter.
The table below summarizes these three kinds of entities.


| Component of Lambda Expression         | Compiled Inner Representation                         |
|:---------------------------------------|:------------------------------------------------------|
| a formal parameter in the body         | `Arg` (has a nesting level and an offset)             |
| an environment                         | `Cell` (has `List`s of actual arguments)              |
| the global environment                 | `Interp`'s `globals` member (is a `Map<Sym, Object>`) |



Below is the definition of the `Arg` class.
The methods `setValue` and `getValue` of `Arg` provide the relation 
between formal parameters occurring in the body  and the environment.

```Dart
/// Bound variable in a compiled lambda/macro expression
class Arg {
  final int level;
  final int offset;
  final Sym symbol;

  Arg(this.level, this.offset, this.symbol);
  @override String toString() => "#$level:$offset:$symbol";

  /// Set a value x to the location corresponding to the variable in env.
  void setValue(x, Cell env) {
    for (int i = 0; i < level; i++)
      env = env.cdr;
    env.car[offset] = x;
  }

  /// Get a value from the location corresponding to the variable in env.
  getValue(Cell env) {
    for (int i = 0; i < level; i++)
      env = env.cdr;
    return env.car[offset];
  }
}
```

------------------------------

**Note:**
So far I have sketched the data structure of the Lisp interpreter.
Since the algorithm for the interpreter would be determined roughly by the specification of 
the Lisp language, the above might suffice to build a skeleton of the interpreter.
The follwing three sections describing the details of the interpreter are supplements to it, in a sense.

------------------------------


<a name="3"></a>
## 3. Lists, Symbols and Built-in Functions

The following is the definition of the `Cell` class, which represents lists of Lisp internally.
The method `toString` is intended only for quick debugging.
The Lisp interpreter uses the `str` function, which is not shown here, to display lists as its output.

```Dart
/// Cons cell
class Cell {
  var car;
  var cdr;

  Cell(this.car, this.cdr);
  @override String toString() => "($car . $cdr)";

  /// Length as a list
  int get length => foldl(0, this, (i, e) => i + 1);
}
```


The following is the definition of the `foldl` function, which is used in the `length` 
property to calculate the length of any list.

```Dart
/// foldl(x, (a b c), fn) => fn(fn(fn(x, a), b), c)
foldl(x, Cell j, fn(y, z)) {
  while (j != null) {
    x = fn(x, j.car);
    j = j.cdr;
  }
  return x;
}
```


------------------------------

**Note:**
`foldl` was named after the function of the same name in Haskell.
In order to allow `foldl` to be applied to `null`, which means *nil* in Lisp, it is not defined as a method of `Cell`.

------------------------------


The reason why `length` is defined as a member of `Cell` specifically is 
to define the built-in *length* function of Lisp

```
> (length "あいうえお")
5
> (length '(a i u e o))
5
>
```


in the way to make use of duck typing which Dart has as a scripting language:

```Dart
    def("length", 1, (List a) => (a[0] == null) ? 0 : a[0].length);
```

------------------------------

**Note:**
This is not a thing to be highly evaluated.
Orthodoxly speaking, branching on type should be preferred.
But it is charm of Dart that it can be loose in this way if favored.

------------------------------


where `def` is a method of the `Interp` class defined as follows;
it constructs a Lisp symbol from the string parameter `name` and stores the `BuiltInFunc` instance 
to the `globals` member of the `Interp` class, using the constructed symbol as the key to the instance:

```Dart
  /// Define a built-in function by giving a name, an arity, and a body.
  void def(String name, int carity, BuiltInFuncBody body) {
    globals[new Sym(name)] = new BuiltInFunc(name, carity, body);
  }
```

------------------------------

**Note:**
The second argument of the `def` medthod, `carity`, is a value that is made up from the arity of the function 
and whether the function has variable length arguments, i.e., *&rest* args.
"Carity" is my coinage; I have not decided yet what "c" of c + arity means:  combined, composite, or calculated.
It counts 1 as an arity for whole *&rest* args.
Then, if the function has &amp;rest args, the sign of the arity number is changed.

For example, the subtraction function `-` has carity `-2`, i.e., the length of the arguments is variable 
and the function takes 1 argument at least when no *&rest* args.
The *&rest* args are passed as a `Cell` list.

```Dart
    def("-", -2, (List a) {
      var x = a[0];
      Cell y = a[1];
      return (y == null) ? -x : foldl(x, y, (i, j) => i - j);
    });
```

It can be used as follows:

```
> (- 7)
-7
> (- 7 8 9)
-10
>
``` 

------------------------------


The following shows the definitions of some major built-in functions:

```Dart
    def("car", 1, (List a) => (a[0] as Cell)?.car);
    def("cdr", 1, (List a) => (a[0] as Cell)?.cdr);
    def("cons", 2, (List a) => new Cell(a[0], a[1]));
    def("atom", 1, (List a) => (a[0] is Cell) ? null : true);
    def("eq", 2, (List a) => identical(a[0], a[1]) ? true : null);
```


The following is the definition of the `Sym` class, which represents Lisp symbols internally.
The `Sym` constructor used in `new Sym(name)` in the above is a factory in fact.
It stores the constructed symbol to the internal `table` in order to implement the *intern* operation of Lisp.

```Dart
/// Lisp symbol
class Sym {
  final String name;

  /// Construct a symbol that is not interned.
  Sym.internal(this.name);

  @override String toString() => name;
  // @override int get hashCode => name.hashCode; // Key to Speed for old Dart

  /// The table of interned symbols
  static final Map<String, Sym> table = {};

  /// Construct an interned symbol; construct a Keyword if isKeyword holds.
  factory Sym(String name, [bool isKeyword=false]) {
    var result = table[name];
    assert(result == null || ! isKeyword);
    if (result == null) {
      result = isKeyword ? new Keyword.internal(name) : new Sym.internal(name);
      table[name] = result;
    }
    return result;
  }

  /// Is it interned?
  bool get isInterned => identical(this, table[name]);
}
```



The `Keyword` class, which represents keywords such as 
*lambda*, *quasiquote*, *quote*, and *setq*, is derived from the `Sym` class.
As you see from the definition of constructor of `Sym` above and that of `Keyword`
below, once you construct a symbol with  `new Keyword("lambda")` first,
you will get the same `Keyword` instance even if you call  `new Symbol("lambda")` later.
Therefore, it is not necessary to look up any keyword table in the lexical analysis;
this makes the operation to *read* Lisp expressions simple.

```Dart
/// Expression keyword
class Keyword extends Sym {
  Keyword.internal(String name): super.internal(name);
  factory Keyword(String name) => new Sym(name, true);
}
```


<a name="4"></a>
## 4. Expression Evaluation and Tail Call Optimization


The `eval` method of the `Interp` class evaluates a Lisp expression in a similar way 
with the method of the same name in
[L2Lisp ver. 9 (l2lisp-in-java)](https://github.com/nukata/l2lisp-in-java).

```Dart
  /// Evaluate a Lisp expression in an environment.
  eval(x, Cell env) {
    try {
      for (;;) {
        if (x is Arg) {
          return x.getValue(env);
        } else if (x is Sym) {
          if (globals.containsKey(x))
            return globals[x];
          throw new EvalException("void variable", x);
        } else if (x is Cell) {
          var fn = x.car;
          Cell arg = cdrCell(x);
          if (fn is Keyword) {
            if (fn == quoteSym) {
              if (arg != null && arg.cdr == null)
                return arg.car;
              throw new EvalException("bad quote", x);
            } else if (fn == prognSym) {
              x = _evalProgN(arg, env);
            } else if (fn == condSym) {
              x = _evalCond(arg, env);
            } else if (fn == setqSym) {
              return _evalSetQ(arg, env);
            } else if (fn == lambdaSym) {
              return _compile(arg, env, Closure.make);
            } else if (fn == macroSym) {
              if (env != null)
                throw new EvalException("nested macro", x);
              return _compile(arg, null, Macro.make);
            } else if (fn == quasiquoteSym) {
              if (arg != null && arg.cdr == null)
                x = qqExpand(arg.car);
              else
                throw new EvalException("bad quasiquote", x);
            } else {
              throw new EvalException("bad keyword", fn);
            }
          } else {      // Application of a function
            // Expand fn = eval(fn, env) here on Sym for speed.
            if (fn is Sym) {
              fn = globals[fn];
              if (fn == null)
                throw new EvalException("undefined", x.car);
            } else {
              fn = eval(fn, env);
            }

            if (fn is Closure) {
              env = fn.makeEnv(this, arg, env);
              x = _evalProgN(fn.body, env);
            } else if (fn is Macro) {
              x = fn.expandWith(this, arg);
            } else if (fn is BuiltInFunc) {
              return fn.evalWith(this, arg, env);
            } else {
              throw new EvalException("not applicable", fn);
            }
          }
        } else if (x is Lambda) {
          return new Closure.from(x, env);
        } else {
          return x;             // numbers, strings, null etc.
        }
      }
    } on EvalException catch (ex) {
      if (ex.trace.length < 10)
        ex.trace.add(str(x));
      throw ex;
    }
  }
```


This methods implements tail call optimization.

To see how it implements the optimization, first,
look at the branch of `if (fn == prognSym)`, which covers an evaluation 
for any Lisp expression *(progn E1 E2 ... En)*.
The branch performs `x = _evalProgN(arg, env)`.

```Dart
            } else if (fn == prognSym) {
              x = _evalProgN(arg, env);
            } ...
```


As shown below, `_evalProgN(arg, env)` evaluates each element of `arg`, 
assumed to be a Lisp list *(E1 E2 ... En)*,
from *E1* to *En-1* in turn and returns the tail expression *En*, not evaluating it:

```Dart
  /// (progn E1 E2.. En) => Evaluate E1, E2, .. except for En and return it.
  _evalProgN(Cell j, Cell env) {
    if (j == null)
      return null;
    for (;;) {
      var x = j.car;
      j = cdrCell(j);
      if (j == null)
        return x;       // The tail expression will be evaluated at the caller.
      eval(x, env);
    }
  }
```


where the function `cdrCell` is defined as follows:

```Dart
/// Get cdr of list x as a Cell or null.
Cell cdrCell(Cell x) {
  var k = x.cdr;
  if (k is Cell)
    return k;
  else if (k == null)
    return null;
  else
    throw new EvalException("proper list expected", x);
}
```


Thus the tail of any *progn* expression in Lisp will be evaluated back in the same level 
of nesting as the *progn* expression itself.
The same applies to any *cond* (conditional) expression.



Now look at the branch of `if (fn is Closure)`, which covers any application of lambda expression
*((lambda (A1 ... An) E1 ... Em) X1 ... Xn)*:

```Dart
            if (fn is Closure) {
              env = fn.makeEnv(this, arg, env);
              x = _evalProgN(fn.body, env);
            } ...
```


First, this branch performs  `env = fn.makeEnv(this, arg, env)` 
that builds a new environment from `arg`, assumed to be a list of actual arguments *X1 ... Xn*, upon
the environment held by `fn`, a `Closure` instance, and replaces `env` with the new environment.
The previous `env` has been used only to evaluate the actual arguments in `fn.makeEnv`.


------------------------------

**Note:**
An environment held by a `Closure` instance is no other than the environment existed when the original 
lambda expression was evaluated and replaced with the `Closure` instance.
Static scoping is implemented by discarding the current `env` and building a new one upon this environment.

------------------------------


Then, the body of the lambda expression `fn.body` is evaluated in the same way as a `progn` expression.
In other words, the tail of the lambda expression will be evaluated back in the same level 
of nesting as the lambda expression itself.


Thus tail call optimization is implemented.


------------------------------

**Note:**
If such a lambda expression happens to be the body of some tail recursive function, 
its tail recursive call will be back to the same level of nesting of its first call.
Thus tail recursion optimization is implemented.

------------------------------



<a name="5"></a>
## 5. Automatic Avoidance of Free Symbol Capture in Macro Expansion


In Common Lisp, it is danger to use a free symbol within a macro definition body. 
The symbol is free if the macro definition does not create a binding for it.
A trouble will occur when the macro is expanded in an expression that happens to 
contain a local variable of the same name as the symbol.
The symbol within the macro will be confused with the local variable.
It is likely to happen since the macro users do not necessarily know the detail of 
the macro definition.


I will show you an example based on the discussion of 
[L2Lisp ver. 2.0 (l2lisp-in-pascal)](https://github.com/nukata/l2lisp-in-pascal/tree/master/v2.0).
The following is an interactive session of GNU CLISP 2.48, an implementation of Common Lisp:

```
[1]> (setq x "poi")
"poi"
[2]> (defmacro m (n) `(setq x ,n))
M
[3]> ((lambda (x) (m 3) (print x))
      100)

3
3
[4]> x
"poi"
[5]>
```

The above application of the lambda expression [3] gives the actual argument *100* to the formal parameter *x*.
The parameter *x* is, however, reset to *3* because the expression *(m 3)* within the lambda expression is expanded to *(setq x 3)*.
The expression *(print x)* prints *3* and the result of the application is also *3*.
The global variable *x* which the macro *m* ought to have set remains the initial value *"poi"*.

The book below discusses this problem of Common Lisp in the ninth chapter as **Free Symbol Capture**.
In Common Lisp, this defect, provided by the language specification in a sense, has been avoided
traditionally by giving global variable names which begin and end with asterisks.
As discussed in the section 9.8 of the book, however, there is no adequate way to avoid it for function names
other than putting the code in a distinct package.

- Paul Graham: ["On Lisp"](http://www.paulgraham.com/onlisp.html), 1993

In other words, it can be said that the existence of this problem has been justified the separation of name space
into one for functions and another for variables, which 
has made the semantics of the Common Lisp language complex unnecessarily.

On the other hand, the Lisp in Dart presented here has no such quirk.
Any formal parameter in a lambda expression is valid only in the **lexical** scope of the body of the expression.
Within alpha equivalence in lambda calculus, it can be renamed to any name as far as it does not conflict with others lexically in the scope.
The name spaces for functions and variables are simply unified without the trouble.


The following session in the Lisp in Dart shows it:

```
> (setq x "poi")
"poi"
> (defmacro m (n) `(setq x ,n))
m
> ((lambda (x) (m 3) (print x))
   100)
100
100
>x
3
> 
``` 


It should be obvious why it goes so if you see the compiled macro expression and the compiled lambda expression:

```
> m
#<macro:1:((list 'setq 'x #0:0:n))>
> (lambda (x) (m 3) (print x))
#<closure:1:nil:((setq x 3) (print #0:0:x))>
> 
```


The `Closure` instance here has replaced the parameter *x* occurring in the expression body by the `Arg` instance
*#0:0:x* **before** expanding the macro application *(m 3)* to *(setq x 3)*.

To see how the replacement and expansion above are implemented,
look at the branch in the `eval` method ([§4](#4)) that covers the compilation of a lambda expression:

```Dart
            } else if (fn == lambdaSym) {
              return _compile(arg, env, Closure.make);
            } ...
```

where the `_compile` method is defined as follows:

```Dart
  /// Compile a Lisp list (macro ...) or (lambda ...).
  DefinedFunc _compile(Cell arg, Cell env, FuncFactory make) {
    if (arg == null)
      throw new EvalException("arglist and body expected", arg);
    Map<Sym, Arg> table = {};
    bool hasRest = _makeArgTable(arg.car, table);
    int arity = table.length;
    Cell body = cdrCell(arg);
    body = _scanForArgs(body, table);
    body = _expandMacros(body, 20); // Expand mcrs statically up to 20 nestings
    body = _compileInners(body);
    return make((hasRest) ? -arity : arity, body, env);
  }
```


This method operates as follows:

 1. `_makeArgTable` makes a table of `Arg` instances from the list of formal parameters.
 2. `_scanForArgs` replaces any parameters occurring in the body with the `Arg` instances by referring to the table.
 3. `_expandMacros` expands any macros and quasi-quotations in the body.
 4. `_compileInners` replaces any nesting lambda expressions with `Lambda` instances.
 5. The third argument of the method, `make`, makes the target instance.
    Now it makes a `closure` instance because `Closure.make` has been given as the actual argument.
    Below is the definition of the `Closure` class:

```Dart
/// Compiled lambda expresssion (Closure with environment)
class Closure extends DefinedFunc {
  /// The environment of the closure
  final Cell env;

  Closure(int carity, Cell body, this.env): super(carity, body);
  Closure.from(Lambda x, Cell env): this(x.carity, x.body, env);
  @override String toString() => "#<closure:$carity:${str(env)}:${str(body)}>";

  /// Make an environment to evaluate the body from a list of actual args.
  Cell makeEnv(Interp interp, Cell arg, Cell interpEnv) {
    List frame = makeFrame(arg);
    evalFrame(frame, interp, interpEnv);
    return new Cell(frame, env); // Prepend the frame to the closure's env.
  }

  static DefinedFunc make(int carity, Cell body, Cell env) =>
    new Closure(carity, body, env);
}
```


Thus, by replacing the formal parameters with `Arg` instances (based on alpha equivalence in lambda calculus)
before expanding the macros and quasi-quotations, the capture is avoided automatically.

------------------------------

**Note:**
This avoidance will be useful also for Scheme, a popular language that unifies name spaces for functions and variables,
if you want to adopt traditional macros similar to those of Common Lisp for it.
I wonder if there exists such a dialect of Scheme.

------------------------------


------------------------------

**Note:**
By the way, as to another type of variable capture given in the ninth chapter of the book,
**Macro Argument Capture**, it is as dangerous as in Common Lisp.
But the danger can be avoided also as safely as in Common Lisp by using the `gensym` function.
The following is such an example from the `prelude` of the interpreter.

```Lisp
(defmacro dolist (spec &rest body) ; (dolist (name list [result]) body...)
  (let ((name (car spec))
        (list (gensym)))
    `(let (,name
           (,list ,(cadr spec)))
       (while ,list
         (setq ,name (car ,list))
         ,@body
         (setq ,list (cdr ,list)))
       ,@(if (cddr spec)
             `((setq ,name nil)
               ,(caddr spec))))))
```

This will be compiled as follows:

```
> dolist
#<macro:-2:((#<lambda:2:((cons 'let (cons (list #0:0:name (list #0:1:list (cadr 
#1:0:spec))) (cons (_append (cons 'while (cons #0:1:list (cons (list 'setq #0:0:
name (list 'car #0:1:list)) #1:1:body))) (list (list 'setq #0:1:list (list 'cdr 
#0:1:list)))) (cond ((cddr #1:0:spec) (list (list 'setq #0:0:name nil) (caddr #1
:0:spec))))))))> (car #0:0:spec) (gensym)))>
> 
``` 

Note also that the local variable *list* introduced by *let* is replaced with the `Arg` instance *#0:1:list*,
which is distinguished from the built-in function *list* introduced by the expansion of quasi-quotations.
This is another example of **automatic avoidance of free symbol capture**.

Below is an example of its application.

```
> (dolist (e '(1 2 3 4 5 6 10)) (print e))
1
2
3
4
5
6
10
nil
> 
```

------------------------------
