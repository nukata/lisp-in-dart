#!/usr/bin/env dart
// Nukata Lisp 2.00.0 in Dart 2.5 (H27.03.16/R01.11.02) by SUZUKI Hisao

import "dart:async";
import "dart:convert";
import "dart:io";

const intBits = 63;             // 53 for dart2js

/// Converts [a] into an int if possible.
normalize(BigInt a) => (a.bitLength <= intBits) ? a.toInt() : a;

/// Is [a] a number?
bool isNumber(a) => a is num || a is BigInt;

/// Calculates [a] + [b].
add(a, b) {
  if (a is int) {
    if (b is int) {
      if (a.bitLength < intBits && b.bitLength < intBits) {
        return a + b;
      } else {
        return normalize(BigInt.from(a) + BigInt.from(b));
      }
    } else if (b is double) {
      return a + b;
    } else if (b is BigInt) {
      return normalize(BigInt.from(a) + b);
    }
  } else if (a is double) {
    if (b is num) {
      return a + b;
    } else if (b is BigInt) {
      return a + b.toDouble();
    }
  } else if (a is BigInt) {
    if (b is int) {
      return normalize(a + BigInt.from(b));
    } else if (b is double) {
      return a.toDouble() + b;
    } else if (b is BigInt) {
      return normalize(a + b);
    }
  }
  throw ArgumentError("$a, $b");
}

/// Calculates [a] - [b].
subtract(a, b) {
  if (a is int) {
    if (b is int) {
      if (a.bitLength < intBits && b.bitLength < intBits) {
        return a - b;
      } else {
        return normalize(BigInt.from(a) - BigInt.from(b));
      }
    } else if (b is double) {
      return a - b;
    } else if (b is BigInt) {
      return normalize(BigInt.from(a) - b);
    }
  } else if (a is double) {
    if (b is num) {
      return a - b;
    } else if (b is BigInt) {
      return a - b.toDouble();
    }
  } else if (a is BigInt) {
    if (b is int) {
      return normalize(a - BigInt.from(b));
    } else if (b is double) {
      return a.toDouble() - b;
    } else if (b is BigInt) {
      return normalize(a - b);
    }
  }
  throw ArgumentError("$a, $b");
}

/// Compares [a] and [b].
/// Returns -1, 0 or 1 as [a] is less than, equal to, or greater than [b].
num compare(a, b) {
  if (a is int) {
    if (b is int) {
      if (a.bitLength < intBits && b.bitLength < intBits) {
        return (a - b).sign;
      } else {
        return (BigInt.from(a) - BigInt.from(b)).sign;
      }
    } else if (b is double) {
      return (a - b).sign;
    } else if (b is BigInt) {
      return (BigInt.from(a) - b).sign;
    }
  } else if (a is double) {
    if (b is num) {
      return (a - b).sign;
    } else if (b is BigInt) {
      return (a - b.toDouble()).sign;
    }
  } else if (a is BigInt) {
    if (b is int) {
      return (a - BigInt.from(b)).sign;
    } else if (b is double) {
      return (a.toDouble() - b).sign;
    } else if (b is BigInt) {
      return (a - b).sign;
    }
  }
  throw ArgumentError("$a, $b");
}

/// Calculates [a] * [b].
multiply(a, b) {
  if (a is int) {
    if (b is int) {
      if (a.bitLength + b.bitLength < intBits) {
        return a * b;
      } else {
        return normalize(BigInt.from(a) * BigInt.from(b));
      }
    } else if (b is double) {
      return a * b;
    } else if (b is BigInt) {
      return BigInt.from(a) * b;
    }
  } else if (a is double) {
    if (b is num) {
      return a * b;
    } else if (b is BigInt) {
      return a * b.toDouble();
    }
  } else if (a is BigInt) {
    if (b is int) {
      return a * BigInt.from(b);
    } else if (b is double) {
      return a.toDouble() * b;
    } else if (b is BigInt) {
      return a * b;
    }
  }
  throw ArgumentError("$a, $b");
}

/// Calculates [a] / [b] (rounded quotient).
double divide(a, b) {
  if (a is int) {
    if (b is num) {
      return a / b;
    } else if (b is BigInt) {
      return BigInt.from(a) / b;
    }
  } else if (a is double) {
    if (b is num) {
      return a / b;
    } else if (b is BigInt) {
      return a / b.toDouble();
    }
  } else if (a is BigInt) {
    if (b is int) {
      return a / BigInt.from(b);
    } else if (b is double) {
      return a.toDouble() / b;
    } else if (b is BigInt) {
      return a / b;
    }
  }
  throw ArgumentError("$a, $b");
}

/// Calculates the quotient of [a] and [b].
quotient(a, b) {
  if (a is int) {
    if (b is num) {
      return a ~/ b;
    } else if (b is BigInt) {
      return normalize(BigInt.from(a) ~/ b);
    }
  } else if (a is double) {
    if (b is num) {
      return a ~/ b;
    } else if (b is BigInt) {
      return a ~/ b.toDouble();
    }
  } else if (a is BigInt) {
    if (b is int) {
      return normalize(a ~/ BigInt.from(b));
    } else if (b is double) {
      return a.toDouble() ~/ b;
    } else if (b is BigInt) {
      return normalize(a ~/ b);
    }
  }
  throw ArgumentError("$a, $b");
}

/// Calculates the remainder of the quotient of [a] and [b].
remainder(a, b) {
  if (a is int) {
    if (b is num) {
      return a.remainder(b);
    } else if (b is BigInt) {
      return normalize(BigInt.from(a).remainder(b));
    }
  } else if (a is double) {
    if (b is num) {
      return a.remainder(b);
    } else if (b is BigInt) {
      return a.remainder(b.toDouble());
    }
  } else if (a is BigInt) {
    if (b is int) {
      return normalize(a.remainder(BigInt.from(b)));
    } else if (b is double) {
      return a.toDouble().remainder(b);
    } else if (b is BigInt) {
      return normalize(a.remainder(b));
    }
  }
  throw ArgumentError("$a, $b");
}

/// Tries to parse a string as an int, a BigInt or a double.
/// Returns null if [s] was not parsed successfully.
tryParse(String s) {
  var r = BigInt.tryParse(s);
  return (r == null) ? double.tryParse(s) : normalize(r);
}

//----------------------------------------------------------------------

/// Cons cell
class Cell {
  var car;
  var cdr;

  Cell(this.car, this.cdr);
  @override String toString() => "($car . $cdr)";

  /// Length as a list
  int get length => foldl(0, this, (i, e) => i + 1);
}


/// mapcar((a b c), fn) => (fn(a) fn(b) fn(c))
Cell mapcar(Cell j, fn(x)) {
  if (j == null)
    return null;
  var a = fn(j.car);
  var d = j.cdr;
  if (d is Cell)
    d = mapcar(d, fn);
  if (identical(j.car, a) && identical(j.cdr, d))
    return j;
  return Cell(a, d);
}

/// foldl(x, (a b c), fn) => fn(fn(fn(x, a), b), c)
foldl(x, Cell j, fn(y, z)) {
  while (j != null) {
    x = fn(x, j.car);
    j = j.cdr;
  }
  return x;
}


/// Lisp symbol
class Sym {
  final String name;

  /// Constructs a symbol that is not interned.
  Sym.internal(this.name);

  @override String toString() => name;
  @override int get hashCode => name.hashCode; // Key to Speed for Dart

  /// The table of interned symbols
  static final Map<String, Sym> table = {};

  /// Constructs an interned symbol.
  /// Constructs a [Keyword] if [isKeyword] holds.
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


/// Expression keyword
class Keyword extends Sym {
  Keyword.internal(String name): super.internal(name);
  factory Keyword(String name) => Sym(name, true);
}


final Sym backQuoteSym = Sym("`");
final Sym commaAtSym = Sym(",@");
final Sym commaSym = Sym(",");
final Sym dotSym = Sym(".");
final Sym leftParenSym = Sym("(");
final Sym rightParenSym = Sym(")");
final Sym singleQuoteSym = Sym("'");

final Sym appendSym = Sym("append");
final Sym consSym = Sym("cons");
final Sym listSym = Sym("list");
final Sym restSym = Sym("&rest");
final Sym unquoteSym = Sym("unquote");
final Sym unquoteSplicingSym = Sym("unquote-splicing");

//----------------------------------------------------------------------

/// Common base class of Lisp functions
abstract class Func {
  /// Number of arguments, made negative if the function has &rest
  final int carity;

  int get arity => (carity < 0) ? -carity : carity;
  bool get hasRest => (carity < 0);
  int get fixedArgs => (carity < 0) ? -carity - 1 : carity; // # of fixed args.

  Func(this.carity);

  /// Makes a frame for local variables from a list of actual arguments.
  List makeFrame(Cell arg) {
    List frame = List(arity);
    int n = fixedArgs;
    int i;
    for (i = 0; i < n && arg != null; i++) { // Sets the list of fixed args.
      frame[i] = arg.car;
      arg = cdrCell(arg);
    }
    if (i != n || (arg != null && !hasRest))
      throw EvalException("arity not matched", this);
    if (hasRest)
      frame[n] = arg;
    return frame;
  }

  /// Evaluates each expression in a frame.
  void evalFrame(List frame, Interp interp, Cell env) {
    int n = fixedArgs;
    for (int i = 0; i < n; i++)
      frame[i] = interp.eval(frame[i], env);
    if (hasRest && frame[n] is Cell) {
      Cell z = null;
      Cell y = null;
      for (Cell j = frame[n]; j != null; j = cdrCell(j)) {
        var e = interp.eval(j.car, env);
        Cell x = Cell(e, null);
        if (z == null)
          z = x;
        else
          y.cdr = x;
        y = x;
      }
      frame[n] = z;
    }
  }
}


/// Common base class of functions which are defined with Lisp expressions
abstract class DefinedFunc extends Func {
  /// Lisp list as the function body
  final Cell body;

  DefinedFunc(int carity, this.body): super(carity);
}


/// Common function type which represents any factory method of DefinedFunc
typedef DefinedFunc FuncFactory(int cariy, Cell body, Cell env);


/// Compiled macro expression
class Macro extends DefinedFunc {
  Macro(int carity, Cell body): super(carity, body);
  @override String toString() => "#<macro:$carity:${str(body)}>";

  /// Expands the macro with a list of actual arguments.
  expandWith(Interp interp, Cell arg) {
    List frame = makeFrame(arg);
    Cell env = Cell(frame, null);
    var x = null;
    for (Cell j = body; j != null; j = cdrCell(j))
      x = interp.eval(j.car, env);
    return x;
  }

  static DefinedFunc make(int carity, Cell body, Cell env) {
    assert(env == null);
    return Macro(carity, body);
  }
}


/// Compiled lambda expression (Within another function)
class Lambda extends DefinedFunc {
  Lambda(int carity, Cell body): super(carity, body);
  @override String toString() => "#<lambda:$carity:${str(body)}>";

  static DefinedFunc make(int carity, Cell body, Cell env) {
    assert(env == null);
    return Lambda(carity, body);
  }
}


/// Compiled lambda expresssion (Closure with environment)
class Closure extends DefinedFunc {
  /// The environment of the closure
  final Cell env;

  Closure(int carity, Cell body, this.env): super(carity, body);
  Closure.from(Lambda x, Cell env): this(x.carity, x.body, env);
  @override String toString() => "#<closure:$carity:${str(env)}:${str(body)}>";

  /// Makes an environment to evaluate the body from a list of actual args.
  Cell makeEnv(Interp interp, Cell arg, Cell interpEnv) {
    List frame = makeFrame(arg);
    evalFrame(frame, interp, interpEnv);
    return Cell(frame, env);    // Prepends the frame to the closure's env.
  }

  static DefinedFunc make(int carity, Cell body, Cell env) =>
    Closure(carity, body, env);
}


/// Function type which represents any built-in function body
typedef BuiltInFuncBody(List frame);

/// Built-in function
class BuiltInFunc extends Func {
  final String name;
  final BuiltInFuncBody body;

  BuiltInFunc(this.name, int carity, this.body): super(carity);
  @override String toString() => "#<$name:$carity>";

  /// Invokes the built-in function with a list of actual arguments.
  evalWith(Interp interp, Cell arg, Cell interpEnv) {
    List frame = makeFrame(arg);
    evalFrame(frame, interp, interpEnv);
    try {
      return body(frame);
    } on EvalException catch (ex) {
      throw ex;
    } catch (ex) {
      throw EvalException("$ex -- $name", frame);
    }
  }
}


/// Bound variable in a compiled lambda/macro expression
class Arg {
  final int level;
  final int offset;
  final Sym symbol;

  Arg(this.level, this.offset, this.symbol);
  @override String toString() => "#$level:$offset:$symbol";

  /// Sets a value [x] to the location corresponding to the variable in [env].
  void setValue(x, Cell env) {
    for (int i = 0; i < level; i++)
      env = env.cdr;
    env.car[offset] = x;
  }

  /// Gets a value from the location corresponding to the variable in [env].
  getValue(Cell env) {
    for (int i = 0; i < level; i++)
      env = env.cdr;
    return env.car[offset];
  }
}


/// Exception in evaluation
class EvalException implements Exception {
  final String message;
  final List<String> trace = [];

  EvalException(String msg, Object x, [bool quoteString=true])
    : message = msg + ": " + str(x, quoteString);

  @override String toString() {
    var s = "EvalException: $message";
    for (String line in trace)
      s += "\n\t$line";
    return s;
  }
}


/// Exception which indicates an absence of a variable
class NotVariableException extends EvalException {
  NotVariableException(x): super("variable expected", x);
}

//----------------------------------------------------------------------

/// Core of the interpreter
class Interp {
  /// Table of the global values of symbols
  final Map<Sym, Object> globals = {};

  /// Standard output of the interpreter
  StringSink cout = stdout;

  /// Sets built-in functions etc. as the global values of symbols.
  Interp() {
    def("car", 1, (List a) => (a[0] as Cell)?.car);
    def("cdr", 1, (List a) => (a[0] as Cell)?.cdr);
    def("cons", 2, (List a) => Cell(a[0], a[1]));
    def("atom", 1, (List a) => (a[0] is Cell) ? null : true);
    def("eq", 2, (List a) => identical(a[0], a[1]) ? true : null);

    def("list", -1, (List a) => a[0]);
    def("rplaca", 2, (List a) { a[0].car = a[1]; return a[1]; });
    def("rplacd", 2, (List a) { a[0].cdr = a[1]; return a[1]; });
    def("length", 1, (List a) => (a[0] == null) ? 0 : a[0].length);
    def("stringp", 1, (List a) => (a[0] is String) ? true : null);
    def("numberp", 1, (List a) => isNumber(a[0]) ? true : null);
    def("eql", 2, (List a) {
      var x = a[0];
      var y = a[1];
      return (x == y) ? true :
        (isNumber(x) && isNumber(y) && compare(x, y) == 0) ? true : null;
    });
    def("<", 2, (List a) => (compare(a[0], a[1]) < 0) ? true : null);
    def("%", 2, (List a) => remainder(a[0], a[1]));
    def("mod", 2, (List a) {
      var x = a[0];
      var y = a[1];
      var q = remainder(x, y);
      return (compare(multiply(x, y), 0) < 0) ? add(q, y) : q;
    });

    def("+", -1, (List a) => foldl(0, a[0], (i, j) => add(i, j)));
    def("*", -1, (List a) => foldl(1, a[0], (i, j) => multiply(i, j)));
    def("-", -2, (List a) {
      var x = a[0];
      Cell y = a[1];
      return (y == null) ? -x : foldl(x, y, (i, j) => subtract(i, j));
    });
    def("/", -3, (List a) =>
        foldl(divide(a[0], a[1]), a[2], (i, j) => divide(i, j)));

    def("truncate", -2, (List a) {
      var x = a[0];
      Cell y = a[1];
      return (y == null) ? quotient(x, 1) :
        (y.cdr == null) ? quotient(x, y.car) :
        throw "one or two arguments expected";
    });

    def("prin1", 1, (List a) { cout.write(str(a[0], true)); return a[0]; });
    def("princ", 1, (List a) { cout.write(str(a[0], false)); return a[0]; });
    def("terpri", 0, (List a) { cout.writeln(); return true; });

    var gensymCounterSym = Sym("*gensym-counter*");
    globals[gensymCounterSym] = 1;
    def("gensym", 0, (List a) {
      int i = globals[gensymCounterSym];
      globals[gensymCounterSym] = i + 1;
      return new Sym.internal("G$i");
    });

    def("make-symbol", 1, (List a) => new Sym.internal(a[0]));
    def("intern", 1, (List a) => Sym(a[0]));
    def("symbol-name", 1, (List a) => (a[0] as Sym).name);

    def("apply", 2, (List a) => eval(Cell(a[0], mapcar(a[1], qqQuote)), null));

    def("exit", 1, (List a) => exit(a[0]));
    def("dump", 0, (List a) => globals.keys.fold(null, (x, y) => Cell(y, x)));
    globals[Sym("*version*")] =
      Cell(2.000, Cell("Dart", Cell("Nukata Lisp", null)));
    // named after Tōkai-dō Mikawa-koku Nukata-gun (東海道 三河国 額田郡)
  }

  /// Defines a built-in function by giving a name, an arity, and a body.
  void def(String name, int carity, BuiltInFuncBody body) {
    globals[Sym(name)] = BuiltInFunc(name, carity, body);
  }

  /// Evaluates a Lisp expression in an environment.
  eval(x, Cell env) {
    try {
      for (;;) {
        if (x is Arg) {
          return x.getValue(env);
        } else if (x is Sym) {
          if (globals.containsKey(x))
            return globals[x];
          throw EvalException("void variable", x);
        } else if (x is Cell) {
          var fn = x.car;
          Cell arg = cdrCell(x);
          if (fn is Keyword) {
            if (fn == quoteSym) {
              if (arg != null && arg.cdr == null)
                return arg.car;
              throw EvalException("bad quote", x);
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
                throw EvalException("nested macro", x);
              return _compile(arg, null, Macro.make);
            } else if (fn == quasiquoteSym) {
              if (arg != null && arg.cdr == null)
                x = qqExpand(arg.car);
              else
                throw EvalException("bad quasiquote", x);
            } else {
              throw EvalException("bad keyword", fn);
            }
          } else {      // Application of a function
            // Expands fn = eval(fn, env) here on Sym for speed.
            if (fn is Sym) {
              fn = globals[fn];
              if (fn == null)
                throw EvalException("undefined", x.car);
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
              throw EvalException("not applicable", fn);
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

  /// (progn E1 E2.. En) => Evaluates E1, E2, .. except for En and returns it.
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

  /// Evaluates a conditional expression and returns the selection unevaluated.
  _evalCond(Cell j, Cell env) {
    for (; j != null; j = cdrCell(j)) {
      var clause = j.car;
      if (clause is Cell) {
        var result = eval(clause.car, env);
        if (result != null) {   // If the condition holds
          Cell body = cdrCell(clause);
          if (body == null)
            return qqQuote(result);
          else
            return _evalProgN(body, env);
        }
      } else if (clause != null) {
        throw EvalException("cond test expected", clause);
      }
    }
    return null;                // No clause holds.
  }

  /// (setq V1 E1 ..) => Evaluates Ei and assigns it to Vi; returns the last.
  _evalSetQ(Cell j, Cell env) {
    var result = null;
    for (; j != null; j = cdrCell(j)) {
      var lval = j.car;
      j = cdrCell(j);
      if (j == null)
        throw EvalException("right value expected", lval);
      result = eval(j.car, env);
      if (lval is Arg)
        lval.setValue(result, env);
      else if (lval is Sym && lval is! Keyword)
        globals[lval] = result;
      else
        throw NotVariableException(lval);
    }
    return result;
  }

  /// Compiles a Lisp list (macro ...) or (lambda ...).
  DefinedFunc _compile(Cell arg, Cell env, FuncFactory make) {
    if (arg == null)
      throw EvalException("arglist and body expected", arg);
    Map<Sym, Arg> table = {};
    bool hasRest = _makeArgTable(arg.car, table);
    int arity = table.length;
    Cell body = cdrCell(arg);
    body = _scanForArgs(body, table);
    body = _expandMacros(body, 20); // Expands ms statically up to 20 nestings.
    body = _compileInners(body);
    return make((hasRest) ? -arity : arity, body, env);
  }

  /// Expands macros and quasi-quotations in an expression.
  _expandMacros(j, int count) {
    if (count > 0 && j is Cell) {
      var k = j.car;
      if (k == quoteSym || k == lambdaSym || k == macroSym) {
        return j;
      } else if (k == quasiquoteSym) {
        Cell d = cdrCell(j);
        if (d != null && d.cdr == null) {
          var z = qqExpand(d.car);
          return _expandMacros(z, count);
        }
        throw EvalException("bad quasiquote", j);
      } else {
        if (k is Sym)
          k = globals[k];       // null if k does not have a value
        if (k is Macro) {
          Cell d = cdrCell(j);
          var z = k.expandWith(this, d);
          return _expandMacros(z, count - 1);
        } else {
          return mapcar(j, (x) => _expandMacros(x, count));
        }
      }
    } else {
      return j;
    }
  }

  /// Replaces inner lambda-expressions with Lambda instances.
  _compileInners(j) {
    if (j is Cell) {
      var k = j.car;
      if (k == quoteSym) {
        return j;
      } else if (k == lambdaSym) {
        Cell d = cdrCell(j);
        return _compile(d, null, Lambda.make);
      } else if (k == macroSym) {
        throw EvalException("nested macro", j);
      } else {
        return mapcar(j, (x) => _compileInners(x));
      }
    } else {
      return j;
    }
  }
}

//----------------------------------------------------------------------

/// Makes an argument-table; returns true if there is a rest argument.
bool _makeArgTable(arg, Map<Sym, Arg> table) {
  if (arg == null) {
    return false;
  } else if (arg is Cell) {
    int offset = 0;             // offset value within the call-frame
    bool hasRest = false;
    for (; arg != null; arg = cdrCell(arg)) {
      var j = arg.car;
      if (hasRest)
        throw EvalException("2nd rest", j);
      if (j == restSym) {       // &rest var
        arg = cdrCell(arg);
        if (arg == null)
          throw NotVariableException(arg);
        j = arg.car;
        if (j == restSym)
          throw NotVariableException(j);
        hasRest = true;
      }
      Sym sym =
        (j is Sym) ? j :
        (j is Arg) ? j.symbol : throw NotVariableException(j);
      if (table.containsKey(sym))
        throw EvalException("duplicated argument name", sym);
      table[sym] = Arg(0, offset, sym);
      offset++;
    }
    return hasRest;
  } else {
    throw EvalException("arglist expected", arg);
  }
}

/// Scans [j] for formal arguments in [table] and replaces them with Args.
/// And scans [j] for free Args not in [table] and promotes their levels.
_scanForArgs(j, Map<Sym, Arg> table) {
  if (j is Sym) {
    return table[j] ?? j;
  } else if (j is Arg) {
    return table[j.symbol] ?? Arg(j.level + 1, j.offset, j.symbol);
  } else if (j is Cell) {
    if (j.car == quoteSym) {
      return j;
    } else if (j.car == quasiquoteSym) {
      return Cell(quasiquoteSym, _scanForQQ(j.cdr, table, 0));
    } else {
      return mapcar(j, (x) => _scanForArgs(x, table));
    }
  } else {
    return j;
  }
}

/// Scans for quasi-quotes.
/// And does [_scanForArgs] them depending on the nesting level.
_scanForQQ(j, Map<Sym, Arg> table, int level) {
  if (j is Cell) {
    var k = j.car;
    if (k == quasiquoteSym) {
      return Cell(k, _scanForQQ(j.cdr, table, level + 1));
    } else if (k == unquoteSym || k == unquoteSplicingSym) {
      var d = (level == 0) ? _scanForArgs(j.cdr, table) :
                             _scanForQQ(j.cdr, table, level - 1);
      if (identical(d, j.cdr))
        return j;
      return Cell(k, d);
    } else {
      return mapcar(j, (x) => _scanForQQ(x, table, level));
    }
  } else {
    return j;
  }
}

/// Gets cdr of list [x] as a Cell or null.
Cell cdrCell(Cell x) {
  var k = x.cdr;
  if (k is Cell)
    return k;
  else if (k == null)
    return null;
  else
    throw EvalException("proper list expected", x);
}

//----------------------------------------------------------------------
// Quasi-Quotation

/// Expands [x] of any quasi-quotation `x into the equivalent S-expression.
qqExpand(x) {
  return _qqExpand0(x, 0);      // Begins with the nesting level 0.
}

_qqExpand0(x, int level) {
  if (x is Cell) {
    if (x.car == unquoteSym) {  // ,a
      if (level == 0)
        return x.cdr.car;       // ,a => a
    }
    Cell t = _qqExpand1(x, level);
    if (t.car is Cell && t.cdr == null) {
      Cell k = t.car;
      if (k.car == listSym || k.car == consSym)
        return k;
    }
    return Cell(appendSym, t);
  } else {
    return qqQuote(x);
  }
}

/// Quotes [x] so that the result evaluates to [x].
qqQuote(x) =>
  (x is Sym || x is Cell) ? Cell(quoteSym, Cell(x, null)) : x;

// Expands [x] of `x so that the result can be used as an argument of append.
// Example 1: (,a b) => h=(list a) t=((list 'b)) => ((list a 'b))
// Example 2: (,a ,@(cons 2 3)) => h=(list a) t=((cons 2 3))
//                              => ((cons a (cons 2 3)))
Cell _qqExpand1(x, int level) {
  if (x is Cell) {
    if (x.car == unquoteSym) {  // ,a
      if (level == 0)
        return x.cdr;           // ,a => (a)
      level--;
    } else if (x.car == quasiquoteSym) { // `a
      level++;
    }
    var h = _qqExpand2(x.car, level);
    Cell t = _qqExpand1(x.cdr, level); // != null
    if (t.car == null && t.cdr == null) {
      return Cell(h, null);
    } else if (h is Cell) {
      if (h.car == listSym) {
        if (t.car is Cell) {
          Cell tcar = t.car;
          if (tcar.car == listSym) {
            var hh = _qqConcat(h, tcar.cdr);
            return Cell(hh, t.cdr);
          }
        }
        if (h.cdr is Cell) {
          var hh = _qqConsCons(h.cdr, t.car);
          return Cell(hh, t.cdr);
        }
      }
    }
    return Cell(h, t);
  } else {
    return Cell(qqQuote(x), null);
  }
}

// (1 2), (3 4) => (1 2 3 4)
_qqConcat(Cell x, Object y) =>
  (x == null) ? y : Cell(x.car, _qqConcat(x.cdr, y));

// (1 2 3), "a" => (cons 1 (cons 2 (cons 3 "a")))
_qqConsCons(Cell x, Object y) =>
  (x == null) ? y :
  Cell(consSym, Cell(x.car, Cell(_qqConsCons(x.cdr, y), null)));

// Expands [y] = x.car of `x so that result can be used as an arg of append.
// Example: ,a => (list a); ,@(foo 1 2) => (foo 1 2); b => (list 'b)
_qqExpand2(y, int level) {
  if (y is Cell) {
    if (y.car == unquoteSym) {  // ,a
      if (level == 0)
        return Cell(listSym, y.cdr); // ,a => (list a)
      level--;
    } else if (y.car == unquoteSplicingSym) { // ,@a
      if (level == 0)
        return y.cdr.car;       // ,@a => a
      level--;
    } else if (y.car == quasiquoteSym) { // `a
      level++;
    }
  }
  return Cell(listSym, Cell(_qqExpand0(y, level), null));
}

//----------------------------------------------------------------------

/// Reader of Lisp expressions
class Reader {
  final StreamIterator<String> _rf;
  var _token;
  Iterator<String> _tokens = <String>[].iterator;
  int _lineNo = 0;
  bool _erred = false;

  /// Constructs a Reader which will read Lisp expressions from a given arg.
  Reader(this._rf);

  /// Reads a Lisp expression; returns #EOF if the input runs out normally.
  Future<Object> read() async {
    try {
      await _readToken();
      return await _parseExpression();
    } on FormatException catch (ex) {
      _erred = true;
      throw EvalException("syntax error",
          "${ex.message} -- $_lineNo: ${_rf.current}", false);
    }
  }

  Future<Object> _parseExpression() async {
    if (_token == leftParenSym) { // (a b c)
      await _readToken();
      return await _parseListBody();
    } else if (_token == singleQuoteSym) { // 'a => (quote a)
      await _readToken();
      return Cell(quoteSym, Cell(await _parseExpression(), null));
    } else if (_token == backQuoteSym) { // `a => (quasiquote a)
      await _readToken();
      return Cell(quasiquoteSym, Cell(await _parseExpression(), null));
    } else if (_token == commaSym) { // ,a => (unquote a)
      await _readToken();
      return Cell(unquoteSym, Cell(await _parseExpression(), null));
    } else if (_token == commaAtSym) { // ,@a => (unquote-splicing a)
      await _readToken();
      return Cell(unquoteSplicingSym, Cell(await _parseExpression(), null));
    } else if (_token == dotSym || _token == rightParenSym) {
      throw FormatException('unexpected "$_token"');
    } else {
      return _token;
    }
  }

  Future<Cell> _parseListBody() async {
    if (_token == #EOF) {
      throw FormatException("unexpected EOF");
    } else if (_token == rightParenSym) {
      return null;
    } else {
      var e1 = await _parseExpression();
      await _readToken();
      var e2;
      if (_token == dotSym) {   // (a . b)
        await _readToken();
        e2 = await _parseExpression();
        await _readToken();
        if (_token != rightParenSym)
          throw FormatException('")" expected: $_token');
      } else {
        e2 = await _parseListBody();
      }
      return Cell(e1, e2);
    }
  }

  /// Reads the next token and sets it to [_token].
  Future _readToken() async {
    while (! _tokens.moveNext() || _erred) { // line ends or erred last time
      _erred = false;
      _lineNo++;
      if (! await _rf.moveNext()) {
        _token = #EOF;
        return;
      }
      _tokens = _tokenPat.allMatches(_rf.current)
        .map((Match m) => m[1])
        .where((String s) => s != null)
        .iterator;
    }
    _token = _tokens.current;
    if (_token[0] == '"') {
      String s = _token;
      int n = s.length - 1;
      if (n < 1 || s[n] != '"')
        throw FormatException("bad string: '$s'");
      s = s.substring(1, n);
      s = s.replaceAllMapped(_escapePat, (Match m) {
        String key = m[1];
        return _escapes[key] ?? "\\$key";
      });
      _token = s;
      return;
    }
    var n = tryParse(_token);
    if (n != null) {
      _token = n;
    } else if (_token == "nil") {
      _token = null;
    } else if (_token == "t") {
      _token = true;
    } else {
      _token = Sym(_token);
    }
  }

  /// Regular expression to split a line into Lisp tokens
  static final _tokenPat =
    RegExp('\\s+|;.*\$|("(\\\\.?|.)*?"|,@?|[^()\'`~"; \t]+|.)');

  /// Regular expression to take an escape sequence out of a string
  static final _escapePat = RegExp(r'\\(.)');

  /// Mapping from a character of escape sequence to its string value
  static final Map<String, String> _escapes = <String, String>{
    "\\": "\\",
    '"': '"',
    "n": "\n", "r": "\r", "f": "\f", "b": "\b", "t": "\t", "v": "\v"
  };
}

//----------------------------------------------------------------------

/// Mapping from a quote symbol to its string representation
final Map<Sym, String> _quotes = <Sym, String>{
  quoteSym: "'", quasiquoteSym: "`", unquoteSym: ",", unquoteSplicingSym: ",@"
};

/// Makes a string representation of a Lisp expression
String str(x, [bool quoteString=true, int count, Set<Cell> printed]) {
  if (x == null) {
    return "nil";
  } else if (x == true) {
    return "t";
  } else if (x is Cell) {
    if (_quotes.containsKey(x.car) && x.cdr is Cell) {
      if (x.cdr.cdr == null)
        return _quotes[x.car] + str(x.cdr.car, true, count, printed);
    }
    return "(" + _strListBody(x, count, printed) + ")";
  } else if (x is String) {
    if (! quoteString)
      return x;
    var bf = StringBuffer('"');
    for (int ch in x.runes)
      switch (ch) {
        case 0x08: bf.write(r"\b"); break;
        case 0x09: bf.write(r"\t"); break;
        case 0x0A: bf.write(r"\n"); break;
        case 0x0B: bf.write(r"\v"); break;
        case 0x0C: bf.write(r"\f"); break;
        case 0x0D: bf.write(r"\r"); break;
        case 0x22: bf.write(r'\"'); break;
        case 0x5C: bf.write(r"\\"); break;
        default: bf.writeCharCode(ch); break;
      }
    bf.write('"');
    return bf.toString();
  } else if (x is List) {
    var s = x.map((e) => str(e, true, count, printed)).join(", ");
    return "[$s]";
  } else if (x is Sym) {
    if (x.isInterned)
      return x.name;
    return "#:$x";
  } else {
    return "$x";
  }
}

/// Makes a string representation of a list omitting its "(" and ")".
String _strListBody(Cell x, int count, Set<Cell> printed) {
  printed ??= Set<Cell>();
  count ??= 4;                  // threshold of ellipsis for circular lists
  var s = List<String>();
  var y;
  for (y = x; y is Cell; y = y.cdr) {
    if (printed.add(y)) {
      count = 4;
    } else {
      count--;
      if (count < 0) {
        s.add("...");           //  an ellipsis for a circular list
        return s.join(" ");
      }
    }
    s.add(str(y.car, true, count, printed));
  }
  if (y != null) {
    s.add(".");
    s.add(str(y, true, count, printed));
  }
  for (y = x; y is Cell; y = y.cdr)
    printed.remove(y);
  return s.join(" ");
}

//----------------------------------------------------------------------

/// Runs REPL (Read-Eval-Print Loop).
Future run(Interp interp, Stream<String> input) async {
  bool interactive = (input == null);
  input ??= stdin.transform(const Utf8Codec().decoder);
  input = input.transform(const LineSplitter());
  var lines = StreamIterator(input);
  var reader = Reader(lines);
  for (;;) {
    if (interactive) {
      stdout.write("> ");
      try {
        var sExp = await reader.read();
        if (sExp == #EOF)
          return;
        var x = interp.eval(sExp, null);
        print(str(x));
      } on Exception catch (ex) {
        print(ex);
      }
    } else {
      var sExp = await reader.read();
      if (sExp == #EOF)
        return;
      interp.eval(sExp, null);
    }
  }
}

// Keywords
final Sym condSym = Keyword("cond");
final Sym lambdaSym = Keyword("lambda");
final Sym macroSym = Keyword("macro");
final Sym prognSym = Keyword("progn");
final Sym quasiquoteSym = Keyword("quasiquote");
final Sym quoteSym = Keyword("quote");
final Sym setqSym = Keyword("setq");

/// Makes a Lisp interpreter initialized with [prelude].
Future<Interp> makeInterp() async {
  // Dart initializes static variables lazily.  Therefore, all keywords are
  // referred explicitly here so that they are initialized as keywords
  // before any occurrences of symbols of their names.
  [condSym, lambdaSym, macroSym, prognSym, quasiquoteSym, quoteSym, setqSym];

  var interp = Interp();
  Future<String> fs = new Future.value(prelude);
  Stream<String> ss = new Stream.fromFuture(fs);
  await run(interp, ss);
  return interp;
}

/// Runs each arg as a Lisp script in order.
/// Runs interactively for no arg or -.
main(List<String> args) async {
  try {
    Interp interp = await makeInterp();
    for (String fileName in (args.isEmpty) ? ["-"] : args) {
      if (fileName == "-") {
        await run(interp, null);
        print("Goodbye");
      } else {
        var file = File(fileName);
        Stream<List<int>> bytes = file.openRead();
        Stream<String> input = bytes.transform(const Utf8Codec().decoder);
        await run(interp, input);
      }
    }
    exit(0);
  } on Exception catch (ex) {
    print(ex);
    exit(1);
  }
}

/// Lisp initialization script
const String prelude = """
(setq defmacro
      (macro (name args &rest body)
             `(progn (setq ,name (macro ,args ,@body))
                     ',name)))

(defmacro defun (name args &rest body)
  `(progn (setq ,name (lambda ,args ,@body))
          ',name))

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun not (x) (eq x nil))
(defun consp (x) (not (atom x)))
(defun print (x) (prin1 x) (terpri) x)
(defun identity (x) x)

(setq
 = eql
 rem %
 null not
 setcar rplaca
 setcdr rplacd)

(defun > (x y) (< y x))
(defun >= (x y) (not (< x y)))
(defun <= (x y) (not (< y x)))
(defun /= (x y) (not (= x y)))

(defun equal (x y)
  (cond ((atom x) (eql x y))
        ((atom y) nil)
        ((equal (car x) (car y)) (equal (cdr x) (cdr y)))))

(defmacro if (test then &rest else)
  `(cond (,test ,then)
         ,@(cond (else `((t ,@else))))))

(defmacro when (test &rest body)
  `(cond (,test ,@body)))

(defmacro let (args &rest body)
  ((lambda (vars vals)
     (defun vars (x)
       (cond (x (cons (if (atom (car x))
                          (car x)
                        (caar x))
                      (vars (cdr x))))))
     (defun vals (x)
       (cond (x (cons (if (atom (car x))
                          nil
                        (cadar x))
                      (vals (cdr x))))))
     `((lambda ,(vars args) ,@body) ,@(vals args)))
   nil nil))

(defmacro letrec (args &rest body)      ; (letrec ((v e) ...) body...)
  (let (vars setqs)
    (defun vars (x)
      (cond (x (cons (caar x)
                     (vars (cdr x))))))
    (defun sets (x)
      (cond (x (cons `(setq ,(caar x) ,(cadar x))
                     (sets (cdr x))))))
    `(let ,(vars args) ,@(sets args) ,@body)))

(defun _append (x y)
  (if (null x)
      y
    (cons (car x) (_append (cdr x) y))))
(defmacro append (x &rest y)
  (if (null y)
      x
    `(_append ,x (append ,@y))))

(defmacro and (x &rest y)
  (if (null y)
      x
    `(cond (,x (and ,@y)))))

(defun mapcar (f x)
  (and x (cons (f (car x)) (mapcar f (cdr x)))))

(defmacro or (x &rest y)
  (if (null y)
      x
    `(cond (,x)
           ((or ,@y)))))

(defun listp (x)
  (or (null x) (consp x)))    ; NB (listp (lambda (x) (+ x 1))) => nil

(defun memq (key x)
  (cond ((null x) nil)
        ((eq key (car x)) x)
        (t (memq key (cdr x)))))

(defun member (key x)
  (cond ((null x) nil)
        ((equal key (car x)) x)
        (t (member key (cdr x)))))

(defun assq (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (eq key (car e)))
                     e
                   (assq key (cdr alist)))))))

(defun assoc (key alist)
  (cond (alist (let ((e (car alist)))
                 (if (and (consp e) (equal key (car e)))
                     e
                   (assoc key (cdr alist)))))))

(defun _nreverse (x prev)
  (let ((next (cdr x)))
    (setcdr x prev)
    (if (null next)
        x
      (_nreverse next x))))
(defun nreverse (list)            ; (nreverse '(a b c d)) => (d c b a)
  (cond (list (_nreverse list nil))))

(defun last (list)
  (if (atom (cdr list))
      list
    (last (cdr list))))

(defun nconc (&rest lists)
  (if (null (cdr lists))
      (car lists)
    (if (null (car lists))
        (apply nconc (cdr lists))
      (setcdr (last (car lists))
              (apply nconc (cdr lists)))
      (car lists))))

(defmacro while (test &rest body)
  (let ((loop (gensym)))
    `(letrec ((,loop (lambda () (cond (,test ,@body (,loop))))))
       (,loop))))

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

(defmacro dotimes (spec &rest body) ; (dotimes (name count [result]) body...)
  (let ((name (car spec))
        (count (gensym)))
    `(let ((,name 0)
           (,count ,(cadr spec)))
       (while (< ,name ,count)
         ,@body
         (setq ,name (+ ,name 1)))
       ,@(if (cddr spec)
             `(,(caddr spec))))))
""";

/*
  Copyright (c) 2015, 2016 OKI Software Co., Ltd.
  Copyright (c) 2018, 2019 SUZUKI Hisao

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.
*/
