# Lisp in Dart

This is a small Lisp interpreter I wrote 3 years ago (2015) in Dart.
It had been presented under the MIT License at
<http://www.oki-osk.jp/esc/dart/lisp.html> until last spring (2017).
Now I have slightly modified it to match Dart 2.0.

See [`IMPLEMENTATION-NOTES.md`](IMPLEMENTATION-NOTES.md)
for the details of implementation.

## How to use

It runs in Dart 1.24 and later.

```
$ dart lisp.dart
> "hello, world"
"hello, world"
> (+ 5 6)
11
> (exit 0)
$
```

You can give it a file name of your Lisp script.

```
$ dart lisp.dart examples/fib15.l
987
$
```

If you put a "`-`" after the file name, it will
begin an interactive session after running the file.

```
$ dart lisp.dart examples/fib15.l -
987
> (fib 0)
1
> (fib 1)
1
> (fib 2)
2
> (exit 0)
$ 
```

## Examples

There are four files ending with `.l` under the `examples` folder.
These run also in Emacs Lisp and Common Lisp.
You will find the Lisp in Dart comparably fast to Emacs Lisp which is
written in C.

```
$ dart lisp.dart examples/qsort.l
(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
$ 
```

```
$ emacs -batch -l examples/qsort.l

(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
$ 
```

```
$ clisp examples/qsort.l

(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
$ 
```

- [`qsort.l`](examples/qsort.l)
  performs a quick sort.

- [`fib15.l`](examples/fib15.l)
  calculates Fibonacci for 15.

- [`eval-fib15.l`](examples/eval-fib15.l)
  calculates Fibonacci for 15 on a meta-circular Lisp evaluator.

- [`eval-eval-fib15.l`](examples/eval-eval-fib15.l)
  calculates Fibonacci for 15 on a meta-circular Lisp evaluator 
  on a meta-circular Lisp evaluator.

There is one more example:

- [`interp_in_isolate.dart`](examples/interp_in_isolate.dart)
  runs a Lisp interpreter in another isolate of Dart.
  You can embed an interpreter within your _Flutter_ app in the same way.

```
$ dart examples/interp_in_isolate.dart
=> 11
=> 1
=> (1)
```

The examples of `eval-fib15.l` and `eval-eval-fib15.l` are inspired 
by <https://github.com/zick/ZickStandardLisp>.

## License

This is under the MIT License.
See [`lisp.dart`](lisp.dart#L1191-L1212).
