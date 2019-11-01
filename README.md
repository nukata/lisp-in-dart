# Lisp in Dart

This is a small Lisp interpreter I wrote the original in 2015 (H27) in Dart 1.9.
It had been presented under the MIT License at
<http://www.oki-osk.jp/esc/dart/lisp.html> (broken link)
until the spring of 2017 (H29).
I slightly modified it to match Dart 2.0 and made the repository in GitHub
in 2018 (H30).

Now in 2019 (R1), I found the old hack in `Sym` class

```Dart
  @override int get hashCode => name.hashCode;
```

which had once accelerated the interpreter is effective again in Dart 2.5.
So I included the hack in `Sym` class again.
In addition, I made use of `BigInt` 
since `int` does not have inifinite-precision in Dart 2.0 and later.

See [`IMPLEMENTATION-NOTES.md`](IMPLEMENTATION-NOTES.md)
for other details of the implementation.


## How to use

It runs in Dart 2.5 and later.

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

There are five files ending with `.l` under the `examples` folder.
These run also in Emacs Lisp and Common Lisp.
You may find the Lisp in Dart comparably fast to Emacs Lisp which is
written in C.

- [`qsort.l`](examples/qsort.l)
  performs a quick sort.

```
$ ./lisp.dart examples/qsort.l
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

- [`fact100.l`](examples/fact100.l)
  calculates 100!.

```
$ ./lisp.dart examples/fact100.l 
93326215443944152681699238856266700490715968264381621468592963895217599993229915
608941463976156518286253697920827223758251185210916864000000000000000000000000
$
```

- [`fib15.l`](examples/fib15.l)
  calculates Fibonacci for 15.

- [`eval-fib15.l`](examples/eval-fib15.l)
  calculates Fibonacci for 15 on a meta-circular Lisp evaluator.

```
$ ./lisp.dart example/eval-fib15.l
987
$ 
```

- [`eval-eval-fib15.l`](examples/eval-eval-fib15.l)
  calculates Fibonacci for 15 on a meta-circular Lisp evaluator 
  on a meta-circular Lisp evaluator.

The examples of `eval-fib15.l` and `eval-eval-fib15.l` are inspired 
by <https://github.com/zick/ZickStandardLisp>.

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


## License

This is under the MIT License.
See [`lisp.dart`](lisp.dart#L1426-L1447).
