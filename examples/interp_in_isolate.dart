#!/usr/bin/env dart
// An example of running Lisp in another isolate

import "dart:async";
import "dart:io";
import "dart:isolate";

import "../lisp.dart" as lisp;

// A simple substitute for stdout by SendPort
class SendPortOut extends StringSink {
  SendPort sp;

  SendPortOut(this.sp);
  @override void write(x) { sp.send(x); }
  @override void writeln([x=""]) { sp.send("${x}\n"); }
  @override void writeAll(x, [sep=""]) { throw new UnimplementedError(); }
  @override void writeCharCode(int code) { throw new UnimplementedError(); }
}

// A Read-Eval-Send-Loop in an isolate
Future resloop(SendPort sp) async {
  try {
    var rp = new ReceivePort();
    sp.send(rp.sendPort);

    lisp.Interp interp = await lisp.makeInterp();
    interp.cout = new SendPortOut(sp);

    StreamIterator<String> rf = new StreamIterator(rp.cast<String>());
    var reader = new lisp.Reader(rf);
    for (;;) {
      var sExp = await reader.read();
      var result = interp.eval(sExp, null);
      sp.send(lisp.str(result));
    }
  } catch (ex) {
    print("Error ${ex}");
  }
}

// Run Lisp in another isolate.
main(List<String> args) async {
  var rp = new ReceivePort();
  Isolate.spawn(resloop, rp.sendPort);

  var rf = new StreamIterator(rp);

  if (! await rf.moveNext()) exit(1);
  SendPort sp = rf.current as SendPort;

  sp.send("(+ 5 6)");
  if (! await rf.moveNext()) exit(1);
  print("=> ${rf.current}");    // => 11 as the result of (+ 5 6)

  sp.send("(cons (princ 1) nil)");
  if (! await rf.moveNext()) exit(1);
  print("=> ${rf.current}");    // => 1 as the effect of (princ 1)

  if (! await rf.moveNext()) exit(1);
  print("=> ${rf.current}");    // => (1) as the result of (cons 1 nil)
  exit(0);
}
