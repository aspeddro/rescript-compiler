// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Queue = require("../../lib/js/queue.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

function to_list(q) {
  return List.rev(Queue.fold((function (l, x) {
                    return {
                            hd: x,
                            tl: l
                          };
                  }), /* [] */0, q));
}

var Q = {
  Empty: Queue.Empty,
  create: Queue.create,
  add: Queue.add,
  push: Queue.push,
  take: Queue.take,
  pop: Queue.pop,
  peek: Queue.peek,
  top: Queue.top,
  clear: Queue.clear,
  copy: Queue.copy,
  is_empty: Queue.is_empty,
  length: Queue.length,
  iter: Queue.iter,
  fold: Queue.fold,
  transfer: Queue.transfer,
  to_list: to_list
};

function does_raise(f, q) {
  try {
    Curry._1(f, q);
    return false;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Queue.Empty) {
      return true;
    }
    throw exn;
  }
}

var q = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

if (!(to_list(q) === /* [] */0 && q.length === 0)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 30, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(1, q);

if (!(Caml_obj.equal(to_list(q), {
          hd: 1,
          tl: /* [] */0
        }) && q.length === 1)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 32, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(2, q);

if (!(Caml_obj.equal(to_list(q), {
          hd: 1,
          tl: {
            hd: 2,
            tl: /* [] */0
          }
        }) && q.length === 2)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 34, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(3, q);

if (!(Caml_obj.equal(to_list(q), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }) && q.length === 3)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 36, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(4, q);

if (!(Caml_obj.equal(to_list(q), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: {
                hd: 4,
                tl: /* [] */0
              }
            }
          }
        }) && q.length === 4)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 38, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.take(q) !== 1) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 39, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!(Caml_obj.equal(to_list(q), {
          hd: 2,
          tl: {
            hd: 3,
            tl: {
              hd: 4,
              tl: /* [] */0
            }
          }
        }) && q.length === 3)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 40, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.take(q) !== 2) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 41, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!(Caml_obj.equal(to_list(q), {
          hd: 3,
          tl: {
            hd: 4,
            tl: /* [] */0
          }
        }) && q.length === 2)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 42, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.take(q) !== 3) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 43, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!(Caml_obj.equal(to_list(q), {
          hd: 4,
          tl: /* [] */0
        }) && q.length === 1)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 44, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.take(q) !== 4) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 45, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!(to_list(q) === /* [] */0 && q.length === 0)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 46, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!does_raise(Queue.take, q)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 47, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q$1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

Queue.add(1, q$1);

if (Queue.take(q$1) !== 1) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 53, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!does_raise(Queue.take, q$1)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 54, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(2, q$1);

if (Queue.take(q$1) !== 2) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 56, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!does_raise(Queue.take, q$1)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 57, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q$1.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 58, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q$2 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

Queue.add(1, q$2);

if (Queue.peek(q$2) !== 1) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 64, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(2, q$2);

if (Queue.peek(q$2) !== 1) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 66, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(3, q$2);

if (Queue.peek(q$2) !== 1) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 68, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.peek(q$2) !== 1) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 69, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.take(q$2) !== 1) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 70, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.peek(q$2) !== 2) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 71, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.take(q$2) !== 2) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 72, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.peek(q$2) !== 3) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 73, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (Queue.take(q$2) !== 3) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 74, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!does_raise(Queue.peek, q$2)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 75, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!does_raise(Queue.peek, q$2)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 76, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q$3 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i = 1; i <= 10; ++i){
  Queue.add(i, q$3);
}

Queue.clear(q$3);

if (q$3.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 85, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!does_raise(Queue.take, q$3)) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 86, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(q$3, {
        length: 0,
        first: "Nil",
        last: "Nil"
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 87, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.add(42, q$3);

if (Queue.take(q$3) !== 42) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 89, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$1 = 1; i$1 <= 10; ++i$1){
  Queue.add(i$1, q1);
}

var q2 = Queue.copy(q1);

if (!Caml_obj.equal(to_list(q1), {
        hd: 1,
        tl: {
          hd: 2,
          tl: {
            hd: 3,
            tl: {
              hd: 4,
              tl: {
                hd: 5,
                tl: {
                  hd: 6,
                  tl: {
                    hd: 7,
                    tl: {
                      hd: 8,
                      tl: {
                        hd: 9,
                        tl: {
                          hd: 10,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 98, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q2), {
        hd: 1,
        tl: {
          hd: 2,
          tl: {
            hd: 3,
            tl: {
              hd: 4,
              tl: {
                hd: 5,
                tl: {
                  hd: 6,
                  tl: {
                    hd: 7,
                    tl: {
                      hd: 8,
                      tl: {
                        hd: 9,
                        tl: {
                          hd: 10,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 99, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q1.length !== 10) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 100, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2.length !== 10) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 101, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

for(var i$2 = 1; i$2 <= 10; ++i$2){
  if (Queue.take(q1) !== i$2) {
    throw new Error("Assertion Failure. File: libqueue_test.res, Line: 103, Col: 4", {
              cause: {
                RE_EXN_ID: "Assertion_failure"
              }
            });
  }
  
}

for(var i$3 = 1; i$3 <= 10; ++i$3){
  if (Queue.take(q2) !== i$3) {
    throw new Error("Assertion Failure. File: libqueue_test.res, Line: 106, Col: 4", {
              cause: {
                RE_EXN_ID: "Assertion_failure"
              }
            });
  }
  
}

var q$4 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

if (q$4.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 112, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

for(var i$4 = 1; i$4 <= 10; ++i$4){
  Queue.add(i$4, q$4);
  if (q$4.length !== i$4) {
    throw new Error("Assertion Failure. File: libqueue_test.res, Line: 115, Col: 4", {
              cause: {
                RE_EXN_ID: "Assertion_failure"
              }
            });
  }
  if (q$4.length === 0) {
    throw new Error("Assertion Failure. File: libqueue_test.res, Line: 116, Col: 4", {
              cause: {
                RE_EXN_ID: "Assertion_failure"
              }
            });
  }
  
}

for(var i$5 = 10; i$5 >= 1; --i$5){
  if (q$4.length !== i$5) {
    throw new Error("Assertion Failure. File: libqueue_test.res, Line: 119, Col: 4", {
              cause: {
                RE_EXN_ID: "Assertion_failure"
              }
            });
  }
  if (q$4.length === 0) {
    throw new Error("Assertion Failure. File: libqueue_test.res, Line: 120, Col: 4", {
              cause: {
                RE_EXN_ID: "Assertion_failure"
              }
            });
  }
  Queue.take(q$4);
}

if (q$4.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 123, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q$4.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 124, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q$5 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$6 = 1; i$6 <= 10; ++i$6){
  Queue.add(i$6, q$5);
}

var i$7 = {
  contents: 1
};

Queue.iter((function (j) {
        if (i$7.contents !== j) {
          throw new Error("Assertion Failure. File: libqueue_test.res, Line: 134, Col: 4", {
                    cause: {
                      RE_EXN_ID: "Assertion_failure"
                    }
                  });
        }
        i$7.contents = i$7.contents + 1 | 0;
      }), q$5);

var q1$1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$1 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

if (q1$1.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 141, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q1$1) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 142, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$1.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 143, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q2$1) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 144, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.transfer(q1$1, q2$1);

if (q1$1.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 146, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q1$1) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 147, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$1.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 148, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q2$1) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 149, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q1$2 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$2 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$8 = 1; i$8 <= 4; ++i$8){
  Queue.add(i$8, q1$2);
}

if (q1$2.length !== 4) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 157, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q1$2), {
        hd: 1,
        tl: {
          hd: 2,
          tl: {
            hd: 3,
            tl: {
              hd: 4,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 158, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$2.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 159, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q2$2) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 160, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.transfer(q1$2, q2$2);

if (q1$2.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 162, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q1$2) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 163, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$2.length !== 4) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 164, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q2$2), {
        hd: 1,
        tl: {
          hd: 2,
          tl: {
            hd: 3,
            tl: {
              hd: 4,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 165, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q1$3 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$3 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$9 = 5; i$9 <= 8; ++i$9){
  Queue.add(i$9, q2$3);
}

if (q1$3.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 173, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q1$3) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 174, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$3.length !== 4) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 175, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q2$3), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 176, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.transfer(q1$3, q2$3);

if (q1$3.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 178, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q1$3) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 179, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$3.length !== 4) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 180, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q2$3), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 181, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

var q1$4 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

var q2$4 = {
  length: 0,
  first: "Nil",
  last: "Nil"
};

for(var i$10 = 1; i$10 <= 4; ++i$10){
  Queue.add(i$10, q1$4);
}

for(var i$11 = 5; i$11 <= 8; ++i$11){
  Queue.add(i$11, q2$4);
}

if (q1$4.length !== 4) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 192, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q1$4), {
        hd: 1,
        tl: {
          hd: 2,
          tl: {
            hd: 3,
            tl: {
              hd: 4,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 193, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$4.length !== 4) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 194, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q2$4), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: /* [] */0
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 195, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

Queue.transfer(q1$4, q2$4);

if (q1$4.length !== 0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 197, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (to_list(q1$4) !== /* [] */0) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 198, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (q2$4.length !== 8) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 199, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

if (!Caml_obj.equal(to_list(q2$4), {
        hd: 5,
        tl: {
          hd: 6,
          tl: {
            hd: 7,
            tl: {
              hd: 8,
              tl: {
                hd: 1,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 3,
                    tl: {
                      hd: 4,
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        }
      })) {
  throw new Error("Assertion Failure. File: libqueue_test.res, Line: 200, Col: 2", {
            cause: {
              RE_EXN_ID: "Assertion_failure"
            }
          });
}

console.log("OK");

exports.Q = Q;
exports.does_raise = does_raise;
/* q Not a pure module */
