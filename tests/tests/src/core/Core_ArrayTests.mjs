// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "./Test.mjs";
import * as Stdlib_List from "rescript/lib/es6/Stdlib_List.js";
import * as Stdlib_Array from "rescript/lib/es6/Stdlib_Array.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";

let eq = Primitive_object.equal;

Test.run([
  [
    "Core_ArrayTests.res",
    3,
    20,
    26
  ],
  "make"
], Stdlib_Array.make(6, 7), eq, [
  7,
  7,
  7,
  7,
  7,
  7
]);

Test.run([
  [
    "Core_ArrayTests.res",
    5,
    20,
    42
  ],
  "getUnsafe - existing"
], 1, eq, 1);

Test.run([
  [
    "Core_ArrayTests.res",
    6,
    20,
    41
  ],
  "getUnsafe - missing"
], [
    0,
    1,
    2
  ][10], eq, undefined);

Test.run([
  [
    "Core_ArrayTests.res",
    9,
    13,
    30
  ],
  "fromInitializer"
], Stdlib_Array.fromInitializer(7, i => i + 3 | 0), eq, [
  3,
  4,
  5,
  6,
  7,
  8,
  9
]);

Test.run([
  [
    "Core_ArrayTests.res",
    15,
    20,
    28
  ],
  "reduce"
], Stdlib_Array.reduce([
  1,
  2,
  3
], /* [] */0, Stdlib_List.add), eq, {
  hd: 3,
  tl: {
    hd: 2,
    tl: {
      hd: 1,
      tl: /* [] */0
    }
  }
});

Test.run([
  [
    "Core_ArrayTests.res",
    16,
    20,
    36
  ],
  "reduce - empty"
], Stdlib_Array.reduce([], /* [] */0, Stdlib_List.add), eq, /* [] */0);

Test.run([
  [
    "Core_ArrayTests.res",
    19,
    13,
    30
  ],
  "reduceWithIndex"
], Stdlib_Array.reduceWithIndex([
  1,
  2,
  3
], /* [] */0, (acc, v, i) => ({
  hd: v + i | 0,
  tl: acc
})), eq, {
  hd: 5,
  tl: {
    hd: 3,
    tl: {
      hd: 1,
      tl: /* [] */0
    }
  }
});

Test.run([
  [
    "Core_ArrayTests.res",
    25,
    13,
    38
  ],
  "reduceWithIndex - empty"
], Stdlib_Array.reduceWithIndex([], /* [] */0, (acc, v, i) => ({
  hd: v + i | 0,
  tl: acc
})), eq, /* [] */0);

Test.run([
  [
    "Core_ArrayTests.res",
    32,
    13,
    26
  ],
  "reduceRight"
], Stdlib_Array.reduceRight([
  1,
  2,
  3
], /* [] */0, Stdlib_List.add), eq, {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: /* [] */0
    }
  }
});

Test.run([
  [
    "Core_ArrayTests.res",
    37,
    20,
    41
  ],
  "reduceRight - empty"
], Stdlib_Array.reduceRight([], /* [] */0, Stdlib_List.add), eq, /* [] */0);

Test.run([
  [
    "Core_ArrayTests.res",
    40,
    13,
    35
  ],
  "reduceEightWithIndex"
], Stdlib_Array.reduceRightWithIndex([
  1,
  2,
  3
], /* [] */0, (acc, v, i) => ({
  hd: v + i | 0,
  tl: acc
})), eq, {
  hd: 1,
  tl: {
    hd: 3,
    tl: {
      hd: 5,
      tl: /* [] */0
    }
  }
});

Test.run([
  [
    "Core_ArrayTests.res",
    46,
    13,
    38
  ],
  "reduceWithIndex - empty"
], Stdlib_Array.reduceRightWithIndex([], /* [] */0, (acc, v, i) => ({
  hd: v + i | 0,
  tl: acc
})), eq, /* [] */0);

Test.run([
  [
    "Core_ArrayTests.res",
    52,
    20,
    41
  ],
  "toShuffled - length"
], Stdlib_Array.toShuffled([
  1,
  2,
  3
]).length, eq, 3);

let arr = [
  1,
  2,
  3
];

Test.run([
  [
    "Core_ArrayTests.res",
    55,
    13,
    31
  ],
  "shuffle - length"
], (Stdlib_Array.shuffle(arr), arr.length), eq, 3);

Test.run([
  [
    "Core_ArrayTests.res",
    66,
    13,
    24
  ],
  "filterMap"
], Stdlib_Array.filterMap([
  1,
  2,
  3,
  4,
  5,
  6
], n => {
  if (n % 2 === 0) {
    return Math.imul(n, n);
  }
  
}), eq, [
  4,
  16,
  36
]);

Test.run([
  [
    "Core_ArrayTests.res",
    71,
    20,
    42
  ],
  "filterMap - no match"
], Stdlib_Array.filterMap([
  1,
  2,
  3,
  4,
  5,
  6
], param => {}), eq, []);

Test.run([
  [
    "Core_ArrayTests.res",
    73,
    13,
    32
  ],
  "filterMap - empty"
], Stdlib_Array.filterMap([], n => {
  if (n % 2 === 0) {
    return Math.imul(n, n);
  }
  
}), eq, []);

Test.run([
  [
    "Core_ArrayTests.res",
    79,
    20,
    30
  ],
  "keepSome"
], Stdlib_Array.keepSome([
  1,
  undefined,
  3
]), eq, [
  1,
  3
]);

Test.run([
  [
    "Core_ArrayTests.res",
    81,
    13,
    34
  ],
  "keepSome - all Some"
], Stdlib_Array.keepSome([
  1,
  2,
  3
]), eq, [
  1,
  2,
  3
]);

Test.run([
  [
    "Core_ArrayTests.res",
    86,
    20,
    41
  ],
  "keepSome - all None"
], Stdlib_Array.keepSome([
  undefined,
  undefined,
  undefined
]), eq, []);

Test.run([
  [
    "Core_ArrayTests.res",
    87,
    20,
    38
  ],
  "keepSome - empty"
], Stdlib_Array.keepSome([]), eq, []);

Test.run([
  [
    "Core_ArrayTests.res",
    90,
    13,
    22
  ],
  "findMap"
], Stdlib_Array.findMap([
  1,
  2,
  3,
  4,
  5,
  6
], n => {
  if (n % 2 === 0) {
    return n - 8 | 0;
  }
  
}), eq, -6);

Test.run([
  [
    "Core_ArrayTests.res",
    95,
    20,
    40
  ],
  "findMap - no match"
], Stdlib_Array.findMap([
  1,
  2,
  3,
  4,
  5,
  6
], param => {}), eq, undefined);

Test.run([
  [
    "Core_ArrayTests.res",
    97,
    13,
    30
  ],
  "findMap - empty"
], Stdlib_Array.findMap([], n => {
  if (n % 2 === 0) {
    return Math.imul(n, n);
  }
  
}), eq, undefined);

Test.run([
  [
    "Core_ArrayTests.res",
    104,
    13,
    27
  ],
  "fromIterator"
], Array.from(new Map([
  [
    1,
    3
  ],
  [
    2,
    4
  ]
]).values()), eq, [
  3,
  4
]);

Test.run([
  [
    "Core_ArrayTests.res",
    110,
    20,
    39
  ],
  "last - with items"
], Stdlib_Array.last([
  1,
  2,
  3
]), eq, 3);

Test.run([
  [
    "Core_ArrayTests.res",
    111,
    20,
    34
  ],
  "last - empty"
], Stdlib_Array.last([]), eq, undefined);

let array = [];

array.splice(1, 0, "foo");

Test.run([
  [
    "Core_ArrayTests.res",
    116,
    22,
    49
  ],
  "splice - Insert no delete"
], array, eq, ["foo"]);

let array$1 = [
  "bar",
  "baz"
];

Test.run([
  [
    "Core_ArrayTests.res",
    122,
    15,
    43
  ],
  "splice - Insert and delete"
], [
  (array$1.splice(1, 1, "foo"), undefined),
  array$1
], eq, [
  undefined,
  [
    "bar",
    "foo"
  ]
]);

export {
  eq,
}
/*  Not a pure module */
