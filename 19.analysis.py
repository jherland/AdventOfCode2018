#!/usr/bin/env python

import sys

a = int(sys.argv[1])
assert a in {0, 1}

c = 10551403 if a else 1003

d = 1
while True:
    for f in range(1, c + 1):
        if d * f == c:
            a += d
    d += 1
    if d > c:
        print(a)
        sys.exit(0)
