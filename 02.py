#!/usr/bin/env python3

from collections import Counter

from utils import first_dup


def removeNthChar(n, words):
    for word in words:
        yield word[:n] + word[n + 1:]


with open('02.input') as f:
    ids = [line.rstrip() for line in f.readlines()]

# part 1
idsWDoubles = len([i for i in ids if 2 in Counter(i).values()])
idsWTriples = len([i for i in ids if 3 in Counter(i).values()])
print(idsWDoubles * idsWTriples)

# part 2
for n in range(len(ids[0])):
    dup = first_dup(removeNthChar(n, ids))
    if dup is not None:
        print(dup)
        break
