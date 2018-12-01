#!/usr/bin/env python3

from itertools import cycle


def shifts():
    with open('01.input') as f:
        for line in f:
            yield int(line)


def shifted(shifts, start=0):
    yield start
    for s in shifts:
        start += s
        yield start


def first_dup(values):
    seen = set()
    for v in values:
        if v in seen:
            return v
        seen.add(v)


# part 1
print(sum(shifts()))

# part 2
print(first_dup(shifted(cycle(shifts()))))
