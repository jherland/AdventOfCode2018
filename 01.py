#!/usr/bin/env python3

from itertools import cycle

from utils import first_dup


def shifted(shifts, start=0):
    yield start
    for s in shifts:
        start += s
        yield start


if __name__ == '__main__':
    with open('01.input') as f:
        shifts = [int(line) for line in f]

    # part 1
    print(sum(shifts))

    # part 2
    print(first_dup(shifted(cycle(shifts))))
