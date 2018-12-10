#!/usr/bin/env python3

import string


def reactive(a, b):
    return a != b and a.upper() == b.upper()


def react(chars):
    ret = []
    for c in chars:
        if ret and reactive(ret[-1], c):
            ret.pop()
        else:
            ret.append(c)
    return ret


def remove(x, chars):
    return filter(lambda c: c not in {x.lower(), x.upper()}, chars)


if __name__ == '__main__':
    with open('05.input') as f:
        polymer = list(f.read().strip())

    # part 1
    print(len(react(polymer)))

    # part 2
    print(min(len(react(remove(x, polymer))) for x in string.ascii_lowercase))
