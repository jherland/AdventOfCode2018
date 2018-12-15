#!/usr/bin/env python

from itertools import islice


def digits(n):
    ds = []
    while n > 0:
        n, d = divmod(n, 10)
        ds.insert(0, d)
    return ds if ds else [0]


def advance(recipes, elves):
    return [(i + 1 + recipes[i]) % len(recipes) for i in elves]


def experiment(recipes, elves):
    recipes = recipes[:]
    yield from recipes
    while True:
        current = [recipes[i] for i in elves]
        combined = digits(sum(current))
        recipes.extend(combined)
        yield from combined
        elves = advance(recipes, elves)


def window(size, it):
    win = list(islice(it, size))
    while True:
        yield win
        del win[0]
        win.append(next(it))


if __name__ == '__main__':
    s = open('14.input').read().strip()

    # part 1
    recipes = list(islice(experiment([3, 7], [0, 1]), int(s) + 10))
    print(''.join(str(r) for r in recipes[-10:]))

    # part 2
    pattern = [int(d) for d in s]
    n = 0
    for win in window(len(pattern), experiment([3, 7], [0, 1])):
        if win == pattern:
            print(n)
            break
        n += 1
