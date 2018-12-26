#!/usr/bin/env python

from collections import Counter
from io import StringIO
from itertools import islice


def bounds(coords):
    ymin = min(y for y, _ in coords)
    ymax = max(y for y, _ in coords)
    xmin = min(x for _, x in coords)
    xmax = max(x for _, x in coords)
    return (ymin, xmin), (ymax, xmax)


def adjacent(coord):
    y, x = coord
    return [
        (y - 1, x - 1), (y - 1, x), (y - 1, x + 1),
        (y,     x - 1),             (y,     x + 1),
        (y + 1, x - 1), (y + 1, x), (y + 1, x + 1)]


def around(area, coord):
    return (area[c] for c in adjacent(coord) if c in area)


def develop(area, coord):
    tally = Counter(around(area, coord))
    return {
        '.': '|' if tally['|'] >= 3 else '.',
        '|': '#' if tally['#'] >= 3 else '|',
        '#': '#' if tally['#'] >= 1 and tally['|'] >= 1 else '.'
    }[area[coord]]


def minute(area):
    return {pos: develop(area, pos) for pos in area}


def iterate(a, func):
    while True:
        yield a
        a = func(a)


def parse(path):
    with open(path) as f:
        for y, line in enumerate(f):
            for x, c in enumerate(line.strip()):
                yield (y, x), c


def render(area):
    (ymin, xmin), (ymax, xmax) = bounds(area)
    out = StringIO()
    for y in range(ymin, ymax + 1):
        for x in range(xmin, xmax + 1):
            out.write(area[(y, x)])
        out.write('\n')
    return out.getvalue()


def resource_value(area):
    tally = Counter(area.values())
    return tally['|'] * tally['#']


def find_repeats(items, key=lambda it: it):
    stats = {}
    for i, it in enumerate(items):
        k = key(it)
        if k in stats:
            return stats[k], i - stats[k]
        else:
            stats[k] = i


if __name__ == '__main__':
    area = dict(parse('18.input'))

    # part 1
    after10 = next(islice(iterate(area, minute), 10, 11))
    print(resource_value(after10))

    # part 2
    start, period = find_repeats(iterate(area, minute), render)
    i = (1000000000 - start) % period + start
    print(resource_value(next(islice(iterate(area, minute), i, i + 1))))
