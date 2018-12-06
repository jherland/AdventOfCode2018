#!/usr/bin/env python

from collections import namedtuple
from itertools import chain


Point = namedtuple('Point', ['x', 'y'])


def parse():
    with open("06.input") as f:
        for line in f:
            x, y = line.split(', ')
            yield Point(int(x), int(y))


def bounding(points):
    '''Bounding box from a set of points.'''
    xs, ys = zip(*points)
    return Point(min(xs), min(ys)), Point(max(xs), max(ys))


def mhdist(a, b):
    '''Manhattan distance between two points.'''
    return abs(a.x - b.x) + abs(a.y - b.y)


def nearest(p, coords):
    '''Return coord c nearest to p, or None if no one c is nearest.'''
    distances = sorted((mhdist(p, c), c) for c in coords)
    if distances[0][0] == distances[1][0]:  # Multiple nearest points
        return None
    return distances[0][1]


def near_edge(p, box, coords):
    '''True iff p is nearer any edge of the box than any c in coords.'''
    topleft, bottomright = box
    p_at_edges = [
        Point(p.x, topleft.y), Point(topleft.x, p.y),
        Point(p.x, bottomright.y), Point(bottomright.x, p.y),
    ]
    return any(nearest(e, coords) == p for e in p_at_edges)


def circle(c, r):
    '''Yield points on the circle/diamond centered at c with radius r.'''
    if r > 0:
        xs = chain(range(r), range(r, -r, -1), range(-r, 0))
        ys = chain(range(r, -r, -1), range(-r, r))
        for x, y in zip(xs, ys):
            yield Point(c.x + x, c.y + y)
    else:
        yield c


def nearea(c, coords):
    '''Return #points that are nearer c than any other c in coords.'''
    assert c in coords
    a, da, r = 0, -1, 0
    while da:
        da = len([n for n in circle(c, r) if nearest(n, coords) == c])
        a += da
        r += 1
    return a


def center(coords):
    return Point(
        sum(c.x for c in coords) // len(coords),
        sum(c.y for c in coords) // len(coords))


def dist_to_all(p, coords):
    '''Sum distance from p to all coords.'''
    return sum(mhdist(p, c) for c in coords)


def within_region(p, coords):
    return dist_to_all(p, coords) < 10000


if __name__ == '__main__':
    coords = set(parse())
    box = bounding(coords)

    # part 1
    outer = set(filter(lambda c: near_edge(c, box, coords), coords))
    inner = coords - outer
    print(max(nearea(c, coords) for c in inner))

    # part 2
    # find a point somewhere within the region and expand from there until the
    # region no longer grows.
    start = center(coords)
    r = 0
    while True:
        for p in circle(start, r):
            if within_region(p, coords):
                start = p
                break
        else:
            r += 1
            continue
        break
    assert within_region(start, coords)

    a, r = 0, 0
    while True:
        da = sum(within_region(p, coords) for p in circle(start, r))
        if not da:
            break
        a += da
        r += 1
    print(a)
