#!/usr/bin/env python

from parse import parse


def parse_lights():
    with open('10.input') as f:
        for line in f:
            px, py, vx, vy = parse(
                'position=<{:6d}, {:6d}> velocity=<{:2d}, {:2d}>', line)
            yield (px, py), (vx, vy)


def bounds(frame):
    xmin = min(px for (px, _), _ in frame)
    xmax = max(px for (px, _), _ in frame)
    ymin = min(py for (_, py), _ in frame)
    ymax = max(py for (_, py), _ in frame)
    return (xmin, ymin), (xmax, ymax)


def size(bounds):
    (xmin, ymin), (xmax, ymax) = bounds
    return (xmax - xmin) * (ymax - ymin)


def step(frame):
    def step_one(light):
        (px, py), (vx, vy) = light
        return (px + vx, py + vy), (vx, vy)

    return set(step_one(l) for l in frame)


def play(frame):
    while True:
        frame = step(frame)
        yield frame


def congregate(start):
    gen, low, last = 0, size(bounds(start)), start
    for frame in play(start):
        sz = size(bounds(frame))
        if sz > low:
            return gen, low, last
        gen, low, last = gen + 1, sz, frame


def render(frame):
    points = set((px, py) for (px, py), _ in frame)

    def pixel(x, y):
        return '#' if (x, y) in points else ' '

    ((xmin, ymin), (xmax, ymax)) = bounds(frame)
    for y in range(ymin, ymax + 1):
        for x in range(xmin, xmax + 1):
            print(pixel(x, y), end='')
        print()


if __name__ == '__main__':
    start = set(parse_lights())
    gen, sz, frame = congregate(start)

    # part 1
    render(frame)

    # part 2
    print(gen)
