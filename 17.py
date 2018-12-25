#!/usr/bin/env python

from parse import parse


def debug(s):
    pass  # print(s)


def parse_coord_lines(f):
    for line in f:  # x=344, y=1749..1767
        c1, i1, c2, i2l, i2h = parse('{:.1w}={:d}, {:.1w}={:d}..{:d}\n', line)
        assert c1 != c2 and c1 in 'xy' and c2 in 'xy'
        i1, i2l, i2h = int(i1), int(i2l), int(i2h)
        assert i2l <= i2h
        for i2 in range(i2l, i2h + 1):
            if c1 == 'y':
                yield (i1, i2)
            else:
                yield (i2, i1)


def bounding_box(points):
    ys, xs = zip(*points)
    ymin, xmin = min(ys), min(xs)
    ymax, xmax = max(ys), max(xs)
    return (ymin, xmin), (ymax, xmax)


def inside(point, bounds):
    y, x = point
    (ymin, xmin), (ymax, xmax) = bounds
    return ymin <= y <= ymax and xmin <= x <= xmax


def up(p):
    y, x = p
    return y - 1, x


def down(p):
    y, x = p
    return y + 1, x


def left(p):
    y, x = p
    return y, x - 1


def right(p):
    y, x = p
    return y, x + 1


class Reservoir:
    @classmethod
    def parse(cls, f):
        return cls(set(parse_coord_lines(f)))

    def __init__(self, clay, spring=(0, 500)):
        self.clay = clay
        self.spring = spring
        (ymin, xmin), (ymax, xmax) = bounding_box(clay)
        self.bounds = (ymin, xmin - 1), (ymax, xmax + 1)
        self.wflow = set()
        self.wpool = set()

    def render(self):
        (ymin, xmin), (ymax, xmax) = self.bounds
        lines = []
        for y in range(ymin, ymax + 1):
            line = []
            for x in range(xmin, xmax + 1):
                if (y, x) in self.wpool:
                    line.append('~')
                elif (y, x) in self.wflow:
                    line.append('|')
                elif (y, x) in self.clay:
                    line.append('#')
                elif (y, x) == self.spring:
                    line.append('+')
                else:
                    line.append(' ')
            lines.append(''.join(line))
        return '\n'.join(lines)

    def flow(self):
        queue = [((self.bounds[0][0], self.spring[1]), False)]
        while queue:
            cur, rwd = queue.pop()
            if not inside(cur, self.bounds):  # out of bounds
                continue
            elif cur in self.wpool:  # already found still water here
                continue
            if not rwd:  # forward flow
                self.wflow.add(cur)
                s, w, e = down(cur), left(cur), right(cur)
                if s not in self.clay | self.wpool:  # no 'floor'
                    if s not in self.wflow:
                        debug('{}: vvv -> {}'.format(cur, s))
                        queue.append((s, False))
                else:  # clay or pool below, spread sideways
                    # leftwards
                    if w in self.clay:  # run into wall
                        debug('{}: |<< -> {}!'.format(cur, cur))
                        queue.append((cur, True))  # back up
                    elif w not in self.wflow:  # can flow here
                        debug('{}: <<< -> {}'.format(cur, w))
                        queue.append((w, False))
                    # rightwards
                    if e in self.clay:
                        debug('{}: >>| -> {}!'.format(cur, cur))
                        queue.append((cur, True))  # back up
                    elif e not in self.wflow:  # can flow here
                        debug('{}: >>> -> {}'.format(cur, e))
                        queue.append((e, False))
            else:  # reverse flow
                assert cur in self.wflow  # forward flow was already here
                pool = {cur}
                w, e = left(cur), right(cur)
                while w in self.wflow:
                    pool.add(w)
                    w = left(w)
                while e in self.wflow:
                    pool.add(e)
                    e = right(e)
                if w in self.clay and e in self.clay:  # walls around pool
                    debug('Found pool between {} and {}'.format(w, e))
                    for p in pool:
                        self.wpool.add(p)
                        n = up(p)
                        if n in self.wflow:
                            debug('Found source at {}'.format(n))
                            queue.append((n, False))  # rise to level above


if __name__ == '__main__':
    with open('17.input') as f:
        reservoir = Reservoir.parse(f)
    reservoir.flow()
    print(reservoir.render())

    # Part 1
    print(len(reservoir.wflow))

    # Part 2
    print(len(reservoir.wpool))
