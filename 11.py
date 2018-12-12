#!/usr/bin/env python

from dataclasses import dataclass
from itertools import takewhile


serial = int(open('11.input').read())


def power_level(coord):
    x, y = coord
    rackid = (x + 10)
    return (((rackid * y + serial) * rackid) // 100 % 10) - 5


@dataclass(frozen=True)
class Region:
    x: int
    y: int
    sz: int

    def __str__(self):
        return '{0.sz}x{0.sz} grid at ({0.x}, {0.y})'.format(self)

    def coords(self):
        return [(x, y)
                for y in range(self.y, self.y + self.sz)
                for x in range(self.x, self.x + self.sz)]

    def score(self):
        return sum(power_level(c) for c in self.coords())

    def regions_within(self, sz):
        assert sz < self.sz
        for y in range(self.y, self.y + self.sz - sz):
            for x in range(self.x, self.x + self.sz - sz):
                yield Region(x, y, sz)

    def max_within(self, sz):
        return max(self.regions_within(sz), key=Region.score)


if __name__ == '__main__':
    grid = Region(1, 1, 300)

    # part 1
    print(grid.max_within(3))

    # part 2
    candidates = takewhile(
        lambda r: r.score() > 0,
        (grid.max_within(sz) for sz in range(3, 300)))
    print(max(candidates, key=Region.score))
