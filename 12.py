#!/usr/bin/env python

from dataclasses import dataclass
from itertools import islice
from parse import parse


Rules = {}


@dataclass(frozen=True)
class Plants:
    gen: int
    offset: int
    pots: str

    @classmethod
    def parse(cls, line):
        pots, = parse('initial state: {}', line)
        assert all(c in '.#' for c in pots)
        return cls(0, 0, pots).trim()

    def __str__(self):
        return '{}/{}/{}'.format(self.gen, self.offset, self.pots)

    def score(self):
        return sum(i for i, p in enumerate(self.pots, self.offset) if p == '#')

    def trim(self):
        first = self.pots.index('#')
        pots = self.pots[first:].rstrip('.')
        return Plants(self.gen, self.offset + first, pots)

    def sliding_window(self):
        s = '....' + self.pots + '....'
        for i in range(0, len(s) - 4):
            yield s[i:i + 5]

    def next(self):
        pots = ''.join(Rules[w] for w in self.sliding_window())
        return Plants(self.gen + 1, self.offset - 2, pots).trim()

    def nexts(self):
        while True:
            yield self
            self = self.next()

    def stabilize(self):
        '''Iterate on .next() until self.pots no longer changes.'''
        prev = Plants(0, 0, '')
        while prev.pots != self.pots:
            prev = self
            self = self.next()
        return prev, self.offset - prev.offset

    def ffwd(self, gen):
        self, d_offset = self.stabilize()
        assert self.gen <= gen
        offset = self.offset + (gen - self.gen) * d_offset
        return Plants(gen, offset, self.pots)


if __name__ == '__main__':
    with open('12.input') as f:
        lines = iter(f)
        plants = Plants.parse(next(lines).strip())
        assert not next(lines).strip()
        for line in lines:
            src, dst = parse('{:.5W} => {:.1W}', line.strip())
            assert all(c in '.#' for c in src + dst)
            Rules[src] = dst

    # part 1
    gen20, = islice(plants.nexts(), 20, 21)
    print(gen20.score())

    # part 2
    genX = plants.ffwd(50 * 1000 * 1000 * 1000)
    print(genX.score())
