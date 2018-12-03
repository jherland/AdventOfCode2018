#!/usr/bin/env python3

from dataclasses import dataclass


@dataclass(frozen=True)
class Claim:
    id: int
    x: int
    y: int
    w: int
    h: int

    @classmethod
    def parse(cls, s):
        id, at, loc, size = s.split()
        assert id.startswith('#')
        assert at == '@'
        assert loc.endswith(':')
        x, y = loc[:-1].split(',')
        w, h = size.split('x')
        return cls(int(id[1:]), int(x), int(y), int(w), int(h))

    def coords(self):
        for i in range(self.x, self.x + self.w):
            for j in range(self.y, self.y + self.h):
                yield (i, j)


def parse_claims(path):
    with open(path) as f:
        for line in f:
            yield Claim.parse(line)


def map_assignments(claims):
    ret = {}  # map coords to claims: (x, y) -> [claim]
    for claim in claims:
        for coord in claim.coords():
            ret.setdefault(coord, set()).add(claim)
    return ret


claims = set(parse_claims('03.input'))
assignments = map_assignments(claims)
contested = {k: v for k, v in assignments.items() if len(v) > 1}

# part 1
print(len(contested))

# part 2
contested_claims = {claim for claims in contested.values() for claim in claims}
uncontested_claims = claims - contested_claims
assert len(uncontested_claims) == 1
print(uncontested_claims.pop().id)
