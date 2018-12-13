#!/usr/bin/env python

from dataclasses import dataclass


Tracks = {}


@dataclass(frozen=True)
class Cart:
    y: int
    x: int
    direction: str
    next_turn: str = 'left'

    NextDir = {
        'E': {'-': 'E', '/': 'N', '\\': 'S'},
        'W': {'-': 'W', '/': 'S', '\\': 'N'},
        'N': {'|': 'N', '/': 'E', '\\': 'W'},
        'S': {'|': 'S', '/': 'W', '\\': 'E'},
    }
    AtCrossing = {
        'E': {'left': 'N', 'straight': 'E', 'right': 'S'},
        'W': {'left': 'S', 'straight': 'W', 'right': 'N'},
        'N': {'left': 'W', 'straight': 'N', 'right': 'E'},
        'S': {'left': 'E', 'straight': 'S', 'right': 'W'},
    }
    NextTurn = {'left': 'straight', 'straight': 'right', 'right': 'left'}

    @property
    def pos(self):
        return (self.y, self.x)

    @staticmethod
    def go(pos, direction):
        y, x = pos
        return {
            'N': (y - 1, x),
            'S': (y + 1, x),
            'W': (y, x - 1),
            'E': (y, x + 1),
        }[direction]

    def move(self):
        pos = self.go(self.pos, self.direction)
        segment = Tracks[pos]
        if segment == '+':
            direction = self.AtCrossing[self.direction][self.next_turn]
            next_turn = self.NextTurn[self.next_turn]
        else:
            direction = self.NextDir[self.direction][segment]
            next_turn = self.next_turn
        return Cart(*pos, direction, next_turn)


Alphabet = {
    '-': ('-', None),
    '|': ('|', None),
    '/': ('/', None),
    '\\': ('\\', None),
    '+': ('+', None),
    '>': ('-', 'E'),
    '<': ('-', 'W'),
    '^': ('|', 'N'),
    'v': ('|', 'S'),
    ' ': (None, None),
}


def parse(path):
    with open(path) as f:
        for y, line in enumerate(f):
            for x, c in enumerate(line.rstrip()):
                yield (y, x), Alphabet[c]


def build(tokens):
    tracks = {}  # (y, x) -> track segment
    carts = {}  # (y, x) -> Cart object
    for pos, (s, d) in tokens:
        if s is not None:
            tracks[pos] = s
        if d is not None:
            carts[pos] = Cart(*pos, d)
    return tracks, carts


def tick(carts):
    after, collisions = {}, set()
    carts = carts.copy()
    for pos in sorted(carts.keys()):
        try:
            c = carts.pop(pos)
        except KeyError:
            continue
        a = c.move()
        if a.pos in carts or a.pos in after:  # collision
            collisions.add(a.pos)
            carts.pop(a.pos, None)
            after.pop(a.pos, None)
        else:
            after[a.pos] = a
    return after, collisions


def tick_until_collision(carts):
    collisions = []
    while not collisions:
        carts, collisions = tick(carts)
    return carts, collisions


def tick_until_last_survivor(carts):
    while len(carts) > 1:
        carts, _ = tick_until_collision(carts)
    return carts.popitem()[1]


if __name__ == '__main__':
    Tracks, carts = build(parse('13.input'))

    # part 1
    _, collisions = tick_until_collision(carts)
    print('{1},{0}'.format(*collisions.pop()))

    # part 2
    survivor = tick_until_last_survivor(carts)
    print('{0.x},{0.y}'.format(survivor))
