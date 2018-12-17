#!/usr/bin/env python

from io import StringIO
import sys


Debug = True


def debug(s):
    if Debug:
        print(s)


def bounds(coords):
    xmin = min(x for _, x in coords)
    xmax = max(x for _, x in coords)
    ymin = min(y for y, _ in coords)
    ymax = max(y for y, _ in coords)
    return (ymin, xmin), (ymax, xmax)


def mhdist(a, b):
    ay, ax = a
    by, bx = b
    return abs(by - ay) + abs(bx - ax)


class Unit:
    def __init__(self, side, pos):
        self.side = side
        self.pos = pos
        self.hp = 200
        self.ap = 3

    def __str__(self):
        return '{}/{}@{}'.format(self.side, self.hp, self.pos)

    def turn(self, game):
        debug('{}\'s turn:'.format(self))
        if self.hp <= 0:
            debug('  already dead...')
            return False
        enemies = self.enemies(game)
        if not enemies:
            debug('  no enemies')
            return True  # Combat ends
        in_range = self.in_range(game)
        if not in_range:
            debug('  no squares in range')
            return False
        elif self.pos not in in_range:
            move_to = self.find_path(game, in_range)
            if move_to is None:
                debug('  no path to enemies')
                return False
            debug('  moving to {}'.format(move_to))
            self.move(move_to)

        if self.pos not in in_range:
            debug('  no enemies in range')
            return False

        suspects = {e for e in enemies if self.pos in game.adjacent(e.pos)}
        debug('  in range of enemies: {}'.format(
            ', '.join(str(s) for s in suspects)))
        victim = sorted(suspects, key=lambda s: (s.hp, s.pos))[0]
        debug('  attacking {}'.format(victim))
        if self.attack(victim):
            debug('  {} has died!'.format(victim))
            game.kill(victim)
        return False

    def enemies(self, game):
        '''Return all remaining enemies in current game.'''
        return {u for u in game.units.values() if u.side != self.side}

    def in_range(self, game):
        '''Return spaces that are in range of any enemy.'''
        return {p for e in self.enemies(game) for p in game.adjacent(e.pos)}

    def find_path(self, game, in_range):
        '''Determine the shortest path to any square in range of an enemy.'''
        debug('  searching for paths to enemies...')
        min_dist, move_to = sys.maxsize, None
        adjacents = game.adjacent(self.pos)  # move_to candidates
        for dst in sorted(in_range, key=lambda p: mhdist(self.pos, p)):
            dists = game.distance_from(dst)
            d, a = sorted(
                (dists.get(adj, sys.maxsize), adj) for adj in adjacents)[0]
            if d < min_dist:
                min_dist, move_to = d, a
        if move_to:
            debug('  approach via {}, which is {} from in-range'.format(
                move_to, min_dist))
            assert move_to in game.adjacent(self.pos)
        return move_to

    def move(self, dst):
        '''Move to the given location.'''
        self.pos = dst

    def attack(self, victim):
        '''Attack the given unit. Return True iff the victim is killed.'''
        victim.hp -= self.ap
        return victim.hp <= 0


class Game:
    @classmethod
    def parse(cls, path):
        space = set()  # Non-wall spaces
        units = {}  # pos -> Unit instance
        with open(path) as f:
            for y, line in enumerate(f):
                for x, c in enumerate(line.strip()):
                    pos = (y, x)
                    if c != '#':
                        space.add(pos)
                        if c in {'E', 'G'}:
                            units[pos] = Unit(c, pos)
        return cls(space, units)

    def __init__(self, space, units):
        self.space = space
        self.units = units

    def render(self):
        (ymin, xmin), (ymax, xmax) = bounds(self.space)
        out = StringIO()
        for y in range(ymin - 1, ymax + 2):
            units = []
            for x in range(xmin - 1, xmax + 2):
                pos = (y, x)
                if pos in self.units:
                    out.write(self.units[pos].side)
                    units.append(self.units[pos])
                elif pos in self.space:
                    out.write('.')
                else:
                    out.write('#')
            out.write('  ')
            out.write(', '.join(str(u) for u in units))
            out.write('\n')
        return out.getvalue()

    def around(self, coord):
        '''Return coords around the given coord, in reading order.'''
        y, x = coord
        return [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

    def available(self):
        '''Return spaces that are not currently occupied.'''
        return {pos for pos in self.space if pos not in self.units}

    def adjacent(self, pos, available=None):
        '''Return any available pos adjacent to the given pos.'''
        if available is None:
            available = self.available()
        return {p for p in self.around(pos) if p in available}

    def distance_from(self, pos, available=None):
        '''Return map: p -> dist(p, pos) for p in available.'''
        if available is None:
            available = self.available()
        ret = {pos: 0}  # p -> distance from p to pos
        available.discard(pos)
        adjacents = self.adjacent(pos, available)
        available -= adjacents
        queue = list(adjacents)
        while queue:
            p = queue.pop(0)
            dist = min(ret.get(a, sys.maxsize) for a in self.around(p)) + 1
            ret[p] = dist
            adjacents = self.adjacent(p, available)
            available -= adjacents
            queue.extend(adjacents)
        return ret

    def kill(self, unit):
        del self.units[unit.pos]

    def round(self):
        '''Do one complete round, i.e. give each unit one turn.'''
        for pos, unit in sorted(self.units.items()):
            if unit.hp <= 0:  # already dead
                continue
            del self.units[pos]  # Lift unit from board
            ends = unit.turn(self)
            self.units[unit.pos] = unit  # Replace unit onto board
            if ends:
                return True
        return False

    def until_combat_end(self):
        rounds, end = 0, False
        debug(self.render())
        while not end:
            debug('Round #{}'.format(rounds + 1))
            end = self.round()
            debug(self.render())
            rounds += 1
        return rounds


if __name__ == '__main__':
    game = Game.parse(sys.argv[1])

    #  print(game.render())
    #  game.round()
    #  print(game.render())

    # part 1
    rounds = game.until_combat_end()
    print((rounds - 1) * sum(u.hp for u in game.units.values()))
