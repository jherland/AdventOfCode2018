#!/usr/bin/env python

from io import StringIO


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


def shortest(lists):
    ret = []
    for l in lists:
        if not ret or len(l) < len(ret[0]):
            ret = [l]
        elif len(l) == len(ret[0]):
            ret.append(l)
    return ret


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
            debug('  searching for paths...')
            paths = sorted(shortest(
                [p for dst in in_range for p in game.paths(self.pos, dst)]))
            if not paths:
                debug('  no path to enemies')
                return False
            debug('  paths: {}'.format(paths))
            move_to = paths[0][0]
            assert move_to in game.adjacent(self.pos)
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
            del game.units[victim.pos]
        return False

    def in_range(self, game):
        '''Return spaces that are in range of any enemy.'''
        return {p for e in self.enemies(game) for p in game.adjacent(e.pos)}

    def enemies(self, game):
        '''Return all remaining enemies in current game.'''
        return {u for u in game.units.values() if u.side != self.side}

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
        return [p for p in self.around(pos) if p in available]

    def paths(self, a, b):
        '''Yield paths from a to b using only available space.'''
        completed = []

        def go(cur, available, in_progress):
            if cur == a:  # done!
                debug('Found {} -> {}: {}'.format(a, b, in_progress))
                completed.append(in_progress)
                return
            elif completed and len(in_progress) >= len(completed[0]):
                return  # too long
            in_progress = [cur] + in_progress
            available = available - {cur}
            for adj in self.adjacent(cur, available):
                go(adj, available, in_progress)

        go(b, self.available(), [])
        return completed

    def round(self):
        '''Do one complete round, i.e. give each unit one turn.'''
        for pos, unit in sorted(self.units.items()):
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
    game = Game.parse('15.input')

    print(game.render())
    game.round()
    print(game.render())

    # part 1
    #  rounds = game.until_combat_end()
    #  print((rounds - 1) * sum(u.hp for u in game.units.values()))
