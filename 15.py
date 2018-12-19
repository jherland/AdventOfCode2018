#!/usr/bin/env python

from io import StringIO
import sys


Debug = False


def debug(s):
    if Debug:
        print(s)


def bounds(coords):
    xmin = min(x for _, x in coords)
    xmax = max(x for _, x in coords)
    ymin = min(y for y, _ in coords)
    ymax = max(y for y, _ in coords)
    return (ymin, xmin), (ymax, xmax)


class Unit:
    def __init__(self, side, pos, attack_power):
        self.side = side
        self.pos = pos
        self.hp = 200
        self.ap = attack_power

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
            try:
                move_to = self.next_move(game, in_range)
            except IndexError:
                debug('  no path to enemies')
                return False
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

    def next_move(self, game, in_range):
        '''Find the adjacent square that brings us closest to the enemy.'''
        target, d = game.nearest(self.pos, in_range)
        debug('  approaching {} from a distance of {}'.format(target, d))
        neighbour, _ = game.nearest(target, game.adjacent(self.pos))
        debug('  via {}'.format(neighbour))
        return neighbour

    def move(self, dst):
        '''Move to the given location.'''
        self.pos = dst

    def attack(self, victim):
        '''Attack the given unit. Return True iff the victim is killed.'''
        victim.hp -= self.ap
        return victim.hp <= 0


class Game:
    @classmethod
    def parse(cls, path, elf_attack=3, goblin_attack=3):
        space = set()  # Non-wall spaces
        units = {}  # pos -> Unit instance
        with open(path) as f:
            for y, line in enumerate(f):
                for x, c in enumerate(line.strip()):
                    pos = (y, x)
                    if c != '#':
                        space.add(pos)
                        if c == 'E':
                            units[pos] = Unit('E', pos, elf_attack)
                        elif c == 'G':
                            units[pos] = Unit('G', pos, goblin_attack)
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

    def nearest(self, pos, points, available=None):
        '''Return (p, distance(pos, p)) for the point p nearest to pos.'''
        def dist_then_coord(item):
            p, d = item
            return (d, p)

        d_from_pos = self.distance_from(pos, available)
        point_ds = {p: d for p, d in d_from_pos.items() if p in points}
        return sorted(point_ds.items(), key=dist_then_coord)[0]

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
    # part 1
    game = Game.parse('15.input', elf_attack=3)
    rounds = game.until_combat_end()
    debug(game.render())
    print((rounds - 1) * sum(u.hp for u in game.units.values()))

    # part 2
    for elf_attack in range(4, sys.maxsize):
        debug('Elves attacking with {}'.format(elf_attack))
        game = Game.parse('15.input', elf_attack=elf_attack)
        before = len([u for u in game.units.values() if u.side == 'E'])
        rounds = game.until_combat_end()
        after = len([u for u in game.units.values() if u.side == 'E'])
        debug(game.render())
        if before > after:
            debug('{} elves died in the making of this combat'.format(
                before - after))
        else:
            debug('All elves survived!')
            print((rounds - 1) * sum(u.hp for u in game.units.values()))
            break
