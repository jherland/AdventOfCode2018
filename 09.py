#!/usr/bin/env python

from collections import deque
from itertools import cycle
from parse import parse
import sys


class Game:
    def __init__(self, players):
        self.scores = dict.fromkeys(range(1, players + 1), 0)
        self.d = deque([0])

    def __str__(self):
        return ' '.join([str(m) for m in self.d]) + ' | ' + str(self.scores)

    def place(self, marble):
        self.d.rotate(2)
        self.d.append(marble)

    def remove(self):
        self.d.rotate(-7)
        return self.d.pop()

    def turn(self, player, marble):
        if marble % 23:
            self.place(marble)
        else:
            score = marble + self.remove()
            self.scores[player] += score

    def play(self, marbles):
        players = cycle(sorted(self.scores.keys()))
        for p, m in zip(players, range(1, marbles + 1)):
            self.turn(p, m)

    def highscore(self):
        return max(self.scores.values())


if __name__ == '__main__':
    with open("09.input") as f:
        nplayers, hmarble = parse(
                '{:d} players; last marble is worth {:d} points\n', f.read())
    if len(sys.argv) == 3:
        nplayers, hmarble = int(sys.argv[1]), int(sys.argv[2])

    # part 1
    game = Game(nplayers)
    game.play(hmarble)
    print(game.highscore())

    # part 2
    game = Game(nplayers)
    game.play(hmarble * 100)
    print(game.highscore())
