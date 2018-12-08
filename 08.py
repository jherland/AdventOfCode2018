#!/usr/bin/env python

from __future__ import annotations
from dataclasses import dataclass
from typing import Iterator, List


@dataclass(frozen=True)
class Tree:
    children: List[Tree]
    metadata: List[int]

    @classmethod
    def build(cls, nums: Iterator[int]) -> Tree:
        c, m = next(nums), next(nums)
        children = [cls.build(nums) for _ in range(c)]
        metadata = [next(nums) for _ in range(m)]
        return cls(children, metadata)

    def traverse(self) -> Iterator[Tree]:
        yield self
        for c in self.children:
            yield from c.traverse()

    def value(self) -> int:
        if not self.children:
            return sum(self.metadata)
        else:
            ret = 0
            for i in self.metadata:
                if i > 0 and i <= len(self.children):
                    ret += self.children[i - 1].value()
            return ret


if __name__ == '__main__':
    with open('08.input') as f:
        nums = map(int, f.read().split())
    tree = Tree.build(iter(nums))

    # part 1
    print(sum(sum(t.metadata) for t in tree.traverse()))

    # part 2
    print(tree.value())
