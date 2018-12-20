#!/usr/bin/env python

from parse import parse


ops = {
    'addr': lambda rs, a, b: rs[a] + rs[b],
    'addi': lambda rs, a, b: rs[a] + b,
    'mulr': lambda rs, a, b: rs[a] * rs[b],
    'muli': lambda rs, a, b: rs[a] * b,
    'banr': lambda rs, a, b: rs[a] & rs[b],
    'bani': lambda rs, a, b: rs[a] & b,
    'borr': lambda rs, a, b: rs[a] | rs[b],
    'bori': lambda rs, a, b: rs[a] | b,
    'setr': lambda rs, a, _: rs[a],
    'seti': lambda rs, a, _: a,
    'gtir': lambda rs, a, b: 1 if a > rs[b] else 0,
    'gtri': lambda rs, a, b: 1 if rs[a] > b else 0,
    'gtrr': lambda rs, a, b: 1 if rs[a] > rs[b] else 0,
    'eqir': lambda rs, a, b: 1 if a == rs[b] else 0,
    'eqri': lambda rs, a, b: 1 if rs[a] == b else 0,
    'eqrr': lambda rs, a, b: 1 if rs[a] == rs[b] else 0,
}

opcodes = {}  # map int -> op name, not yet deduced


def parseInstruction(line):
    return tuple(parse("{:d} {:d} {:d} {:d}\n", line))


def parseRegs(line):
    return tuple(parse("[{:d}, {:d}, {:d}, {:d}]\n", line))


def parseSamples(f):
    line = next(f)
    while line != '\n':
        assert line.startswith("Before: "), line
        before = parseRegs(line.split(None, 1)[1])
        instruction = parseInstruction(next(f))
        line = next(f)
        assert line.startswith("After:  "), line
        after = parseRegs(line.split(None, 1)[1])
        yield (before, instruction, after)
        assert next(f) == '\n'
        line = next(f)


def parseInstructions(f):
    for line in f:
        yield parseInstruction(line)


def parseInput(path):
    with open(path) as f:
        samples = list(parseSamples(f))
        assert next(f) == '\n'
        program = list(parseInstructions(f))
        return samples, program


def op(name, args, rs):
    a, b, c = args
    regs = list(rs)
    regs[c] = ops[name](rs, a, b)
    return tuple(regs)


def possibilities(samples):
    for before, (opcode, *args), after in samples:
        possibles = set()
        for name in ops.keys():
            if op(name, args, before) == after:
                possibles.add(name)
        yield opcode, possibles


def deduce_opcodes(possibilities):
    opcodes = {}  # opcode -> set(op names)

    # combine
    for opcode, possible_ops in possibilities:
        if opcode in opcodes:
            opcodes[opcode] &= possible_ops
        else:
            opcodes[opcode] = possible_ops

    # process of elimination
    determined = set()
    while opcodes:
        unsure = {}
        for opcode, names in opcodes.items():
            remaining = names - determined
            if len(remaining) == 1:
                name = remaining.pop()
                determined.add(name)
                yield opcode, name
            else:
                unsure[opcode] = remaining
        opcodes = unsure.copy()


if __name__ == '__main__':
    samples, program = parseInput('16.input')
    samples_tested = list(possibilities(samples))

    # Part 1
    print(sum(1 for _, possibles in samples_tested if len(possibles) >= 3))

    # Part 2
    opcodes = dict(deduce_opcodes(samples_tested))
    regs = (0, 0, 0, 0)
    for opcode, *args in program:
        regs = op(opcodes[opcode], args, regs)
    print(regs[0])
