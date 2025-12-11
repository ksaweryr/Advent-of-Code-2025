from dataclasses import dataclass
from functools import reduce
from z3 import *


@dataclass
class Problem:
    target: list[int]
    switches: list[list[int]]
    joltages: list[int]

    def switches_matrix(self):
        num_lights = len(self.target)
        return [[int(i in s) for i in range(num_lights)] for s in self.switches]


def parse_problem(s):
    [target, *switches, joltages] = s.strip().split()
    target = [int(c == '#') for c in target[1:-1]]
    switches = [[int(x) for x in s[1:-1].split(',')] for s in switches]
    joltages = [int(x) for x in joltages[1:-1].split(',')]
    return Problem(target, switches, joltages)


def part1_single(problem):
    sm = problem.switches_matrix()
    ns = len(problem.switches)

    for hi in range(ns):
        s = Solver()
        vs = [Int(f'v_{i}') for i in range(ns)]

        for v in vs:
            s.add(v >= 0, v <= 1)

        for i, t in enumerate(problem.target):
            s.add(reduce(lambda acc, p: acc + (p[0] * p[1]), zip(vs, [sm[j][i] for j in range(ns)]), 0) % 2 == t)
        s.add(sum(vs) == hi)
        if s.check() == sat:
            return hi


def part1(problems):
    return [part1_single(p) for p in problems]


def main():
    with open('input/day_10.txt', 'rt') as f:
        problems = [parse_problem(line) for line in f]
    print(sum(part1(problems)))
    # p = parse_problem()


if __name__ == '__main__':
    main()
