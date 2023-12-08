import os

ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

node_to_int = lambda node: sum(ALPHABET.index(node[-i]) * len(ALPHABET)**i for i in range(1, 4))

nodes = [None] * (len(ALPHABET)**4)
instructs = None

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    instructs = f.readline().rstrip()
    f.readline()
    for line in f:
        n, value = line.rstrip().split(' = ')
        l, r = value.strip('()').split(', ')
        n, l, r = node_to_int(n), node_to_int(l), node_to_int(r)
        nodes[n] = (l, r)

p = 0
i = 0
while p != node_to_int('ZZZ'):
    p = nodes[p][instructs[i % len(instructs)] == 'R']
    if p is None: raise RuntimeError()
    i += 1

print(i)
