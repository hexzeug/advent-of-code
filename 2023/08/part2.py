import os
from math import lcm

ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

node_to_int = lambda node: sum(ALPHABET.index(node[-i]) * len(ALPHABET)**i for i in range(1, 4))

nodes = [None] * (len(ALPHABET)**4)
instructs = None
pointer = []

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    instructs = f.readline().rstrip()
    f.readline()
    for line in f:
        n, value = line.rstrip().split(' = ')
        l, r = value.strip('()').split(', ')
        i = node_to_int(n)
        nodes[i] = {'L': node_to_int(l), 'R': node_to_int(r), 'Z': n[2] == 'Z'}
        if n[2] == 'A': pointer.append({'node': i, 'offset': 0, 'circle': 0, 'Z': []})
        
found = []
i = 0
while len(pointer) > 0:
    for p in pointer[:]:
        if nodes[p['node']]['Z']:
            if len(p['Z']) > 0 and p['Z'][0][0] == p['node']:
                pointer.remove(p)
                found.append(p)
                continue
            p['Z'].append((p['node'], p['circle']))
        p['node'] = nodes[p['node']][instructs[i % len(instructs)]]
        if len(p['Z']) == 0: p['offset'] += 1
        else: p['circle'] += 1
    i += 1

for x in found:
    print(x)

print(lcm(*(x['offset'] for x in found)))