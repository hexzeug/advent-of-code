import os
from sys import setrecursionlimit

def edges(v):
    i, j = v
    if i == 0: return [(i + 1, j)]
    if i + 1 == len(graph): return [(i - 1, j)]
    return [(di, dj) for s, di, dj in [('^', i - 1, j), ('>', i, j + 1), ('v', i + 1, j), ('<', i, j - 1)] if graph[i][j] in [s, '.'] and graph[di][dj] in [s, '.']]

def dfs(v, d, r, p=[]):
    if v in p: return
    if v == d:
        r.append(p[:])
        return
    p.append(v)
    for u in edges(v): dfs(u, d, r, p)
    p.pop()

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    graph = [list(l.strip()) for l in f]

setrecursionlimit(len(graph) * len(graph[0]))

start = (0, graph[0].index('.'))
end = (len(graph) - 1, graph[-1].index('.'))
res = []
dfs(start, end, res)
print(max(map(len, res)))
