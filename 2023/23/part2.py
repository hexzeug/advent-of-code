import os
from functools import cache

@cache
def edges(v):
    i, j = v
    if graph[i][j] == '#': return []
    if i == 0: return [(i + 1, j)]
    if i + 1 == len(graph): return [(i - 1, j)]
    return [(di, dj) for di, dj in [(i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1)] if graph[di][dj] != '#']

def dfs(v, d, r, p=[], pl=0):
    if v in p: return
    if v == d:
        r.append(pl)
        return
    p.append(v)
    for u, w in adj[v]: dfs(u, d, r, p, pl + w)
    p.pop()

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    graph = [list(l.strip()) for l in f]

start = (0, graph[0].index('.'))
end = (len(graph) - 1, graph[-1].index('.'))

vertices = [(i, j) for i, row in enumerate(graph) for j, _ in enumerate(row) if len(edges((i, j))) > 2 or (i, j) in [start, end]]
adj = []

for v in vertices:
    adj.append([])
    for u in edges(v):
        p = [v, u]
        while p[-1] not in vertices:
            for e in edges(p[-1]):
                if e != p[-2]:
                    p.append(e)
                    break
        adj[-1].append((vertices.index(p[-1]), len(p) - 1))
        
print(vertices)
print()
print(adj)

res = []
dfs(0, len(vertices) - 1, res)
print(max(res))
