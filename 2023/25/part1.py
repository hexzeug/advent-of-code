import os
from sys import setrecursionlimit
from itertools import count
from collections import defaultdict, deque
from time import time as second

setrecursionlimit(10_000)

adj = []
edg = []
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    itr = count()
    nodes = defaultdict(lambda: next(itr))
    for l in f:
        v_str, u_strs = l.strip().split(': ')
        v = nodes[v_str]
        for _ in range(len(adj), v + 1): adj.append([])
        for u_str in u_strs.split():
            u = nodes[u_str]
            for _ in range(len(adj), u + 1): adj.append([])
            adj[v].append(u)
            adj[u].append(v)
            edg.append(tuple(sorted((v, u))))

def dfs(u, vis, par, low, disc, time, res):
    vis[u] = True
    disc[u] = time
    low[u] = time
    time += 1
    for v in adj[u]:
        if not vis[v]:
            par[v] = u
            dfs(v, vis, par, low, disc, time, res)
            low[u] = min(low[u], low[v])
            if low[v] > disc[u]:
                res.append(tuple(sorted((u, v))))
        elif v != par[u]:
            low[u] = min(low[u], disc[v])

cnt = 0
for i, [e11, e12] in enumerate(edg[:-1]):
    adj[e11].remove(e12)
    adj[e12].remove(e11)
    start = second()
    for e21, e22 in edg[i+1:]:
        cnt += 1
        adj[e21].remove(e22)
        adj[e22].remove(e21)
        res = []
        dfs(0, [False] * len(adj), [-1] * len(adj), [float('Inf')] * len(adj), [float('Inf')] * len(adj), 0, res)
        if len(res) > 0:
            print((e11, e12), (e21, e22), *res)
            e31, e32 = res[0]
            adj[e31].remove(e32)
            adj[e32].remove(e31)
            break
        adj[e21].append(e22)
        adj[e22].append(e21)
    else:
        print(f"{cnt}/{len(edg) * (len(edg) - 1) // 2} ({round((second() - start) * 10_000)/10}ms)")
        adj[e11].append(e12)
        adj[e12].append(e11)
        continue
    break

q = deque((e11,))
vis = [False] * len(adj)
vis[e11] = True
while len(q) > 0:
    v = q.popleft()
    for u in adj[v]:
        if not vis[u]:
            vis[u] = True
            q.append(u)

print(vis.count(True) * vis.count(False))
