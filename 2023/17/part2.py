import os

graph = []
data = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    data = [list(l.strip()) for l in f]
    graph = [[] for x in data for _ in x]
    width = len(data[0])
    height = len(data)
    for i, l in enumerate(data):
        for j, w in enumerate(l):
            w = int(w)
            v = i * width + j
            if i > 0: graph[(i - 1) * width + j].append((v, w, 3))
            if i + 1 < height: graph[(i + 1) * width + j].append((v, w, 1))
            if j > 0: graph[i * width + j - 1].append((v, w, 0))
            if j + 1 < width: graph[i * width + j + 1].append((v, w, 2))

K = 10
q = [[] for _ in range(K)]
dv = 0
dist = dict()

q[0].append((0, 0, 0))

while len(q[dv % K]) > 0:
    v, rv, lv = q[dv % K].pop()
    if v == len(graph) - 1: break
    for u, w, ru in graph[v]:
        lu = 1
        if ru == rv:
            if lv == 10: continue
            lu = lv + 1
        elif ru & 1 == rv & 1 or lv < 4: continue
        du = dv + w
        if (u, ru, lu) in dist:
            old_du = dist[(u, ru, lu)]
            if old_du <= du: continue
            else: q[old_du % K].remove((u, ru, lu))
        q[du % K].append((u, ru, lu))
        dist[(u, ru, lu)] = du
    for _ in range(K):
        if len(q[dv % K]) > 0: break
        dv += 1

print(dv)