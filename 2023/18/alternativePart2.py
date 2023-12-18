import os
import numpy as np

USE_FAKE = False

points = [(1, 1)]
x, y = 1, 1
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        fr, fd, hex = l.split()
        r = hex[-2]
        d = int(hex[2:-2], base=16)
        if USE_FAKE:
            r = {'R': '0', 'D': '1', 'L': '2', 'U': '3'}[fr]
            d = int(fd)
        if r == '0':
            x += d
        elif r == '2':
            x -= d
        elif r == '1':
            y += d
        elif r == '3':
            y -= d
        points.append((x, y))

a = 0
r = 0
# points = points[::-1]
print(points)
for i, p in enumerate(points[:-1]):
    q = points[i + 1]
    r += abs(p[0] - q[0]) + abs(p[1] - q[1])
    a += np.linalg.det(np.array(((p[0], q[0]), (p[1], q[1]))))
a /= 2

print(a, r)
print(round(a + r/2 + 1))



