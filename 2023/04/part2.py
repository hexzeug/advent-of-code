import os
from queue import PriorityQueue

q = PriorityQueue()
sum = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for i, l in enumerate(f):
        sum += 1
        cards = l.split(': ')[1]
        win, my = cards.split(' | ')
        win = set(win.split())
        my = set(my.split())
        points = len(win.intersection(my))
        while True:
            for j in range(points): q.put(i + j + 1)
            if q.qsize() == 0 or q.queue[0] != i: break
            sum += 1
            q.get()
print(sum)