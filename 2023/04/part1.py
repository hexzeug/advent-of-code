import os

sum = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        cards = l.split(': ')[1]
        win, my = cards.split(' | ')
        win = set(win.split())
        my = set(my.split())
        points = win.intersection(my)
        if len(points) > 0: sum += 2**(len(points) - 1)
print(sum)