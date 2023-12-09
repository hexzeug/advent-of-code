import os

sum = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        history = [list(map(int, l.rstrip().split()))]
        while any(x != 0 for x in history[-1]):
            history.append([history[-1][i+1] - x for i, x in enumerate(history[-1][:-1])])
        for i in range(len(history) - 1, 0, -1):
            history[i-1].insert(0, history[i-1][0] - history[i][0])
        sum += history[0][0]
print(sum)