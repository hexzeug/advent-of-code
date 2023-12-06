import os

product = 1
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for t, r in zip(*map(lambda x: map(int, x.rstrip().split(': ')[1].split()), f.readlines()[:2])):
        x = t // 2
        y = t // 2 + (t & 0x1)
        d = 0
        while (x - d) * (y + d) > r:
            d += 1
        product *= d * 2 - 1 + (t & 0x1)
print(product)
