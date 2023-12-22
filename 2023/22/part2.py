import os

ABC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'

class Brick:
    def __init__(self, name, x1, y1, z1, x2, y2, z2):
        self.name = name
        self.x1 = x1
        self.y1 = y1
        self.z1 = z1
        self.x2 = x2
        self.y2 = y2
        self.z2 = z2
        self.bot = set()
        self.top = set()
    
    def z(self, z):
        self.z2 -= self.z1 - z
        self.z1 = z
    
    def __hash__(self):
        return hash(self.name)
    
    def __repr__(self):
        return ABC[self.name % len(ABC)]
    
    def __str__(self):
        return self.__repr__()

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    max_x, max_y, max_z = 0, 0, 0
    bricks = []
    for i, l in enumerate(f):
        [x1, x2], [y1, y2], [z1, z2] = [sorted(x) for x in zip(*(map(int, x.split(',')) for x in l.strip().split('~')))]
        max_x, max_y, max_z = max(max_x, x2), max(max_y, y2), max(max_z, z2)
        bricks.append(Brick(i, x1, y1, z1, x2, y2, z2))
    bricks.sort(key=lambda x: x.z1)

    bot_view = [[[] for _ in range(max_x + 1)] for _ in range(max_y + 1)]
    for b in bricks:
        z = 0
        for x in range(b.x1, b.x2 + 1):
            for y in range(b.y1, b.y2 + 1):
                if len(bot_view[y][x]):
                    c = bot_view[y][x][-1]
                    if c.z2 > z:
                        z = c.z2
                        for r in b.bot: r.top.remove(b)
                        b.bot = set()
                    if c.z2 == z:
                        c.top.add(b)
                        b.bot.add(c)
                bot_view[y][x].append(b)
        b.z(z + 1)
    
    def dis(b):
        fell = set()
        def rec(bs, fell):
            if len(bs) == 0: return 0
            fell.update(bs)
            fall = {c for b in bs for c in b.top if c.bot.issubset(fell)}
            rec(fall, fell)
        rec({b}, fell)
        return len(fell) - 1
        
    print(sum(map(dis, bricks)))
    