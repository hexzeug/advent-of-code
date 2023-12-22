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

def print_side_views():
    xz = [['.'] * (max_x + 1) for _ in range(max_z)]
    yz = [['.'] * (max_y + 1) for _ in range(max_z)]
    for b in sorted(sorted(bricks, key=lambda x: x.x1), key=lambda x: x.y1):
        for z in range(b.z1 - 1, b.z2):
            for x in range(b.x1, b.x2 + 1):
                if xz[z][x] == '.': xz[z][x] = ABC[hash(b) % len(ABC)]
            for y in range(b.y1, b.y2 + 1):
                if yz[z][y] == '.': yz[z][y] = ABC[hash(b) % len(ABC)]
    print('x\n' + '\n'.join(''.join(x) + f" {max_z - i}" for i, x in enumerate(reversed(xz))))
    print('-' * (max_x + 1), 0)
    print('y\n' + '\n'.join(''.join(x) + f" {max_z - i}" for i, x in enumerate(reversed(yz))))
    print('-' * (max_y + 1), 0)


with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    max_x, max_y, max_z = 0, 0, 0
    bricks = []
    for i, l in enumerate(f):
        [x1, x2], [y1, y2], [z1, z2] = [sorted(x) for x in zip(*(map(int, x.split(',')) for x in l.strip().split('~')))]
        max_x, max_y, max_z = max(max_x, x2), max(max_y, y2), max(max_z, z2)
        bricks.append(Brick(i, x1, y1, z1, x2, y2, z2))
    bricks.sort(key=lambda x: x.z1)

    print_side_views()

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

    print_side_views()
    
    print(len([None for b in bricks if not any([len(c.bot) == 1 for c in b.top])]))
    
        