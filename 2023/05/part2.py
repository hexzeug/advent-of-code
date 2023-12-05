import os

class Range:
    def __init__(self, start, length):
        self.start = start
        self.length = length
        self.end = start + length - 1
    
    def __contains__(self, other: 'Range'):
        return other.start <= self.end and other.end >= self.start 

    def mask(self, range: 'Range'):
        new_start = max(self.start, range.start)
        new_end = min(self.end, range.end)
        return Range(new_start, new_end - new_start + 1)
    
    def slice(self, range: 'Range'):
        mask = self.mask(range)
        front = Range(self.start, mask.start - self.start)
        back = Range(mask.end + 1, self.end - mask.end)
        return front, mask, back
    
    def empty(self):
        return self.length == 0
    
    def __len__(self):
        return self.length
    
    def __add__(self, other: int):
        if not isinstance(other, int): raise TypeError('can only add integers to Range')
        return Range(self.start + other, self.length)
    
    def __eq__(self, other: 'Range'):
        return self.start == other.start and self.length == other.length

    def __lt__(self, other: 'Range'):
        return self.start < other.start
    
    def __gt__(self, other: 'Range'):
        return self.start > other.start
    
    def __repr__(self):
        return f"{self.start}..{self.end}({self.length})"
    
    def __str__(self):
        return self.__repr__()
    
    def __hash__(self):
        return self.start

data = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    data = f.read().rstrip().split('\n\n')

seed_desc = list(map(int, data[0].split(': ')[1].split()))
seeds = []
for i in range(len(seed_desc) // 2):
    seeds.append(Range(seed_desc[i*2], seed_desc[i*2+1]))
seeds.sort()

for block in data[1:]:
    old_seeds, seeds = seeds, []
    for m in block.split('\n')[1:]:
        d, s, l = map(int, m.split())
        mask = Range(s, l)
        new_old_seeds = []
        for r in old_seeds:
            if mask in r:
                f, source, b = r.slice(mask)
                # print(r, '/', mask, '=', f, source, b)
                if not f.empty(): new_old_seeds.append(f)
                if not source.empty(): seeds.append(source + (d - s))
                if not b.empty(): new_old_seeds.append(b)
            else:
                new_old_seeds.append(r)
        old_seeds = new_old_seeds
    seeds.extend(old_seeds)
    seeds.sort()
    i = 1
    while i < len(seeds):
        prev, curr = seeds[i-1:i+1]
        if prev.end + 1 == curr.start:
            seeds.pop(i)
            seeds[i - 1] = Range(prev.start, prev.length + curr.length)
        else:
            i += 1
    # print(old_seeds, '->', seeds)

print(seeds[0].start)
        