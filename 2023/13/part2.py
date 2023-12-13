import os

def difference(a, b):
    if isinstance(a[0], list): return(sum(difference(a, b) for a, b in zip(a, b)))
    return sum(a != b for a, b in zip(a, b))

blocks = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    blocks = [list(map(list, x.strip().split('\n'))) for x in f.read().split('\n\n')]

s = 0
for block_rows in blocks:
    block_cols = list(map(list, zip(*block_rows)))
    for comp_idx, block, multiplier in [(0, block_rows, 100), (len(block_rows) - 1, block_rows, 100), (0, block_cols, 1), (len(block_cols) - 1, block_cols, 1)]:
        comp_stack = block[comp_idx]
        for i, stack in enumerate(block):
            if i == comp_idx: continue
            mirror_len = abs(comp_idx - i) + 1
            if mirror_len % 2 != 0 or difference(comp_stack, stack) > 1: continue
            mirror = min(i, comp_idx) + mirror_len // 2
            if difference(block[min(i, comp_idx):mirror], block[max(comp_idx, i):mirror-1:-1]) == 1:
                s += mirror * multiplier
                break
        else: continue
        break

print(s)