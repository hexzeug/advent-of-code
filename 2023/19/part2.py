import os
from functools import cache, reduce

wfs = dict()

@cache
def accept(wf, win_min=(1, 1, 1, 1), win_max=(4000, 4000, 4000, 4000)):
    if wf == 'R': return 0
    if wf == 'A':
        return reduce(lambda x, y: x * y, [y - x + 1 for x, y in zip(win_min, win_max)])
    win_min, win_max = list(win_min), list(win_max)
    s = 0
    for act, cat, comp, val in wfs[wf]:
        if cat is None:
            s += accept(act, win_min=tuple(win_min), win_max=tuple(win_max))
        elif comp:
            win = win_max[:]
            win[cat] = val - 1
            if win[cat] >= win_min[cat]:
                s += accept(act, win_min=tuple(win_min), win_max=tuple(win))
            if val > win_max[cat]: break
            win_min[cat] = val
        else:
            win = win_min[:]
            win[cat] = val + 1
            if win[cat] <= win_max[cat]:
                s += accept(act, win_min=tuple(win), win_max=tuple(win_max))
            if val < win_min[cat]: break
            win_max[cat] = val
    return s

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        l = l.strip()
        if len(l) == 0: break
        name, wf_read = l[:-1].split('{')
        wf_read = wf_read.split(',')
        wf = []
        for wf_part in wf_read[:-1]:
            cat, comp, *_ = wf_part
            val, act = wf_part[2:].split(':')
            cat = 'xmas'.index(cat)
            comp = comp == '<'
            val = int(val)
            wf.append((act, cat, comp, val))
        wf.append((wf_read[-1], None, None, None))
        wfs[name] = wf

print(accept('in'))