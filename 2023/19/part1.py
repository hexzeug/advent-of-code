import os

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    wfs = dict()
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
    s = 0
    for l in f:
        l = l.strip()
        part = [int(x[2:]) for x in l[1:-1].split(',')]
        wf = 'in'
        while wf != 'A' and wf != 'R':
            for act, cat, comp, val in wfs[wf]:
                if cat is None or comp and part[cat] < val or not comp and part[cat] > val:
                    wf = act
                    break
        if wf == 'A': s += sum(part)
    print(s)