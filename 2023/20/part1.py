import os

class Module:
    def __init__(self, name, type, outputs):
        self.name = name
        self.type = type
        self.state = {'%': False, '&': dict()}.get(type)
        self.outputs = outputs
    
    def handle(self, pulse, sender):
        outs = len(self.outputs)
        if self.type == 'broadcaster':
            return list(zip(self.outputs, [pulse] * outs, [self.name] * outs))
        if self.type == '%' and pulse:
            self.state = not self.state
            return list(zip(self.outputs, [not self.state] * outs, [self.name] * outs))
        if self.type == '&':
            self.state[sender] = pulse
            return list(zip(self.outputs, [not any(self.state.values())] * outs, [self.name] * outs))
        return []
    
    def snapshot(self):
        if self.type == '&': return tuple(self.state.values())
        return self.state

modules = dict()
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    input_cache = dict()
    for l in f:
        module, outputs = l.strip().split(' -> ')
        outputs = outputs.split(', ')
        if module == 'broadcaster':
            type, name = module, module
        else:
            type = module[0]
            name = module[1:]
        modules[name] = Module(name, type, outputs)
        if type == '&' and name in input_cache:
            modules[name].state.update({x: True for x in input_cache[name]})
        for out in outputs:
            if out in modules and modules[out].type == '&':
                modules[out].state[name] = True
            else:
                if out not in input_cache: input_cache[out] = []
                input_cache[out].append(name)

snapshots = []
count_snapshots = []
pulse_counts = [0, 0]
for i in range(1000):
    snapshot = tuple(modules[x].snapshot() for x in modules)
    if snapshot in snapshots:
        j = snapshots.index(snapshot)
        print(i, 'same as', j)
        offset = count_snapshots[j]
        circle = [x - y for x, y in zip(pulse_counts, offset)]
        circle_overshoot = (1000 - i) % (i - j)
        circle_repeat = (1000 - i) // (i - j) + 1
        overshoot = [x - y for x, y in zip(count_snapshots[j + circle_overshoot], offset)]
        pulse_counts = [x + y * circle_repeat + z for x, y, z in zip(offset, circle, overshoot)]
        break
    snapshots.append(snapshot)
    count_snapshots.append(tuple(pulse_counts))
    q = [('broadcaster', True, None)]
    j = 0
    while j < len(q):
        target, pulse, sender = q[j]
        print(f"{sender} -{'low' if pulse else 'high'}-> {target}")
        pulse_counts[pulse] += 1
        if target in modules:
            q.extend(modules[target].handle(pulse, sender))
        j += 1
    print()

print(pulse_counts[0] * pulse_counts[1])