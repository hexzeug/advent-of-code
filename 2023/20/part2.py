import os
from math import lcm

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

before_rx = None
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
            if out == 'rx':
                before_rx = name

i = 0
dings = {}
while True:
    q = [('broadcaster', True, None)]
    j = 0
    if len(dings) == len(modules[before_rx].state): break
    while j < len(q):
        target, pulse, sender = q[j]
        if target == before_rx and not pulse and sender not in dings:
            dings[sender] = i + 1
        if target in modules:
            q.extend(modules[target].handle(pulse, sender))
        j += 1
    i += 1

print(lcm(*dings.values()))