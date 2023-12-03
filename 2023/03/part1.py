import os

schematic = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    schematic = [l[:-1] for l in f]

sum = 0   
for i, line in enumerate(schematic):
    j = 0
    while j < len(line):
        if (line[j].isdigit()):
            s = j
            while j < len(line) and line[j].isdigit(): j += 1
            s_x = max(s - 1, 0)
            s_y = max(i - 1, 0)
            e_x = min(j + 1, len(line))
            e_y = min(i + 2, len(schematic))
            if any(not x.isdigit() and x != '.' for l in schematic[s_y:e_y] for x in l[s_x:e_x]): sum += int(line[s:j])
        j += 1
print(sum)