import os

MAP = {
    'one': '1',
    'two': '2',
    'three': '3',
    'four': '4',
    'five': '5',
    'six': '6',
    'seven': '7',
    'eight': '8',
    'nine': '9',
}

sum = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for line in f:
        first = len(line), None
        last = -1, None
        for key in MAP:
            i = line.find(key)
            if (i != -1 and i < first[0]): first = i, MAP[key]
            i = line.rfind(key)
            if (i > last[0]): last = i, MAP[key]
        digits = [x for x in enumerate(line) if x[1].isdigit()]
        if len(digits) > 0:
            if digits[0][0] < first[0]: first = digits[0]
            if digits[-1][0] > last[0]: last = digits[-1]
        if first[1] is None or last[1] is None: continue
        sum += int(first[1] + last[1])
print(sum)