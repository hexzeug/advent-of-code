import os

sum = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for line in f:
        digits = [x for x in line if x.isdigit()]
        sum += int(digits[0] + digits[-1])
print(sum)