import os

COLORS = ['red', 'green', 'blue']

sum = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for line in f:
        line = line[:-1]
        _, game = line.split(': ', 1)
        samples = [{y.split(' ')[1]: int(y.split(' ')[0]) for y in x.split(', ')} for x in game.split('; ')]
        product = 1
        for color in COLORS:
            product *= max([x[color] for x in samples if color in x], default=0)
        sum += product
            
print(sum)