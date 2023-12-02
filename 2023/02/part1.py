import os

CUBE_MAX = {
    'red': 12,
    'green': 13,
    'blue': 14,
}

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    sum = 0
    for line in f:
        line = line[:-1]
        game_id, game = line.split(': ', 1)
        id = int(game_id[5:])
        samples = [{y.split(' ')[1]: int(y.split(' ')[0]) for y in x.split(', ')} for x in game.split('; ')]
        for sample in samples:
            in_max = True
            for color in CUBE_MAX:
                if color in sample and sample[color] > CUBE_MAX[color]: in_max = False
            if not in_max: break
        else:
            sum += id
print(sum)