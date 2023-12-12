import os
import re

row, groups, cache = None, None, None

def count_arrangements(row_offset=0, group_index=0):
    if (row_offset, group_index) in cache: return cache[(row_offset, group_index)]
    result = 0
    if group_index == len(groups):
        result += '#' not in row[row_offset:]
    else:
        group = groups[group_index]
        # moving window of length {group} over {row} starting at {row_offset}
        for i in range(row_offset, len(row) - group + 1):
            no_dot_inside = '.' not in row[i:i + group]
            no_hashtag_before = i == 0 or row[i - 1] != '#'
            no_hashtag_after = i + group == len(row) or row[i + group] != '#'
            if no_hashtag_before and no_dot_inside and no_hashtag_after: # group fits at window position
                result += count_arrangements(i + group + 1, group_index + 1)
            if row[i] == '#': break # stop moving window if '#' forces to place a group
    cache[(row_offset, group_index)] = result
    return result

s = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for line in f:
        cache = dict()
        row, groups = line.rstrip().split()
        row = list('?'.join([re.sub(r"\.{2,}", '.', row)] * 5).strip('.'))
        groups = list(map(int, groups.split(','))) * 5
        c = count_arrangements()
        print(''.join(row), groups, c)
        s += c

print(s)

            