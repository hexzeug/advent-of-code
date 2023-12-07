import os

ORDER = '23456789TJQKA'
HAND_TYPES = [
    [0, 1, 2, 3, 4],
    [0, 0, 1, 2, 3],
    [0, 0, 1, 1, 2],
    [0, 0, 0, 1, 2],
    [0, 0, 0, 1, 1],
    [0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0],
]

hands = []
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        hand, bid = l.split()
        bid = int(bid)
        ordered_hand = sorted(hand, key=lambda x: hand.count(x) * len(ORDER) + ORDER.index(x), reverse=True)
        normalized_ordered_hand = [0]
        for i, x in enumerate(ordered_hand[1:]):
            normalized_ordered_hand.append(normalized_ordered_hand[-1] + (ordered_hand[i] != x))
        hand_type = HAND_TYPES.index(normalized_ordered_hand)
        hand_value = hand_type * len(ORDER)**6
        for i in range(1, 6):
            hand_value += ORDER.index(hand[-i]) * len(ORDER)**i
        hands.append((hand_value, bid))

hands.sort()
print(sum(x[1] * (i + 1) for i, x in enumerate(hands)))