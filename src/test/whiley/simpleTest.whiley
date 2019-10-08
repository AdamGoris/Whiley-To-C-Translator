function abs(int x) -> (int y)
ensures y >= 0:
    if x < 0:
        x = -x
    return x

