// Test final static variable
final int CONSTANT = 0

function id(int x) -> (int r)
ensures r == x:
    //
    return x + CONSTANT

public export method test():
    assert id(-1) == -1
    assert id(0) == 0
    assert id(1) == 1