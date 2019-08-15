function id<T>(T x) -> (T r)
ensures x == r:
    //
    return x

public export method test():
    //
    assert id<int>(1) == 1
    assert id<bool>(false) == false
    assert id<int[]>([1,2,3]) == [1,2,3]
    