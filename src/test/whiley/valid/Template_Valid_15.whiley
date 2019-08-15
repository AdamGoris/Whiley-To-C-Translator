type LinkedList is null | { LinkedList next }

function recursive<T>(LinkedList l, T value) -> (T r)
ensures r == value:
    //
    if l is null:
        return value
    else:
        return recursive(l.next,value)

public export method test():
    LinkedList l1 = null
    LinkedList l2 = { next: l1 }
    LinkedList l3 = { next: l2 }
    //
    int x = recursive(l1,1)
    int y = recursive(l2,2)
    int z = recursive(l3,3)    
    //
    assert x == 1
    assert y == 2
    assert z == 3
    