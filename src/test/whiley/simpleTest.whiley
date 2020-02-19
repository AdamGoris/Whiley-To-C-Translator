/*function sum(int[] items) -> (int r)
requires all {i in 0 .. |items| | items[i] >= 0}
ensures r >= 0:
	int i = 0
	r = 0
	while (i < |items|) where (i >= 0 && r >= 0):
		r = r + items[i]
		i = i + 1
    end
	return r
end

//int[] x

function indexOf(int[] items, int item) -> (int r):
    nat i = 0
    while i < |items| where all { k in 0 .. i | items[k] != item }:   
        if items[i] == item:
            return i
		end
        i = i + 1
	end
    return -1
end

function binarySearch(int[] items, int item) -> (bool result)
// The input list must be in sorted order
requires all { i in 0 .. |items|-1 | items[i] < items[i+1]}
// If return true, then matching item must exist in items
ensures result ==> some { i in 0..|items| | items[i] == item }
// If return false, then no matching item exists in items
ensures !result ==> all { i in 0..|items| | items[i] != item }:
    //
    int lo = 0
    int hi = |items|

    while lo < hi
        where 0 <= lo && hi <= |items| && lo <= hi
        where all { i in 0 .. lo | items[i] != item }
        where all { i in hi .. |items| | items[i] != item }:
        //
        // Note, the following is safe in Whiley because we have
        // unbounded integers.  If that wasn't the case, then this could
        // potentially overflow leading to a very subtle bug (like that
        // eventually found in the Java Standard Library).
        //
        int mid = (lo + hi) / 2

        if items[mid] < item:
            lo = mid + 1
        else if items[mid] > item:
            hi = mid
        else:
            return true
        end
    end
    return false
end


function count(int n) -> (int r)
ensures r == n || r == 0:
    //
    int i = 0
    //
    do:
        if n <= 0:
            break
        end
        i = i + 1
    while i < n where n > 0 && i >= 0 && i <= n
    //
    return i
end
*/

function selectPos(int[] xs) -> (int[] ys)
ensures |ys| <= |xs|
ensures all { i in 0..|ys| | ys[i] >= 0 }:
    int i = 0
    int size = |xs|
    int count = 0

    while i < |xs| 
        where i >= 0 && i <= |xs| && |xs| == size 
        where count >= 0 && count <= i:
        if xs[i] >= 0:
            count = count + 1
        end
        i = i + 1
    end

    int[] zs = [0; count]
    i = 0
    int j = 0
    while j < |zs|
        where i >= 0 && j >= 0 && j <= |zs| && |zs| == count
        where all { k in 0 .. j | zs[k] >= 0 }:
        if i < |xs| && xs[i] >= 0:
            zs[j] = xs[i]
            j = j + 1
        end
        i = i + 1
    end
    assert j >= |zs|
    return zs
end
