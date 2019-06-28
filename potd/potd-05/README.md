POTD 5 --- Mergesort
==============================

Your work is to implement Mergesort, which is an efficient divide and conquer algorithm.

Recall the steps of Mergesort, when given an input list of length `n`:

1. Divide the list into two halves, each of size roughly `n/2`. The dividing process ends when we have split the subsequences down to a single element, since lists of length one are trivially sorted.
2. Sort each subsequence by calling Mergesort recursively on each.
3. Merge the sorted subsequences into a single sorted list.

You will be implementing the `mergesort` function, along with two helper functions, `split` and `merge`.

Given a list, `split` will iterate through the elements of the list, assigning each element to one of two output lists in alternating order. The output of the function is a tuple of these lists. (The order of the lists inside this tuple does not matter.)

```
split [] == ([],[])
split [1] == ([1],[])
split [7,5] == ([7],[5])

-- both of these are considered correct!
split [6,3,9,2,5,7,1] == ([6,9,5,1],[3,2,7])	
split [6,3,9,2,5,7,1] == ([3,2,7],[6,9,5,1])
```

Given two input lists, `merge` will combine these two lists by repeatedly
taking the smallest of the initial elements of the two lists.  If the two
initial elements are equal, you should take the initial element from the first
list.  (If both lists happen to be sorted, the output will also be sorted.)

```
merge [] [3,5,7] == [3,5,7]
merge [1,3,7,8] [2,4,5,6] == [1,2,3,4,5,6,7,8]
merge [8,3,2,9] [2,4,9,6] == [2,4,8,3,2,9,6,9]
```

Given an unsorted list, `mergesort` will return the list in sorted order.

```
mergesort [8,4,3,6,2,5,1,7,9] == [1,2,3,4,5,6,7,8,9]
```

Put the code for these functions in `src/Lib.hs`.
