Interval Sum II
================
Liwen Huang
8/27/2018

Description Given an integer array in the construct method, implement two methods query(start, end) and modify(index, value):

For query(start, end), return the sum from index start to index end in the given array. For modify(index, value), modify the number in the given index to value

Example Given array A = \[1,2,7,8,5\].

query(0, 2), return 10. modify(0, 4), change A\[0\] from 1 to 4. query(0, 1), return 6. modify(2, 1), change A\[2\] from 7 to 1. query(2, 4), return 14.

This is a good problem to practice writing nested function and use function as an object within another funciton.

``` r
#the two nested function we need 
query<-function(A, start, end){
  return (sum(A[start:end]))
}

modify<-function(A, index, value){
A[index]=value
return (A)
}

array_sum<-function(A, FUN, a, b){
##use FUN as a placeholder for the nested function
result=FUN(A, a, b)
return(result)
}
```

Test our function

``` r
A = c(1,2,7,8,5)
array_sum(A, FUN=query, 1, 3)
```

    ## [1] 10

``` r
array_sum(A, FUN=modify, 1, 4)
```

    ## [1] 4 2 7 8 5
