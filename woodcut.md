Woodcut
================
Liwen Huang
8/27/2018

Description Given n pieces of wood with length L\[i\] (integer array). Cut them into small pieces to guarantee you could have equal or more than k pieces with the same length. What is the longest length you can get from the n pieces of wood? Given L & k, return the maximum length of the small pieces.

You couldn't cut wood into float length.

If you couldn't get &gt;= k pieces, return 0.

Have you met this question in a real interview?
Example For L=\[232, 124, 456\], k=7, return 114.

This is a pretty easy one. First, the maxmimum length would have to be smaller or at least equal to the minimal length of all the woods in the pile. We would start with that. And if by using that length, we could not get k pieces, we can adjust the length until we do get k pieces.

``` r
woodcut<-function(l, k) {
max_length=min(l)
while ( sum(as.integer(l/max_length))<k ) {
  max_length=max_length-1
}
return (max_length)
}
```

Test our function

``` r
l=c(232, 124, 456); k=7
woodcut(l,k)
```

    ## [1] 114
