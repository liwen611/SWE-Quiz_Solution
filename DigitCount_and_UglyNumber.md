Digit Count and Ugly Number
================
Liwen Huang
8/25/2018

Digit Count
===========

Description Count the number of k's between 0 and n. k can be 0 - 9. Example if n = 12, k = 1 in

\[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12\] we have FIVE 1's (1, 10, 11, 12)

``` r
count_digits<-function(n, k){
series=seq(1:n)
series_split=series%>%
  as.character()%>%
  strsplit(split=character(0))%>%
  unlist()
  position_count=sum(series_split==1)
return (position_count)
}
```

Test our function

``` r
count_digits(12, 1)
```

    ## [1] 5

Ugly Number
===========

Description Ugly number is a number that only have factors 2, 3 and 5.

Design an algorithm to find the nth ugly number. The first 10 ugly numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12... Note that 1 is typically treated as an ugly number. Example If n=9, return 10.

``` r
ugly_seq<-function(n){
ugly_seq=NULL
s=1;ugly_seq=s
while (length(ugly_seq)<n){
 s_new=s+1
 if (s_new%%2==0|s_new%%3==0|s_new%%5==0){
   ugly_seq=c(ugly_seq, s_new)
 }
 s=s_new
}
return(ugly_seq[n])
}
```

Test our function

``` r
ugly_seq(9)
```

    ## [1] 10

``` r
ugly_seq(15)
```

    ## [1] 20
