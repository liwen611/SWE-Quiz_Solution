Fibnacci sequence
================

There are many ways to solve the Fibnacci sequence problem, top-down with recursion or bottom-up by producing the whole sequence up to n-th fib number. Here are but a few possibilities.

``` r
fib=function(len){
  
 # This function produce the fib sequence with bottom-up technique
 
fibvals <- numeric(len)
fibvals[1] <- 1
fibvals[2] <- 1
for (i in 3:len) { 
  fibvals[i] <- fibvals[i-1]+fibvals[i-2]
} 
return (fibvals)
}
```

test the function with 10

``` r
fib(10)
```

    ##  [1]  1  1  2  3  5  8 13 21 34 55

``` r
recurse_fibonacci <- function(n) {
  
  #This function produce the n-th fib number with recusion
  
if(n <= 1) {
return(n)
} else {
return(recurse_fibonacci(n-1) + recurse_fibonacci(n-2))
}
}
```

test the function with 10

``` r
recurse_fibonacci(10)
```

    ## [1] 55

This is a solution found R document, which is a cleaner version of the above. The purpose is to demonstrate the use of Recall, but a do.call and simply call the function will work just as well.

``` r
## A trivial (but inefficient!) example:
fib <- function(n)
   if(n<=2) { if(n>=0) 1 else 0 } else Recall(n-1) + Recall(n-2)
fibonacci <- fib; rm(fib)
## renaming wouldn't work without Recall
fibonacci(10) # 55
```

    ## [1] 55

For example, the following function do exactly the same.

``` r
#naive recursive implementation
fib <- function(n)  {
  if(n == 1 || n == 2) return(1)
  fib(n-1) + fib(n-2)
}
```

``` r
#with memoization
fibm <- function(n)  {
  if(n == 1 || n == 2) return(1)

  seq <- integer(n)
  seq[1:2] <- 1

  calc <- function(n) {
    if (seq[n] != 0) return(seq[n])
    seq[n] <<- calc(n-1) + calc(n-2)
    seq[n]
  }

  calc(n)
}
```

``` r
fibm(20)
```

    ## [1] 6765

Is memoization faster? microbenchmark(fib(20), fibm(20)) Unit: microseconds \#expr min lq mean median uq max neval cld \#fib(20) 8005.314 8804.130 9758.75325 9301.6210 9798.8500 46867.182 100 b \#fibm(20) 38.991 44.798 54.12626 53.6725 60.4035 97.089 100 a

<span style="color:blue">Fun fact: There is another problem that is basically a Fibnacci problem. </span>

<span style="color:blue">Description You are climbing a stair case. It takes n steps to reach to the top.

<span style="color:blue">Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top? <span style="color:blue">Do you know why? Try to solve it and you'll see.</span>
