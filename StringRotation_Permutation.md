String Rotation and Permutation
================
Liwen Huang
8/25/2018

String Rotation
===============

Description Given a string and an offset, rotate string by offset. (rotate from left to right)

Example Given "abcdefg".

offset=0 =&gt; "abcdefg" offset=1 =&gt; "gabcdef" offset=2 =&gt; "fgabcde" offset=3 =&gt; "efgabcd

``` r
string_rotate<-function(string, index){
  if (index==0){
    paste(string, collapse='')
  }else{
  string=string%>%
    strsplit(., split=character(0))%>%
    unlist()
    rotate_index=length(string)-(index-1)
    result= c(string[rotate_index:length(string)],      string[1:rotate_index-1])
    paste(result, collapse='')
  }   
}
```

Test the function

``` r
string="abcdefg"
string_rotate(string, 0)
```

    ## [1] "abcdefg"

``` r
string_rotate(string, 1)
```

    ## [1] "gabcdef"

``` r
string_rotate(string, 3)
```

    ## [1] "efgabcd"

String Permutation
==================

This is a self-assigned problem, I think it is necessary that I know how to do it at the back of my head.

This is a solution from stack exchange that I have yet to understand

``` r
# Another recursive implementation    
# for those who like to roll their own, no package required 
permutations <- function( x, prefix = c() )
{
  if(length(x) == 0 ) return(prefix)
  do.call(rbind, sapply(1:length(x), FUN = function(idx) permutations( x[-idx], c( prefix, x[idx])), simplify = FALSE))
}

permutations(letters[1:3])
```

    ##      [,1] [,2] [,3]
    ## [1,] "a"  "b"  "c" 
    ## [2,] "a"  "c"  "b" 
    ## [3,] "b"  "a"  "c" 
    ## [4,] "b"  "c"  "a" 
    ## [5,] "c"  "a"  "b" 
    ## [6,] "c"  "b"  "a"

Also from stack exchange. Although a bit longer, it is easier to understand

``` r
getPerms <- function(x) {
    if (length(x) == 1) {
        return(x)
    }
    else {
        res <- matrix(nrow = 0, ncol = length(x)) #make an empty matrix with length of the object to be permutated
        for (i in seq_along(x)) {
            res <- rbind(res, cbind(x[i], getPerms(x[-i]))) 
#make a new row for reach i element, bind it as a new row to the existing matrix#
#each new row place the i element at the first position and permutate the rest without it#
        }
        return(res)
    }
}


getPerms(letters[1:3])
```

    ##      [,1] [,2] [,3]
    ## [1,] "a"  "b"  "c" 
    ## [2,] "a"  "c"  "b" 
    ## [3,] "b"  "a"  "c" 
    ## [4,] "b"  "c"  "a" 
    ## [5,] "c"  "a"  "b" 
    ## [6,] "c"  "b"  "a"
