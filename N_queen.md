N\_queen Problem
================
Liwen Huang
8/25/2018

Description The n-queens puzzle is the problem of placing n queens on an n×n chessboard such that no two queens attack each other.

Given an integer n, return all distinct solutions to the n-queens puzzle.

Each solution contains a distinct board configuration of the n-queens' placement, where 'Q' and '.' both indicate a queen and an empty space respectively.

Example There exist two distinct solutions to the 4-queens puzzle:

// Solution 1 \[".Q..", "...Q", "Q...", "..Q." \], // Solution 2 \["..Q.", "Q...", "...Q", ".Q.." \]

Challenge Can you do it without recursion?

I think the without recursion part is tough. But I want to remind myself how to do permuation problem first. Because this is essentially a permutation problem too. And I'll explain why later on. So a while ago, I have to figure out how to permuate using base R code

``` r
permutations <- function(n){
  if(n==1){
    return(matrix(1)) ### this is our base case
  } else {
    A=permutations(n-1) 
    B=matrix(nrow=0, ncol=n)
    for (i in 1:nrow(A)) {
      for (j in 0:(n-1)){ #for each row of the result (n-1), we want to append the new element in different position
        B=rbind(B, append(A[i,],n, after=j)) 
      }
    }
    return (B)
  }
}
```

Test the function

``` r
permutations(3)
```

    ##      [,1] [,2] [,3]
    ## [1,]    3    2    1
    ## [2,]    2    3    1
    ## [3,]    2    1    3
    ## [4,]    3    1    2
    ## [5,]    1    3    2
    ## [6,]    1    2    3

Ok, now let's go back to why this is essentially a permutation problem. So if we pay attention to the solution set for n=4 before, we will see that if the position of the queen is expressed in the index for its positin, it would be \[2, 4, 1, 3\] and \[3, 1, 4, 2\] The obsolute distance between any idexes are larger than 2, that is the condition to garantee no adjacent queens.

In order the replicate the above resuits, we can ask R to return the subsets of n permutation that fit the requirement.

``` r
per4=permutations(4)
for (i in 1:nrow(per4)){
    if (abs(per4[i,2]-per4[i,1])>=2 &
        abs(per4[i,3]-per4[i,2])>=2 &
        abs(per4[i,4]-per4[i,3])>=2)  
      print (per4[i,])
  }
```

    ## [1] 2 4 1 3
    ## [1] 3 1 4 2

Here, let's put everything together. I am too lazy so I'll just nest one function within another. You know, too much debugging if you are moving things around.

``` r
n_queen<-function(n) {
  #insert the permutation function
  permutations <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      A=permutations(n-1)
      B=matrix(nrow=0, ncol=n)
      for (i in 1:nrow(A)) {
        for (j in 0:(n-1)){
          B=rbind(B, append(A[i,],n, after=j)) 
        }
      }
      return (B)
    }
  }
  
  B=permutations(n) 
  
  subset=matrix(nrow=0, ncol=n)
  row_index=numeric(0)
  for (i in 1:nrow(B)){
    r=numeric(0)
    for (j in 2:n) r=c(r, abs(B[i,j]-B[i,j-1])>=2)
    if(sum(r)==n-1) row_index=c(row_index,i)
    subset=B[row_index,]
  }  
  return (subset) 
}
```

Test our function. Giving n, the function returns a set of position index for the queen in each row

``` r
n_queen(4)
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    2    4    1    3
    ## [2,]    3    1    4    2

``` r
n_queen(5)
```

    ##       [,1] [,2] [,3] [,4] [,5]
    ##  [1,]    3    5    2    4    1
    ##  [2,]    4    2    5    3    1
    ##  [3,]    2    5    3    1    4
    ##  [4,]    4    2    5    1    3
    ##  [5,]    5    2    4    1    3
    ##  [6,]    2    4    1    5    3
    ##  [7,]    2    4    1    3    5
    ##  [8,]    5    3    1    4    2
    ##  [9,]    3    5    1    4    2
    ## [10,]    3    1    4    2    5
    ## [11,]    3    1    5    2    4
    ## [12,]    4    1    3    5    2
    ## [13,]    1    3    5    2    4
    ## [14,]    1    4    2    5    3

Actually, just found this solution is wrong, because I do not play chess, therefore, did not know how it works. Looks like the queen can attack no just the immediate cell it's next too, it attacks like a radiactive light, straight and diagnally. Ugh~ so I modified the above function, now it works!

``` r
n_queen_correct<-function(n) {
  #insert the permutation function
  permutations <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      A=permutations(n-1)
      B=matrix(nrow=0, ncol=n)
      for (i in 1:nrow(A)) {
        for (j in 0:(n-1)){
          B=rbind(B, append(A[i,],n, after=j)) 
        }
      }
      return (B)
    }
  }
  
  B=permutations(n) 
  
  #if the sum of row index and column index is the same, queen can attack.
  upper_diag=matrix(nrow=nrow(B), ncol=n)
  for (i in 1:nrow(B)){
    upper_diag[i,]=(1:n+B[i,1:n]) #sum the row and column index by row
  }
  #row sum with duplicated numbers are the rows that disqualify
  lower_diag=matrix(nrow=nrow(B), ncol=n)
  for (i in 1:nrow(B)){
    lower_diag[i,]=(1:n-B[i,1:n])
  }
  #similarly, duplicated numbers that represent row index substracted by column index disqualify
  u=NULL
  for (i in 1:nrow(B)){
    if (anyDuplicated(upper_diag[i,])==0)
      u=c(u, i)
  }
  
  l=NULL
  for (i in 1:nrow(B)){
    if (anyDuplicated(lower_diag[i,])==0)
      l=c(l, i)
  }
  subset_index=intersect(u,l)
  subset=B[subset_index,]
  return (subset) 
}
```

Test the function. If correct, the function should return 10 subsets for n=5

``` r
n_queen_correct(5)
```

    ##       [,1] [,2] [,3] [,4] [,5]
    ##  [1,]    3    5    2    4    1
    ##  [2,]    4    2    5    3    1
    ##  [3,]    2    5    3    1    4
    ##  [4,]    5    2    4    1    3
    ##  [5,]    2    4    1    3    5
    ##  [6,]    5    3    1    4    2
    ##  [7,]    3    1    4    2    5
    ##  [8,]    4    1    3    5    2
    ##  [9,]    1    3    5    2    4
    ## [10,]    1    4    2    5    3

I realize that I am being a lazy ass here and did not return the answer as instructed. But I have reference pretty good print out from the internet, and here it is!

``` r
print_board <- function(p, q.char = "Q", sep = " ") {
  n <- length(p)
  row <- rep("-", n)
  for (i in seq_len(n)) {
    row_i <- row
    row_i[p[i]] <- q.char
    cat(paste(row_i, collapse = sep))
    cat("\n")
  }
}
```

``` r
sol_5=n_queen_correct(5)

for (i in 1:nrow(sol_5)){
  print(print_board(sol_5[i,]))
  cat("\n")
}
```

    ## - - Q - -
    ## - - - - Q
    ## - Q - - -
    ## - - - Q -
    ## Q - - - -
    ## NULL
    ## 
    ## - - - Q -
    ## - Q - - -
    ## - - - - Q
    ## - - Q - -
    ## Q - - - -
    ## NULL
    ## 
    ## - Q - - -
    ## - - - - Q
    ## - - Q - -
    ## Q - - - -
    ## - - - Q -
    ## NULL
    ## 
    ## - - - - Q
    ## - Q - - -
    ## - - - Q -
    ## Q - - - -
    ## - - Q - -
    ## NULL
    ## 
    ## - Q - - -
    ## - - - Q -
    ## Q - - - -
    ## - - Q - -
    ## - - - - Q
    ## NULL
    ## 
    ## - - - - Q
    ## - - Q - -
    ## Q - - - -
    ## - - - Q -
    ## - Q - - -
    ## NULL
    ## 
    ## - - Q - -
    ## Q - - - -
    ## - - - Q -
    ## - Q - - -
    ## - - - - Q
    ## NULL
    ## 
    ## - - - Q -
    ## Q - - - -
    ## - - Q - -
    ## - - - - Q
    ## - Q - - -
    ## NULL
    ## 
    ## Q - - - -
    ## - - Q - -
    ## - - - - Q
    ## - Q - - -
    ## - - - Q -
    ## NULL
    ## 
    ## Q - - - -
    ## - - - Q -
    ## - Q - - -
    ## - - - - Q
    ## - - Q - -
    ## NULL

Argh, don't ask, I am too lazy to fix the NULL in between. Let's just leave it like that.

However, I believe this solution is not entirely sufficient. First, I have used recursion; secondly, I have enuerate a bunch of possibilities that I do not need then eliminate them one by one. However, there is some merits in this solution, as by permutation, I don't need to deal with the constraint of placing the queen in the same row or the same column.

The standard solution calls for backtracking, which is a concept that I don't need to master, but plenty good examples on the leetcode discussion forum.
