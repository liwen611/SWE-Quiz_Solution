Longest Common Sequence and Longest Common Substring
================
Liwen Huang
8/27/2018

Longest Common Subsequence
==========================

Description Given two strings, find the longest common subsequence (LCS).

Your code should return the length of LCS.

Example For "ABCD" and "EDCA", the LCS is "A" (or "D", "C"), return 1. For "ABCD" and "EACB", the LCS is "AC", return 2.

Try a naive approach that is along the line of the DP. Find all the common elements between the two string, and see what is the order in the seond string, return the longest ordered index.

``` r
lcs<-function(a, b){
a=a%>%strsplit(split=character(0))%>%unlist()
b=b%>%strsplit(split=character(0))%>%unlist()

common=a[which(a%in%b)]
##note that the order of the argument matters, intersect always return the common elements in the order of the first argument
index=numeric()
for (i in 1:length(common)){
  index=c(index, which(b==common[i]))
}

r=1;c=0
q=index[1]
for (i in 2:length(index)){
  if (index[i]>q[length(q)]) q=c(q,index[i])
  else q=index[i]

c=length(q)
r=max(r, c)
}
return (r)
}
```

Test our function

``` r
a="ABCD"
b="EDCA"
c="EACB"

lcs(a,b)
```

    ## [1] 1

``` r
lcs(a,c)
```

    ## [1] 2

Test our function on longer strings

``` r
x="AGGTAB"
y="GXTXAYB"

lcs(x,y)
```

    ## [1] 4

There is a very nice recursion that I found on Greeks for Greeks. The idea is that is the last letter matches, than the length would just be 1+ the lcs. Here is what that solution looks like: def lcs(X, Y, m, n):

    if m == 0 or n == 0:
       return 0;
    elif X[m-1] == Y[n-1]:
       return 1 + lcs(X, Y, m-1, n-1);
    else:
       return max(lcs(X, Y, m, n-1), lcs(X, Y, m-1, n));

I would try to replicate this in R.

``` r
lcs_2<-function(x,y){
  x=x%>%strsplit(split=character(0))%>%unlist()
  y=y%>%strsplit(split=character(0))%>%unlist()
  
  m=length(x);n=length(y)
  
  lcs_3<-function(x, y, m, n){
    if (m == 0 | n == 0) return (0)
    else if (x[m] == y[n]) return (1 + lcs_3(x, y, m-1, n-1))
    else return (max(lcs_3(x, y, m, n-1), lcs_3(x, y, m-1, n)))
  }
  
  max_length=lcs_3(x, y, m, n)
  return (max_length)
}
```

Test our function

``` r
lcs_2(x,y)
```

    ## [1] 4

``` r
a="ABCD"
b="EDCA"
c="EACB"

lcs_2(a, b)
```

    ## [1] 1

``` r
lcs_2(a, c)
```

    ## [1] 2

``` r
lcs_2(b, c)
```

    ## [1] 2

Longest Common String
=====================

Description Given two strings, find the longest common substring.

Return the length of it.

Example Given A = "ABCD", B = "CBCE", return 2.

This problem can be solved in DP

``` r
lc_string<-function(A, B){
A=A%>%strsplit(split = character(0))%>%unlist()
B=B%>%strsplit(split = character(0))%>%unlist()

m=length(A);n=length(B)
dp=matrix(0,nrow=n+1, ncol=m+1)
for (i in 2:ncol(dp)){
dp[i,2:ncol(dp)]=(B[i-1]==A)
}
for (i in 2:nrow(dp)){
  for (j in 2:ncol(dp)){
    if (!dp[i,j]==0) dp[i,j]=dp[(i-1),(j-1)]+1
  }
}
return (max(dp))
}
```

``` r
A = "ABCD"; B = "CBCE"
lc_string(A, B)
```

    ## [1] 2

If we are to asked to provide this longest common string, we just need to refer to the DP matrix and return according to the index

``` r
A=A%>%strsplit(split = character(0))%>%unlist()
B=B%>%strsplit(split = character(0))%>%unlist()
m=length(A);n=length(B)
dp=matrix(0,nrow=n+1, ncol=m+1)
for (i in 2:ncol(dp)){
dp[i,2:ncol(dp)]=(B[i-1]==A)
}
for (i in 2:nrow(dp)){
  for (j in 2:ncol(dp)){
    if (!dp[i,j]==0) dp[i,j]=dp[(i-1),(j-1)]+1
  }
}

colnames(dp)=c(0, A)
rownames(dp)=c(0, B)
dp
```

    ##   0 A B C D
    ## 0 0 0 0 0 0
    ## C 0 0 0 1 0
    ## B 0 0 1 0 0
    ## C 0 0 0 2 0
    ## E 0 0 0 0 0

The longest common substring is 'BC'.
