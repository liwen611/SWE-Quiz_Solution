Palindrome Partitioning
================
Liwen Huang
8/27/2018

23. Palindrome Partitioning II
==============================

Given a string s, partition s such that every substring of the partition is a palindrome.

Return the minimum cuts needed for a palindrome partitioning of s.

Example:

Input: "aab" Output: 1 Explanation: The palindrome partitioning \["aa","b"\] could be produced using 1 cut.

The following solution is inspired by a leetcode contributor, here is her original python solution

    def minCut(self, s):

      cut = [x for x in range(-1,len(s))]

      for i in range(0,len(s)):

        for j in range(i,len(s)):
      
            if s[i:j] == s[j:i:-1]:
      
            cut[j+1] = min(cut[j+1],cut[i]+1)
    return cut[-1]

The main algorithm idea is if s\[i,j\] is a palindrome, then the minCut(s\[:j\]) is at most minCut(s\[:i-1\])+1.

``` r
min_palincut<-function(s){
s=s%>%strsplit(split=character(0))%>%unlist()
if (identical(s, rev(s))) return(0)
###establish how to evaluate whether a string is a palin or not
cut=seq(-1,length(s)-2) ###get a vector to store the worse case scenario when no palin is found
for (i in 1:length(s)){
  for (j in i:length(s)){
    if (identical(s[i:j], rev(s[i:j]))){
    cut[j] = min(cut[j],cut[i-1]+1) 
    }
  }
} 
return(cut[length(cut)])
}
```

The problem can also be solved in DP, but I like this logic better. Let's test our function.

``` r
s="aab"
min_palincut(s)
```

    ## [1] 1
