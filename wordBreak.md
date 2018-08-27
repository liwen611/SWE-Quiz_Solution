Word Break
================
Liwen Huang
8/27/2018

Description Given a string s and a dictionary of words dict, determine if s can be break into a space-separated sequence of one or more dictionary words.

Example Given s = "lintcode", dict = \["lint", "code"\].

Return true because "lintcode" can be break as "lint code".

This is an easy one, we can do it iteratively or recursively, but I always refer iteration as it is easy to understand.

``` r
wordbreask<-function(s, dict){
s_string=s%>%strsplit(split=character(0))%>%unlist()
n=length(s_string)

for (i in 1:n){
s_pre=s_string[1:i]%>%paste(.,collapse="")
s_suf=s_string[(i+1):n]%>%paste(.,collapse="")
if (s_pre%in%dict | s_suf%in%dict) return(c(TRUE, paste(s_pre, s_suf, sep=" ")))
}
}
```

Test our function

``` r
s = "lintcode"; dict = c("lint", "code")
wordbreask(s, dict)
```

    ## [1] "TRUE"      "lint code"

``` r
dict2=c('i', 'like', 'sam', 'sung', 'samsung', 'mobile', 'ice', 'cream', 'icecream', 'man', 'go', 'mango')
s2='ilike'

wordbreask(s2, dict2)
```

    ## [1] "TRUE"   "i like"
