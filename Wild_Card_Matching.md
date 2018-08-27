Wild Card Matching
================
Liwen Huang
8/27/2018

Description Implement wildcard pattern matching with support for '?' and '\*'.

'?' Matches any single character. '\*' Matches any sequence of characters (including the empty sequence). The matching should cover the entire input string (not partial).

Example isMatch("aa","a") → false isMatch("aa","aa") → true isMatch("aaa","aa") → false isMatch("aa", "*") → true isMatch("aa", "a*") → true isMatch("ab", "?*") → true isMatch("aab", "c*a\*b") → false

Right now, I am using the built-in function to solve the wild card situation, but I know this is not what the question is intended for. I'll probably update this solution once I figure out how to.

``` r
text_matching<-function(text, pattern){
  
text=text%>%strsplit(split=character(0))%>%unlist()
pattern=pattern%>%strsplit(split=character(0))%>%unlist()
###write a string matching function to deal with all the non-wildcard situation.
str_match<-function(text, pattern){
if (!length(text)==length(pattern)) return (FALSE)
if(length(text)==1 & length(pattern)==1) {
  if (pattern=='?') return (TRUE)
  else return (identical(text, pattern))
  }
else return (str_match(text[-1], pattern[-1]))
}

if (!"*"%in%pattern ) return ( str_match(text, pattern) )
else {
  pattern=paste(pattern, collapse="")
  text=paste(text, collapse="")
  return(grepl(glob2rx(pattern), text))
    }
}
```
