MinumumWindownSubstring
================
Liwen Huang
8/27/2018

Given a string source and a string target, find the minimum window in source which will contain all the characters in target.

Clarification Should the characters in minimum window has the same order in target?

Not necessary. Example For source = "ADOBECODEBANC", target = "ABC", the minimum window is "BANC"

In CS language, this is a sliding window problem.Initialize the head and tail pointers and start researching. Move the tail only when not all target characters are found; Move the head when all target characters are found. By eyeballing the source string, the first window would be \[*A**D**O**B**E**C*\]*O**D**E**B**A**N**C* and the second would be *A**D**O*\[*B**E**C**O**D**E**B**A*\]*N**C* etc etc. At the same time, we keep track of the minimal length.

``` r
min_windown<-function(source, target){

source=source%>%strsplit(split=(character(0)))%>%unlist()
target=target%>%strsplit(split=(character(0)))%>%unlist()

i=j=1
c=r=0
n=length(target)
if(sum(source[i]==target)>0){
  while(sum(target%in%source[i:j])<n)
  j=j+1
} else {
  i=i+1
}
#found the first window at this point
c=length(source[i:j]);r=c
head=i;tail=j
while(j<length(source)){ #do this until tail reaching the end of the source string
#now move the head
if (sum(target%in%source[i:j])==n){
  i=i+1
  while(sum(source[i]==target)==0) i=i+1
    while(sum(target%in%source[i:j])<n) j=j+1
}      
#at this point, found the second window
c=length(source[i:j])
if (c<r){  ##update head and tail when the shorter window has been found
  r=c
  head=i;tail=j
  }
}
window=paste(source[head:tail], collapse="")
return (window)
}
```

Test our function

``` r
source = "ADOBECODEBANC"; target = "ABC"
min_windown(source, target)
```

    ## [1] "BANC"

While I was solving this quiz, I watch a youtube clip to help me going. It was fun, at least it helps me feel less longly! Here is the clip:

[Yusen solving minial window substring](https://www.youtube.com/watch?v=OXLgNDt4QMY)
