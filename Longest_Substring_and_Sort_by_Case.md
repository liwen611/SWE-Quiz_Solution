Longest Substring & Sort Letters by Case
================
Liwen Huang
8/25/2018

Longest Substring
=================

Given a string, find the length of the longest substring without repeating characters. For example, the longest substring without repeating letters for "abcabcbb" is "abc", which the length is 3. For "bbbbb" the longest substring is "b", with the length of 1.

This is a rather easy one!

``` r
longest_sub1<-function(string){
  string=string%>%strsplit(split=character(0))%>%unlist()
  max_length=0
  sub=NULL
  longest_sub=NULL
  for (i in 1:length(string)){
    if (!string[i]%in%sub){
      sub=c(sub, string[i])
      longest_sub=sub
    } else {
      sub=NULL
      sub=c(sub, string[i])
      if (length(sub)>length(longest_sub)){
        longest_sub=sub
      }
    }
  } 
  max_length=length(longest_sub)
  longest_sub=paste(longest_sub, collapse = "")
  return (paste("The longest Substring is:", longest_sub,", and its length is:", max_length, collapse=""))
}
```

Test our function.

``` r
string="abcabcbb"
string1="bbbbbbb"

longest_sub1(string)
```

    ## [1] "The longest Substring is: abc , and its length is: 3"

``` r
longest_sub1(string1)
```

    ## [1] "The longest Substring is: b , and its length is: 1"

I have done another version, which is the longest substring with alphabetical order, I might as well document that function as well. So here it goes:

``` r
longest_sub2<-function(string){
string=string%>%strsplit(split=character(0))%>%unlist()
max_length=0
sub=NULL
sub_2=NULL

for (i in 1:length(string)){
  if (length(sub)==0) sub=string[i] 
  else if (string[i]>=sub[length(sub)]) sub=c(sub, string[i]) #piece the next character if it is in order
  else if (string[i]<sub[length(sub)]) {
    if (length(sub)>length(sub_2)){
      sub_2=sub #sub_2 keep track of the qualified substring so far
      sub=string[i]
    } else {
      sub=string[i]
    }
  }
  max_length=length(sub_2)
}
sub_2=paste(sub_2, collapse = '')
return (c(sub_2, max_length))
}
```

Test our function.

``` r
string3 = 'cyqfjhcclkbxpbojgkar'

longest_sub2(string3)
```

    ## [1] "ccl" "3"

14.Sort Letters by Case
=======================

Description Given a string which contains only letters. Sort it by lower case first and upper case second.

Example For "abAcD", a reasonable answer is "acbAD"

Challenge Do it in one-pass and in-place.

The one to go about it is to first chop up the string into letters, use regrex to seperate the lower and upper case letters, sort each, then paste them together again. Here is an example of what I mean

``` r
s="TEsT"
s=strsplit(s, split=character(0))%>%unlist()
grepl("^[[:upper:]]+$", s)
```

    ## [1]  TRUE  TRUE FALSE  TRUE

Now use the bolean to retrieve the uppercase letters in the string

``` r
upperindex=grepl("^[[:upper:]]+$", s)
s[upperindex]
```

    ## [1] "T" "E" "T"

Now, let's put it together

``` r
case_sorter<-function(string){
  string=string%>%strsplit(split=character(0))%>%unlist()
  u_index=grepl("^[[:upper:]]+$", string)
  l_index=grepl("^[[:lower:]]+$", string)
  
  u_sorted=sort(string[u_index])
  l_sorted=sort(string[l_index])
  
  string_new=append(l_sorted, u_sorted)%>%paste(collapse = "")
  return(string_new)
}
```

Test the function

``` r
case_sorter(s)
```

    ## [1] "sETT"

``` r
s2="abAcD"
case_sorter(s2)
```

    ## [1] "abcAD"
