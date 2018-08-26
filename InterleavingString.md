InterleavingString
================
Liwen Huang
8/25/2018

Description Given three strings: s1, s2, s3, determine whether s3 is formed by the interleaving of s1 and s2.

Example For s1 = "aabcc", s2 = "dbbca"

When s3 = "aadbbcbcac", return true. When s3 = "aadbbbaccc", return false.

This turns out to be a much harder problem so it is taking me a long while. After watching and reading many solutions, I finally got it! Thank ya'll youtubers and quizz enthusiasts!

``` r
##this function return TRUE or FALSE
interLeaving<-function(s1, s2, s3){
  #transfer the string to something that is manipulatable
  s1=strsplit(s1, split=character(0))%>%unlist()
  s2=strsplit(s2, split=character(0))%>%unlist()
  s3=strsplit(s3, split=character(0))%>%unlist()
  
  #initialize a matrix, each cell represent i+j-2 position in s3
  DP=matrix(NA, nrow=length(s1)+1, ncol=length(s1)+1) 
  #initialize the 0 row and the 0 colum, I will lable them with the according string, so if I made any mistake, I can always go back to check 
  colnames(DP)=c(0, s1)
  rownames(DP)=c(0, s2)
  #the [0,0]cell would always be true
  DP['0', '0']=TRUE
  DP['0', 2:ncol(DP)]=(s1==s3[1:length(s1)])
  DP[2:nrow(DP),'0']=(s2==s3[1:length(s2)])
  for (i in 2:nrow(DP)){
    for (j in 2:ncol(DP)){
      DP[i,j]=(s3[i+j-2]==s1[j-1] & DP[i,j-1] |s3[i+j-2]==s2[i-1] & DP[i-1,j])
      #check top string and left cell | left string and top cell
    }
  }
  return(DP[nrow(DP),ncol(DP)]) #the last cell is the answer!
}
```

Test our function. The correct function will return TRUE for s3; FALSE for s4

``` r
s1 = "aabcc"; s2 = "dbbca"
s3 = "aadbbcbcac";s4 = "aadbbbaccc"

interLeaving(s1, s2, s3)
```

    ## [1] TRUE

``` r
interLeaving(s1, s2, s4)
```

    ## [1] FALSE
