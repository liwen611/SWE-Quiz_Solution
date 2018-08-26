MaxSubarray Problem Collection
================
Liwen Huang
8/25/2018

Maximum Subarray
================

Description Given an array of integers, find two non-overlapping subarrays which have the largest sum. The number in each subarray should be contiguous. Return the largest sum.

Example For given \[1, 3, -1, 2, -1, 2\], the two subarrays are \[1, 3\] and \[2, -1, 2\] or \[1, 3, -1, 2\] and \[2\], they both have the largest sum 7.

I accidently solve for the maximal subarray, but since I took the effect, I might as well document it too. Maybe later on, I can think about how to add it it.

``` r
maxSubarray<-function(array){
  slice_sum=matrix(0,nrow=length(array), ncol=length(array))
for (i in 1:nrow(slice_sum)){
  for (j in 1:ncol(slice_sum)){
    if(i<=j){
      slice_sum[i,j]=sum(array[i:j])
    }
  }
}
index=which(slice_sum==max(slice_sum), arr.ind = TRUE)
max_sub=array[index[1]:index[2]]
result=list(MaxSubarray=max_sub, sum=max(slice_sum))
return (result)
}
```

Test our function.

``` r
array=c(1, 3, -1, 2, -1, 2)

maxSubarray(array)
```

    ## $MaxSubarray
    ## [1]  1  3 -1  2 -1  2
    ## 
    ## $sum
    ## [1] 6

In solving for 2 non-overlapping subarrays, I can use a point at the array, solve for the maximal subarray from the right, and from the right, then I know these two combined will give me the largest pair. Incooperating the function I already have, this is probably going to take an hour or two. But in the following, I would like to give Kadane's Algorithm a shot. The idea is that, every time I move one number onward, I only need to check if the maxsubarray so far is bigger or less than my current number combing with the my current number. The following functions has stolen the ideal from the maxsub function from the adagio package.

``` r
max_sum1<-function(array){
  m1=m2=0 #initialize m1 m2 as 0
  for (i in 1:length(array)){
  m2=m2+array[i]# store sum as iterate one element onward
  m1=max(m2, m1) #keep track on the local maximum
  }
  return (m1)
}
#in a simplier case, this version of the function returns the maximal sum of the subarray
```

Test our function.

``` r
max_sum1(array)
```

    ## [1] 6

``` r
max_sum1(array[1:3])
```

    ## [1] 4

The function correctly return the maximal subarray sum of the whole array, and the maximal sum of up to the third element.

We are going to look at a more complicated task, when we need to return both the array sum, and the subarray itself.

``` r
max_sum2<-function(array){
m1 <- m2 <- 0 #initialize both the local sum the global sum as 
p1 <- p2 <- 0 #the global index
q1 <- q2 <- 1 #the local index
for (i in 1:length(array)) {
  if (m2 > -array[i]) { 
    #check if including the current iteration will have a bigger local sum 
    m2 <- m2 + array[i]
    q2 <- i #update the local index
    if (m2 > m1) {
      m1 <- m2
      p1 <- q1; p2 <- q2 #update the global index
    }
  } 
  else {
    m2 <- 0
    q1 <- q2 <- i+1
  }
}
return(list(sum = m1, sub_array=array[p1:p2]))
}
```

Test our function

``` r
max_sum2(array)
```

    ## $sum
    ## [1] 6
    ## 
    ## $sub_array
    ## [1]  1  3 -1  2 -1  2

Now, the critical point. If we are to solve for two non-overlapping subarray that gives the largest sum, we can think of devided the array into two slices. We solve for the subarray from the left side while also solve for one on the right side. Then the two of these two subarray combined would give the largest sum.

``` r
Two_sub<-function(array){
  ###get the max sum function first
  max_sum2<-function(array){
    m1 <- m2 <- 0 #initialize both the local sum the global sum as 
    p1 <- p2 <- 0 #the global index
    q1 <- q2 <- 1 #the local index
    for (i in 1:length(array)) {
      if (m2 > -array[i]) { 
        #check if including the current iteration will have a bigger local sum 
        m2 <- m2 + array[i]
        q2 <- i #update the local index
        if (m2 > m1) {
          m1 <- m2
          p1 <- q1; p2 <- q2 #update the global index
        }
      } 
      else {
        m2 <- 0
        q1 <- q2 <- i+1
      }
    }
    return(list(sum = m1, sub_array=array[p1:p2]))
  }
  
  ###now we divide
  max_list=list() #this would store the sum of all the subarray at each dividing point
  r=0;c=0
  for (i in 1:(length(array)-1)){
    
    left=array[1:i]
    right=array[(i+1):length(array)]
    
    left_max=max_sum2(left)
    right_max=max_sum2(right)
   c=left_max[[1]]+right_max[[1]]
   r=max(r,c)
  }
  return (r)
}

Two_sub(array)
```

    ## [1] 7

This function return the maximal sum but not the subarrays.
