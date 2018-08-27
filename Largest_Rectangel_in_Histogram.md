Largest Rectangle in Histogram
================
Liwen Huang
8/27/2018

Description Given n non-negative integers representing the histogram's bar height where the width of each bar is 1, find the area of largest rectangle in the histogram.

![histogram](https://lintcode-media.s3.amazonaws.com/problem/histogram1.png)

Above is a histogram where width of each bar is 1, given height = \[2,1,5,6,2,3\].

![histogram](https://lintcode-media.s3.amazonaws.com/problem/histogram_area.png)

The largest rectangle is shown in the shaded area, which has area = 10 unit. Example Given height = \[2,1,5,6,2,3\], return 10.

Pretty easy, the only trick is that the width is conditional.

``` r
max_area<-function(height){
n=length(height)
###the area of the rectagles is height*width, width is as long as the next height index is larger than the height index
area=numeric(n)
for (i in 1:(n-1)){
  if (height[i+1]>height[i]){
  area[i]=height[i]*(1+length(which(height[i:n]>height[i])))
  } else {
    area[i]=height[i]
  }
}
area[n]=height[n]
return (max(area))
}
```

``` r
height = c(2,1,5,6,2,3)
max_area(height)
```

    ## [1] 10
