Skyline
================
Liwen Huang
8/27/2018

Description Given N buildings in a x-axis，each building is a rectangle and can be represented by a triple (start, end, height)，where start is the start position on x-axis, end is the end position on x-axis and height is the height of the building. Buildings may overlap if you see them from far away，find the outline of them。

An outline can be represented by a triple, (start, end, height), where start is the start position on x-axis of the outline, end is the end position on x-axis and height is the height of the outline. ![skyline](https://lintcode-media.s3.amazonaws.com/problem/jiuzhang3.jpg)

Example Given 3 buildings：

\[ \[1, 3, 3\], \[2, 4, 4\], \[5, 6, 1\]\] The outlines are：

\[ \[1, 2, 3\], \[2, 4, 4\], \[5, 6, 1\]\]

``` r
skyline<-function(building){
outline=list()
for (i in 1: (length(building)-1)){
if (building[[i]][2]>building[[i+1]][1]){
  outline[[i]]=c(building[[i]][1],building[[i+1]][1], building[[i]][3])
}else {
  outline[[i]]=building[[i]]
}
  #the last outline has its own rule, it is always the builing itslef
  outline[[length(building)]]=building[[length(building)]]
}
return (outline)
}
```

Test our function.

``` r
building=list(
  c(1, 3, 3),
  c(2, 4, 4),
  c(5, 6, 1)
  )

skyline(building)
```

    ## [[1]]
    ## [1] 1 2 3
    ## 
    ## [[2]]
    ## [1] 2 4 4
    ## 
    ## [[3]]
    ## [1] 5 6 1

There is a different version on leetcode. Instead of returning the outline, the required function returns points to draw the outline. Logic is exactly the same.

A city's skyline is the outer contour of the silhouette formed by all the buildings in that city when viewed from a distance. Now suppose you are given the locations and height of all the buildings as shown on a cityscape photo (Figure A), write a program to output the skyline formed by these buildings collectively (Figure B).

!(figure A)\[<https://leetcode.com/static/images/problemset/skyline1.jpg>\]

!(figure B)\[<https://leetcode.com/static/images/problemset/skyline2.jpg>\]

The geometric information of each building is represented by a triplet of integers \[Li, Ri, Hi\], where Li and Ri are the x coordinates of the left and right edge of the ith building, respectively, and Hi is its height. It is guaranteed that 0 ≤ Li, Ri ≤ INT\_MAX, 0 &lt; Hi ≤ INT\_MAX, and Ri - Li &gt; 0. You may assume all buildings are perfect rectangles grounded on an absolutely flat surface at height 0.

For instance, the dimensions of all buildings in Figure A are recorded as: \[ \[2 9 10\], \[3 7 15\], \[5 12 12\], \[15 20 10\], \[19 24 8\] \] .

The output is a list of "key points" (red dots in Figure B) in the format of \[ \[x1,y1\], \[x2, y2\], \[x3, y3\], ... \] that uniquely defines a skyline. A key point is the left endpoint of a horizontal line segment. Note that the last key point, where the rightmost building ends, is merely used to mark the termination of the skyline, and always has zero height. Also, the ground in between any two adjacent buildings should be considered part of the skyline contour.

For instance, the skyline in Figure B should be represented as:\[ \[2 10\], \[3 15\], \[7 12\], \[12 0\], \[15 10\], \[20 8\], \[24, 0\] \].

Notes:

The number of buildings in any input list is guaranteed to be in the range \[0, 10000\]. The input list is already sorted in ascending order by the left x position Li. The output list must be sorted by the x position. There must be no consecutive horizontal lines of equal height in the output skyline. For instance, \[...\[2 3\], \[4 5\], \[7 5\], \[11 5\], \[12 7\]...\] is not acceptable; the three lines of height 5 should be merged into one in the final output as such: \[...\[2 3\], \[4 5\], \[12 7\], ...\]

``` r
skyline_point<-function(buildings){
  
  #function takes a list of building outline and return keypoints to draw the skyline
###maybe matrixized the list to make it more workable
buildings=buildings%>%unlist()%>%matrix(.,ncol=3, byrow=T)
pointlist=list()
pointlist[[1]]=buildings[1,c(1,3)]
for (i in 2:dim(buildings)[1] ){
  if (buildings[i, 1]>max(buildings[(1:i-1), 2])) {
    pointlist[[length(pointlist)+1]]=c(buildings[(i-1),2],0)
    pointlist[[length(pointlist)+1]]=buildings[i, c(1,3)]
  } else {
      if (buildings[i, 3]>buildings[(i-1), 3]) { 
      pointlist[[length(pointlist)+1]]= buildings[i, c(1,3)]
    } else {
      pointlist[[length(pointlist)+1]]=c(buildings[(i-1),2],buildings[i, 3])  
  }
  }
}
pointlist[[length(pointlist)+1]]=c(buildings[dim(buildings)[1],2],0)
return(pointlist)
}
```

Test our function

``` r
buildings=list(c(2, 9, 10), c(3, 7, 15), c(5, 12, 12), c(15, 20, 10), c(19, 24, 8) )

skyline_point(buildings)
```

    ## [[1]]
    ## [1]  2 10
    ## 
    ## [[2]]
    ## [1]  3 15
    ## 
    ## [[3]]
    ## [1]  7 12
    ## 
    ## [[4]]
    ## [1] 12  0
    ## 
    ## [[5]]
    ## [1] 15 10
    ## 
    ## [[6]]
    ## [1] 20  8
    ## 
    ## [[7]]
    ## [1] 24  0

There is a detailed explanation online, which is well-made. I did not read it when I was solving this problem but if someone happened to stummbled on this problem and needs more visual help. Here it is: [visual help on skyline problem](https://briangordon.github.io/2014/08/the-skyline-problem.html)
