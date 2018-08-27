Tower of Hanoi
================
Liwen Huang
8/27/2018

Description Tower of Hanoi problem, is a well-known problem. On the A, B, C three pillars, there are n disks of different sizes (radii 1-n), they are stacked in a start on A, your goal is to a minimum number of legal steps to move all the plates move from A to C tower tower. Each step in the rules of the game are as follows:

Each step is only allowed to move a plate (from the top of one pillars to the top of another pillars) The process of moving, you must ensure that a large dish is not at the top of the small plates (small can be placed on top of a large, below the maximum plate size can not have any other dish) Diagram: ![](http://ww4.sinaimg.cn/large/0060lm7Tly1fphwld4at7j30dm05q74d.jpg)

Example Given n = 3

return \["from A to C","from A to B","from C to B","from A to C","from B to A","from B to C","from A to C"\]

This is a classic recursive problem. The thing to remember is that there are three moves in a Hanoi problem T(m, begin, spare, end), they are: T(n-1, begin, end, spare) ---moving the top n-1 disks to the spare spike T(1, begin, spare, end) --- moving the bottom disk to the target spike T(n-1, spare, begin, end) ----moving the top n-1 to the target spike.

The following is the most barebone solution:

``` r
hanoi<-function(n, begin, spare, end){
  #the begining, spare, and end position need to be specified
  if (n==1){
    print(c("Move from ", begin, "to", end))
  } else {
    hanoi(n -1, begin, end, spare)
    hanoi(1, begin, spare, end)
    hanoi(n-1, spare, begin, end)
  }
}
```

Test our function

``` r
hanoi(3, "A", "B", "C")
```

    ## [1] "Move from " "A"          "to"         "C"         
    ## [1] "Move from " "A"          "to"         "B"         
    ## [1] "Move from " "C"          "to"         "B"         
    ## [1] "Move from " "A"          "to"         "C"         
    ## [1] "Move from " "B"          "to"         "A"         
    ## [1] "Move from " "B"          "to"         "C"         
    ## [1] "Move from " "A"          "to"         "C"

I found a very very elaborate solution from a r programmer online [](https://github.com/yihui/fun/blob/master/R/tower_of_hanoi.R)

``` r
tower_of_hanoi <- function(n = 7) {
  if (!interactive()) return()
  tower <- list(1:n, NULL, NULL)
  color <- rainbow(n)
  par(mfrow = c(1, 3), mar = rep(0, 4), ann = FALSE)
  bgcolor <- par("bg")
  if (bgcolor == "transparent") bgcolor <- "white"
  
  draw.hanoi <- function() {
    for (i in 1:3) {
      plot(c(-n, n), c(0, n + 2), type = "n", xlab = "",
           ylab = "", axes = FALSE)
      rect(-n, 0, n, n + 2, border = bgcolor, col = bgcolor)
      if (length(tower[[i]]) > 0) {
        barplot(rev(tower[[i]]), add = TRUE, horiz = TRUE,
                col = color[rev(tower[[i]])])
        barplot(-rev(tower[[i]]), add = TRUE, horiz = TRUE,
                col = color[rev(tower[[i]])])
      }
    }
  }
  
  move.hanoi <- function(k, from, to, via) {
    if (k > 1) {
      move.hanoi(k - 1, from, via, to)
      move.hanoi(1, from, to, via)
      move.hanoi(k - 1, via, to, from)
    }
    else {
      cat("Move ", tower[[from]][1], " from ", LETTERS[from],
          " to ", LETTERS[to], "\n")
      tower[[to]] <<- c(tower[[from]][1], tower[[to]])
      tower[[from]] <<- tower[[from]][-1]
      draw.hanoi()
      Sys.sleep(0.5)
    }
  }
  
  draw.hanoi()
  move.hanoi(n, 1, 2, 3)
}
```

Wow, isn't it mindblowing? I urge you to test it function on your console!
