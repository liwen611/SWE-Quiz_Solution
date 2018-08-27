Expression Evaluation
================
Liwen Huang
8/27/2018

Description Given an expression string array, return the final result of this expression. Example For the expression 2\*6-(23+7)/(1+2), input is

\[ "2", "\*", "6", "-", "(", "23", "+", "7", ")", "/", "(", "1", "+", "2", ")"\], return 2

Once gain, I realize that R built in function makes this so easy, but it is not the intention of this problem.

``` r
evaluate<-function(input)  {
input=input%>%unlist()%>%paste(.,collapse="")
 result=eval(parse(text=input))
 return (result)
}
```

Test our function

``` r
input=list("2", "*", "6", "-", "(",
  "23", "+", "7", ")", "/",
  "(", "1", "+", "2", ")")

evaluate (input)
```

    ## [1] 2
