Apple Stock
================
Liwen Huang
8/25/2018

Writing programming interview questions hasn't made me rich yet ... so I might give up and start trading Apple stocks all day instead.

First, I wanna know how much money I could have made yesterday if I'd been trading Apple stocks all day.

So I grabbed Apple's stock prices from yesterday and put them in a list called stock\_prices, where:

The indices are the time (in minutes) past trade opening time, which was 9:30am local time. The values are the price (in US dollars) of one share of Apple stock at that time. So if the stock cost $500 at 10:30am, that means stock\_prices\[60\] = 500.

Write an efficient function that takes stock\_prices and returns the best profit I could have made from one purchase and one sale of one share of Apple stock yesterday.

For example:

stock\_prices = \[10, 7, 5, 8, 11, 9\]

get\_max\_profit(stock\_prices) Returns 6 (buying for $5 and selling for $11)

No "shorting"—you need to buy before you can sell. Also, you can't buy and sell in the same time step—at least 1 minute has to pass.

``` r
get_max_profit<-function(stock_prices){
profit=NULL
for (i in 1:length(stock_prices)){
  buy=stock_prices[i]
  profit[i]=as_tibble(stock_prices[i:length(stock_prices)]-buy)
  max_profit=max(unlist(profit))
}
return(max_profit)
}
```

Test our function

``` r
stock_prices = c(10, 7, 5, 8, 11, 9)
get_max_profit(stock_prices)
```

    ## [1] 6

Maybe a nested loop can give a solution that is easier to implement.

``` r
get_max_profit2<-function(stock_prices){
  c=r=0
  for (i in 1:length(stock_prices)){
    for (j in i:length(stock_prices)){
      buy=stock_prices[i];sell=stock_prices[j]
      c=sell-buy
      r=max(c,r)
    }
  }
  return(r)
}
```

``` r
stock_prices = c(10, 7, 5, 8, 11, 9)
get_max_profit2(stock_prices)
```

    ## [1] 6
