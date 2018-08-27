Optimal Account Balancing
================
Liwen Huang
8/27/2018

A group of friends went on holiday and sometimes lent each other money. For example, Alice paid for Bill's lunch for 5 for a taxi ride. We can model each transaction as a tuple (x, y, z) which means person x gave person y $z. Assuming Alice, Bill, and Chris are person 0, 1, and 2 respectively (0, 1, 2 are the person's ID), the transactions can be represented as \[\[0, 1, 10\], \[2, 0, 5\]\].

Given a list of transactions between a group of people, return the minimum number of transactions required to settle the debt.

Note:

A transaction will be given as a tuple (x, y, z). Note that x ≠ y and z &gt; 0. Person's IDs may not be linear, e.g. we could have the persons 0, 1, 2 or we could also have the persons 0, 2, 6.

Example 1:

Input: \[\[0,1,10\], \[2,0,5\]\]

Output: 2

Explanation: Person \#0 gave person \#1 $10. Person \#2 gave person \#0 $5.

Two transactions are needed. One way to settle the debt is person \#1 pays person \#0 and \#2 $5 each.

Example 2:

Input: \[\[0,1,10\], \[1,0,1\], \[1,2,5\], \[2,0,5\]\]

Output: 1

Explanation: Person \#0 gave person \#1 $10. Person \#1 gave person \#0 $1. Person \#1 gave person \#2 $5. Person \#2 gave person \#0 $5.

Therefore, person \#1 only need to give person \#0 $4, and all debt is settled.

This is a fun problem to solve

``` r
min_trans<-function(trans){
  #trans is a list of all the transition, each a list of three numbers
leger=matrix(nrow=0, ncol=2)
##the easy thing to do is to figure out the debt the surplus situation on each account
for (i in 1:length(trans)){
  giver=trans[[c(i, 1)]];taker=trans[[c(i, 2)]];amount=trans[[c(i, 3)]]
  leger=rbind(leger, c(giver, -amount))
  leger=rbind(leger, c(taker, amount))
}
colnames(leger)=c('account', 'balance')
leger=aggregate(balance~account,data=leger, FUN=sum)
###initiate a value that is the worst case scenario, which is the number of original tuples
###outer loop iterates all the element with increment of 1, inner loop iterate all the element after i
###add to the balance that is not the same signs
###skipping 0 account
worst_case=length(trans)
m=nrow(leger)
operation=0
for (i in 1:m){
  for (j in i:m){
    if (leger[i,'balance']!=0 & leger[j,'balance']!=0 & sign(leger[i,'balance'])!=sign(leger[j,'balance'])){
      leger[j,'balance']=sum(leger[i,'balance'],leger[j,'balance'])
      operation=operation+1
    }
  }
}
return (min(worst_case, operation))
}
```

Test our function

``` r
trans=list(c(0,1,10), c(2,0,5))
min_trans(trans)
```

    ## [1] 2

``` r
trans2=list(c(0,1,10),c(1,0,1),c(1,2,5),c(2,0,5))
min_trans(trans2)
```

    ## [1] 1
