library(dplyr)
library(stringr)

###wild card matching/leetcode 44
s = "aa"
p = "a"
grepl(glob2rx(p), s)
#Output: false
s = "aa"
p = "*"
grepl(glob2rx(p), s)
#Output: true
s = "cb"
p = "?a"
grepl(glob2rx(p), s)
#Output: false
s = "adceb"
p = "*a*b"
grepl(glob2rx(p), s)
#Output: true
s = "acdcb"
p = "a*c?b"
grepl(glob2rx(p), s)
#Output: false

###there is a very simple answer to this question
grepl(glob2rx(p), s)

###This needs to be debug, but I will leave it for Sunday now
wild_matching<-function(s, p){
  #base case
  if(!grepl("[?|*]", p) ) { 
    if( !nchar(s)==nchar(p) ){ return (FALSE) } ##when the length is not the same
     else { return (identical(s,p)) } #where wildcard char is not present
} else {
         s=s%>%strsplit(split=character(0))%>%unlist()
         p=p%>%strsplit(split=character(0))%>%unlist()
         n=length(s)
         match_vector=(s==p)
         if ("?"%in%p) { match_vector[p=="?"]=TRUE}
         if ("*"%in%p) {
           if (length(p)==1) return(TRUE)
           else {
             ##the p[*]+1 can be found in s | * is the very last char, safe to return true
             if(p[length(p)]=="*") return (TRUE)
             if(p[which(p=='*')+1]%in%s) return(TRUE)
           }
         }
         
         }
         
  if(sum(match_vector)<n){ return(FALSE) }
  else { return (TRUE)}
}





### Add Two Numbers/leetcode
input="(2 -> 4 -> 3) + (5 -> 6 -> 4)"
Output: 7 -> 0 -> 8
#Explanation: 342 + 465 = 807.


###split the string with +
library(stringr)
input=input%>%strsplit(split= '+', fixed=T)%>%unlist()
# input=sapply(input, function(input) gsub(" -> ", "", input)) 
# input=sapply(input, function(input) strsplit(input,split=character(0)))
# input=unlist(input)
# input=sapply(input, function(input) rev(input))

    
### remove -> symbol,  reverse the string within () seems too much work at this point
###how about just extract the numbers instead

input=gsub("[^0-9]", '', input)
input=stringi::stri_reverse(input)
input=paste(input, sep="", collapse="+")
result=eval(parse(text=input))%>%as.character()
result=stringi::stri_reverse(result)
result=paste(strsplit(result, split=NULL)%>%unlist(), collapse = ' -> ')

###put it all together
input="(2 -> 4 -> 3) + (5 -> 6 -> 4)"

add_2num<-function(input){
  #break the string and extract the numbers
  #rev and evaluate
  #rev the result
  input=input%>%strsplit(split= '+', fixed=T)%>%unlist()%>%
             gsub('[^0-9]', '', .)
  input=input%>%stringi::stri_reverse()%>%paste(.,sep="", collapse="+")
  result=eval(parse(text=input))%>%as.character()%>%stringi::stri_reverse()
  result=result%>%strsplit(split=NULL)%>%unlist()%>%paste(.,collapse=" -> ")
  return (result)
 
}

add_2num(input)
######Apple Stock problem
stock_prices = c(10, 7, 5, 8, 11, 9)

p=max_p=0
n=length(stock_prices)
for(i in 1:n){
  for(j in i:n){
    p=stock_prices[j]-stock_prices[i]
    max_p=max(p,max_p)
    print(c(p, max_p))
  }
}


###factor resursive function

get_factor<-function(n){
  if (n==1) return (n)
  else return (n*get_factor(n-1) )
}

get_factor(3)

####zigzag conversion
string= "PAYPALISHIRING" 
nrow=3

###if the last column is the has two letters, this function create infinite loop
zigzag<-function(string, nrow){
  n=nrow
  string=string%>%strsplit(split=NULL)%>%unlist()
  final_matrix=matrix(nrow=nrow, ncol=0)
  w_v=matrix(nrow=nrow, ncol=1)

  ##initiate the first column
  w_v[1:nrow]=string[1:nrow]
  string=string[(nrow+1):length(string)]
  final_matrix=cbind(final_matrix, w_v)
  w_v=matrix(nrow=nrow, ncol=1)
  
while (!string%>%is.na()%>%sum()>=n){ ###when there is character left for a full word vector
  
  if(final_matrix[,ncol(final_matrix)]%>%is.na()%>%sum()==0){
  ###when the last word_vector is a full vector -- start from the bottom
    w_v[nrow-1]=string[1]
    string=string[2:length(string)]
    final_matrix=cbind(final_matrix, w_v)
    w_v=matrix(nrow=nrow, ncol=1)
  } else if(final_matrix[,ncol(final_matrix)]%>%is.na()%>%sum()==n-1){
     pos=which(!final_matrix[,ncol(final_matrix)]=='NA')
      if(pos==2) {
        ##if the last position is second to the top --next word vector is a full string
        w_v[1:nrow]=string[1:nrow]
        string=string[(nrow+1):length(string)]
        final_matrix=cbind(final_matrix, w_v)
        w_v=matrix(nrow=nrow, ncol=1)
      } else{
        ###if the position is not second to the top --fill +1 position towards the top
        w_v[pos+1]=string[1]
        string=string[2:length(string)]
        final_matrix=cbind(final_matrix, w_v)
        w_v=matrix(nrow=nrow, ncol=1)
        }
  } 
}
  return(final_matrix)
}

#####30.Substring with Concatenation of All Words
s = "barfoothefoobarman"
words = c("foo","bar")

n=length(words)
for(i in 1:n){
  p=paste(i, words[i])
  assign(p, paste("p", i))
}

##

##actually this need to be pertumation
###write a permutation function
get_permute<-function(n){
  if(n==1) return ( matrix(n) ) ###it is important to coerce the base case into a matrix, otherwise the next step would not work
  else {
    A=get_permute(n-1)
    B=matrix(nrow=0, ncol=n)
    for (i in 1:nrow(A)){
      for (j in 0:(n-1)){
        B=rbind(B, append(A[i,], n, after=j)) ###make each line of B as we go
      }
    }
    return(B)
  }
}

get_permute(3)

###permute elements rather than numbers
get_permute2<-function(input){
  n=length(input)
  if(n==1) return (matrix(input))
  else{
    A=get_permute2(input[-1])
    B=matrix(nrow=0, ncol=n)
    for (i in 1:nrow(A)){
      for (j in 0:(n-1)){
        B=rbind(B, append(A[1,], input[1], after=j))
      }
    }
    return(B)
  }
}

get_permute2(words)

###we can also use the permutation functin in the r package permute
words_perms=get_permute2(words)
word_v=NULL
for (i in 1:nrow(words_perms)) {
  word_v=c(word_v, paste(words_perms[i,], collapse="") )
}
  



gregexpr(word_v[2], s, fixed = T)[[1]][1]

####put in all together
s = "barfoothefoobarman"
words = c("foo","bar")

find_substring<-function(s, words){
  ###produce the cat words string by ###introduce the permutate function
  get_permute<-function(input){
    n=length(input)
    if(n==1) return (matrix(input))
    else{
      A=get_permute(input[-1])
      B=matrix(nrow=0, ncol=n)
      for (i in 1:nrow(A)){
        for (j in 0:(n-1)){
          B=rbind(B, append(A[1,], input[1], after=j))
        }
      }
      return(B)
    }
  }

  words_perms=get_permute(words) 
  word_v=NULL
  for (i in 1:nrow(words_perms)) {
    word_v=c(word_v, paste(words_perms[i,], collapse="") )
  }
  
  ###locate all the elements in word_v in s -> gregexpr function first index
  result=NULL
  for (i in 1:length(word_v)) {
    index=gregexpr(word_v[i], s, fixed = T)[[1]][1]
    result=c(result, index)
  }
  
   return(result)
    
}

find_substring(s,words)

###another example
s = "wordgoodstudentgoodword"
words = c("word","student")

###Advanced R
Z <- stats::rnorm(10000)
table(cut(Z, breaks = -6:6))
sum(table(cut(Z, breaks = -6:6, labels = FALSE)))
sum(graphics::hist(Z, breaks = -6:6, plot = FALSE)$counts)


###word search 212
words = c("oath","pea","eat","rain")
board =list(
  c('o','a','a','n'),
  c('e','t','a','e'),
  c('i','h','k','r'),
  c('i','f','l','v')
)

###transfer the board into a matrix
n=length(board)
board=board%>%unlist()%>%
  matrix(., nrow = n, byrow=T)

##iterate over each word

# if (first pos found){
#   is the next pos at [i+1,j]|[i,j+1]|[i+1, j+1]
# }
w_3=words[3]%>%strsplit(split=NULL)%>%unlist()
index=list()
# for (i in 1:length(w_1)){
#   index[[length(index)+1]]=which(board==w_1[i], arr.ind = T)
# }

#extract the index of the character position
index=map(w_3, function(w_3)which(board==w_2, arr.ind = T))

###in the case of the first word, the first index exist and is unique
i=index[[1]][1]
j=index[[1]][2]
###what to do when the indexes are not unique?

##if one of the rowSum return 2, that means that the second char in the word can be located
(c(i, j+1)==index[[2]])%>%rowSums() 
(c(i+1, j)==index[[2]])%>%rowSums()
(c(i+1, j+1)==index[[2]])%>%rowSums()

#update i and j
i=i
j=j+1

###if one of the rowSum return 2....
(c(i, j+1)==index[[3]])%>%rowSums()
(c(i+1, j)==index[[3]])%>%rowSums() 
(c(i+1, j+1)==index[[3]])%>%rowSums()

###update i and j
i=i+1
j=j

###run through the whole routine again 
(c(i, j+1)==index[[4]])%>%rowSums()
(c(i+1, j)==index[[4]])%>%rowSums() 
(c(i+1, j+1)==index[[4]])%>%rowSums()

###last character, paste the word into a result vector
result=c(result, word)

###cases when all the char can be located, paste the word into a result vector
###cases when any of the character in the word can not be located, do nothing


###interleaving string /leetcode 97
s1 = "aabcc"; s2 = "dbbca"; s3 = "aadbbcbcac"
#Output: true

s1 = "aabcc"; s2 = "dbbca"; s3 = "aadbbbaccc"
#Output: false
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

interLeaving(s1,s2,s3)



###scramble string
union(c("a", "b"), c("a", 'c'))




####minal widown string
S = "ADOBECODEBANC"
tt = "ABC"
#Output: "BANC"

###break the string T
min_windown_count<-function(S,tt){
  
t=tt%>%strsplit(split=NULL)%>%unlist()

index=map(t, function(t)gregexpr(t, S, fixed=T))

###extract all the indexes, form the subset and see which one has the shortest length
col1=lapply(index, '[[', 1)%>%lapply(., '[[', 1)%>%unlist()
col2=lapply(index, '[[', 1)%>%lapply(., '[[', 2)%>%unlist()
index_c=cbind(col1, col2)
index_c=t(index_c)
###need to expand.grid on the different combination of the position index and apply range to get the min_window
expand.grid(index_c[,1], index_c[,2], index_c[,3])


###got stuck and do not know how to make the different combination of columns

}


# We build all combinations of names, greetings and separators from our
# list of data and pass each one to paste()
data <- list(
  id = c("John", "Jane"),
  greeting = c("Hello.", "Bonjour."),
  sep = c("! ", "... ")
)

data %>%
  cross() %>%
  map(lift(paste))



###there is alternative solution for this solution which is more mechanical and less mathy, and here it is
min_window<-function(S, tt){
  ###decompose the string
s=S%>%strsplit(split = NULL)%>%unlist()
t=tt%>%strsplit(split = NULL)%>%unlist()

if (!sum(t%in%s)==length(t)) return (c(' '))

c=0
r=length(s)

for (i in 1:length(s)){
    for (j in 1:length(s)){
     
      if ((t%in%s[i:j])%>%sum()==length(t)) { 
        c=length(s[i:j])
        r=min(c,r)
      }
      
  }
}
return (r)
}

min_window(S,tt)


###214.Shortest Palindrome
input= "aacecaaa"
input="abcd"

shortest_palin<-function(input){
  ##input is a string
  if (input==stringi::stri_reverse(input)) return (input)
  else {
    
  s=input%>%strsplit(split=NULL)%>%unlist()
  ###base case---nothing needs to be done, already a palin
  
  ###potential string---add string in front
  n=length(s)
  for (i in 1:n){
    if (identical(s[1:i],rev(s[1:i]))){
      
        if (!i==n) add_string=rev(s[(i+1):n])
        else  add_string=rev(s[2:n])
    }
  }
  
  add_string=paste(add_string, sep="", collapse = "")
  result_s=paste(add_string, input, sep='', collapse='') 
  }
  ###no potential string and need to rev [2:n] and piece that in front
  return (result_s)
}

shortest_palin(input)



######word search 212 ---second attempt
words = c("oath","pea","eat","rain")
board =list(
  c('o','a','a','n'),
  c('e','t','a','e'),
  c('i','h','k','r'),
  c('i','f','l','v')
)


###transform the board into a matrix first
n=length(board)
board = board %>% unlist() %>%
        matrix(., nrow = n, byrow=T)

check_board<-function(word){
  ###this function returns TRUE when all the chars in the words can be located on the board
  word = word %>% strsplit(split = NULL) %>% unlist()
  pos = map(word, function(word)which(board==word, arr.ind = T)) 
  
  if (map(pos, length) %>% unlist() %>% sum() < length(pos)*2) return (FALSE)
  else return (TRUE)
  
}

check_link <- function(word){
  ###return the index of the word if check link test is passed, otherwise, return nothing
  word = word %>% strsplit(split = NULL) %>% unlist()
  pos = map(word, function(word)which(board==word, arr.ind = T))
  
  ##initial the first index---what if length > 2
  i=pos[[1]][1]
  j=pos[[1]][2]
  
  
  
}


###128.Longest Consecutive Sequence
input=c(100, 4, 200, 1, 3, 2)
#Output: 4

max_length <- function(){
  sorted_input = sort(input)
  c=r=0
  for (i in 1:length(input)){
    number=sorted_input[i]
    number_vector=number
    
    if (sorted_input[i+1] - number == 1){
      next_number=sorted_input[i+1]
      number_vector=c(number_vector, next_number)
      c=length(number_vector)
      r=max(c,r)
      
    } 
  }
}



###merge sorted list
list = c('1->4->5', '1->3->4', '2->6')

merge_sort <- function(list){
###first strip of the all the non-numeric symbols, convert into numbers, then sort
  list=gsub('[^0-9]', "", list) %>%
     strsplit(split=NULL) %>%
     unlist() %>%
     as.numeric() %>% 
     sort()

###then coerce into characters again to put the symbols back in
  list = list %>% as.character()  %>%
          paste (collapse=" -> ")

  return (list)
}

merge_sort(list)     



###alien dict/leet 269
list=c(
  "wrt",
  "wrf",
  "er",
  "ett",
  "rftt"
)

###this tells me how many characters I need to find the order of
character = map(list, function(list)strsplit(list, split=NULL)) %>% 
          unlist() %>%
          unique()

###this return the sequence info getting for the first letter of each entry
first_letter = substr(list, start=1, stop=1) %>%
               unique()

###extract the common sequence will give more information about character order
###write a function to extract common strings
lcs <- function (s1, s2) {
  ###both s2 and s2 has been strsplit into letter sequence
  common=NULL
  for (i in 1 : length(s1)){
    for (j in 1: length(s2)){
      if (s1[i] == s2[j] & s1[i+1] == s2[j+1])
        common = c (common, s1[i], s1[i+1])
    }
  }
  return (common)
}

lcs(w1, w2)

###this is a function that I wrote a long time ago, let's call it lcs2, it probably works more accurately
lcs2<-function(a, b){
  a=a%>%strsplit(split=character(0))%>%unlist()
  b=b%>%strsplit(split=character(0))%>%unlist()
  
  common=a[which(a%in%b)]
  ##note that the order of the argument matters, intersect always return the common elements in the order of the first argument
  index=numeric()
  for (i in 1:length(common)){
    index=c(index, which(b==common[i]))
  }
  
  r=1;c=0
  q=index[1]
  for (i in 2:length(index)){
    if (index[i]>q[length(q)]) q=c(q,index[i])
    else q=index[i]
    
    c=length(q)
    r=max(r, c)
  }
  return (r)
}

##write a function to extract order info from two words
extract_order <- function (w1_str, w2_str) {
  ##w1 and w2 being two strings
  w1 = w1_str %>% strsplit(split=NULL) %>% unlist()
  w2 = w2_str %>% strsplit(split=NULL) %>% unlist()
  common = LCS(w1, w2)$LCS
  common_str = paste (common, collapse = '')
  position_list = regexpr(common_str,c(w1_str,w2_str), fixed = T) %>% unlist ()
  position = position_list[[1]] + attributes(position_list)$match.length

  pos_1 = position[1] ##this is the position in w1
  pos_2 = position[2] ### this is the position in w2

###restore the position of two letters in the alien dict
  order_sequence = c (
    w1_str %>% substr(start=pos_1, stop =pos_1),
    w2_str %>% substr(start=pos_2, stop =pos_2)
                    )
  
  return (order_sequence)
}

##test the function
seq2 = extract_order (w1_str, w2_str)

seq3 = extract_order (list[3], list[4])

###try to merge the first_letters sequence and the 2nd sequence
pull_letter = c (first_letter, seq2, seq3) %>% unique()


####convert numbers to English
library(english)
english(1:10)^2 + 1:10
(x <- english(sample(1:100, 10)))
sort(x)
toupper(english(1:10))
## For mothers of small children:
cat(paste("This is the", ordinal(1:5), "time I've told you!"), sep = "\n")


english()


library(pryr)

LCS
function (a, b) 
{
  stopifnot(is.character(a), is.character(b))
  if (any(is.na(c(a, b))) | any(list(a, b) == "")) {
    out <- list(a = a, b = b, LLCS = NA, LCS = NA, QSI = NA, 
                va = NA, vb = NA)
  }
  else {
    out <- .Call("lcs", as.character(a), as.character(b), 
                 max(nchar(c(a, b))), PACKAGE = "qualV")
  }
  invisible(out)
}

objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)

exists("a")

x <- list(1:3, 4:9, 10:12)
sapply(x, "[", 2)
sapply(x, function(y) y[2])


args <- list(1:10, na.rm = TRUE)
do.call(mean, args)

english


#######273.Integer to English Words

num_to_words <- function (num) {
  if (length(num) == 0) return ("Input must be a valid number.")
  if (num == 0) return ('Zero')
  
  LESS_THAN_20 = c ("", "One", "Two", "Three", "Four", "Five", 
                    "Six", "Seven", "Eight", "Nine", "Ten", 
                    "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", 
                    "Sixteen", "Seventeen", "Eighteen", "Nineteen")
  
  TENS = c ("", "Ten", "Twenty", "Thirty", "Forty", "Fifty", 
            "Sixty", "Seventy", "Eighty", "Ninety")
  
  THOUSANDS = c ("", "Hundred", "Thousand", "Million", "Billion")
  
  ###convert num into a string, split it and find the correspoding words
  
  num_char = num %>% as.character( ) %>%
            strsplit (split=NULL) %>%
            unlist ()
  
  n = length (num_char)

###first, deal with anything before 100
  last2_digits = num_char [(n-1):n]
  before2_digits = num_char [1:(n-2)]
  
  if ( last2_digits %>% paste(sep="", collapse="") <= 20) {
      last2_digits_char = LESS_THAN_20 [last2_digits %>% as.numeric() +1 ]
  } else {
      last_digit = num_char[n]
      last_digit_char = LESS_THAN_20 [last_digit %>% as.numeric() +1 ]
      last_but2_digit = num_char[n-1]
      last_but2_digit_char = TENS [ last_but2_digit %>% as.numeric() +1 ]
      
      last2_digits_char = paste (last_but2_digit_char, last_digit_char)
  }
  
###deal with everything after 100
  before2_digits_char=NULL
  for (i in 1: length(before2_digits) ) {
    before2_digits_char = c(before2_digits_char, 
                            paste(LESS_THAN_20[(rev(before2_digits[i] %>% as.numeric())+1)], 
                                  THOUSANDS[i+1]))
    before2_digits_char =rev (before2_digits_char)
  }
    

 return ( cat (before2_digits_char, last2_digits_char, collapse=" ")) 
}

num_to_words(6834)


###282.Expression Add Operators
num = "123"; target = 6
operators <- c ('+', '-', '*')

###right now the function only works for three digit numbers, needs more modification
add_operators <- funciton (num, target) {
  ##split the num
  num = num %>% as.character() %>%
        strsplit (split=NULL) %>%
        unlist()
  
  ###make permutation of all the possible answers
  res = expand.grid(num[1], operators) ###for the first digit
  res = paste(res[,1], res[,2]) 
  res = expand.grid(res,num[2],operators) ##for the 2nd onward ---this needs to be changed
  res = paste (res[,1], res[,2], res[,3])
  res = paste (res, num[length(num)]) ##for the last digit

 results=numeric(length(res))
  for (i in 1:length(res)) {
     results[i] = eval(parse(text=res[i]))
  }
 
  return ( res [ results == target ])
}


### word pattern 2/291
p_str= "ba"; s_str = "xyzabcxzyabc"

word_pattern <- function (p_str, s_str) {
  
  ###if the pattern is not in s | there is only one letter, return T
  p = p_str %>% strsplit (split=NULL) %>%
        unlist()
  
  s = s %>% strsplit (split=NULL) %>%
    unlist()
  
  if ((p %in% s) %>% sum() == 0 | p %>% unique() %>% length() ==1 ) return (TRUE)
  
  common = p [ which (p %in% s)]
  ###if the position of "a" has a value that is smaller than "b" than returns "F"
  index = sapply (common, function(x)gregexpr(x, s_str, fixed = T) [[1]])
  
  if (any (index[,1] > index[,2])) return (F) ###this needs to be modified
    
}


#####Smallest Rectangle Enclosing Black Pixels 
image= c(
  "0010",
  "0110",
  "0100"
      )

find_min_reg <- function (image) {
  ###transform the image into a matrix
  image_matrix = strsplit(image, split=NULL) %>% unlist() %>%
                matrix (nrow = length(image), byrow = T)
        
  index = which (image_matrix == '1', arr.ind = T) 

  index_row = index[,1] ; index_col = index[,2]
  row_span = range(index_row)[2] - range(index_row)[1] + 1 
  col_span = range(index_col)[2] - range(index_col)[1] + 1 

  retangle_area = row_span * col_span
  
  return (retangle_area)
}

find_min_reg(image)  


####Range Sum Query 2D - Immutable

matrix = c(
  c(3, 0, 1, 4, 2),
  c(5, 6, 3, 2, 1),
  c(1, 2, 0, 1, 5),
  c(4, 1, 0, 1, 7),
  c(1, 0, 3, 0, 5)
)

matrix = matrix (matrix, nrow = 5, ncol = 5, byrow = T)

range_sum <- function (matrix,  border) {
  ###extract row span and col span
  submatrix = matrix [border[1]:border[3], 
                      border[2]:border[4]]
  
  return (sum(submatrix))
}

border = c (3, 2, 5, 4)
border = c (2, 2, 3, 3)

range_sum (matrix, border)


###315.Count of Smaller Numbers After Self
input = c(5,2,6,1)
#Output: [2,1,1,0] 

count_smaller <- function (input) {
  n = length(input)
  count_smaller = numeric(n)
  for (i in 1:(n-1)){
    count_smaller[i] = (input[(i+1):n] < input[i]) %>% sum()
  }
  
  count_smaller[n] = 0
  
  return (count_smaller)
}

count_smaller(input)


###remove duplicated letters 316
input = c('b', 'c', 'a', 'b', 'c')

unique(input)


###321. Create Maximum Number

###first, try to realize retrieving last k digit to the order of the original num_seq
num=c(5,9, 1, 2, 5, 8, 3)

n=length(num)
vec=num[1]
for (i in 1:n){
  if (num[i]>vec[length(vec)]){
   vec=NULL
   vec=c(vec, num[i])
     for (j in i:n){
       vec=c(vec, num[j])
     }
       
  }
  
}



####354.Russian Doll Envelopes
input=list(c(5,4),c(6,4),c(6,7),c(2,3))

num_env <- function (input){
  ###turn the input into a matrix
  n=length(input)
  input = input %>% unlist() 
  input = matrix(input, nrow = n, byrow = T)
  
  w = max(input[,1]) ; h = max(input[,2])
  
  ###find the env that would fit into this first
  num=1
  for (i in 1:n) {
    if (input[i,1] < w & input[i,2] < h) {
      num = num + 1
      w = input[i,1] ; h = input[i,2]
    }
  }
  return (num)
}

num_env(input)


####word break
s = "catsanddog"
wordDict = c("cat", "cats", "and", "sand", "dog")

###brainstorm idea--this would return the location of all the words
index = sapply(wordDict, function(x)gregexpr(x, s, fixed = T))
###the start position and attribute matchLength is going to help locate the word
### if encounter 1, segment the vector 

word_break <- function (s, wordDict) {
  ###locate all the words
  index = sapply(wordDict, function(x)gregexpr(x, s, fixed = T))
  ###what if the index is empty?
  s_split = s %>% strsplit(split=NULL) %>% unlist()
  words=NULL
  for (i in 1:length(index)) {
    word_retrieve =  s_split[index[[i]][1] : ( index[[i]][1] + attributes(index[[i]])$match.length -1)]
    word_retrieve = paste (word_retrieve, collapse = "")
    words= c (words, word_retrieve)
  }
  
  return (words)
}

word_break (s, wordDict)

s = "pineapplepenapple"
wordDict = c("apple", "pen", "applepen", "pine", "pineapple")



### find common substring
s1="ABCD"; s2="EAB"

common_sub <- function(s1, s2) {
  s1 = s1 %>% strsplit(split=NULL) %>% 
          unlist()
  
  s2 = s2 %>% strsplit(split=NULL) %>% 
          unlist()
  
  common_sub = NULL
  for (i in 1: (length(s1)-1)){
    for (j in 1: (length(s2)-1)){
      
        if (s1[i] == s2[j] & s1[i+1] == s2[j+1]){
          common_sub = c(common_sub, s1[i], s1[i+1])
        }
    }
    
  }
  
  common_sub = paste0(common_sub, collapse="")
  return (common_sub)
}

common_sub(s1, s2)


###tower of hannoi
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

hanoi(2, "begin", "spare", "end")

#####mininal cut
s = "kayakmadamotto"

min_palincut<-function(s){
  s=s%>%strsplit(split=character(0))%>%unlist()
  
  if (identical(s, rev(s))) return(0)
  ###establish how to evaluate whether a string is a palin or not
  cut=seq(-1,length(s)-2) ###get a vector to store the worse case scenario when no palin is found
  for (i in 1:length(s)){
    for (j in i:length(s)){
      if (identical(s[i:j], rev(s[i:j]))){
        cut[j] = min(cut[j],cut[i-1]+1) 
      }
    }
  } 
  return(cut[length(cut)])
}

unique_path<-function(m, n){
  dp=matrix(nrow=m, ncol=n)
  dp[1,]=1;dp[,1]=1;
  for (i in 2:m){
    for (j in 2:n){
      dp[i,j]=dp[(i-1),j]+dp[i,(j-1)]
    }
  }
  return(dp[nrow(dp), ncol(dp)])
}