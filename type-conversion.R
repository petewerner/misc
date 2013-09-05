a <- c(1,2,3)
sum(a)
#[1] 6
a
#[1] 1 2 3
a <- c(a, "hi")
a
#[1] "1"  "2"  "3"  "hi"
sum(a)
#Error in sum(a) : invalid 'type' (character) of argument

a <- c(1,2,3)
typeof(a)
#[1] "double"
is.numeric(a)
#[1] TRUE

a <- c(a, "hi")
typeof(a) 
#[1] "character"
is.numeric(a)
[1] FALSE

n <- 5
df <- data.frame(cbind(rbinom(n, 1, 0.5), rnorm(n, 10, 5), rnorm(n, 20, 10)))
df[,1] <- as.factor(df[,1])
head(df)
#  X1        X2       X3
#1  1  8.911567 27.28325
#2  1  9.933021 13.74879
#3  0 10.177231 20.65490
#4  0  6.368177 27.10183
#5  1 12.084135 14.54369
apply(df, 1, function(x) x[2] + x[3] )
#Error in x[2] + x[3] : non-numeric argument to binary operator

df[,2] + df[,3]
#[1] 36.19481 23.68181 30.83213 33.47001 26.62783
apply(df, 1, function(x) mode(x))
#[1] "character" "character" "character" "character" "character"
# could also be run as 
# apply(df, 1, mode)
 
a <- df[1,]
a
#  X1       X2       X3
#1  1 8.911567 27.28325
mode(a)
#[1] "list" 

#If X is not an array but an object of a class with a non-null dim value (such as a data frame), apply attempts to coerce it to an array via as.matrix if it is two-dimensional (e.g., a data frame) or via as.array.

b <- as.matrix(a)
b
#  X1  X2         X3        
#1 "1" "8.911567" "27.28325"
b[2] + b[3]
#Error in b[2] + b[3] : non-numeric argument to binary operator

#what if we just selected our numeric columns? (dont forget its now index 1 and 2)
apply(df[,2:3], 1, function(x) x[1] + x[2])
#[1] 36.19481 23.68181 30.83213 33.47001 26.62783

#data frames and strings as factors
df <- data.frame(cbind(paste("subject", 1:n, sep=''), rnorm(n, 10, 5), rnorm(n, 20, 10)))
#        X1               X2               X3
#1 subject1 14.6619839711866 6.94472759446703
#2 subject2  11.603910222178 27.6225162121889
#3 subject3 5.21881004622993 20.3409476386206
#4 subject4 16.3574724782284 39.0904723579448
#5 subject5 9.35407053787977 23.8568796326835
apply(df[,2:3], 1, function(x) x[1] + x[2])
#Error in x[1] + x[2] : non-numeric argument to binary operator
a <- df[,2:3]
mode(a)
#[1] "list"
mode(a[,1])
#[1] "numeric"
as.matrix(a)
#     X2                  X3                
#[1,] "-3.89274205212847" "12.7336046818466"
#[2,] "12.3494043977024"  "17.9329667214396"
#[3,] "4.7419241278816"   "16.0664073330786"
#[4,] "8.50784944656814"  "8.65139145569206"
#[5,] "9.56191506080518"  "21.2114650777001"

#strings?!

a[,1]
#[1] -3.89274205212847 12.3494043977024  4.7419241278816   8.50784944656814  9.56191506080518 
#Levels: -3.89274205212847 12.3494043977024 4.7419241278816 8.50784944656814 9.56191506080518
#why are you a factor??

df[,1]
#[1] subject1 subject2 subject3 subject4 subject5
#Levels: subject1 subject2 subject3 subject4 subject5

#its been turned into a factor. 
default.stringsAsFactors()
#[1] TRUE

df <- data.frame(cbind(paste("subject", 1:n, sep=''), rnorm(n, 10, 5), rnorm(n, 20, 10)), stringsAsFactors=FALSE)
as.matrix(df[,2:3])
#     X2                 X3                
#[1,] "7.19530271823023" "26.4186991862312"
#[2,] "13.6715492467442" "25.452128137706" 
#[3,] "8.89363806613213" "20.1618970554355"
#[4,] "16.296512734304"  "16.2581582721134"
#[5,] "11.6454577442585" "17.5241594066948"

#why are they still strings? cbind is the culprit

cbind(paste("subject", 1:n, sep=''), rnorm(n, 10, 5), rnorm(n, 20, 10))
#     [,1]       [,2]                [,3]              
#[1,] "subject1" "14.0342542696833"  "30.5672885598002"
#[2,] "subject2" "8.44141744459018"  "35.1337567509022"
#[3,] "subject3" "11.6550656524794"  "10.1554349193507"
#[4,] "subject4" "18.0303214118231"  "14.9638066872277"
#[5,] "subject5" "0.180686583194847" "11.7124424267387"

#dropping the cbind gives us what we are after

df <- data.frame(paste("subject", 1:n, sep=''), rnorm(n, 10, 5), rnorm(n, 20, 10))
as.matrix(df[,2:3])
#     rnorm.n..10..5. rnorm.n..20..10.
#[1,]       13.557665        33.519719
#[2,]       15.086483        41.457651
#[3,]        7.010492         1.757224
#[4,]       11.008779        29.707944
#[5,]       15.777351        10.280138
apply(df[,2:3], 1, function(x) x[1] + x[2])
#[1] 47.077384 56.544134  8.767716 40.716723 26.057489

a <- c(1:3, "hi")
storage.mode(a)
#[1] "character"
storage.mode(a) <- 'integer'
#Warning message:
#In storage.mode(a) <- "integer" : NAs introduced by coercion
a
#[1]  1  2  3 NA