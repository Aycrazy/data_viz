## IntRo!

#Atomic Vector - 1d homogenous
vec <- c(1,2,3,4)

class(vec) #class of object
typeof(vec) #R's internal type
str(vec) #structure

5*6 #each number is an atomic vector of len() = 1

vec2<- vec #copy of vec

vec <- c("a","b","c")

vec2

class(vec)
typeof(vec)
str(vec)

#R is index to 1 (not 0)
vec[1]

#note returns character(0)
vec[0]

#2 dimensional homogenous data structures in R
mat <- matrix(c(1,2,3,4),2,2)

mat
class(mat)
typeof(mat)

mat[1,1]
mat[2,1]

test<- list(c(1,2),"cat", mtcars)

test[[3]]

#sample data
data()
data(mtcars)

drugs <- read.csv("NSDUH.csv")

head(drugs)

head(x=drugs)

head(drugs,3)

hist(drugs$AGE2)

#$ = within

?hist

hist(x=drugs$Age2)

hist(drugs$AGE2, 8, main="Hist of Age")


#dplyr, tidyr, stringr functions and all dplyr functions don't require $ operator
count(drugs, CIGEVER)

#chain operator ^ same as above
drugs %>% count(CIGEVER)

drug_lim <- drugs %>%
        select(CIGEVER) %>%
        filter(CIGEVER == "Yes")

drug_lim
