## proper roll functions as per the book

## Object and functions require to roll 2 dice and sum them.
roll <- function() {
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE)
  sum(dice)
}

## Test the distribution:
## Always require to load the library since R won't.
rolls <- replicate(1000000, roll())
library("ggplot2")
qplot(rolls, binwidth=1)

## Modify the distribution by weighting the dice:
## Modify the roll function with weighted dice
roll <- function() {
  die <- 1:6
  dice <- sample(die, size=2, replace=TRUE, prob=c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
  sum(dice)
}


## Calculate the expected outcome
die <- 1:6
rolls <- expand.grid(die, die)
rolls$value <- rolls$Var1 + rolls$Var2

prob <- c("1"=1/8, "2"=1/8, "3"=1/8, "4"=1/8, "5"=1/8, "6"=3/8)
rolls$prob1 <- prob[rolls$Var1]
rolls$prob2 <- prob[rolls$Var2]
rolls$prob <- rolls$prob1 * rolls$prob2

sum(rolls$value * rolls$prob)


## Old code created to test along the way with the book - refer to above for
## official version of each method from the book.
roll <- function(num = 1) {
  rollCustomDie(num = num)
}

rollCustomDie <- function(die = 1:6, num = 1) {
  # Die must be an integer vector.
  sample(x = die, size = num, replace = TRUE)
}
