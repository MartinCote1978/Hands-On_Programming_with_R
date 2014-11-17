## Reading in the deck.csv file
deck <- read.csv("deck.csv")

## Save the file as a csv
write.csv(deck, file="cards.csv", row.names=FALSE)

## Return the first row of a card
deal <- function(cards) {
  cards[1, , drop=FALSE]
}

## Shuffle the deck
shuffle <- function(cards) {
  random <- sample(1:52, size=52)
  cards[random, , drop=FALSE]
}

## NOT WORKING; producing an incorrect, unexpected data.frame
shuffleAndDeal <- function(cards) {
  deal(shuffle(cards))
}

## Test probability of each cards
deck5 <- deck
deck5 <- replicate(1, shuffleAndDeal(deck5))

## To play War
## Aces are worth 14; modify their values:
deck3 <- deck
deck3$value[deck3$face == "ace"] <- 14

## To play Hearts
## All cards are worth a value of 0:
deck4 <- deck
deck4$value <- 0
## Except the 'hearts' at 1 and the queen of spades at 13:
deck4$value[deck4$suit == "hearts"] <- 1
deck4$value[deck4$suit == "spades" & deck4$face == "queen"] <- 13

## To play Blackjacks
deck5 <- deck
facecard <- deck5$face %in% c("king","queen","jack")
deck5$value[facecard] <- 10
deck5$value[deck5$face == "ace"] <- NA


## Deal the first card and then remove it
deal <- function() {
  card <- deck[1, ]
  #deck <- deck[-1, ] # Equivalent below, overwriting the Global Env "deck"
  assign("deck", deck[-1, ], envir = globalenv())
  card
}

## Return all cards to the deck and shuffle the deck
shuffle <- function() {
  random <- sample(1:52, size=52)
  assign("deck",DECK[random, , drop=FALSE], envir = globalenv())
}

