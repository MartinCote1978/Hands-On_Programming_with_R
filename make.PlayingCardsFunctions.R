## Build all functions required to deal and shuffl a deck of cards, including
## loading the deck itself and preparing the main functions (deal & shuffle)
## within the global environment
make.PlayingCardsFunctions <- function(deck) {
  DECK <- deck
  
  ## Deal the first card and then remove it
  DEAL <- function() {
    card <- deck[1, ]
    #deck <- deck[-1, ] # Equivalent below, overwriting the Global Env "deck"
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  
  ## Return all cards to the deck and shuffle the deck
  SHUFFLE <- function() {
    random <- sample(1:52, size=52)
    assign("deck",DECK[random, , drop=FALSE], envir = parent.env(environment()))
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

## Reading in the deck.csv file
deck <- read.csv("deck.csv")

cards_func <- make.PlayingCardsFunctions(deck)
deal <- cards_func$deal
shuffle <- cards_func$shuffle
