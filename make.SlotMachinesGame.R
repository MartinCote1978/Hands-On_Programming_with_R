## Slot Machines

# Randomly provide 3 symbols as a vector
get_symbols <- function() {
  wheel <- c("DD","7","BBB","BB","B","C","0")
  sample(wheel, size=3, replace=TRUE,
         prob=c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}

# Score a vector of 3 symbols
score <- function(symbols = c("0","0","0")) {
  
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  slots <- symbols[symbols != "DD"]
  same <- length(unique(slots)) == 1
  bars <- slots %in% c("B","BB","BBB")
  
  if (diamonds == 3) {
    prize <- 100
  } else if (same) {
    # case 1: all the same
    
    # prize lookup table
    payouts <- c("DD" = 100,"7" = 80,"BBB" = 40,"BB"=25,"B"=10,"C"=10,"0"=0)
    prize <- unname(payouts[slots[1]])
    
  } else if (all(bars)) {
    # case 2: all bars
    
    prize <- 5
    
  } else if (cherries > 0) {
    prize <- c(0,2,5)[cherries+diamonds+1]
    
  } else {
    prize <- 0
  }
  
  # double the prize if necessary
  prize * 2 ^ diamonds
}

# Establish symbols and score them
play <- function() {
  symbols <- get_symbols()
  #print(symbols)
  structure(score(symbols), class="slots",symbols=symbols)
}

#slot_display <- function(prize) {
print.slots <- function(x, ...) {
  # extract symbols
  symbols <- attr(x, "symbols")
  
  # collapse symbols into single string
  symbols <- paste(symbols, collapse=" ")
  
  # combine symbol with prize as a regular expression
  # \n is regular expression for new line (i.e. return or enter)
  string <- paste(symbols, x, sep="\n$")
  
  # display regular expression in console without quotes
  cat(string)
}




## Calculate the expected outcome
wheel <- c("DD","7","BBB","BB","B","C","0")
plays <- expand.grid(wheel, wheel, wheel, stringsAsFactors=FALSE)

prob <- c("DD"=0.03, "7"=0.03, "BBB"=0.06, "BB"=0.1, "B"=0.25, "C"=0.01, "0"=0.52)
plays$prob1 <- prob[plays$Var1]
plays$prob2 <- prob[plays$Var2]
plays$prob3 <- prob[plays$Var3]
plays$prob <- plays$prob1 * plays$prob2 * plays$prob3

plays$prize <- NA
for (i in 1:nrow(plays)) {
  plays$prize[i] <- score(c(plays[i,1],plays[i,2],plays[i,3]))
}
sum(plays$prize * plays$prob)


plays_till_broke <- function(start_with) {
  cash <- start_with
  
  n <- 0
  while (cash > 0) {
    cash <- cash - 1 + play()
    n <- n + 1
  }
  n
}

plays_till_broke_set <- replicate(100, plays_till_broke(10))
qplot(plays_till_broke_set, binwidth=100)


# slow change symbols function
change_symbols <- function(vec) {
  for (i in 1:length(vec)){
    if (vec[i] == "DD") {
      vec[i] <- "joker"
    } else if (vec[i] == "C") {
      vec[i] <- "ace"
    } else if (vec[i] == "7") {
      vec[i] <- "king"
    } else if (vec[i] == "B") {
      vec[i] <- "queen"
    } else if (vec[i] == "BB") {
      vec[i] <- "jack"
    } else if (vec[i] == "BBB") {
      vec[i] <- "ten"
    } else {
      vec[i] <- "nine"
    }
  }
  vec
}

# fast change symbols method
change_symbols_fast <- function(vec) {
  new_symbols <- c("DD"="joker","C"="ace","7"="king","B"="queen","BB"="jack","BBB"="ten","0"="nine")
  
  unname(new_symbols[vec])
}

# test
vec <- c("DD","C","DD","7","B","BB","BBB","0")
change_symbols(vec)
change_symbols_fast(vec)

many <- rep(vec, 1000000)
system.time(change_symbols(many))
system.time(change_symbols_fast(many))


# test obtain the mean by averaging the winnings
winnings <- vector(length = 10000000)
for (i in 1:10000000) {
  winnings[i] <- play()
}
mean(winnings)



# vectorized code for establishing winnings
get_many_symbols <- function(n) {
  wheel <- c("DD","7","BBB","BB","B","C","0")
  
  vec <- sample(wheel, size=3*n, replace=TRUE,
         prob=c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
  matrix(vec, ncol=3)
}
get_many_symbols(5)

play_many <- function(n) {
  symb_mat <- get_many_symbols(n = n)
  data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2], w3 = symb_mat[,3], prize = score_many(symb_mat))
}
score_many <- function(symbols) {
  
  # Step 1: Assign base prize based on cherries and diamonds ---------
  ## Count the number of cherries and diamonds in each combination
  cherries <- rowSums(symbols == "C")
  diamonds <- rowSums(symbols == "DD")
  
  ## Wild diamonds count as cherries
  prize <- c(0, 2, 5)[cherries + diamonds + 1]
  
  ## ...but not if there are zero real cherries
  ### (cherries is coerced to FALSE where cherries == 0)
  prize[!cherries] <- 0
  
  # Step 2: Change prize for combinations that contain three of a kind
  same <- symbols[, 1] == symbols[, 2] &
    symbols[, 2] == symbols[, 3]
  payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40,
               "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
  prize[same] <- payoffs[symbols[same, 1]]
  
  # Step 3: Change prize for combinations that contain all bars ------
  bars <- symbols == "B" | symbols ==  "BB" | symbols == "BBB"
  all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
  prize[all_bars] <- 5
  
  # Step 4: Handle wilds ---------------------------------------------
  
  ## combos with two diamonds"
  two_wilds <- diamonds == 2
  
  ### Identify the nonwild symbol
  one <- two_wilds & symbols[, 1] != symbols[, 2] &
    symbols[, 2] == symbols[, 3]
  two <- two_wilds & symbols[, 1] != symbols[, 2] &
    symbols[, 1] == symbols[, 3]
  three <- two_wilds & symbols[, 1] == symbols[, 2] &
    symbols[, 2] != symbols[, 3]
  
  ### Treat as three of a kind
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  ## combos with one wild
  one_wild <- diamonds == 1
  
  ### Treat as all bars (if appropriate)
  wild_bars <- one_wild & (rowSums(bars) == 2)
  prize[wild_bars] <- 5
  
  ### Treat as three of a kind (if appropriate)
  one <- one_wild & symbols[, 1] == symbols[, 2]
  two <- one_wild & symbols[, 2] == symbols[, 3]
  three <- one_wild & symbols[, 3] == symbols[, 1]
  prize[one] <- payoffs[symbols[one, 1]]
  prize[two] <- payoffs[symbols[two, 2]]
  prize[three] <- payoffs[symbols[three, 3]]
  
  # Step 5: Double prize for every diamond in combo ------------------
  unname(prize * 2^diamonds)
}


plays <- play_many(10000000)
mean(plays$prize)

