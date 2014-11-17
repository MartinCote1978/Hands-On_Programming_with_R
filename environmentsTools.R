## DevTools - R environments tools
install.packages("devtools")

# NOT WORKING; devtools requires rtools but not avail to 3.1.2 (or 3.1.1)?!
install.packages("Rtools") # Not avail. for 3.1.2?!?
find_rtools() # 

library(devtools)
parenvs(all = TRUE)

## Show environment
show_env <- function(x = foo) {
  # argument of function becomes object within the Runtime environment
  # Objects created only within the Runtime environment
  a <- 1
  b <- 2
  c <- 3
  list(ran.in = environment(),
       parent = parent.env(environment()),
       objects = ls.str(environment())
       )
}
