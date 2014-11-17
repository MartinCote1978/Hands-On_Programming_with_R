abs_loop <- function(vec) {
  for (i in 1:length(vec)) {
    if (vec[i] < 0) {
      vec[i] <- -vec[i]
    }
  }
  vec
}

abs_set <- function(vec) {
  negs <- vec < 0
  vec[negs] <- vec[negs] * -1
  vec
}

long <- rep(c(-1,1), 5000000)

# test execution time
system.time(abs_loop(long))
system.time(abs_set(long))

system.time(abs(long))
