rm(list=ls())
library(purrr)

add.n.maker <- function(n) {
  function(x) {
    n + x
  }
}

add2 <- add.n.maker(2)
add3 <- add.n.maker(3)

add2(4)
add3(4)

# Map
map_chr(
  c(5, 4, 3, 2, 1), 
  function(x) {
    c('one', 'two', 'three', 'four', 'five')[x]
  })

map_lgl(
  c(1, 2, 3, 4, 5), 
  function(x) {
    x > 3
})

map_if(1:5,
       function(x) { x %% 2 == 0}, # Condition
       function(x) { x^2 }) %>%    # Mapping
  unlist()                         # Prettify

map_at(seq(100, 500, 100),         # input
       c(1, 3, 5),                 # indices
       function(x) { x - 10 }) %>% # Mapping
  unlist()

map2_chr(letters, 1:26, paste) # arg1, arg2, func

pmap_chr(list(list(1, 2, 3),               # list of args
              list('one', 'two', 'three'),
              list('ichi', 'ni', 'san')),
         paste)                            # func


# Reduce
reduce(
  c(1, 3, 5, 7), 
  function(x, y) {
  	message(sprintf('  x: %d\n  y: %d\n', x, y))
  	x + y
  })
  
reduce(
  letters[1:4],
  function(x, y) {
  	message(sprintf('  x: %s\n  y: %s\n', x, y))
  	paste(x, y, sep='')
  })
  
reduce_right(
  letters[1:4],
  function(x, y) {
  	message(sprintf('  x: %s\n  y: %s\n', x, y))
  	paste(x, y, sep='')
  })


# Search
has_element(letters, 'a')
has_element(letters, 'A')

detect(20:40, function(x) { x > 22 && x %% 2 == 0 })
detect_index(20:40, function(x) { x > 22 && x %% 2 == 0 })
(20:40)[5]


# Filter
keep(1:20, function(x) { x %% 2 == 0 })
discard(1:20, function(x) { x %% 3 == 0 })
every(1:20, function(x) { x %% 2 == 0 }) # F
some(1:20, function(x) { x %% 2 == 0 })  # T


# Compose
x <- rep(1:5, 1:5)
x # 1 2 2 3 3 3 ...
n.unique <- function(x) {
  length(unique(x))
}
n.unique(x) # 5

# Same as:
n_unique <- compose(length, unique)
n_unique(x) # 5


# Partial Application
mult3n <- function(x, y, z) {
  x * y * z
}

mult15 <- partial(mult3n, x=3, y=5)
mult15(4) # same as
mult15(z=4)


# Side effects
walk(c('Friends, Romans, countrymen,',
       'lend me your ears;',
       'I come to bury Caesar,',
       'not to praise him.'),
     message)
     

# Recursion
vector.sum.loop <- function(v) {
  result <- 0
  for (i in v) {
  	result <- result + i
  }
  result
}
vector.sum.loop(1:10)

vector.sum.recursive <- function(v) {
  if (length(v) == 1) {
  	v
  } else {
  	v[1] + vector.sum.recursive(v[-1])
  }
}
vector.sum.recursive(1:10)

fibonacci <- function(n) {
  stopifnot(n > 0)
  if (n == 1) {
  	0
  } else if (n == 2) {
  	1
  } else {
  	fibonacci(n - 1) + fibonacci(n - 2)
  }
}

fibonacci(0)
fibonacci(12)
map_dbl(1:12, fibonacci)

# Memoization
fib.t <- c(0, 1, rep(NA, 23))

fib.mem <- function(n) {
  stopifnot(n > 0)
  if (!is.na(fib.t[n])) {
  	fib.t[n]
  } else {
  	fib.t[n - 1] <<- fib.mem(n - 1) # <<- is assignment out-of-scope
  	fib.t[n - 2] <<- fib.mem(n - 2)
  	fib.t[n - 1] + fib.t[n - 2]
  }
}

t0 <- Sys.time()
fib.mem(1)
Sys.time() - t0

t0 <- Sys.time()
fib.mem(400)
Sys.time() - t0

t0 <- Sys.time()
fib.mem(400)
Sys.time() - t0

