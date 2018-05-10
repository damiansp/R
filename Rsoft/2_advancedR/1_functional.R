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