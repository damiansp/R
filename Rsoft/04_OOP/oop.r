#---------#---------#---------#---------#---------#---------#---------#---------
ls(list=ls())
setwd('~/Learning/R/Rsoft/04_OOP')

# S3
special_num_1 <- structure(1, class = "special_number")
class(special_num_1)
special_num_2 <- 2
class(special_num_2)
class(special_num_2) <- "special_number"
class(special_num_2)


# constructor
shape_s3 <- function(side_lengths) {
  structure(list(side_lengths = side_lengths), class = "shape_S3")
}

square_4 <- shape_s3(c(4, 4, 4, 4))
class(square_4)
triangle_3 <- shape_s3(c(3, 3, 3))
class(triangle_3)

# Constructor
is_square.shape_S3 <- function(x) {
  length(x$side_lengths) == 4 &&
    x$side_lengths[1] == x$side_lengths[2] &&
    x$side_lengths[2] == x$side_lengths[3] &&
    x$side_lengths[3] == x$side_lengths[4]
}

is_square(square_4)
is_square(triangle_3)

is_square.default <- function(x) {
  NA
}

is_square('square')

print(square_4)

print.shape_S3 <- function(x) {
  if (length(x$side_lengths) == 3) {
    paste("A triangle with side lengths of", x$side_lengths[1], 
          x$side_lengths[2], "and", x$side_lengths[3])
  } else if (length(x$side_lengths) == 4) {
    if (is_square(x)) {
      paste("A square with four sides of length", x$side_lengths[1])
    } else {
      paste("A quadrilateral with side lengths of", x$side_lengths[1],
            x$side_lengths[2], x$side_lengths[3], "and", x$side_lengths[4])
    }
  } else {
    paste("A shape with", length(x$side_lengths), "sides.")
  }
}

print(square_4)
print(triangle_3)
print(shape_s3(c(10, 10, 20, 20, 30)))
print(shape_s3(c(2, 3, 4, 5)))

head(methods(print), 10)

class(square_4)
class(square_4) <- c("shape_S3", "square")
class(square_4)
inherits(square_4, "square")