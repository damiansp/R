#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())

library(magrittr)
library(pryr)

mem_used()
x <- rnorm(1000)
sapply(ls(), function(x) object_size(get(x))) %>% sort
mem_change(rm(x))
ls()


# Quick memory calculations
object_size(integer(0)) # 40B
object_size(integer(1000)) # 4.04 kB; 4 bytes/int
object_size(numeric(1000)) # 8.04 kB; 8 bytes/numeric

str(.Machine)



# Internal memory management
gc() # garbage collection (never needed, but output is handy:)
# used: memory currently used by R
# gc trigger: memory that can be used befor garbage collection is triggered
# max used: max memory used since last call to gc(reset=T)
