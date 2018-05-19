measure <- structure(
  list(
    V1=1:20, 
    V2=c(34L, 37L, 38L, 36L, 38L, 43L,40L, 38L, 40L, 41L, 36L, 36L, 34L, 33L, 36L, 
         37L, 34L, 36L, 38L, 35L), 
    V3=c(30L, 32L, 30L, 33L, 29L, 32L, 33L, 30L, 30L, 32L, 24L, 25L, 24L, 22L, 26L, 
         26L, 25L, 26L, 28L, 23L), 
    V4=c(32L, 37L, 36L, 39L, 33L, 38L, 42L, 40L, 37L, 39L, 35L, 37L, 37L, 34L, 38L, 
         37L, 38L, 37L, 40L, 35L)), 
  .Names=c("V1", "V2", "V3", "V4"), 
  class="data.frame", 
  row.names=c(NA, -20L))
  
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")

write.csv(measure, 'data/measure.csv')
