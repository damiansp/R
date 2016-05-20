`bivd` <-
function(cop, marginX = "unif", marginY = marginX, 
            param.marginX = c(0,1),
            param.marginY = param.marginX) {

     yyy <- class(cop)
       cl <- substring(yyy,1,nchar(yyy) - 7)
 
     val <- new( paste(cl,"bivd",sep = "."), copula = cop, Xmarg = marginX, Ymarg = marginY,
       param.Xmarg = param.marginX ,       
       param.Ymarg = param.marginY)
       
       val  
}

