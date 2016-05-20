#===============================#
#								#
#  knitr and R markdown (.rmd)	#
#								#
#===============================#

library(knitr)

?knit
?knit2html

knit('~/Desktop/R/knitr/RMarkdownDemo.rmd', '~/Desktop/R/knitr/Outfile')
knit2html('~/Desktop/R/knitr/RMarkdownDemo.rmd', '~/Desktop/R/knitr/outfile.html')