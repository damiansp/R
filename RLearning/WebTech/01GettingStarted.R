#=======================================================#
#														#
#	XML and Web Technologies for Data Sciences with R	#
#		Nolan & Lang (2014)								#
#														#
#											#===========#
#											#
#	1. Getting Started with XML and JSON		#
#											#
#===========================================#
rm(list=ls())
library(rjson)
library(RJSONIO)
library(XML)

load('~/Desktop/R/WebTech/webTech.RData')

# 1.1 Introduction



# 1.2 Reading Data from HTML Tables
url <- 'http://en.wikipedia.org/wiki/Country_population'
tbls <- readHTMLTable(url) # returns ALL <table> elems on page
sapply(tbls, nrow)	# looks like the first is the one we want
pop <- readHTMLTable(url, which=1)
rm(tbls)
pop



# 1.3 Reading Data from XML-Formatted Documents
doc <- xmlParse('~/Desktop/R/WebTech/kiva_ds_xml/lenders/1.xml')
kiva.list <- xmlToList(doc, addAttributes=F)
str(kiva.list)
kiva.list$lenders[[1000]]$lender_id
lenders.node <- xmlRoot(doc)[['lenders']]
lenders <- xmlToDataFrame(xmlChildren(lenders.node))
names(lenders) 
# where nodes, like "image" have mult children, onlyt the first is used

	# 1.3.1 Extracting Data from XML Attributes
	bill <- xmlParse('~/Desktop/R/WebTech/bills/data.xml')
	root <- xmlRoot(bill)
	actions <- root[['actions']]
#	rBills <- getNodeSet(actions, './Desktop/R/WebTech/bills')
	xmlAttrs(root)
	
	kiva <- RJSONIO::fromJSON('~/Desktop/R/WebTech/kiva_ds_xml/lenders/1.json')
	names(kiva)
	kiva$lenders[[1]]






save.image('~/Desktop/R/WebTech/webTech.RData')
