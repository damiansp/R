#=======================================================#
#														#
#	XML and Web Technologies for Data Sciences with R	#
#		Nolan & Lang (2014)								#
#														#
#							#===========================#
#							#
#	3. Parsing XML Content	#
#							#
#===========================#
rm(list=ls())
library(XML)
load('~/Desktop/R/WebTech/webTech.RData')


# 3.1 Introduction to Reading XML in R
doc = xmlParse('~/Desktop/R/WebTech/kivaLender.xml')

# Retrieve contents of the <occupation> nodes
# First access <lenders> from root node
lendersNode = xmlRoot(doc)[['lenders']]

# Iterate over all children (<lender> nodes), and extract the value from the occupation node
occ = sapply( xmlChildren(lendersNode), function(node) { 
				xmlValue(node[['occupation']]) 
			})



# 3.2 Document Object Model (DOM)
doc = xmlParse('~/Desktop/R/WebTech/merged_catalog.xml.gz')
docName(doc)
root = xmlRoot(doc)	# the whole root node, including all descendants

# Methods on all element nodes
xmlName(root)
xmlAttrs(root)
xmlChildren(root)
xmlValue(root)
xmlSize(root) # number of children; same as
# length(xmlChildren(root))



# 3.3 Accessing Nodes in the DOM
event1 = root[['event']]	# extracts the first event
event1[[10]]
root[1:3]
evs = root['event']	# extracts all events
length(evs)	# 1047

xmlSize(evs[[1]])
evs[[1]]

names(event1)	# names of all child nodes
xmlName(event1)


doc = xmlParse('~/Desktop/R/WebTech/kivaLender.xml')
lendersNode = xmlRoot(doc)[['lenders']]
xmlSize(lendersNode)
lendersNode[1:2]

lendersNode[[1]][c('name', 'occupation', 'image')]

w = sapply(xmlChildren(lendersNode[[1]]), xmlSize) > 1
length(w)
w[2:5]
lendersNode[[1]][w]

template.id = sapply(xmlChildren(lendersNode), function(x) {
	xmlValue(x[['image']][['template_id']])
})

# Same with simpler syntax:
xmlSApply(lendersNode[[1]], xmlSize) # Crashes R









save.image('~/Desktop/R/WebTech/webTech.RData')
