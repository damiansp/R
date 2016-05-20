# Read file & define variables 
test = read.csv('~/Desktop/TEST1.csv')
numbers = as.character(test [,2])

# Transform data 
s = strsplit(numbers,', ')
newMatrix = data.frame(numbers=unlist(s), letters=rep(letters, sapply(s, FUN=length)))
# I copied the code above...it works beautifully but I don't understand the syntax. Please translate. 

# Remove spaces and quotation marks
newMatrix [,1] = gsub("^\\s+|\\s+$", "", newMatrix[,1])
newMatrix [,1] = gsub("[^[:alnum:][:blank:]+?&/\\-]", "", newMatrix[,1])

# Could you help me interpret the 2nd gsub syntax "+?&/\\-]", and why it is written this way? I also want to add other characters in the exclusion list.  !  # $ % ' ( ) * + , - . / : ; < = > @ [ \ ] ^ _ ` { | } ~ . how do I do this? Reference: http://stackoverflow.com/questions/21641522/how-to-remove-specific-special-characters-in-r . 

test 
newMatrix

