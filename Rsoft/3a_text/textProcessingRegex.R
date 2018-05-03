#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
library(stringr)


# Text manipulation
paste('Square', 'Circle', 'Triangle', sep=':')
shapes <- c('Square', 'Circle', 'Triangle')
paste('My favorite shape is a', shapes)
two.cities <- c('best', 'worst')
paste('It was the', two.cities, 'of times.')
paste(shapes)
paste(shapes, collapse=' ')
nchar('Satterthwaite')
cases <- c('UPPER', 'lower', 'Title')
toupper(cases)
tolower(cases)


# Regular expressions
reg.ex <- 'a'
search.string <- 'Maryland'
grepl(reg.ex, search.string) # TRUE
grepl('u', search.string)    # FALSE
grepl('land', search.string) # T

head(state.name)
grepl('.', search.string)    # T (. = wildcard)
grepl('.', '')               # F
grepl('a.b', c('aaa', 'aab', 'abb', 'acadb')) # F T T T
grepl('a+', search.string)   # T (Maryland has >= 1 a)
grepl('x*', search.string)   # T (has >= 0 x)
grepl('s{2}', 'Mississippi') # T
grepl('s{2,3}', 'Mississippi') # T
grepl('i{2,3}', 'Mississippi') # F
grepl('(iss){2}', 'Mississippi') # T
grepl('(i.{2}){3}', 'Mississippi') # T (i followed by a double, 3x)

grepl('\\w', 'abcdefg') # T \w = alphnumeric
grepl('\\w', '0123')    # T
grepl('\\w', 1234)      # T
grepl('\\d', 1234)      # T \d = digit
grepl('\\d', '123')     # T
grepl('\\d', 'a1b2c3')  # T
grepl('\\d', 'this')    # F
grepl('\\D', 'this')    # T \D = non-digit
grepl('\\W', '\t  \n')  # T \W = non-alphanumeric

grepl('[aeiou]', 'rhythms') # F  (contains any?)
grepl('[^aeiou]', 'rhythms') # T (doesn't contain any?)
grepl('[a-m]', 'no')         # F

grepl('\\+', 'this + that')  # T
grepl('\\.', 'mail.com')     # T

grepl('^a', c('bab', 'aba')) # F T (starts with a)
grepl('b$', c('bab', 'aba')) # T F (ends with b)
grepl('^[ab]+$', c('bab', 'aab', 'abc')) # T T F (starts and ends with 1+ a or b)

grepl('a|b', c('abc', 'bcd', 'cde')) # T T F (contains a or b)
grepl('North|South', c('North Dakota', 'South Dakota', 'Nebraska')) # T T F

starts.and.ends.w.vowel <- '^[AEIOU].+[aeiou]$'
state.name[grepl(starts.and.ends.w.vowel, state.name)]


# Regex in R
grepl('[Ii]', c('Hawaii', 'Illinois', 'Kentucky')) # T T F
grep('[Ii]', c('Hawaii', 'Illinois', 'Kentucy'))   # 1 2
sub('[Ii]', '!', c('Hawaii', 'Illinois', 'Kentucky')) # Replaces 1st instance
gsub('[Ii]', '!', c('Hawaii', 'Illinois', 'Kentucky')) # Replaces all instances

double.s.states <- state.name[grep('ss', state.name)]
double.s.states
strsplit(double.s.states, 'ss')


# stringr package
state.data <- paste(state.name, state.area, state.abb)
state.data[1:4]
str_extract(state.data, '[0-9]+') # numeric portion (state.areas)
str_order(state.name) # already ordered
state.abb[str_order(state.abb)]

str_pad('Thai', width=8, side='left', pad='-') # ----Thai
str_pad('Thai', width=8, side='both', pad='-') # --Thai--
cases <- c('UPPER CASE', 'lower case', 'Title Case')
str_to_title(cases)
str_trim('     too   much white \tspace\n.      ') # does not remove \t, \n

twenty.states <- paste(state.name[1:20], collapse=' ')
cat(str_wrap(twenty.states, width=80))
cat(str_wrap(twenty.states, width=30, indent=4)) # indents first line only

tale <- 'It was the best of times; it was the worst of times.'
word(tale, 2)
word(tale, end=3)
word(tale, start=6)
word(tale, start=7, end=9)
