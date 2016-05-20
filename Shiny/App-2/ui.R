library(shiny)

# Define UI for app draws a histogram
shinyUI(fluidPage(
	titlePanel('title panel'),
	
	sidebarLayout(
		position = 'left',	# or right
		sidebarPanel('sidebar panel'),
		mainPanel(
			h1('HTML can be passed in like this'),
			h2('Or to center', align = 'center'),
			p('All the following are understood', 
			  style = 'font-family: cursive; color: blue; font-size: 22px'),
			HTML('<ul>
					<li>a</li>
					<li>br</li>
					<li>div</li>
					<li>span</li>
					<li>pre</li>
					<li>code</li>
					<li>img</li>
					<li>strong</li>
					<li>em</li>'),
			# Note: images must be within a folder called 'www' in the same
			# directory
			img(src = 'Me.png', height = 100, width = 75)
		)
	)
))