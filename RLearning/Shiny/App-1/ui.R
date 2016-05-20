library(shiny)

# Define UI for app draws a histogram
shinyUI(fluidPage(

	# App title
	titlePanel('Hello World'),
	
	# Sidebar with a slider input for the no. of bins
	sidebarLayout(
		sidebarPanel(
			sliderInput('bins', 
				   	 	'Number of bins:',
				   	 	min = 5,
				   	 	max = 50,
				   	 	value = 25)
		),
		
		# Show a plot of the generated distribution
		mainPanel(
			plotOutput('distPlot')
		)
	)

))