library(shiny)

# Define UI for app draws a histogram
introText = 'Create demographic maps with information form the 2010 US Census.'

shinyUI(fluidPage(
	titlePanel('Census Visualization'),
	
	sidebarLayout(
		sidebarPanel(
			helpText(introText),
			
			selectInput(
				'var',
				label = 'Choose variable to display',
				choices = c('Percent White', 'Percent Black', 
							'Percent Hispanic', 'Percent Asian'),
				selected = 'Percent White'
			),
			
			sliderInput(
				'range',
				label = 'Range of interest',
				min = 0, max = 100, value = c(0, 100)
			)
		),
		
		mainPanel(
			textOutput('text1')
		)
	)
))