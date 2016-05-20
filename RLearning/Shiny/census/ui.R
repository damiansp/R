library(shiny)

# Define UI for app draws a histogram
shinyUI(fluidPage(
	titlePanel('censusVis'),
	
	sidebarLayout(
		sidebarPanel(
			helpText('Create demographic maps with 2010 Census data'),
			
			selectInput(
				'var', label = 'Choose a variable to display',
				choices = c('% White', '% Black', '% Hispanic', '% Asian'),
				selected = '% White'
			),
			
			sliderInput(
				'range', label = 'Range of interest',
				min = 0, max = 100, value = c(0, 100)
			)
		),
		
		mainPanel(plotOutput('map'))
	)
))