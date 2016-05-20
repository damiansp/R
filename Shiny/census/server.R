library(shiny)
library(maps)
library(mapproj)

# Load helper R code
source('helpers.R')

# Load data
counties = readRDS('data/counties.rds')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
	
	output$map = renderPlot({
		data = switch(input$var,
			'% White' = counties$white,
			'% Black' = counties$black,
			'% Hispanic' = counties$hispanic,
			'% Asian' = counties$asian
		)
				
		percent_map(var = data, color = 'black', legend.title = input$var,
					min = input$range[1], max = input$range[2])
	})

})