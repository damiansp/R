library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
	output$text1 = renderText({ 
		paste('You have selected: ', input$var, ' over the range of ', 
			  input$range[1], '-', input$range[2], '%', sep = '')
	})
})