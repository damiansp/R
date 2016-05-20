# server.R

library(quantmod)
source("helpers.R")

shinyServer(function(input, output) {

	dataInput = reactive({
		getSymbols(input$symb, 
				   src = 'yahoo', 
				   from = input$dates[1], 
				   to = input$dates[2], auto.assign = F)
	})
	
  	output$plot <- renderPlot({
	    chartSeries(dataInput(), theme = chartTheme("white"), type = "line", 
	    			log.scale = input$log, TA = NULL)
  })
  
})