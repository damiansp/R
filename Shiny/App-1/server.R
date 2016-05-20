library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
	# Epression that generates the histogram. The expression is warpped in a 
	# call to renderPlot to indicate that:
	# 	1) it is 'reactive' and therefore should re-execute automatically when
	#		inputs change;
	#	2) its output type is a plot
	
	output$distPlot = renderPlot({
		x = faithful[, 2]
		bins = seq(min(x), max(x), length.out = input$bins + 1)
		
		# draw the histogram with specified no. of bins
		hist(x, breaks = bins, col = 'skyblue', border = 'white')
	})
})