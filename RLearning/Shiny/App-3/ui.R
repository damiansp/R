library(shiny)

# Define UI for app draws a histogram
shinyUI(fluidPage(
	titlePanel('Basic widgets'),
	
	# Create a row
	fluidRow(
		column(
			# Column width (of 12 possible)
			3,
			h3('Buttons'),
			br(),
			br(),
			submitButton('Submit')
		),
		
		column(
			3,
			h3('Single Checkbox'),
			checkboxInput('checkbox', label = 'Choice A', value = T)
		),
		
		column(
			3,
			checkboxGroupInput(
				'checkGroup',	# var name to access
				label = h3('Checkbox Group'),
				choices = list('Choice 1' = 1, 'Choice 2' = 2, 
							   'Choice 3' = 3),
				selected = 1
			)
		),
		
		column(
			3,
			dateInput(
				'date',
				label = h3('Date Input'),
				value = '2014-11-03'
			)
		)
	),
	
	fluidRow(
		column(
			3,
			radioButtons('radio', label = h3('Radio Buttons'),
						 choices = list('Choice 1' = 1, 'Choice 2' = 2,
						 				'Choice 3' = 3),
						 selected = 1)
		),
		
		column(
			3,
			selectInput('select', label = h3('Select Box'),
						choices = list('Choice 1' = 1, 'Choice 2' = 2,
									   'Choice 3' = 3),
						selected = 1)
		),
		
		column(
			3,
			sliderInput('slider1', label = h3('Sliders'), min = 0, max = 100,
						value = 50),
			sliderInput('slider2', label = '', min = 0, max = 100, 
					    value = c(25, 75))
		),
		
		column(
			3,
			textInput('text', label = h3('Text Input'), 
					  value = 'Enter text...')
		)
	)
))