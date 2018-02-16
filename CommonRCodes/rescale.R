rescale = function(x, inMin, inMax, outMin, outMax) {
	outMin * (1 - (x - inMin) / (inMax - inMin)) + 
		outMax * ((x - inMin) / (inMax - inMin))
}


rescale(335, 335, 995, 4, 10)
rescale(995, 335, 995, 4, 10)
rescale(550, 335, 995, 4, 10)