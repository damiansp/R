A = c(0,0)
B = c(0,1)
C = c(0,2)
D = c(1,0)
E = c(1,1)
G = c(1,2)
H = c(2,0)
I = c(2,1)
J = c(2,2)


#Cellular automaton
# if specified, init must be vector length "size" whose values must all be from {0, 1, 2}
cell.aut = function( size = 300, A = c(0,0), B = c(0,1), C = c(0,2), 
					 D = c(1,0), E = c(1,1), G = c(1,2), H = c(2,0), 
					 I = c(2,1), J = c(2,2), init = NULL) { 
	N = matrix(nrow = size, ncol = size)

	if (is.null(init)) {
		N[1,] = sample(0:2, size, TRUE)	# create first row (3color)
	} else {
		N[1,] = init
	}

	for (i in 2:size) {
		# even numbered rows start flush w/ left
		if (i%%2 == 0){ 
			# count every other sq. starting w/ 1
			for (j in seq(1, size - 1, 2)) {	
				# list all combinations and rules
				# first == 0
				if (N[i - 1, j] == 0) {	
					if (N[i - 1, j + 1] == 0) {
						# 00 (for each row like this marked #00, #01, etc, 
						# try any combination of 0, 1, and 2)
						N[i,j] = A[1]; N[i, j+1] = A[2] 
					}
					
					if (N[i - 1, j + 1] == 1) {
						N[i, j] = B[1]; N[i, j + 1] = B[2] # 01
					}
						
					if (N[i - 1, j + 1] == 2){
						N[i, j] = C[1]; N[i, j + 1] = C[2] # 02
					}	
				}
					
				# first == 1
				if (N[i - 1, j] == 1){	
					if (N[i - 1, j + 1] == 0) {
						N[i, j] = D[1]; N[i, j + 1] = D[2] # 10
					}
					
					if (N[i - 1, j + 1] == 1) {
						N[i, j] = E[1]; N[i, j + 1] = E[2] # 11
					}
					
					if(N[i - 1, j + 1] == 2) {
						N[i, j] = F[1]; N[i, j + 1] = F[2] # 12
					}
				}			
				
				# first == 2
				if (N[i - 1, j] == 2) {	
					if (N[i - 1, j + 1] == 0) {
						N[i, j] = G[1]; N[i, j + 1] = G[2] # 20
					}
					
					if (N[i - 1, j + 1] == 1) {
						N[i, j] = H[1]; N[i, j + 1] = H[2] # 21
					}
						
					if (N[i-1, j+1] == 2){
						N[i, j] = I[1]; N[i, j + 1] = I[2] # 22
					}
				}
			} # end for	
		} # end if(even row)
		
		
		if(i%%2 == 1){
			#randomly assign first and last pixels
			N[i,1] = sample(0:2, 1); N[i,size] = sample(0:2, 1)
			for(j in seq(2,size-2,2)){ #count every other sq. starting w/ 2
				#repeat rules
				if(N[i-1, j] == 0){	#first == 0
					if(N[i-1, j+1] == 0){
						N[i,j] = A[1]; N[i, j+1] = A[2] #00
						}
					if(N[i-1, j+1] == 1){
						N[i,j] = B[1]; N[i, j+1] = B[1] #01
						}	
					if(N[i-1, j+1] == 2){
						N[i,j] = C[1]; N[i, j+1] = C[2] #02
						}	
					}	
				if(N[i-1, j] == 1){	#first == 1
					if(N[i-1, j+1] == 0) {
						N[i,j] = D[1]; N[i, j+1] = D[2] #10
						}
					if(N[i-1, j+1] == 1){
						N[i,j] = E[1]; N[i, j+1] = E[2] #11
						}
					if(N[i-1, j+1] == 2){
						N[i,j] = F[1]; N[i, j+1] = F[2] #12
						}
					}			
				if(N[i-1, j] == 2){	#first == 2
					if(N[i-1, j+1] == 0){
						N[i,j] = G[1]; N[i, j+1] = G[2] #20
						}
					if(N[i-1, j+1] == 1){
						N[i,j] = H[1]; N[i, j+1] = H[2] #21
						}	
					if(N[i-1, j+1] == 2){
						N[i,j] = I[1]; N[i, j+1] = I[2] #22
						}
					}
				} #end for
			} #end if(odd rows 3+)
		}
	image(1:size, 1:size, (N))	
}