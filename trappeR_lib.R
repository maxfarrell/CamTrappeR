# Camera TrappeR FUNCTIONS - Camera Trap Image Annotation Program for R

# Library of functions used in Camera TrappeR

# Version 1.1

# Maxwell Farrell 
# maxwellfarrell@gmail.com



# Opening a JPEG image
require(jpeg)
# jpg <- readJPEG(photo, native=F) # read the file (moved to trappeR function)
plot_jpeg = function(jpg, add=FALSE){
  res <- dim(jpg)[1:2] # get the resolution
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[2]),ylim=c(1,res[1]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,xleft=1,xright=res[2],ytop=res[1],ybottom=1)
}
# plot_jpeg (photo)
# require(zoom)
# zm()



# Extract Date and Time
exifExtract <- function(photo){
	exif_data <- system2("exiftool",photo, stdout=T)
	exif_data <- strsplit(exif_data, " :")
	exif_data <- data.frame(matrix(unlist(exif_data), nrow=length(exif_data), byrow=T))
	exif_data <- as.character(exif_data$X2[exif_data$X1=="Date/Time Original             "])
	exif_data <- (matrix(unlist(strsplit(exif_data, " ")), nrow=length(exif_data), byrow=T))
	date <- (exif_data[2])
	time <- (exif_data[3])
	return(c(date,time))
}


# Automated Motion Detection

MT_Recognition<-function(photo){
  	mx<-c(227,305) ; my<-c(14,87) #bounding coordinate of the M/T portions for both medium and high res photos
  	ymax <- dim(photo)[1] 
  	cand.profile <- photo[(ymax-my[2]):(ymax-my[1]),mx[1]:mx[2],] #candidate image (array of 3 rgb values)
  	euc <- (cand.profile[,,1]^2+cand.profile[,,2]^2+cand.profile[,,3]^2)^0.5 #euclidean distance
  	euc2 <- euc<0.85 #those more black than white
  	temp <- ifelse(euc2,1,0) #turns matrix of logical values into matrix of 1 (true) or 0 (false). NIFTY!
  if (sum(temp==1)==1400){ #you could set it strictly so that it's all 39 pixels, but maybe a pixel may get lost here and there so i set it as a threshold instead. change as needed!
      return(1)
  } else if (sum(temp)==1060){
      return(0)
  } else { 
      intMotion()
  }
}



# Automated Temperature Detection (Fahrenheit °F)

# Functions used to detect numerals
vsim <- function(cand){ #how much vertical symmetry (fraction of matching pixels)
  tot <- sum(cand) #total pixels
  vflip <- apply(cand,2,rev) #flip vertically!
  length(which(cand+vflip==2))/tot #how much overlap (as fraction)
}
hsim <- function(cand){ #how much horizontal symmetry (fraction of matching pixels)
  tot <- sum(cand) #total pixels
  hflip <- t(apply(cand,1,rev)) #reversing on rows TRANSPOSES also, so you need to untranspose. stupid functionality
  length(which(cand+hflip==2))/tot
}
vsumsim <- function(cand){ #how much vertical symmetry (in terms of pixel count)
  c1 <- apply(cand,1,sum)
  c2 <- rev(c1)
  sum(abs(c2-c1))
}
hsumsim <- function(cand){ #how much horizontal symmetry (in terms of pixel count)
  c1 <- apply(cand,2,sum)
  c2 <- rev(c1)
  sum(abs(c2-c1))
}
vtot <- function(cand){ #how much vertical imbalance (top half pixel count / bottom half pixel count)
  topsum <- sum(cand[1:(dim(cand)[1]/2),])
  lowsum <- sum(cand[((dim(cand)[1]/2)+1):nrow(cand),])
  topsum/lowsum
}
htot <- function(cand){ #how much horizontal imbalance (left half pixel count / right half pixel count)
  leftsum <- sum(cand[,1:(dim(cand)[2]/2)])
  rightsum <- sum(cand[,((dim(cand)[2]/2)+1):ncol(cand)])
  leftsum/rightsum
}

# Diagnostic Function

diagnostic <- function(cand){
  if (ncol(cand)>=38) { ## rule for f
    return("f")
  } 
  	if (sum(cand[,ncol(cand)])==nrow(cand)) { ## rule for 1
    	return(1)
  } 
  	if (vsim(cand)>0.9 & hsim(cand)>0.9) { ## rule for 0
    	return(0)
  } 
  	if (sum(cand[1,])==ncol(cand)) { ## rule for 7
    	return(7)
  } 
  	if (htot(cand)<0.7 & vtot(cand)<0.9) { ## rule for 4
    	return(4)
  } 
  	if (vtot(cand)>1.2) { ## rule for 5
    	return(5)
  } 
  	if (sum(cand[nrow(cand),])==ncol(cand)) { ##rule for 2
    	return(2)
  } 
  	if (htot(cand)<0.7 & vtot(cand)>0.9) { ## rule for 3
    	return(3)
  } 
  	if ((abs(vsumsim(cand)-hsumsim(cand))>68) & hsim(cand)>0.7) { ## rule for 8
    	return(8)
  } 
  	if (sum(cand[,ncol(cand)])<sum(cand[,1])) { ## rule for 6
    	return(6)
  } else {
    	return(9) ## 9 is remaining option
  }
}	


Temp_Recognition <- function(photo){
	
	ymax <- dim(photo)[1]

 	# Bounding box for temp (image dimensions 2448 * 3264)
	if (ymax==3312){ 
 	mx <- c(1600,2300); my <- c(15,97)}

 	# Bounding box for temp (image dimensions 3312 * 4416)
 	if (ymax==2448){
 	mx <- c(1054,2031); my <- c(7,91)} 

 	prof <- photo[(ymax-my[2]):(ymax-my[1]),mx[1]:mx[2],] #load temperature portion of image
 
 	# convert to [black/white] ; [1/0]
 	euc <- (prof[,,1]^2+prof[,,2]^2+prof[,,3]^2)^0.5 #euclidean distance
 	euc2 <- euc<0.85 #more black than white (approx half the root of 3... euc dist of [0,0,0] to [1,1,1])
 	euc3 <- ifelse(euc2,1,0) #turns matrix of logical values into matrix of 1 (true) or 0 (false). NIFTY!
	  
 	# identify where characters end/begin by looking for white columns
 	boundaries <- apply(euc3,2,sum) #number of pixels in each column
 	rle.mat <- matrix(c(rle(boundaries)$values,rle(boundaries)$lengths,cumsum(rle(boundaries)$lengths)),nrow=3,byrow=TRUE)
	# rle produces df (class "rle") with values and lenghts column
  	# corresponding to the number of sequential columns with this value 
  	# (ex. the number of columns in a row that are all zeros/white)
  	# rle.mat is formatted with rows equal to values, ncols , cumulative sum (to keep track of where you are)

 	temp <- NULL

for (i in 1:sum(rle.mat[1,]==0)){
	
	inds <- which(rle.mat[1,]==0)[i:(i+1)] # zeros in rle.mat are bounding points each character
	startpt <- rle.mat[3,inds[1]]+1  ; endpt <- rle.mat[3,inds[2]-1]
	cand <- euc3[,startpt:endpt] # candidate character
	boundaries <- apply(cand,1,sum) # removes whitespace at top and bottom
	cand.mat <- matrix(c(rle(boundaries)$values,rle(boundaries)$lengths,cumsum(rle(boundaries)$lengths)),nrow=3,byrow=TRUE)
	startpt <- cand.mat[3,1]+1 ; endpt <- cand.mat[3,(ncol(cand.mat))-1]
	cand <- cand[startpt:endpt,] #clean image without extraneous whitespace

	if (diagnostic(cand)=="f"){ 
		return(temp)
		} else {
	
     digit <- diagnostic(cand)
     temp <- as.numeric(paste0(temp,digit))
	
		}
	}
}



#######################################################################

# Functions to read user input
# Based on code from http://www.rexamples.com/4/Reading%20user%20input

# BASELINE INFORMATION

# Motion detection or Time lapse?
intMotion <- function(){
	n <- readline(prompt="Motion Triggered? (Enter 1 for M, 0 for T):")
	if(!grepl("[01]",n) | nchar(n)>1)
	{return(intMotion())
} 
	return(as.integer(n))
}

# Temperature (F)
intTemp <- function(){
	n <- readline(prompt="Temperature (F): ")
	if(!grepl("[0-9]",n) | nchar(n)>3)
	{return(intTemp())
} 
	return(as.integer(n))
}


# Notes on Photo
charNotes <- function(){ 
  n <- readline(prompt="Notes on photo: ")
	return(as.character(n))
}



# SPECIES INFORMATION

# Number of Species
intSpecies <- function(){ 
  n <- readline(prompt="Number of species in photo: ")
  if(!grepl("^[0-9]+$",n))
	{return(intSpecies())
} 
	return(as.integer(n))
}



# Asking for input for each species
nameSpecies <- function(i){ 
  n <- readline(prompt = paste("Name of Species ", i ,":", sep=""))
  if(!grepl("^[A-z]+$",n))
	{return(nameSpecies(i))
} 
	return(as.character(n))
}
# Problem with above: if non-character reponse is given, i resets to "1" in prompt, 
# but the value of i is correct in the "input" dataframe produced.


intPresent <- function(i, input){ 
  n <- readline(prompt=paste(input$species[i],": Number of individuals in photo: ", sep=""))
  if(!grepl("^[0-9]+$",n))
	{return(intPresent(i, input))
} 
	return(as.integer(n))
}


intWaterhole <- function(i, input){ 
  n <- readline(prompt=paste(input$species[i],": Number of individuals at the water hole: ", sep=""))
  if(!grepl("^[0-9]+$",n))
	{return(intWaterhole(i, input))
} 
	return(as.integer(n))
}


intContact <- function(i, input){ 
  n <- readline(prompt=paste(input$species[i],": Number of individuals directly in contact with water: ", sep=""))
  if(!grepl("^[0-9]+$",n))
	{return(intContact(i, input))
} 
	return(as.integer(n))
}


intJuveniles <- function(i, input){ 
  n <- readline(prompt=paste(input$species[i],": Number of juveniles in photo: ", sep=""))
  if(!grepl("^[0-9]+$",n))
	{return(intJuveniles(i, input))
} 
	return(as.integer(n))
}





###############################################################


# Amalgamating metadata and user input data

# Function to merge annotation data with existing data frame and save as .csv
saveInput <- function(meta, input, n_species){

# Repeat metadata N times as a function of number of species:
	meta_rep <- do.call("rbind", replicate(n_species, meta, simplify = FALSE))
	annotated <- cbind(meta_rep, input)
	return(annotated)
	# For now use return - then later use write.csv after performing checks	
}