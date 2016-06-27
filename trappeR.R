
# Camera TrappeR - Camera Trap Image Annotation Program for R

# Version 1.1

# Maxwell J. Farrell
# maxwellfarrell@gmail.com

# Works for Medium and High resolution photos from Bushnell Aggressor 2015 Model

rm(list=ls())

# Sourcing function library
source("trappeR_lib.R")

annotate <- function(){

############
# SET UP
############

# Choose folder with photos to work on
# Interactive folder choice varies with OS
if (Sys.info()[1]=="Windows"){
	path <- choose.dir() 
} else {
	require(tcltk2)
	path <- tk_choose.dir()
}

setwd(path)


# List files in photo directory
photo.list <- list.files(path, pattern="JPG$")


# if a previous csv is in the folder, start annotating from where session left off
if (any(grepl("csv", list.files(path)))){
	csv <- list.files(path, pattern=".csv$")
	annotated_photos <- read.csv(csv, as.is=TRUE)

	last_photo <- tail(sort(annotated_photos$filename), 1)
	index <- which(photo.list%in%last_photo)+1
} else {index <- 1}


# Pulling file path data:
folders <- strsplit(getwd(), "/")
nfolders <- length(folders[[1]])
site <- as.character(folders[[1]][nfolders-3])
date_range <- as.character(folders[[1]][nfolders-2])
camera <- as.character(folders[[1]][nfolders-1])
folder_num <- as.character(folders[[1]][nfolders])


#############################
# Annotate photos one by one
#############################


for (k in index:length(photo.list)){

	photo <- photo.list[k]

	print(paste("Photo",k, sep=" "))

	# Plot photo
	jpg <- readJPEG(photo, native=F)
	plot_jpeg(jpg)


	# get time & date from EXIF data
	exif <- exifExtract(photo)


	# Creating a data frame for meta data
	meta <- data.frame(	site=(site), date_range=(date_range), 
						camera=(camera), folder=(folder_num), 
						filename=(photo), date=(exif[1]), 
						time=(exif[2]), stringsAsFactors=FALSE)


	# Data for Motion/Timelapse and Temperature Displayed in Photo 

	# Can automate detection if image is [2448 * 3264] OR [3312 * 4416]
	if (dim(jpg)[1]==2448 | dim(jpg)[1]==3312){
		motion <- MT_Recognition(jpg)
		temp_F <- Temp_Recognition(jpg)
	} else {
		motion <- intMotion()	
		temp_F <- intTemp()
	}
	


	# How Many Species are there? (essentially how many rows will be in the dataframe)
	n_species <- intSpecies()
	n_speciesREAL <- n_species

 	if (n_species==0) n_species <- 1

	# Creating a data frame for user input data
	input <- data.frame(motion=integer(n_species), temp_F=integer(n_species),
						species=character(n_species), n_present=integer(n_species), 
						n_waterhole=integer(n_species), n_contact=integer(n_species), 
						notes=character(n_species), stringsAsFactors=FALSE)

	# Inputing Data for Motion/Timelapse and Temperature in data frame 
	input$motion <- motion
	input$temp_F <- temp_F


	# Populating the Data Frame with Species Data

	if (n_speciesREAL==0){

		input$species <- NA 
		input$n_present <- NA
		input$n_waterhole <- NA
		input$n_contact <- NA
		
	} else {

	for (x in 1:n_species){
		input$species[x] <- nameSpecies(x) 
		input$n_present[x] <- intPresent(x, input)
		input$n_waterhole[x] <- intWaterhole(x, input)
		input$n_contact[x] <- intContact(x, input)
		
		}
	}
	
	# Additional Notes
	input$notes <- charNotes()

	# Saving new data 
	
	new_annotation <- saveInput(meta, input, n_species)

	if (exists("annotated_photos")) {

		annotated_photos <- rbind(annotated_photos, new_annotation)

		} else { annotated_photos <- new_annotation}

		write.csv(annotated_photos, file=paste0(paste(site,date_range,camera,folder_num, sep="_"), ".csv"), row.names=FALSE)
	

	# dev.off()


	}

}

annotate()

