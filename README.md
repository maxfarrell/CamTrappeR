#CamTrappeR

Interactive camera trap annotation in R. Plots .jpg photos and asks for user input regarding numbers of species, species identities, and abundances per species based on proximity to watering holes. Currently the script is compatible with photos taken with the Bushnell Aggressor 2015 series cameras.

Timestamp metadata is gathered using exiftool (http://www.sno.phy.queensu.ca/~phil/exiftool/) while motion vs time-lapse triggering and temperature (F) data are scraped using pixel information from the jpg (thanks Herc!). exiftool must be installed and executable with system().

Metadata scraping for motion vs time-lapse triggering and for temperature only work with medium (8M) and high (12M) resolution photos, but can be easiliy adapted for low resolution (3M) photos.

The script also extracts metadata based on the file path, which is assumed to be Site / Date Range / Camera Number / Photo Batch / File, where photo batch is the automatic creation of folders for each group of 1000 photos (done by the camrera automatically when >1000 photos are stored).

  