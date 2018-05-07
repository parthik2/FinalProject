library("dplyr")
library("stringr")

dat = read.csv('SCHOOL_FULL_DATA.csv', stringsAsFactors=FALSE)

# Get schools with low poverty levels and find their first quartile of math test scores
not_pov = subset(dat, Poverty < .25)
quant = quantile(not_pov$MAT_mean_score)
TARGET_SCORE = quant[[3]]

# Find the schools with high poverty who test above this quartile
hsp = subset(dat, Poverty > .5)
hsp = subset(hsp, MAT_mean_score > TARGET_SCORE)

# Schools have 5 rows, one for each year ... get only the ID number and remove duplicates
hsp_DBN = str_extract(hsp$ID, "^[:alnum:]+")
hsp_DBN_save = hsp_DBN
hsp_DBN = unique(hsp_DBN)

# Save DBN's in separate text file
write(hsp_DBN, file="top_school_poverty.txt")