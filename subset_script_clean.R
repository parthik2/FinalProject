library("dplyr")
library("stringr")

# Make sure the file names in the directory match these ones
# I'll upload the versions I'm working with just to be safe
dmg_data = read.csv('school_demographics.csv', stringsAsFactors=FALSE)
mat_data = read.csv('school_MATH_all.csv', stringsAsFactors=FALSE)
ela_data = read.csv('school_ELA_all.csv')

# Use the average test scores from all grades in the school
# Isolate only the variables we're interested in
mat_all_grades = subset(mat_data, grade=="All Grades")
mat_extract = mat_all_grades[,c("DBN", "year", "num_tested", "mean_scale_score")]
ela_all_grades = subset(ela_data, grade=="All Grades")
colnames(ela_all_grades)[1] = "DBN"
ela_extract = ela_all_grades[,c("DBN", "year", "num_tested", "mean_scale_score")]

# Clean up the global variable environment, just to keep it organized
rm("ela_data", "ela_all_grades", "mat_data", "mat_all_grades")

# Give unique column names to ELA and MATH test results
colnames(ela_extract)[3] = "ELA num_tested"
colnames(ela_extract)[4] = "ELA mean_score"
colnames(mat_extract)[3] = "MAT num_tested"
colnames(mat_extract)[4] = "MAT mean_score"
colnames(dmg_data)[1] = "DBN"

# Isolate rows (DBN's) which are shared between all 3 data sets
shared_dmg = subset(dmg_data, DBN %in% mat_extract$DBN)
shared_mat = subset(mat_extract, DBN %in% shared_dmg$DBN)
shared_dmg = subset(shared_dmg, DBN %in% shared_mat$DBN)
shared_ela = subset(ela_extract, DBN %in% shared_dmg$DBN)

# Clean up the variable environment
rm("mat_extract", "ela_extract", "dmg_data")

# Clean up column names in Demographics .. they're funky for some reason
pat = "X.."
rep = ""
colnames(shared_dmg) = str_replace(colnames(shared_dmg), pat, rep)

# Remove columns reporting raw numbers of demographics; use percentages instead (since we have total enrollment, too)
# These both have the same column names ... Male (numbers)   Male.1 (percent)
shared_dmg$Female = NULL
shared_dmg$Male = NULL
shared_dmg$Asian = NULL
shared_dmg$White = NULL
shared_dmg$Black = NULL
shared_dmg$Hispanic = NULL
shared_dmg$Asian = NULL
shared_dmg$Multiple.Race.Categories.Not.Represented = NULL
shared_dmg$Multiple.Race.Categories.Not.Represented.1 = NULL
shared_dmg$Students.with.Disabilities = NULL
shared_dmg$English.Language.Learners = NULL
shared_dmg$Poverty = NULL
shared_dmg$Students.with.Disabilities = NULL
colnames(shared_dmg)[11] = "Disabilities.1"
colnames(shared_dmg) = str_replace(colnames(shared_dmg), ".1$", "")
shared_dmg$School.Name = NULL
colnames(shared_ela)[2] = "year"
colnames(shared_mat)[2] = "year"

colnames(shared_dmg)

# Correct formatting of years in demographic data 
pat = "^[[:digit:]]{4}-"
rep = "20"
shared_dmg$Year = str_replace(shared_dmg$Year, pat, rep)
rm("pat", "rep")

# Combine DBN and Year variables into a single ID variable
shared_dmg$ID = paste(shared_dmg$DBN, shared_dmg$Year)
shared_dmg$Year = NULL
shared_dmg$DBN = NULL
shared_ela$ID = paste(shared_ela$DBN, shared_ela$year)
shared_ela$year = NULL
shared_ela$DBN = NULL
shared_mat$ID = paste(shared_mat$DBN, shared_mat$year)
shared_mat$year = NULL
shared_mat$DBN = NULL

# Finally join the datasets! 
shared = merge(shared_dmg, shared_mat, by="ID")
shared = merge(shared, shared_ela, by="ID")

# No spaces in column names
colnames(shared) = str_replace(colnames(shared), " ", "_")


# All dataframe entries are strings. We need their actual numbers for the linear regression

# Convert each column with numbers into actual numbers, not strings
shared$Total.Enrollment = as.numeric(shared$Total.Enrollment)
shared$MAT_num_tested = as.numeric(shared$MAT_num_tested)
shared$MAT_mean_score = as.numeric(shared$MAT_mean_score)
shared$ELA_num_tested = as.numeric(shared$ELA_num_tested)
shared$ELA_mean_score = as.numeric(shared$ELA_mean_score)

# Convert percentages to decimals
# I couldn't find a "cool" way to do this, but it works 
# Put these 3 commands into a one-liner, and run it for each column with percentages
#   shared$column = str_replace(shared$column, "%", "")
#   shared$column = as.numeric(shared$column)
#   shared$column = shared$column / 100
# shared$column = (as.numeric( str_replace(shared$column, "%", "") ) ) / 100
shared$Male = (as.numeric( str_replace(shared$Male, "%", "") ) ) / 100
shared$Female = (as.numeric( str_replace(shared$Female, "%", "") ) ) / 100
shared$Asian = (as.numeric( str_replace(shared$Asian, "%", "") ) ) / 100
shared$Black = (as.numeric( str_replace(shared$Black, "%", "") ) ) / 100
shared$Hispanic = (as.numeric( str_replace(shared$Hispanic, "%", "") ) ) / 100
shared$White = (as.numeric( str_replace(shared$White, "%", "") ) ) / 100
shared$Disabilities = (as.numeric( str_replace(shared$Disabilities, "%", "") ) ) / 100
shared$English.Language.Learners = (as.numeric( str_replace(shared$English.Language.Learners, "%", "") ) ) / 100
shared$Poverty = (as.numeric( str_replace(shared$Poverty, "%", "") ) ) / 100
shared$Economic.Needs.Index = (as.numeric( str_replace(shared$Economic.Needs.Index, "%", "") ) ) / 100


# Save the data, yo
file_out = file(description="SCHOOL_FULL_DATA.csv", "w")
write.csv(shared, file_out, row.names=FALSE)
close(file_out)


rm("shared_dmg", "shared_ela", "shared_mat", "shared", "file_out")
