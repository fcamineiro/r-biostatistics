##############################################################
# Housekeeping                          Use for All Analyses #
##############################################################
date()             # Current system time and date.
Sys.time()         # Current system time and date (redundant).
R.version.string   # R version and version release date.
options(digits=6)  # Confirm default digits.
options(scipen=999)# Suppress scientific notation.
options(width=60)  # Confirm output width.
ls()               # List all objects in the working directory.
rm(list = ls())    # CAUTION: Remove all files in the working 
                   # directory.  If this action is not desired, 
                   # use rm() one-by-one to remove the objects 
                   # that are not needed.
ls.str()           # List all objects with finite detail.
getwd()            # Identify the current working directory.
setwd("F:/R_BiostatisticsIntroduction")
                   # Set to a new working directory.
list.files()       # List files at the PC directory.
.libPaths()        # Library pathname.
.Library           # Library pathname.
sessionInfo()      # R version, locale, and packages.
search()           # Attached packages and objects.
searchpaths()      # Attached packages and objects.
###############################################################