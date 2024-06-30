# Import CSV file
GenEnd.df <- utils::read.table(
  file = "data/ch1/GenderEndurance.csv",
  header = TRUE,
  dec = ".",
  sep = ","
)                      
# Quality assurance
ls()                   # List objects
attach(GenEnd.df)      # Attach the data, for later use
str(GenEnd.df)         # Identify structure
head(GenEnd.df, n = 3) # Show the first 3 rows
summary(GenEnd.df)     # Summary statistics
par(ask = TRUE)        # Confirm before plots are drawn
plot(
  density(GenEnd.df$Endurance, na.rm = TRUE),
  main = "Endurance of Selected Female and Male Subjects: Quality Assurance Density Plot",
  col = "red",
  lwd = 5
)                      

# Import TXT file
BreedMilk.df <- utils::read.table(
  file = "data/ch1/BreedMilkLb365.txt",
  header = TRUE,
  dec = ".",
  sep = "\t"
)                      
attach(BreedMilk.df)
str(BreedMilk.df)
head(BreedMilk.df, n = 3)
summary(BreedMilk.df)     
plot(
  density(BreedMilk.df$MilkLb365, na.rm = TRUE),
  main = "Annual Milk Production (Pounds) of Holstein and Jersey Cows: Quality Assurance Density Plot",
  col = "red",
  lwd = 5
)     

# Import fixed width file
Wellness.df <- utils::read.fwf(
  "data/ch1/WellnessInventory.txt",
  header = FALSE,
  skip = 0,            # Skip no lines
  na.strings = " ",    # Any spaces in the file will be treated as NA
  width = c(           # This vector defines the widths of the fields
    -1,3,
    -1,1,
    -1,10,
    -1,3,
    -1,1,
    -1,3,
    -1,2,
    -1,3,
    -1,3,
    -1,3,
    -1,3,
    -1,2,
    -1,1,
    -1,1,
    -1,1,
    -1,1,
    -1,1
  )
)
# Add column names to dataframe
names(Wellness.df) <- c(
  "ID",
  "Gender",
  "DOB",
  "Height",
  "Race",
  "Weight",
  "Waist",
  "SBP",
  "DBP",
  "RHR",
  "TBC",
  "Sleep",
  "Smoke",
  "Drink",
  "Income",
  "Exercise",
  "Food"
)
attach(Wellness.df)
str(Wellness.df)
head(Wellness.df, n = 3)
summary(Wellness.df)
plot(
  density(
    Wellness.df$Waist, 
    na.rm = TRUE),
    main = "Waist (Inches) of Selected Wellness Inventory Subjects: \nQuality Assurance Density Plot",
    col = "red",
    lwd = 5
)    

# Load a .xlsx file into R
Sorghum.df <- readxl::read_excel("data/ch1/Sorghum2012to2016.xlsx", 1)
attach(Sorghum.df)
str(Sorghum.df)
head(Sorghum.df, n = 3)
summary(Sorghum.df)

plot(
  density(Sorghum.df$BUperAcre, na.rm = TRUE),
  main = "Sorghum Yield (Bushels per Acre): \nQuality Assurance Density Plot",
  col = "red",
  lwd = 5
)

# Import a .csv file from an online source into R
library(data.table)
help(package = data.table)
sessionInfo()

ChildHealth.df <- data.table::fread(
  'https://chronicdata.cdc.gov/api/views/vba9-s8jp/rows.csv'
)
getwd()
ls()
attach(ChildHealth.df)
str(ChildHealth.df)
head(ChildHealth.df, n = 3)
summary(ChildHealth.df)

plot(
  density(Sorghum.df$BUperAcre, na.rm = TRUE),
  main = "Sorghum Yield (Bushels per Acre): \nQuality Assurance Density Plot",
  col = "red",
  lwd = 5
)

ChildHealthFruitVegetable.df <- ChildHealth.df[
  which(ChildHealth.df$Topic == "Fruits and Vegetables - Behavior"),
]

length(ChildHealthFruitVegetable.df$Data_Value)
table(is.na(ChildHealthFruitVegetable.df$Data_Value))

mean(ChildHealthFruitVegetable.df$Data_Value, na.rm = TRUE)
sd(ChildHealthFruitVegetable.df$Data_Value, na.rm = TRUE)
median(ChildHealthFruitVegetable.df$Data_Value, na.rm = TRUE)
summary(ChildHealthFruitVegetable.df$Data_Value)

par(ask=TRUE)
plot(
  density(ChildHealthFruitVegetable.df$Data_Value, na.rm = TRUE),
  main="Percent of Students in Grades 9-12 Who Consume Fruit Less 
  Than 1 Time Daily:  Quality Assurance Density Plot",
  col = "red", 
  lwd = 5
)
