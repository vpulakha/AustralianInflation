# An animated visualisation of Australian Inflation.  
# Background: The Reserve Bank of Australia (RBA) produces quarterly inflation figures.  The Australian 

# Install the packages required to generate the Inflation animated Infographic
library(data.table)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(gganimate)
library(readxl)
library(httr)

#Download the data from the ABS website -  use the series 640111 produced by ABS on CPI pricing
link <- "https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/sep-quarter-2022/640111.xlsx"
GET(link, write_disk(TF <- tempfile(fileext = ".xlsx")))

# temporarily read the file to figure out the number of columns this file includes - Assume that the first column is the Data column
temp <- read_excel(path = TF, sheet=2)
ncol.temp <- ncol(temp)

# Based on viewing the Excel sheet, the first column is the date column. The rest of the columns are numeric.
inflation <- read_excel(TF, sheet=2, col_types=c("date", rep("numeric", ncol.temp-1)))


# Clean up column names
names(inflation) <- gsub("Index Numbers ;  ", "", names(inflation))
names(inflation) <- gsub(";  Australia ;", "", names(inflation))
names(inflation)[1] <- "quarter"


#Get rid of the first 9 rows of the data and convert to a data.table
inflation <- as.data.table(inflation[9:nrow(inflation),])


# Convert date into standard time format
inflation$quarter <- ymd(inflation$quarter)

# Filter inflation data > 2012 (Note: ABS has indicated that the year 2011-2012 is the baseline index of 100 )
inflation <- inflation[quarter > "2012-06-30"]


# Convert the wide dataframe to long and remove the leading and trailing spaces in the variable column
dt <- melt(inflation, id.vars = c("quarter"))
dt$variable <- trimws(dt$variable)

# Filter out specific food categories of interest
food <- dt[variable %in% c(                      
   "Bread", "Cakes and biscuits ", "Breakfast cereals ",                                     
    "Beef and veal",                                         
   "Pork", "Lamb and goat", "Poultry", "Other meats",                                         
   "Fish and other seafood", "Milk",                                
    "Cheese",                                                
   "Ice cream and other dairy products",                    
   "Fruit",                                                 
   "Vegetables",                                            
   "Eggs",                                                  
   "Snacks and confectionery",                              
   "Coffee, tea and cocoa",                                 
   "Waters, soft drinks and juices",                        
   "Restaurant meals",                                      
   "Take away and fast foods",                              
   #"Alcohol and tobacco",                                   
   "Spirits",                                               
   "Wine",                                                  
   "Beer")]

# Filter out specific Shelter categories of interest
shelter <- dt[variable %in% c("Health","Housing", "Medical and hospital services","Rents", "New dwelling purchase by owner-occupiers",
                              "Maintenance and repair of the dwelling", "Property rates and charges",
                              "Water and sewerage", "Electricity", "Gas and other household fuels",
                              "Transport", "Motor vehicles", "Automotive fuel", "Urban transport fares",
                              "Communication", "Education", "Preschool and primary education","Dental services",
                              "Secondary education", "Tertiary education", "Insurance", "Child care")]

# Filter out specific "other" categories of interest
other <- dt[variable %in% c("Garments for men", "Garments for women", "Garments for infants and children", "Footwear for men",
                            "Footwear for women", "Footwear for infants and children", "Domestic holiday travel and accommodation", "International holiday travel and accommodation",
                             "Pets and related products", "Sports participation", "Games, toys and hobbies",
                             "Recreation and culture", "Clothing and footwear", "Major household appliances",
                             "Hairdressing and personal grooming services", "Personal care products", "Audio, visual and computing equipment",
                            "Audio, visual and computing media and services", "Telecommunication equipment and services",
                            "Veterinary and other services for pets", "Pets and related products")]


# Plot for Food 
p <- ggplot(data = food, aes(x = value, y = variable, color = variable)) + 
  geom_path() + 
  geom_point() + theme_minimal()+theme(legend.position="none")+
  transition_reveal(along = quarter) + 
  ease_aes("linear") +
  scale_x_continuous(limits = c(min(food$value), max(food$value)), breaks = seq(round(min(food$value), -1), round(max(food$value),-1), by=10))+
  labs(caption="Source: Australian Bureau of Statistics (ABS) - Seasonally adjusted",
       title="Australian Food Price Index: {(frame_along)}",
       subtitle="Price index for select food categories, 2011-12 = 100",
       y="", x="")+
  theme(plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0))+view_follow(fixed_x = TRUE,fixed_y = TRUE)

# Plot for Shelter
q <- ggplot(data = shelter, aes(x = value, y = variable, color = variable)) + 
  geom_path() + 
  geom_point() + theme_minimal()+theme(legend.position="none")+
  transition_reveal(along = quarter) + 
  ease_aes("linear") +
  scale_x_continuous(limits = c(min(shelter$value), max(shelter$value)), breaks = seq(round(min(shelter$value), -1), round(max(shelter$value),-1), by=10))+
  labs(caption="Source: Australian Bureau of Statistics (ABS) - Seasonally adjusted",
       title="Australian Shelter Price Index: {(frame_along)}",
       subtitle="Price index for select Shelter categories, 2011-12=100",
       y="")+
  theme(plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0))+view_follow(fixed_x = TRUE,fixed_y = TRUE)

# Plot for other
r <- ggplot(data = other, aes(x = value, y = variable, color = variable)) + 
  geom_path() + 
  geom_point() + theme_minimal()+theme(legend.position="none")+
  transition_reveal(along = quarter) + 
  ease_aes("linear") +
  scale_x_continuous(limits = c(min(other$value), max(other$value)), breaks = seq(round(min(other$value), -1), round(max(other$value),-1), by=10))+ 
  labs(caption="Source: Australian Bureau of Statistics (ABS) - Seasonally adjusted",
       title="Clothing+Other Price Index: {(frame_along)}",
       subtitle="Price index for select Clothing+Other categories, 2011-12=100",
       y="")+
  theme(plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0))+view_follow(fixed_x = TRUE,fixed_y = TRUE)


# Create Animated plots for the three plots - Frames per Second is 3 and height and width = 600 (you can change this as per your requirement)
animate(p, fps=3,height = 600, width =600)
animate(q, fps=3,height = 600, width =600)
animate(r, fps=3, height = 600, width =600)




