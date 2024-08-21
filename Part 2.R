library(dplyr)
standardized_columns <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')
library(readr)
library(readxl)

data_2016 <- read_csv("C:/Users/dr. naresh ledwani/Downloads/2016_brooklyn (1).csv", skip = 4, col_names = standardized_columns)
data_2018 <- read_csv("C:/Users/dr. naresh ledwani/Downloads/2018_brooklyn (1).csv", skip = 4, col_names = standardized_columns)
data_2019 <- read_csv("C:/Users/dr. naresh ledwani/Downloads/2019_brooklyn (1).csv", skip = 4, col_names = standardized_columns)
data_2020 <- read_csv("C:/Users/dr. naresh ledwani/Downloads/2020_brooklyn (1).csv", skip = 6, col_names = standardized_columns)
data_2017 <- read_csv("C:/Users/dr. naresh ledwani/Downloads/2017_brooklyn (1).csv", skip = 4, col_names = standardized_columns)

#  function to standardize data types
standardize_data_types <- function(data_list, data_types) {
  for (data_name in names(data_list)) {
    for (col_name in names(data_types)) {
      current_type <- data_types[[col_name]]
      if (current_type == "numeric") {
        data_list[[data_name]][[col_name]] <- as.numeric(gsub("[^0-9.-]", "", data_list[[data_name]][[col_name]]))
      } else if (current_type == "character") {
        data_list[[data_name]][[col_name]] <- as.character(data_list[[data_name]][[col_name]])
      } # Add other data type conversions if needed
    }
  }
  return(data_list)
}

# a list of datasets
data_list <- list(
  data_2016 = data_2016,
  data_2017 = data_2017,
  data_2018 = data_2018,
  data_2019 = data_2019,
  data_2020 = data_2020
)

data_types <- list(
  "borough" = "character",
  "neighborhood" = "character",
  "bldclasscat" = "character",
  "taxclasscurr" = "character",
  "grosssqft" = "numeric",
  "totunits" = "numeric",
  "resunits" = "numeric",
  "comunits" = "numeric",
  "price" = "numeric",
  "block" = "numeric",
  "lot" = "numeric",
  "address" = "character",
  "zip" = "numeric",
  "yrbuilt" = "numeric",
  "taxclasssale" = "numeric",
  "bldclasssale" = "character",
  "landsqft" = "numeric"  
)

# Apply the function to standardize data types
standardized_data <- standardize_data_types(data_list, data_types)

# Combine all datasets into one
all_data <- bind_rows(standardized_data$data_2016, standardized_data$data_2017, standardized_data$data_2018, standardized_data$data_2019, standardized_data$data_2020)
summary(all_data)
str(all_data)


# Filter the data based on the given conditions
filtered_data <- all_data %>%
  filter(
    substr(bldclasssale, 1, 1) %in% c('A', 'R'),  # Building class starts with 'A' or 'R'
    totunits == 1,                                  # Total units is 1
    resunits == 1,                                  # Residential units is 1
    grosssqft > 0,                                  # Gross square footage is more than 0
    !is.na(price)                                   # Sale price is non-missing
  )
summary(filtered_data)
str(filtered_data)

# the number of rows are 19640 at this point
library(dplyr)
# Trimming white space 
filtered_data$neighborhood <- gsub("\\s+", "", filtered_data$neighborhood)
str(filtered_data)
filtered_data$bldclasscat <- gsub("\\s+", "", filtered_data$bldclasscat)
filtered_data$address <- gsub("\\s+", "", filtered_data$address)
str(filtered_data)
library(lubridate)
#changing date column
filtered_data$date <- mdy(filtered_data$date)
str(filtered_data)

# Extracting year, month, quarter and day
filtered_data$year <- lubridate::year(filtered_data$date)
filtered_data$month <- lubridate::month(filtered_data$date)
filtered_data$day <- lubridate::day(filtered_data$date)
filtered_data$quarter <- quarter(filtered_data$date)


#EDA FOR ANALYSIS OF COLUMNS

# Checking the distribution of the response variable (price)
hist(filtered_data$price, main = "Distribution of Price", xlab = "Price")
# Scatter plot between gross square footage and price 
plot(filtered_data$grosssqft, filtered_data$price, 
     main = "Scatter Plot: Gross Sqft vs Price", 
     xlab = "Gross Square Footage", ylab = "Price")

boxplot(filtered_data$landsqft, main="Landsqft Boxplot")
boxplot(filtered_data$grosssqft, main="Grosssqft Boxplot")
boxplot(filtered_data$price, main="Price Boxplot")



# sqrt(Price) vs. Gross Sqft
plot(filtered_data$grosssqft, sqrt(filtered_data$price), 
     main = "sqrt(Price) vs Gross Sqft", xlab = "Gross Square Footage", ylab = "sqrt(Price)")



library(ggplot2)

ggplot(filtered_data, aes(x = neighborhood, y = price)) +
  geom_boxplot(fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of Prices Across Neighborhoods",
       x = "Neighborhood",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#block
ggplot(filtered_data, aes(x = as.factor(block), y = price)) +
  geom_point(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Prices Across Blocks",
       x = "Block",
       y = "Price") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


cor_matrix <- cor(filtered_data[, c("grosssqft", "block", "landsqft", "price" , "lot")])
library(corrplot)
corrplot::corrplot(cor_matrix, method = "circle")
#filtered_data$borough <- as.numeric(as.factor(filtered_data$borough))
str(filtered_data)
#filtered_data <- filtered_data %>% mutate(bldclasssale = as.factor(bldclasssale))
#filtered_data <- filtered_data %>% mutate(zip = as.factor(zip))
#filtered_data <- filtered_data %>% mutate(yrbuilt = as.factor(yrbuilt))
#filtered_data <- filtered_data %>% mutate(year = as.factor(year))

filtered_data$neighborhood <- factor(filtered_data$neighborhood)

# subsetting the data with research after the EDA
#rows reduced to 13493
filtered_data <- subset(filtered_data, price >= 10000 & price <= 5000000)
#unique_neighborhoods <- unique(filtered_data$neighborhood)
#print(unique_neighborhoods)
#used external site like Wikipedia to group all the nieghborhoods into geographical locations
levels(filtered_data$neighborhood) <- list("Center" = c("CROWNHEIGHTS", "FLATBUSH-CENTRAL","FLATBUSH-EAST","FLATBUSH-LEFFERTSGARDEN","FLATBUSH-NORTH","WINDSORTERRACE","KENSINGTON","OCEANPARKWAY-NORTH","OCEANPARKWAY-SOUTH"),
                                 "East" = c("BROWNSVILLE", "CANARSIE","EASTNEWYORK","CYPRESSHILLS","SPRINGCREEK"),
                                 "North" = c("BEDFORDSTUYVESANT","OCEANHILL","BUSHWICK","WYCKOFFHEIGHTS","WILLIAMSBURG-EAST","GREENPOINT","WILLIAMSBURG-NORTH","WILLIAMSBURG-SOUTH","WILLIAMSBURG-CENTRAL"),
                                 "NorthWest" = c("BROOKLYNHEIGHTS","NAVYYARD","CLINTONHILL","DOWNTOWN-FULTONMALL","DOWNTOWN-FULTONFERRY","DOWNTOWN-METROTECH","FORTGREENE","PROSPECTHEIGHTS","BOERUMHILL","CARROLLGARDENS","COBBLEHILL","GOWANUS","PARKSLOPE","PARKSLOPESOUTH","REDHOOK","COBBLEHILL-WEST"),
                                 "South" = c("BERGENBEACH","CONEYISLAND","BRIGHTONBEACH","MANHATTANBEACH","SEAGATE","SHEEPSHEADBAY","MADISON","FLATLANDS","GERRITSENBEACH","GRAVESEND","MARINEPARK","MILLBASIN","OLDMILLBASIN","MIDWOOD"),
                                 "SouthWest"= c("BAYRIDGE","BENSONHURST","BATHBEACH","BOROUGHPARK","DYKERHEIGHTS","SUNSETPARK","BUSHTERMINAL"))

library(dplyr)


str(filtered_data)
#dividing block and lot into bins( factors)
filtered_data$block_break <- cut(filtered_data$block, breaks = 12)
filtered_data$lot_break <- cut(filtered_data$lot, breaks = 3)

#best model after many attempts
bestmodel35 = lm((price) ~ sqrt(grosssqft) * block_break  + 
                +neighborhood*sqrt(grosssqft)+lot_break*grosssqft  ,
                data = filtered_data)
summary(bestmodel35)
plot(bestmodel35)

# Adjusted R square : 0.6248 , DOF - 38
RMSE_value <- sqrt(mean((bestmodel35$residuals)^2))
#RMSE : 415785
# ALL THE FOUR CRITERIAS MENTIONED IN THE ASSIGNMENT ARE SATISFIED
filtered_data <- filtered_data %>% mutate(year = as.factor(year))
filtered_data <- filtered_data %>% mutate(quarter = as.factor(quarter))

saveRDS(list(model=bestmodel35, data=filtered_data), file='Devanshi.RDS') 

#getwd()
# FINAL ASSIGNMENT 2
str(filtered_data$quarter)


Q3_2020_data <- filtered_data %>% 
  filter(year == "2020", quarter == "3")

Q4_2020_data <- filtered_data %>% 
  filter(year == "2020", quarter == "4")
# Predicting prices using the model for Q3 and Q4
Q3_2020_data$predicted_price <- predict(bestmodel35, newdata = Q3_2020_data, type = "response")^2
Q4_2020_data$predicted_price <- predict(bestmodel35, newdata = Q4_2020_data, type = "response")^2

# Calculating average predicted prices for each quarter
avg_price_Q3_2020 <- mean(Q3_2020_data$predicted_price, na.rm = TRUE)
avg_price_Q4_2020 <- mean(Q4_2020_data$predicted_price, na.rm = TRUE)

# Calculating the change in prices
price_change <- avg_price_Q4_2020 - avg_price_Q3_2020
print(price_change)
print(avg_price_Q3_2020)
print(avg_price_Q4_2020)


bestmodel345 = lm((price) ~ sqrt(grosssqft) * block_break  + quarter
                   +neighborhood*sqrt(grosssqft)+lot_break*grosssqft  ,
                 data = filtered_data)
summary(bestmodel345)
plot(bestmodel345)


Q3_2020_data$quarter <- as.factor(Q3_2020_data$quarter)
Q4_2020_data$quarter <- as.factor(Q4_2020_data$quarter)

library(ggplot2)
ggplot() +
  geom_density(data = Q3_2020_data, aes(x = predicted_price, fill = quarter),
               alpha = 0.5, color = "black", size = 0.7) +
  geom_density(data = Q4_2020_data, aes(x = predicted_price, fill = quarter),
               alpha = 0.5, color = "black", size = 0.7) +
  labs(title = "Density Plot of Q3/Q4 Predicted Housing Sale Prices",
       x = "Predicted Housing Sale Prices",
       y = "Density") +
  theme_minimal()


ggplot(Q3_2020_data, aes(x = quarter, y = predicted_price, fill = quarter)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) + 
  labs(title = "Box Plot of Q3 Predicted Housing Sale Prices",
       x = "Quarter",
       y = "Predicted Housing Sale Prices (Log Scale)") +
  theme_minimal()
ggplot(Q4_2020_data, aes(x = quarter, y = predicted_price, fill = quarter)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) + 
  labs(title = "Box Plot of Q4 Predicted Housing Sale Prices",
       x = "Quarter",
       y = "Predicted Housing Sale Prices (Log Scale)") +
  theme_minimal()

# Q-Q plot for residuals
qqnorm(residuals(bestmodel35))
qqline(residuals(bestmodel35), col = "red")

str(filtered_data)
anova_result <- anova(bestmodel345)

print(anova_result)



#t_test_result <- t.test(Q3_2020_data$predicted_price, Q4_2020_data$predicted_price, paired = FALSE, var.equal = FALSE)

#print(t_test_result)


# Durbin-Watson Test
library(lmtest)
dwtest(bestmodel3_qu)




