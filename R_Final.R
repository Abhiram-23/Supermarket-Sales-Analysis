library(ggplot2)
library(corrplot)



#Establishing the working directory for the R project within the project folder.
setwd("add data path")
getwd()

#Reading the superMarketSales data set
superMarket <- read.csv(file="supermarketSales.csv",header = TRUE, sep=',')

#Displaying the superMarketSales data frame as a table
View(superMarket)

#Statistics of the superMarketSales data frame
str(superMarket)

#Printing the first five lines(head) of the superMarketSales data frame
head(superMarket)

#Printing the last five lines(tail) of the superMarketSales data frame
tail(superMarket)

#Summary of the data frame
summary(superMarket)

#Changing the names of the columns
colnames(superMarket) <- c("InvoiceId","Branch","City","CustomerType","Gender","ProductLine","UnitPrice","Quantity","Tax5%","Total","Date","Time","Payment","Cogs","GrossMarginPercent","GrossIncome","Rating")

#Converting the character to factor
superMarket$InvoiceId <- factor(superMarket$InvoiceId)
superMarket$Branch <- factor(superMarket$Branch)
superMarket$City <- factor(superMarket$City)
superMarket$CustomerType <- factor(superMarket$CustomerType)
superMarket$Gender <- factor(superMarket$Gender)
superMarket$ProductLine <- factor(superMarket$ProductLine)
superMarket$Payment <- factor(superMarket$Payment)

#Converting the character date to date format
superMarket$Date <- as.Date(superMarket$Date,format="%d-%m-%Y")

head(superMarket)

str(superMarket)

#Assessing the presence of missing or NA values in the data frame.
any(is.na(superMarket))
beforeCleaning <- dim(superMarket)
beforeCleaning
install.packages("tidyverse")
library(tidyr)
superMarket <-  drop_na(superMarket)
any(is.na(superMarket))
afterCleaning <- dim(superMarket)
afterCleaning

# Checking whether the data types of the variables are correct or not
is.factor(superMarket$InvoiceId)
is.factor(superMarket$Branch)
is.factor(superMarket$City)
is.factor(superMarket$CustomerType)
is.factor(superMarket$Gender)
is.factor(superMarket$ProductLine)
is.numeric(superMarket$UnitPrice)
is.numeric(superMarket$Quantity)
is.numeric(superMarket$`Tax5%`)
is.numeric(superMarket$Total)
is.numeric(superMarket$Time)
is.factor(superMarket$Payment)
is.numeric(superMarket$Cogs)
is.numeric(superMarket$GrossMarginPercent)
is.numeric(superMarket$GrossIncome)
is.numeric(superMarket$Rating)

#Removing the unwanted columns and using the necessary columns from the supermarket data frame
newSuperMarketData <- superMarket[,c("Branch","City","CustomerType","Gender","UnitPrice","Quantity","Tax5%","ProductLine","Total","Date","Time","Payment","Rating")]

head(newSuperMarketData)

summary(newSuperMarketData)

str(newSuperMarketData)




#Bar plot for the branch column
branch <- ggplot(newSuperMarketData, aes(x = Branch)) +
  geom_bar(fill = c("lightblue", "lightgreen", "lightcoral"), color = "black") +
  labs(title = "Distribution of Branches", x = "Branch", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(branch)


#Bar plot for the City column
city <- ggplot(newSuperMarketData, aes(x = City)) + 
  geom_bar(fill = c("#66c2a5", "#fc8d62", "#8da0cb"), color = "black") + 
  labs(title = "Distribution of Cities", x = "City", y = "Count") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

print(city)


#Bar plot for the Customer Type column
customerType <- ggplot(newSuperMarketData, aes(x = CustomerType)) + 
  geom_bar(fill = c("#94d2bd", "#e9d8a6"), color = "black") + 
  labs(title = "Distribution of Customer", x = "CustomerType", y = "Count") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

print(customerType)


#Bar plot for the Gender column
gender <- ggplot(newSuperMarketData, aes(x = Gender)) + 
  geom_bar(fill = c("#fb6f92", "#0096c7"), color = "black") + 
  labs(title = "Distribution of Genders", x = "Gender", y = "Count") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

print(gender)


#Bar plot for the Unit Price column
unitPrice <- ggplot(newSuperMarketData, aes(x = UnitPrice)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + 
  labs(title = "Distribution of Unit Prices", x = "Unit Price", y = "Frequency") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

print(unitPrice)


#Bar plot for the Quantity column
quantity <- ggplot(newSuperMarketData, aes(x = Quantity)) + 
  geom_bar(fill = "#9a031e", color = "#edf2f4") + 
  labs(title = "Distribution of Quanties", x = "Quantity", y = "Count") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

print(quantity)


#Bar plot for the Product Line column
productLine <- ggplot(newSuperMarketData, aes(x = ProductLine)) + 
  geom_bar(fill = c("#ff9f1c","#cbf3f0","#f4acb7","#b7b7a4","#cae9ff", "#e9d8a6"), color = "black") + 
  labs(title = "Product Line Distribution", x = "ProductLine", y = "Count") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5))

print(productLine)

#Bar plot for the Total column
breaks <- seq(min(superMarket$Total), max(superMarket$Total), length.out = 15)
total <- ggplot(newSuperMarketData, aes(x = cut(Total, breaks = breaks))) +
  geom_bar(fill = "#66c2a5", color = "black") +
  labs(title = "Total Price Distribution", x = "Total Price Range", y = "Count") +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+theme(plot.title = element_text(hjust = 0.5))
print(total)


#Bar plot for the Dates column
dates <- ggplot(newSuperMarketData, aes(x = format(Date, "%Y-%m"))) +
  geom_bar(fill = "#f7aef8", color = "black") +
  labs(title = "Monthly Date Distribution", x = "Month", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(plot.title = element_text(hjust = 0.5)) 
print(dates)


#Bar plot for the Time column
time <- ggplot(newSuperMarketData, aes(x = Time)) +
  geom_histogram(binwidth = 1, fill = "#9381ff", color = "black") +
  scale_x_continuous(breaks = seq(min(superMarket$Time), max(superMarket$Time)+1, by = 1)) +
  labs(title = "Time Distribution", x = "Time In Hours", y = "Count") +
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5)) 
print(time)


#Bar plot for the Payment column
payment <- ggplot(newSuperMarketData, aes(x =Payment)) + 
  geom_bar(fill = c("#4ecdc4","#ff6b6b","#ffe66d"), color = "black") + 
  labs(title = "Payment Type Distribution", x = "Payment", y = "Count") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
print(payment)

#Bar plot for the Rating column
rating <- ggplot(superMarket, aes(x = factor(Rating))) +
  geom_bar(fill = "#5e548e", color = "black") +
  labs(title = "Rating Distribution", x = "Rating", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks = seq(1, 10, by = 1))
print(rating)

#-----------------------------------------------------------------
str(newSuperMarketData)
# Select numeric columns
numeric_columns <- newSuperMarketData[, c("UnitPrice", "Quantity","Tax5%", "Total", "Rating")]
# Compute correlation matrix
cor_matrix <- cor(numeric_columns)
cor_matrix
corrplot(
  cor_matrix,
  method = 'color',
  addCoef.col = TRUE,
  tl.col = 'black',
  tl.srt = 90,
  number.cex = 1
)


#------------------------------scatter plot-----------------------------------
# Scatter plot for Unit Price vs. Quantity
ggplot(newSuperMarketData, aes(x = UnitPrice, y = Quantity, color = Quantity)) +
  geom_point() +
  labs(title = 'Scatterplot: UnitPrice vs. Quantity', x = 'Unit Price', y = 'Quantity') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot for Branch vs. Total
ggplot(newSuperMarketData, aes(x = Branch, y = Total ,color = Total)) +
  geom_point() +
  labs(title = 'Scatterplot: Branch VS Total', x = 'Branch', y = 'Total') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Scatter plot for Branch VS Rating
ggplot(newSuperMarketData, aes(x = Branch , y = Rating ,color = Total)) +
  geom_point() +
  labs(title = 'Scatterplot: Branch VS Rating', x = 'Branch', y = 'Rating') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_gradient(low = "red", high = "green")

# Scatter plot for Date VS Quantity
ggplot(newSuperMarketData, aes(x = `Date` , y = `Quantity` ,color = Total)) +
  geom_point() +
  labs(title = 'Scatterplot: Date VS Quantity', x = 'Date', y = 'Quantity') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_gradient(low = "blue", high = "red")

# Scatter plot for City VS Total
ggplot(newSuperMarketData, aes(x = City, y = Total, color = ProductLine)) +
  geom_point() +
  labs(title = 'Scatterplot: City VS Total', x = 'City', y = 'Total') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),   legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = 'right',legend.background = element_rect(fill = 'white', color = 'grey90'),
        panel.grid.major = element_line(color = 'grey90'),
        panel.grid.minor = element_line(color = 'grey95'),
        panel.background = element_rect(fill = 'white'),
        plot.background = element_rect(fill = 'white'),
        panel.border = element_rect(color = 'grey90', fill = NA, size = 0.7)) +
  scale_color_manual(values = c(
    "Electronic accessories" = "blue",
    "Food and beverages" = "red",
    "Health and beauty" = "green",
    "Home and lifestyle" = "purple",
    "Sports and travel" = "orange",
    "Fashion accessories" = "pink"
  ))


# Scatter plot for Product Line VS Quantity
ggplot(newSuperMarketData, aes(x = ProductLine , y = Quantity ,color = Gender)) +
  geom_point() +
  labs(title = 'Scatterplot: ProductLine VS Quantity', x = 'ProductLine', y = 'Quantity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(plot.title = element_text(hjust = 0.5))

# Scatter plot for Quantity VS Total
ggplot(newSuperMarketData, aes(x = Quantity , y = Total ,color = Rating)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = 'Scatterplot with linear regression line: Quantity VS Total', x = 'Quantity', y = 'Total') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_gradient(low = "red", high = "green")


#------------------------------Heat maps-----------------------------------
library(dplyr)

# Set up a common theme
common_theme <- theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = 'grey90'),
    panel.grid.minor = element_line(color = 'grey95'),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'white'),
    panel.border = element_rect(color = 'grey90', fill = NA, size = 0.7)
  )

# Heatmap for Unit Price vs. Quantity 
ggplot(newSuperMarketData, aes(x = UnitPrice, y = Quantity)) +
  geom_tile(aes(fill = ..density..), stat = "density2d", alpha = 0.7) +
  labs(title = 'Heatmap: UnitPrice vs. Quantity', x = 'Unit Price', y = 'Quantity') +
  common_theme

# Heatmap for Date VS Quantity 
ggplot(newSuperMarketData, aes(x = Date, y = Quantity, color = Total)) +
  geom_tile(aes(fill = ..density..), stat = "density2d", alpha = 0.7) +
  labs(title = 'Heatmap: Date VS Quantity', x = 'Date', y = 'Quantity') +
  common_theme

# Heatmap for Quantity VS Total 
ggplot(newSuperMarketData, aes(x = Quantity, y = Total, color = Rating)) +
  geom_tile(aes(fill = ..density..), stat = "density2d", alpha = 0.7) +
  labs(title = 'Heatmap: Quantity VS Total', x = 'Quantity', y = 'Total') +
  common_theme
#-----------------------Histograms--------------------
ggplot(newSuperMarketData, aes(x = Quantity)) +
  geom_histogram(fill = "#f48c06", color = "black", bins = 10) +
  labs(title = paste("Histogram of Quantity"), x = "Quantity", y = "Frequency") +
  theme_minimal()+  theme(plot.title = element_text(hjust = 0.5))

ggplot(newSuperMarketData, aes(x = UnitPrice)) +
  geom_histogram(fill = "#84a59d", color = "black", bins = 30) +
  labs(title = paste("Histogram of Unit Price"), x = "Unit Price", y = "Frequency") +
  theme_minimal()+  theme(plot.title = element_text(hjust = 0.5))

ggplot(newSuperMarketData, aes(x = Total)) +
  geom_histogram(fill = "#ffbe0b", color = "black", bins = 30) +
  labs(title = paste("Histogram of Total"), x = "Total", y = "Frequency") +
  theme_minimal()+  theme(plot.title = element_text(hjust = 0.5))

ggplot(newSuperMarketData, aes(x = Rating)) +
  geom_histogram(fill = "#cdb4db", color = "black", bins = 30) +
  labs(title = paste("Histogram of Rating"), x = "Rating", y = "Frequency") +
  theme_minimal()+  theme(plot.title = element_text(hjust = 0.5))

ggplot(newSuperMarketData, aes(x = Time)) +
  geom_histogram(fill = "#8ecae6", color = "black", bins = 24) +
  labs(title = paste("Histogram of Time"), x = "Time", y = "Frequency") +
  theme_minimal()+  theme(plot.title = element_text(hjust = 0.5))



#-----------------------------------Density Plots-------------------------------

# Density Plot for Quantity
ggplot(newSuperMarketData, aes(x = Quantity)) +
  geom_density(fill = "#f48c06", color = "black") +  
  labs(title = "Density Plot of Quantity", x = "Quantity", y = "Density") +  
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 

# Density Plot for Unit Price
ggplot(newSuperMarketData, aes(x = UnitPrice)) +
  geom_density(fill = "#84a59d", color = "black") +  
  labs(title = "Density Plot of Unit Price", x = "Unit Price", y = "Density") +  
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 

# Density Plot for Total
ggplot(newSuperMarketData, aes(x = Total)) +
  geom_density(fill = "#ffbe0b", color = "black") +  
  labs(title = "Density Plot of Total", x = "Total", y = "Density") +  
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 

# Density Plot for Rating
ggplot(newSuperMarketData, aes(x = Rating)) +
  geom_density(fill = "#cdb4db", color = "black") +  
  labs(title = "Density Plot of Rating", x = "Rating", y = "Density") +  
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 

# Density Plot for Time
ggplot(newSuperMarketData, aes(x = Time)) +
  geom_density(fill = "#8ecae6", color = "black") +  
  labs(title = "Density Plot of Time", x = "Time", y = "Density") +  
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 

#-------------------------Box plots---------------------------------------------

#Box plot for Branch and Unit Price
ggplot(newSuperMarketData, aes(x = Branch, y = UnitPrice, fill = Branch)) +
  geom_boxplot() +
  labs(title = 'Boxplot: Unit Price across Branches', x = 'Branch', y = 'Unit Price') +
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) 

#Box Plot for Branch and Total
ggplot(newSuperMarketData, aes(x = Branch, y = Total, fill = Branch)) +
  geom_boxplot() +
  labs(title = 'Boxplot: Total across Branches', x = 'Branch', y = 'Total') +
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) 

#Box Plot for Branch and Rating
ggplot(newSuperMarketData, aes(x = Branch, y = Rating, fill = Branch)) +
  geom_boxplot() +
  labs(title = 'Boxplot: Rating across Branches', x = 'Branch', y = 'Rating') +
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) 

#Box Plot for City and Unit price
ggplot(newSuperMarketData, aes(x = City, y = UnitPrice, fill = City)) +
  geom_boxplot() +
  labs(title = 'Boxplot: Unit Price across City', x = 'City', y = 'Unit Price') +
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5)) 

#Box Plot for City and Quantity
ggplot(newSuperMarketData, aes(x = City, y = Quantity, fill = City)) +
  geom_boxplot() +
  labs(title = 'Boxplot: Quantity across City', x = 'City', y = 'Quantity') +
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5))

#Box Plot for City and Time
ggplot(newSuperMarketData, aes(x = City, y = Time, fill = City)) +
  geom_boxplot() +
  labs(title = 'Boxplot: Time across City', x = 'City', y = 'Time') +
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5))

#Box Plot for City and Date
ggplot(newSuperMarketData, aes(x = City, y = Date, fill = City)) +
  geom_boxplot() +
  labs(title = 'Boxplot: Date across City', x = 'City', y = 'Date') +
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5))

# 
#,	
a <- c("Branch", "City", "CustomerType", "Gender", "ProductLine", "Payment")
b <- c("UnitPrice","Quantity", "Total", "Time", "Rating")

for (cat_var in a) {
  for (num_var in b) {
    # Box Plot
    p <- ggplot(newSuperMarketData, aes_string(x = cat_var, y = num_var, fill = cat_var)) +
      geom_boxplot() +
      labs(title = paste('Boxplot: ', num_var, ' across ', cat_var), x = cat_var, y = num_var) +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    # Print the plot
    print(p)
  }
}




# Assuming 'Total' is the dependent variable and other relevant columns as independent variables
#model <- lm(Total ~ UnitPrice + Quantity + Rating, data = newSuperMarketData)

model <- lm(Total~Branch + City + CustomerType + Gender + UnitPrice + Quantity + ProductLine + Date + Time + Payment + Rating,data=newSuperMarketData)
model
attributes(model)
k <- data.frame(UnitPrice=58.07,Branch="A",City="Yangon",CustomerType="Member",Gender="Male",ProductLine="Home and lifestyle",Quantity=9,Date="19-01-2019",Time=20.07,Payment="Ewallet",Rating=4.3)
predict(model,k)
# Summary of the linear regression model
summary(model)
summary(model)$r.squared
summary(model)$adj.r.squared
summary(model)$coefficient
plot(newSuperMarketData, pch = 16, col = "blue") #Plot the results
abline(model) #Add a regression line


# Assuming 'Total' is the dependent variable and 'Quantity' is the independent variable
model <- lm(Time ~ Branch, data = newSuperMarketData)
summary(model)
model2 <- lm(Total ~ UnitPrice + Quantity + Rating+Date, data = newSuperMarketData)
summary(model2)
# Scatter plot with regression line
library(ggplot2)

ggplot(newSuperMarketData, aes(x = UnitPrice, y = Quantity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot with Linear Regression Line", x = "Quantity", y = "Total")


# Selecting numerical variables from the data set
numeric_data <- newSuperMarketData[, sapply(newSuperMarketData, is.numeric)]
# Scaling the numeric data
scaled_data <- scale(numeric_data)
#head of numeric data
head(numeric_data)
# Performing k-means clustering with k=3 (you can adjust k as needed)
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3)

# Adding cluster labels to the original dataset
superMarket$Cluster <- kmeans_result$cluster

# Printing the results
print(kmeans_result)
head(superMarket,10)
