#Loading the packages
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)      # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model
library(cowplot)    # used for combining multiple plots 
library(magrittr)
#Reading in the data
train = fread("Train_UWu5bXk.csv") 
test = fread("Test_u94Q5KV.csv")
submission = fread("SampleSubmission_TmnO39y.csv")

#Check the dimensions of the data
dim(train)
dim(test)
names(train)
names(test)

#Structure of the data
str(train)
str(test)

#Combining the data sets for ease of use
test[, Item_Outlet_Sales := NA]
combi = rbind(train, test) 
dim(combi)
str(combi)

#Exploratory Data Analysis
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), 
                 binwidth = 100, fill = "darkblue") + xlab("Item Outlet Sales")

#Checking the trend and plotting numeric independent variables in a histogram
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "green")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "green")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "green" )
plot_grid(p1, p2, p3, nrow = 1) #plot_grid() from cowplot package

#Checking and plotting the categorical variables
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

#Combining LF, lowfat and other similar categories
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

#Plotting other categorical variables
#Plot for Item_Type
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) + 
  theme(axis.text = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

# Plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))
# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))
# ploting both plots together
plot_grid(p7, p8, ncol = 2)

#Bivariate analysis
#Scatter plots for continous/numeric variables 
#Violin plots for categorical variables

train = combi[1:nrow(train)] # extracting train data from the combined data

#Target Variable vs Independent Numerical Variables

# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + 
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))
second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)

#Target Variable vs Independent Categorical Variables

# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + 
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + 
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + 
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))
second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

#Outlet_Size vs Item_Outlet_Sales
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

#Outlet_Location_Type and Outlet_Type
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "brown")
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "brown")
plot_grid(p15, p16, ncol = 1)

# Missing values treatment
#Find the missing values in item weight
sum(is.na(combi$Item_Weight))

#Impute missing values with the mean weight
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index){
  
  item = combi$Item_Identifier[i] 
  combi$Item_Weight[i]= mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
  
}
sum(is.na(combi$Item_Weight))

#Replacing zeros in item visibility variable
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100) #visual depiction of 0's

zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier ==item], na.rm = T)
  
}

#Using feature engineering to add new variables to the system

#Variable:Item_type_new categorized as perishable/non-perishable
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

#Creating "Item_Type_new"
combi[, Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

#Categorizing items
#using item identifier : Drinks, food and non-consummable

table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))

#Categorizing items in Item_category
combi[, Item_category := substr(combi$Item_Identifier, 1, 2)]

#Change the "fat content" wherever category is "NC"
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"

#Calculating the Years of operation of an outlet
combi[, Outlet_Years := 2018 - combi$Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)

#Price per unit weight
combi[, price_per_unit_wt := Item_MRP/Item_Weight]

# creating new independent variable - Item_MRP_clusters
combi[,item_MRP_clusters := ifelse(Item_MRP < 69, "1st", 
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]
str(combi)

#Encoding categorical variables to increase accuracy.
#Label encoding first
combi[, Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                                  ifelse(Outlet_Size == "Medium", 1,2))]
combi[, Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                           ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
#Removing categorical variables after encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#One Hot encoding for categorical variables
ohe = dummyVars("~.", data = combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[, -c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[, "Item_Identifier"], ohe_df)

#Data Pre-processing
#handling the skewness of the item visibility and price per unit weight
#Using log transformation
combi[, Item_Visibility := log(Item_Visibility + 1)] #log +1 to avoid division by zero
combi[, price_per_unit_wt := log(price_per_unit_wt +1)]

#Scaling the numeric predicators from 0 to 1
num_vars = which(sapply(combi, is.numeric)) #index of numeric features
num_vars_name = names(num_vars)
combi_numeric = combi[, setdiff(num_vars_name, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method = c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)
combi[, setdiff(num_vars_name, "Item_Outlet_Sales") := NULL] #removing numeric independent variable
combi = cbind(combi, combi_numeric_norm)

#Splitting the combine data combi back to train and test
train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)]
test[, Item_Outlet_Sales := NULL]#Removing item_outlet_sales as it contains 

#Finding the corelated variables
cor_train = cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)
str(combi)
str(train)

#Building a linear regression model
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[, -c("Item_Identifier","Item_Weight","Item_Fat_ContentNon-Edible", "Item_Fat_ContentRegular", "Item_Visibility")])

#Making predictions on test data
# preparing dataframe for submission and writing it in a csv file
submission$Item_Outlet_Sales = predict(linear_reg_mod, test[,-c("Item_Identifier")])
write.csv(submission, "Linear_Reg_submit.csv", row.names = F)