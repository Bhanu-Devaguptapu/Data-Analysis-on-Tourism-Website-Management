rm(list = ls)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(viridis)
library(moments) #To find skewness and kurtosis
library(cowplot)
###TOURISM DATASET
##1.Loading the data into R studio:
#Data Collection:
df = read.csv(file.choose(), header = T)
df
##To view dataset in another file
View(df)
#UserID:	Unique ID of the user
#Buy_ticket:	Buy a ticket in the next month (target variable)
#Yearly_avg_view_on_travel_page:	Average yearly views on any travel-related page by the user
#preferred_device:	Preferred device for user login
#total_likes_on_outstation_checking_given:	Total number of likes given by the user on out-of-station check-ins in the last year
#yearly_avg_Outstation_checkins:	Average number of out-of-station check-ins done by the user
#member_in_family:	Total number of relationships mentioned by the user in the account
#preferred_location_type:	Preferred type of location for traveling by the user
#Yearly_avg_comment_on_travel_page:	Average yearly comments on any travel-related page by the user
#total_likes_on_outofstation_checkin_received:	Total number of likes received by the user on out-of-station check-ins in the last year
#week_since_last_outstation_checkin:	Number of weeks since the last out-of-station check-in update by the user
#following_company_page:	Whether the customer is following the company page (Yes or No)
#montly_avg_comment_on_company_page:	Average monthly comments on the company page by the user
#working_flag:	Whether the customer is working or not
#travelling_network_rating:	The rating indicating if the user has close friends who also like traveling. 1 is high, 4 is lowest
#Adult_flag:	Whether the customer is an adult or not
#Daily_Avg_mins_spend_on_traveling_page	Average time spent on the company's travel page by the user


#To find top rows in dataset
head(df)
#To find last rows in dataset
tail(df)
##To know the rows and columns in dataset (dimensions)
dim(df)
##To find out if there are any null values in the dataset
is.na(df)
##To find out null values count in every column in the datset
colSums(is.na(df)) 
#To find the duplicate values in the given dataset
sum(duplicated(df$UserID))
#As there is no duplicate values in UserID means there is no repetition of same customer
#To find the datatype of each column in the dataset
str(df) ##This functions shows data type along with some data in the column like structure
c<-sapply(df,class)
c
##To fill missing values with 'NA' in a dataset
df[df=='']<-NA
print(df)
#To know any unique values present in the given dataset
unique(df$Taken_product) #It should have only two values-"YES" or "NO"
unique(df$preferred_device) #It should have 7 categories like ios,Android,others,Laptop,Mobile,ios and android and tab
unique(df$member_in_family) #In given data there are maximum range of 10 members in a family 
unique(df$preferred_location_type) #There are mainly 14 categories in this column
unique(df$following_company_page) #It should has only two values like - "YES" or "NO"
unique(df$working_flag) #It should has only two values like - "YES" or "NO"
unique(df$travelling_network_rating) #It should have the range of 1 to 4
unique(df$Adult_flag) ##It should have only 2 values 0 and 1.

#Transforming each column data into its actual/preferred data values
#To replace NA values in this column with mean of the column
df$Yearly_avg_view_on_travel_page[is.na(df$Yearly_avg_view_on_travel_page)]<-mean(df$Yearly_avg_view_on_travel_page,na.rm=TRUE) 
df$Yearly_avg_view_on_travel_page <- floor(df$Yearly_avg_view_on_travel_page)
##To convert the data values into proper lower case in this column
#this is preferred_device column
table(df$preferred_device) #To know the count of the datavalues in a column
df$preferred_device[df$preferred_device == 'ANDROID'] <- 'Android'
df$preferred_device[df$preferred_device == 'Other'] <- 'Others'
df$preferred_device[df$preferred_device == 'Android OS'] <- 'Android'
table(df$preferred_device) #To check once again
unique(df$preferred_device) #To check whether there are still any NA values in the column
df
sum(is.na(df$preferred_device)) #To check no.of NA values in this column
# Find the indices of NA values in preferred_device column
na_indices <- which(is.na(df$preferred_device))
na_indices
# Determine the replacement values
replacement_values <- c("Android", "iOS", "iOS and Android", "Mobile","Tab","Laptop","Others")  # Add more values as needed
# Specify the number of replacement values in each group
replacement_count <- 7
#As there are 53 NA values in preferred_device column then I replaced every 7 NA values with all the different categories in this column
# Loop through replacement_values
start_index <- 1
for (replacement_value in replacement_values) {
# Replace NA values with the current replacement value
df$preferred_device[na_indices[start_index:(start_index + replacement_count - 1)]] <- replacement_value
# Update the starting index for the next set of replacement values
start_index <- start_index + replacement_count
# Break the loop if all NA values are replaced
if (start_index > length(na_indices)) {
    break
  }
}
#Again check no.of NA values in the column
sum(is.na(df$preferred_device))
table(df$preferred_device)
#As 4 NA values left in this column we will replace the values with more frequent value "Tab"
your_value <- 'Tab'
num_na_to_replace <- 4
# Find the last 4 NA values in the column
last_na_indices <- tail(which(is.na(df$preferred_device)), n = num_na_to_replace)
# Replace the NA values with the specified value
df$preferred_device[last_na_indices] <- your_value
#So the preferred_device column in data set cleaned.
#########################
#Cleaning of total_likes_on_outstation_checkin_given column
#To find NA values in Yearly_avg_view_on_travel_page column
sum(is.na(df$total_likes_on_outstation_checkin_given))
#We can replace all the 381 NA values with the mean of this column so that there will be no change in the data
df$total_likes_on_outstation_checkin_given[is.na(df$total_likes_on_outstation_checkin_given)]<-mean(df$total_likes_on_outstation_checkin_given,na.rm=TRUE) 
#As there is no use of float values in likes column. So we will remove the decimal values.
df$total_likes_on_outstation_checkin_given <- as.numeric(df$total_likes_on_outstation_checkin_given)
df$total_likes_on_outstation_checkin_given <- floor(df$total_likes_on_outstation_checkin_given)
###############################
#Cleaning of yearly_avg_Outstation_checkins column
sum(is.na(df$yearly_avg_Outstation_checkins))
#To check the indices of NA values in this column
na_indices<-which(is.na(df$yearly_avg_Outstation_checkins))
na_indices
#table function is used to show the count of each unique value in that column
table(df$yearly_avg_Outstation_checkins)
#To convert '*' value into NA
df$yearly_avg_Outstation_checkins[df$yearly_avg_Outstation_checkins == "*"] <- NA
#As the given column is already the average no.of out station checkins, So we will replace the NA values with the mode of same column
# Calculate the mode of the 'yearly_avg_Outstation_checkins' column
mode_value <- as.numeric(names(sort(table(df$yearly_avg_Outstation_checkins), decreasing = TRUE)[1]))
# Replace NA values in the 'yearly_avg_Outstation_checkins' column with the mode value
df$yearly_avg_Outstation_checkins[is.na(df$yearly_avg_Outstation_checkins)] <- mode_value
#To check if there are any NA values left or not
sum(is.na(df$yearly_avg_Outstation_checkins))
#############################
#There is no NA value in the "member_in_family" column 
table(df$member_in_family)
sum(is.na(df$member_in_family))
#replacing "Three" with 3
df$member_in_family[df$member_in_family == "Three"] <- 3
#To check again if there "Three" is added to 3 or not 
table(df$member_in_family)
######################
sum(is.na(df$preferred_location_type))
table(df$preferred_location_type)
#To convert "Tour  Travel" into "Tour and Travel" because they both are same
df$preferred_location_type[df$preferred_location_type=="Tour  Travel"]<-"Tour and Travel"
unique(df$preferred_location_type)
#We will find the mode of the given data to replace NA values with it
freq_table<-table(df$preferred_location_type)
mode_value <- names(freq_table)[which.max(freq_table)]
cat("Mode:", mode_value, "\n") 
#To replace the NA values with the mode value i.e Beach
df$preferred_location_type[is.na(df$preferred_location_type)]<-mode_value
sum(is.na(df$preferred_location_type))
############################
sum(is.na(df$Yearly_avg_comment_on_travel_page))# There are 206 NA values in the given dataset
#Calculating the mean value of Yearly_avg_comment_on_travel_page column
# Calculate the mean without decimals
mean_value <- round(mean(df$Yearly_avg_comment_on_travel_page, na.rm = TRUE))
mean_value #75 is mean value
# Replace NA values with the rounded mean
df$Yearly_avg_comment_on_travel_page[is.na(df$Yearly_avg_comment_on_travel_page)] <- mean_value
#To check if any NA values left
na_indices=which(is.na(df$Yearly_avg_comment_on_travel_page))
na_indices
########################
#To check if there are any null values 
sum(is.na(df$total_likes_on_outofstation_checkin_received))
#To check if there are any negative values in the column
print(subset(df, total_likes_on_outofstation_checkin_received < 0)$total_likes_on_outofstation_checkin_received)
####################
sum(is.na(df$week_since_last_outstation_checkin))
#To check if there are any negative values in the column
print(subset(df, week_since_last_outstation_checkin < 0)$week_since_last_outstation_checkin)
#############################
sum(is.na(df$following_company_page))
table(df$following_company_page)
##To convert the data into mainly 2 categories i.e "Yes" or "No"
df$following_company_page[df$following_company_page=='Yeso']<-'Yes'
df$following_company_page[df$following_company_page==1]<-'Yes'
df$following_company_page[df$following_company_page==0]<-'No'
table(df$following_company_page)
####################
sum(is.na(df$montly_avg_comment_on_company_page))
# Find the range of the column
column_range <- range(df$montly_avg_comment_on_company_page)
# Print the range
cat("Range of the column:", column_range, "\n")
#There highest commenting range is 500 and lowest is 4
#########################
sum(is.na(df$montly_avg_comment_on_company_page))
table(df$working_flag)
#TRANSFORMATION of data into 0's and 1's
df$working_flag[df$working_flag=='Yes']<-1
df$working_flag[df$working_flag=="No"]<-0
###################
sum(is.na(df$travelling_network_rating))
# Find the range of the column
column_range <- range(df$travelling_network_rating)
# Print the range
cat("Range of the column:", column_range, "\n")
#The range of the friends who interested in tourism along with the user is from 1 to 10
###########################
sum(is.na(df$Adult_flag))
table(df$Adult_flag)
#There is only 1 NA value and replacing it with 0
df$Adult_flag[is.na(df$Adult_flag)] <- 0
#Finding out the outliers in the data
Q1 <- quantile(df$Adult_flag, 0.25)
Q3 <- quantile(df$Adult_flag, 0.75)
Q1
Q3
IQR <- Q3 - Q1
IQR
# Define the lower and upper bounds for potential outliers
lower_bound <- Q1 
upper_bound <- Q3 
# Identify potential outliers
outliers <- df$Adult_flag < lower_bound | df$Adult_flag> upper_bound
# Print the results
cat("Lower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")
cat("Potential outliers:", df$Adult_flag[outliers], "\n")
#Create a boxplot highlighting outliers
boxplot(df$Adult_flag, main = "Boxplot with Outliers Highlighted", ylab = "Values", outline = TRUE)  
points(which(outliers), df$Adult_flag[outliers], col = "red", pch = 16)
#This plot shows us the outliers that is 2,3 and the IQR = 1 i.e Q2 median as the lower bound is 0.
#We will replace the outlier values with the main values i.e 1 and 0
df$Adult_flag[df$Adult_flag==2]<-"Outlier"
df$Adult_flag[df$Adult_flag==3]<-"Outlier"
##Now divide it with two
table_result <- table(df$Adult_flag)
outlier_count <- table_result["Outlier"]
# Divide the outlier count by 2
divided_outlier_count <- outlier_count / 2
# Update the counts in the "0" and "1" categories
table_result["0"] <- table_result["0"] + divided_outlier_count
table_result["1"] <- table_result["1"] + divided_outlier_count
# Update the original data frame with the new counts
df$Adult_flag[df$Adult_flag == "Outlier"] <- rep(c("0", "1"), each = divided_outlier_count)
table(df$Adult_flag)
##########################
#To fin out NA values
sum(is.na(df$Daily_Avg_mins_spend_on_traveling_page))
# Calculate the mean without decimals
mean_value <- round(mean(df$Daily_Avg_mins_spend_on_traveling_page, na.rm = TRUE))
# Print the range 
column_range <- range(df$Daily_Avg_mins_spend_on_traveling_page)
cat("Range of the column:", column_range, "\n")
# Replace NA values with the rounded mean
df$Daily_Avg_mins_spend_on_traveling_page[is.na(df$Daily_Avg_mins_spend_on_traveling_page)] <- mean_value
########################################################
#To know the basic statistical values about all the columns in the given data set
describe(df)
##Adding "min" string to the Daily_Avg_mins_spend_on_traveling_page column
df$Daily_Avg_mins_spend_on_traveling_page<- paste(df$Daily_Avg_mins_spend_on_traveling_page,"min")
#To know the data type of column after adding string.
class(df$Daily_Avg_mins_spend_on_traveling_page)
#Changing of Column indices for our convenience
# Specify the desired order of column indices
column_indices <- c(1,2,12,4,8,3,5,6,7,9,10,11,13,14,15,16,17)
# Reorder the columns using column indices
df <- df[, column_indices]
#Performing Normalization on week_since_last_outstation_checkin
df <- df %>%
  mutate(normalized_column = (week_since_last_outstation_checkin - min(week_since_last_outstation_checkin)) /
           (max(week_since_last_outstation_checkin) - min(week_since_last_outstation_checkin)))
#Performing Standardization on week_since_last_outstation_checkin
df <- df %>%
  mutate(standardized_column = (week_since_last_outstation_checkin - mean(week_since_last_outstation_checkin)) /
           sd(week_since_last_outstation_checkin))
######################################
#Data Visualization:
#As the Taken product is a target variable so, we can show the percentage of users going to buy ticket next month
counts <- table(df$Taken_product)
counts
# Create a 3D pie chart with plotly
pie_chart_3d <- plot_ly(labels = names(counts), values = counts, type = 'pie', pull = c(0.1, 0),
                        text = paste(names(counts), "(", round(prop.table(counts) * 100, 1), "%)"),
                        marker = list(colors = c("skyblue", "lightcoral"),line = list(color = 'black', width = 2)),
                        textinfo = "text+percent",title = "3D Percentage Distribution of Target Variable")
#To plot a pie chart
print(pie_chart_3d)
#Observation: By this we can say that there is only 16% of users who are interested in buying tickets and 89% of users are not interested in buying tickets
##########################
count <- table(df$following_company_page)
count
# Create a bar plot
bar_colors <- c("red", "green")
bp<-barplot(count, col = c("red", "green"), main = "Following Company page",
        xlab = "Categories", ylab = "Count", names.arg = c("NO", "YES"))
# Add legend
legend("topright", legend = c("NO", "YES"), fill = bar_colors,text(x = barplot(count) - 0.2, y = count + 0.2, labels = count))
#Add text labels for each bar with matching colors
text(bp,x = bp, y =bp, labels = count,pos = 3, col = "black", cex = 1.2)
#Observation: In this bar plot we can see that the red bar indicates the no.of users not following page and green bar indicates no.of users are following company's page
####################
# Create a bar plot
device_counts = table(df$preferred_device)
device_counts
bar_colors<-c('orange','forestgreen','slategrey','magenta','lightgreen','blue','purple')
bp<-barplot(device_counts, col =bar_colors , main = "Devices Used by Users",
        xlab = "Devices", ylab = "Count", ylim = c(0, 5000), space = 0.5)
# Add data labels near the bars
text(x = bp, y = device_counts + 0.2, labels = device_counts, pos = 3, col = "black", cex = 1.2)
# Add legend with reduced size
legend("topleft", legend = names(device_counts), fill =bar_colors, title = "Devices", cex = 0.7)
#Observation: By the above bar plot we can observe that users mostly preferred "Tab" to log in into the tourism page
################################
category<-unique(df$preferred_location_type)
count<-table(df$preferred_location_type)
count

# Create a data frame for plotting
plot_data <- data.frame(
  category = as.factor(names(count)),
  count = as.numeric(count)
)
my_palette <- viridis_pal()(14)
my_palette
# Calculate midpoints for labeling
midpoints <- cumsum(plot_data$count) - plot_data$count / 2
# Create a donut chart using ggplot2
ggplot(plot_data, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_bar(stat = "identity", width = 0.3, color = "white", fill = "white") +
  coord_polar(theta = "y",start = 0) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.key.size = unit(2, "lines"),  # Adjust the size of the legend key
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5) # Adjust the size of the legend text
  ) +
  scale_fill_manual(values = my_palette)+
  ggtitle( "Donut Chart of Preferred Location Types")
#Observation: From the above graph we can say that most of the users are intereseted in "Beach" location
################################
#To find the least viewers
least_avg_views <- df %>%
  filter(Yearly_avg_view_on_travel_page == min(Yearly_avg_view_on_travel_page)) %>%
  select(UserID, Yearly_avg_view_on_travel_page)
print(least_avg_views)
#To find the users who view maximum 
maximum_avg_views <- df %>%
  filter(Yearly_avg_view_on_travel_page == max(Yearly_avg_view_on_travel_page)) %>%
  select(UserID, Yearly_avg_view_on_travel_page)
print(maximum_avg_views)
# Plot the histogram
data=data.frame(df$Yearly_avg_view_on_travel_page)
# Create a histogram with density
ggplot(data, aes(x = df$Yearly_avg_view_on_travel_page)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", color = "black", alpha = 2) +
  geom_vline(aes(xintercept = mean(df$Yearly_avg_view_on_travel_page)), color = "red", linetype = "dashed", size = 1,show.legend = TRUE) +
  geom_text(aes(x = mean(df$Yearly_avg_view_on_travel_page) + 2,y=0.02, label = "Mean"), color = "red", vjust = -2, size = 3) +
  geom_vline(aes(xintercept = quantile(df$Yearly_avg_view_on_travel_page, 0.25)), color = "green", linetype = "dashed", size = 1,show.legend = TRUE) +
  geom_text(aes(x = quantile(df$Yearly_avg_view_on_travel_page, 0.25), y = 0.02, label = "Q1"), color = "green", vjust = -2, size = 3) +
  geom_vline(aes(xintercept = median(df$Yearly_avg_view_on_travel_page)), color = "purple", linetype = "dashed", size = 1,show.legend = TRUE) +
  geom_text(aes(x = median(df$Yearly_avg_view_on_travel_page), y = 0.02, label = "Median"), color = "purple", vjust = -0.5, size = 3) +
  geom_vline(aes(xintercept = quantile(df$Yearly_avg_view_on_travel_page, 0.75)), color = "green", linetype = "dashed", size = 1,show.legend = TRUE) +
  geom_text(aes(x = quantile(df$Yearly_avg_view_on_travel_page, 0.75), y = 0.02, label = "Q3"), color = "green", vjust = -2, size = 3) +
  labs(title = "Distribution of Yearly Average Views per User",
       x = "Average Views per User",
       y = "Density",
       caption =paste(
         "Skewness: ", skewness(df$Yearly_avg_view_on_travel_page),
         "\nKurtosis: ", kurtosis(df$Yearly_avg_view_on_travel_page),
         "\nMean: ", mean(df$Yearly_avg_view_on_travel_page),
         "\nQ1: ", quantile(df$Yearly_avg_view_on_travel_page, 0.25),
         "\nMedian: ", median(df$Yearly_avg_view_on_travel_page),
         "\nQ3: ", quantile(df$Yearly_avg_view_on_travel_page, 0.75)
       )) +
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 1,vjust = 250, margin = margin(t = 0, r = 10),size = 10),text = element_text(size = 15))
#Observation: In this plot we can see the avg views(highest and lowest) and its statistical measurements and it is positively skewed, and also leptokurtic curve. This plot shows us the distribution of data Yearly_avg_view_on_travel_page 
############################
#Maximum likes given by user
maximum_likes <- df %>%
  filter(total_likes_on_outstation_checkin_given == max(total_likes_on_outstation_checkin_given)) %>%
  select(UserID, total_likes_on_outstation_checkin_given)
print(maximum_likes)
#Minimum likes given by user
minimum_likes <- df %>%
  filter(total_likes_on_outstation_checkin_given == min(total_likes_on_outstation_checkin_given)) %>%
  select(UserID, total_likes_on_outstation_checkin_given)
print(minimum_likes)
######################################
data<- data.frame(df$total_likes_on_outstation_checkin_given)
# Set the binwidth based on your data
binwidth <- 10# You can adjust this based on your data distribution

# Create a ggplot with a frequency polygon
plot1<-ggplot(data, aes(x = df$total_likes_on_outstation_checkin_given)) +
  geom_freqpoly( color = "blue", size = 1) +
  labs(title = "Frequency Polygon of Total Likes Given by Users",
       x = "Total Likes Given by Users",
       y = "Density",
       caption = paste(
         "Highest Likes: 252430",
         "\nLowest Likes: 3570"
       )) +
  scale_x_continuous(limits = c(0, 26000), breaks = seq(0, 26000, by = 2000)) +# Adjust limits and breaks based on your data
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 1,vjust = 280, margin = margin(t = 0, r = 10),size = 10),text = element_text(size = 15))
plot1
####################
maximum_likes <- df %>%
  filter(total_likes_on_outofstation_checkin_received == max(total_likes_on_outofstation_checkin_received)) %>%
  select(UserID, total_likes_on_outofstation_checkin_received)
print(maximum_likes)

minimum_likes <- df %>%
  filter(total_likes_on_outofstation_checkin_received == min(total_likes_on_outofstation_checkin_received)) %>%
  select(UserID, total_likes_on_outofstation_checkin_received)
print(minimum_likes)

data<- data.frame(df$total_likes_on_outstation_checkin_given)
# Set the bin width based on your data
binwidth <- 10# You can adjust this based on your data distribution

# Create a ggplot with a frequency polygon
plot2<-ggplot(data, aes(x = df$total_likes_on_outofstation_checkin_received)) +
  geom_freqpoly( color = "blue", size = 1) +
  labs(title = "Frequency Polygon of Total Likes Received from Users",
       x = "Total Likes Received from Users",
       y = "Density",
       caption = paste(
         "Highest Likes: 20065",
         "\nLowest Likes: 1009"
       )) +
  scale_x_continuous(limits = c(0, 26000), breaks = seq(0, 26000, by = 2000)) +# Adjust limits and breaks based on your data
  theme_minimal()+
  theme(plot.caption = element_text(hjust = 1,vjust = 280, margin = margin(t = 0, r = 10),size = 10),text = element_text(size = 15))
plot2
#Merging of both the plots
#Use the same data frame for both plots
data <- data.frame(total_likes = c(df$total_likes_on_outstation_checkin_given, df$total_likes_on_outofstation_checkin_received),
                   type = rep(c("Likes Given", "Likes Received"), each = nrow(df)))

# Overlay the plots
merged_plot <- ggplot(data, aes(x = total_likes, color = type)) +
  geom_freqpoly(binwidth = 10, size = 1) +
  labs(title = "Merged Frequency Polygons",
       x = "Total Likes",
       y = "Density",
       caption = paste(
         "Highest Likes Given Count: 252430",
         "\nLowest Likes Given Count: 3570",
           "\nHighest Likes Received Count: 20065",
           "\nLowest Likes Received Count: 1009"
       )) +
  scale_x_continuous(limits = c(0, 26000), breaks = seq(0, 26000, by = 2000)) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 1, vjust = 280, margin = margin(t = 0, r = 10), size = 10),
        text = element_text(size = 15))

# Show the merged plot
print(merged_plot)
#######################################
table(df$yearly_avg_Outstation_checkins)
result <- df %>%
  group_by(yearly_avg_Outstation_checkins) %>%
  summarise(number_of_users = n_distinct(UserID)) %>%
  mutate(yearly_avg_Outstation_checkins = order(factor(yearly_avg_Outstation_checkins), -number_of_users))
# Print the result
print(result)
View(result)
result$yearly_avg_Outstation_checkins <- as.numeric(as.character(result$yearly_avg_Outstation_checkins))
############# Calculate summary statistics
skewness_val <- skewness(result$number_of_users)
kurtosis_val <- kurtosis(result$number_of_users)
mean_val <- mean(result$number_of_users)
median_val <- median(result$number_of_users)
# Assuming result is your dataframe
ggplot(result, aes(x = yearly_avg_Outstation_checkins, y = number_of_users)) +
  geom_histogram(stat = "identity", fill = "skyblue", color = "white") +
  geom_text(aes(label = number_of_users), vjust = -0.5, color = "white", size = 3) + 
  labs(title = "Number of Users in Each Check-in Category",
       x = "Check-in Category",
       y = "Number of Users") +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 2)) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "black"),
        axis.text.x = element_text(color = "white"),  # Change color of x-axis labels
        axis.text.y = element_text(color = "white"),
        plot.title = element_text(color = "white",hjust = 0.5))+
  annotate("text", x = 25, y = max(result$number_of_users), label = paste("Skewness: ", round(skewness_val, 2)), hjust = 0, vjust = 1, color = "white",size = 5) +
  annotate("text", x = 25, y = max(result$number_of_users) - 5, label = paste("Kurtosis: ", round(kurtosis_val, 2)), hjust = 0, vjust = 3, color = "white",size=5) +
  annotate("text", x = 25, y = max(result$number_of_users) - 10, label = paste("Mean: ", round(mean_val, 2)), hjust = 0, vjust = 5, color = "white",size=5) +
  annotate("text", x = 25, y = max(result$number_of_users) - 15, label = paste("Median: ", round(median_val, 2)), hjust = 0, vjust = 7, color = "white",size=5)
###########################
table(df$member_in_family)
result <- df %>%
  group_by(member_in_family) %>%
  summarise(number_of_users = n_distinct(UserID)) %>%
  mutate(member_in_family = reorder(factor(member_in_family), -number_of_users))
result
#Ploting a bar
result_bar_plot <- ggplot(result, aes(x = member_in_family, y = number_of_users, fill =member_in_family)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = number_of_users), 
            vjust = -0.5,  
            size = 3) + 
  labs(title = "Number of Users by Family Member",
       x = "Family Member",
       y = "Number of Users") +
  theme_minimal()
print(result_bar_plot)
#########################
result <- df %>%
  group_by(Adult_flag) %>%
  summarise(number_of_users = n_distinct(UserID)) %>%
  mutate(Adult_flag = reorder(factor(Adult_flag), -number_of_users))
result
#To print plot 
result_area_plot <- ggplot(result, aes(x = Adult_flag, y = number_of_users, fill = Adult_flag)) +
  geom_col(alpha = 0.7) +
  geom_text(aes(label = number_of_users), position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(title = "Number of Users by Adult Flag",
       x = "Adult Flag",
       y = "Number of Users") +
  theme_minimal()
print(result_area_plot)
##########################
table(df$Daily_Avg_mins_spend_on_traveling_page)
result <- df %>%
  group_by(Daily_Avg_mins_spend_on_traveling_page) %>%
  summarise(number_of_users = n_distinct(UserID)) %>%
  mutate(Daily_Avg_mins_spend_on_traveling_page = reorder(factor(Daily_Avg_mins_spend_on_traveling_page), -number_of_users))
result
############
ggplot(result, aes(x = Daily_Avg_mins_spend_on_traveling_page, y = number_of_users)) +
  geom_point(stat = "identity",color="gold") +
  labs(title = "Number of Users vs. Daily Avg Mins Spend on Traveling Page",
       x = "Daily Avg Mins Spend on Traveling Page",
       y = "Number of Users") +
  theme_minimal()+
scale_x_discrete(labels = function(x) gsub(" min", "", x))

