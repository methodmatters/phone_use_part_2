# load the libraries we'll need
library(ggplot2)
library(RColorBrewer)
library(plyr); library(dplyr)
library(readr)
options(scipen=999)

# directory where the data file is located
in_dir = 'D:\\Directory\\'
# read the data
full_data <- read_csv(paste0(in_dir, 'phone_use.csv'))

# [1] 548 unique days
length(unique(full_data$oneday))
# first date
full_data$oneday[1]
# last date
full_data$oneday[length(full_data$oneday)]


# what do the data look like?
full_data[90:100,]


### Formatting issues
### Formatting issues
### Formatting issues


# there is a problem with the units for the mobile data
# will need to convert / harmonize the data
# original formating says that it's in kb, but some months are in mbs

# make a year/month column
# we will use this to determine whether we 
# should convert the data 
full_data$year_month = format(as.Date(full_data$oneday), "%Y-%m")

# first, we specify the months for which the data are in 
# megabytes
months_in_mb = c("2018-01", "2018-02", "2018-03", 
                 "2018-04", "2018-05", "2018-06", 
                 "2018-07", "2018-08", "2018-09", 
                 "2018-10", "2018-11", "2018-12", 
                 "2019-01", "2019-02", "2019-03")

# create a function with logic built in:
# if data are already in mb, use that number
# if data are in kb, divide by 1000 
# to convert to mb
divide_it <- function(year_month_col_f, data_col_f){
  if (year_month_col_f %in% months_in_mb) {
    data_use_mb = data_col_f
  } else {
    data_use_mb = data_col_f/1000
  }
  return(data_use_mb)
} 

# apply the function to our dataset to create
# the harmonized data usage variable
# all values are now in mb
full_data$data_mb <- mapply(divide_it, 
                            year_month_col_f = full_data$year_month, 
                            data_col_f = full_data$data_usage_kb)

### Make the plots
### Make the plots
### Make the plots


# Hexadecimal color specification
# for the color of the plot
color_palette <- brewer.pal(n = 3, name = "Dark2")

# set up the x-axis labels
x_axis_labels = seq(from = 0, to = 23, by = 1)


### Plot Mean Use By Hour
### Plot Mean Use By Hour
### Plot Mean Use By Hour

# pass the full data to a dplyr chain
# we will format the data and then plot it
full_data %>% 
  # select just the variables we will need
  select(oneday, hour, data_mb) %>%
  # create the variable that represents the 
  # number of days in the dataset (n_days)
  mutate(n_days = length(unique(oneday))) %>% 
  # group by hour and calculate the mean data use per hour of day
  # we do this by calculating the sum for each hour, and 
  # dividing it by the n_days variable created above
  group_by(hour) %>% 
  # because n_days is a constant, we can use the max() to
  # grab the correct number of observed days for the division
  summarize(sum_data_mb = sum(data_mb, na.rm = TRUE),
            mean_data_mb = sum_data_mb / max(n_days)) %>% 
  # pass the data ggplot
  ggplot(aes(x = hour, y = mean_data_mb))  + 
  # ask for a bar plot, using the same color for mobile data
  # as in the previous post
  geom_bar(stat = 'identity', fill = color_palette[1])  +
  # set up the axis labels and plot title
  labs(x = "Hour of Day", y = "Mean Mobile Data Usage (mb)", 
       title = 'Mean Mobile Data Usage Across the Day' ) +
  # set up the x-axis - one tick per hour of the day
  scale_x_continuous(labels = x_axis_labels, 
                     breaks = x_axis_labels) 


### Plot Mean Use By Hour: Separate Plots for Weekdays / Weekends
### Plot Mean Use By Hour: Separate Plots for Weekdays / Weekends
### Plot Mean Use By Hour: Separate Plots for Weekdays / Weekends

# separate out week/weekend
full_data %>% 
  # select only the columns we'll need
  select(week_weekend, hour, data_mb) %>% 
  # group by week_weekend and hour because we need to
  # calculate the mean data usage per hour on the 
  # weekdays and on the weekends separately
  group_by(week_weekend, hour) %>% 
  # as above, we calculate the sum data used.
  # we also calculate the minimum number of obsersations
  # for each hour of the day 
  # as noted above, some days have more than 1 line, 
  # e.g. when I sent a text message and used mobile data
  # for a given hour on a given day
  summarize(sum_data_mb = sum(data_mb, na.rm = TRUE),
            n = n()) %>%  
  # we mutate here to get the minimum number of days
  # observed for each weekday / weekend
  # we take the minimum, because some days have more
  # than one line per hour as noted above
  mutate(min_n = min(n),
         mean_data_mb = sum_data_mb/min_n) %>%
  # pass the data ggplot
  ggplot(aes(x = hour, y = mean_data_mb))  + 
  # ask for a bar plot, using the same color for mobile data
  # as in the previous post
  geom_bar(stat = 'identity', fill = color_palette[1])  +
  # set up the axis labels and plot title
  labs(x = "Hour of Day", y = "Mean Mobile Data Usage (mb)", 
       title = 'Mean Mobile Data Usage Across the Day' ) +
  # set up the x-axis - one tick per hour of the day
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  # facet by week_weekend to get separate panels
  # for weekdays and weekends
  facet_grid(week_weekend ~ .) 


### Plot Total Use by Month
### Plot Total Use by Month
### Plot Total Use by Month

# plot total monthly mobile data usage across time
# select just the columns we're interested in
full_data %>% select(year_month, data_mb) %>% 
  # group by year/month
  # (we created this variable above)
  group_by(year_month) %>% 
  # calculate the sum of all the data used for each year/month
  summarize(sum_data_mb = sum(data_mb, na.rm = TRUE)) %>% 
  # remove September 2019 because we don't have the entire month
  # in our dataset
  filter(year_month != "2019-09" )  %>%    
  # pass the data ggplot
  ggplot(., aes(x = year_month, y = sum_data_mb, group = 1)) + 
  # ask for a bar plot, using the same color for mobile data
  # as in the previous post
  geom_bar(stat = 'identity', fill = color_palette[1])  +
  # set up the axis labels and plot title
  labs(x = "Year-Month", y = "Total Mobile Data Use (mb)", 
       title = 'Mobile Data Usage (mb) Across Time' ) + 
  # rotate the x axis labels 90 degrees so they're horizontal
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=-1)) +
  # add the value labels above the bars
  geom_text(aes(label = round(sum_data_mb)), vjust = -0.5) + 
  # set the y axis limits to extend all the way to zero
  # at the bottom
  coord_cartesian(ylim = c(0, 1650)) 
