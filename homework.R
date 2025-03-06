# 259 Homework - exploratory data analysis + integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

# set the directory where the data are stored
data_dir <- 'us-weather-history/'

# define function for importing data
read_weather <- function(station) {
  
  # initialize an empty tibble to store the data
  station_data <- tibble()
  
  # read the data into the tibble
  station_data <- read_csv(paste0(data_dir,station,'.csv'))
  
  # add a column for the station name and move it to the front of the tibble
  station_data <- station_data %>%
    mutate(station_name = station) %>%
    relocate(station_name, .before = date)
  
  # convert date column to date
  station_data <- station_data %>%
    mutate(date = as.Date(date))
  
  # return the data file in tibble format
  return(station_data)
}

# call read_weather to get one station's weather
single_station_data <- read_weather(stations[1])

# look at single station's file
glimpse(single_station_data)


# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, you will need to do so in the code.
#> Save the resulting dataset to "ds"

ds <- bind_rows(map(stations, read_weather))


# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

# create the factor called city and move it to the front of the data frame
ds <- ds %>%
  mutate(city = factor(station_name, levels = stations, labels = cities)) %>%
  relocate(city, .before = station_name)

# count rows per city
fct_count(ds$city)


# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

# function to convert from F to C
f2c = function(temp) {
  
  # convert from F to C
  converted_temp <- (temp - 32) * (5/9)
  
  return(converted_temp)
  
}

# convert temperature columns to numeric, then convert to C, then round to 1 decimal place
ds <- ds %>%
  mutate(across(actual_mean_temp:record_max_temp, as.numeric)) %>%
  mutate(across(actual_mean_temp:record_max_temp, f2c)) %>%
  mutate(across(actual_mean_temp:record_max_temp, ~round(.,1)))


### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

# write file to compare with complied_data.csv
write.csv(ds, file = 'compiled_data_2.csv')

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!

# define function to check whether a recorded temp is equal to current record at that time, rounded to 1 decimal place in C
check_record_temp <- function(min_temp, max_temp, record_min, record_max){
  
  # set record as true if either the min or max temp is the record temp
  record <- (min_temp == record_min) | (max_temp == record_max)
  
}

# check and note whether any given observation was a record observation
ds <- ds %>%
  mutate(record = check_record_temp(
    actual_min_temp,
    actual_max_temp,
    record_min_temp,
    record_max_temp))

# count the number of extreme days per city and sort in descending order
record_count <- ds %>%
  group_by(city) %>%
  summarize(record_count = sum(record)) %>%
  arrange(desc(record_count))



# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

# Get month from date as an ordered factor and place it after date
ds <- ds %>%
  mutate(month = month(date, label = T, abbr = F)) %>%
  relocate(month, .after = date)

# Make a list of tibbles by month
ds_months <- ds %>%
  group_split(month)

# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before

# define function for calculating and printing correlations
correlate <- function(df) {
  
  df %>%
    summarize(
      precip_correlation = cor(actual_precipitation, average_precipitation, use = "complete.obs"),
      min_temp_correlation   = cor(actual_mean_temp, average_min_temp, use = "complete.obs"),
      max_temp_correlation   = cor(actual_mean_temp, average_max_temp, use = "complete.obs")
    )
  
}


# loop through each month in ds_months and print month name and correlations
for (i in c(1:12)) {
  
  # print the month by accessing the first row of month in the ith tibble
  print(as.character(ds_months[[i]]$month[1]))
  
  # then print the correlations by calling the correlation function
  print(correlate(ds_months[[i]]))
  
}


# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this

# generate box plots for all numeric variables by city, then month
plot_boxplot(ds, by = 'city')
plot_boxplot(ds, by = 'month')

# plot correlations
plot_correlation(ds, type = 'continuous')


# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month




# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month


