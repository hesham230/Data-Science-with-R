# This data contains all 336,776 flights that departed from New York City in 2013
library(nycflights13)
library(dplyr)

flights = flights

##############################  Filter ########################################

# filter() allows you to subset observations based on their values
jan1 = filter(flights, month == 1, day == 1)

NovOrDec = filter(flights, month == 11 | month == 12)

nov_dec = filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))

filter(flights, arr_delay <= 120, dep_delay <= 120)


# Exercises: Find all flights that
# Had an arrival delay of two or more hours
temp = filter(NYCflights, arr_delay >= 2)
temp$arr_delay
# Flew to Houston (IAH or HOU)
temp = filter(NYCflights, dest == "IAH" | dest == "HOU")
temp$dest
# Departed in summer (July, August, and September)
temp = filter(NYCflights,month == 7 | month == 8 | month ==9)

##############################  Arrange ########################################

# arrange() works similarly to filter() except that instead of selecting
# rows, it changes their order

# Order by year, month and day
temp = arrange(flights, year, month, day)

# Descending order of arr_delay
arrange(flights, desc(arr_delay))

##############################  Select ########################################
#Its not uncommon to get datasets with hundreds or even thousands
#of variables. In this case, the first challenge is often narrowing in on
#the variables youre actually interested in.

# # Select columns by name
temp = select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)

# Select all columns except those from year to day (exclusive)
select(flights, -(year:day))

# Helper Functions (starts_with, ends_with, contains, matches:
select(flights, starts_with("arr_"))
temp2 = select(flights, ends_with("delay"))

# Rename - a variant of select (doesnt drop other vars)
rename(NYCflights,tail_num = tailnum)


############################## MUTATE ######################################
# mutate() adds new columns at the end of your dataset
# transmute creates a tibble with the new variables only

flights_sml = select(flights, year:day, ends_with("delay"), distance, air_time)

mutate(flights_sml, gain = arr_delay - dep_delay, speed = distance / air_time * 60)
transmute(flights_sml, gain = arr_delay - dep_delay, speed = distance / air_time * 60)

############################# RANKING #####################################
x = c(2,3,3,4,5,6)
min_rank(x)
min_rank(desc(x))


############################ Summarize ######################################
# The last key verb is summarize(). It collapses a data frame to a single row
summarize(flights, delay = sum(dep_delay, na.rm = TRUE))

# summarize() is not terribly useful unless we pair it with group_by().
# This changes the unit of analysis from the complete dataset to individual
# groups. Then, when you use the dplyr verbs on a grouped
# data frame they will be automatically applied by group.

by_month = group_by(flights, year, month)
summarize(by_month, delay = mean(dep_delay, na.rm = TRUE))

# Exwrcise:
# We want to explore the relationship between the distance
# and average delay for each location

by_dest = group_by(flights, dest)
delay = summarize(by_dest, count = n(), dist = mean(distance, na.rm = TRUE),
                                        delay = mean(arr_delay, na.rm = TRUE))

plot(delay ~ dist, data = delay)
delay = filter(delay, count > 20, dest != "HNL") #remove noisy points and honolulu airport which is far

# This code is a little frustrating to write because we have to give each
# intermediate data frame a name, even though we dont care about it.
# Naming things is hard, so this slows down our analysis.
# Theres another way to tackle the same problem with the pipe

delays = flights %>% group_by(dest) %>% 
summarize(count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE)) %>%
filter(count > 20, dest != "HNL")


delays = not_cancelled %>% group_by(tailnum) %>%
         summarize(delay = mean(arr_delay, na.rm = TRUE), n = n())