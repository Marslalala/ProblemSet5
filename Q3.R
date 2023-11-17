###Question 3


## Part a

#  Load necessary packages first.
library(data.table)
library(nycflights13)
#  Get the data.table and clean the data as we only need departure and arrival delays, destination
#  and departing airport in this case.
flight_dt <- as.data.table(flights)
flight_times <- flight_dt[, .(dep_delay, arr_delay, origin, dest)]

#  Compute the mean and the median.
flight_dep_delay <- flight_times[, .(mean_delay_time = mean(dep_delay, na.rm = TRUE), 
                                     median_delay_time = median(dep_delay, na.rm = TRUE)), by = origin]


#  Change the name of the departing airports and order the table in descending mean delay.
airport_dt <- as.data.table(airports)
airport_names <- airport_dt[, .(faa, name)]
flight_dep_delay <- merge(flight_dep_delay, airport_names, by.x = "origin", by.y = "faa", all.x = TRUE)
flight_dep_delay <- flight_dep_delay[, .(departing_airport = name, mean_delay_time, 
                                         median_delay_time)][order(-mean_delay_time)]
#  Display the first table
print(flight_dep_delay)

#  To generate the second table, we need to exclude any destination with under 10 flights first.
flight_counts <- flight_times[, .N, by = dest][N >= 10]$dest
flight_times <- flight_times[dest %in% flight_counts]

#  Compute the mean and the median.
flight_arr_delay <- flight_times[, .(mean_delay_time = mean(arr_delay, na.rm = TRUE), 
                                     median_delay_time = median(arr_delay, na.rm = TRUE)), by = dest]


#  Change the name of the departing airports and order the table in descending mean delay.
flight_arr_delay <- merge(flight_arr_delay, airport_names, by.x = "dest", by.y = "faa", all.x = TRUE)
flight_arr_delay <- flight_arr_delay[, .(arriving_destination = name, mean_delay_time, 
                                         median_delay_time)][order(-mean_delay_time)]
flight_arr_delay <- na.omit(flight_arr_delay)
print(flight_arr_delay, n = 98)


## Part b

#  Firstly, join two data.tables flights and planes and get the variables of interests.
planes_dt <-as.data.table(planes)
flight_planes <- merge(flight_dt, planes_dt, by.x = "tailnum", by.y = "tailnum", all.x = TRUE)
flight_planes <- flight_planes[, speeds := distance / (air_time / 60)][, .(model, speeds)]

#  Then get the result table by reordering the data and calculate the number of flights of the fastest model.
flight_planes <- flight_planes[, .(avg_speed = mean(speeds, na.rm = TRUE), number_of_flights = .N),
                               by = model][order(-avg_speed)]
flight_planes <- na.omit(flight_planes)
#  Display the result.
result <- flight_planes[1,]
print(result)