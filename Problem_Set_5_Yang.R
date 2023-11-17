#######################################################################
#######################################################################
######                                                           ######
######                     Problem Set #05                       ######
######                        Yang Han                           ######
######                                                           ######
#######################################################################
#######################################################################



###Question 1


## Part a

#  Load the data and ggplot2 library.
nnmaps <- read.csv("chicago-nnmaps.csv")
library(ggplot2)

#  Compute the mean monthly temperature in Celsius and get the result data.
library(dplyr)
mean_temp <- nnmaps %>%
  group_by(month) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE)) %>%
  mutate(mean_temp = (mean_temp - 32) * 5 / 9) %>%
  ungroup()

result <- nnmaps %>%
  left_join(mean_temp, by = "month") %>%
  distinct(month, season, mean_temp)

#  Adjust the order of the x-axis and the legends(which is the variable 'season').
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
result$month <- factor(result$month, levels = month_order)
result$season <- factor(result$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

#  Produce the graph.
ggplot(result, aes(x = month, y = mean_temp, color = season, group = season)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Mean Monthly Temperature by Season",
       x = "Month",
       y = "Mean Monthly Temperature (¡ãC)",
       color = "Season") +
  theme_bw()


## Part b

#  Compute the mean monthly O3, the mean monthly PM10, and the mean monthly dewpoint.
mean_o3 <- nnmaps %>%
  group_by(month) %>%
  summarize(mean_o3 = mean(o3, na.rm = TRUE)) %>%
  ungroup()
mean_pm10 <- nnmaps %>%
  group_by(month) %>%
  summarize(mean_pm10 = mean(pm10, na.rm = TRUE)) %>%
  ungroup()
mean_dewpoint <- nnmaps %>%
  group_by(month) %>%
  summarize(mean_dewpoint = mean(dewpoint, na.rm = TRUE)) %>%
  ungroup()

#  Join the data to get ready for plotting.
result <- result %>%
  left_join(mean_o3, by = "month") %>%
  distinct(month, season, mean_temp, mean_o3)
result <- result %>%
  left_join(mean_pm10, by = "month") %>%
  distinct(month, season, mean_temp, mean_o3, mean_pm10)
result <- result %>%
  left_join(mean_dewpoint, by = "month") %>%
  distinct(month, season, mean_temp, mean_o3, mean_pm10, mean_dewpoint)

#  Adjust the order of the x-axis.
result$month <- factor(result$month, levels = month_order)

#  Produce the graph.
ggplot(result, aes(x = month, group = season)) +
  #  Use different types of line to distinguish between the four lines
  geom_line(aes(y = mean_temp, color = "Mean Temperature (¡ãC)"), size = 1) +
  geom_line(aes(y = mean_o3, color = "Mean O3"), size = 1, linetype = "dashed") + 
  geom_line(aes(y = mean_pm10, color = "Mean PM10"), size = 1, linetype = "dotted") +
  geom_line(aes(y = mean_dewpoint, color = "Mean Dewpoint"), size = 1, linetype = "dotdash") +
  #  Add points for mean values
  geom_point(aes(y = mean_temp), color = "black", size = 3) +  
  geom_point(aes(y = mean_o3), color = "red", size = 3) +
  geom_point(aes(y = mean_pm10), color = "blue", size = 3) +
  geom_point(aes(y = mean_dewpoint), color = "green", size = 3) +
  labs(title = "Mean Monthly Variables by Season",
       x = "Month",
       y = "Mean Monthly Values",
       color = "Variable",
       linetype = "Variable") +
  scale_color_manual(values = c("Mean Temperature (¡ãC)" = "black", "Mean O3" = "red",
                                "Mean PM10" = "blue", "Mean Dewpoint" = "green")) +
  theme_bw() +
  theme(legend.position = "top")

#  From the plot we can see that the 'blue line' fluctuates the least with the change in month. We know that
#  each piece of a line segment represents a season. Hence we know that PM10 seems to have the least seasonal
#  trend.



###Question 2


#  Create a class to represent a polynomial expression.
setClass("poly", slots = c(coefficients = "numeric", 
                           exponents = "numeric"))

## Part a

#  Define the constructor function for "poly" class.
poly_c <- function(coefficients, exponents) {
  return(new("poly", coefficients = coefficients, exponents = exponents))
}

#  Define the validator for "poly" class.
setValidity("poly", function(object){
  #  Check the validity of the coefficients and exponents
  if (!is.numeric(object@coefficients) | !is.numeric(object@exponents)) {
    stop("This is not a valid polynomial")
  }
  
  if (length(object@coefficients) != length(object@exponents)) {
    stop("The length of coefficients must equal to the length of exponents")
  }
  return(TRUE)
})

#  Define the show method.
##' @title Display a `polynomial` object
##' @param object A `polynomial` object
setMethod("show", signature = "poly", function(object) {
  terms <- sapply(1:length(object@coefficients), function(i) {
    if (object@coefficients[i] == 0) {
      return(NULL)
    }
    sign <- ifelse(object@coefficients[i] < 0, "- ", ifelse(i == 1, "", "+ "))
    paste0(sign, abs(object@coefficients[i]), "x^", object@exponents[i])
  })
  
  poly_string <- paste(na.omit(terms), collapse = " ")
  cat(poly_string, "\n")
})


#  Define the addition method.
setMethod("+", signature = c("poly", "poly"), function(e1, e2) {
    #  Ensure that the polynomials are valid
    validObject(e1)
    validObject(e2)

    #  Determine the maximum exponent in either polynomial
    max_exponent <- max(max(e1@exponents), max(e2@exponents))

    #  Create vectors to store the result coefficients
    result_coefficients <- numeric(max_exponent + 1)

    #  Add the coefficients for each term with the same exponent
    result_coefficients[e1@exponents + 1] <- result_coefficients[e1@exponents + 1] + e1@coefficients
    result_coefficients[e2@exponents + 1] <- result_coefficients[e2@exponents + 1] + e2@coefficients

    #  Create a new polynomial
    result_poly <- new("poly", coefficients = rev(result_coefficients), 
                       exponents = rev(seq_along(result_coefficients)) - 1)
    return(result_poly)
  }
)

#  Define the subtraction method.
setMethod("-", signature = c("poly", "poly"), function(e1, e2) {
  #  Ensure that the polynomials are valid
  validObject(e1)
  validObject(e2)
            
  #  Determine the maximum exponent in either polynomial
  max_exponent <- max(max(e1@exponents), max(e2@exponents))
            
  #  Create vectors to store the result coefficients
  result_coefficients <- numeric(max_exponent + 1)
            
  #  Subtract the coefficients for each term with the same exponent
  result_coefficients[e1@exponents + 1] <- result_coefficients[e1@exponents + 1] + e1@coefficients
  result_coefficients[e2@exponents + 1] <- result_coefficients[e2@exponents + 1] - e2@coefficients
            
  #  Create a new polynomial
  result_poly <- new("poly", coefficients = rev(result_coefficients), 
                      exponents = rev(seq_along(result_coefficients)) - 1)
  return(result_poly)
})


## Part b

#  Run the tweaked version of the code.
p1 <- poly_c(coefficients = c(3, 2), exponents = c(2, 0))
p2 <- poly_c(coefficients = c(7, -2, -1, 17), exponents = c(3, 2, 1, 0))
p1
p2
p1 + p2
p1 - p2



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

























