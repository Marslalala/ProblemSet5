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