#are we making any assumptions about DS weekends?

#1.Calling all the libraries
library(dplyr)
library(ggplot2)
library(statsr)

#2. example of calling a histogram
ggplot(data=diamonds, aes(x=price)) + geom_histogram(binwidth = 0.1)
#3. example of calling a box plot
ggplot(data=diamonds, aes(x="", y=price)) + geom_boxplot()
#4. example of calling a bar plot
ggplot(data=diamonds, aes(x=cut)) + geom_bar()
#5. importing the data
data(nycflights)
View(nycflights)

#6. names of the variables
names(nycflights)

#7. dimensions of the dataframes
str(nycflights)

#**********Question 1******************
#8. Plotting a histogram 
ggplot(data=nycflights, aes(x = dep_delay )) + geom_histogram(binwidth = 1)

#9. Do we see the extreme value visibly?
ggplot(data = nycflights, aes(x = dep_delay)) + geom_histogram(binwidth = 1) + ylim(c(0,50)) + xlim(c(-50,1500))

#10. Summarise mean of dep_delay
nycflights %>% summarise(mean_dd = mean(dep_delay))

#11. Summarise median of dep_delay
nycflights %>% summarise(median_dd = median(dep_delay))

#12. Visualization of boxplot with dep_delay
ggplot(data = nycflights, aes(x = "", y = dep_delay)) + geom_boxplot()

#13. To understand the distribution of the departure delays accross months we will visualize side by side box plots.
ggplot(nycflights, aes(x = factor(month), y = dep_delay)) + geom_boxplot()
ggplot(nycflights, aes(x = factor(month), y = dep_delay)) + geom_boxplot() + ylim(c(0,500))

#14. Lets summarize the mean of the departure delays grouped by month.
nycflights %>% group_by(month) %>% summarise(mean_dd = mean(dep_delay)) %>% arrange(desc(mean_dd))

#15. Lets summarize the median of the departure delays grouped by month.
nycflights %>% group_by(month) %>% summarise(median_dd = median(dep_delay)) %>% arrange(desc(median_dd))

#16. Lets create a new variable to add in our data frame to determine if the departed flight took off on-time or was delayed.
nycflights <- nycflights %>% mutate(dep_type = ifelse(dep_delay < 5, 'on-time', 'delay'))

#17. Before we said that highest means of departure delays lies in the July/June, we can use the on time departure rate to evaluate the proportions of the dep_type with months.
nycflights %>% group_by(month) %>% summarise(ot_dep_rate = sum(dep_type == 'on-time')/ n()) %>% arrange(desc(ot_dep_rate))

#**********Question 2******************
#Why do we need to caluate proptions?
#18. Lets calculate the on-time departure rate the flights grouped by origin that reperesents the three major airports in the New York City.
nycflights %>% group_by(origin) %>% summarise(ot_dep_rate = sum(dep_type == 'on-time')/ n()) %>% arrange(desc(ot_dep_rate))

#19. We can also visualize the distribution of the on-time departure rate accross the three airports using the segmented bar plot.
ggplot(nycflights, aes(x = origin, fill = dep_type)) + geom_bar()

#**********Question 3******************
#20. We will first see the distribution of the arrival delays.
ggplot(data = nycflights, aes(x = arr_delay)) + geom_histogram(binwidth = 50)

#21. Summarise mean of arr_delay
nycflights %>% summarise(mean_ad = mean(arr_delay))

#22. Summarise median of arr_delay
nycflights %>% summarise(median_ad = median(arr_delay))

#23. Create avg_speed
nycflights <- nycflights %>% mutate(avg_speed = distance/air_time)

#24. Scatter plot out of curiosity
ggplot(data = nycflights, aes(x = avg_speed, y = distance)) + geom_point()
ggplot()
#25.Now we compare thgpe speed with the arrival delay
breaks <- c(0, 3, 6, 9, 12)
bins_avg_speed <- cut(nycflights$avg_speed, breaks)
ggplot(data = nycflights, aes(x = bins_avg_speed, y = arr_delay)) + geom_boxplot()

#The same thing for distance
breaks <- c(0, 1000, 2000, 3000, 4000, 5000)
bins_distance <- cut(nycflights$distance, breaks)
ggplot(data = nycflights, aes(x = bins_distance, y = arr_delay)) + geom_boxplot()



