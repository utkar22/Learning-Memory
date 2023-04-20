library(ggplot2)
library(readxl)
library(plotly)
library(gridExtra)

#Taking the average of data across all trials per day for each time point to get one averaged signal
#per day

df_day1 <- read.csv("C://Users//Utkarsh//Documents//homework//Learning and Memory//Assignment 2//day1.csv", header=FALSE)
df_day2 <- read.csv("C://Users//Utkarsh//Documents//homework//Learning and Memory//Assignment 2//day2.csv", header=FALSE)
df_day3 <- read.csv("C://Users//Utkarsh//Documents//homework//Learning and Memory//Assignment 2//day3.csv", header=FALSE)
df_day4 <- read.csv("C://Users//Utkarsh//Documents//homework//Learning and Memory//Assignment 2//day4.csv", header=FALSE)

avg_signal_day1 <- c()
avg_signal_day2 <- c()
avg_signal_day3 <- c()
avg_signal_day4 <- c()


for (i in df_day1){
  curr = mean(as.numeric(i))
  if (!is.na(curr)){
    avg_signal_day1 = append(avg_signal_day1, curr)
  }
}

for (i in df_day2){
  curr = mean(as.numeric(i))
  if (!is.na(curr)){
    avg_signal_day2 = append(avg_signal_day2, curr)
  }
}

for (i in df_day3){
  curr = mean(as.numeric(i))
  if (!is.na(curr)){
    avg_signal_day3 = append(avg_signal_day3, curr)
  }
}

for (i in df_day4){
  curr = mean(as.numeric(i))
  if (!is.na(curr)){
    avg_signal_day4 = append(avg_signal_day4, curr)
  }
}

#Calculating the rolling average

filter_day1 <- c()
filter_day2 <- c()
filter_day3 <- c()
filter_day4 <- c()

curr_full_1 = 0
curr_full_2 = 0
curr_full_3 = 0
curr_full_4 = 0

t <- c()


for (i in 1:1000){
  curr_day1 = avg_signal_day1[i]
  curr_day2 = avg_signal_day2[i]
  curr_day3 = avg_signal_day3[i]
  curr_day4 = avg_signal_day4[i]
  
  curr_full_1 = curr_full_1 + curr_day1
  curr_full_2 = curr_full_2 + curr_day2
  curr_full_3 = curr_full_3 + curr_day3
  curr_full_4 = curr_full_4 + curr_day4
  
  if (i<21){
    x = i
  }
  else{
    x = 20
    curr_full_1 = curr_full_1 - avg_signal_day1[i-20]
    curr_full_2 = curr_full_2 - avg_signal_day2[i-20]
    curr_full_3 = curr_full_3 - avg_signal_day3[i-20]
    curr_full_4 = curr_full_4 - avg_signal_day4[i-20]
  }
  
  filter_day1 = append(filter_day1, curr_full_1/x)
  filter_day2 = append(filter_day2, curr_full_2/x)
  filter_day3 = append(filter_day3, curr_full_3/x)
  filter_day4 = append(filter_day4, curr_full_4/x)
  t = append(t, i)
}

#Performing full wave rectification

for (i in 1:1000){
  filter_day1 = abs(filter_day1)
  filter_day2 = abs(filter_day2)
  filter_day3 = abs(filter_day3)
  filter_day4 = abs(filter_day4)
}


# Create four separate plots
p1 <- plot_ly() %>% 
  add_lines(x = ~t, y = ~avg_signal_day1, line = list(color = "blue"), name = "Average Signal") %>% 
  add_lines(x = ~t, y = ~filter_day1, line = list(color = "red"), name = "Filtered Signal") %>% 
  layout(title = "Average Signal and Filtered Signal (Day 1)", 
         xaxis = list(title = "Time (in ms)"), 
         yaxis = list(title = "Signal Strength (in Hz)"), 
         showlegend = TRUE,
         legend = list(title = "Signal Type", 
                       x = 0.8, y = 1)) %>% 
  config(displayModeBar = FALSE)

p2 <- plot_ly() %>% 
  add_lines(x = ~t, y = ~avg_signal_day2, line = list(color = "blue"), name = "Average Signal") %>% 
  add_lines(x = ~t, y = ~filter_day2, line = list(color = "red"), name = "Filtered Signal") %>% 
  layout(title = "Average Signal and Filtered Signal (Day 2)", 
         xaxis = list(title = "Time (in ms)"), 
         yaxis = list(title = "Signal Strength (in Hz)"), 
         showlegend = TRUE,
         legend = list(title = "Signal Type", 
                       x = 0.8, y = 1)) %>% 
  config(displayModeBar = FALSE)

p3 <- plot_ly() %>% 
  add_lines(x = ~t, y = ~avg_signal_day3, line = list(color = "blue"), name = "Average Signal") %>% 
  add_lines(x = ~t, y = ~filter_day3, line = list(color = "red"), name = "Filtered Signal") %>% 
  layout(title = "Average Signal and Filtered Signal (Day 3)", 
         xaxis = list(title = "Time (in ms)"), 
         yaxis = list(title = "Signal Strength (in Hz)"), 
         showlegend = TRUE,
         legend = list(title = "Signal Type", 
                       x = 0.8, y = 1)) %>% 
  config(displayModeBar = FALSE)

p4 <- plot_ly() %>% 
  add_lines(x = ~t, y = ~avg_signal_day4, line = list(color = "blue"), name = "Average Signal") %>% 
  add_lines(x = ~t, y = ~filter_day4, line = list(color = "red"), name = "Filtered Signal") %>% 
  layout(title = "Average Signal and Filtered Signal (Day 4)", 
         xaxis = list(title = "Time (in ms)"), 
         yaxis = list(title = "Signal Strength (in Hz)"), 
         showlegend = TRUE,
         legend = list(title = "Signal Type", 
                       x = 0.8, y = 1)) %>% 
  config(displayModeBar = FALSE)

# Combine the plots using the gridExtra package
grid.arrange(p1, p2, p3, p4, ncol = 2)