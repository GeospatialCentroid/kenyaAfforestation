# retrieve usage metrics for Kenya partners
library(tidyverse)

#note, had to install dev version of rsconnect for metrics to work
# install.packages("devtools")
#devtools::install_github("rstudio/rsconnect")
library(rsconnect)

# CPU usage in user space in nanosections
usage <- rsconnect::showMetrics(
  "docker_container_cpu",
  c("usage_in_usermode"),
  server = "shinyapps.io",
  appName = "kenyaAfforestation",
  account = "geocentroid",
  from = "1682920800",
  until = "1689141600",
  interval = "60s"
)
#sum per day, then convert to hours?
usage %>%
  as_tibble() %>% 
  separate(time, into = c("day", "time"), sep = " ") %>% 
  group_by(day) %>% 
  summarise(daily_usage = sum(usage_in_usermode)) %>% 
  
  
# test steps from this post?: https://stackoverflow.com/questions/48044097/understand-the-measurement-metrics-in-a-shiny-app-and-their-links

  
  
account_use <- rsconnect::accountUsage('geocentroid', from = '120d', interval = '30d')
  

# get hourly usage 
kenya_use <- showUsage(appName="kenyaAfforestation", 
                    account="geocentroid", usageType="hours", 
                    from="60d", 
                    interval="1h")

usage_hours <- kenya_use %>% 
  as_tibble() %>% 
  mutate(time = as_datetime(timestamp)) %>%
  #separate out day and time
  separate(time, into = c("date", "time"), sep = " ") %>% 
  #sum hours per day
  group_by(date) %>% 
  summarise(hours = sum(hours))

write_csv(usage_hours, "usage_stats/kadst_usage_051523_071423.csv")
  

usage_hours %>% 
  mutate(date = as.Date(date),
         date = format(date, "%m/%d")) %>%
  {
    dates <<- unique(.$date)
    .
  } %>% 
  ggplot(aes(x = date, y = hours))+
  geom_bar(stat = "identity") +
  scale_x_discrete(breaks = dates[seq(1, length(dates), 3)])+
  theme_minimal()+
  labs(y = "Total Hours")+
  theme(axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank(),
        plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, unit = "cm"))
