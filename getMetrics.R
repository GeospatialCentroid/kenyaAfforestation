# retrieve usage metrics for Kenya partners
library(tidyverse)

#note, had to install dev version of rsconnect for metrics to work
# install.packages("devtools")
devtools::install_github("rstudio/rsconnect")
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
  


kenya_use <- showUsage(appName="kenyaAfforestation", 
                    account="geocentroid", usageType="hours", 
                    from="90d", 
                    interval="1h")

kenya_use %>% 
  as_tibble() %>% 
  mutate(time = as_datetime(timestamp)) %>% 
  ggplot(aes(x = time, y = hours))+
  geom_bar(stat = "identity")
