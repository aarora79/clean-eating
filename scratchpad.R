library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
# global constants
RAW_DATA_DIR <- "./raw_data/"
RAW_DATA_FILE_NAME <- "data.csv"

# read the data
df <- read_csv(file.path(RAW_DATA_DIR, RAW_DATA_FILE_NAME))

df <- df %>%
  select(-`muscle-percentage`, -`lean-mass`) %>%
  drop_na() %>%
  arrange(date)
# what does it look like?
summary(df)
str(df)
df_tidy <- df %>%
  gather(metric, value, -date, -name)
head(df_tidy)
p <- df_tidy %>%
  ggplot(aes(x=date, y=value, col=name)) +
  geom_line() +
  facet_wrap(~metric, scales="free_y") + 
  #facet_grid(metric~name, scales="free_y") +
  theme_bw() +
  theme(legend.position = "bottom")
p
rownames(df) <- df$name 
corr <- round(cor(df %>% select(-date, -name)), 1)



# Correlation matrix
#data(mtcars)
#corr <- round(cor(mtcars), 1)
#class(mtcars)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of clean-eating", 
           ggtheme=theme_bw)

library(glue)
df_lag <- df %>%
  group_by(name) %>%
  arrange(date, .by_group=TRUE) %>%
  select(name, date, weight) %>%
  mutate(loss = weight - lag(weight, 1)) %>%
  select(-weight) %>%
  drop_na()

df_lag$day <- weekdays(as.Date(df_lag$date))
df_lag$week <- lubridate::week(df_lag$date)
df_lag <- df_lag %>%
  mutate(week = week - min(df_lag$week) + 1)
min_week <- min(df_lag$week)
max_week <- max(df_lag$week)
df_lag <- df_lag %>%
  #filter(week != min_week & week != max_week) %>%
  mutate(week = glue("week_{week}"))

df3 <- df_lag %>%
  select(name, day, week, loss)
df3$day2 <- factor(df3$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
df3 <- df3 %>%
  arrange(day2)
View(df3)
df3 %>%
  filter(name=="Amit") %>%
  ggplot(aes(x=day2,y=loss, group=week, col=week)) +  #, col=as.factor(week))) +
  geom_line() 

#+ 
#  facet_wrap(~name)

ggseasonplot(as.ts(df3)) + labs(title="Seasonal plot: International Airline Passengers")

library(ggplot2)
library(forecast)
theme_set(theme_classic())

# Subset data
nottem_small <- window(nottem, start=c(1920, 1), end=c(1925, 12))  # subset a smaller timewindow
class(AirPassengers)
# Plot
ggseasonplot(AirPassengers) + labs(title="Seasonal plot: International Airline Passengers")

library(hrbrthemes)
library(GGally)
library(viridis)

df_radar <- df %>%
  filter(date==min(date) | date==max(date)) %>%
  mutate(group = ifelse(date==min(date),
                        glue("{name} at the start of clean eating period"),
                        glue("{name} at the end of clean eating period"))) %>%
  select(-name, -date) %>%
  select(everything(), group)

colnames(df_radar)
# Plot
class(df_radar)
ggparcoord(df_radar,
           columns = 1:5, groupColumn = 6, order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )

class(iris)
head(iris)

library(hrbrthemes)
library(GGally)
library(viridis)

# Data set is provided by R natively
data <- iris

# Plot
ggparcoord(df_radar %>% group_by(group),
           columns = c("weight", "bmi"), groupColumn = "group", order = "anyClass",
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  )


library(ggradar)
library(dplyr)
library(scales)
library(tibble)
library(glue)
mtcars_radar <- mtcars %>% 
  as_tibble(rownames = "group") %>% 
  mutate_at(vars(-group), rescale) %>% 
  tail(4) %>% 
  select(1:10)
mtcars_radar
ggradar(mtcars_radar)

df
 #%>%
  #mutate_at(vars(-group), rescale)
View(df_radar)
ggradar(df_radar, grid.max = 300, grid.min = 0)  


library(plotly)

fig <- plot_ly(type = 'parcoords', line = list(color = 'blue'),
               dimensions = list(
                 list(range = c(1,5),
                      constraintrange = c(1,2),
                      label = 'A', values = c(1,4)),
                 list(range = c(1,5),
                      tickvals = c(1.5,3,4.5),
                      label = 'B', values = c(3,1.5)),
                 list(range = c(1,5),
                      tickvals = c(1,2,4,5),
                      label = 'C', values = c(2,4),
                      ticktext = c('text 1', 'text 2', 'text 3', 'text 4')),
                 list(range = c(1,5),
                      label = 'D', values = c(4,2))
               )
)

fig
