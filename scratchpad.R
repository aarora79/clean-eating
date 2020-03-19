library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# global constants
RAW_DATA_DIR <- "./raw_data/"
RAW_DATA_FILE_NAME <- "data.csv"

# read the data
df <- read_csv(file.path(RAW_DATA_DIR, RAW_DATA_FILE_NAME))

# what does it look like?
summary(df)

df_tidy <- df %>%
  gather(metric, value, -date, -name)

p <- df_tidy %>%
  ggplot(aes(x=date, y=value, col=metric)) +
  geom_line() +
  facet_wrap(~metric, scales="free_y") + 
  #facet_grid(metric~name, scales="free_y") +
  theme_bw() +
  theme(legend.position = "bottom")
p


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
