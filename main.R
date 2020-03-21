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

theme_set(theme_bw())

df_wt_loss <- df %>%
  select(name, date, weight) %>%
  group_by(name) %>%
  arrange(date, .by_group=TRUE) %>%
  mutate(loss_per_day = weight-lag(weight, 1)) 

df_wt_loss %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(median_wt_loss_per_day=median(loss_per_day))
median_wt_loss_per_day_amit <- round(median(df_wt_loss[df_wt_loss$name == "Amit", ]$loss_per_day, na.rm=TRUE), 2)
median_wt_loss_per_day_nidhi <- round(median(df_wt_loss[df_wt_loss$name == "Nidhi", ]$loss_per_day, na.rm=TRUE), 2)


p <- df_wt_loss %>%
  drop_na() %>%
  ggplot(aes(x=name, y=loss_per_day)) + 
  geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               aes(fill=name)) +
  #theme(axis.text.x = element_text(angle=0, vjust=0.6)) + 
  labs(title="Distribution of per day weight loss (in pounds)", 
       subtitle=glue("Median weight loss per day, Amit={median_wt_loss_per_day_amit} pounds, Nidhi={median_wt_loss_per_day_nidhi} pounds"),
       caption="Source: Data collected during clean eating 30day challenge",
       x="",
       y="Weight loss per day in pounds (-ve represents loss)") + 
  theme(legend.position = "none")
p 

# is the curse of the weekend real?
df_wt_loss <- df_wt_loss %>%
  mutate(day = weekdays(as.Date(date))) %>%
  mutate(day_after_weekend = ifelse(day %in% c("Monday", "Tuesday"), TRUE, FALSE))
df_wt_loss

df_wt_loss %>%
  ggplot(aes(x=name, y=loss_per_day, col=day_after_weekend)) + 
  geom_boxplot()




