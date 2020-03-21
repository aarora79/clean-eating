library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggcorrplot)
library(heatmaply)
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
  geom_point() +
  geom_smooth(method = "lm") +
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





heatmaply(
  (mtcars),
  xlab = "Features",
  ylab = "Cars", 
  main = "Data transformation using 'normalize'"
)

df_wt_loss_by_day <- df_wt_loss %>%
  group_by(name, day) %>%
  summarize(median_wt_loss = median(loss_per_day, na.rm=T))
df_wt_loss_by_day$day <- factor(df_wt_loss_by_day$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

df_wt_loss_by_day %>%
  arrange(day) %>%
  ggplot(aes(x=day, y=median_wt_loss, fill=name)) +
  geom_bar(stat="identity", position="dodge")

df_change_amit <- df %>%
  gather(key, value, -name, -date) %>%
  filter(name=="Amit") %>%
  filter(date==min(date) | date==max(date)) %>%
  spread(date, value)
colnames(df_change_amit) <- c("name", "metric", "value_at_start", "value_at_end")
df_change_amit <- df_change_amit %>%
  mutate(abs_change = abs(value_at_end-value_at_start)) %>%
  select(-value_at_end, -value_at_start)
  

df_change_nidhi <- df %>%
  gather(key, value, -name, -date) %>%
  filter(name=="Nidhi") %>%
  filter(date==min(date) | date==max(date)) %>%
  spread(date, value)
colnames(df_change_nidhi) <- c("name", "metric", "value_at_start", "value_at_end")
df_change_nidhi
df_change_nidhi <- df_change_nidhi %>%
  mutate(abs_change = -abs(value_at_end-value_at_start)) %>%
  select(-value_at_end, -value_at_start)

df_change <- bind_rows(df_change_amit, df_change_nidhi) %>%
  arrange((abs_change))

df_change$metric <- factor(df_change$metric, levels = c("weight", "bmi", "total-lean-mass-percentage", "body-fat-percentage", "hydration-percentage"))
df_change <- df_change %>%
  arrange(desc(metric))
# X Axis Breaks and Labels 
brks <- seq(-15, 15, 1)
lbls = paste0(as.character(c(seq(15, 0, -1), seq(1, 15, 1))), "")

# Plot
ggplot(df_change , aes(x = metric, y = abs_change, fill = name)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Email Campaign Funnel") +
  theme_economist() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette

df_wt_loss_result <- df_wt_loss %>%
  drop_na() %>%
  mutate(wt_loss_result = ifelse(loss_per_day < 0,
                                 "lost", 
                                 ifelse(loss_per_day == 0, "same", "gained"))) %>%
  select(name, wt_loss_result)
df_wt_loss_result
waffle_data<- waffle_iron(df_wt_loss_result %>% filter(name=="Nidhi"), aes_d(group = wt_loss_result))

p1 <- ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_tableau() + 
  theme_waffle()+ 
  labs(title="Nidhi") + xlab("") + ylab("") +  
  theme(legend.position = "bottom",  legend.title = element_blank())

waffle_data <- waffle_iron(df_wt_loss_result %>% filter(name=="Amit"), aes_d(group = wt_loss_result))

p2 <- ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_tableau() + 
  theme_waffle() + 
  labs(title="Amit") + xlab("") + ylab("") +  
  theme(legend.position = "none")

patchwork <- p1 + p2

patchwork + plot_annotation(
  title = 'How many days did we lose/gain weight',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  #caption = 'Disclaimer: None of these plots are insightful'
)

library(ggplot2)
library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 + p2

library(ggwaffle)
iris$Species <- as.character(iris$Species)
waffle_data <- waffle_iron(iris, aes_d(group = Species))
ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() 
ggplot(waffle_data, aes(x, y, fill = group)) + 
  geom_waffle() + 
  coord_equal() + 
  scale_fill_waffle() + 
  theme_waffle() 
