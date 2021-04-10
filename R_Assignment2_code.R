## Yuexia Tian
## u976100

## Load packages ---------------------------------------------------------------
library(tidyverse)
library(caret)
install.packages("dplyr")
library(dplyr)

## Loading data sets -----------------------------------------------------------
tornadoes <- read.delim("input/tornadoes.txt", stringsAsFactors = FALSE)
states <- read.delim("input/states.txt", stringsAsFactors = FALSE)
state_codes <- read.delim("input/state_codes.txt", stringsAsFactors = FALSE)

## Question 1 ------------------------------------------------------------------
tornadoes1 <- tornadoes %>%
  filter(injuries == 0, fatalities == 0, crop_loss != 0) %>%
  arrange(yr, desc(crop_loss))

answer1 <- tornadoes1

## Question 2 ------------------------------------------------------------------
maine_population <- ggplot(data = states, aes(x = Year, y = Maine * 1000)) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(limits = c(600000, 1500000), name = "Population of Maine",
                     breaks = c(600000, 900000, 1200000, 1500000),
                     labels = c("0.6 million", "0.9 million", "1.2 million",
                                "1.5 million"))

answer2 <- maine_population

## Question 3 ------------------------------------------------------------------
tornadoes3 <- tornadoes 
plotting <- ggplot(data = tornadoes3, 
                   aes(x = yr, fill = as.factor(magnitude))) +
  geom_bar() + 
  scale_fill_manual(values = c("-9" = "#D8D5D9", "0" = "#EFF3FF",
                                "1" = "#C6DBEF", "2" = "#9ECAE1", 
                                "3" = "#6BAED6", "4" = "#3182BD",
                                "5" = "#08519C")) +
  guides(fill = guide_legend(title = "magnitude")) +
  theme_dark()

answer3 <- plotting

## Question 4 ------------------------------------------------------------------
states4 <- states
real_numbers <- function(a_vector) {
  a_vector <- signif(a_vector * 1000, 3)
  a_vector
}
year_rn <- mutate_at(states4, vars(Alabama : Wyoming),
                     list(real_numbers))
year_rn$Year <- as.factor(substr(as.character(year_rn$Year), 
                                 start = 3, stop = 4))

answer4 <- year_rn

## Question 5 ------------------------------------------------------------------
tornadoes5 <- tornadoes %>%
  group_by(yr) %>%
  summarise(fatalities = sum(fatalities), 
            injuries = sum(injuries)) %>%
  filter(yr == 1984) %>%
  pivot_longer(!yr, names_to = "casualties", values_to = "count") %>%
  ggplot(aes(x = casualties, y = count)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "statistic") +
  scale_y_continuous(name = "total_cost") 

answer5 <- tornadoes5  

## Question 6 ------------------------------------------------------------------
set.seed(1)
true_values <-sample(c("Yes", "No"), 10000, replace = TRUE,
                     prob = c(0.3, 0.7))
predicted_values <- true_values
predicted_values[sample.int(10000, 4000)] <-
  sample(c("Yes", "No"), 4000, replace = TRUE, prob = c(0.3, 0.7))

confM <- caret::confusionMatrix(data = as.factor(predicted_values), 
                                reference = as.factor(true_values),
                                positive = "Yes")
precision <- confM$byClass["Pos Pred Value"]

answer6 <- precision

## Question 7 ------------------------------------------------------------------
set.seed(1)
tornadoes7 <- tornadoes
tornadoes7$fatalities[tornadoes7$fatalities == 0] <- "No"
tornadoes7$fatalities[tornadoes7$fatalities != "No"] <- "Yes"
tornadoes7$fatalities <- as.factor(tornadoes7$fatalities)

sample_random <- sample_n(tornadoes7, 1000)
trctrl <- trainControl(method = "repeatedcv", repeats = 3)
fatalities_knn1 <- train(fatalities ~ magnitude + time,
                        data = sample_random, method = "knn",
                        trControl = trctrl,
                        tuneGrid = data.frame(k = seq(5, 15, by = 2))
)

yes_500 <- tornadoes7 %>%
  filter(fatalities == "Yes") %>%
  slice(1:500)
no_500 <- tornadoes7 %>%
  filter(fatalities == "No") %>%
  slice(1:500)
sample_balanced <- bind_rows(yes_500, no_500)
trctrl <- trainControl(method = "repeatedcv", repeats = 3)
fatalities_knn2 <- train(fatalities ~ magnitude + time,
                         data = sample_balanced, method = "knn",
                         trControl = trctrl,
                         tuneGrid = data.frame(k = seq(5, 15, by = 2))
)
result_random <- fatalities_knn1$results[ , 1:2]
result_balanced <- fatalities_knn2$results[ , 1:2]
result_random[2] <- result_random[2] - result_balanced[2]

answer7 <- result_random

## Question 8 ------------------------------------------------------------------
plot_tornadoes <- function(dataframe, a_state, a_color = "darkgreen") {
  dataframe1 <- filter(dataframe, state == a_state)
  ggplot(dataframe1, aes(x = magnitude, y = injuries)) + 
    geom_point(shape = 23, fill = a_color, color = "black", size = 3,
             position = "jitter") +
    ggtitle(paste("Tornadoes in", 
                  state_codes$Name[which(state_codes$Abbreviation == a_state)])
            ) +
    theme(plot.title = element_text(hjust = 0.5, color = a_color)) 
}

answer8 <- plot_tornadoes

## Question 9 ------------------------------------------------------------------
state_code9 <- state_codes %>%
  slice(-2, -11) %>%
  add_row(Name = "District of Columbia", Abbreviation = "DC")
states9 <- states %>%
  relocate(District.Of.Columbia, .after = last_col()) %>%
  rename(yr = Year)

colnames(states9)[2:50] <- state_code9[, 2]
states_long <- pivot_longer(states9, 
                        cols = (AL : DC), 
                        names_to = "state", 
                        values_to = "population")
tornadoes9 <- tornadoes
left_join(tornadoes9, states_long, by = c("yr", "state"))

answer9 <- tornadoes9
  
## Question 10 -----------------------------------------------------------------
tornadoes10 <- tornadoes
missing_data <- function(a_column) {
  a_column[a_column == 0] = -9
  a_column
} 
tornadoes10$crop_loss <- missing_data(tornadoes10$crop_loss)
tornadoes10$property_loss <- missing_data(tornadoes10$property_loss)
unique(is.na(tornadoes10$property_loss))

categorise_losses <- function(a_vector) {
  a_vector <- a_vector * 1000000
  a_vector = case_when(
    a_vector >= 0 & a_vector < 50 ~ 1,
    a_vector >= 50 & a_vector < 500 ~ 2,
    a_vector >= 500 & a_vector < 5000 ~ 3,
    a_vector >= 5000 & a_vector < 50000 ~ 4,
    a_vector >= 50000 & a_vector < 500000 ~ 5,
    a_vector >= 500000 & a_vector < 5000000 ~ 6,
    a_vector >= 5000000 & a_vector < 50000000 ~ 7,
    a_vector >= 50000000 & a_vector < 500000000 ~8)
  a_vector
          }
tornadoes10[49643:61598, 10] <- 
  categorise_losses(tornadoes10[49643:61598, 10])
tornadoes10[35677:61598, 11] <- 
  categorise_losses(tornadoes10[35677:61598, 11])
tornadoes10$crop_loss[is.na(tornadoes10$crop_loss)] = -9
tornadoes10$property_loss[is.na(tornadoes10$property_loss)] = -9

answer10 <- tornadoes10


