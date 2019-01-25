### -----------------------------------------------------------------------------------------
### Analysis of Super Bowl National Anthem performance times
###
### -----------------------------------------------------------------------------------------

library(tidyverse)
library(forecast)
library(lubridate)
library(eeptools)
require("theme538")

sb_times <- read_csv("data/superbowl_times.csv")

colnames(sb_times) <- c("year", "superbowl", "singer",
                        "start", "end", "length", "length_s",
                        "sex", "youtube", "birthdate")

### What is the mean time of the performance? ----------------------------------------------

avg_anthem_time <- sb_times %>%
   mutate(length_s = as.numeric(length_s)) %>%
   na.omit() %>%
   summarize(avg_length = mean(length_s))

### Are mens and women's performances different? -------------------------------------------

avg_anthem_time_sex <- sb_times %>%
   mutate(length_s = as.numeric(length_s)) %>%
   na.omit() %>%
   filter()
   group_by(sex) %>%
   summarize(avg_length = mean(length_s))

men <- avg_anthem_time_sex %>%
   filter(sex == "M")

women <- avg_anthem_time_sex %>%
   filter(sex == "F")

### Let's plot the distributions of both mens and womens anthems

sb_times_hist <- sb_times %>%
   select(length_s, sex) %>%
   filter(sex != "Ensemble") %>%
   na.omit()

ggplot(sb_times_hist, aes(length_s, fill = sex)) +
   geom_density(alpha = .5, binwidth = 10) +
   geom_vline(xintercept = men$avg_length, color = "black", linetype = "dashed") +
   geom_vline(xintercept = women$avg_length, color = "black", linetype = "dashed") +
   theme_538 +
   theme(legend.position="none") +
   labs(y = "Density", x = "Anthem Length (seconds)",
        title = "Women perform longer than men TKTK",
        subtitle = "Distribution of female performance length in pink, male in blue.",
        caption = "SOURCE: Youtube")

### magic incantation to save the plot to disk ---------------------------------------------

dev.copy(png,'sex_density_plot.png')
dev.off()

### Does age matter? -----------------------------------------------------------------------

calc_age <- function(birthDate, refDate = Sys.Date()) {

   require(lubridate)

   period <- as.period(interval(birthDate, refDate),
                       unit = "year")
   period$year

}

calc_age("1990-06-30", "2003-07-12")

sb_ages <- sb_times %>%
   mutate(sb_date = paste0(as.character(year),"-02-01"),
          age_at_sb = calc_age(birthdate, sb_date)) %>%
   select(age_at_sb, length_s, sex) %>%
   na.omit() %>%
   filter(sex != "Ensemble")

age_model <- lm(data = sb_ages, length_s ~ age_at_sb)
summary(age_model)

sex_model <- lm(data = sb_ages, length_s ~ sex)
summary(sex_model)

### Create a time series object to perform ARIMA on ----------------------------------------

sb_timeseries <- sb_times %>%
   arrange(year) %>%
   select(year, length_s) %>%
   na.omit()

sb_ts <- ts(sb_timeseries[, 2], start = c(1979, 1), frequency = 1)

### Chart # 1 -------------------------------------------------------------------------------

sb_ts %>%
   auto.arima() %>%
   forecast(h = 40,
            level = c(50, 75, 95),
            robust = TRUE) %>%
   autoplot() +
   geom_hline(yintercept = 105, color = "gray", linetype = "dashed") +
   scale_x_continuous(
      limit = c(1979, 2021),
      breaks = c(seq(
         from = 1979,
         to = 2021,
         by = 5))) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   theme_538 +
   theme(legend.position = "none") +
   labs(y = "Length of performance (in seconds)", x = "Year",
        title = "Anthems have gotten longer",
        subtitle = "2019 Forecasted length with 50, 75 and 95% confidence levels in blue.\n
        Dotted line is 40 year avg.",
        caption = "SOURCE: Youtube")

### magic incantation to save the plot to disk ----------------------------------------------

dev.copy(png,'anthem_length_over_time.png')
dev.off()
