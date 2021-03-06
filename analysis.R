### -----------------------------------------------------------------------------------------------
### Analysis of Super Bowl National Anthem performance times
###
### -----------------------------------------------------------------------------------------------

library(tidyverse)
library(forecast)
library(lubridate)
library(eeptools)
require("theme538")
library("Cairo")
library(beeswarm)

sb_times <- read_csv("data/superbowl_times.csv")

colnames(sb_times) <- c("year", "superbowl", "singer",
                        "start", "end", "length", "length_s",
                        "sex", "youtube", "birthdate")

covers <- read_csv("data/covers.csv")

colnames(covers) <- c("song", "gladys_start", "gladys_finish",
                        "gladys_length", "original_start", "original_finish", "original_length",
                        "difference", "percent_diff", "original_artist", "link")

### What is the mean time of the performance? -----------------------------------------------------

avg_anthem_time <- sb_times %>%
   mutate(length_s = as.numeric(length_s)) %>%
   filter(year != 1988) %>%
   na.omit() %>%
   summarize(avg_length = mean(length_s))

### Are mens and women's performances different? --------------------------------------------------

avg_anthem_time_sex <- sb_times %>%
   mutate(length_s = as.numeric(length_s)) %>%
   na.omit() %>%
   filter(sex != "Ensemble") %>%
   group_by(sex) %>%
   summarize(avg_length = mean(length_s))

men <- avg_anthem_time_sex %>%
   filter(sex == "M")

women <- avg_anthem_time_sex %>%
   filter(sex == "F")

### Let's make a sex model!

sex_model <- lm(data = sb_ages, length_s ~ sex)
summary(sex_model)

### Let's plot the distributions of both mens and womens anthems

sb_times_hist <- sb_times %>%
   select(length_s, sex) %>%
   filter(sex != "Ensemble") %>%
   na.omit()

beeswarm(sb_times_hist$length_s, pch = 16, cex=2.5, pwcol = as.factor(sb_times_hist$sex), horizontal=TRUE)

ggplot(sb_times_hist, aes(length_s, color = sex)) +
   geom_freqpoly(alpha = 1, binwidth = 1.8, bins = 40) +
   geom_vline(xintercept = men$avg_length, color = "#18B3B7", linetype = "dashed") +
   geom_vline(xintercept = women$avg_length, color = "#F16E6A", linetype = "dashed") +
   theme_538 +
   theme(legend.position="none") +
   labs(y = "Count", x = "Anthem Length (seconds)",
        title = "Women perform longer than men TKTK",
        subtitle = "Distribution of female performance length in pink, male in blue. Dashed lines are averages.",
        caption = "SOURCE: Youtube")

### magic incantation to save the plot to disk ----------------------------------------------------

ggsave("sex_density_plot.png")

### Does age matter? ------------------------------------------------------------------------------

calc_age <- function(birthDate, refDate = Sys.Date()) {

   require(lubridate)

   period <- as.period(interval(birthDate, refDate),
                       unit = "year")
   period$year

}

sb_ages <- sb_times %>%
   mutate(sb_date = paste0(as.character(year),"-02-01"),
          age_at_sb = calc_age(birthdate, sb_date)) %>%
   select(age_at_sb, length_s, sex) %>%
   na.omit() %>%
   filter(sex != "Ensemble")

age_model <- lm(data = sb_ages, length_s ~ age_at_sb)
summary(age_model)

### Create a time series object to perform ARIMA on -----------------------------------------------

sb_timeseries <- sb_times %>%
   arrange(year) %>%
   select(year, length_s) %>%
   na.omit()

sb_ts <- ts(sb_timeseries[, 2], start = c(1979, 1), frequency = 1)

### Chart # 1 -------------------------------------------------------------------------------------

forecast <- sb_ts %>%
   auto.arima() %>%
   forecast(h = 40,
            level = c(50, 75, 95))

t <- sb_ts %>%
   auto.arima() %>%
   forecast(h = 40,
            level = c(50, 75, 95)) %>%
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
        title = "Anthems performances have gotten longer over time.",
        subtitle = "2019 Forecasted length with 50, 75 and 95% confidence levels in blue.\nDotted line is 40 year avg.",
        caption = "SOURCE: Youtube")

t

ggsave(t, filename = "arimia.pdf", device = cairo_pdf)

### save the plot to disk -------------------------------------------------------------------------

ggsave("anthem_length_over_time1.png")

### Does Gladys tend to sing longer than the original artist when she covers a song? --------------

covers_hist <- covers %>%
   mutate(difference = as.numeric(difference),
          song_and_artist = paste0(song,"\n", "(",original_artist,")"))

mean_cover_diff <- mean(covers_hist$difference)

### bar chart -------------------------------------------------------------------------------------

ggplot(covers_hist, aes(x = reorder(song_and_artist, -difference), y = difference)) +
   geom_bar(stat = "identity", position = position_dodge()) +
   theme_minimal() +
   ggtitle(paste("Gladys Knight's covers are", round(mean_cover_diff, 1), "seconds longer than the original on average")) +
   labs(x = "Cover", y = "Difference in length (seconds)",
        caption = "Source: Youtube") +
   theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
   guides(fill = guide_legend(title = ""))

ggsave("covers.png")
