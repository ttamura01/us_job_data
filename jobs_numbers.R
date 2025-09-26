# Using ggplot2 to display the revised change in US Jobs Numbers for May and June (CC367)

setwd("/Users/takayukitamura/Documents/R_Computing/us_job_data")

library(tidyverse)
library(readxl)
library(scales)
library(sysfonts)
library(showtextdb)
library(showtext)

font_add_google("Libre Franklin", "franklin", bold.wt = 500)
showtext_opts(dpi = 300)
showtext_auto()

parse_month <- function(x) {
  month(x, label = TRUE, abbr = TRUE)
}

read_excel("SeriesReport-20250828095601_5042a7.xlsx",
           range = "A13:M24") %>% 
  pivot_longer(-Year, names_to = "month", values_to = "number_of_jobs") %>% 
  rename_all(tolower) %>% 
  mutate(new_jobs = number_of_jobs - lag(number_of_jobs),
  date = ymd(paste(year, month, 1, sep = "-")),
  type = if_else(date != "2025-07-01", "real", "july")) %>% 
  filter(date >= "2024-07-01" & date <= "2025-07-01") %>% 
  bind_rows(
    tibble(year = c(2025, 2025),
           month = c("May", "June"), 
           date = ymd("2025-05-01", "2025-06-01"),
           new_jobs = c(144, 147),
           type = "preliminary")
  ) %>% 
  select(date, new_jobs, type) %>% 
  mutate(type = factor(type, levels = c("preliminary", "real", "july")),
         date = as.character(date)) %>% 
  arrange(date, type) %>% 
  ggplot(aes(x = date, y = new_jobs, fill = type, color = type)) +
  geom_col(position = position_identity(), linewidth = 0.3, alpha = 0.9) +
  annotate(
    geom = "text",hjust = 0.01, 
    x = -0.4, y = c(108, 208, 308), 
    label = c("+100,000", "+200,000", "+300,000"), 
    family = "franklin", size = 8.5, size.unit = "pt", color = "gray40") +
  scale_fill_manual(
    breaks = c("preliminary", "real", "july"),
    values = c("#ffffff", "gray70", "#FE8918")
  ) +
  annotate(geom = "label",
           x = c(11.5, 8.5, 13.5),
           y = c(95, 200, 270),
           label = c("REVISED\nDOWN",
                     "Growth in May and\nJune was lower than\n initally estimated.",
                     "+73,000 jobs\nin July"),
           hjust = c(0.5, 0, 1),
           color = c("gray40", "black", "black"),
           fontface = c("bold.italic", "bold", "bold"),
           vjust = 0.5, alpha = 0.8, lineheight = 0.9, 
           label.size = 0, label.padding = unit(0, "pt"), family = "franklin",
           size = 9, size.unit = "pt"
  ) +
  annotate(
    geom = "segment",
    x = 13,
    y = 73,
    yend = 250,
    linewidth = 0.4) +
  annotate(
    geom = "curve",
    x = 10.9, xend = 11.5,
    y = 180, yend = 155,
    curvature =-0.5 ,
    linewidth = 0.4
  ) +
  scale_color_manual(
    breaks = c("preliminary", "real", "july"),
    values = c("#bdbdbd", "#bdbdbd", "#FE8918")
  ) +  
  scale_x_discrete(
    breaks = c("2024-07-01", "2024-09-01", "2024-11-01", "2025-01-01", "2025-03-01", "2025-05-01", "2025-07-01"),
    # labels = parse_month
    labels = c("July '24", "Sept.", "Nov.", 
               "Jan.'25", "March", "May", "July")
  ) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Monthly change in jobs",
       caption = "Source: Bureau of Labor Statidtics  •  Note: Data is seasonally adjusted. • by Takayuki Tamura") +
  theme(
    text = element_text(family = "franklin"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.4, linetype = "12"),
    axis.line.x = element_line(linewidth = .3),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(colour = "gray40", size = 10, face = "bold",
                              margin = margin(l = -15, b = 10)),
    plot.caption = element_text(color = "gray40", size = 9, hjust = 0, 
                                margin = margin(t = 10, l = -15)),
    plot.margin = margin(t = 8, r = 15, b = 8, l = 20)) 
        
ggsave("jobs_numbers.png", width = 6, height = 3.6)

