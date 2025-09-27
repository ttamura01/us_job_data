# Using ggplot2 to display the revised change in US Jobs Numbers for June, July in September

setwd("/Users/takayukitamura/Documents/R_Computing/us_job_data")

library(tidyverse)
library(readxl)
library(scales)
library(sysfonts)
library(showtextdb)
library(showtext)
library(fredr)

font_add_google("Libre Franklin", "franklin", bold.wt = 500)
showtext_opts(dpi = 300)
showtext_auto()

parse_month <- function(x) {
  month(x, label = TRUE, abbr = TRUE)
}

#Set my FRED API key
fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244")

# Overall unemployment
nfp_data <- fredr(series_id = "PAYEMS") %>% 
  select(date, nfp = value) %>% 
  mutate(year = year(date),
         month = month(date, label = TRUE, abbr = TRUE),
         new_jobs = nfp - lag(nfp),
         type = if_else(date != "2025-08-01", "real", "august")) %>% 
  filter(date >= "2024-08-01") %>% 
  bind_rows(
    tibble(year = c(2025, 2025),
           month = c("June", "July"),
           date = ymd("2025-06-01", "2025-07-01"),
           new_jobs = c(14, 73),
           type = "preliminary")
  ) %>% 
  select(date, new_jobs, type) %>% 
  mutate(type = factor(type, levels = c("preliminary", "real", "august")),
         date = as.character(date)) %>% 
  arrange(date, type) 

nfp_data %>% 
  ggplot(aes(x = date, y = new_jobs, fill = type, colour = type)) +
  geom_col(position = position_identity(), linewidth = 0.3, alpha = 0.5) +
  annotate(
    geom = "text",hjust = 0.01, 
    x = -0.4, y = c(108, 208, 308), 
    label = c("+100,000", "+200,000", "+300,000"), 
    family = "franklin", size = 8.5, size.unit = "pt", color = "gray40") +
  scale_fill_manual(
    breaks = c("preliminary", "real", "august"),
    values = c("#ffffff", "gray70", "#FE8918")
  ) +
  annotate(geom = "label",
           x = c(12, 10, 13.5),
           y = c(190, 100, 270),
           label = c("Job Growth in July\n was slightly\n revised up to\n 79,000",
                     "Job Growth in June\n was revised lower to\n -13,000.",
                     "+22,000 jobs\nin August"),
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
    y = 22,
    yend = 260,
    linewidth = 0.4) +
  annotate(
    geom = "segment",
    x = 11, 
    y = 14, 
    yend = 90,
    linewidth = 0.4
  ) +
  annotate(
    geom = "segment",
    x = 12, 
    y = 79, 
    yend = 170,
    linewidth = 0.4
  ) +
  scale_color_manual(
    breaks = c("preliminary", "real", "august"),
    values = c("#bdbdbd", "#bdbdbd", "#FE8918")
  ) +  
  scale_x_discrete(
    breaks = c("2024-08-01", "2024-10-01", "2024-12-01", "2025-02-01", "2025-04-01", "2025-06-01", "2025-08-01"),
    # labels = parse_month
    labels = c("August '24", "Oct.", "Dec.", 
               "Feb.'25", "April", "June", "August")
  ) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Monthly change in Non-Farm Payroll",
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
    plot.title = element_text(colour = "black", size = 20, face = "bold",
                              margin = margin(l = -15, b = 10)),
    plot.caption = element_text(color = "gray40", size = 9, hjust = 0, 
                                margin = margin(t = 10, l = -15)),
    plot.margin = margin(t = 8, r = 15, b = 8, l = 20)) 

ggsave("jobs_numbers_aug.png", width = 6, height = 5.6)
