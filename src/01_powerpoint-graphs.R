library(tidyverse)
library(readxl)
library(lubridate)
library(broom)
library(themebg)
library(officer)
library(rvg)

data_glucose <- read_excel("data/raw/glucose_values.xlsx", skip = 1, col_names = c("patient", "period", "datetime", "value")) %>%
    mutate_at("period", factor, levels = c("Pre", "Drip", "Post")) %>%
    group_by(patient) %>%
    mutate(duration = difftime(datetime, first(datetime), units = "hours")) %>%
    mutate_at("duration", as.numeric)

p_scatter <- ggplot(data_glucose, aes(x = duration, y = value)) +
    geom_point(aes(color = period), shape = 1) +
    geom_smooth(method = "loess", color = "black") +
    scale_color_brewer("", palette = "Set1") +
    xlab("Time from admission (hours)") +
    ylab("Blood glucose (mg/dL)") +
    theme_bg() +
    theme(axis.line = element_line(color = "grey50"))

p_box <- ggplot(data_glucose, aes(x = period, y = value)) +
    geom_boxplot() +
    xlab(NULL) +
    ylab("Blood glucose (mg/dL)") +
    theme_bg() +
    theme(axis.line = element_line(color = "grey50"))

read_pptx() %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_vg(code = print(p_scatter), type = "body") %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_vg(code = print(p_box), type = "body") %>%
    print(target = "figs/plots.pptx") %>%
    invisible()

# layout_summary(pptx)
# layout_properties(pptx, layout = "Title and Content", master = "Office Theme")
