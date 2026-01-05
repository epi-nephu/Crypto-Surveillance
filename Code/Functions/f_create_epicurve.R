# Cryptosporidiosis epi curves

# Function for creating epi curves based on symptom onset date

# Format VIC/LPHU level epi curve ----------------------------------------------
f_format_epicurve_lphu <- function(data, date_breaks, y_max = NA) {
  
  figure <- ggplot(data) +
    geom_histogram(aes(x = event_date),
                   color    = "black",
                   fill     = "darkturquoise",
                   alpha    = 0.75,
                   binwidth = 7) +
    #
    scale_x_date(expand      = expansion(add = c(3, 3)),
                 date_breaks = date_breaks,
                 date_labels = "%d-%b") +
    #
    scale_y_continuous(limits = c(0, y_max),
                       breaks = scales::breaks_pretty(),
                       expand = expansion(mult = c(0, 0.1))) +
    #
    labs(title = NULL,
         x     = "Notification date",
         y     = "Number of cases") +
    #
    theme_classic() +
    #
    theme(axis.title  = element_text(size = 9),
          axis.text   = element_text(size = 8),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          #
          plot.margin = margin(0, 0, 0, 0),
          #
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)))
  
  return(figure)
  
}

# Format facility level epi curve ----------------------------------------------
f_format_epicurve_facility <- function(data, start_date, end_date, hyperchl_date, escalation_date) {
  
  figure <- data %>% 
    ggplot() +
    #
    geom_histogram(aes(x = symptom_onset),
                   color    = "black",
                   fill     = "#5bc788",
                   alpha    = 0.75,
                   binwidth = 1) +
    #
    geom_vline(aes(xintercept = hyperchl_date,
                   col        = "Most recent hyperchlorination",
                   linetype   = "Most recent hyperchlorination"),
               linewidth   = 1,
               show.legend = TRUE) +
    #
    geom_vline(aes(xintercept = escalation_date,
                   col        = "Most recent escalation",
                   linetype   = "Most recent escalation"),
               linewidth   = 1,
               show.legend = TRUE) +
    #
    scale_color_manual(name   = NULL,
                       values = c("Most recent hyperchlorination" = "#5bc788", 
                                  "Most recent escalation"        = "cornflowerblue")) +
    #
    scale_linetype_manual(name   = NULL,
                          values = c("Most recent hyperchlorination" = "dashed",
                                     "Most recent escalation"        = "dashed")) +
    #
    scale_x_date(limits      = c(start_date, end_date),
                 date_breaks = "3 day",
                 date_labels = "%d-%b")
  
  ggplot_build(figure)$data[[1]]
  
  figure <- figure +
    scale_y_continuous(limits = c(0, dplyr::case_when(max(ggplot_build(figure)$data[[1]]$count) < 5 ~ 5.25,
                                                      TRUE ~ max(ggplot_build(figure)$data[[1]]$count) * 1.1)),
                       expand = expansion(add = c(0, 0))) +
    #
    labs(title   = " ",
         x       = "Symptom onset date",
         y       = "Number of cases") +
    #
    theme_classic() +
    theme(axis.title  = element_text(size = 9),
          axis.text   = element_text(size = 8),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          #
          plot.margin = margin(0, 0, 0, 0),
          #
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          #
          legend.position = "bottom")
  
  return(figure)
  
}
