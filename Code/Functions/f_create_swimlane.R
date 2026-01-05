# Cryptosporidiosis swimlane plots

# Function for creating swimlane plots

# Format swimlane plot ---------------------------------------------------------
f_swimlane_format <- function(data, start_date, end_date, hyperchl_date, escalation_date) {
  
  figure <- data %>% 
    ggplot() +
    #
    geom_segment(aes(x    = symptom_onset - 12, 
                     xend = symptom_onset, 
                     y    = forcats::fct_reorder(phess_id, symptom_onset), 
                     yend = forcats::fct_reorder(phess_id, symptom_onset),
                     col  = "Acquisition period"),
                 linewidth = 5) +
    #
    geom_segment(aes(x    = symptom_onset, 
                     xend = symptom_end + 14, 
                     y    = forcats::fct_reorder(phess_id, symptom_onset), 
                     yend = forcats::fct_reorder(phess_id, symptom_onset),
                     col  = "Infectious period"),
                 linewidth = 5) +
    #
    scale_color_manual(name   = NULL,
                       labels = c("Acquisition period", "Infectious period"),
                       values = c("lightblue", "#ffa78c")) +
    #
    ggnewscale::new_scale_color() +
    #
    geom_point(aes(x     = interview_date,
                   y     = phess_id,
                   col   = "Interview date",
                   shape = "Interview date"),
               size   = 2.5,
               stroke = 2) +
    #
    geom_point(aes(x     = date_attended,
                   y     = phess_id,
                   col   = "Dates attended",
                   shape = "Dates attended"),
               size   = 2,
               stroke = 2) +
    #
    scale_color_manual(name   = NULL,
                       values = c("Interview date" = "black", 
                                  "Dates attended" = "purple"),
                       guide  = guide_legend(reverse = TRUE)) +
    #
    scale_shape_manual(name   = NULL,
                       values = c("Interview date" = 4, 
                                  "Dates attended" = 16),
                       guide  = guide_legend(reverse = TRUE)) +
    #
    ggnewscale::new_scale_color() +
    #
    geom_vline(aes(xintercept = hyperchl_date,
                   colour     = "Most recent Hyper-Cl",
                   linetype   = "Most recent Hyper-Cl"),
               linewidth  = 1) +
    #
    geom_vline(aes(xintercept = escalation_date,
                   colour     = "Most recent escalation",
                   linetype   = "Most recent escalation"),
               linewidth  = 1) +
    #
    scale_color_manual(name   = NULL,
                       values = c("Most recent Hyper-Cl"   = "#5bc788", 
                                  "Most recent escalation" = "cornflowerblue")) +
    #
    scale_linetype_manual(name   = NULL,
                          values = c("Most recent Hyper-Cl"   = "dashed",
                                     "Most recent escalation" = "dashed")) +
    #
    scale_x_date(date_breaks = "3 day",
                 date_labels = "%d-%b",
                 limits = c(start_date, end_date)) +
    #
    labs(title = NULL,
         x     = NULL,
         y     = NULL) +
    #
    theme_classic() +
    #
    theme(axis.text   = element_text(size = 9),
          axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          #
          plot.margin = margin(0, 0, 0, 0),
          #
          legend.position = "bottom",
          legend.location = "plot",
          legend.box      = "horizontal",
          legend.margin   = margin(),
          legend.text     = element_text(size = 9),
          #
          plot.caption = element_text(hjust = 0))
  
  return(figure)
  
}
