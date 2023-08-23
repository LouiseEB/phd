#meta_risk

library(tidyverse)

source("R/Functions.R")

data <- tibble(
  title = c("Major cardiovascular event", rep(NA, 3),
            "Hospitalization for heart failure", rep(NA, 3)),
  endpoint = c(rep("mace", 4), rep("hf", 4)),
  study = c(NA, "DPP-4 inhibitors", "GLP-1 receptor agonists", "SGLT2 inhibitors",
            NA, "DPP-4 inhibitors", "GLP-1 receptor agonists", "SGLT2 inhibitors"
           ),
  n_total_treat = rep(c(NA, 21807, 27977, 21266
  ), 2),
  n_event_treat = c(NA, 2097, 2954, 2048,
                    NA, 832, 918, 550),
  n_total_plac = rep(c(NA, 21715, 28027, 17457), 2),
  n_event_plac = c(NA, 2091, 3311, 1780,
                   NA, 772, 998, 642),
  hr = c(NA, 1.00, 0.88, 0.88,
         NA, 1.07, 0.91, 0.68),
  ci_l = c(NA, 0.93, 0.82, 0.82,
           NA, 0.91, 0.84, 0.60),
  ci_u = c(NA, 1.06, 0.94, 0.94,
           NA, 1.27, 1.00, 0.76)) %>%
  mutate(print_est = case_when(!is.na(hr) ~ paste0(
    format(round(hr, 2), nsmall = 2),
    " (",
    format(round(ci_l, 2), nsmall = 2),
    "-",
    format(round(ci_u, 2), nsmall = 2),
    ")"
  )),
  mutate(across(
    starts_with("n_total"), ~ case_when(!is.na(.x) ~ format(.x, big.mark = ","))
  )),
  row = 9- row_number())

ggplot()+
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 7.5))+
  geom_segment(aes(x = 0.45, xend = 1.3, y = 0, yend = 0))+
  coord_cartesian(ylim = c(0,10.5), expand = FALSE)+
  geom_text(data = data, aes(x = -0.7 , y = row, label = title), hjust = 0, fontface = "bold") +
  geom_text(data = data, aes(x = -0.65, y = row, label = study), hjust = 0)+
  geom_text(data = data, aes(x = 0, y = row, label = n_total_treat), hjust = 1)+
  geom_text(data = data, aes(x = 0.1, y = row, label = n_event_treat), hjust = 1)+
  geom_text(data = data, aes(x = 0.3, y = row, label = n_total_plac), hjust = 1)+
  geom_text(data = data, aes(x = 0.4, y = row, label = n_event_plac), hjust = 1)+
  # geom_text(data = data, aes(x = 1.3, y = row, label = weight), hjust = 1)+
  geom_text(data = data, aes(x = 1.67, y = row, label = print_est), hjust = 1)+
  annotate("text", x= 0.05, y =10, label = "N, treatment", fontface = "bold", hjust = 0.7)+
  annotate("text", x= 0.35, y =10, label = "N, placebo", fontface = "bold", hjust = 0.7)+
  annotate("text", x = 0, y = 9, label = "Total", fontface = "bold", hjust = 1.2)+
  annotate("text", x = 0.1, y = 9, label = "Events", fontface = "bold", hjust = 0.85)+
  annotate("text", x = 0.3, y = 9, label = "Total", fontface = "bold", hjust = 1.2)+
  annotate("text", x = 0.4, y = 9, label = "Events", fontface = "bold", hjust = 0.85)+
  # annotate("text", x = 1.3, y = 22, label = "Weight (%)", fontface = "bold", hjust = 0.8)+
  annotate("text", x = 1.67, y = 9, label = "HR (95% CI)", fontface = "bold", hjust = 1.1)+
  # geom_point(data = filter(data, study != "Fixed effects model"), aes(x = hr, y =row))+
  # geom_errorbar(data = filter(data, study != "Fixed effects model"), aes(xmin = ci_l, xmax = ci_u, y =row), width = .3)+
  geom_polygon(data = rMaster2(data, "mace", stu = "DPP-4 inhibitors"),
               mapping = aes(x = x, y = y),
               color = "gray70",
               fill = "gray70",
               inherit.aes = FALSE
  )+
  geom_polygon(data = rMaster2(data, "mace", stu = "GLP-1 receptor agonists"),
               mapping = aes(x = x, y = y),
               color = "royalblue4",
               fill = "royalblue4",
               inherit.aes = FALSE
  )+
  geom_polygon(data = rMaster2(data, "mace", stu = "SGLT2 inhibitors"),
               mapping = aes(x = x, y = y),
               color = "#E69F00",
               fill = "#E69F00",
               inherit.aes = FALSE
  )+
  geom_polygon(data = rMaster2(data, "hf", stu = "DPP-4 inhibitors"),
               mapping = aes(x = x, y = y),
               color = "gray70",
               fill = "gray70",
               inherit.aes = FALSE
  )+
  geom_polygon(data = rMaster2(data, "hf", stu = "GLP-1 receptor agonists"),
               mapping = aes(x = x, y = y),
               color = "royalblue4",
               fill = "royalblue4",
               inherit.aes = FALSE
  )+
  geom_polygon(data = rMaster2(data, "hf",  stu = "SGLT2 inhibitors"),
               mapping = aes(x = x, y = y),
               color = "#E69F00",
               fill = "#E69F00",
               inherit.aes = FALSE
  )+

  xlab("Hazard ratio (95% confidence interval)")+
  scale_x_continuous(breaks= c(0.6, 0.8, 1, 1.2))+
  theme_classic()+
  theme(
    axis.title.x = element_text(hjust =0.73),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank()
  )



ggsave(filename = "meta_gluc_lowering.png", width = 9.8, height = 4)
