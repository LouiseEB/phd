#meta_risk

library(tidyverse)

source("R/Functions.R")

data <- tibble(
  title = c("Major cardiovascular event", rep(NA, 6),
            "Cardiovascular death", rep(NA, 6),
            "Hospitalization for heart failure", rep(NA, 6)),
  endpoint = c(rep("mace", 7), rep("cvd", 7), rep("hf", 7)),
  study = c(NA, "EMPA-REG OUTCOME", "CANVAS", "DECLARE-TIMI 58", "CREDENCE", "VERTIS-CV", "Fixed effects model",
            NA, "EMPA-REG OUTCOME", "CANVAS", "DECLARE-TIMI 58", "CREDENCE", "VERTIS-CV", "Fixed effects model",
            NA, "EMPA-REG OUTCOME", "CANVAS", "DECLARE-TIMI 58", "CREDENCE", "VERTIS-CV", "Fixed effects model"),
  n_total_treat = rep(c(NA, 4687, 5795, 8582, 2202, 5499, NA
  ), 3),
  n_event_treat = c(NA, 490, NA, 756, 217, 735, NA,
                    NA, 172, NA, 245, 110, 341, NA,
                    NA, 126, NA, 212, 89, 139, NA),
  n_total_plac = rep(c(NA, 2333, 4347, 8578, 2199, 2747, NA), 3),
  n_event_plac = c(NA, 282, NA, 803, 269, 368, NA,
                   NA, 137, NA, 249, 140, 184, NA,
                   NA, 95, NA, 286, 141, 99, NA),
  hr = c(NA, 0.86, 0.86, 0.93, 0.8, 0.99, 0.9,
         NA, 0.62, 0.87, 0.98, 0.78, 0.91, 0.85,
         NA, 0.65, 0.67, 0.73, 0.61, 0.7, 0.68),
  ci_l = c(NA, 0.74, 0.75, 0.84, 0.67, 0.88, 0.85,
           NA, 0.49, 0.72, 0.82, 0.61, 0.77, 0.78,
           NA, 0.5, 0.52, 0.61, 0.47, 0.54, 0.61),
  ci_u = c(NA, 0.99, 0.97, 1.03, 0.95, 1.12, 0.95,
           NA, 0.77, 1.06, 1.17, 1.0, 1.1, 0.93,
           NA, 0.85, 0.87, 0.88, 0.8, 0.9, 0.76),
  weight = c(NA, 15.7, 20.1, 32, 10, 21.2, NA,
             NA, 15.6, 21.3, 25.2, 13.1, 24.8, NA,
             NA, 16.1, 17.1, 33.7, 16, 17.1, NA)
) %>%
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
  row = 22- row_number())

ggplot()+
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 20.5))+
  geom_segment(aes(x = 0.45, xend = 1.25, y = 0, yend = 0))+
  coord_cartesian(ylim = c(0,24), expand = FALSE)+
  geom_text(data = data, aes(x = -0.7 , y = row, label = title), hjust = 0, fontface = "bold") +
  geom_text(data = data, aes(x = -0.65, y = row, label = study), hjust = 0)+
  geom_text(data = data, aes(x = 0, y = row, label = n_total_treat), hjust = 1)+
  geom_text(data = data, aes(x = 0.1, y = row, label = n_event_treat), hjust = 1)+
  geom_text(data = data, aes(x = 0.3, y = row, label = n_total_plac), hjust = 1)+
  geom_text(data = data, aes(x = 0.4, y = row, label = n_event_plac), hjust = 1)+
  geom_text(data = data, aes(x = 1.3, y = row, label = weight), hjust = 1)+
  geom_text(data = data, aes(x = 1.67, y = row, label = print_est), hjust = 1)+
  annotate("text", x= 0.05, y =23, label = "N, treatment", fontface = "bold", hjust = 0.7)+
  annotate("text", x= 0.35, y =23, label = "N, placebo", fontface = "bold", hjust = 0.7)+
  annotate("text", x = 0, y = 22, label = "Total", fontface = "bold", hjust = 1.2)+
  annotate("text", x = 0.1, y = 22, label = "Events", fontface = "bold", hjust = 0.77)+
  annotate("text", x = 0.3, y = 22, label = "Total", fontface = "bold", hjust = 1.2)+
  annotate("text", x = 0.4, y = 22, label = "Events", fontface = "bold", hjust = 0.77)+
  annotate("text", x = 1.3, y = 22, label = "Weight (%)", fontface = "bold", hjust = 0.8)+
  annotate("text", x = 1.67, y = 22, label = "HR (95% CI)", fontface = "bold", hjust = 1.1)+
  geom_point(data = filter(data, study != "Fixed effects model"), aes(x = hr, y =row))+
  geom_errorbar(data = filter(data, study != "Fixed effects model"), aes(xmin = ci_l, xmax = ci_u, y =row), width = .3)+
  geom_polygon(data = rMaster(data, "mace"),
               mapping = aes(x = x, y = y),
               color = "#E69F00",
               fill = "#E69F00",
               inherit.aes = FALSE
  )+
  geom_polygon(data = rMaster(data, "cvd"),
               mapping = aes(x = x, y = y),
               color = "#E69F00",
               fill = "#E69F00",
               inherit.aes = FALSE
  )+
  geom_polygon(data = rMaster(data, "hf"),
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



ggsave(filename = "meta_risk.png", width = 9, height = 6)
