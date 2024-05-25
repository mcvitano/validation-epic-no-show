


#knitr::purl(input = "_analysis.Rmd", output = "Report.R", documentation = 2)

#```{r setup, include=FALSE}
#knitr::knit_hooks$set(purl = knitr::hook_purl)
#knitr::opts_chunk$set(echo = TRUE)
#```

library(dplyr)
library(duckdb)
# cowplot, glue, stringr, rmda, gtsummary, gt, flexsummary
dir.create('figures', showWarnings = FALSE)

source('utils.R')


con <- dbConnect(duckdb(), 'data/noshow_data.duckdb')

appts <- dbGetQuery(con, 'select * from noshow_first_eligible_encounter')



################################################################################
#                               DEMOGRAPHICS
#
################################################################################
appts %>% 
  gtsummary::tbl_summary(
    include = c(age, sex, insurance, raceth,
                is_adopted_yn, current_smoker_yn, homeless_180_days_flag, bmi,
                recent_noshow_count,
                language_primary, relationship_status,
                noshow_flag, noshow_risk, noshow_risk_cat),
    
    label = list(
      age ~ 'Age',
      sex ~ 'Sex',
      raceth ~ 'Race/Ethnicity',
      language_primary ~ 'Preferred Language (current)',
      relationship_status ~ 'Marital Status (current)',
      is_adopted_yn ~ 'Is adopted',
      current_smoker_yn ~ 'Current Smoker',
      insurance ~ 'Insurance',
      homeless_180_days_flag ~ 'Homeless (past 180 days)',
      bmi ~ 'BMI',
      recent_noshow_count ~ 'No-show count (of last 2 appointments)',
      noshow_flag ~ 'Outcome (no-show)',
      noshow_risk ~ 'Risk of no-show',
      noshow_risk_cat ~ 'Risk of no-show'
    )
  ) %>% 
  gtsummary::as_gt() %>% 
  gt::gtsave('figures/table1.docx')



################################################################################
#                               OUTCOMES
#
################################################################################
appts %>% 
  group_by(noshow_risk_cat) %>% 
  summarise(
    'Outcomes (N)' = sum(noshow_flag), 
    'Proportion (%)' = round(mean(noshow_flag), 2)) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/table2-outcomes-by-threshold.docx')


################################################################################
#                               CALIBRATION
#
################################################################################

# Calibration Plot
calplot_overall_binned <- binned_calibration_plot(
  data = appts, 
  p = noshow_risk,
  outcome = noshow_flag,
  nbins=10,
  display_plot = FALSE)

ggsave(calplot_overall_binned,
       filename='figures/calplot-overall-binned.png')


calplot_overall_smoothed <- smoothed_calibration_plot(
  data = appts, 
  p = noshow_risk,
  outcome = noshow_flag,
  display_plot = FALSE)

ggsave(calplot_overall_smoothed,
       filename='figures/calplot-overall-smooth.png')


# Calibration Metrics
metrics_overall <- binary_calibration_metrics(
  data = appts, 
  p = noshow_risk, 
  outcome = noshow_flag,
  xb = linear_predictor)

metrics_overall %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/metrics-overall.docx')


################################################################################
#                               NET BENEFIT
#
################################################################################

#############
# Full Range
#
#############
baseline.model <- rmda::decision_curve(
  noshow_flag ~ noshow_risk, 
  appts,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  confidence.intervals='none'
)

png('figures/net-benefit-noshow-overall-full.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("No-show Model"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.10, 0.10), 
  xlim = c(0, 1)
)
dev.off()

#############
# Detail
#
#############
baseline.model <- rmda::decision_curve(
  noshow_flag ~ noshow_risk, 
  appts,
  fitted.risk = TRUE, 
  policy = 'opt-in', 
  thresholds = c(0.15, 0.3, 0.4),
  confidence.intervals='none'
)

png('figures/net-benefit-noshow-overall-detail.png')

rmda::plot_decision_curve(
  baseline.model, 
  standardize=FALSE, 
  cost.benefit.axis = FALSE,
  confidence.intervals = FALSE,
  curve.names = c("No-show Model"),
  col = c('black', 'red', 'blue'),
  lty = c(2, 1, 1),
  ylim = c(-0.10, 0.10)
)
dev.off()




################################################################################
#                                 BY PCMH
#
################################################################################

metrics_by_pcmh <- data.frame()

for(dept in unique(appts$department_name)) {
  
  this_pcmh <- appts[appts$department_name == dept, ]
  
  # Calibration plot
  this_pcmh_calplot <- binned_calibration_plot(
    data = this_pcmh,
    p = noshow_risk,
    outcome = noshow_flag,
    nbins=10,
    display_plot = FALSE)
  
  # shorten department name
  dept_clipped <- stringr::str_remove(dept, 'jps')
  dept_clipped <- stringr::str_remove(dept_clipped, 'chc')
  dept_hyphen <- stringr::str_replace_all(dept_clipped, ' ', '-')
  
  ggsave(this_pcmh_calplot,
         filename=glue::glue('figures/calplot-{dept_hyphen}.png'))
  
  # Calibration metrics
  metrics_by_pcmh <- bind_rows(
    metrics_by_pcmh,
    
    binary_calibration_metrics(
      data = this_pcmh,
      p = noshow_risk,
      outcome = noshow_flag,
      xb = linear_predictor
    ) %>% mutate(PCMH = stringr::str_to_title(dept_clipped))
  )
  
  # Net benefit
  # { not performed }
}

tidyr::pivot_wider(
  id_cols = Metric, values_from = Value, names_from = PCMH,
  data = metrics_by_pcmh) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/metrics-by-pcmh.docx')


################################################################################
#                             BY RACE/ETHNICITY
#
################################################################################

# Combined Calibration plot (Hispanic, NH Black, NH White)
race_counts <-  appts %>% 
  group_by(raceth) %>% 
  summarise(n = n())

n_hispanic <- race_counts[race_counts$raceth=='hispanic',]$n
n_black <- race_counts[race_counts$raceth=='nh black',]$n
n_white <- race_counts[race_counts$raceth=='nh white',]$n

loess_hispanic <- data.frame(
  x = lowess(x = appts[appts$raceth=='hispanic',]$noshow_risk, 
             y = appts[appts$raceth=='hispanic',]$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = appts[appts$raceth=='hispanic',]$noshow_risk,
             y = appts[appts$raceth=='hispanic',]$noshow_flag, f = 2/3, iter = 0)$y,
  raceth = 'Hispanic')

loess_black <- data.frame(
  x = lowess(x = appts[appts$raceth=='nh black',]$noshow_risk, 
             y = appts[appts$raceth=='nh black',]$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = appts[appts$raceth=='nh black',]$noshow_risk,
             y = appts[appts$raceth=='nh black',]$noshow_flag, f = 2/3, iter = 0)$y,
  raceth = 'NH Black')

loess_white <- data.frame(
  x = lowess(x = appts[appts$raceth=='nh white',]$noshow_risk, 
             y = appts[appts$raceth=='nh white',]$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = appts[appts$raceth=='nh white',]$noshow_risk,
             y = appts[appts$raceth=='nh white',]$noshow_flag, f = 2/3, iter = 0)$y,
  raceth = 'NH White')

loess_combined <- as.data.frame(
  rbind(loess_hispanic, loess_black, loess_white))


calplot_raceth_all <- 
  ggplot(loess_combined, 
         aes(.data$x, y = .data$y, group = .data$raceth, col = .data$raceth)) + 
  geom_line() + 
  coord_cartesian(xlim = c(0, 1), 
                  ylim = c(0, 1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Predicted Risk",
       y = "Observed Event Rate") + 
  scale_colour_discrete(
    name = "Race/Ethnicity", 
    labels = c(
      glue::glue("Hispanic (N = {n_hispanic})"), 
      glue::glue("NH Black (N = {n_black})"), 
      glue::glue("NH White (N = {n_white})"))
  )


ggsave(calplot_raceth_all,
       filename=glue::glue('figures/calplot-raceth-all.png'))



# # Risk groups (deciles)
# risk_groups <- 
#   filter(appts, raceth %in% c('hispanic', 'nh black', 'nh white')) %>% 
#   select(noshow_risk, noshow_flag, raceth) %>% 
#   mutate(bin = ntile( noshow_risk, 10 )) %>% 
#   group_by(raceth, bin) %>%
#   mutate(n = n(), 
#          bin_pred = mean( noshow_risk ), 
#          bin_prob = mean( noshow_flag ), 
#          se = sqrt((bin_prob * (1 - bin_prob)) / n), 
#          ul = bin_prob + 1.96 * se, 
#          ll = bin_prob - 1.96 * se) %>% 
#   # may produce *WARNINGS* due to "rows containing missing values"
#   # (removed by lower bound of confiddence limit < 0)
#   mutate(ll = replace(ll, which(ll<0), 0)) %>% 
#   ungroup()
# 
# 
# # Base plot
# g1 <- risk_groups %>%
#   ggplot(aes(x=bin_pred, y=bin_prob, 
#              shape = raceth, color = raceth)) +
#   geom_point(size=2) +
#   geom_linerange(aes(ymin = ll, ymax = ul), linetype = "dashed") +
#   scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank(),
#         panel.border = element_blank(), 
#         axis.line = element_line()) +
#   xlab("Predicted Risk") +
#   ylab("Observed Event Rate") +
#   scale_colour_manual(name = '',
#                       labels = c('Hispanic', 'NH Black', 'NH White'),
#                       values = c("#1b9e77", "#d95f02", "#7570b3")) +   
#   scale_shape_manual(name = '',
#                      labels = c('Hispanic', 'NH Black', 'NH White'),
#                      values = c(15, 16, 17))
# 
# ggsave(g1,
#        filename=glue::glue('figures/calplot-raceth-all-binned.png'))

rm(loess_hispanic, loess_black, loess_white, loess_combined)


# By Race/Ethnicity
metrics_by_raceth <- data.frame()

for(raceth_cat in c('hispanic', 'nh black', 'nh white')) {
  
  this_raceth <- appts[appts$raceth == raceth_cat, ]
  
  # Calibration plot
  this_raceth_calplot <- binned_calibration_plot(
    data = this_raceth,
    p = noshow_risk,
    outcome = noshow_flag,
    nbins=10,
    display_plot = FALSE)
  
  # hyphenated race/ethnicity
  raceth_hyphen <- stringr::str_replace_all(raceth_cat, ' ', '-')
  
  ggsave(this_raceth_calplot,
         filename=glue::glue('figures/calplot-{raceth_hyphen}.png'))
  
  # Calibration metrics
  metrics_by_raceth <- bind_rows(
    metrics_by_raceth,
    
    binary_calibration_metrics(
      data = this_raceth,
      p = noshow_risk,
      outcome = noshow_flag,
      xb = linear_predictor
    ) %>% mutate(RACETH = stringr::str_to_title(raceth))
  )
  
  # Net benefit
  # { not performed }
  
  rm(this_raceth_calplot)
}

tidyr::pivot_wider(
  id_cols = Metric, values_from = Value, names_from = RACETH,
  data = metrics_by_raceth) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/metrics-by-raceth.docx')



################################################################################
#                       WITH VS. WITHOUT TELEHEALTH
#
################################################################################

# Combined Calibration plot
no_telehealth <- appts %>%
  #  remove telehealth/telephone
  filter(!grepl('telehealth|telephone', visit_type))

n_with_telehealth <- nrow(appts)
n_wo_telehealth <- nrow(no_telehealth)

loess_with_telehealth <- data.frame(
  x = lowess(x = appts$noshow_risk, 
             y = appts$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = appts$noshow_risk,
             y = appts$noshow_flag, f = 2/3, iter = 0)$y,
  group = 'Y')

loess_wo_telehealth <- data.frame(
  x = lowess(x = no_telehealth$noshow_risk, 
             y = no_telehealth$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = no_telehealth$noshow_risk,
             y = no_telehealth$noshow_flag, f = 2/3, iter = 0)$y,
  group = 'N')

loess_combined <- as.data.frame(
  rbind(loess_with_telehealth, loess_wo_telehealth))


calplot_telehealth_all <- 
  ggplot(loess_combined, 
         aes(.data$x, y = .data$y, group = .data$group, col = .data$group)) + 
  geom_line() + 
  coord_cartesian(xlim = c(0, 1), 
                  ylim = c(0, 1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Predicted Risk",
       y = "Observed Event Rate") + 
  scale_colour_discrete(
    name = "Include Telehealth?", 
    labels = c(
      glue::glue("Y (N = {n_with_telehealth})"), 
      glue::glue("N (N = {n_wo_telehealth})"))
  )


ggsave(calplot_telehealth_all,
       filename=glue::glue('figures/calplot-telehealth-all.png'))

rm(loess_with_telehealth, loess_wo_telehealth, loess_combined)


# Calibration Metrics
tidyr::pivot_wider(
  id_cols = Metric, values_from = Value, names_from = telehealth_pop,
  data = bind_rows(
    
    binary_calibration_metrics(
      data = appts,
      p = noshow_risk,
      outcome = noshow_flag,
      xb = linear_predictor) %>% mutate(telehealth_pop = 'With Telehealth'),
    
    binary_calibration_metrics(
      data = no_telehealth,
      p = noshow_risk,
      outcome = noshow_flag,
      xb = linear_predictor) %>% mutate(telehealth_pop = 'Without Telehealth')
  )) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/metrics-by-telehealth.docx')



################################################################################
#                                 TIME HORIZONS
#
################################################################################
appts <- appts %>% 
  mutate(lead_time = 
           as.numeric(lubridate::date(appt_time) - lubridate::date(appt_made_date))) %>% 
  filter(lead_time %in% c(1, 3, 7))

horizon_counts <-  appts %>% 
  group_by(lead_time) %>% 
  summarise(n = n())

n_7days <- horizon_counts[horizon_counts$lead_time==7,]$n
n_3days <- horizon_counts[horizon_counts$lead_time==3,]$n
n_1day <- horizon_counts[horizon_counts$lead_time==1,]$n

loess_7d <- data.frame(
  x = lowess(x = appts[appts$lead_time==7,]$noshow_risk, 
             y = appts[appts$lead_time==7,]$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = appts[appts$lead_time==7,]$noshow_risk,
             y = appts[appts$lead_time==7,]$noshow_flag, f = 2/3, iter = 0)$y,
  lead_time = '7 days')

loess_3d <- data.frame(
  x = lowess(x = appts[appts$lead_time==3,]$noshow_risk, 
             y = appts[appts$lead_time==3,]$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = appts[appts$lead_time==3,]$noshow_risk,
             y = appts[appts$lead_time==3,]$noshow_flag, f = 2/3, iter = 0)$y,
  lead_time = '3 days')

loess_1d <- data.frame(
  x = lowess(x = appts[appts$lead_time==1,]$noshow_risk, 
             y = appts[appts$lead_time==1,]$noshow_flag, f = 2/3, iter = 0)$x, 
  y = lowess(x = appts[appts$lead_time==1,]$noshow_risk,
             y = appts[appts$lead_time==1,]$noshow_flag, f = 2/3, iter = 0)$y,
  lead_time = '1 day')

loess_combined <- as.data.frame(
  rbind(loess_7d, loess_3d, loess_1d))


calplot_time_horizons_all <- 
  ggplot(loess_combined, 
         aes(.data$x, y = .data$y,
             group = .data$lead_time, col = .data$lead_time)) + 
  geom_line() + 
  coord_cartesian(
    xlim = c(0, 1), 
    ylim = c(0, 1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Predicted Risk",
       y = "Observed Event Rate") + 
  scale_colour_discrete(
    name = "Lead Time", 
    labels = c(
      glue::glue("7 days (N = {n_7days})"), 
      glue::glue("3 days (N = {n_3days})"), 
      glue::glue("1 day (N = {n_1day})"))
  )

ggsave(calplot_time_horizons_all,
       filename=glue::glue('figures/calplot-time-horizons-all.png'))

rm(loess_7d, loess_3d, loess_1d, loess_combined)


# Calibration metrics
tidyr::pivot_wider(
  id_cols = Metric, values_from = Value, names_from = lead_time,
  data = bind_rows(
    
    binary_calibration_metrics(
      data = appts[appts$lead_time == 1, ],
      p = noshow_risk,
      outcome = noshow_flag,
      xb = linear_predictor) %>% mutate(lead_time = '1 day'),
    
    binary_calibration_metrics(
      data = appts[appts$lead_time == 3, ],
      p = noshow_risk,
      outcome = noshow_flag,
      xb = linear_predictor) %>% mutate(lead_time = '3 days'),
    
    binary_calibration_metrics(
      data = appts[appts$lead_time == 7, ],
      p = noshow_risk,
      outcome = noshow_flag,
      xb = linear_predictor) %>% mutate(lead_time = '7 days'),
  )) %>% 
  flextable::regulartable() %>%
  flextable::autofit() %>% 
  flextable::save_as_docx(path='figures/metrics-by-lead-time.docx')
