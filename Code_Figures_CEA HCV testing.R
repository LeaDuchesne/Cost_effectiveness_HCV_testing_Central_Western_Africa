# Code to replicate the figures from the article "Model-based cost-effectiveness estimates of testing strategies 
# for diagnosing hepatitis C virus infection in Central and Western Africa" published in PlosOne in 2020

##############################################################################################################################

######## Packages required
library(ggplot2)
library(scales)
library(glue)
library(ggtext)
library(dplyr)
library(httr) 

######## Symbols
euro <- "\u20AC"
infinity <- "\u221e"
arrow <- "\u2192"

##############################################################################################################################

######## Figures

#### Fig 2: Cost-effectiveness acceptability curve 

## Data file

# The data used in this analysis were retrieved from the Cost-effectiveness acceptability curve text report of TreeAge Pro software (© 2018, TreeAge Software, Inc. Williamstown, MA, USA). 
# A simplified version of the corresponding csv file can be accessed from GitHub as follows:
file_ceac <- "https://raw.githubusercontent.com/LeaDuchesne/Cost_effectiveness_HCV_testing_Central_Western_Africa/master/CEA%20HCV%20testing_Fig%202_CEAC.csv"

df_ceac <- read.csv(file_ceac,
                    header = TRUE,
                    sep = ";",
                    dec = ".")

## Variables

# wtp: range of willingness-to-pay values analyzed  
# strat_name: abbreviated names of the strategies
# strat_des: description of the strategies  
# prob: percentage of iterations that favor each strategy for the corresponding WTP value

# Legend labels
lab_graph =  c(expression(paste(S[12], ": POC HCV-RNA")),
               expression(paste(S[4], ": POC HCV-Ab ", symbol('\256'), " Lab HCV-RNA (DBS)")),
               expression(paste(S[5], ": POC HCV-Ab ", symbol('\256'), " POC HCV-RNA")),
               expression(paste(S[8], ": POC HCV-Ab ", symbol('\256'), " Lab HCV-cAg (venepuncture)"))
)

# Figure
g_ceac <- ggplot (data = df_ceac, 
                  aes(x = wtp,
                      y = prob, 
                      color = strat_name)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = c(0.65, 0.87), 
        legend.title=element_blank(),
        legend.text.align = 0) +
  scale_color_manual(values = c("red", "green", "blue", "purple"),
                     breaks  = levels(df_ceac$strat_name),
                     labels = lab_graph) +
  scale_y_continuous(name = "Percentage of simulations (n = 10,000) favouring intervention (%)",
                     breaks = c(0, 25, 50, 75, 100),
                     labels = c(0, 25, 50, 75, 100))+
  scale_x_continuous(name = paste("Willingness-to-pay (", euro, " per true positive case detected)", sep = ""),
                     labels = format(seq(0, 5000, 1000), 
                                     big.mark = ",", 
                                     scientific = FALSE)) 

#### S1 Fig: Percentage of HCV-infected individuals diagnosed versus lost-to-follow-up rate, according to the type of samples used

## Data file

# The data used in this analysis were retrieved from TreeAge Pro (One-Way cost-effectiveness sensitivity analysis text report). 
# A simplified version of the corresponding csv file can be accessed from GitHub as follows:
file_ltfu_se_strat <- "https://raw.githubusercontent.com/LeaDuchesne/Cost_effectiveness_HCV_testing_Central_Western_Africa/master/CEA%20HCV%20testing_S1%20Fig_LTFU.csv"

df_ltfu_se_strat <- read.csv(file_ltfu_se_strat,
                             header = TRUE,
                             sep = ";",
                             dec = ".")

## Variables

# ltfu_range: range of loss-to-follow-up rate explored in the analysis (corresponds to the deterministic sensitvitiy analysis range indicated in Table 2)
# strat-name: abbreviated names of the strategies 
# strat_des: description of the strategies  
# se_strat: sensitivity of each strategy (i.e., the percentage of HCV-infected individuals correctly diagnosed) for each loss-to-follow-up rate value
# sequence: describes the sequence of tests used in each strategy
# sample_type: binary variable which designates the type of sample used in the corresponding strategy (venepuncture or DBS)  

# Creation of a variable (sequence_with_color) containing the facets labels with the strategies' names in color
df_ltfu_se_strat <- df_ltfu_se_strat %>%
  mutate(
    sequence_with_color = case_when(
      sequence == "Lab HCV-Ab - Lab HCV-RNA" ~ glue::glue("</span> Lab HCV-Ab \u2192 Lab HCV-RNA <br> <span style = 'color:red'> S<sub>ref</sub> </span> versus <span style = 'color:blue'> S<sub>2</sub>"),
      sequence == "Lab HCV-Ab - Lab HCV-cAg" ~ glue::glue("</span> Lab HCV-Ab \u2192 Lab HCV-cAg <br> <span style = 'color:red'> S<sub>6</sub> </span> versus <span style = 'color:blue'> S<sub>7</sub>"),
      sequence == "POC HCV-Ab - Lab HCV-RNA" ~ glue::glue("</span> POC HCV-Ab \u2192 Lab HCV-RNA <br> <span style = 'color:red'> S<sub>3</sub> </span> versus <span style = 'color:blue'> S<sub>4</sub>"),
      sequence == "POC HCV-Ab - Lab HCV-cAg" ~ glue::glue("</span> POC HCV-Ab \u2192 Lab HCV-cAg <br> <span style = 'color:red'> S<sub>8</sub> </span> versus <span style = 'color:blue'> S<sub>9</sub>"),
      TRUE ~ "na")
  )

# Order factor levels 
df_ltfu_se_strat$sequence_with_color <- as.factor(df_ltfu_se_strat$sequence_with_color)
df_ltfu_se_strat$sequence_with_color <- factor(df_ltfu_se_strat$sequence_with_color,
                                               levels(df_ltfu_se_strat$sequence_with_color)[c(2, 1, 4, 3)])

# Graphic
g_ltfu_se_strat <- ggplot(data = df_ltfu_se_strat,
                          aes(x = ltfu_range,
                              y = se_strat, 
                              group = sample_type)) +
  geom_line(aes(col = sample_type)) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  facet_wrap(~ df_ltfu_se_strat$sequence_with_color,
             scales = "free") +
  theme(strip.text.x = element_markdown(),
        axis.title.x = element_text(vjust = 0),
        legend.title=element_blank(),
        legend.position = c(0.5,0.52),
        legend.box.background = element_rect(colour = "black"),
        legend.spacing.y = unit(0, "mm"), 
        panel.spacing.y = unit(2, "cm"),
        panel.spacing.x = unit(0.4, "cm"),
        axis.line=element_line(),
        axis.ticks = element_line()) + 
  labs(y ="Percentage of HCV-infected individuals diagnosed \n in the target population (%)",
       x = "Lost-to-follow-up rates (%)") +
  coord_cartesian(xlim = c(0, 50), 
                  ylim = c(40, 100),
                  clip = "off")


#### S2 Fig: Cost per screened individual versus levels of HCV seroprevalence

## Data file

# The data used in this analysis were retrieved from TreeAge Pro (One-Way cost-effectiveness sensitivity analysis text report). 
# A simplified version of the corresponding csv file can be accessed from GitHub as follows:
file_cost_seroprev <- "https://raw.githubusercontent.com/LeaDuchesne/Cost_effectiveness_HCV_testing_Central_Western_Africa/master/CEA%20HCV%20testing_S2%20Fig_Seroprev.csv"

df_cost_seroprev <- read.csv(file_cost_seroprev,
                             header = TRUE,
                             sep = ";",
                             dec = ".")

## Variables

# seroprev: range of HCV seroprevalence values analyzed
# strat_name: abbreviated names of the strategies
# strat_des: description of the strategies  
# cost: cost of each strategy depending on the seroprevalence value
# domination: factor variable with 5 levels (S3, S4, S5, S12, dominated); aims to highlight the strategies that were consistently dominant in the cost-effectiveness analysis

## Graphic
g_cost_seroprev <- ggplot(data = df_cost_seroprev,
                          aes(x = seroprev, 
                              y = cost, 
                              group = strat_name)) +
  geom_line(aes(col = domination, 
                size = domination)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(0.1, 10, 20, 30, 40, 50, 60),
                     labels = c(0.1, 10, 20, 30, 40, 50, 60)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 60, 10)) +
  scale_color_manual(values = c("darkgrey", "blue", "green", "orange", "red")) +
  scale_size_manual(values = c(0.5, 1, 1, 1, 1)) +
  xlab("HCV seroprevalence (%)") +
  ylab(paste("Cost per screened individual (", euro, ")", sep = "")) +
  coord_cartesian(xlim = c(0.1, 60),
                  ylim = c(0,65),
                  clip = "off") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "cm"),
        legend.position = "none",
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"))

## Annotations

# Strategies' names
g_cost_seroprev <- g_cost_seroprev + 
  annotate(geom = "text", 
           x = 2, y = 41.4, 
           label = "S[11]", 
           color = "darkgrey", size = 4.3, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 2, y = 35.5, 
           label = "S[10]",
           color = "darkgrey", size = 4.3, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 28.7, 
           label = "S[8]", 
           color = "darkgrey", size = 4.3, angle = 31, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 32.1, 
           label = "S[9]", 
           color = "darkgrey", size = 4.3, angle = 31, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 9, y = 22, 
           label = "S[6]", 
           color = "darkgrey", size = 4.3, angle = 30, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 59.5, 
           label = "S[ref]", 
           color = "darkgrey", size = 4.3, angle = 40, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 9, y = 27.8, 
           label = "S[7]", 
           color = "darkgrey", size = 4.3, angle = 30, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 64.5, 
           label = "S[2]", 
           color = "darkgrey", size = 4.3, angle = 40, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 17.1, 
           label = "S[5]", 
           color = "red", size = 4.3, angle = 25, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 53.2, 
           label = "S[4]", 
           color = "orange", size = 4.3, angle = 40, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 13.1, 
           label = "S[12]", 
           color = "blue", size = 4.3, angle = 0, 
           parse = TRUE) +
  annotate(geom = "text", 
           x = 58, y = 49.8, 
           label = "S[3]", 
           color = "green", size = 4.3, angle = 40, 
           parse = TRUE) 

# Base-case value 
g_cost_seroprev <- g_cost_seroprev +
  geom_segment(aes(x = 3.9, 
                   y = 0, 
                   xend = 3.9, 
                   yend = 65), 
               linetype = "dashed", 
               size = 0.5) + 
  labs(tag = expression(paste(3.9, "%"))) +
  theme(plot.tag.position = c(0.119, 1.018),
        plot.tag = element_text(size = 11))

#### S3 Fig: Tornado diagram

## Data file

# The data used in this analysis were retrieved from TreeAge Pro (Tornado text report). 
# A simplified version of the corresponding csv file can be accessed from GitHub as follows:
file_tornado <- "https://raw.githubusercontent.com/LeaDuchesne/Cost_effectiveness_HCV_testing_Central_Western_Africa/master/CEA%20HCV%20testing_S3%20Fig_Tornado.csv"

df_tornado <- read.csv(file_tornado,
                       header = TRUE,
                       sep = ";",
                       dec = ".")

## Variables

# var_name: abbreviated names of the variables analyzed
# var_des: description of the variables 
# var_value: numeric values used in the 1-way analyses
# var_bound: indicates whether var_value corresponds to the lower or the higher bound of the interval defined for the 1-way sensitvity analyses (see Table 1)
# EV_icer: expected value (EV) of the ICER for the corresponding var_value 
# EV_icer_type: indicates whether EV_icer corresponds to the lower or higher ICER value obtained in the 1-way analysis of the considered variable
# EV_icer_spread: width of the bar (high EV_icer minus low EV_icer)

# Difference between the expected values of the ICER in the base-case analysis (EV_icer_ref) and in the 1-way analyses (EV_icer)
EV_icer_ref <- 3689.55
df_tornado$diff_EV <- df_tornado$EV_icer - EV_icer_ref

# Reorder variables' levels in order to sort tornado's bars in increasing order of width
df_tornado$var_name <- reorder(df_tornado$var_name, df_tornado$EV_icer_spread)
df_tornado$var_des <- reorder(df_tornado$var_des, df_tornado$EV_icer_spread)

# Graphic 
g_tornado <- ggplot(data = df_tornado,
                    aes(x = var_name, 
                        y = diff_EV)) +
  geom_bar(data = df_tornado[df_tornado$var_bound == "bound_high",],
           fill = "red", 
           stat = "identity", 
           position = "dodge", 
           width = 0.4) +
  geom_bar(data = df_tornado[df_tornado$var_bound == "bound_low",],
           fill = "blue", 
           stat = "identity", 
           position = "dodge", 
           width = 0.4) +
  coord_flip() +
  scale_y_continuous(name = paste("ICER (", euro, " per additional TP case identified)", 
                                  sep = ""),
                     limits = c(-3039.55, 1810.45),
                     breaks = seq(-3189.55, 1810.45, 500),
                     labels = format(seq(500, 5500, 500), 
                                     big.mark = ",", 
                                     scientific = FALSE)) +
  scale_x_discrete(name = "",
                   labels = levels(df_tornado$var_des)) +
  geom_hline(yintercept = 0, 
             colour = "black", 
             linetype = "dashed") +
  theme_minimal() +
  theme (axis.text.y = element_text(size = 10),
         axis.line.x = element_line(colour = "black"),
         axis.ticks.x = element_line(colour = "black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank())

# Annotations
g_tornado + annotate(geom = "text", 
                     x = 0.6, y = 900, 
                     label = format(paste("EV = ", EV_icer_ref), 
                                    big.mark = ",", 
                                    scientific = FALSE),
                     size = 3.6, color = "black") +
  annotate(geom = "text", 
           x = 9, y = 1530, 
           label = infinity, 
           size = 5, color = "black", angle = 90, fontface = "bold", 
           parse = TRUE)