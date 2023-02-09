library(mgcv)
library(gratia)
library(tidyverse)
library(patchwork)

df <- readxl::read_xlsx("dat/test_final.xlsx") %>%
        mutate(rel_tourists = 100*(tourists/max(tourists)), 
               rel_biomass = 100*(biomass/max(biomass)))

p1 <- ggplot(df, aes(x=biomass, y=tourists)) +
        geom_point() +
        geom_smooth(method = "glm", se = F, col = "black") +
        labs(x="Fish biomass (ton/ha)", y="Number of visitors") +
        xlim(0,5) +
        ylim(0,5000) +
        theme_bw()
p1

m1 <- glm(tourists ~ biomass, data = df)

summary(m1)
report::report(m1)

m2 <- gam(tourists_rate ~ s(biomass_rate, bs = "tp", k = 5), data = df)

summary(m2)

report::report(m2)

draw(m2, residuals = T)


m2 <- gam(tourists_rate ~ s(biomass_rate, bs = "tp", k = 5) , data = df)

summary(m2)


report::report(m2)

draw(m2, residuals = T)

p2 <- ggplot(df, aes(x=biomass_rate, y=tourists_rate)) +
        geom_point() +
        geom_smooth(method = "gam", method.args = list(family = "binomial"), formula = y ~ s(x, bs = "tp", k = 5)) +
        labs(x="Fish biomass (ton/ha)", y="Number of visitors") +
        theme_bw()
p2

predictions <- cbind(df, as.data.frame(predict(m2, se.fit = T)))

ggplot(predictions, aes(x=biomass_rate, y=fit)) +
        geom_line() +
        geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit), alpha=.2)


newdat <- data.frame(
        biomass_rate = seq(-150,150, by=1))


predval <- predict(m2,interval = 'confidence',se.fit = T, 
                   exclude = "year",
                   type = "response", 
                   newdata = newdat) |> 
        as.data.frame()

final <- cbind(predval, newdat)

p3 <- final |> 
        ggplot(aes(x=biomass_rate, y=fit)) +
        geom_hline(yintercept = 0, col = "gray30") +
        geom_vline(xintercept = 0, col = "gray30") +
        geom_point(data = df, aes(x=biomass_rate, y=tourists_rate)) +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit), alpha=.2) +
        labs(x="Fish biomass change rate (%)", y="Number of visitors change rate (%)") +
        xlim(c(-150, 150)) +
        theme_bw()

p3






m2 <- gam(tourists_rate ~ s(biomass_rate, bs = "tp", k = 5) + year, data = df)

summary(m2)

report::report(m2)

draw(m2, residuals = T)

predictions <- cbind(df, as.data.frame(predict(m2, se.fit = T)))

ggplot(predictions, aes(x=biomass_rate, y=fit)) +
        geom_line() +
        geom_ribbon(aes(ymin=fit-se.fit, ymax=fit+se.fit), alpha=.2)


newdat <- data.frame(
        biomass_rate = seq(0,100, by=9), 
        year = seq(2023,2034, by = 1))


predval <- predict(m2,interval = 'confidence',se.fit = T,
                   type = "response", 
                   newdata = newdat) |> 
        as.data.frame()

final <- cbind(predval, newdat)




p4 <- final |> 
        ggplot(aes(x=biomass_rate, y=fit)) +
        geom_line() +
        # geom_hline(yintercept = 0, col = "gray30") +
        # geom_vline(xintercept = 0, col = "gray30") +
        #geom_point(data = df, aes(x=biomass_rate, y=tourists_rate)) +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin=fit-se.fit/2, ymax=fit+se.fit/2), alpha=.2) +
        labs(x="Fish biomass change rate (%)", y="Number of visitors change rate (%)") +
        #xlim(c(-150, 150)) +
        theme_bw()

p4

p3 

ggsave("figs/Final_biomass_rel.png", dpi = 600, height = 4, width = 4)




final




eco <- readxl::read_xlsx("dat/da_me_economic_value_scuba1.xlsx") %>% 
        janitor::clean_names()


eco <- eco %>% 
        filter(locality %in% c("LA_PAZ", "CABO_SAN_LUCAS", "LORETO", "CABO_PULMO"))



results_table <- eco %>% 
        mutate(region = stringr::str_to_title(str_replace_all(locality, "_", " "))) |> 
        mutate(locality = factor(locality,
                                 labels = c("FP", "NP", "LP", "LP"))) %>% 
        group_by(locality, region) %>% 
        summarise(buzos_median = quantile(n_buzos_week, probs = .5),
                  buzos_Q2 = quantile(n_buzos_week, probs = .25),
                  buzos_Q3 = quantile(n_buzos_week, probs = .75),
                  price_median = quantile(cost_divetrip, probs = .5),
                  price_Q2 = quantile(cost_divetrip, probs = .25),
                  price_Q3 = quantile(cost_divetrip, probs = .75),
                  p_costs_median = quantile(p_costs, probs = .5),
                  p_costs_Q2 = quantile(p_costs, probs = .25),
                  p_costs_Q3 = quantile(p_costs, probs = .75), 
                  n_ops_survey = n_distinct(respondent_id)) %>% 
        mutate(revenues_median = price_median*buzos_median*52,
               net_revenues_median = revenues_median - (revenues_median * (p_costs_median/100)),
               revenues_Q2 = price_Q2*buzos_Q2*52, 
               net_revenues_Q2 = revenues_Q2 - (revenues_Q2 * (p_costs_Q2/100)),
               revenues_Q3 = price_Q3*buzos_Q3*52,
               net_revenues_Q3 = revenues_Q3 - (revenues_Q3 * (p_costs_Q3/100))) |> 
        select(locality, region, revenues_median, revenues_Q2, revenues_Q3)


p5 <- final |> 
        mutate(csl = as.vector(results_table[2,3]$revenues_median), 
               csl = csl * (fit/100), 
               csl_Q2 = as.vector(results_table[2,4]$revenues_Q2), 
               csl_Q2 = csl_Q2 * (fit/100), 
               csl_Q3 = as.vector(results_table[2,5]$revenues_Q3), 
               csl_Q3 = csl_Q3 * (fit/100),
               lp = as.vector(results_table[3,3]$revenues_median), 
               lp = lp * (fit/100), 
               lp_Q2 = as.vector(results_table[3,4]$revenues_Q2), 
               lp_Q2 = lp_Q2 * (fit/100), 
               lp_Q3 = as.vector(results_table[3,5]$revenues_Q3), 
               lp_Q3 = lp_Q3 * (fit/100), 
               lo = as.vector(results_table[4,3]$revenues_median), 
               lo = lo * (fit/100), 
               lo_Q2 = as.vector(results_table[4,4]$revenues_Q2), 
               lo_Q2 = lo_Q2 * (fit/100), 
               lo_Q3 = as.vector(results_table[4,5]$revenues_Q3), 
               lo_Q3 = lo_Q3 * (fit/100)
        ) |> 
        select(-fit, -se.fit, -biomass_rate) |> 
        pivot_longer(names_to = "area", values_to = "revenue", cols = csl:lo_Q3) |> 
        mutate(estimate = case_when(
                str_detect(area, "Q2") ~ "Q2", 
                str_detect(area, "Q3") ~ "Q3", 
                TRUE ~ "est"),
               region = case_when(
                       str_detect(area, "csl") ~ "Cabo San Lucas", 
                       str_detect(area, "lp") ~ "La Paz",
                       str_detect(area, "lo") ~ "Loreto"), 
               pl = case_when(
                       str_detect(area, "csl") ~ "NP", 
                       str_detect(area, "lp") ~ "LP",
                       str_detect(area, "lo") ~ "LP"
               )) |> 
        select(-area) |> 
        pivot_wider(names_from = estimate, values_from = revenue) |> 
        group_by(year, pl) |> 
        summarise_if(is.numeric, mean) |> 
        ggplot(aes(x = year, y = est/1000,  col = pl)) +
        geom_ribbon(aes(ymin=(est-Q2/2)/1000, ymax = (est+Q3/2)/1000), alpha=.3, fill = NA, linetype = 2, linewidth = .5) +
        geom_line(, linewidth = 1) +
        scale_x_continuous(breaks = seq(2019, 2030, by = 1)) +
        labs(x="Year", y="Annual increase rate in revenues (in thousands USD)", color = "Protection level") +
        theme_bw() +
        scale_color_manual(values = c("#994F00", "#006CD1")) +
        theme(axis.text.x = element_text(angle = 90))

p4 + p5 +
        plot_annotation(tag_levels = "A") +
        plot_layout(guides = "collect") & theme(legend.position = "bottom")


ggsave('figs/final_figure_biomass_model.png', dpi = 600, height = 6, width = 8)


p5 +
        plot_layout(guides = "collect") & theme(legend.position = "bottom")


ggsave('figs/final_figure_biomass_model.png', dpi = 600, height = 6, width = 6)




