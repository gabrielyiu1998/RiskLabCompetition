source("functions_analysis.R")
library(extrafont)

loadfonts(device = "win")

rbc_sdata = generalized_factor_prep("Royal Bank of Canada", 0.0235/12, 3,
                                    list(SP500, oil), c("SP", "OIL"))
rbc_sdata = left_join(rbc_sdata, fama_french, by = c("Year" = "YearDM", "Month" = "MonthDM")) %>%
  left_join(exc_rates, by = c("Year" = "YearEX", "Month" = "MonthEX"))

rbc_simulated = simulation_market_variables(3000, rbc_sdata) %>% data.frame()

sun_sdata = generalized_factor_prep("Suncor Energy Inc.", 0.0235/12, 3,
                                     list(SP500, oil), c("SP", "OIL"))
sun_sdata = left_join(sun_sdata, fama_french, by = c("Year" = "YearDM", "Month" = "MonthDM")) %>%
  left_join(exc_rates, by = c("Year" = "YearEX", "Month" = "MonthEX"))

sun_simulated = simulation_market_variables(3000, sun_sdata) %>% data.frame()

plotting_data = data.frame(Company = rep("RY", nrow(rbc_simulated)),
                           St_Rtn = rbc_simulated$St_Rtn) %>%
  rbind(data.frame(Company = rep("SU", nrow(sun_simulated)),
                   St_Rtn = sun_simulated$St_Rtn))

p = ggplot(plotting_data, aes(x = St_Rtn, group = Company, fill = Company)) +
  scale_fill_manual(values=alpha(c("#7192c6", "#e86666"))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(xintercept = quantile(rbc_simulated$St_Rtn, 0.01),
             color = "#5495ff", size=1) +
  geom_vline(xintercept = quantile(sun_simulated$St_Rtn, 0.01),
             color = "Red", size=1) +
  labs(x = "Excess Returns", y = "Count", title = "Value at Risk") +
  geom_text(aes(x=quantile(sun_simulated$St_Rtn, 0.01) - 0.05,
                label="SU", y=750),
            size = 5,
            angle = 90,
            colour="black",
            family = "Book Antiqua") +
  geom_text(aes(x=quantile(rbc_simulated$St_Rtn, 0.01) - 0.05,
                label="RY", y=750),
            size = 5,
            angle = 90,
            colour="black",
            family = "Book Antiqua") +
  theme_bw() +
  theme(text = element_text(size = 18, family = "Book Antiqua"),
        axis.text = element_text(size = 18, family = "Book Antiqua"),
        plot.title = element_text(size = 18, family = "Book Antiqua"),
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"))
  
  
