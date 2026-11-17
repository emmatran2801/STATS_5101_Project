setwd("~/STATS_5101_Project")

library(dplyr)
library(ggplot2)
library(gridExtra)

h1b_2021 = read.csv("data/h-1b-data-export-2021.csv")
h1b_2022 = read.csv("data/h-1b-data-export-2022.csv")
h1b_2023 = read.csv("data/h-1b-data-export-2023.csv")

h1b_all <- rbind(h1b_2021, h1b_2022, h1b_2023)
h1b_all$Fiscal.Year <- as.factor(h1b_all$Fiscal.Year)

# Aggregate for all states
h1b_agg <- h1b_all %>%
  group_by(Fiscal.Year, State) %>%
  summarise(
    initial_approvals    = sum(Initial.Approval,    na.rm = TRUE),
    continuing_approvals = sum(Continuing.Approval, na.rm = TRUE),
    .groups = "drop"
  )

# Plot the data
plot_data <- h1b_agg %>%
  group_by(Fiscal.Year) %>%
  summarise(
    mean_initial    = mean(initial_approvals),
    se_initial      = sd(initial_approvals) / sqrt(n()),
    mean_continuing = mean(continuing_approvals),
    se_continuing   = sd(continuing_approvals) / sqrt(n())
  )

# Mean H-1B initial approvals by fiscal year
p1 <- ggplot(plot_data, aes(x = Fiscal.Year, y = mean_initial, fill = Fiscal.Year)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_initial - se_initial,
                    ymax = mean_initial + se_initial), width = 0.2) +
  labs(x = "Fiscal Year", y = "Mean Initial Approvals") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(expand = c(0, 0))

# Mean H-1B continuing approvals by fiscal year
p2 <- ggplot(plot_data, aes(x = Fiscal.Year, y = mean_continuing, fill = Fiscal.Year)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = mean_continuing - se_continuing,
                    ymax = mean_continuing + se_continuing), width = 0.2) +
  labs(x = "Fiscal Year", y = "Mean Continuing Approvals") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid = element_blank(),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(expand = c(0, 0))

pdf("figures/mean_approvals_initial_continuing_barplots.pdf", width = 10, height = 5)
grid.arrange(p1, p2, ncol = 2)
dev.off()


# Initial model for differences in H-1B initial approvals
anova_approvals <- aov(initial_approvals ~ Fiscal.Year, data = h1b_agg)

# Checking assumptions: Normality of residuals (Q-Q plot) & Homogeneity of variance (Residuals vs Fitted)
pdf("figures/anova_initial_approvals_first_model_diagnostic_plots.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
plot(anova_approvals, which = c(1, 2))
dev.off()

# Log-transformation:
h1b_agg <- h1b_agg %>%
  mutate(log_initial_approvals = log1p(initial_approvals))

log_anova_approvals <- aov(log_initial_approvals ~ Fiscal.Year, data = h1b_agg)

pdf("figures/anova_initial_approvals_log_model_diagnostic_plots.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
plot(log_anova_approvals, which = c(1, 2))
dev.off()

# Log-transformed model for differences in H-1B initial approvals
summary(log_anova_approvals) # not significant => stop here


# Initial model for differences in H-1B continuing approvals
anova_continuing <- aov(continuing_approvals ~ Fiscal.Year, data = h1b_agg)

# Checking assumptions: Normality of residuals (Q-Q plot) & Homogeneity of variance (Residuals vs Fitted)
pdf("figures/anova_continuing_approvals_first_model_diagnostic_plots.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
plot(anova_approvals, which = c(1, 2))
dev.off()

# Log-transformation:
h1b_agg <- h1b_agg %>%
  mutate(log_continuing_approvals = log1p(continuing_approvals))

log_continuing_approvals <- aov(log_continuing_approvals ~ Fiscal.Year, data = h1b_agg)

pdf("figures/anova_continuing_approvals_log_model_diagnostic_plots.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
plot(log_anova_approvals, which = c(1, 2))
dev.off()

# Log-transformed model for differences in H-1B continuing approvals
summary(log_continuing_approvals) # not significant => stop here



