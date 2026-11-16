setwd("~/STATS_5101_Project")

library(fastDummies)
library(dplyr)
library(tidyr)

h1b_2021 = read.csv("data/h-1b-data-export-2021.csv")
h1b_2022 = read.csv("data/h-1b-data-export-2022.csv")
h1b_2023 = read.csv("data/h-1b-data-export-2023.csv")

h1b_all <- rbind(h1b_2021, h1b_2022, h1b_2023)

# Get top 5 states by total H1B applications
top5_states <- h1b_all %>%
  group_by(State) %>%
  summarise(total = sum(Initial.Approval + Initial.Denial, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  pull(State)

# Create a column that labels state as top 5 or "Other"
h1b_all <- h1b_all %>%
  mutate(Top_state = ifelse(State %in% top5_states, State, "Other"))

# One-hot encode
h1b_all <- dummy_cols(h1b_all, 
                      select_columns = "Top_state", 
                      remove_first_dummy = FALSE,   # keep all 6 columns
                      remove_selected_columns = TRUE)  # drop original Top_state

# Check for multicolinearity
cor(h1b_all[, c("Initial.Approval", "Initial.Denial", "Continuing.Approval")], 
    use = "complete.obs")

# Create an approval rate instead
h1b_all <- h1b_all %>%
  mutate(Total_application = Initial.Approval + Initial.Denial,
         Approval_rate = Initial.Approval / Total_application)

h1b_all <- h1b_all %>%
  mutate(Continuing_application = Continuing.Approval + Continuing.Denial,
         Continuing_approval_rate = Continuing.Approval / Continuing_application)

# Remove rows with very few total applications (unreliable rates)
h1b_clean <- h1b_all %>%
  filter(Total_application >= 10,         
         Continuing_application >= 10)

model <- lm(Continuing_approval_rate ~ Top_state_CA + Top_state_IL + 
            Top_state_MD + Top_state_NJ + Top_state_TX +
            Approval_rate, data = h1b_all)

pdf("figures/linear_regression_first_model_diagnostic_plots.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
plot(model, which = c(1, 2))
dev.off()

# Log-transformation
h1b_clean <- h1b_clean %>%
  filter(Continuing_approval_rate > 0 & Continuing_approval_rate < 1)

h1b_clean <- h1b_clean %>%
  mutate(logit_rate = log(Continuing_approval_rate / (1 - Continuing_approval_rate)))

model_final <- lm(logit_rate ~ Top_state_CA + Top_state_IL + 
                  Top_state_MD + Top_state_NJ + Top_state_TX +
                  Approval_rate, data = h1b_clean)

pdf("figures/linear_regression_log_model_diagnostic_plots.pdf", width = 8, height = 4)
par(mfrow = c(1, 2))
plot(model_final, which = c(1, 2))
dev.off()

# Results
summary(model_final)





