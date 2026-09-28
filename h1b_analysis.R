library(ggplot2)
library(dplyr)
library(tidyr)

h1b_2021 = read.csv("project/data/h-1b-data-export-2021.csv")
h1b_2022 = read.csv("project/data/h-1b-data-export-2022.csv")
h1b_2023 = read.csv("project/data/h-1b-data-export-2023.csv")

h1b_all <- bind_rows(
  h1b_2021 %>% mutate(year = 2021),
  h1b_2022 %>% mutate(year = 2022),
  h1b_2023 %>% mutate(year = 2023)
) %>%
  filter(!is.na(State), State != "")

state_counts <- h1b_all %>%
  count(year, State) %>%
  group_by(year) %>%
  slice_max(n, n = 10) # top 10 states per year for readability

ggplot(state_counts, aes(x = reorder(State, n), y = n, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 1, scales = "free_y") +
  coord_flip() +
  labs(x = "State", y = "Applications") +
  theme_minimal(base_size = 12) +  # sets all text to size 12
  theme(axis.text.y = element_text(size = 12))  # override the shrunk state labels too

ggsave("project/figures/h1b_by_top_10_state.png", width = 10, height = 16, dpi = 300)


categories <- list(
  list(col = "Initial.Approval",    label = "Initial Approvals",    filename = "h1b_initial_approval_by_top_10_employer.png"),
  list(col = "Initial.Denial",      label = "Initial Denials",      filename = "h1b_initial_denial_by_top_10_employer.png"),
  list(col = "Continuing.Approval", label = "Continuing Approvals", filename = "h1b_continuing_approval_by_top_10_employer.png"),
  list(col = "Continuing.Denial",   label = "Continuing Denials",   filename = "h1b_continuing_denial_by_top_10_employer.png")
)

for (cat in categories) {
  
  df <- h1b_all %>%
    group_by(year, Employer) %>%
    summarise(total = sum(.data[[cat$col]], na.rm = TRUE), .groups = "drop") %>%
    filter(total > 0) %>%
    group_by(year) %>%
    slice_max(total, n = 10) %>% # top 10 employer per year for readability
    ungroup()
  
  p <- ggplot(df, aes(x = reorder(Employer, total), y = total, fill = factor(year))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~year, ncol = 1, scales = "free_y") +
    coord_flip() +
    labs(x = cat$label, y = "Count") +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_text(size = 12))
  
  ggsave(paste0("project/figures/", cat$filename), p, width = 10, height = 16, dpi = 300)
  
  message("Saved: ", cat$filename)
}


h1b_all %>%
  select(year, `Initial.Approval`, `Initial.Denial`, 
         `Continuing.Approval`, `Continuing.Denial`) %>%
  pivot_longer(cols = -year, names_to = "category", values_to = "count") %>%
  filter(!is.na(count)) %>%
  group_by(year, category) %>%
  summarise(
    mean   = round(mean(count), 2),
    median = median(count),
    sd     = round(sd(count), 2),
    min    = min(count),
    max    = max(count),
    total  = sum(count),
    .groups = "drop"
  ) %>%
  print(n = Inf)




