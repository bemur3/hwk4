library(scales)

# Load cleaned Medicare Advantage plan-level data
ma_data <- read_rds("data/output/full_ma_data.rds")

# 1. 

# Filter out:
# - SNPs (special needs plans)
# - 800-series plans
# - Prescription Drug Plans (PDPs) that do not offer Part C benefits
ma_filtered <- ma_data %>%
  filter(
    snp != "Y",
    planid < 800,
    !(partd == "Y" & plan_type == "PDP")
  )

# Count number of plans per county per year
county_plan_counts <- ma_filtered %>%
  group_by(year, state, county) %>%
  summarise(plan_count = n(), .groups = "drop")

# Create boxplot of plan counts by county over time
plan_counts <- ggplot(county_plan_counts, aes(x = factor(year), y = plan_count)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Distribution of Medicare Advantage Plans per County (2007–2015)",
    x = "Year",
    y = "Number of Plans per County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )
ggsave("plan_counts.png", plot = plan_counts, width = 8, height = 5)

# 2.

# Load cleaned star ratings
star_data <- read_rds("data/output/star_ratings.rds")

# Filter for target years and clean scores
# Filter and prep the data
star_subset <- star_data %>%
  filter(year %in% c(2010, 2012, 2015), !is.na(partc_score)) %>%
  mutate(
    year = as.integer(year),  # make sure year is numeric
    partc_score = factor(partc_score, levels = sort(unique(partc_score)))
  )

# Function to generate and save each plot
save_star_plot <- function(data, year_val) {
  filtered <- data %>% filter(year == year_val)

  message("Saving star rating plot for ", year_val, " (", nrow(filtered), " plans)")

  star_plot <- ggplot(filtered, aes(x = partc_score)) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(
      title = paste("MA Part C Star Rating Distribution -", year_val),
      x = "Star Rating",
      y = "Number of Plans"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_line(color = "gray90")
    )

  ggsave(
    filename = paste0("results/star_rating_", year_val, ".png"),
    plot = star_plot,
    width = 8,
    height = 5,
    dpi = 300,
    bg = "white"
  )
}

# Save each plot
save_star_plot(star_subset, 2010)
save_star_plot(star_subset, 2012)
save_star_plot(star_subset, 2015)

# 3. 
# Load benchmark data
benchmark <- read_rds("data/output/ma_benchmark.rds")

# Keep 2010–2015 and extract the right benchmark per year
bench_clean <- benchmark %>%
  filter(year %in% 2010:2015) %>%
  mutate(
    benchmark_payment = case_when(
      year %in% 2010:2011 ~ risk_ab,
      year %in% 2012:2014 ~ risk_star5,
      year == 2015 ~ risk_bonus5
    )
  ) %>%
  filter(!is.na(benchmark_payment))  # remove missing

# Calculate average by year
avg_benchmark <- bench_clean %>%
  group_by(year) %>%
  summarize(avg_payment = mean(benchmark_payment, na.rm = TRUE))

# Plot it
benchmark_plot <- ggplot(avg_benchmark, aes(x = year, y = avg_payment)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Average MA Benchmark Payment (2010–2015)",
    x = "Year",
    y = "Average Benchmark Payment ($)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  )

# Save it
ggsave(
  filename = "results/avg_benchmark_payment_2010_2015.png",
  plot = benchmark_plot,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# How much has the average benchmark payment risen over the years?
round(100 * (last(avg_benchmark$avg_payment) - first(avg_benchmark$avg_payment)) / first(avg_benchmark$avg_payment), 2)

#4. 

# Load data
penetration <- read_rds("data/output/ma_penetration.rds")
benchmark <- read_rds("data/output/ma_benchmark.rds")

# Prepare Medicare Advantage share data
ma_share_data <- penetration %>%
  filter(year %in% 2010:2015, avg_eligibles > 0) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles)

# Prepare benchmark data
benchmark_clean <- benchmark %>%
  filter(year %in% 2010:2015) %>%
  mutate(
    benchmark_payment = case_when(
      year %in% 2010:2011 ~ risk_ab,
      year %in% 2012:2014 ~ risk_star5,
      year == 2015 ~ risk_bonus5
    )
  ) %>%
  select(ssa, year, benchmark_payment)

# Fix ssa types for joining
ma_share_data <- ma_share_data %>%
  mutate(ssa = as.character(ssa))

benchmark_clean <- benchmark_clean %>%
  mutate(ssa = as.character(ssa))

# Join and filter
combined <- ma_share_data %>%
  left_join(benchmark_clean, by = c("ssa", "year")) %>%
  filter(!is.na(ma_share), !is.na(benchmark_payment))

# Average MA share over time
avg_share <- ma_share_data %>%
  group_by(year) %>%
  summarize(avg_ma_share = mean(ma_share, na.rm = TRUE))

# Plot and save average MA share
ma_plot <- ggplot(avg_share, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "dodgerblue", linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Average Share of Medicare Advantage Enrollment (2010–2015)",
    x = "Year",
    y = "MA Share of Medicare Eligibles"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "results/avg_ma_share_2010_2015.png",
  plot = ma_plot,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Correlation
cor_test <- cor.test(combined$ma_share, combined$benchmark_payment)
print(cor_test)

# 5.
library(knitr)

# Prepare 2010 data
data_2010 <- ma_data %>%
  filter(year == 2010) %>%
  left_join(
    star_data %>% filter(year == 2010) %>% select(contractid, year, partc_score),
    by = c("contractid", "year")
  ) %>%
  left_join(
    penetration %>% filter(year == 2010) %>% select(ssa, avg_enrolled),
    by = "ssa"
  ) %>%
  filter(!is.na(partc_score), !is.na(avg_enrolled))

# Filter and count rounded star ratings in 2010
rounded_table <- data_2010 %>%
  filter(partc_score %in% c(3, 3.5, 4, 4.5, 5)) %>%
  count(partc_score) %>%
  arrange(partc_score)

colnames(rounded_table) <- c("Rounded Rating", "Number of Plans")
kable(rounded_table, caption = "Number of Plans Rounded to Each Star Rating")