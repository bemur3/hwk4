library(scales)

# Load merged and cleaned Medicare Advantage data
ma_data <- read_rds("data/output/final_ma_data.rds")

# 1. Plan Count Distribution

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

## 2. Star Rating Distributions

# Filter for target years and clean scores
star_subset <- ma_data %>%
  filter(year %in% c(2010, 2012, 2015), !is.na(partc_score)) %>%
  mutate(
    year = as.integer(year),
    partc_score = factor(partc_score, levels = sort(unique(partc_score)))
  )

# 2010
star_rating_2010 <- star_subset %>%
  filter(year == 2010) %>%
  ggplot(aes(x = partc_score)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "MA Part C Star Rating Distribution - 2010",
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

# 2012
star_rating_2012 <- star_subset %>%
  filter(year == 2012) %>%
  ggplot(aes(x = partc_score)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "MA Part C Star Rating Distribution - 2012",
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

# 2015
star_rating_2015 <- star_subset %>%
  filter(year == 2015) %>%
  ggplot(aes(x = partc_score)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "MA Part C Star Rating Distribution - 2015",
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
print(star_rating_2010)
print(star_rating_2012)
print(star_rating_2015)


# 3. Average Benchmark Payment

benchmark_summary <- ma_data %>%
  filter(year %in% 2010:2015, !is.na(ma_rate)) %>%
  group_by(year) %>%
  summarize(avg_benchmark = mean(ma_rate, na.rm = TRUE))

# Plot benchmark payments
benchmark_plot <- ggplot(benchmark_summary, aes(x = year, y = avg_benchmark)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Average MA Benchmark Payment (2010–2015)",
    x = "Year",
    y = "Average Benchmark Payment ($)"
  ) +
  theme_minimal(base_size = 14)

ggsave("results/avg_benchmark_payment_2010_2015.png", plot = benchmark_plot, width = 8, height = 5, dpi = 300)


# 4. 
ma_share_summary <- ma_data %>%
  filter(year %in% 2010:2015, avg_eligibles > 0, avg_enrolled >= 0) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles) %>%
  group_by(year) %>%
  summarize(avg_ma_share = mean(ma_share, na.rm = TRUE))

# Plot MA Share
ma_plot <- ggplot(ma_share_summary, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "dodgerblue", linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Average Share of MA Enrollment (2010–2015)",
    x = "Year",
    y = "MA Share of Medicare Eligibles"
  ) +
  theme_minimal(base_size = 14)

ggsave("results/avg_ma_share_2010_2015.png", plot = ma_plot, width = 8, height = 5, dpi = 300)



cor_data <- ma_data %>%
  filter(year %in% 2010:2015, avg_eligibles > 0, avg_enrolled >= 0, !is.na(ma_rate)) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles)

cor_result <- cor.test(cor_data$ma_share, cor_data$ma_rate)
print(cor_result)

# 5. 

# Filter for 2010 and relevant columns
rating_2010 <- ma_data %>%
  filter(year == 2010, !is.na(partc_score)) %>%
  mutate(rounded_score = round(partc_score * 2) / 2) %>%
  count(rounded_score) %>%
  filter(rounded_score %in% c(3, 3.5, 4, 4.5, 5)) %>%
  rename(`Rounded Star Rating` = rounded_score,
         `Number of Plans` = n)

# Display table
kable(rating_2010, caption = "Number of Plans Rounded to Each Star Rating in 2010")


# 6. 

library(rdrobust)

# Filter to 2010 data with relevant variables
rd_data <- ma_data %>%
  filter(year == 2010, !is.na(partc_score), !is.na(avg_enrollment))

# Define function for comparing cutoff groups directly
estimate_rd_discrete <- function(data, below_score, above_score) {
  below <- data %>% filter(partc_score == below_score)
  above <- data %>% filter(partc_score == above_score)

  effect <- mean(above$avg_enrollment, na.rm = TRUE) - mean(below$avg_enrollment, na.rm = TRUE)
  se <- sqrt(var(above$avg_enrollment, na.rm = TRUE)/nrow(above) +
             var(below$avg_enrollment, na.rm = TRUE)/nrow(below))

  t_stat <- effect / se
  p_val <- 2 * (1 - pt(abs(t_stat), df = min(nrow(above), nrow(below)) - 1))

  tibble(
    `Cutoff` = paste0(above_score, " vs ", below_score, " Stars"),
    `Estimate` = effect,
    `Std. Error` = se,
    `P-value` = p_val
  )
}

# Estimate effects at 3.0 and 3.5 star thresholds
rd_3 <- estimate_rd_discrete(rd_data, below_score = 2.5, above_score = 3.0)
rd_35 <- estimate_rd_discrete(rd_data, below_score = 3.0, above_score = 3.5)

# Combine and print
rd_results <- bind_rows(rd_3, rd_35)
print(rd_results)

# 7.

library(ggplot2)
# Filter and prepare the data for RD analysis
star_rd_data <- ma_data %>%
  filter(year == 2010, !is.na(partc_score), !is.na(avg_enrollment)) %>%
  select(partc_score, avg_enrollment)

# Function to run rdrobust and return a clean tibble
run_rdrobust <- function(cutoff, bandwidth) {
  data <- star_rd_data %>%
    filter(partc_score >= (cutoff - bandwidth), partc_score <= (cutoff + bandwidth))

  if (nrow(data) < 50) {
    return(tibble(
      cutoff = cutoff,
      bandwidth = bandwidth,
      estimate = NA,
      se = NA
    ))
  }

  model <- tryCatch(
    rdrobust(
      y = data$avg_enrollment,
      x = data$partc_score,
      c = cutoff,
      h = bandwidth
    ),
    error = function(e) NULL
  )

  if (is.null(model)) {
    return(tibble(
      cutoff = cutoff,
      bandwidth = bandwidth,
      estimate = NA,
      se = NA
    ))
  }

  tibble(
    cutoff = cutoff,
    bandwidth = bandwidth,
    estimate = model$coef[1],
    se = model$se[1]
  )
}

# Grid of cutoffs and bandwidths
cutoffs <- c(3, 3.5)
bandwidths <- c(0.10, 0.12, 0.13, 0.14, 0.15)
grid <- expand.grid(cutoff = cutoffs, bandwidth = bandwidths)

# Run all combinations
results <- purrr::pmap_dfr(grid, function(cutoff, bandwidth) {
  run_rdrobust(cutoff, bandwidth)
})

# Add readable labels for plotting
results <- results %>%
  mutate(
    cutoff_label = case_when(
      cutoff == 3 ~ "3 vs 2.5 Stars",
      cutoff == 3.5 ~ "3.5 vs 3 Stars"
    )
  )

# Filter for valid estimates only
results_clean <- results %>% filter(!is.na(estimate))

# Plot the results
bandwidth_plot <- ggplot(results_clean, aes(x = bandwidth, y = estimate, color = cutoff_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.005) +
  labs(
    title = "Sensitivity of RD Estimates to Bandwidth Choice",
    x = "Bandwidth",
    y = "Estimated Effect on Enrollment",
    color = "Rating Cutoff"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "top"
  )

# Save the plot
ggsave("results/rd_sensitivity_bandwidths.png", plot = bandwidth_plot, width = 9, height = 6, dpi = 300, bg = "white")

rm(list=c("ma_data", "ma_filtered", "county_plan_counts", "cor_data", "rating_2010", "rd_data", "rd_3", "rd_35", "rd_results", "star_rd_data", "run_rdrobust", "cutoffs", "bandwidths", "grid", "results", "results_clean"))
save.image("submission1/Hmwk4_workspace.RData")
