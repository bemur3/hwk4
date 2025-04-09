library(scales)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes, fixest,
               scales, gganimate, gapminder, gifski, png, tufte, plotly, OECD,
               ggrepel, survey, foreign, devtools, pdftools, kableExtra, modelsummary,
               kableExtra, broom, gridExtra, cobalt, patchwork)
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
    !(partd == "Y" & plan_type == "PDP"),
    year %in% 2010:2015
  )

# Count number of plans per county per year
county_plan_counts <- ma_filtered %>%
  group_by(year, state, county) %>%
  summarise(plan_count = n(), .groups = "drop")

# Create boxplot of plan counts by county over time
plan_counts <- ggplot(county_plan_counts, aes(x = factor(year), y = plan_count)) +
  geom_boxplot(fill = "skyblue", color = "darkblue", outlier.shape = NA) +
  labs(
    title = "Distribution of Medicare Advantage Plans per County (2007–2015)",
    x = "Year",
    y = "Number of Plans per County"
  ) +
  scale_y_continuous(limits = c(0, 50)) + 
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
  )


# 2. Star Rating Distributions

# Filter for target years and clean scores
star_subset <- ma_data %>%
  filter(year %in% c(2010, 2012, 2015), !is.na(partc_score)) %>%
  mutate(
    year = factor(year),  # use as categorical for facetting
    partc_score = factor(partc_score, levels = sort(unique(partc_score)))
  )

# Create a faceted bar plot for star rating distributions
star_rating_plot <- ggplot(star_subset, aes(x = partc_score)) +
  geom_bar(fill = "steelblue", color = "black") +
  facet_wrap(~ year, nrow = 1) +
  labs(
    title = "MA Part C Star Rating Distributions (2010, 2012, 2015)",
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

# Print the plot
print(star_rating_plot)



# 3. Average Benchmark Payment

benchmark_summary <- ma_data %>%
  filter(year %in% 2010:2015, !is.na(ma_rate)) %>%
  group_by(year) %>%
  summarize(avg_benchmark = mean(ma_rate, na.rm = TRUE))

# Plot benchmark payments
benchmark_plot <- ggplot(benchmark_summary, aes(x = year, y = avg_benchmark)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(500, 900)) +
  labs(
    title = "Average MA Benchmark Payment (2010–2015)",
    x = "Year",
    y = "Average Benchmark Payment ($)"
  ) +
  theme_minimal(base_size = 14)

print(benchmark_plot)

library(scales)

# 4. 
ma_share_summary <- ma_data %>%
  filter(year %in% 2010:2015, avg_eligibles > 0, avg_enrolled >= 0) %>%
  mutate(ma_share = avg_enrolled / avg_eligibles) %>%
  group_by(year) %>%
  summarize(avg_ma_share = mean(ma_share, na.rm = TRUE))

# Plot MA Share
ma_share_plot <- ggplot(ma_share_summary, aes(x = year, y = avg_ma_share)) +
  geom_line(color = "dodgerblue", linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Average Share of MA Enrollment (2010–2015)",
    x = "Year",
    y = "MA Share of Medicare Eligibles"
  ) +
  theme_minimal(base_size = 14)

print(ma_share_plot)


# 5. 

# Filter for 2010 and relevant columns

data.2010 <- ma_data %>%
             filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score))
colnames(data.2010)

data.2010 <- data.2010 %>%
  mutate(raw.rating=rowMeans(
    cbind(breastcancer_screen, rectalcancer_screen, cv_diab_cholscreen, glaucoma_test,
          monitoring, flu_vaccine, pn_vaccine, physical_health, mental_health,
          osteo_test, physical_monitor, primaryaccess, osteo_manage,
          diab_healthy, bloodpressure, ra_manage, copd_test, bladder,
          falling, nodelays, doctor_communicate, carequickly, customer_service,                    
          overallrating_care, overallrating_plan, complaints_plan, appeals_timely,
          appeals_review, leave_plan, audit_problems, hold_times, info_accuracy,
          ttyt_available),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw.rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type, partd) %>%
    mutate(mkt_share = avg_enrollment/avg_eligibles,
          HMO=str_detect(plan_type, "HMO"))
colnames(data.2010)

### how many were rounded up?
rating.2010 <- data.2010 %>%
  mutate(rounded_30=ifelse(raw.rating>=2.75 & raw.rating<3.00 & Star_Rating==3.0,1,0),
         rounded_35=ifelse(raw.rating>=3.25 & raw.rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40=ifelse(raw.rating>=3.75 & raw.rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45=ifelse(raw.rating>=4.25 & raw.rating<4.50 & Star_Rating==4.5,1,0),
         rounded_50=ifelse(raw.rating>=4.75 & raw.rating<5.00 & Star_Rating==5.0,1,0)) %>%
  group_by(Star_Rating) %>% filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>%
  summarize(count_30=sum(rounded_30),
            count_35=sum(rounded_35),
            count_40=sum(rounded_40),
            count_45=sum(rounded_45),
            count_50=sum(rounded_50)) %>%
  mutate(rounded.up = count_30 + count_35 + count_40 + count_45 + count_50) %>%
  select(Star_Rating, rounded.up) 

kable(rating.2010, caption="Number of Plans Rounded Up to Each Start Ratint(2010)")

# 6. 
star30 <- lm(mkt_share ~ treat +score,
             data=(data.2010 %>%
             filter(raw.rating>=(2.75-0.125),
             raw.rating<=(2.75+0.125),
             Star_Rating %in% c(2.5, 3.0)) %>%
             mutate(treat=(Star_Rating==3.0),
                    score=raw.rating-2.75)))

star35 <- lm(mkt_share ~ treat +score,
             data=(data.2010 %>%
             filter(raw.rating>=(3.25-0.125),
             raw.rating<=(3.25+0.125),
             Star_Rating %in% c(3.0, 3.5)) %>%
             mutate(treat=(Star_Rating==3.5),
                    score=raw.rating-3.25)))

# Load broom for tidy model summaries
library(broom)

# Function to extract model info
extract_model_info <- function(model, label) {
  tidy_vals <- broom::tidy(model)
  glance_vals <- broom::glance(model)

  tibble::tibble(
    `Cutoff` = label,
    `Intercept` = round(tidy_vals$estimate[tidy_vals$term == "(Intercept)"], 3),
    `Rounded Up` = round(tidy_vals$estimate[tidy_vals$term == "treatTRUE"], 3),
    `Running Score` = round(tidy_vals$estimate[tidy_vals$term == "score"], 3),
    `Num Obs` = glance_vals$nobs,
    `R-squared` = round(glance_vals$r.squared, 3),
    `RMSE` = round(sqrt(glance_vals$sigma^2), 3)
  )
}

# Apply to both models
rd_table_30 <- extract_model_info(star30, "3 vs 2.5 Stars")
rd_table_35 <- extract_model_info(star35, "3.5 vs 3 Stars")

# Combine
rd_summary_table <- dplyr::bind_rows(rd_table_30, rd_table_35)

# Display
knitr::kable(rd_summary_table, caption = "RDD Regression Summary (Bandwidth = 0.125)")



# 7.

# Define bandwidths and cutoff points
bandwidths <- c(0.10, 0.12, 0.13, 0.14, 0.15)
cutoffs <- c(2.75, 3.25)
labels <- c("3 vs 2.5 Stars", "3.5 vs 3 Stars")

# Function to estimate model and return treatment effect
run_rd_model <- function(data, cutoff, bandwidth, label) {
  lower <- cutoff - bandwidth
  upper <- cutoff + bandwidth
  treat_val <- ifelse(cutoff == 2.75, 3.0, 3.5)
  control_val <- ifelse(cutoff == 2.75, 2.5, 3.0)
  
  df <- data %>%
    filter(raw.rating >= lower, raw.rating <= upper,
           Star_Rating %in% c(control_val, treat_val)) %>%
    mutate(treat = Star_Rating == treat_val,
           score = raw.rating - cutoff)
  
  model <- lm(mkt_share ~ treat + score, data = df)
  tidy_model <- tidy(model)
  glance_model <- glance(model)
  
  tibble(
    Cutoff = label,
    Bandwidth = bandwidth,
    Estimate = tidy_model$estimate[tidy_model$term == "treatTRUE"],
    SE = tidy_model$std.error[tidy_model$term == "treatTRUE"],
    N = glance_model$nobs,
    R2 = glance_model$r.squared
  )
}

# Run models for all bandwidths and cutoffs
results <- purrr::map2_dfr(
  rep(cutoffs, each = length(bandwidths)),
  rep(labels, each = length(bandwidths)),
  ~ {
    map_dfr(bandwidths, function(bw) {
      run_rd_model(data.2010, .x, bw, .y)
    })
  }
)

# Plot results
effect_plot <- ggplot(results, aes(x = Bandwidth, y = Estimate, color = Cutoff)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * SE,
                    ymax = Estimate + 1.96 * SE), width = 0.005) +
  labs(
    title = "RDD Treatment Effect Estimates Across Bandwidths",
    x = "Bandwidth",
    y = "Estimated Treatment Effect on Market Share"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("3 vs 2.5 Stars" = "darkblue", "3.5 vs 3 Stars" = "firebrick"))

print(effect_plot)

# 8. 

library(gridExtra)

# Subset data around each cutoff
cutoff_3 <- data.2010 %>% filter(raw.rating >= 2.75 & raw.rating < 3.25)
cutoff_35 <- data.2010 %>% filter(raw.rating >= 3.25 & raw.rating < 3.75)

# Plot around 3.0 cutoff
plot_3 <- ggplot(cutoff_3, aes(x = raw.rating)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = 3.0, linetype = "dashed", color = "red") +
  labs(title = "(a) Around 3.0 Cutoff", x = "Running Variable", y = "Density") +
  theme_minimal(base_size = 14)

# Plot around 3.5 cutoff
plot_35 <- ggplot(cutoff_35, aes(x = raw.rating)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red") +
  labs(title = "(b) Around 3.5 Cutoff", x = "Running Variable", y = "Density") +
  theme_minimal(base_size = 14)

# Combine the plots side-by-side
grid.arrange(plot_3, plot_35, ncol = 2, top = "Density of the Running Variable Near RD Thresholds")

# 9. 

lp.vars <- data.2010 %>% ungroup() %>%
  filter( (raw.rating >=2.75-.125 & Star_Rating==2.5) |
          (raw.rating >=2.75-.125 & Star_Rating==3.0) ) %>%
          mutate(rounded=(Star_Rating==3.0)) %>%
          select(HMO, partd, rounded) %>%
          filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.30 <- love.plot(bal.tab(lp.covs,treat=lp.vars$rounded), colors="black", shapes="circle",threshold=0.1) +
  theme_bw() + theme(legend.position="none") 

lp.vars <- data.2010 %>% ungroup() %>%
  filter( (raw.rating >=3.25-.125 & Star_Rating==3.0) |
          (raw.rating >=3.25-.125 & Star_Rating==3.5)) %>%
          mutate(rounded=(Star_Rating==3.5)) %>%
          select(HMO, partd, rounded) %>%
          filter(complete.cases(.))
     
lp.covs <- lp.vars %>% select(HMO, partd)

plot.35 <- love.plot(bal.tab(lp.covs,treat=lp.vars$rounded), colors="black", shapes="circle",threshold=0.1) +
  theme_bw() + theme(legend.position="none") 

# Combine Plots
combined_plot <- plot.30 + plot.35 + plot_layout(ncol = 2)

# Save Output
ggsave("results/love_plot_cutoffs_combined.png", plot = combined_plot, width = 12, height = 5, dpi = 300)

rm(list=c("ma_filtered", "county_plan_counts", "star_subset", "benchmark_summary", 
   "ma_share_summary", "cor_data", "data.2010", 
   "cutoffs", "bandwidths", "labels", "run_rd_model", 
   "cutoff_3", "cutoff_35"))
   
save.image("submission2/Hwk4_workspace.RData")
