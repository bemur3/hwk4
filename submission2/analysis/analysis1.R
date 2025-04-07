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
ggsave("plan_counts.png", plot = plan_counts, width = 8, height = 5)

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
         bid, avg_ffscost, ma_rate, plan_type) %>%
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
# 7.

rm(list=c("ma_data", "ma_filtered", "county_plan_counts", "cor_data", "rating_2010", "rd_data", "rd_3", "rd_35", "rd_results", "star_rd_data", "run_rdrobust", "cutoffs", "bandwidths", "grid", "results", "results_clean"))
save.image("submission1/Hmwk4_workspace.RData")
