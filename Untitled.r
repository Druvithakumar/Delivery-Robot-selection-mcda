# ================================================================
# PART 2: ROBOT SELECTION - DATA UNDERSTANDING (FIXED)
# ================================================================

cat("\n=== LOADING AND FIXING DATA ===\n")

# ----------------------------------------------------------------
# FIX 1: Load and Transpose Robot Data
# ----------------------------------------------------------------

# Load the CSV
robots_raw <- read.csv("/Users/druvitha/Desktop/Bussiness Analytics and Decision science Course Work/Part-2/Robot_Info.csv", 
                       header = TRUE)

# Display raw structure
cat("\n--- Raw Data Structure ---\n")
print(robots_raw)

# Transpose the data (flip rows and columns)
# First column becomes row names, rest becomes data
robots <- as.data.frame(t(robots_raw[, -1]))  # Remove first column, transpose
colnames(robots) <- robots_raw[, 1]           # Use first column as column names
robots$Robot <- rownames(robots)              # Add Robot names as a column
rownames(robots) <- NULL                      # Remove row names

# Reorder columns (Robot name first)
robots <- robots[, c("Robot", "Carrying Capacity", "Battery Size", "Speed", 
                     "Mobility", "Aesthetic", "Cost Per Unit", "Reliability")]

cat("\n--- Fixed Robot Data Structure ---\n")
print(robots)
str(robots)

# Convert all criteria columns to numeric (they're currently character)
robots[, 2:8] <- lapply(robots[, 2:8], as.numeric)

cat("\n--- After Converting to Numeric ---\n")
str(robots)
summary(robots)

# ----------------------------------------------------------------
# FIX 2: Load Management Priorities Correctly
# ----------------------------------------------------------------

library(readxl)

# Try reading from row 4 (skip header rows)
priorities <- read_excel("/Users/druvitha/Desktop/Bussiness Analytics and Decision science Course Work/Part-2/Management_Priority.xlsx", 
                         skip = 3)  # Skip first 3 rows

cat("\n--- Management Priorities ---\n")
print(priorities)

# Clean up column names
colnames(priorities) <- c("Criterion", "Importance_Description")

cat("\n--- Cleaned Priorities ---\n")
print(priorities)

# ----------------------------------------------------------------
# DATA UNDERSTANDING: Summary Statistics
# ----------------------------------------------------------------

cat("\n=== DESCRIPTIVE STATISTICS FOR EACH CRITERION ===\n")

for(i in 2:8) {
  criterion_name <- colnames(robots)[i]
  cat("\n---", criterion_name, "---\n")
  cat("Mean:", mean(robots[, i]), "\n")
  cat("Median:", median(robots[, i]), "\n")
  cat("Min:", min(robots[, i]), "\n")
  cat("Max:", max(robots[, i]), "\n")
  cat("SD:", sd(robots[, i]), "\n")
}

# ----------------------------------------------------------------
# IDENTIFY CRITERION DIRECTION
# ----------------------------------------------------------------

cat("\n=== CRITERION DIRECTIONS ===\n")
cat("Higher is Better:\n")
cat("  - Carrying Capacity\n")
cat("  - Battery Size\n")
cat("  - Speed\n")
cat("  - Mobility\n")
cat("  - Aesthetic\n")
cat("  - Reliability\n")
cat("\nLower is Better:\n")
cat("  - Cost Per Unit\n")

# ----------------------------------------------------------------
# CHECK FOR MISSING DATA
# ----------------------------------------------------------------

cat("\n=== MISSING DATA CHECK ===\n")
cat("Missing values in robot data:\n")
print(colSums(is.na(robots)))

cat("\n=== DATA UNDERSTANDING COMPLETE ===\n")

# ============================================================================
# ROBOT PROTOTYPE ANALYSIS - THREE PROFESSIONAL VISUALIZATIONS
# Business Analytics Assessment
# ============================================================================
install.packages(c("ggplot2", "reshape2", "scales", "gridExtra"))

# Load required libraries
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)

# Load and prepare data (assuming you already have the 'robots' dataframe)
# If not, uncomment and run your data loading code first

# ============================================================================
# VISUALIZATION 1: COST-CAPACITY SCATTER PLOT
# ============================================================================

cat("\n=== Creating Figure 1: Cost-Capacity Analysis ===\n")

# Create the scatter plot
fig1 <- ggplot(robots, aes(x = `Carrying Capacity`, y = `Cost Per Unit`)) +
  geom_point(size = 4, color = "#2563eb", alpha = 0.7) +
  geom_text(aes(label = Robot), 
            vjust = -1, 
            hjust = 0.5, 
            size = 3.5, 
            fontface = "bold",
            color = "#1e40af") +
  labs(
    title = "Figure 1: Cost Efficiency Analysis",
    subtitle = "Trade-off between carrying capacity and unit cost",
    x = "Carrying Capacity (Litres)",
    y = "Cost per Unit (£)",
    caption = "Note: Lower-right quadrant represents optimal value proposition"
  ) +
  scale_y_continuous(labels = comma, limits = c(4000, 10500)) +
  scale_x_continuous(limits = c(30, 75)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5)
  )

# Display the plot
print(fig1)

# Save as high-quality image
ggsave("Figure1_Cost_Capacity_Analysis.png", 
       plot = fig1, 
       width = 10, 
       height = 6, 
       dpi = 300)

cat("✓ Figure 1 saved as 'Figure1_Cost_Capacity_Analysis.png'\n")

# ============================================================================
# VISUALIZATION 2: RELIABILITY RANKING BAR CHART
# ============================================================================

cat("\n=== Creating Figure 2: Reliability Ranking ===\n")

# Sort by reliability (descending)
robots_sorted <- robots[order(-robots$Reliability), ]
robots_sorted$Robot <- factor(robots_sorted$Robot, levels = robots_sorted$Robot)

# Create bar chart
fig2 <- ggplot(robots_sorted, aes(x = Robot, y = Reliability, fill = Robot)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = Reliability), 
            vjust = -0.5, 
            size = 3.5, 
            fontface = "bold") +
  labs(
    title = "Figure 2: Reliability Performance Ranking",
    subtitle = "Days between breakdowns by prototype (higher is better)",
    x = "Robot Prototype",
    y = "Days Between Breakdowns",
    caption = "Note: Gamma leads with 35 days (133% better than Alpha's 15 days)"
  ) +
  scale_fill_manual(values = c("#16a34a", "#ea580c", "#dc2626", "#ca8a04", 
                               "#db2777", "#7c3aed", "#2563eb")) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5)
  )

# Display the plot
print(fig2)

# Save as high-quality image
ggsave("Figure2_Reliability_Ranking.png", 
       plot = fig2, 
       width = 10, 
       height = 6, 
       dpi = 300)

cat("✓ Figure 2 saved as 'Figure2_Reliability_Ranking.png'\n")

# ============================================================================
# OPTIONAL: CREATE A COMBINED FIGURE WITH ALL THREE PLOTS
# ============================================================================

cat("\n=== Creating Combined Figure (Optional) ===\n")

# Combine all three plots
combined_plot <- grid.arrange(fig1, fig2, fig3, ncol = 1)

# Save combined figure
ggsave("Combined_Analysis_Figures.png", 
       plot = combined_plot, 
       width = 10, 
       height = 18, 
       dpi = 300)

cat("✓ Combined figure saved as 'Combined_Analysis_Figures.png'\n")

# ============================================================================
# SUMMARY STATISTICS FOR REPORT
# ============================================================================

cat("\n=== Key Insights Summary ===\n")
cat("\nFigure 1 Insights:\n")
cat("  • Best Value: Foxtrot (70L capacity at £7,500)\n")
cat("  • Cost Leader: Bravo (£4,500 with 50L capacity)\n")
cat("  • Premium Option: Alpha (£10,000 with only 35L)\n")

cat("\nFigure 2 Insights:\n")
cat("  • Most Reliable: Gamma (35 days between breakdowns)\n")
cat("  • Least Reliable: Alpha (15 days - 133% worse than Gamma)\n")
cat("  • Top Tier: Gamma, Delta, Echo, Foxtrot (30-35 days)\n")

cat("\nFigure 3 Insights:\n")
cat("  • Balanced Performer: Gamma (high scores across multiple criteria)\n")
cat("  • Specialist: Foxtrot (capacity leader, mobility weakness)\n")
cat("  • Speed Champion: Delta (25 km/h - 67% faster than base models)\n")

cat("\n=== All visualizations complete! ===\n")
cat("Files saved:\n")
cat("  1. Figure1_Cost_Capacity_Analysis.png\n")
cat("  2. Figure2_Reliability_Ranking.png\n")
cat("  3. Figure3_Performance_Heatmap.png\n")
cat("  4. Combined_Analysis_Figures.png (optional)\n")


# ================================================================
# PHASE 3: DATA PREPARATION - NORMALIZATION
# ================================================================

cat("\n========================================\n")
cat("PHASE 3: DATA PREPARATION\n")
cat("========================================\n")

cat("\n=== STEP 1: NORMALIZING DATA TO 0-1 SCALE ===\n")

# Create a copy for normalization
robots_norm <- robots

# Normalize each criterion to 0-1 scale
for(i in 2:8) {
  criterion_name <- colnames(robots)[i]
  
  if(criterion_name == "Cost Per Unit") {
    # REVERSE normalization: Lower cost is better
    # Formula: (max - value) / (max - min)
    robots_norm[, i] <- (max(robots[, i]) - robots[, i]) / 
      (max(robots[, i]) - min(robots[, i]))
    cat(criterion_name, ": Normalized (REVERSE - lower is better)\n")
  } else {
    # NORMAL normalization: Higher is better
    # Formula: (value - min) / (max - min)
    robots_norm[, i] <- (robots[, i] - min(robots[, i])) / 
      (max(robots[, i]) - min(robots[, i]))
    cat(criterion_name, ": Normalized (higher is better)\n")
  }
}

cat("\n--- Normalized Robot Data (0-1 scale) ---\n")
robots_norm_print <- robots_norm
robots_norm_print[ , 2:8] <- round(robots_norm_print[ , 2:8], 3)

print(robots_norm_print)
# Summary of normalized data
cat("\n--- Normalized Data Summary ---\n")
summary(robots_norm[, 2:8])

# ================================================================
# VERIFICATION: Check normalization ranges
# ================================================================

cat("\n=== STEP 2: VERIFICATION OF NORMALIZATION ===\n")

for(i in 2:8) {
  criterion <- colnames(robots_norm)[i]
  cat(criterion, "- Min:", round(min(robots_norm[, i]), 3), 
      ", Max:", round(max(robots_norm[, i]), 3), "\n")
}

cat("\nAll values should be between 0 and 1 ✓\n")

# ================================================================
# PHASE 4: MODELING - BUSINESS PLAN 1
# ================================================================

cat("\n=== STEP 3: DEFINE WEIGHTS (MANAGEMENT PRIORITIES) ===\n")

# Based on management priorities from Excel file:
# 1. Carrying Capacity: "Most important"
# 2. Cost Per Unit: "Second most important"
# 3. Battery Size: "More important than reliability, but not as much as cost"
# 4. Reliability: "Slightly more important than aesthetic"
# 5. Speed: "As important as mobility"
# 6. Mobility: "Rated 6 out of 10, equal to speed"
# 7. Aesthetic: "Least important"

weights_plan1 <- c(
  Carrying_Capacity = 0.24,   # 25% - Most important
  Battery_Size = 0.17,         # 18% - More than reliability
  Speed = 0.10,                # 11% - Equal to mobility
  Mobility = 0.10,             # 11% - Equal to speed
  Aesthetic = 0.04,            # 0% - Least important (excluded)
  Cost_Per_Unit = 0.20,        # 20% - Second most important
  Reliability = 0.15           # 15% - Slightly more than aesthetic
)
sum(weights_plan1)
cat("\n--- Weight Distribution for Business Plan 1 ---\n")
weight_df <- data.frame(
  Criterion = names(weights_plan1),
  Weight = weights_plan1,
  Percentage = paste0(weights_plan1 * 100, "%")
)
print(weight_df)

cat("\nTotal Weight:", sum(weights_plan1), "(must equal 1.00)\n")

# ================================================================
# STEP 4: CALCULATE WEIGHTED SCORES
# ================================================================

cat("\n=== STEP 4: CALCULATING WEIGHTED SCORES ===\n")

# Calculate weighted score for each robot
robots_norm$Score_Plan1 <- 
  robots_norm$`Carrying Capacity` * weights_plan1["Carrying_Capacity"] +
  robots_norm$`Battery Size` * weights_plan1["Battery_Size"] +
  robots_norm$Speed * weights_plan1["Speed"] +
  robots_norm$Mobility * weights_plan1["Mobility"] +
  robots_norm$Aesthetic * weights_plan1["Aesthetic"] +
  robots_norm$`Cost Per Unit` * weights_plan1["Cost_Per_Unit"] +
  robots_norm$Reliability * weights_plan1["Reliability"]

cat("Weighted scores calculated for all robots.\n")

# ================================================================
# STEP 5: RANK ROBOTS
# ================================================================

cat("\n=== STEP 5: RANKING ROBOTS ===\n")

# Rank robots (1 = best, 7 = worst)
robots_norm$Rank_Plan1 <- rank(-robots_norm$Score_Plan1, ties.method = "min")

# Create results table
results_plan1 <- robots_norm[, c("Robot", "Score_Plan1", "Rank_Plan1")]
results_plan1 <- results_plan1[order(results_plan1$Rank_Plan1), ]

cat("\n--- Final Rankings for Business Plan 1 ---\n")
print(results_plan1)

# ================================================================
# STEP 6: DETAILED BREAKDOWN OF TOP ROBOT
# ================================================================

cat("\n=== STEP 6: DETAILED ANALYSIS OF TOP ROBOT ===\n")

# Get top robot
top_robot <- results_plan1$Robot[1]
top_score <- results_plan1$Score_Plan1[1]

cat("\n*** RECOMMENDATION FOR BUSINESS PLAN 1 ***\n")
cat("Selected Robot:", top_robot, "\n")
cat("Overall Score:", round(top_score, 3), "out of 1.000\n")

# Show original values for top robot
cat("\n--- Original Performance Metrics ---\n")
top_robot_original <- robots[robots$Robot == top_robot, ]
print(top_robot_original)

# Show normalized values for top robot
cat("\n--- Normalized Scores (0-1 scale) ---\n")
top_robot_norm <- robots_norm[robots_norm$Robot == top_robot, 2:8]
print(round(top_robot_norm, 3))

# Calculate contribution of each criterion to final score
cat("\n--- Contribution Breakdown ---\n")
contributions <- data.frame(
  Criterion = c("Carrying Capacity", "Battery Size", "Speed", 
                "Mobility", "Aesthetic", "Cost Per Unit", "Reliability"),
  Normalized_Score = round(as.numeric(top_robot_norm[1, ]), 3),
  Weight = weights_plan1,
  Contribution = round(as.numeric(top_robot_norm[1, ]) * weights_plan1, 3)
)
print(contributions)
cat("\nTotal Score:", round(sum(contributions$Contribution), 3), "\n")

# ================================================================
# STEP 7: COMPARE TOP 3 ROBOTS
# ================================================================

cat("\n=== STEP 7: COMPARISON OF TOP 3 ROBOTS ===\n")

top3_robots <- results_plan1$Robot[1:3]
top3_comparison <- robots[robots$Robot %in% top3_robots, ]

cat("\n--- Original Values Comparison ---\n")
print(top3_comparison)

cat("\n--- Normalized Scores Comparison ---\n")
top3_norm_print <- top3_norm
top3_norm_print[ , 2:10] <- round(top3_norm_print[ , 2:10], 3)

print(top3_norm_print)


# ================================================================
# STEP 8: VISUALIZE RESULTS
# ================================================================

cat("\n=== STEP 8: CREATING VISUALIZATIONS ===\n")

# Bar plot of scores
barplot(results_plan1$Score_Plan1,
        names.arg = results_plan1$Robot,
        main = "Business Plan 1: Robot Scores",
        xlab = "Robot Prototype",
        ylab = "Weighted Score",
        col = c("darkgreen", "green", "lightgreen", 
                "yellow", "orange", "coral", "red"),
        border = "black",
        ylim = c(0, 1))

# Add score labels on bars
text(x = barplot(results_plan1$Score_Plan1, plot = FALSE),
     y = results_plan1$Score_Plan1,
     labels = round(results_plan1$Score_Plan1, 3),
     pos = 3, cex = 0.8)

# Add horizontal line at mean
abline(h = mean(results_plan1$Score_Plan1), col = "blue", lty = 2, lwd = 2)
legend("topright", legend = "Mean Score", col = "blue", lty = 2, lwd = 2)

# Radar chart for top robot (if needed for report)
cat("\nBar chart created for all robots.\n")

# ================================================================
# STEP 9: SAVE RESULTS
# ================================================================

cat("\n=== STEP 9: SAVING RESULTS ===\n")

# Save normalized data
write.csv(robots_norm, "robots_normalized_plan1.csv", row.names = FALSE)
cat("Saved: robots_normalized_plan1.csv\n")

# Save results
write.csv(results_plan1, "results_business_plan1.csv", row.names = FALSE)
cat("Saved: results_business_plan1.csv\n")

# ================================================================
# SUMMARY OUTPUT
# ================================================================

cat("\n========================================\n")
cat("BUSINESS PLAN 1 ANALYSIS COMPLETE\n")
cat("========================================\n\n")

cat("*** FINAL RECOMMENDATION ***\n")
cat("Robot Selected:", top_robot, "\n")
cat("Final Score:", round(top_score, 3), "/ 1.000\n")
cat("\nKey Strengths:\n")

# Identify top 3 criteria for recommended robot
top_criteria <- contributions[order(-contributions$Contribution), ][1:3, ]
for(i in 1:3) {
  cat(i, ".", top_criteria$Criterion[i], 
      ": Normalized Score =", top_criteria$Normalized_Score[i],
      ", Contribution =", top_criteria$Contribution[i], "\n")
}

cat("\nThis robot is optimal for the 'Operating at Scale' business model.\n")
cat("========================================\n")


# ================================================================
# BUSINESS PLAN 2: TECHNOLOGY COMMERCIALIZATION
# ================================================================

cat("\n========================================\n")
cat("BUSINESS PLAN 2: TECHNOLOGY LICENSING\n")
cat("========================================\n")

cat("\n=== STEP 1: DEFINE WEIGHTS FOR PLAN 2 ===\n")

# Focus on sellable technology: Battery, Cost, Reliability
weights_plan2 <- c(
  Carrying_Capacity = 0.02,   # Not relevant for tech licensing
  Battery_Size = 0.38,        # 40% - Key innovation (energy efficiency)
  Speed = 0.02,               # Not relevant for tech licensing
  Mobility = 0.02,            # Not relevant for tech licensing
  Aesthetic = 0.03,           # Not relevant for tech licensing
  Cost_Per_Unit = 0.33,       # 35% - Manufacturing efficiency
  Reliability = 0.20          # 25% - Proven durability
)
sum(weights_plan2)

cat("\n--- Weight Distribution for Business Plan 2 ---\n")
weight_df2 <- data.frame(
  Criterion = names(weights_plan2),
  Weight = weights_plan2,
  Percentage = paste0(weights_plan2 * 100, "%"),
  Rationale = c(
    "Not relevant for IP licensing",
    "PRIMARY: Energy efficiency innovation",
    "Not relevant for IP licensing",
    "Not relevant for IP licensing",
    "Not relevant for IP licensing",
    "SECONDARY: Manufacturing optimization",
    "TERTIARY: Proven technology reliability"
  )
)
print(weight_df2)

cat("\nTotal Weight:", sum(weights_plan2), "(must equal 1.00)\n")

cat("\n=== STEP 2: CALCULATING WEIGHTED SCORES ===\n")

# Calculate weighted score for each robot
robots_norm$Score_Plan2 <- 
  robots_norm$`Carrying Capacity` * weights_plan2["Carrying_Capacity"] +
  robots_norm$`Battery Size` * weights_plan2["Battery_Size"] +
  robots_norm$Speed * weights_plan2["Speed"] +
  robots_norm$Mobility * weights_plan2["Mobility"] +
  robots_norm$Aesthetic * weights_plan2["Aesthetic"] +
  robots_norm$`Cost Per Unit` * weights_plan2["Cost_Per_Unit"] +
  robots_norm$Reliability * weights_plan2["Reliability"]

cat("Weighted scores calculated for all robots.\n")

cat("\n=== STEP 3: RANKING ROBOTS ===\n")

# Rank robots
robots_norm$Rank_Plan2 <- rank(-robots_norm$Score_Plan2, ties.method = "min")

# Create results table
results_plan2 <- robots_norm[, c("Robot", "Score_Plan2", "Rank_Plan2")]
results_plan2 <- results_plan2[order(results_plan2$Rank_Plan2), ]

cat("\n--- Final Rankings for Business Plan 2 ---\n")
print(results_plan2)

cat("\n=== STEP 4: DETAILED ANALYSIS ===\n")

# Get top robot
top_robot2 <- results_plan2$Robot[1]
top_score2 <- results_plan2$Score_Plan2[1]

cat("\n*** RECOMMENDATION FOR BUSINESS PLAN 2 ***\n")
cat("Selected Robot:", top_robot2, "\n")
cat("Overall Score:", round(top_score2, 3), "out of 1.000\n")

# Show original values
cat("\n--- Original Performance Metrics ---\n")
top_robot2_original <- robots[robots$Robot == top_robot2, ]
print(top_robot2_original)

# Show normalized values
cat("\n--- Normalized Scores (0-1 scale) ---\n")
top_robot2_norm <- robots_norm[robots_norm$Robot == top_robot2, 2:8]
print(round(top_robot2_norm, 3))

# Contribution breakdown
cat("\n--- Contribution Breakdown ---\n")
contributions2 <- data.frame(
  Criterion = c("Carrying Capacity", "Battery Size", "Speed", 
                "Mobility", "Aesthetic", "Cost Per Unit", "Reliability"),
  Normalized_Score = round(as.numeric(top_robot2_norm[1, ]), 3),
  Weight = weights_plan2,
  Contribution = round(as.numeric(top_robot2_norm[1, ]) * weights_plan2, 3)
)
print(contributions2)
cat("\nTotal Score:", round(sum(contributions2$Contribution), 3), "\n")

cat("\n=== STEP 5: COMPARE WITH PLAN 1 WINNER ===\n")

cat("\nBusiness Plan 1 Winner:", top_robot, "(Score:", round(top_score, 3), ")\n")
cat("Business Plan 2 Winner:", top_robot2, "(Score:", round(top_score2, 3), ")\n")

if(top_robot != top_robot2) {
  cat("\n✓ Different robots recommended for different strategies!\n")
  cat("This demonstrates the importance of aligning robot selection with business objectives.\n")
} else {
  cat("\n⚠ Same robot optimal for both strategies.\n")
}

cat("\n=== STEP 6: VISUALIZATION ===\n")

# Side-by-side comparison
barplot(rbind(results_plan1$Score_Plan1, results_plan2$Score_Plan2),
        beside = TRUE,
        names.arg = results_plan1$Robot,
        main = "Comparison: Plan 1 vs Plan 2 Scores",
        xlab = "Robot Prototype",
        ylab = "Weighted Score",
        col = c("steelblue", "coral"),
        border = "black",
        ylim = c(0, 1),
        legend.text = c("Plan 1: Operating", "Plan 2: Tech Licensing"),
        args.legend = list(x = "topright"))

cat("\n=== BUSINESS PLAN 2 COMPLETE ===\n")

# Save results
write.csv(results_plan2, "results_business_plan2.csv", row.names = FALSE)
cat("Saved: results_business_plan2.csv\n")

cat("\n========================================\n")
cat("FINAL RECOMMENDATIONS\n")
cat("========================================\n")
cat("\nFor Leeds Trial, select TWO robots:\n")
cat("1. Robot", top_robot, "- For 'Operating at Scale' business model\n")
cat("2. Robot", top_robot2, "- For 'Technology Commercialization' strategy\n")
cat("========================================\n")

