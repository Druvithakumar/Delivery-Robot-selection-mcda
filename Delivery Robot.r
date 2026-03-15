# ================================================================
# PART 2: Business Decision-Making: Trial of Delivery Robot 
# STUDENT ID : 202003543
# ================================================================



library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)

# ================================================================
# PHASE 1: DATA LOADING AND UNDERSTANDING
# ================================================================

cat("\n=== LOADING AND FIXING DATA ===\n")

# Load the CSV
robots_raw <- read.csv("Robot_Info.csv", 
                       header = TRUE)

cat("\n--- Raw Data Structure ---\n")
print(robots_raw)

# Transpose the data
robots <- as.data.frame(t(robots_raw[, -1]))
colnames(robots) <- robots_raw[, 1]
robots$Robot <- rownames(robots)
rownames(robots) <- NULL

# Reorder columns
robots <- robots[, c("Robot", "Carrying Capacity", "Battery Size", "Speed", 
                     "Mobility", "Aesthetic", "Cost Per Unit", "Reliability")]

# Convert to numeric
robots[, 2:8] <- lapply(robots[, 2:8], as.numeric)

cat("\n--- Fixed Robot Data Structure ---\n")
print(robots)
str(robots)
summary(robots)

# Load Management Priorities
priorities <- read_excel("Management_Priority.xlsx", 
                         skip = 3)
colnames(priorities) <- c("Criterion", "Importance_Description")

cat("\n--- Management Priorities ---\n")
print(priorities)

# Descriptive Statistics
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

# Missing Data Check
cat("\n=== MISSING DATA CHECK ===\n")
print(colSums(is.na(robots)))

# ================================================================
# PHASE 2: DATA VISUALIZATIONS
# ================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# FIGURE 1: Cost-Capacity Scatter Plot
fig1 <- ggplot(robots, aes(x = `Carrying Capacity`, y = `Cost Per Unit`)) +
  geom_point(size = 4, color = "#2563eb", alpha = 0.7) +
  geom_text(aes(label = Robot), vjust = -1, hjust = 0.5, size = 3.5, 
            fontface = "bold", color = "#1e40af") +
  labs(title = "Figure 1: Cost Efficiency Analysis",
       subtitle = "Trade-off between carrying capacity and unit cost",
       x = "Carrying Capacity (Litres)", y = "Cost per Unit (£)",
       caption = "Note: Lower-right quadrant represents optimal value") +
  scale_y_continuous(labels = comma, limits = c(4000, 10500)) +
  scale_x_continuous(limits = c(30, 75)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0),
        axis.title = element_text(size = 11, face = "bold"),
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5))

print(fig1)
ggsave("Figure1_Cost_Capacity_Analysis.png", plot = fig1, width = 10, height = 6, dpi = 300)

# FIGURE 2: Reliability Ranking
robots_sorted <- robots[order(-robots$Reliability), ]
robots_sorted$Robot <- factor(robots_sorted$Robot, levels = robots_sorted$Robot)

fig2 <- ggplot(robots_sorted, aes(x = Robot, y = Reliability, fill = Robot)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = Reliability), vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(title = "Figure 2: Reliability Performance Ranking",
       subtitle = "Days between breakdowns by prototype",
       x = "Robot Prototype", y = "Days Between Breakdowns") +
  scale_fill_manual(values = c("#7c3aed", "#9B7EBD", "#8E7CC3", "#B89FD8", 
                               "#B89FD8", "#B89FD8", "#D4C4E8")) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0),
        axis.text.x = element_text(face = "bold"),
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5))

print(fig2)
ggsave("Figure2_Reliability_Ranking.png", plot = fig2, width = 10, height = 6, dpi = 300)


# ================================================================
# PHASE 3: DATA PREPARATION - NORMALIZATION
# ================================================================

cat("\n=== NORMALIZING DATA TO 0-1 SCALE ===\n")

robots_norm <- robots

for(i in 2:8) {
  criterion_name <- colnames(robots)[i]
  if(criterion_name == "Cost Per Unit") {
    robots_norm[, i] <- (max(robots[, i]) - robots[, i]) / (max(robots[, i]) - min(robots[, i]))
    cat(criterion_name, ": Normalized (REVERSE - lower is better)\n")
  } else {
    robots_norm[, i] <- (robots[, i] - min(robots[, i])) / (max(robots[, i]) - min(robots[, i]))
    cat(criterion_name, ": Normalized (higher is better)\n")
  }
}

cat("\n--- Normalized Robot Data (0-1 scale) ---\n")
robots_norm_print <- robots_norm
robots_norm_print[, 2:8] <- round(robots_norm_print[, 2:8], 3)
print(robots_norm_print)

# ================================================================
# PHASE 4: BUSINESS PLAN 1 - OPERATING AT SCALE
# ================================================================

cat("\n========================================\n")
cat("BUSINESS PLAN 1: OPERATING AT SCALE\n")
cat("========================================\n")

weights_plan1 <- c(
  Carrying_Capacity = 0.24,
  Battery_Size = 0.17,
  Speed = 0.10,
  Mobility = 0.10,
  Aesthetic = 0.04,
  Cost_Per_Unit = 0.20,
  Reliability = 0.15
)

cat("\n--- Weight Distribution ---\n")
print(data.frame(
  Criterion = names(weights_plan1),
  Weight = weights_plan1,
  Percentage = paste0(weights_plan1 * 100, "%")
))
cat("Total Weight:", sum(weights_plan1), "\n")

# Calculate Scores
robots_norm$Score_Plan1 <- 
  robots_norm$`Carrying Capacity` * weights_plan1["Carrying_Capacity"] +
  robots_norm$`Battery Size` * weights_plan1["Battery_Size"] +
  robots_norm$Speed * weights_plan1["Speed"] +
  robots_norm$Mobility * weights_plan1["Mobility"] +
  robots_norm$Aesthetic * weights_plan1["Aesthetic"] +
  robots_norm$`Cost Per Unit` * weights_plan1["Cost_Per_Unit"] +
  robots_norm$Reliability * weights_plan1["Reliability"]

robots_norm$Rank_Plan1 <- rank(-robots_norm$Score_Plan1, ties.method = "min")
results_plan1 <- robots_norm[, c("Robot", "Score_Plan1", "Rank_Plan1")]
results_plan1 <- results_plan1[order(results_plan1$Rank_Plan1), ]

cat("\n--- Final Rankings for Business Plan 1 ---\n")
print(results_plan1)

top_robot <- results_plan1$Robot[1]
top_score <- results_plan1$Score_Plan1[1]

cat("\n*** PLAN 1 WINNER:", top_robot, "***\n")
cat("Score:", round(top_score, 3), "/ 1.000\n")

# Contribution Breakdown
top_robot_norm <- robots_norm[robots_norm$Robot == top_robot, 2:8]
contributions <- data.frame(
  Criterion = c("Carrying Capacity", "Battery Size", "Speed", 
                "Mobility", "Aesthetic", "Cost Per Unit", "Reliability"),
  Normalized_Score = round(as.numeric(top_robot_norm[1, ]), 3),
  Weight = weights_plan1,
  Contribution = round(as.numeric(top_robot_norm[1, ]) * weights_plan1, 3)
)
cat("\n--- Contribution Breakdown ---\n")
print(contributions)

# Save Results
write.csv(results_plan1, "results_business_plan1.csv", row.names = FALSE)

# ================================================================
# PHASE 5: BUSINESS PLAN 2 - TECHNOLOGY LICENSING
# ================================================================

cat("\n========================================\n")
cat("BUSINESS PLAN 2: TECHNOLOGY LICENSING\n")
cat("========================================\n")

weights_plan2 <- c(
  Carrying_Capacity = 0.00,
  Battery_Size = 0.40,
  Speed = 0.00,
  Mobility = 0.00,
  Aesthetic = 0.00,
  Cost_Per_Unit = 0.35,
  Reliability = 0.25
)

cat("\n--- Weight Distribution ---\n")
print(data.frame(
  Criterion = names(weights_plan2),
  Weight = weights_plan2,
  Percentage = paste0(weights_plan2 * 100, "%")
))
cat("Total Weight:", sum(weights_plan2), "\n")

# Calculate Scores
robots_norm$Score_Plan2 <- 
  robots_norm$`Carrying Capacity` * weights_plan2["Carrying_Capacity"] +
  robots_norm$`Battery Size` * weights_plan2["Battery_Size"] +
  robots_norm$Speed * weights_plan2["Speed"] +
  robots_norm$Mobility * weights_plan2["Mobility"] +
  robots_norm$Aesthetic * weights_plan2["Aesthetic"] +
  robots_norm$`Cost Per Unit` * weights_plan2["Cost_Per_Unit"] +
  robots_norm$Reliability * weights_plan2["Reliability"]

robots_norm$Rank_Plan2 <- rank(-robots_norm$Score_Plan2, ties.method = "min")
results_plan2 <- robots_norm[, c("Robot", "Score_Plan2", "Rank_Plan2")]
results_plan2 <- results_plan2[order(results_plan2$Rank_Plan2), ]

cat("\n--- Final Rankings for Business Plan 2 ---\n")
print(results_plan2)

top_robot2 <- results_plan2$Robot[1]
top_score2 <- results_plan2$Score_Plan2[1]

cat("\n*** PLAN 2 WINNER:", top_robot2, "***\n")
cat("Score:", round(top_score2, 3), "/ 1.000\n")

# Contribution Breakdown
top_robot2_norm <- robots_norm[robots_norm$Robot == top_robot2, 2:8]
contributions2 <- data.frame(
  Criterion = c("Carrying Capacity", "Battery Size", "Speed", 
                "Mobility", "Aesthetic", "Cost Per Unit", "Reliability"),
  Normalized_Score = round(as.numeric(top_robot2_norm[1, ]), 3),
  Weight = weights_plan2,
  Contribution = round(as.numeric(top_robot2_norm[1, ]) * weights_plan2, 3)
)
cat("\n--- Contribution Breakdown ---\n")
print(contributions2)

# Save Results
write.csv(results_plan2, "results_business_plan2.csv", row.names = FALSE)

# ================================================================
# PHASE 6: PLAN 2 VISUALIZATIONS
# ================================================================

cat("\n=== CREATING PLAN 2 VISUALIZATIONS ===\n")

# FIGURE 3: Plan 2 Rankings
fig4 <- ggplot(results_plan2, aes(x = reorder(Robot, -Score_Plan2), 
                                  y = Score_Plan2, fill = Robot)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = round(Score_Plan2, 3)), vjust = -0.5, 
            size = 3.5, fontface = "bold") +
  labs(title = "Figure 3: Technology Licensing Strategy Rankings",
       subtitle = "IP value: Battery (40%), Cost (35%), Reliability (25%)",
       x = "Robot Prototype", y = "Weighted Score") +
  scale_fill_manual(values = c("#16a34a", "#ea580c", "#dc2626", "#ca8a04", 
                               "#db2777", "#7c3aed", "#2563eb")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold"),
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5))

print(fig4)
ggsave("Figure4_Plan2_Rankings.png", plot = fig4, width = 10, height = 6, dpi = 300)

# FIGURE 4: Plan Comparison
comparison_data <- data.frame(
  Robot = results_plan1$Robot,
  Plan1 = results_plan1$Score_Plan1,
  Plan2 = results_plan2$Score_Plan2[match(results_plan1$Robot, results_plan2$Robot)]
)
comparison_long <- melt(comparison_data, id.vars = "Robot")
colnames(comparison_long) <- c("Robot", "Plan", "Score")

fig5 <- ggplot(comparison_long, aes(x = Robot, y = Score, fill = Plan)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(Score, 2)), 
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3) +
  labs(title = "Figure 4: Strategic Comparison - Plan 1 vs Plan 2",
       subtitle = "Operating at Scale vs Technology Licensing",
       x = "Robot Prototype", y = "Weighted Score") +
  scale_fill_manual(values = c("Plan1" = "#2563eb", "Plan2" = "#16a34a"),
                    labels = c("Operating", "Licensing")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.position = "top",
        panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5))

print(fig5)
ggsave("Figure5_Plan_Comparison.png", plot = fig5, width = 10, height = 6, dpi = 300)

cat("✓ All Plan 2 visualizations saved\n")

# ================================================================
# PHASE 7: FINAL RECOMMENDATIONS
# ================================================================

cat("\n========================================\n")
cat("FINAL TRIAL RECOMMENDATIONS\n")
cat("========================================\n")

cat("\nPlan 1 Winner:", top_robot, "(Score:", round(top_score, 3), ")\n")
cat("Plan 2 Winner:", top_robot2, "(Score:", round(top_score2, 3), ")\n\n")

if(top_robot != top_robot2) {
  cat("✓ STRATEGIC DIVERGENCE: Different robots optimal\n")
  cat("\nRecommendation for Leeds Trial:\n")
  cat("  1.", top_robot, "- Operating at Scale\n")
  cat("  2.", top_robot2, "- Technology Licensing\n")
} else {
  cat("⚠ STRATEGIC CONVERGENCE: Same robot optimal for both\n")
  second_robot <- results_plan2$Robot[2]
  cat("\nRecommendation for Leeds Trial:\n")
  cat("  1.", top_robot, "- Primary (both strategies)\n")
  cat("  2.", second_robot, "- Secondary (Plan 2 alternative)\n")
  cat("\nRationale:", top_robot, "excels in both strategies, but testing\n")
  cat(second_robot, "provides valuable Plan 2 comparison data.\n")
}

cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")

