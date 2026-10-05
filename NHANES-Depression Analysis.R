# Install packages 
# install.packages("haven")
# install.packages("dplyr")
# install.packages("broom")
# install.packages("gtsummary")
# install.packages("KernSmooth")
# install.packages("ggplot2)
# install.packages("randomforrest")

# Load libraries
library(haven)
library(dplyr)
library(broom)
library(gtsummary)
library(KernSmooth)
library(ggplot2)
library(randomForest)

# 2. READ NHANES DATA
demo <- read_xpt("C:/Users/mouni/Downloads/DEMO_L.xpt")
dpq  <- read_xpt("C:/Users/mouni/Downloads/DPQ_L.xpt")
bmx  <- read_xpt("C:/Users/mouni/Downloads/BMX_L.xpt")

# 3. Merge dataset
nhanes_data <- demo %>%
  inner_join(dpq, by = "SEQN") %>%
  inner_join(bmx, by = "SEQN")


# 4. Create variables
phq_vars <- c(
  "DPQ010", "DPQ020", "DPQ030", "DPQ040", "DPQ050",
  "DPQ060", "DPQ070", "DPQ080", "DPQ090"
)

nhanes_data <- nhanes_data %>%
  mutate(
    depression_score = rowSums(select(., all_of(phq_vars)), na.rm = FALSE),
    depression_bin = ifelse(depression_score >= 10, 1, 0),
    depression = factor(depression_bin,
                        levels = c(0, 1),
                        labels = c("No Depression", "Depression")),
    depression_binary = factor(ifelse(depression_score >= 10,
                                      "Depressed", "Not Depressed")),
    
    sex = factor(RIAGENDR,
                 levels = c(1, 2),
                 labels = c("Male", "Female")),
    
    race = factor(RIDRETH1,
                  levels = c(1, 2, 3, 4, 5),
                  labels = c("Mexican American",
                             "Other Hispanic",
                             "Non-Hispanic White",
                             "Non-Hispanic Black",
                             "Other Race")),
    
    education = factor(DMDEDUC2,
                       levels = c(1, 2, 3, 4, 5),
                       labels = c("Less than 9th grade",
                                  "9th - 11th grade",
                                  "High school/GED",
                                  "Some college/AA",
                                  "College graduate or above")),
    
    marital = factor(DMDMARTZ,
                     levels = c(1, 2, 3, 4, 5, 6),
                     labels = c("Married",
                                "Widowed",
                                "Divorced",
                                "Separated",
                                "Never married",
                                "Living with partner")),
    
    pregnancy = factor(RIDEXPRG,
                       levels = c(1, 2, 3),
                       labels = c("Pregnant", "Not pregnant", "Unknown")),
    
    exam_month = factor(RIDEXMON,
                        levels = c(1, 2),
                        labels = c("Nov-Apr", "May-Oct")),
    
    income_group = case_when(
      INDFMPIR < 1.3 ~ "Low",
      INDFMPIR >= 1.3 & INDFMPIR < 3.5 ~ "Middle",
      INDFMPIR >= 3.5 ~ "High",
      TRUE ~ NA_character_
    ),
    income_group = factor(income_group, levels = c("Low", "Middle", "High")),
    
    bmi_group = case_when(
      BMXBMI < 18.5 ~ "Underweight",
      BMXBMI >= 18.5 & BMXBMI < 25 ~ "Normal",
      BMXBMI >= 25 & BMXBMI < 30 ~ "Overweight",
      BMXBMI >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    bmi_group = factor(bmi_group,
                       levels = c("Underweight", "Normal", "Overweight", "Obese")),
    
    obesity = case_when(
      BMXBMI < 30 ~ "Not obese",
      BMXBMI >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    obesity = factor(obesity, levels = c("Not obese", "Obese")),
    
    age_group = cut(
      RIDAGEYR,
      breaks = c(0, 19, 39, 59, Inf),
      labels = c("0-19", "20-39", "40-59", "60+"),
      right = TRUE
    )
  )

# 5. TABLE 1: DESCRIPTIVE STATISTICS
table1 <- nhanes_data %>%
  select(exam_month, sex, RIDAGEYR, race, education,
         marital, pregnancy, INDFMPIR, BMXBMI, depression_score) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  bold_labels()

table1

# 6. TABLE 2: BIVARIATE ANALYSIS
table2 <- nhanes_data %>%
  select(depression, exam_month, sex, RIDAGEYR, race,
         education, marital, pregnancy, INDFMPIR, BMXBMI) %>%
  tbl_summary(
    by = depression,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      education ~ "fisher.test",
      marital ~ "fisher.test",
      pregnancy ~ "fisher.test",
      all_categorical() ~ "chisq.test",
      all_continuous() ~ "t.test"
    ),
    test.args = list(
      education ~ list(simulate.p.value = TRUE),
      marital ~ list(simulate.p.value = TRUE),
      pregnancy ~ list(simulate.p.value = TRUE)
    )
  ) %>%
  bold_labels()

table2

# 7. TABLE 3: MULTIVARIABLE LOGISTIC REGRESSION
reg_data <- nhanes_data %>%
  select(depression_bin, exam_month, sex, RIDAGEYR, race,
         education, marital, pregnancy, INDFMPIR, BMXBMI) %>%
  na.omit()

factor_vars <- c("exam_month", "sex", "race", "education", "marital", "pregnancy")
reg_data[factor_vars] <- lapply(reg_data[factor_vars], droplevels)

levels_count <- sapply(reg_data[factor_vars], nlevels)
valid_factors <- names(levels_count[levels_count >= 2])

final_vars <- c("depression_bin", valid_factors, "RIDAGEYR", "INDFMPIR", "BMXBMI")

model_formula <- as.formula(
  paste("depression_bin ~", paste(final_vars[-1], collapse = " + "))
)

model <- glm(model_formula, data = reg_data[, final_vars], family = binomial())

table3 <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  select(
    Variable = term,
    `Odds Ratio` = estimate,
    `CI Lower` = conf.low,
    `CI Upper` = conf.high,
    `P-value` = p.value
  )

table3

# 8. KERNEL SMOOTHING PLOTS

plot_ksmooth_two_groups <- function(data, group_var, group_labels,
                                    plot_title, colors_vec = c("blue", "red"),
                                    y_limit = 0.4) {
  
  plot_data <- data %>%
    select(RIDAGEYR, depression_bin, !!sym(group_var)) %>%
    filter(!is.na(RIDAGEYR),
           !is.na(depression_bin),
           !is.na(.data[[group_var]]))
  
  g1 <- subset(plot_data, plot_data[[group_var]] == group_labels[1])
  g2 <- subset(plot_data, plot_data[[group_var]] == group_labels[2])
  
  fit1 <- ksmooth(g1$RIDAGEYR, g1$depression_bin,
                  kernel = "normal", bandwidth = 5,
                  n.points = nrow(g1))
  
  fit2 <- ksmooth(g2$RIDAGEYR, g2$depression_bin,
                  kernel = "normal", bandwidth = 5,
                  n.points = nrow(g2))
  
  plot(fit1$x, fit1$y, type = "l",
       col = colors_vec[1],
       lwd = 3,
       ylim = c(0, y_limit),
       xlab = "Age",
       ylab = "Proportion with Depression",
       main = plot_title)
  
  lines(fit2$x, fit2$y, col = colors_vec[2], lwd = 3)
  
  legend("topright",
         legend = group_labels,
         col = colors_vec,
         lwd = 3,
         bty = "n")
}

plot_ksmooth_multi_groups <- function(data, group_var, plot_title,
                                      colors_vec = NULL, y_limit = 0.4) {
  
  plot_data <- data %>%
    select(RIDAGEYR, depression_bin, !!sym(group_var)) %>%
    filter(!is.na(RIDAGEYR),
           !is.na(depression_bin),
           !is.na(.data[[group_var]]))
  
  group_levels <- levels(as.factor(plot_data[[group_var]]))
  
  if (is.null(colors_vec)) {
    colors_vec <- rainbow(length(group_levels))
  }
  
  plot(NULL,
       xlim = range(plot_data$RIDAGEYR, na.rm = TRUE),
       ylim = c(0, y_limit),
       xlab = "Age",
       ylab = "Proportion with Depression",
       main = plot_title)
  
  for (i in seq_along(group_levels)) {
    
    subset_data <- subset(plot_data, plot_data[[group_var]] == group_levels[i])
    
    if (nrow(subset_data) > 10) {
      
      fit <- ksmooth(subset_data$RIDAGEYR,
                     subset_data$depression_bin,
                     kernel = "normal",
                     bandwidth = 5,
                     n.points = nrow(subset_data))
      
      lines(fit$x, fit$y,
            col = colors_vec[i],
            lwd = 2)
    }
  }
  
  legend("topright",
         legend = group_levels,
         col = colors_vec,
         lwd = 2,
         cex = 0.7,
         bty = "n")
}


# Sex plot: y-axis 0.4
plot_ksmooth_two_groups(
  nhanes_data,
  "sex",
  c("Male", "Female"),
  "Kernel Smoothing: Sex vs Depression",
  colors_vec = c("blue", "red"),
  y_limit = 0.4
)


# Obesity plot: y-axis 0.4
plot_ksmooth_two_groups(
  nhanes_data,
  "obesity",
  c("Not obese", "Obese"),
  "Kernel Smoothing: Obesity vs Depression",
  colors_vec = c("blue", "red"),
  y_limit = 0.4
)


# Exam month plot: y-axis 0.4
plot_ksmooth_two_groups(
  nhanes_data,
  "exam_month",
  c("Nov-Apr", "May-Oct"),
  "Kernel Smoothing: Time Period vs Depression",
  colors_vec = c("blue", "red"),
  y_limit = 0.4
)


# Income group plot: y-axis 0.4
plot_ksmooth_multi_groups(
  nhanes_data,
  "income_group",
  "Kernel Smoothing: Income Group vs Depression",
  colors_vec = c("blue", "red", "darkgreen"),
  y_limit = 0.4
)


# Marital status: only Married, Widowed, Divorced
marital_filtered <- nhanes_data %>%
  filter(marital %in% c("Married", "Widowed", "Divorced")) %>%
  droplevels()

plot_ksmooth_multi_groups(
  marital_filtered,
  "marital",
  "Kernel Smoothing: Selected Marital Status vs Depression",
  colors_vec = c("blue", "red", "darkgreen"),
  y_limit = 0.6
)

# Pregnancy plot
pregnancy_filtered <- nhanes_data %>%
  filter(pregnancy %in% c("Pregnant", "Not pregnant")) %>%
  droplevels()
plot_ksmooth_two_groups(
  pregnancy_filtered,
  "pregnancy",
  c("Not pregnant", "Pregnant"),
  "Kernel Smoothing: Pregnancy vs Depression",
  y_limit = 0.4
)


# Education: only selected groups
education_filtered <- nhanes_data %>%
  filter(education %in% c(
    "Less than 9th grade",
    "9th - 11th grade",
    "College graduate or above"
  )) %>%
  droplevels()

plot_ksmooth_multi_groups(
  education_filtered,
  "education",
  "Kernel Smoothing: Selected Education Levels vs Depression",
  colors_vec = c("blue", "red", "darkgreen"),
  y_limit = 0.4
)


# 9. PROPORTION PLOTS BY DEPRESSION STATUS
analysis_data <- nhanes_data %>%
  select(
    SEQN, depression_score, depression_binary, RIDAGEYR, BMXBMI,
    INDFMPIR, exam_month, sex, race, education, marital,
    pregnancy, age_group, income_group, bmi_group
  ) %>%
  filter(!is.na(depression_binary)) %>%
  filter(!is.na(RIDAGEYR),
         !is.na(BMXBMI),
         !is.na(INDFMPIR),
         !is.na(exam_month),
         !is.na(sex),
         !is.na(race),
         !is.na(education),
         !is.na(marital))

analysis_data$depression_binary <- as.factor(analysis_data$depression_binary)

variables <- c(
  "age_group", "sex", "race", "education",
  "marital", "pregnancy", "income_group",
  "bmi_group", "exam_month"
)

for (var_name in variables) {
  filtered_data <- na.omit(analysis_data[, c(var_name, "depression_binary")])
  
  if (nrow(filtered_data) == 0) next
  
  table_data <- table(filtered_data[[var_name]], filtered_data$depression_binary)
  prop_table <- prop.table(table_data, margin = 1)
  prop_data <- as.data.frame(prop_table)
  
  colnames(prop_data) <- c("Category", "depression_binary", "Proportion")
  
  p <- ggplot(prop_data, aes(x = Category, y = Proportion, fill = depression_binary)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = paste("Proportion of", var_name, "by Depression Status"),
      x = var_name,
      y = "Proportion",
      fill = "Depression"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# 10. RANDOM FOREST + LOCO
significant_vars <- c(
  "RIDAGEYR", "BMXBMI", "INDFMPIR", "exam_month",
  "sex", "race", "education", "marital", "pregnancy"
)

rf_data <- analysis_data[, c(significant_vars, "depression_binary")]
rf_data <- na.omit(rf_data)

rf_data <- rf_data %>%
  mutate(across(where(is.character), as.factor))

set.seed(123)

train_index <- sample(seq_len(nrow(rf_data)), size = 0.7 * nrow(rf_data))
train_data <- rf_data[train_index, ]
test_data  <- rf_data[-train_index, ]

formula_rf <- as.formula(
  paste("depression_binary ~", paste(significant_vars, collapse = " + "))
)

rf_model <- randomForest(
  formula = formula_rf,
  data = train_data,
  importance = TRUE,
  ntree = 500
)

print(rf_model)

var_importance <- importance(rf_model)
importance_col <- if ("MeanDecreaseAccuracy" %in% colnames(var_importance)) {
  "MeanDecreaseAccuracy"
} else {
  colnames(var_importance)[1]
}

var_importance_df <- data.frame(
  Variable = rownames(var_importance),
  Importance = var_importance[, importance_col]
)

print(var_importance_df)
varImpPlot(rf_model, main = "Variable Importance for Depression Prediction")

loco_results <- data.frame(
  Variable = significant_vars,
  Accuracy = NA,
  Difference = NA
)

baseline_pred <- predict(rf_model, test_data)
baseline_accuracy <- mean(baseline_pred == test_data$depression_binary)

for (var in loco_results$Variable) {
  loco_formula <- as.formula(paste("depression_binary ~ . -", var))
  
  loco_model <- randomForest(
    formula = loco_formula,
    data = train_data,
    importance = TRUE,
    ntree = 500
  )
  
  loco_pred <- predict(loco_model, test_data)
  loco_accuracy <- mean(loco_pred == test_data$depression_binary)
  
  loco_results$Accuracy[loco_results$Variable == var] <- loco_accuracy
  loco_results$Difference[loco_results$Variable == var] <- baseline_accuracy - loco_accuracy
}

loco_results <- loco_results[order(-loco_results$Difference), ]
print(loco_results)

ggplot(loco_results, aes(x = reorder(Variable, Difference), y = Difference)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Leave-One-Covariate-Out (LOCO) Analysis",
    x = "Covariate",
    y = "Decrease in Accuracy"
  ) +
  theme_minimal()