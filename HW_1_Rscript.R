### Predicting CHD in patient 

# Load required libraries

library(pacman)
p_load(ISLR, 
       ISLR2, 
       ROCR, 
       tidyverse, 
       caret, 
       patchwork, class, smotefamily, e1071, dplyr, ggplot2, corrplot, car)


#### ETL ####

# Load and inspect data

data <- read.csv("C:/Users/ettor/OneDrive/Documenti/UNITN - MAGISTRALE/CORSI/First Year/Second Semester/Stat mod/HWs_Directory/chd.csv")
#summary(data)

# Impute missing values

simple_imputer <- function(data) {
  # imputing continuous variables
  data$cpd[is.na(data$cpd)] <- median(data$cpd, na.rm = TRUE)
  data$chol[is.na(data$chol)] <- median(data$chol, na.rm = TRUE)
  data$HR[is.na(data$HR)] <- median(data$HR, na.rm = TRUE)
  data$BMI[is.na(data$BMI)] <- median(data$BMI, na.rm = TRUE)
  # Impute categorical variable
  data$education[is.na(data$education)] <- median(data$education, na.rm=TRUE)
  return(data)
}

imputed_data <- simple_imputer(data)
#summary(imputed_data) # Check

# Cast nominal variable to proper type

data_cleaned <- imputed_data |>
  mutate(sex = as.factor(sex),
         education = factor(education, levels = c(1,2,3,4),
                               labels = c("NoHS", "HS", "COL", "P-COL")),
         smoker = factor(smoker, levels = c(0, 1), labels = c("No", "Yes")),
         stroke = factor(stroke, levels = c(0, 1), labels = c("No", "Yes")),
         HTN = factor(HTN, levels = c(0, 1), labels = c("No", "Yes")),
         diabetes = factor(diabetes, levels = c(0,1), labels = c("No", "Yes")),
         CHD = as.factor(CHD))

# Saving nominal var. references

categ <- c()

for(name in names(data_cleaned)){
  if(!is.numeric(data_cleaned[[name]])){
    categ <- append(name, categ)
  }
}


#### EDA ####

# Box plots to search relationships between CHD and continuous var 

p1 <- ggplot(data_cleaned, aes(x = CHD, y = age, fill = CHD)) + 
  geom_boxplot() +
  labs(title = "Age", x = "CHD Status", y = "Age") + 
  theme_minimal() + 
  theme(legend.position = "none")

p2 <- ggplot(data_cleaned, aes(x = CHD, y = cpd, fill = CHD)) + 
  geom_boxplot() +
  labs(title = "Cigpday", x = "CHD Status", y = "Cigpday") + 
  theme_minimal() + 
  theme(legend.position = "none")

p3 <- ggplot(data_cleaned, aes(x = CHD, y = chol, fill = CHD)) + 
  geom_boxplot() +
  labs(title = "Chol", x = "CHD Status", y = "Chol") + 
  theme_minimal() + 
  theme(legend.position = "none")

p4 <- ggplot(data_cleaned, aes(x = CHD, y = BMI, fill = CHD)) + 
  geom_boxplot() +
  labs(title = "BMI", x = "CHD Status", y = "BMI") + 
  theme_minimal() + 
  theme(legend.position = "none")

p5 <- ggplot(data_cleaned, aes(x = CHD, y = DBP, fill = CHD)) + 
  geom_boxplot() +
  labs(title = "DBP", x = "CHD Status", y = "DBP") + 
  theme_minimal() + 
  theme(legend.position = "none")

p6 <- ggplot(data_cleaned, aes(x = CHD, y = HR, fill = CHD)) + 
  geom_boxplot() +
  labs(title = "HR", x = "CHD Status", y = "HR") + 
  theme_minimal() + 
  theme(legend.position = "none")

(p1|p2|p3) /
(p4|p5|p6)

# Bar plots to search relationships between CHD and categorical var.

chd_fill <- scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                              name = "CHD",
                              labels = c("Yes","No"))

p1 <- ggplot(data_cleaned, aes(x = sex, fill = CHD)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Sex vs CHD", x = "Sex", y = "Prop") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p2 <- ggplot(data_cleaned, aes(x = education, fill = CHD)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Edu vs CHD", x = "Edu") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p3 <- ggplot(data_cleaned, aes(x = smoker, fill = CHD)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Smoke vs CHD", x = "Smk") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p4 <- ggplot(data_cleaned, aes(x = stroke, fill = CHD)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Stroke vs CHD", x = "Str") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p5 <- ggplot(data_cleaned, aes(x = HTN, fill = CHD)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "HTN vs CHD", x = "HTN") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p6 <- ggplot(data_cleaned, aes(x = diabetes, fill = CHD)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = "Diabetes vs CHD", x = "Diab") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "bottom")

(p1|p2|p3) /
  (p4|p5|p6) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Check for Data distribution into different classes

p1 <- ggplot(data_cleaned, aes(x = sex, fill = CHD)) + 
  geom_bar() +
  labs(title = "Sex vs CHD", x = "Sex", y = "Prop") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p2 <- ggplot(data_cleaned, aes(x = education, fill = CHD)) + 
  geom_bar() +
  labs(title = "Edu vs CHD", x = "Edu") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p3 <- ggplot(data_cleaned, aes(x = smoker, fill = CHD)) + 
  geom_bar() +
  labs(title = "Smoke vs CHD", x = "Smk") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p4 <- ggplot(data_cleaned, aes(x = stroke, fill = CHD)) + 
  geom_bar() +
  labs(title = "Stroke vs CHD", x = "Str") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p5 <- ggplot(data_cleaned, aes(x = HTN, fill = CHD)) + 
  geom_bar() +
  labs(title = "HTN vs CHD", x = "HTN") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "none")

p6 <- ggplot(data_cleaned, aes(x = diabetes, fill = CHD)) + 
  geom_bar() +
  labs(title = "Diabetes vs CHD", x = "Diab") + 
  chd_fill + 
  theme_minimal() + 
  theme(legend.position = "bottom")

(p1|p2|p3) /
  (p4|p5|p6) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Notes:

# The majority of variables, both categorical and continuous, appears to
# have discriminant power in identifying CHD levels. Therefore, they may offer
# valuable information for modelling CHD risk. However, caution is needed with  
# stroke and diabetes, as their positive cases are underrepresented. This may 
# lead to unstable or misleading coefficients estimates and inflated 
# standard errors in regression-based models. In decision-based models rare
# categories may over fit to noise or create decision boundaries too specific. 

# Example: If only 1.5% of the population had a stroke, and most of them also
# had CHD, the model might overestimate the importance of stroke — or fail 
# to detect it entirely, depending on the algorithm and evaluation metric.

# Quantify the associations between cat. var. and CHD

c2 <- V <- p_vals <- c()

for(col in categ){
  
  ch2 <- chisq.test(data_cleaned[[col]], data_cleaned$CHD)
  V2 <- sqrt((ch2$statistic/sum(ch2$observed))/ch2$parameter)
  
  c2 <- append(c2, ch2$statistic)
  
  p_vals <- append(p_vals, ch2$p.value)
  
  V <- append(V, V2)
}

sum_tab_cat <- as.data.frame(rbind(c2, V, p_vals))
names(sum_tab_cat) <- categ
sum_tab_cat <- select(sum_tab_cat, -CHD)
rownames(sum_tab_cat) <- c('Chi-squared', "Cramer's V", 'p-value')
sum_tab_cat <- round(sum_tab_cat, 2)
print(sum_tab_cat)

# Comment: The table summarize the result of a Pearson's Chi-Squared test of 
# independence carried out between each categorical variable in the dataset and
# CHD. It also adds the Cramer's Index of Association which measure the strength
# of association. The features showing a weak association with CHD are sex, 
# diabetes and Hypertension (HTN).Smoking status does not exhibit a significant 
# association,while education and stroke show very weak relationships. 

# Collinearity search

one_hot <- model.matrix(~ . -1, data = data_cleaned)
corrmat <- cor(one_hot, use = "pairwise.complete.obs")
corrplot(corrmat, method = "color", type = "upper", number.cex = 0.5,
         number.digits=2, tl.cex = 0.5, tl.col = "black", addCoef.col= "black")

# Comment: As expected we observe some strong correlations between: the number
# of cigarettes per day and smokerYES, the Diastolic Blood pressure and HTNYES. 
# This reflect logical associations, but we'll keep them in mind and consider 
# performing multicollinearity diagnostics (e.g., VIF) during the modelling 
# phase to ensure they don't distort the model performance.

# Response variable distribution analysis

prop.table(table(data_cleaned$CHD))

chd_fill <- scale_fill_manual(values = c("#F8766D", "#00BFC4"), name = "CHD",
                              labels = c("Yes", "No"))

ggplot(data_cleaned, aes(x = CHD, fill = CHD)) +
  geom_bar() + 
  labs(title = "CHD distribution", y = "Count") + 
  theme_minimal() + 
  chd_fill +
  theme(leggend.position="bottom")

# Comment: The response variable is clearly imbalanced, with the majority of
# observations classified as “No” for CHD, meaning most individuals do not 
# experience coronary heart disease within 10 years. This imbalance has 
# important modeling implications—standard classifiers like logistic regression
# tend to favor the majority class, which can result in poor prediction 
# performance on the minority class.


#### Stratified Train-Test Split + Scaled Train-Test ####

set.seed(42)
stratified_sampling <- function(data) {
  train_index <- createDataPartition(data$CHD, p=0.7, list=FALSE)
  train_data <- data[train_index,]
  test_data <- data[-train_index,]
  return(list(train= train_data, test = test_data))
}

split_data <- stratified_sampling(data_cleaned)
train <- split_data$train
test <- split_data$test

# Check new train/test CHD levels prop.

prop.table(table(data_cleaned$CHD))
prop.table(table(train$CHD))
prop.table(table(test$CHD))

# Features standardization 

train_scaled <- train |> 
  select(-categ) |>
  scale() |>
  as_tibble() |>
  cbind(train[, categ]) |>
  mutate(across(setdiff(categ, 'CHD'), as.numeric))

test_scaled <- test |>
  select(-categ) |>
  scale() |> 
  as_tibble() |>
  cbind(test[, categ]) |>
  mutate(across(setdiff(categ, 'CHD'), as.numeric))


#### Modeling  1 ####


## GLM models - Logistic Regression

glm_logit <- glm(CHD ~ ., data = train, family = binomial)
summary(glm_logit)

# Collinearity diagnostic

vif(glm_logit)

# Notes: 

# No critical collinearity detected among predictors. The highest VIF 
# value (1.62) corresponds to 'cpd', primarily due to its correlation with 
# 'smoker' and possibly other related factors. However, each of these predictors
# captures distinct aspect of smoking behavior, so it is reasonable to retain 
# both. We conclude that all predictors with moderate correlation reflect 
# different facets of the same underlying phenomena and can be safely included
# in the model. 


## Decision-based models - K-NN

# K-NN

ctrl <- trainControl(method = "cv", number = 10)
k_grid <- expand.grid(k = 3:30)
knn_model <- train(CHD ~ ., data = train_scaled, method = "knn",
                  tuneGrid = k_grid, trControl = ctrl)
best_k <- knn_model$bestTune$k
best_acc <- knn_model$results |>
  filter(k == best_k) |>
  pull(Accuracy)
cat("Best Accuracy of", round(best_acc, 2), "with k =", best_k)


#### Evaluation 1 ####

# Logistic Regression 

#levels(test$CHD) # Expected output:  [1] "No"  "Yes" 
predics_glm <- predict(glm_logit, newdata = test, type = 'response')
classes <- factor(ifelse(predics_glm > .5, 'Yes', 'No'), levels = c("No", "Yes"))
glm_cm <- confusionMatrix(classes, test$CHD, positive = "Yes")
glm_cm

sensitivity_glm <- glm_cm$byClass["Sensitivity"]
fnr_glm <- 1 - sensitivity_glm
cat("FNR:", round(fnr_glm, 4))

# Notes: 

# Good precision (Positive predicted values) ~72% of the times a 
# patient is detected as high risk to CHD, it will develop the disease.

# Almost perfect specificity (True Negative Rate) ~99% of the times a patient
# is detected as no risk, it won't develop the disease.

# Very bad sensitivity (True Positive Rate) less than ~4,5% of the times a 
# patient is detected as high risk, we it actually is. FRN very high, almost 
# 95% of the times (1-sensitivity) the patient with high risk that will in fact
# develop CHD is not detected. 

# Accuracy is no use in this scenario due to imbalance distribution of the  
# response variable.

# K-NN

#levels(test_scaled$CHD) # Expected output:  [1] "No"  "Yes" 
predics_knn <- predict(knn_model, newdata = test_scaled)
knn_cm <- confusionMatrix(predics_knn, test$CHD, positive = "Yes")
knn_cm

sensitivity_knn <- knn_cm$byClass["Sensitivity"]
fnr_knn <- 1 - sensitivity_knn
cat("FNR:", round(fnr_knn, 4))

# Notes: 

# The situation is the same as in the logistics model: good overall accuracy 
# and almost perfect specificity. However, the model completely fails to 
# identify the minority class of the response variable. Therefore, we cannot a 
# a priori evaluate these models, unless we handle the imbalance problem first.


#### Modeling and Evaluation 2 ####

# We deploy SMOTE to generate Synthetic data of the minority class
# (Synthetic Minority Over-sampling Technique)


## GLM - SMOTE Logistic Regression

# Create SMOTE Train Set

convert_to_numeric <- function(data, categ) {
  for(name in (categ)){
    if(name == "sex") {
      data$sex <- ifelse(data$sex == "Male", 1, 0)
    } else {
      data[[name]] <- as.numeric(data[[name]]) 
    }
  }
  return(data)
}

categ_no_chd <- c("diabetes","HTN","stroke","smoker","education","sex")
smote_train <- train # Copy train
smote_train$CHD_bin <- ifelse(smote_train$CHD == "Yes", 1, 0)
smote_train <- convert_to_numeric(smote_train, categ_no_chd)

X_train <- smote_train[, !(names(smote_train) %in% c("CHD", "CHD_bin"))]
y_train <- smote_train$CHD_bin

set.seed(42)
synt_train_plus_metadata <- SMOTE(X_train, y_train, K = 5, dup_size = 3)

synt_train <- synt_train_plus_metadata$data
synt_train$CHD <- factor(ifelse(synt_train$class == 1, "Yes", "No"))
synt_train$class <- NULL

# Convert original test set to numerical

numerical_test_logit <- convert_to_numeric(test, categ_no_chd)

# RE-train logit

glm_logit_synt <- glm(CHD ~ ., data = synt_train, family = "binomial")
#summary(glm_logit_synt)

#levels(numerical_test_logit$CHD) # Expected Output: [1] "No"  "Yes"
synt_pred_log <- predict(glm_logit_synt, newdata = numerical_test_logit,
                         type = 'response')
synt_pred_classes <- factor(ifelse(synt_pred_log > .5, "Yes", "No"), 
                            levels = c("No", "Yes"))
synt_logit_cm <- confusionMatrix(synt_pred_classes, numerical_test_logit$CHD,
                                 positive = "Yes")
synt_logit_cm
fnr_glm_smote <- synt_logit_cm$byClass["Sensitivity"]
cat("FNR:", fnr_glm_smote)


## SMOTE K-NN  

# Create SMOTE scaled train set

smote_train_scaled <- train_scaled # copy of scaled train
smote_train_scaled$CHD_bin <- ifelse(smote_train_scaled$CHD == "Yes", 1, 0)
smote_train_scaled <- convert_to_numeric(smote_train_scaled, categ_no_chd)
X_train_scaled <- smote_train_scaled[, !(names(smote_train_scaled) %in% c("CHD",
                                                                          "CHD_bin"))]
y_train_scaled <- smote_train_scaled$CHD_bin
set.seed(42)
synt_train_plus_metadata_scaled <- SMOTE(X_train_scaled,
                                             y_train_scaled, K = 5, dup_size = 3)
synt_train_scaled <- synt_train_plus_metadata_scaled$data
synt_train_scaled$CHD <- factor(ifelse(synt_train_scaled$class == 1, "Yes", "No"))
synt_train_scaled$class <- NULL

# Convert original scaled test set to numerical

numerical_test_knn <- convert_to_numeric(test_scaled, categ_no_chd)

## RE-train K-NN

# Note:

# This time, as we are targeting specifically False Negatives, we use 
# sensitivity as the metric to maximize.

ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

k_grid <- expand.grid(k = 3:30)

# Make sure "No" is reference Level a.k.a "0";
# We are already sure, but is good practice
synt_train_scaled$CHD <- relevel(synt_train_scaled$CHD, ref = "No")

set.seed(42)
knn_model_synt <- train(CHD ~ ., data = synt_train_scaled, method = "knn",
                        tuneGrid = k_grid, 
                        trControl = ctrl, 
                        metric = "Sens")

best_k_synt <- knn_model_synt$bestTune$k
best_acc_synt <- knn_model_synt$results |>
  filter(k == best_k_synt) |>
  pull(Sens)

cat("Best Sensitivity of", round(best_acc_synt, 2), "with k =", best_k_synt)

smote_predics_knn <- predict(knn_model_synt, newdata = numerical_test_knn)
smote_knn_cm <- confusionMatrix(smote_predics_knn, test$CHD, positive = "Yes")
smote_knn_cm

smote_sensitivity_knn <- smote_knn_cm$byClass["Sensitivity"]
smote_fnr_knn <- 1 - smote_sensitivity_knn
cat("FNR:", round(smote_fnr_knn, 4))

# Notes:

# Overall great improvement, with FNR diminished a lot in both cases. To signal
# some issues in the K-NN training with synthetic data: the k is fine tuned with
# accuracy without keeping track of sensitivity, 
