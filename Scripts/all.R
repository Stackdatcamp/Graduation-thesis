
## PACKAGES ####################################################################

pkgs <- c("tidyr", "readr", "ggplot2", "dplyr", "broom", "scales", 
          "caret", "kernlab", "GGally", "ggfortify", "plotROC",
          "glmnet", "kernlab")
sapply(pkgs, require, character.only = TRUE)


## PLOTTING THEME ##############################################################

theme_thesis <- theme_bw() + 
  theme(legend.background = element_blank(), 
        legend.key = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        strip.background = element_rect(fill = "#2c3e50"),
        strip.text = element_text(color = "white"),
        plot.background = element_blank(), 
        axis.line = element_line(), 
        panel.grid.minor =   element_blank(),
        panel.grid.major =   element_line(colour = "lightgrey",
                                          linetype = "dashed"),
        legend.box.background = element_rect(fill = "white"),
        text = element_text(size = 15)
  )
theme_set(theme_thesis)

## IMPORT ######################################################################

credit <- read_csv("/media/Logical_Drive/Study/Graduation Thesis/Thesis/Data/UCI_Credit_Card.csv", col_types = list(
  ID = col_skip(),
  DEFAULT = col_logical()
))

credit$EDUCATION <- factor(credit$EDUCATION, 
                           levels = 1:4,
                           labels = c("Sau đại học", "Đại học", 
                                      "Phổ thông", "Khác"))
credit$MARRIAGE <- factor(credit$MARRIAGE,
                          levels = 1:3,
                          labels = c("Đã cưới", "Độc thân", "Khác"))


## PREPROCESSING ###############################################################

pre_proc <- preProcess(credit, method = c("center", "scale"))
dummies <- dummyVars(DEFAULT ~ ., data = credit, sep = "_")

x_set <- predict(pre_proc, newdata = credit)
x_set <- predict(dummies, newdata = x_set)
y_set <- credit["DEFAULT"]

 
set.seed(0)
train_obs <- createDataPartition(credit$DEFAULT, p = 0.75)[[1]]

train_x_set <- x_set[train_obs, ]
train_y_set <- y_set[train_obs, ]

train_set <- cbind(train_x_set, train_y_set) %>%
  as_data_frame()

test_x_set <- x_set[-train_obs, ]
test_y_set <- y_set[-train_obs, ]

test_set <- cbind(test_x_set, test_y_set) %>%
  as_data_frame()

## test_set <- credit[-train_obs, ]

## x_set %>% as_data_frame() %>%
##   select(starts_with("BILL"), starts_with("PAY"), LIMIT_BAL) %>%
##   gather(col, value, everything()) %>%
##   ggplot(aes(col, value, group = col)) +
##   geom_boxplot() +
##   scale_color_manual(values = c("#00bfff", "#8b0000"))


## Exploratory #################################################################

pca_result <- prcomp(select(as_data_frame(train_x_set),
                            starts_with("PAY"),
                            starts_with("BILL"),
                            LIMIT_BAL)
                     )

pc_percent <- pca_result$sdev^2/sum(pca_result$sdev^2)*100

pca_result %>% 
  autoplot(data = train_set,
           colour = 'DEFAULT',
           loadings = TRUE, loadings.colour = 'black',
           loadings.label = TRUE, loadings.label.colour = "#005000",
           loadings.label.repel = TRUE, label.size = 3,
           alpha = 0.5) +
  geom_rug(aes(color = DEFAULT)) +
  scale_color_manual(values = c("skyblue", "darkred")) +
  labs(x = paste0("Thành phần chính 1 (giải thích ", 
                  round(pc_percent[1], 2),
                  "% phương sai)"),
       y = paste0("Thành phần chính 2 (giải thích ", 
                  round(pc_percent[2], 2),
                  "% phương sai)"),
       color = "Giá trị của biến\nDEFAULT") +
  theme(legend.position = c(0.8, 0.8))


## Lasso #######################################################################
train_downsample <- downSample(x = train_x_set,
                             y = factor(train_y_set$DEFAULT,
                                        labels = c("non_default", "default")),
                             list = TRUE)

lasso_downsample <-glmnet(train_downsample$x, train_downsample$y,
                   family = "binomial", alpha = 1)

lasso_downsample_cv <- cv.glmnet(train_downsample$x, train_downsample$y,
                      family = "binomial", alpha = 1)

lasso_full_fit <- glmnet(train_x_set, train_y_set$DEFAULT,
                         family = "binomial", alpha = 1)

lasso_full_cv <- cv.glmnet(train_x_set, train_y_set$DEFAULT,
                         family = "binomial", alpha = 1)


rbind(
  lasso_downsample$beta  %>% as.matrix() %>% t() %>% as.data.frame() %>% 
    mutate(lambda = lasso_downsample$lambda,
           model = "downsample"),
  lasso_downsample$beta  %>% as.matrix() %>% t() %>% as.data.frame() %>% 
    mutate(lambda = lasso_downsample$lambda,
           model = "fullsample")
) %>%
  gather(Varname, Beta, everything(), -lambda, -model) %>% 
  ggplot(aes(x = lambda, y = Beta, group = Varname)) +
  geom_line(aes(color = Varname)) +
  labs(x = latex2exp::TeX("Giá trị của $\\log(\\lambda)$"), 
       y = "Hệ số ước lượng", color = "Biến") +
  theme(legend.position = "top") +
  facet_grid(model ~ .)

lasso_downsample_cv$lambda.min

lasso_full_cv$lambda.min

--

data_frame(
 m1 = predict.glmnet(lasso_full_fit, newx = test_x_set,
                     s = lasso_full_cv$lambda.min)[,1],
 m2 = predict.glmnet(lasso_downsample, newx = test_x_set,
                     s = lasso_downsample_cv$lambda.min)[,1],
 d = test_y_set$DEFAULT
) %>%
  ggplot() +
  geom_roc(aes(m = m1, d = as.numeric(d)), color = "blue")+
  geom_roc(aes(m = m2, d = as.numeric(d)), color = "red")


--
result <- data_frame(
  Predict = (predict.glmnet(lasso_full_fit, newx = test_x_set,
                           s = lasso_full_cv$lambda.min)[,1] > 0.5),
  Actual = test_set$DEFAULT
) %>% table() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  mutate(color = ifelse(Predict == Actual, Freq, -1*Freq),
         percent = round(Freq/sum(Freq) * 100, 2))
result[result == TRUE] <- "Vỡ nợ"
result[result == FALSE] <- "Không vỡ nợ"

result <- data_frame(
  Predict = (predict.glmnet(lasso_downsample, newx = test_x_set,
                           s = lasso_downsample_cv$lambda.min)[,1] > 0.5),
  Actual = test_set$DEFAULT
) %>% table() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  mutate(color = ifelse(Predict == Actual, Freq, -1*Freq),
         percent = round(Freq/sum(Freq) * 100, 2))
result[result == TRUE] <- "Vỡ nợ"
result[result == FALSE] <- "Không vỡ nợ"


result %>%  
  ggplot(aes(y = Predict, x = Actual)) + 
  geom_tile(aes(fill = color), color = "black", size = 1) +
  geom_text(aes(label = paste0(Freq,"\n(", percent,"%)"))) +
  scale_fill_gradient2(low = "red", high = "light blue") +
  labs(x = "Giá trị thực", y = "Giá trị dự báo") +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

## lasso_fit$beta %>% as.matrix() %>% t() %>% as.data.frame() %>% 
##   mutate(lambda = lasso_fit$lambda) %>% 
##   gather(Varname, Beta, everything(), -lambda) %>% 
##   ggplot(aes(x = lambda, y = Beta, group = Varname)) +
##   geom_line(aes(color = Varname)) +
##   coord_trans(x = "log10")



## lasso_cv <- cv.glmnet(x_matrix, train_set$DEFAULT, 
##                       family = "binomial", alpha = 1)


## do.call(data.frame, lasso_cv[1:6]) %>% 
##   ggplot(aes(x = log(lambda), y = cvm)) +
##   geom_vline(xintercept = log(lasso_cv$lambda.min), 
##              color = "darkblue") +
##   geom_errorbar(aes(ymax = cvup, ymin = cvlo)) +
##   geom_point(color = "red") +
##   labs(y = "Deviance trung bình của kiểm định chéo",
##        x = latex2exp::TeX("$\\log(\\lambda)$"))


## SVM  ########################################################################
## svm_fit <-
##   read_rds("/media/Logical_Drive/Study/Graduation Thesis/svm_small.rds")

geom_roc()

set.seed(0)
small_sample <- sample(length(train_downsample$y), 1000)

small_set_x <- train_downsample$x[small_sample,]
small_set_y <- train_downsample$y[small_sample]

small_set_x %>% as.data.frame() %>%
  sapply(moments::skewness)

set.seed(0)
svm_linear_small <- 
  train(
    x = small_set_x, y = small_set_y,
    method = "svmLinear",
    metric = "ROC",
    tuneGrid = expand.grid(C = 10^(-3:3)),
    trControl = trainControl(method = "cv", number = 10,
                             summaryFunction=twoClassSummary,
                             classProbs = TRUE)
  )

ggplot(svm_small) +
  scale_x_log10()

write_rds(svm_linear_small, 
          "/media/Logical_Drive/Study/Graduation Thesis/Thesis/Scripts/svm_linear_small.rds")



grid <- expand.grid(sigma = c(.03, .035, 0.4),
                    C = c(0.1, 0.2, 0.3, 0.4, 0.5))
set.seed(0)
svm_rbf_small <- 
  train(
    x = small_set_x, y = small_set_y,
    method = "svmRadial",
    metric = "ROC",
    tuneGrid = grid,
    preProc = c("center","scale"),
    trControl = trainControl(method = "cv", number = 10,
                             summaryFunction=twoClassSummary,
                             classProbs = TRUE)
  )
ggplot(svm_rbf_small)
write_rds(svm_rbf_small, 
          "/media/Logical_Drive/Study/Graduation Thesis/svm_rbf_small.rds")

svm_fit <- 
  ksvm(x = train_downsample$x, y = train_downsample$y,
       C = 0.5, sigma = 0.03443114,kernel = "rbfdot")

data_frame(
  predict(svm_fit, scale(test_x_matrix)) , test_set$DEFAULT 
) %>% table()


result <- data_frame(
  Predict = ifelse(predict(svm_fit, test_x_set) == "default", TRUE, FALSE),
  Actual = test_set$DEFAULT
) %>% table() %>% as.data.frame(stringsAsFactors = FALSE) %>%
  mutate(color = ifelse(Predict == Actual, Freq, -1*Freq),
         percent = round(Freq/sum(Freq) * 100, 2))

result[result == TRUE] <- "Vỡ nợ"
result[result == FALSE] <- "Không vỡ nợ"


result %>%  
  ggplot(aes(y = Predict, x = Actual)) + 
  geom_tile(aes(fill = color), color = "black", size = 1) +
  geom_text(aes(label = paste0(Freq,"\n(", percent,"%)"))) +
  scale_fill_gradient2(low = "red", high = "light blue") +
  labs(x = "Giá trị thực", y = "Giá trị dự báo") +
  theme(legend.position = "none", 
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


# Backtest #####################################################################
test_set_dummerise$PREDICT_SVM <- 
  predict(svm_fit, scale(test_x_matrix))

levels(test_set_dummerise$PREDICT_SVM) <- c(FALSE, TRUE)

test_set_dummerise$PREDICT_SVM <- 
  as.logical(test_set_dummerise$PREDICT_SVM)


test_set_dummerise$PREDICT_LASSO <-  
  predict.glmnet(lasso_fit, newx = test_x_matrix,
                          s = lasso_cv$lambda.min)[,1] > 0.5

test_set_dummerise <- test_set_dummerise %>% 
  mutate(CORRECT_LASSO = ifelse(PREDICT_LASSO != DEFAULT, TRUE, NA),
         CORRECT_SVM = ifelse(PREDICT_SVM != DEFAULT, TRUE, NA),
         DIFFERENCE = (PREDICT_LASSO != PREDICT_SVM))

test_pca <- test_x_matrix %>% prcomp(scale = TRUE)

fortify(test_pca, data = test_set_dummerise) %>% 
  gather(type, value, CORRECT_LASSO, PREDICT_LASSO, 
         CORRECT_SVM, PREDICT_SVM) %>%
  separate(type, c("type", "model"), sep = "_") %>% 
  na.omit() %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = value), alpha = 0.1) +
  scale_color_manual(values = c("lightskyblue", 
                                "darkred")) +
  facet_grid(type ~ model) +
  theme(panel.background = element_rect())


# Difference bween 2 models
fortify(test_pca, data = test_set_dummerise) %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = DIFFERENCE), alpha = 0.5) +
  scale_color_manual(values = c("lightskyblue", 
                                "darkred"))
# Density plot
predict_dist <- data_frame(
  predict = predict.glmnet(lasso_fit, newx = test_x_matrix,
               s = lasso_cv$lambda.min)[,1],
  predict_lasso = (predict >= 0.5),
  default = test_set$DEFAULT,
  range = cut_interval(predict, length = 0.1),
  predict_svm= predict(svm_fit, scale(test_x_matrix))
)

predict_dist$predict_svm <- 
  predict_dist$predict_svm %>% as.character() %>%  
  recode("default" = TRUE, "non_default" = FALSE)

lower_thresh <- 
  predict_dist$predict[predict_dist$default == TRUE] %>% quantile(0.05)

higher_thresh <- 
  predict_dist$predict[predict_dist$default == FALSE] %>% quantile(0.95)

predict_dist$group <-
  ifelse(predict_dist$predict <= lower_thresh|
           predict_dist$predict >= higher_thresh, "Separate",
                             "Mixed")

predict_dist %>% group_by(group) %>% 
  summarise(lasso = sum(default == predict_lasso)/n(),
            svm = sum(default == predict_svm)/n())
# Looks like two model result equally on divided set:(
  
predict_dist %>% 
  ggplot(aes(x = predict)) +
  geom_density(alpha = 0.5, fill = "blue", color = "blue") +
  geom_vline(xintercept = lower_thresh) +
  geom_vline(xintercept = higher_thresh)


predict_dist %>% 
  ggplot(aes(x = predict, group = default)) +
  geom_density(aes(color = default, fill = default), alpha = 0.5) +
  geom_vline(xintercept = lower_thresh) +
  geom_vline(xintercept = higher_thresh) +
  geom_rug(aes(color = default)) +
  scale_color_manual(values = c("lightskyblue", 
                                "darkred"))+
  scale_fill_manual(values = c("lightskyblue", 
                               "darkred"))



# predict_dist %>% 
#   ggplot(aes(x = predict, group = predict_svm)) +
#   geom_density(aes(color = predict_svm, fill = predict_svm), alpha = 0.5) +
#   geom_vline(xintercept = lower_thresh) +
#   geom_vline(xintercept = higher_thresh) +
#   geom_rug(aes(color = predict_svm)) +
#   scale_color_manual(values = c("lightskyblue", 
#                                 "darkred"))+
#   scale_fill_manual(values = c("lightskyblue", 
#                                 "darkred"))
# 

library(plotROC)
rocplot <- 
  predict_dist %>% 
  ggplot(aes(d = as.numeric(default), m = predict)) + 
  geom_roc(aes(color = ..cutoffs..)) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.5) +
  labs(x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  scale_x_continuous(expand = c(0, 0), limits = c(0,1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0,1)) +
  theme(axis.line = element_blank())

rocplot + 
  annotate("text", x = .75, y = .25, 
           label = paste("AUC =", round(calc_auc(rocplot)$AUC, 5)))
