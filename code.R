library(robustbase)

setwd("D:\\seidm\\leuven\\2\\robust\\project_2")


set.seed(0913246)

traindata <- read.csv("StarGalaxy_train_RobSta.csv", row.names=1)
testdata <- read.csv("StarGalaxy_test_RobSta.csv", row.names=1)


nsamp <- 1000
mytraindata <- traindata[sample(nrow(traindata), nsamp), ]
mytestdata <- testdata[sample(nrow(traindata), nsamp), ]


################### Robust Estimation of the Full Model ########################


# Build standard non-robust logistic regression
log_glm = glm(Class ~ ., data = mytraindata, family = binomial)


# Build robust logististic regression
rob_glm = glmrob(Class ~ ., data = mytraindata, family = binomial, 
                 control= glmrobMqle.control(tcc=1.6), method= "Mqle", weights.on.x = 'robCov')


# model analytics
log_glm_summary = summary(log_glm)
rob_glm_summary = summary(rob_glm)

log_glm_is_significant = log_glm_summary$coefficients[, 4] < 0.05
rob_glm_is_significant = rob_glm_summary$coefficients[, 4] < 0.05

common_summary = cbind(round(log_glm_summary$coefficients[, colnames(log_glm_summary$coefficients)], 5),
                       ifelse(log_glm_is_significant, 'T', 'F'),
                       round(rob_glm_summary$coefficients[, colnames(rob_glm_summary$coefficients)], 5),
                       ifelse(rob_glm_is_significant, 'T', 'F'))

# significance inconsistency between the models                       
which(!(log_glm_is_significant == rob_glm_is_significant))


summarizeRobWeights(rob_glm$w.x)


colnames(common_summary) <- c('Non-robust_Estimate', 'Non-robust_Std', 'Non-robust_z_value', 'Non-robust_p', 'Non-robust_Is_significant',
                              'Robust_Estimate', 'Robust_Std', 'Robust_z_value', 'Robust_p', 'Robust_Is_significant')
