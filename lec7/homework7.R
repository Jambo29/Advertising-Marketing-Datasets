#############
# Load Config
#############
	
	options("width" = 200)
	library(bit64)
	library(data.table)
	library(glmnet)
	library(ranger)
	library(ggplot2)
	library(corrplot)
	library(knitr)
	library(data.table)
	library(glmnet)
	library(parallel)
	library(boot)
		
	username <- Sys.info()[["user"]]
	dir <- paste("/home/", username, "/projects/datascience_marketing/lec7/", sep = ""); 
	setwd(dir)

#############
# Data 
#############

	load("Customer-Development.RData")

	set.seed(1999)
	crm_DT[, training_sample := rbinom(nrow(crm_DT), 1, 0.5)]

# Describe the data
	#crmDT[mailing_indicator == 1, outcome_spend]

# Make correlation plot
	cor_matrix <- cor(crm_DT[, !c("customer_id", "mailing_indicator", "outcome_spend"), with = FALSE])

	cor_matrix[1:5, 1:5] # see first 5 corplots

	cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA

	cor_DT <- data.table(row = rep(rownames(cor_matrix), ncol(cor_matrix)),
						 col = rep(colnames(cor_matrix), each = ncol(cor_matrix)),
						 cor = as.vector(cor_matrix))
	cor_DT <- cor_DT[is.na(cor) == FALSE]
	
	remove <- cor_DT[cor > 0.95 | cor < -0.95, row] # remove correlation greater than 95%
	crm_DT <- crm_DT[ ,!remove, with = FALSE] # new DT

# Build models on sample
	
	train <- crm_DT[training_sample == 1][ ,training_sample := NULL][]
	test <- crm_DT[training_sample == 0][ ,c("outcome_spend", "training_sample") := NULL][]

	# OLS
	ols.model <- glm(outcome_spend ~., data = train)
	ols <- cv.glm(train, ols.model, K = 5) # 5 fold cross validation with OLS
	coef.ols <- coef(ols.model)

	# LASSO
	y <- as.numeric(train$outcome_spend)
	x <- data.matrix(train[ ,-grep("outcome_spend", colnames(train)), with = FALSE])
	lasso <- cv.glmnet(x = x, y = y, alpha =  1, family = "gaussian", nfolds = 5) # CV.lasso
	coef.lasso <- coef(lasso, s = "lambda.min")

	# ELASTIC NET
	mse_eval <- data.table(alphas = seq(0.1, 1, by = 0.1), mse = 0)
	for(i in 1:NROW(mse_eval)){
		elastic.net <- cv.glmnet(x = x, y = y, alpha =  mse_eval$alphas[i], family = "gaussian", nfolds = 5) # e.n
		mse_eval[i, mse := min(elastic.net$cvm)]
		print(paste("Finished", i, "of", NROW(mse_eval)))
	}

	best.alpha <- mse_eval[which.min(mse_eval$mse)]$alphas # 0.5
	elastic.net <- cv.glmnet(x = x, y = y, alpha =  0.5, family = "gaussian") # re-run cv at best alpha
	coef.elastic.net <- coef(elastic.net, s = "lambda.min")

	# RF
	random.forest <- ranger(outcome_spend ~., data = train, num.trees = 1000, write.forest = TRUE, num.threads = detectCores() - 1, verbose = FALSE)

	# Compare Coeficients 
	coefs <- cbind(coef.ols, coef.lasso, coef.elastic.net)
	colnames(coefs) <- c("OLS", "LASSO", "ELASTIC_NET")
	coefs

	# Compare OOS MSE
	yhat.ols <- predict(ols.model, newdata = test)
	yhat.lasso <- predict(lasso, newx = as.matrix(test), s = "lambda.min")
	yhat.elastic.net <- predict(elastic.net, newx = as.matrix(test), s = "lambda.min")

	getMSE <- function(yhat, true = crm_DT[training_sample == 0, outcome_spend])	return(round(mean((yhat - true) ^ 2),4))

	cbind(OLS = getMSE(yhat.ols), LASSO = getMSE(yhat.lasso), ELASTIC_NET = getMSE(yhat.elastic.net))

# Create lift charts

	# Predictive Modeling
	trainlogit <- train[ ,spend := ifelse(outcome_spend > 0, 1, 0)][ ,outcome_spend := NULL][] # make spend indicator NULL, unknown at time

	testlogit <- crm_DT[training_sample == 0][ ,c("training_sample") := NULL][]
	testlogit <- testlogit[ ,spend := ifelse(outcome_spend > 0, 1, 0)][ ,outcome_spend := NULL] # make spend indicator

	# Predict Spend or Not
	logit <- glm(spend ~., data = trainlogit, family = "binomial")
	phats <- predict(logit, newdata = testlogit, type = "response")

	# Predict Amount of spending
	linear     	<- lm(outcome_spend ~ ., data = train)
	yhats  		<- predict(linear, newdata = test)
	
	# Can't get this working 	
	linear.log 	<- lm(log(outcome_spend + 1) ~ ., data = train)
	yhats.log 	<- predict(linear.log, newdata = test)
	yhats.log <- exp(yhats.log + ((summary(linear.log)$sigma) ^ 2 / 2))

	# Conditional expectation
	conditional.expectation <- phats * yhats

	# MSE comparison
	getMSE(conditional.expectation)

# Lift tables
