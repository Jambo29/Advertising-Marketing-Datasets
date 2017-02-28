#############
# Load Config
#############
	
	options("width" = 200)
	library(bit64)
	library(data.table)
	library(lfe)
	library(psych)
	library(stargazer)
	library(ggplot2)
	library(stringr)
	library(foreach)
	library(doMC)
	library(parallel)
	library(broom)
	library(knitr)
	library(Hmisc)
	library(erer)
	
	username <- Sys.info()[["user"]]
	dir <- paste("/home/", username, "/projects/datascience_marketing/lec6/", sep = ""); 
	setwd(dir)

#############
# Data 
#############

	load('Cell2Cell.RData')

#############
# Model Estimation
#############

	cell2cell_DT <- cell2cell_DT[complete.cases(cell2cell_DT)] # clean NA
	test  <- cell2cell_DT[calibrat == 0] 
	train <- cell2cell_DT[calibrat == 1]
	test[ ,calibrat := NULL]
	train[ ,calibrat := NULL]

	fit <- glm(churn ~., data = train, family = 'binomial', x = TRUE) # logit

	results_DT <- as.data.table(tidy(fit))
	kable(results_DT, digits = 5)

	pe_hat <- train[, mean(churn)]
	pv_hat <- test[, mean(churn)]

	offset_var <- (log(pe_hat) - log(1 - pe_hat)) - (log(pv_hat) - log(1 -pv_hat))
	train[ ,offset_var := offset_var]
	test[ ,offset_var := 0] # validation offset set to 0 in testing sample

	fit_offset <- glm(churn ~ offset(offset_var) + ., data = train, family = 'binomial', x = TRUE) # get NA for offset intercept...

	pred.prob <- predict(fit_offset, newdata = test, type = 'response')

	mean(pred.prob) # average predicted 
	pv_hat # average observed

#############
# Lift 
#############

	liftTable <- function(pred, response, segments = 20){
					data <- as.data.table(cbind.data.frame(pred, response))
					data <- data[order(pred, decreasing = TRUE)] # sort from highest prob to lowest

					data[ ,group_interval := cut_number(data$pred, n = segments)]
					data[, score_group := as.integer(group_interval)]

					data[ ,num_customers := .N, by = score_group]
					data[response == 1, num_customers_bought := .N, by = score_group]
					data[is.na(num_customers_bought), num_customers_bought := 0] # if na, then make 0
					data[ ,mean_pred := mean(pred), by = score_group]
					data[ ,mean_response := mean(response), by = score_group]

					data[ ,phat := num_customers_bought / num_customers, by = score_group] # where phat is the proportion of successes in Bernoulli trial
					data[ ,mean_pred_upper := phat + 1.96 * sqrt( (1 / .N) * phat * (1 - phat)), by = score_group]
					data[ ,mean_pred_lower := phat - 1.96 * sqrt( (1 / .N) * phat * (1 - phat)), by = score_group]

					avg_response_rate <- data[ ,mean(pred)]
					data[ ,lift_factor := 100 * mean_pred / avg_response_rate, by = score_group]
					return(data)
	}

	lift.table <- liftTable(pred.prob, test[ ,churn], segments = 20)

	ggplot(data = lift.table, aes(x = score_group, y = mean_response)) +
		geom_errorbar(aes(ymin = mean_pred_lower, ymax = mean_pred_upper)) +
		geom_point(shape = 21, color = "gray30", fill = "red", size = 2.5)

	ggplot(data = lift.table, aes(x = score_group, y = lift_factor)) + 
		geom_line() + 
		geom_hline(yintercept = 100)

#############
# Customer Churn
#############

	results_DT[ ,marginal_effect := maBina(fit, x.mean = TRUE, digits = 4)$out$effect]
	results_DT[ ,marginal_effect2 := round(estimate * mean(pred.prob) * (1 - mean(pred.prob)), 4)] # do it this way?

	sd_list <- lapply(cell2cell_DT, sd)
	sd_DT <- data.table(term = names(sd_list),
						std_dev = unlist(sd_list))

	results_DT <- merge(results_DT, sd_DT, by = 'term')

#############
# Economics of Churn Management
#############

	discount.rate <- 0.10
	groups <- 10
	gamma <- 0.50

	test[ ,predicted_churn := pred.prob] # add predicted churn to test data

	test[ ,annual_predicted_churn := 1 - ((1 - predicted_churn) ^ 12)]
	test[ ,annual_revenue := revenue * 12]

	lift.table <- liftTable(test[,annual_predicted_churn], test[ ,churn], segments = groups)
	test[ ,score_group := lift.table[ ,score_group]]
	test[ ,mean_annual_churn := lift.table[ ,mean_pred]] # mean annual predicted churn by score group

	test[ ,yr1_mean_annual_churn := (mean_annual_churn * (1 - mean_annual_churn) ^ 1)/(1 + discount.rate) ^ 1] #note mean_annual_churn is year 0
	test[ ,yr2_mean_annual_churn := (mean_annual_churn * (1 - mean_annual_churn) ^ 2)/(1 + discount.rate) ^ 2] 
	test[ ,yr3_mean_annual_churn := (mean_annual_churn * (1 - mean_annual_churn) ^ 3)/(1 + discount.rate) ^ 3] 
	test[ ,yr4_mean_annual_churn := (mean_annual_churn * (1 - mean_annual_churn) ^ 4)/(1 + discount.rate) ^ 4] 
	test[ ,yr5_mean_annual_churn := (mean_annual_churn * (1 - mean_annual_churn) ^ 5)/(1 + discount.rate) ^ 5] 
	test[ ,life_time_value := rowSums(.SD), .SDcols = grep("mean_annual", colnames(test))[-1]] # remove year 0

	test[ ,base_churn := 1- ((gamma * 1) + ((1 - gamma) * (1 - mean_annual_churn)))]
	test[, churn_yr_1 := (base_churn * (1 - base_churn)) / (1 + discount.rate) ^ 1]
	test[, churn_yr_2 := (base_churn * (1 - churn_yr_1) * (1 - mean_annual_churn)) / (1 + discount.rate) ^ 2]
	test[, churn_yr_3 := (base_churn * (1 - churn_yr_1) * ((1 - mean_annual_churn) ^ 2)) / (1 + discount.rate) ^ 3]
	test[, churn_yr_4 := (base_churn * (1 - churn_yr_1) * ((1 - mean_annual_churn) ^ 3)) / (1 + discount.rate) ^ 4]
	test[, churn_yr_5 := (base_churn * (1 - churn_yr_1) * ((1 - mean_annual_churn) ^ 4)) / (1 + discount.rate) ^ 5]

	test[ ,life_time_value_churn := rowSums(.SD), .SDcols = grep("churn_yr_", colnames(test))] 
	test[ ,economic_val := life_time_value - life_time_value_churn]

