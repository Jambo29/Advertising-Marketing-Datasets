#############
# Load Config
#############
	
	options("width" = 250)
	library(bit64)
	library(data.table)
	library(lfe)
	library(stargazer)
	library(ggplot2)

	username <- Sys.info()[["user"]]
	dir <- paste("/home/", username, "/projects/datascience_marketing/lec2/", sep = ""); setwd(dir)

	load("Brands.RData")
	load('Stores.RData')

	selected_module = 7012
	model_name = "Detergents-Tide"

	brand_name 	<- 	brands[product_module_code == selected_module][ , , sort(revenue, decreasing = TRUE)][ ,brand_code_uc][1:4]

	brand_own 	<- 	brand_name[1] # top ranked brand
	comp_1 		<- 	brand_name[2] # competittor 1
	comp_2 		<- 	brand_name[3] # competittor 2
	comp_3 		<-  brand_name[4] # competittor 3

	top_brands 	<- cbind.data.frame(brand_name = c('brand_own', 'comp_1', 'comp_2', 'comp-3'), brand_code_uc = brand_name)

	load("brand_move_7012.RData")
	# load("brand_move_1036.RData")
	# load("brand_move_1040.RData")

	setnames(move, "units", "quantity")
	setnames(move, "promo_dummy", "promotion")

	move[, brand_code_uc := as.numeric(brand_code_uc)]
	move[ ,brand_name := 	ifelse(brand_code_uc == brand_own, "brand_own", 
							ifelse(brand_code_uc == comp_1, "comp_1", 
							ifelse(brand_code_uc == comp_2, "comp_2", 
							ifelse(brand_code_uc == comp_3, "comp_3", 
								NA))))]

	move[ ,promotion := as.numeric(promotion)]

	isOutlier <- function(x, threshold_bottom, threshold_top){
					is_outlier = rep(FALSE, times = length(x))
					median_x = median(x, na.rm = TRUE)
					is_outlier[x/median_x < threshold_bottom | x/median_x > threshold_top] = TRUE
					return(is_outlier)
	}

	threshold.bottom = 0.35
	threshold.top = 2.5

	move[ ,outlier := isOutlier(price, threshold.bottom, threshold.top), by = .(brand_code_uc, store_code_uc)]

	table(move[ ,outlier]) # number of outliers
	move <- move[outlier != TRUE] # remove outliers

	move <- dcast(move, store_code_uc + week_end ~ brand_name, value.var = c("quantity", "price", "promotion")) # cast long to wide
	stores <- stores[!is.na(retailer_code)][ ,c("store_code_uc", "retailer_code", "SMM_code", "SMM_description"), with = FALSE] # remove NAs and select columns

	data <- merge(move, stores, by = 'store_code_uc')
	length(unique(data[ ,store_code_uc])) # number of stores

	data[ ,time_trend_month := month(week_end)] # get time trends and vars
	data[ ,time_trend_year := year(week_end)]

	data[ ,time_trend_fixed := .N, by = .(time_trend_month, time_trend_year)]
	data <- data[order(time_trend_year, time_trend_month)]
	data[ ,time_trend_fixed := rep(1:48)]

	data <- data[complete.cases(data)]

	ggplot(data = data, aes(price_brand_own)) + geom_histogram() #own price 
	ggplot(data = data, aes(price_brand_own / price_comp_1)) + geom_histogram() # ratio to comps
	ggplot(data = data, aes(price_brand_own / price_comp_2)) + geom_histogram()
	ggplot(data = data, aes(price_brand_own / price_comp_3)) + geom_histogram()

	# demand estimation

	# model 1
	fit_base <- felm(log(1 + quantity_brand_own) ~ log(1 + price_brand_own), data = data)

	# model 2
	fit_store_FE <- felm(log(1 + quantity_brand_own) ~ log(1 + price_brand_own) | store_code_uc, data = data)

	# model 3
	fit_trend <- felm(log(1 + quantity_brand_own) ~ log(1 + price_brand_own) + log(time_trend_fixed) | store_code_uc, data = data)
	
	# model 4
	fit_month_FE <- felm(log(1 + quantity_brand_own) ~ log(1 + price_brand_own) | store_code_uc + time_trend_fixed, data = data)

	stargazer(fit_base, fit_store_FE, fit_trend, fit_month_FE, type = "text", column.labels = c("Base", "Store FE", "Trend", "Store + year/month FE"), dep.var.labels.include = FALSE)

	rm(fit_base, fit_store_FE, fit_trend)

	#add competitor prices
	fit_competitors <- felm(log(1 + quantity_brand_own) ~ log(price_brand_own) + log(price_comp_1) + log(price_comp_2) + log(price_comp_3) | store_code_uc + time_trend_fixed, data = data)
	summary (fit_competitors)

	#add promotions for own brand
	fit_promo_own <- felm(log(1 + quantity_brand_own) ~ log(price_brand_own) + log(price_comp_1) + log(price_comp_2) + log(price_comp_3) + promotion_brand_own | store_code_uc + time_trend_fixed, data = data)
	summary (fit_promo_own)

	#add promotions for all brands
	fit_promo <- felm(log(1 + quantity_brand_own) ~ log(price_brand_own) + log(price_comp_1) + log(price_comp_2) + log(price_comp_3) + promotion_comp_1 + promotion_comp_2 + promotion_comp_3 + promotion_brand_own| store_code_uc + time_trend_fixed, data = data)
	summary(fit_promo)

	save(fit_promo, file = paste0("./Results/fit_", model_name, ".RData"))
	summary_promo <- summary(fit_promo)

	source("./predict.felm.R")

	# Usage:
	# -------------------------------------------------------------------------------------------------
	# fit must be the output from an felm regression
	# newdata must be a data frame or data.table with all the variables contained in the 
	# original regression!

	predict.felm <- function(fit, newdata) {
	   
	   if (class(fit) != "felm") stop("'fit' is not a felm object")
	   if (!("data.frame" %in% class(newdata))) stop("'newdata' must be a data.frame or data.table")
	   
	   setDT(newdata)

	   # Predict output based on estimated coefficients, not inclucing fixed effects
	   var_names = rownames(fit$coefficients)
	   original_formula = paste("~ 0 +", paste(var_names, collapse = " + "))
	   X = model.matrix(formula(original_formula), newdata)
	   y = X %*% fit$coefficients
	   
	   # Add fixed effect values to prediction
	   FE = as.data.table(getfe(fit))
	   cols = c("fe", "idx")
	   FE[, (cols) := lapply(.SD, as.character), .SDcols = cols]
	   
	   for (name in unique(FE$fe)) {
	      fe_DT = newdata[, name, with = FALSE]
	      fe_DT[, obs_no := .I]
	      setnames(fe_DT, name, "idx")
	      fe_DT[, idx := as.character(idx)]
	      
	      fe_DT = merge(fe_DT, FE[fe == name, .(idx, effect)], by = "idx")
	      fe_DT = fe_DT[order(obs_no)]
	      
	      y = y + fe_DT$effect
	   }
	   
	   return(y)
	} 

	predictProfit <- function(fit, move_DT, price, cost, retail_margin) {
					Y = predict.felm(fit, move_DT) # Y is the prediction of log(1+Q)
					profit = (exp(Y)-1)*(price*(1-retail_margin) - cost) # Important to subtract 1 from exp(Q) = 1+Q!
					return(sum(profit))
	}

	# Only retain for year 2013
	subdata <- data[time_trend_year == 2013]

	gross_margin <- 0.38
	retail_margin <- 0.18
	base_price <- mean(subdata$price_brand_own, na.rm = TRUE)
	cost <- (1 - gross_margin) * (1 - retail_margin) * mean(subdata$price_brand_own, na.rm = TRUE)
	percentage_delta <- seq(-0.1, 0.1, 0.02)

	prices <- base_price * (1 + percentage_delta)

	results <- list()	
	for(price in prices)  results[[price]] <- predictProfit(fit = fit_promo, move_DT = subdata, price = price, cost = 0.07, retail_margin = 0.18) # predict the prices
	
	# TO DO plot