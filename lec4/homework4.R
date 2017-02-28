#############
# Load Config
#############
	
	options("width" = 100)
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

	username <- Sys.info()[["user"]]
	dir <- paste("/home/", username, "/projects/datascience_marketing/lec4/", sep = ""); 
	dir <- "/home/johnconnor/Documents/Education/Chicago_Booth/Classes/37105_Data_Science_for_Decision_Making/datascience_marketing/lec4/"
	setwd(dir)

	selected_module = 7260
	selected_brand = 526996
	model_name = "Toilet-Tissue-Charmin"

#############
# Data Preparation
#############
	
	load('Brands.RData')
	load('Stores-DMA.RData')

	setwd(paste(dir, "AdIntel", sep = ""))
	load(paste(selected_module, '.RData', sep = ""))

	setwd(paste(dir, "RMS", sep = ""))
	load(paste(selected_module, '.RData', sep = ""))

	setnames(move, "units", "quantity")
	setnames(move, "promo_percentage", "promotion")

	# get price and quanity of competitors and quantities
	move[brand_code_uc == selected_brand, brand_name := 'own']
	move[brand_code_uc != selected_brand, brand_name := 'comp']
	move[ ,brand_code_uc := NULL] # no longer needed
	setkey(move, brand_name, store_code_uc, week_end)

	move[ ,':='(price = mean(price), promotion = mean(promotion), quantity = sum(quantity)), by = .(brand_name, store_code_uc, week_end)] # mean price and total quanity by brand, by store and by week
	move <- unique(move[ ,.(brand_name, store_code_uc, week_end, price, promotion, quantity)]) # remove duplicated comptitors (3 exist because of sum/mean function)

	setkey(stores, store_code_uc)
	stores_dma <- unique(stores[, .(store_code_uc, dma_code)])
	move <- merge(move, stores_dma) # uniquely merge dma_code with move data

	setkey(adv_DT, brand_code_uc, dma_code, week_start)
	brands <- unique(adv_DT$brand_code_uc)
	dma_codes <- unique(adv_DT$dma_code)
	weeks <- seq(from = min(adv_DT$week_start), to = max(adv_DT$week_start), by = "week")

	adv_DT <- adv_DT[CJ(brands, dma_codes, weeks)]
	adv_DT[is.na(adv_DT)] <- 0

	# get price and quanity of competitors and quantities
	adv_DT[brand_code_uc == selected_brand, brand_name := 'own']
	adv_DT[brand_code_uc != selected_brand, brand_name := 'comp']
	adv_DT[ ,brand_code_uc := NULL] # no longer needed
	setkey(adv_DT, brand_name, dma_code, week_start)

	adv_DT[ ,':='(	national_grp = sum(national_grp), local_grp = sum(local_grp), 
					grp = sum(national_grp, local_grp)), by = .(brand_name, dma_code, week_start)] # mean price and total quanity by brand, by store and by week
	adv_DT[ ,local_occ := NULL]
	adv_DT <- unique(adv_DT[ ,.(brand_name, dma_code, week_start, grp)]) # remove duplicated comptitors (3 exist because of sum/mean function)

	#calc ad stock
	N_lags = 52
	delta = 0.9

	geom_weights <- cumprod(c(1.0, rep(delta, times = N_lags)))
	geom_weights <- sort(geom_weights)
	tail(geom_weights)

	setkey(adv_DT, brand_name, dma_code, week_start)

	library(RcppRoll)
	adv_DT[, adstock := roll_sum(log(1 + grp), n = N_lags + 1, weights = geom_weights, normalize = FALSE, align = "right", fill = NA), by = .(brand_name, dma_code)]

	#merge data
	adv_DT[, week_end := week_start + 5]

	setkey(move, brand_name, dma_code, week_end)
	setkey(adv_DT, brand_name, dma_code, week_end)

	move <- merge(move, adv_DT)
	move <- dcast(move, dma_code + store_code_uc + week_end ~ brand_name, value.var = c("quantity", "price", "promotion", "adstock"))
	move <- move[complete.cases(move)]

	move[ ,month_index := 12 * (year(week_end) - min(year(week_end))) + month(week_end)]
	
	code = 602
	name = 'CHIAGO IL'

	# plot
	ggplot(data = adv_DT[dma_code == code], aes(y = grp, x = week_end)) + geom_line(color = 'blue')
# TO DO, not sure if this is what he means by plot

	# normalize
	adv_DT[ ,normalized_grp := 100 * grp / mean(grp), by = .(dma_code)]
	ggplot(data = adv_DT[dma_code == code], aes(normalized_grp)) + geom_histogram()

	# fit base
	move[ ,dma_code := as.factor(dma_code)] # control for store
	move[ ,store_code_uc := as.factor(store_code_uc)] # control for store
	model_base <- "log(quantity_own + 1) ~ log(price_own) + log(price_comp) + promotion_own + promotion_comp | store_code_uc + month_index"
# TO DO, quantity should be own right?	
	fit_base <- felm(formula(model_base), data = move)

	# fit adstock
	model_adstock <- "log(quantity_own + 1) ~ log(price_own) + log(price_comp) + promotion_own + promotion_comp + adstock_own + adstock_comp | store_code_uc + month_index"
	fit_adstock <- felm(formula(model_adstock), data = move)

	# fit fixed time effects
	model_time <- "log(quantity_own + 1) ~ log(price_own) + log(price_comp) + promotion_own + promotion_comp + adstock_own + adstock_comp | store_code_uc "
	fit_time <- felm(formula(model_time), data = move)

	# Stargazer
	stargazer(	fit_base, fit_adstock, fit_time, 
				column.labels = c("Base", "Adstock", "Time Fixed"), 
				type = "text", dep.var.labels.include = FALSE)

	# Border Strategy
	stores[, border_name := as.factor(border_name)]
	stores <- stores[on_border == TRUE]

	setkey(move, store_code_uc)
	move <- merge(move, stores[on_border == TRUE, .(store_code_uc, border_name)], allow.cartesian = TRUE)

	# Advertisment model stores fixed efedts with order/name/time
	model_advertisment <- "log(quantity_own + 1) ~ log(price_own) + log(price_comp) + promotion_own + promotion_comp | store_code_uc + border_name:month_index"
	fit_advertisment <- felm(formula(model_advertisment), data = move)

	model_cluster <- "log(quantity_own + 1) ~ log(price_own) + log(price_comp) + promotion_own + promotion_comp | store_code_uc + border_name:month_index | 0 | dma_code"
	fit_cluster <- felm(formula(model_cluster), data = move)

	# Stargazer
	stargazer(	fit_advertisment, fit_cluster, 
				column.labels = c("Advertisment", "Cluster"), 
				type = "text", dep.var.labels.include = FALSE)

	# Optional
	N_lags = 52
	delta = 1:9 / 10
	results <- list()
# TO DO getting wierd predictions

	for(d in delta){

		dir <- paste("/home/", username, "/projects/datascience_marketing/lec4/", sep = ""); setwd(dir)
		load('Brands.RData')
		load('Stores-DMA.RData')

		setwd(paste(dir, "AdIntel", sep = ""))
		load(paste(selected_module, '.RData', sep = ""))

		setwd(paste(dir, "RMS", sep = ""))
		load(paste(selected_module, '.RData', sep = ""))

		setnames(move, "units", "quantity")
		setnames(move, "promo_percentage", "promotion")

		# get price and quanity of competitors and quantities
		move[brand_code_uc == selected_brand, brand_name := 'own']
		move[brand_code_uc != selected_brand, brand_name := 'comp']
		move[ ,brand_code_uc := NULL] # no longer needed
		setkey(move, brand_name, store_code_uc, week_end)

		move[ ,':='(price = mean(price), promotion = mean(promotion), quantity = sum(quantity)), by = .(brand_name, store_code_uc, week_end)] # mean price and total quanity by brand, by store and by week
		move <- unique(move[ ,.(brand_name, store_code_uc, week_end, price, promotion, quantity)]) # remove duplicated comptitors (3 exist because of sum/mean function)

		setkey(stores, store_code_uc)
		stores_dma <- unique(stores[, .(store_code_uc, dma_code)])
		move <- merge(move, stores_dma) # uniquely merge dma_code with move data

		setkey(adv_DT, brand_code_uc, dma_code, week_start)
		brands <- unique(adv_DT$brand_code_uc)
		dma_codes <- unique(adv_DT$dma_code)
		weeks <- seq(from = min(adv_DT$week_start), to = max(adv_DT$week_start), by = "week")

		adv_DT <- adv_DT[CJ(brands, dma_codes, weeks)]
		adv_DT[is.na(adv_DT)] <- 0

		# get price and quanity of competitors and quantities
		adv_DT[brand_code_uc == selected_brand, brand_name := 'own']
		adv_DT[brand_code_uc != selected_brand, brand_name := 'comp']
		adv_DT[ ,brand_code_uc := NULL] # no longer needed
		setkey(adv_DT, brand_name, dma_code, week_start)

		adv_DT[ ,':='(national_grp = sum(national_grp), local_grp = sum(local_grp), grp = sum(national_grp, local_grp)), by = .(brand_name, dma_code, week_start)] # mean price and total quanity by brand, by store and by week
		adv_DT[ ,local_occ := NULL]
		adv_DT <- unique(adv_DT[ ,.(brand_name, dma_code, week_start, grp)]) # remove duplicated comptitors (3 exist because of sum/mean function)

		#calc ad stock
		geom_weights <- cumprod(c(d, rep(delta, times = N_lags)))
		geom_weights <- sort(geom_weights)
		setkey(adv_DT, brand_name, dma_code, week_start)
		adv_DT[, adstock := roll_sum(log(1 + grp), n = N_lags + 1, weights = geom_weights, normalize = FALSE, align = "right", fill = NA), by = .(brand_name, dma_code)]

		#merge data
		adv_DT[, week_end := week_start + 5]

		setkey(move, brand_name, dma_code, week_end)
		setkey(adv_DT, brand_name, dma_code, week_end)

		move <- merge(move, adv_DT)
		move <- dcast(move, dma_code + store_code_uc + week_end ~ brand_name, value.var = c("quantity", "price", "promotion", "adstock"))
		move <- move[complete.cases(move)]
		move[ ,month_index := 12 * (year(week_end) - min(year(week_end))) + month(week_end)]
	
		# normalize
		adv_DT[ ,normalized_grp := 100 * grp / mean(grp), by = .(dma_code)]

		# fit base
		move[ ,store_code_uc := as.factor(store_code_uc)] # control for store
		model_base <- "log(quantity_own + 1) ~ log(price_own + 1) + log(price_comp + 1) + log(promotion_own + 1) + log(promotion_comp + 1) | store_code_uc + month_index"

		#fit model
		model_base <- "log(quantity_own + 1) ~ log(price_own + 1) + log(price_comp + 1) + log(promotion_own + 1) + log(promotion_comp + 1) | store_code_uc + month_index"
		fit_base <- felm(formula(model_base), data = move)
		sqrt(mean((fit_base$fitted.values - log(move$quantity_own + 1) ^ 2)))

tail(log(move$quantity_own + 1))
tail(fit_base$fitted.values)
sqrt(mean((fit_base$residuals)^2))

exp(tail(fit_base$fitted.values))

	}