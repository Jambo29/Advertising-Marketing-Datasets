#############
# Load Config
#############
	
	options("width" = 250)
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
	dir <- paste("/home/", username, "/projects/datascience_marketing/lec3/", sep = ""); setwd(dir)

	# Setup Parallel environment
	print("Setting up clusters across cores...")
	registerDoMC(detectCores() - 1) #detect number of cores to split work apart
	# cl <- makeCluster(detectCores() - 1) #make cluseter once, outside of any loop
	# clusterEvalQ(cl, {		#load libarires and packages across all nodes
	# 		.libPaths(c(.libPaths(), "/home/nmatare/R/x86_64-redhat-linux-gnu-library/3.2", '/home/nmatare/R/x86_64-redhat-linux-gnu-library/3.3'))
	# 		library(distrom)
	# 	})

	load('Products.RData')	

#############
# Data Preperation
#############

	products[ ,is_PL := str_detect(upc_descr, "CTL BR")] # create private label flag
	products <- products[department_code != 9 & !is.na(department_code)] # remove General Merchandise department
	products <- products[!product_module_code %in% c(0750, 0445:0468)] # remove Magnet data (0750) for before 2011 and (0445:0468) after 2011
	setkey(products, upc, upc_ver_uc)

	table(products[is_PL == TRUE]$upc_descr) # inspect matches

	setwd(paste(dir, "purchases/", sep = ""))
	years <- c(2004:2014)
	results <- foreach(i = years, .packages = 'data.table', .combine = 'rbind') %dopar% { # change to do %do% if you want serial computation

					load(paste("purchases_", i, ".RData", sep = ""))
					setkey(purchases, upc, upc_ver_uc)

					purchases[ ,year := year(purchase_date)]
					purchases[ ,month := month(purchase_date)]

					purchases <- merge(purchases, products[, .(upc, upc_ver_uc, is_PL)]) # merged by keys

					purchases[is_PL == TRUE, total_spend := sum(total_price_paid - coupon_value), by = .(household_code, year, month)] # as given by handout
					purchases[is_PL == FALSE, total_spend := sum(total_price_paid - coupon_value), by = .(household_code, year, month)]

					purchases[ ,all_spend := sum(total_price_paid - coupon_value), by = .(household_code, year, month)] # both private and national spending
					purchases[ ,percentage_shares := total_spend / all_spend] # percentage of private eor national spending against all spending
					purchases <- purchases[is_PL == TRUE] # keep only private label data

					print(paste("Finished", i, "|", which(i == years), "of", length(years))); gc()
					return(purchases)
	}

	shares_DT <- results[complete.cases(results)] # remove NaN values
	setkey(shares_DT, household_code, year, month)
	shares_DT <- shares_DT[ ,list(percentage_shares = head(percentage_shares, 1)), by = .(household_code, year, month)] #aggregate months together into annuals 
	setwd(dir); saveRDS(shares_DT, "shares_DT.rds") # save file

#############
# Household data preparation
#############

	load("panelists.RData")

	income_levels <- c(	2500, 6500, 9000, 11000, 13500, 17500, 22500, 27500, 32500, 37500, 42500,
						47500, 55000, 65000, 85000, 100000, 112500, 137500, 175000, 200000)

	panelists[, income := household_income]
	levels(panelists$income) = income_levels # assign levels

	panelists[, income := as.numeric(levels(income))[income]]
	panelists[, head(.SD, 1), by = household_income, .SDcols = "income"] # keyby is the same as by expect that it keys column first

	panelists[income >= 100000, income := 112500]
	setkey(panelists, household_code, panel_year) # ensure key'ed correctly

	panelists[, income := shift(income, n = 2, type = "lead"), by = household_code]

	panelists[, female_head := male_head_age == "No Such Head"] # make heads of households
	panelists[, age := male_head_birth]

	panelists[female_head == TRUE, age := female_head_birth]
	panelists[, age := panel_year - as.numeric(substr(age, 1, 4))]

	panelists[, unemployed := male_head_employment == "Not Employed for Pay"]
	panelists[female_head == TRUE, unemployed := female_head_employment == "Not Employed for Pay"]

	panelists[, education := male_head_education]
	panelists[female_head == TRUE, education := female_head_education]

	head(panelists$female_head_birth) # yep, strings
	levels(panelists$household_size)

	panelists[ ,size := ifelse(household_size == "Single Member", 1, # make size of household feature // vectorized
						ifelse(household_size == "Two Members", 2,
						ifelse(household_size == "Three Members", 3,
						ifelse(household_size == "Four Members", 4,
						ifelse(household_size == "Five Members", 5,
						ifelse(household_size == "Six Members", 6,
						ifelse(household_size == "Seven Members", 7,
						ifelse(household_size == "Eight Members", 8,
						ifelse(household_size == "Nine+ Members", 9, NA
	)))))))))]

	panelists[ ,has_children := factor(ifelse(age_and_presence_of_children == "No Children Under 18", 0, 1))] # dummy var for children or not

#############
# Merge Zillow, Shares, and Household
#############
	
	load("Zillow-Data.RData")
	shares_DT <- readRDS("shares_DT.rds")
	shares_DT[ ,panel_year := NULL] # remove old panel_year; because of data entry, its not really the 'year'
	setnames(panelists, "panelist_zip_code", "zip_code") # rename to zip_code so you can merge with zillow data
	setnames(panelists, "panel_year", "year") # rename household year to year so you can match to shares_DT

	shares <- merge(	shares_DT, # merge the product information with household panel information
						panelists[, .(household_code, year, zip_code, dma_code, projection_factor, 
									income, unemployed, education, age, size, has_children, 
									female_head, marital_status, race, hispanic_origin)])
	
	setkey(shares, household_code, zip_code, year, month) # add zip_code key
	setkey(zillow_DT, zip_code, year, month) # insure zillow_DT is keyed correctly

	shares <- merge(shares, zillow_DT[, .(zip_code, year, month, zillow_index)], all.x = TRUE) # make shares work
	shares[ ,percentage_shares := round(percentage_shares * 100, 2)] # make percentage easy to read

#############
# Data Description
#############

	shares[ ,percentage_shares_annual := mean(percentage_shares), by = .(household_code, year)]

	ggplot(data = shares, aes(percentage_shares_annual)) + geom_histogram() # histogram of percentage shares
	summary(shares[ ,percentage_shares_annual]) # summary stats

	shares[ ,percentage_shares_monthly := weighted.mean(x = percentage_shares, w = projection_factor), by = .(household_code, year, month)] # %shares per month by household, weighted by projection factor
	shares[ ,date := as.Date(ISOdate(year, month, 1))]

	ggplot(data = shares, aes(y = percentage_shares_monthly, x = date)) + 
		geom_line() +
		# geom_point() +
		annotate("rect", xmin = as.Date("2007-12-1"), xmax = as.Date("2009-6-1"), ymin = -Inf, ymax = Inf, fill = "lightblue1", alpha = 0.4) +
		scale_x_date("Year", date_labels = "%Y", date_breaks = "1 years", minor_breaks = NULL)

		shares[ ,zillow_index36mo := shift(zillow_index, n = 36, type = "lag"), by = zip_code] # get period 36 months ago
		shares[ ,zillow_index_percentage_change := round((zillow_index - zillow_index36mo) / zillow_index36mo, 2)]

		ggplot(data = shares[year == 2009 & month == 7], aes(zillow_index_percentage_change)) + geom_histogram() # histogram of percentage shares
		
#############
# Regression Analysis
#############
	
# TO DO, debug why felm returns NaN

	shares <- shares[complete.cases(shares)] 

	# Base model
	model_base <- "log(percentage_shares_monthly + 1) ~ log(income + 1) + log(zillow_index + 1) | unemployed" 
	fit_base <- felm(formula(model_base), data = shares)

	# Demographics
	model_demo <- "| income + education + age + size + has_children + female_head + marital_status + race + hispanic_origin" 
	fit_demographics <- felm(formula(paste(model_base, model_demo)), data = shares)

	# Household
	shares[ ,household_code := factor(household_code)]
	model_house <- "| household_code"
	fit_household <- felm(formula(paste(model_base, model_house)), data = shares)

	# Time Controls
	shares[ ,month_index := 12 * (year - min(year)) + month]
	shares[ (year == 2007 & month == 12 | year == 2008 | year == 2009 & month %in% 1:7), recession := 1]  # recession dummy var
	shares[!(year == 2007 & month == 12 | year == 2008 | year == 2009 & month %in% 1:7), recession := 0]

	shares[ , ':='( # factor variables that will be controled for
			recession = factor(recession),
			year = factor(year),
			month = factor(month)
	)]

	model_time <- "| month_index + recession + year + month"
	fit_time <- felm(formula(paste(model_base, model_time)), data = shares)

# TO DO; determine which is best model and add here

	# Cluster Controls
	shares[ ,dma_code := factor(dma_code)]
	model_cluster <- "| 0 | dma_code + year"
	fit_cluster <- felm(formula(paste(model_base, model_cluster)), data = shares)

	# Startgazer
	stargazer(	fit_base, fit_demographics, fit_household, fit_time, fit_cluster,
				column.labels = c("Base", "Demographics", "Household", "Time Control", "Base + Error Cluster"), 
				type = "text", dep.var.labels.include = FALSE)

#############
# Discussion
#############

	# Write up and coef interpretation