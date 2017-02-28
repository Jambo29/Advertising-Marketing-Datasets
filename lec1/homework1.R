#############
# Load Config
#############
	
	options("width" = 250)
	library(data.table)
	library(ggplot2)
	library(bit64)

	username <- Sys.info()[["user"]]
	dir <- paste("/home/", username, "/projects/datascience_marketing/lec1/", sep = ""); setwd(dir)

	load("purchases_beverages.RData")
	load('products_beverages.RData')

#############
# Clean Data
#############

	setkey(purchases, upc, upc_ver_uc)
	setkey(products, upc, upc_ver_uc)
	purchases <- merge(purchases, products[, .(upc, upc_ver_uc, product_group_code)]) # merge on the aformetioned keys

	selected_groups <- c(507, 1503, 1508, 2006)
	purchases <- purchases[product_group_code %in% selected_groups]
	products <- products[product_group_code %in% selected_groups]

	purchases[, product_group_code := NULL] # remove column
	purchases[, purchase_date := as.Date(purchase_date)] # change to date

	anyNA(purchases) # NA check
	anyNA(products)

	not_missing <- complete.cases(products)
	table(not_missing)
	products <- products[not_missing]

	products[brand_descr == "COCA-COLA CLASSIC R", brand_descr := "COCA-COLA R"]
	products[brand_descr == "MTN DEW R", brand_descr := "MOUNTAIN DEW R"]

	setkey(purchases, household_code, purchase_date, upc, upc_ver_uc) # rekey everything
	save(purchases, file = "purchases_beverages.RData") # save stuff again 
	save(products, file = "products_beverages.RData")

#############
# Sampling Example
#############

	purchases_sub <- purchases[sample(.N, 4000000)]

	N_households <- length(unique(purchases$household_code))
	N_subsample <- round(0.25 * N_households)
	household_code_sub <- sample(unique(purchases$household_code), N_subsample)

	purchases_sub_hh <- purchases[household_code %in% household_code_sub] # slower
	purchases_sub_hh_a <- purchases[.(household_code_sub)] # uses key to subset data; note the .() subset

	rm(purchases_sub) 
	rm(list = c("purchases_sub_hh", "purchases_sub_hh_a"))

#############
# Variable Inspection
#############

	# by = .(upc, upc_ver_uc), note UPCs are not unique, they're unique in combination with a versioning
	columns <- c("dataset_found_uc", "brand_descr", "product_group_descr", "department_descr", "product_module_descr", "upc_descr")
	for(c in columns) print(table(products[ , c, with = FALSE])) # answer to question, explore the data 

	module_DT <- products[, head(.SD, 1), by = product_module_code, .SDcols = c("product_module_descr", "product_group_descr")] # gets the first produce_model and groups it with the description
	module_DT[order(product_group_descr)] # reorder

#############
# Prepare Data
#############

	load("purchases_beverages.RData")
	load('products_beverages.RData')

	purchases[, year := year(purchase_date)] # grap the year out of purchase_date and put it into year
	purchases <- purchases[year != 2003, ] # remove the erronous 2003 purchases

	# products[,category := 	ifelse(product_module_descr == "SOFT DRINKS - CARBONATED", "carbonated_soft_drinks", # table(products[,product_module_descr])
	# 						ifelse(product_module_descr == "SOFT DRINKS - LOW CALORIE", "diet_soft_drinks", 
	# 						ifelse(product_module_descr == "WATER-BOTTLED", "bottled_water", "other")))] # vectorized ifelse

	table(products[, category]) # number of observations per category

	purchases <- merge(purchases, products[, .(upc, upc_ver_uc, category, multi, size1_amount, size1_units)]) # merge data.tables together
	table(purchases[, size1_units]) # number of observations per category

	purchases <- purchases[size1_units != 'CT'] # remove count 'CT' from data.table // note don't add mutli here because it's used later
	purchases[ ,volume := 	ifelse(size1_units == 'OZ', multi * size1_amount * 1, # keep oz as oz
							ifelse(size1_units == 'QT', multi * size1_amount * 32, NA))] # convert quarters down to ounces
	purchases[ ,volume := volume / 128] # 128 ounces in gallon, so convert to callong

	purchases[ ,no_households := length(unique(household_code)), by = year]
	table(purchases[ ,no_households])

#############
# Category-level analysis
#############

	purchases_category <- purchases[, .(spend = sum(total_price_paid - coupon_value), 
										purchase_volume = sum(volume),
										no_households = head(no_households, 1)),
										keyby = .(category, year)]

	purchases_category[ ,':='	(per_capita_spend = spend / no_households,
								 per_capita_volume = purchase_volume / no_households
						)]

	ggplot(	data = purchases_category, aes(x = year, y = purchase_volume, group = category, color = factor(category))) + 
			geom_line() +
			ggtitle("Yearly per capita purchase volume") +	ylab("Category") + xlab("Year") + labs(color = 'Category')# plot the first graph

	normalizeVolume <- function(x) {return((x - head(x, 1)) / (max(x) - min(x)) + 1)} # normalize the data to the first observation of the dataset and set to 1
	purchases_category[ ,norm_vol := normalizeVolume(purchase_volume), by = category]

	ggplot(	data = purchases_category, aes(x = year, y = norm_vol, group = category, color = factor(category))) + 
			geom_line() +
			ggtitle("Yearly per capita purchase volume (normalized)") +	ylab("Category") + xlab("Year") + labs(color = 'Category') # plot the first graph

	purchases_category[ ,sum(purchase_volume), by = .(category)]

	purchases[ ,NROW(.SD), by = .(category)]

#############
# Brand-level analysis
###########

	purchases <- merge(purchases, products[, .(upc, upc_ver_uc, brand_descr)]) # merge data.tables together

	brand_summary <- purchases[, .(spend = sum(total_price_paid * quantity)), by = .(category, brand_descr)]
	brand_summary[, rank := frankv(spend, order = -1), by = category]

	merge(purchases, brand_summary[, .(category, brand_descr)])

