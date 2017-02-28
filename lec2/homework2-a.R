
setwd('/home/nmatare/projects/datascience_marketing/lec2')

# Prepare Data for Demand Analysis
library(bit64)
library(data.table)
library(lfe)
library(stargazer)
library(ggplot2)

# Prepare the data
load("Brands.RData")
load("Stores.RData")
ls() 

DT_brands = as.data.table(brands)
DT_stores = as.data.table(stores)

# Create deep copies
DT_brands = copy(DT_brands) 
DT_stores = copy(DT_stores)

# Inspect data tables
head(DT_brands)
head(DT_stores)

# Select the category and brands
# selected_module = 7012
selected_module = 1036
# selected_module = 1040

# model_name = "Detergents - Tide"
model_name = "Fruit Juice - CTL BR"
# model_name = "Orange Juice - Tropicana"


# Rank revenue based on selected module
DT_brands_1 = DT_brands[product_module_code == selected_module, rank := frankv(revenue, order = -1)]

# Omit blanks
DT_brands_1 = DT_brands[rank != ""]

# Choose only top 4 values
DT_brands_1 = subset(DT_brands_1, rank <= 4)

# Add brand_name variable
DT_brands_1 = DT_brands_1[rank == 1, brand_name := "own"][rank == 2, brand_name := "comp_1"][rank == 3, brand_name := "comp_2"][rank == 4, brand_name:= "comp_3"]

# Load Movement Data
load("brand_move_7012.RData")
load("brand_move_1036.RData")
load("brand_move_1040.RData")
DT_move = as.data.table(move)
DT_move = copy(DT_move)

# Prepare movement data 
DT_move = setnames(DT_move, "units", "quantity") #change units to quantity
DT_move = setnames(DT_move, "promo_dummy", "promotion") #change promo_dummy to promotion
DT_move = DT_move[, promotion := as.numeric(promotion)] #change data type of promotion variable


setkey(DT_brands_1, brand_code_uc)
setkey(DT_move, brand_code_uc)
DT_move = merge(DT_move, DT_brands_1[, brand_name, by = brand_code_uc]) #merge brand_name variable with move data

#isOutlier function
isOutlier <- function(x, threshold_bottom, threshold_top){
        is_outlier = rep(FALSE, times = length(x))
        median_x = median(x, na.rm = TRUE)
        is_outlier[x / median_x < threshold_bottom | x / median_x > threshold_top] = TRUE
        return(is_outlier)
}

# Run function on price data
threshold_bottom = 0.35
threshold_top = 2.5
DT_move = DT_move[, outlier := isOutlier(c(price), threshold_bottom, threshold_top), by = .(brand_code_uc, store_code_uc)]

#Tabulate outliers
DT_observations = DT_move[outlier == "TRUE", .N, by = outlier] 
length(which(DT_move[ ,outlier] == TRUE))
DT_observations

#Remove outliers
DT_move = DT_move[outlier != "TRUE"]

#Long to wide format
DT_move = dcast(DT_move, store_code_uc + week_end ~ brand_name, value.var = c("quantity", "price", "promotion"))
head(DT_move)

#Merge store information with movement data
setkey(DT_move, store_code_uc)
setkey(DT_stores, store_code_uc)
DT_move = merge(DT_move, DT_stores[, .(retailer_code, store_zip3, SMM_code, SMM_description), by = store_code_uc])

#Remove missing retailer codes
DT_move = DT_move[retailer_code != is.na(retailer_code)]

#Check number of stores in data
DT_observations = DT_move[, length(unique(retailer_code))]
DT_observations

#Create time variables/trends
DT_move = DT_move[, month := month(week_end)][, year := year(week_end)]

DT_time_Series = DT_move[ ,.N, by = .(month, year)]
DT_time_Series = DT_time_Series[order(year, month)]
DT_time_Series = DT_time_Series[, time_series := rep(1:48)]

setkey(DT_time_Series, month, year)
setkey(DT_move, month, year)
DT_move = merge(DT_move, DT_time_Series[, .(month, year, time_series)])

# Remove missing values
DT_move = DT_move[complete.cases((DT_move))]

# Data Inspection

# Histogram of own prices
ggplot(DT_move, aes(x = price_own)) + geom_histogram(binwidth = 0.01)

#Histogram of ratios of own prices relative to price of competitors
ggplot(DT_move, aes(x = price_own / price_comp_1)) + geom_histogram()
ggplot(DT_move, aes(x = price_own / price_comp_2)) + geom_histogram()
ggplot(DT_move, aes(x = price_own / price_comp_3)) + geom_histogram()

# Estimation

# log of own price as only input
fit_base = felm(log(1 + quantity_own) ~ log(price_own), data = DT_move)
summary(fit_base)

# add store fixed effects
fit_store_FE = felm(log(1 + quantity_own) ~ log(price_own) | store_code_uc, data = DT_move)
summary(fit_store_FE)

# add time trend
fit_trend = felm(log(1 + quantity_own) ~ log(price_own) + log(time_series) | store_code_uc, data = DT_move)
summary(fit_trend)

# add time trend fixed effects
fit_month_FE = felm(log(1 + quantity_own) ~ log(price_own) | store_code_uc + time_series, data = DT_move)
summary(fit_month_FE)

# stargazer
stargazer(fit_base, fit_store_FE, fit_trend, fit_month_FE,
type = "text", column.labels = c("Base", "Store FE", "Trend", "Store + year/month FE"),
dep.var.labels.include = FALSE)

# much better regression model if you control for time and store fixed effects. R^2 is much higher, indicating that there's much better explanation of the data.

rm(fit_base, fit_store_FE, fit_trend)

#add competitor prices
fit_competitors = felm(log(1 + vquantity_own) ~ log(price_own) + log(price_comp_1) + log(price_comp_2) + log(price_comp_3) | store_code_uc + time_series, data = DT_move)
summary (fit_competitors)

#add promotions for own brand
fit_promo_own = felm(log(1 + quantity_own) ~ log(price_own) + log(price_comp_1) + log(price_comp_2) + log(price_comp_3) + promotion_own| store_code_uc + time_series, data = DT_move)
summary (fit_promo_own)

#add promotions for all brands
fit_promo = felm(log(1 + quantity_own) ~ log(price_own) + log(price_comp_1) + log(price_comp_2) + log(price_comp_3) + promotion_comp_1 + promotion_comp_2 + promotion_comp_3 + promotion_own| store_code_uc + time_series, data = DT_move)
summary(fit_promo)

#doesn't change significantly

save(fit_promo, file = paste0("./Results/fit_", model_name, ".RData"))
summary_promo = summary(fit_promo)

##Profitability Analysis

source("./predict.felm.R")
predictProfit <- function(fit, move_DT, price, cost, retail_margin) {
 
  Q = predict.felm(fit, move_DT)
  profit = (exp(Q) - 1) * (price * (1 - retail_margin) - cost) # Important to subtract 1 from Q!

  return(sum(profit))
}

#retain only 2013 data
DT_move = DT_move[year != 2010 & year != 2011 & year != 2012]

# Assign variables
gross_margin = 0.38
retail_margin = 0.18
base_price = mean(DT_move$price_own)
cost = (1 - gross_margin) * (1 - retail_margin) * mean(DT_move$price_own)

#Create new data table
DT_profit = data.table(percentage_delta = c(-0.10, -0.08, -0.06, -0.04, -0.02, 0.00, 0.02, 0.04, 0.06, 0.08, 0.10))

DT_profit = DT_profit[, new_price := c((1 + percentage_delta) * base_price)]

setkey(DT_profit, percentage_delta)

#DT_profit[, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]


#There needs to better code for this!

DT_profit

predictProfit(fit_promo, DT_move, -0.10, cost, retail_margin)


DT_profit[percentage_delta == -0.10, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta == -0.08, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta == -0.06, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta == -0.04, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta == -0.02, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta ==  0.00, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta ==  0.02, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta ==  0.04, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta ==  0.06, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta ==  0.08, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]
DT_profit[percentage_delta ==  0.10, profit := predictProfit(fit_promo, DT_move, c(new_price), cost, retail_margin)]

DT_profit
save(DT_profit, file = paste0("./Results/DT_profit", model_name, ".RData"))

DT_profit[ , base_profit := predictProfit(fit_promo, DT_move, base_price, cost, retail_margin)]

# the quantity regression 

DT_profit[ , profit_ratio := profit / base_profit]
DT_profit[ , profit_incr_decr := profit - base_profit]

plot_ratio = ggplot(DT_profit, aes(x = percentage_delta, y = profit_ratio)) + geom_point(shape = 21, color = "gray30", fill = "hotpink", size = 2, stroke = 0.5, alpha = 0.5) + geom_line() + theme_bw()
plot_difference = ggplot(DT_profit, aes(x = percentage_delta, y = profit_incr_decr)) + geom_point(shape = 21, color = "gray30", fill = "hotpink", size = 2, stroke = 0.5, alpha = 0.5) + geom_line() + theme_bw()

ggsave(filename = "plot_ratio_Tropicana.png", plot_ratio)
ggsave(filename = "plot_difference_Tropicana.png", plot_difference)

