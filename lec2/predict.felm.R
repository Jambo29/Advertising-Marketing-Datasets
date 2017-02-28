#  predict.felm.R
#  ------------------------------------------------------------------------------------------------
#  Guenter J. Hitsch, January 2016
#
#  Adds a predict method to the lfe package

require(lfe)
require(data.table)



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


