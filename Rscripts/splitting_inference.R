library(tree)
library(dplyr)

# generate data -----------------------------------------------
all = readRDS("E:/OneDrive - University of Leeds/CREDS Data/github-secure-data/lsoa_all.Rds")

all_noNA <- all[!is.na(all$MeanDomGas_11_kWh), ]
all_noNA <- all_noNA[,!sapply(all_noNA, anyNA)]
all_noNA <- all_noNA[,!names(all_noNA) %in% c("DomMet_17","MeanDomElec_17_kWh","TotDomElec_17_kWh","GasMet_11",
                                              "TotDomGas_11_kWh","cars_total","cars_miles","pu5k",
                                              "TotDomGas_11_kWh","GasMet_11","MeanDomElec_17_kWh")]

all_sub <- all_noNA[,!names(all_noNA) %in% c("MeanDomElec_11_kWh","dense_2017","pop2016",
                                             "cars_total","cars_miles",
                                             "pu5k","p5_12k","po12k","age_av","miles_av_u3",
                                             "miles_av_o13","pcars_diesel","pmiles_diesel","vans_total","vans_miles",
                                             "pmiles_car","pmiles_vans","cars_percap","miles_percap",
                                             "diesel_n",
                                             "electric diesel_n","hybrid electric_n","other_n","petrol_n","all_cars_n",
                                             "diesel_co2","electric diesel_co2","hybrid electric_co2","other_co2","petrol_co2",
                                             "northing","petrol_emissions","diesel_emissions","petrol_litres","diesel_litres",
                                             "petrol_kwh","diesel_kwh","driving_kwh","driving_kwh_percap")]

X <- data.matrix(all_sub[,!names(all_sub) %in% c("LSOA11","MeanDomGas_11_kWh")])

# I generate a Y variabile which is a linear combination of these 5 variables
Y <- all_sub$MeanDomGas_11_kWh

# obviously the lm - estimates are super good
coefficients(lm(Y ~ ., data = data.frame(X)))

# sample splitting inference ----------------------------------------------

# Split data
indexes <- sample(1:nrow(X), size = nrow(X) / 2)

# indentify important variable using only train data
# YOU SHOULD DO THIS USING THE TREE ALGORITHM
lm_model <- lm(Y ~ ., data = data.frame(X), subset = indexes)
signif_variables <- names(which(summary(lm_model)$coefficients[, 4] < 0.01))

model_tree = tree(MeanDomGas_11_kWh ~ ., data=all_sub, na.action = na.exclude)
summary(model_tree)
plot(model_tree, type = "proportional")
text(model_tree)

signif_variables <- as.character(summary(model_tree)$used)

# Estimate coef of these "important" variables using the other part of data
estimate_coefs_formula <- paste0("Y ~ ", paste(setdiff(signif_variables, "(Intercept)"), collapse = " + "))
estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = data.frame(X), subset = -indexes)
sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
res <- sample_split_coefs

# Repeat the same idea again and again
n_replicates <- 1000
# for(i in seq_len(n_replicates)) {
#   indexes <- sample(1:nrow(X), size = nrow(X) / 2)
#   
#   model_tree = tree(MeanDomGas_11_kWh ~ ., data=all_sub, na.action = na.exclude)
#   signif_variables <- as.character(summary(model_tree)$used)
#   
#   estimate_coefs_formula <- paste0("Y ~ ", paste(setdiff(signif_variables, "(Intercept)"), collapse = " + "))
#   estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = data.frame(X), subset = -indexes)
#   
#   sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
#   colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
#   res <- dplyr::bind_rows (res, sample_split_coefs)
#   
#   if (!i %% 100) print(i)
# }

rep_sats <- function(i, dat, Y){
  indexes <- sample(1:nrow(dat), size = nrow(dat) / 2)
  
  model_tree = tree::tree(MeanDomGas_11_kWh ~ ., 
                          data = dat[indexes,], 
                          na.action = na.exclude)
  
  signif_variables <- as.character(summary(model_tree)$used)
  signif_variables <- signif_variables[signif_variables != "(Intercept)"]
  
  estimate_coefs_formula <- paste0(Y," ~ ", paste(signif_variables, collapse = " + "))
  estimate_coefs <- lm(as.formula(estimate_coefs_formula), data = dat, subset = -indexes)
  
  sample_split_coefs <- data.frame(matrix(coefficients(estimate_coefs), nrow = 1))
  colnames(sample_split_coefs) <- names(coefficients(estimate_coefs))
  return(sample_split_coefs)
}

#system.time(rep_sats(X))
# results
ncores = 4
cl <- parallel::makeCluster(ncores)
res <- pbapply::pblapply(1:1000, rep_sats, dat = all_sub, Y = "MeanDomGas_11_kWh", cl = cl)
parallel::stopCluster(cl)
rm(cl)


foo <- res %>% 
  dplyr::bind_rows() %>%
  summarize_all(function(x) sum(!is.na(x)))
foo <- t(foo)
plot(foo)

summary(foo > 990)
bar <- foo[foo > 990,]

sub <- all_noNA[,names(all_noNA) %in% c("MeanDomGas_11_kWh",names(bar))]

lm1 <- lm(MeanDomGas_11_kWh ~ ., data = sub)
summary(lm1)
