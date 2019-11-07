# heatline results
if(dir.exists("E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data")){
  secure_path <- "E:/Users/earmmor/OneDrive - University of Leeds/CREDS Data"
} else {
  secure_path <- "E:/OneDrive - University of Leeds/CREDS Data"
}

all = readRDS(paste0(secure_path,"/github-secure-data/lsoa_all.Rds"))

res_gas <- readRDS("data/importance_gas_tree.Rds")
top_gas <- rownames(res_gas)[res_gas[,1] > 900]
top_gas <- top_gas[top_gas != "(Intercept)"]

model_gas <- lm(as.formula(paste0("MeanDomGas_11_kWh ~ ",paste(top_gas, collapse = " + "))),
                data = all)
summary(model_gas)
plot(all$MeanDomGas_11_kWh[!is.na(all$MeanDomGas_11_kWh)], predict(model_gas),
     xlab = "Actual mean gas consumption (kWh)",
     ylab = "Predicted consumption")
abline(0,1, col = "red")
