library(devtools)
load_all()
data <- import_data(test = TRUE)
merged.data <- merge_data(data, test = TRUE)
merged.data$ofi <- create_ofi(merged.data)
merged.data <- add_ofi_categories(merged.data)
unique(merged.data$Problemomrade_.FMP)
