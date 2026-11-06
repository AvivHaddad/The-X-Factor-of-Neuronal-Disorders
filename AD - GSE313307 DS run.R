#AD_GSE313307 <-getGEO("GSE313307", GSEMatrix = TRUE, getGPL = FALSE)
#AD_GSE313307 <- AD_GSE313307[[1]]
#saveRDS(AD_GSE313307,"C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE313307")
AD_GSE313307 <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE313307")
#getGEOSuppFiles("GSE313307")
#files <- list.files("GSE313307", full.names = TRUE)
#raw_file <- files[grepl("raw", files, ignore.case = TRUE)]
#AD_GSE313307_counts <- read.csv(raw_file, row.names = 1, check.names = FALSE)
#saveRDS(AD_GSE313307_counts, "C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE313307_raw_counts")
AD_GSE313307_counts <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE313307_raw_counts")

#AD_GSE313307_results <- get_GEO("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE313307", sex = "gender:ch1",
#                                diagnosis = "infection:ch1")

#AD_GSE313307_df <-readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/df_AD_GSE313307")
#titel <- AD_GSE313307@phenoData$description
#titel <- gsub("Library name: ","",titel)
#AD_GSE313307_df$title <- titel
#saveRDS(AD_GSE313307_df,"C:/Users/avive/Documents/GitHub/X_Factor/AD/df_AD_GSE313307")
AD_GSE313307_df <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/df_AD_GSE313307")
#AD_GSE313307_train_ds <- as.data.frame(t(AD_GSE313307_counts))
#AD_GSE313307_train_ds$group <- AD_GSE313307_df$diagnosis[match(rownames(AD_GSE313307_train_ds), AD_GSE313307_df$title)]
#saveRDS(AD_GSE313307_train_ds,"C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE313307_train_ds")
AD_GSE313307_train_ds <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE313307_train_ds")
cross.validation.SVM(AD_GSE313307_train_ds, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE313307")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE313307", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE313307", 10)

#-----------------------------------------------------------------------------------------------------------------#
#AD_GSE313307_df_female <- filter(AD_GSE313307_df, AD_GSE313307_df$sex == "F")
#index_female <- rownames(AD_GSE313307_train_ds) %in% AD_GSE313307_df_female$title
#AD_GSE313307_train_ds_female <- AD_GSE313307_train_ds[index_female,]
#saveRDS(AD_GSE313307_train_ds_female,"C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/AD_GSE313307_train_ds_female")
AD_GSE313307_train_ds_female <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/AD_GSE313307_train_ds_female")

cross.validation.SVM(AD_GSE313307_train_ds_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE313307")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE313307", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE313307", 10)

#-----------------------------------------------------------------------------------------------------------------#
#AD_GSE313307_df_male <- filter(AD_GSE313307_df, AD_GSE313307_df$sex == "M")
#index_male <- rownames(AD_GSE313307_train_ds) %in% AD_GSE313307_df_male$title
#AD_GSE313307_train_ds_male <- AD_GSE313307_train_ds[index_male,]
#saveRDS(AD_GSE313307_train_ds_male,"C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/AD_GSE313307_train_ds_male")
AD_GSE313307_train_ds_male <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/AD_GSE313307_train_ds_male")

cross.validation.SVM(AD_GSE313307_train_ds_male, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE313307")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE313307", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE313307", 10)

#-------------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(AD_GSE313307_train_ds_male,AD_GSE313307_train_ds_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE313307_M_train_F_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE313307_M_train_F_test", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE313307_M_train_F_test", 10)

#-------------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(AD_GSE313307_train_ds_female,AD_GSE313307_train_ds_male, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE313307_F_train_M_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE313307_F_train_M_test", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE313307_F_train_M_test", 10)

Latex_AD_GSE313307 <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE313307","All")
Latex_AD_GSE313307_female <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE313307","Female")
Latex_AD_GSE313307_male <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE313307","Male")
Latex_AD_GSE313307_M_train_F_test <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE313307_M_train_F_test","M_train_F_test")
Latex_AD_GSE313307_M_train_F_test <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE313307_F_train_M_test","F_train_M_test")

