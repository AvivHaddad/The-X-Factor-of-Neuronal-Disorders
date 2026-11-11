AD_GSE131617<-getGEO("GSE131617", GSEMatrix = TRUE, getGPL = FALSE)
AD_GSE131617<- AD_GSE131617[[1]]
saveRDS(AD_GSE131617, "C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE131617")
AD_GSE131617<- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/AD_GSE131617")

AD_GSE131617_results <- get_GEO("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE131617", sex = "Sex:ch1",
                              diagnosis = "description")

AD_GSE131617_counts <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/caunts_AD_GSE131617")
AD_GSE131617_df <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/df_AD_GSE131617")
AD_GSE131617_train_df <- AD_GSE131617_results$gse_train_df
AD_GSE131617_train_df$group <- ifelse(grepl("_0_",AD_GSE131617_train_df$group), 0, 1)
saveRDS(AD_GSE131617_train_df ,"C:/Users/avive/Documents/GitHub/X_Factor/AD/train_df_AD_GSE131617")
AD_GSE131617_train_df <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/train_df_AD_GSE131617")

#Creating the donor id to use in the cross validation. This makes sure that each donor can be either in the training
#or testing data, not both.
descriptions <- trimws(
  as.character(AD_GSE131617_df$diagnosis)
)
AD_GSE131617_df$donor_id <- sub(
  "_(EC|FC|TC)$",
  "",
  descriptions
)
sample_match <- match(
  rownames(AD_GSE131617_train_df),
  AD_GSE131617_df$ID
)

stopifnot(
  !anyDuplicated(AD_GSE131617_df$ID),
  !anyNA(sample_match)
)

donor_ids <- AD_GSE131617_df$donor_id[sample_match]
cross.validation.SVM(AD_GSE131617_train_df, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE131617", donor_id = AD_GSE131617_df$donor_id)
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE131617", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE131617", 10)

#---------------------------------------------------------------------------------------------------------------------#
AD_GSE131617_df_female <- filter(AD_GSE131617_df, AD_GSE131617_df$sex == "F")
index_female <- rownames(AD_GSE131617_train_df) %in% AD_GSE131617_df_female$ID
AD_GSE131617_train_df_female <- AD_GSE131617_train_df[index_female,]
saveRDS(AD_GSE131617_train_df_female,"C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/AD_GSE131617_train_ds_female")
AD_GSE131617_train_df_female <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/AD_GSE131617_train_ds_female")

cross.validation.SVM(AD_GSE131617_train_df_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617",donor_id = AD_GSE131617_df$donor_id)
#Wasn't able as there are not enogh donors.
cross.validation.SVM(AD_GSE131617_train_df_female, 5, 12333, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_5_Folds")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_5_Folds", 5)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_5_Folds", 5)
cross.validation.SVM(AD_GSE131617_train_df_female, 3, 12333, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_3_Folds")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_3_Folds", 3)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_3_Folds", 3)

#-----------------------------------------------------------------------------------------------------------------#
AD_GSE131617_df_male <- filter(AD_GSE131617_df, AD_GSE131617_df$sex == "M")
index_male <- rownames(AD_GSE131617_train_df) %in% AD_GSE131617_df_male$ID
AD_GSE131617_train_df_male <- AD_GSE131617_train_df[index_male,]
saveRDS(AD_GSE131617_train_df_male,"C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/AD_GSE131617_train_df_male")
AD_GSE131617_train_df_male <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/AD_GSE131617_train_df_male")

cross.validation.SVM(AD_GSE131617_train_df_male, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617",donor_id = AD_GSE131617_df$donor_id)
#Wasn't able as there are not enogh donors.
cross.validation.SVM(AD_GSE131617_train_df_male, 5, 12333, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_5_Folds")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_5_Folds", 5)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_5_Folds", 5)
cross.validation.SVM(AD_GSE131617_train_df_male, 3, 12333, "C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_3_Folds")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_3_Folds", 3)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_3_Folds", 3)

#-----------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(AD_GSE131617_train_df_male,AD_GSE131617_train_df_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE131617_M_train_F_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE131617_M_train_F_test", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE131617_M_train_F_test", 10)

#-----------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(AD_GSE131617_train_df_female,AD_GSE131617_train_df_male, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE131617_F_train_M_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE131617_F_train_M_test", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE131617_F_train_M_test", 10)


Latex_AD_GSE131617 <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/", "AD_GSE131617", "ALL")
#Latex_AD_GSE131617_female <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617", "Female")
#Latex_AD_GSE131617_male <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617", "Male")
Latex_AD_GSE131617_M_train_F_test <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/M_train_F_test/", "AD_GSE131617_M_train_F_test", "M_train_F_test")
Latex_AD_GSE131617_M_train_F_test <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/F_train_M_test/", "AD_GSE131617_F_train_M_test", "F_train_M_test")
Latex_AD_GSE131617_female <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_5_Folds", "Female")
Latex_AD_GSE131617_male <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_5_Folds", "Male")
Latex_AD_GSE131617_female <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Female/", "AD_GSE131617_3_Folds", "Female")
Latex_AD_GSE131617_male <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/AD/Male/", "AD_GSE131617_3_Folds", "Male")
