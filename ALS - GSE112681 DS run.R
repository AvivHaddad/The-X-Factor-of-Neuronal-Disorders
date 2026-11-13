ALS_GSE112681 <-getGEO("GSE112681", GSEMatrix = TRUE, getGPL = FALSE)
ALS_GSE112681 <- ALS_GSE112681[[2]]
saveRDS(ALS_GSE112681, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/ALS_GSE112681")
ALS_GSE112681 <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/ALS/ALS_GSE112681")

ALS_GSE112681_results <- get_GEO("C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681", sex = "Sex:ch1",
                                 diagnosis = "diagnosis:ch1")

ALS_GSE112681_counts <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/ALS/caunts_ALS_GSE112681")
ALS_GSE112681_df <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/ALS/df_ALS_GSE112681")
ALS_GSE112681_train_df <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/ALS/train_df_ALS_GSE112681")

cross.validation.SVM(ALS_GSE112681_train_df, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681", 10,positive = "ALS",control = "CON")

#-----------------------------------------------------------------------------------------------------------------#
ALS_GSE112681_df_female <- filter(ALS_GSE112681_df, ALS_GSE112681_df$sex == "f")
index_female <- rownames(ALS_GSE112681_train_df) %in% ALS_GSE112681_df_female$ID
ALS_GSE112681_train_df_female <- ALS_GSE112681_train_df[index_female,]
saveRDS(ALS_GSE112681_train_df_female,"C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/train_df_female")
ALS_GSE112681_train_df_female <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/train_df_female")

cross.validation.SVM(ALS_GSE112681_train_df_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681", 10,positive = "ALS",control = "CON")

#-----------------------------------------------------------------------------------------------------------------#
ALS_GSE112681_df_male <- filter(ALS_GSE112681_df, ALS_GSE112681_df$sex == "m")
index_male <- rownames(ALS_GSE112681_train_df) %in% ALS_GSE112681_df_male$ID
ALS_GSE112681_train_df_male <- ALS_GSE112681_train_df[index_male,]
saveRDS(ALS_GSE112681_train_df_male,"C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/ALS_GSE112681_train_df_male")
ALS_GSE112681_train_df_male <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/ALS_GSE112681_train_df_male")

cross.validation.SVM(ALS_GSE112681_train_df_male, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681", 10,positive = "ALS","CON")

#------------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(ALS_GSE112681_train_df_male,ALS_GSE112681_train_df_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/M_train_F_test/", "ALS_GSE112681_M_train_F_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/M_train_F_test/", "ALS_GSE112681_M_train_F_test", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/M_train_F_test/", "ALS_GSE112681_M_train_F_test", 10,positive = "ALS","CON")

#--------------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(ALS_GSE112681_train_df_female,ALS_GSE112681_train_df_male, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/F_train_M_test/", "ALS_GSE112681_F_train_M_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/F_train_M_test/","ALS_GSE112681_F_train_M_test", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/F_train_M_test/", "ALS_GSE112681_F_train_M_test", 10,positive = "ALS","CON")

export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681", "All")
export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681", "Female")
export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681", "Male")
export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/M_train_F_test/", "ALS_GSE112681_M_train_F_test", "M_train_F_test")
export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/F_train_M_test/", "ALS_GSE112681_F_train_M_test", "F_train_M_test")

#--------------------------------------------------------------------------------------------------------------#
#To test the model on even grounds I will run the experiment on mix-sex, female and male data sets that are the same
#size and have the same amount of controls and ALS samples.

ALS_GSE112681_train_df_female_ALS <- ALS_GSE112681_train_df_female[ALS_GSE112681_df_female$diagnosis == "ALS",]
ALS_GSE112681_train_df_female_control <- ALS_GSE112681_train_df_female[ALS_GSE112681_df_female$diagnosis == "CON",]
ALS_GSE112681_train_df_male_ALS <- ALS_GSE112681_train_df_male[ALS_GSE112681_df_male$diagnosis == "ALS",]
ALS_GSE112681_train_df_male_control <- ALS_GSE112681_train_df_male[ALS_GSE112681_df_male$diagnosis == "CON",]
mixed_ALS <- rbind(
  ALS_GSE112681_train_df_female_ALS[sample(nrow(ALS_GSE112681_train_df_female_ALS), 45), ],
  ALS_GSE112681_train_df_male_ALS[sample(nrow(ALS_GSE112681_train_df_male_ALS), 45), ]
)

mixed_control <- rbind(
  ALS_GSE112681_train_df_female_control[sample(nrow(ALS_GSE112681_train_df_female_control), 45), ],
  ALS_GSE112681_train_df_male_control[sample(nrow(ALS_GSE112681_train_df_male_control), 45), ]
)

ALS_GSE112681_train_df_mixed <- rbind(mixed_ALS, mixed_control)

ALS_GSE112681_train_df_Full_female <- rbind(
  ALS_GSE112681_train_df_female_ALS[sample(nrow(ALS_GSE112681_train_df_female_ALS), 90), ],
  ALS_GSE112681_train_df_female_control[sample(nrow(ALS_GSE112681_train_df_female_control), 90), ]
)

ALS_GSE112681_train_df_Full_male <- rbind(
  ALS_GSE112681_train_df_male_ALS[sample(nrow(ALS_GSE112681_train_df_male_ALS), 90), ],
  ALS_GSE112681_train_df_male_control[sample(nrow(ALS_GSE112681_train_df_male_control), 90), ]
)

cross.validation.SVM(ALS_GSE112681_train_df_mixed , 5, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681_180")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681_180", 5)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681_180", 5,positive = "ALS",control = "CON")

#female
cross.validation.SVM(ALS_GSE112681_train_df_Full_female, 5, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681_180")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681_180", 5)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681_180", 5,positive = "ALS",control = "CON")

#Male
cross.validation.SVM(ALS_GSE112681_train_df_Full_male, 5, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681_180")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681_180", 5)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681_180", 5,positive = "ALS",control = "CON")

export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/", "ALS_GSE112681_90", "All-90")
export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Female/", "ALS_GSE112681_90", "Female-90")
export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/ALS/Male/", "ALS_GSE112681_90", "Male-90")
