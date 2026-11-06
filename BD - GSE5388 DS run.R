#BD_GSE5388<-getGEO("GSE5388", GSEMatrix = TRUE, getGPL = FBDE)
#BD_GSE5388<- BD_GSE5388[[1]]
#saveRDS(BD_GSE5388, "C:/Users/avive/Documents/GitHub/X_Factor/BD/BD_GSE5388")
BD_GSE5388<- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/BD/BD_GSE5388")

#BD_GSE5388_results <- get_GEO("C:/Users/avive/Documents/GitHub/X_Factor/BD/", "BD_GSE5388", sex = "Gender:ch1",
#                                 diagnosis = "Disease_status:ch1")

BD_GSE5388_counts <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/BD/caunts_BD_GSE5388")
BD_GSE5388_df <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/BD/df_BD_GSE5388")
BD_GSE5388_train_df <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/BD/train_df_BD_GSE5388")

cross.validation.SVM(BD_GSE5388_train_df, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/BD/", "BD_GSE5388")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/BD/", "BD_GSE5388", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/BD/", "BD_GSE5388", 10,"Bipolar disorder","Healthy control")

#----------------------------------------------------------------------------------------------------------------------#
#BD_GSE5388_df_female <- filter(BD_GSE5388_df, BD_GSE5388_df$sex == "Female")
#index_female <- rownames(BD_GSE5388_train_df) %in% BD_GSE5388_df_female$ID
#BD_GSE5388_train_df_female <- BD_GSE5388_train_df[index_female,]
#saveRDS(BD_GSE5388_train_df_female,"C:/Users/avive/Documents/GitHub/X_Factor/BD/Female/train_df_female")
BD_GSE5388_train_df_female <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/BD/Female/train_df_female")

cross.validation.SVM(BD_GSE5388_train_df_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/BD/Female/","BD_GSE5388")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/BD/Female/", "BD_GSE5388", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/BD/Female/", "BD_GSE5388", 10,"Bipolar disorder","Healthy control")

#-----------------------------------------------------------------------------------------------------------------#
#BD_GSE5388_df_male <- filter(BD_GSE5388_df, BD_GSE5388_df$sex == "Male")
#index_male <- rownames(BD_GSE5388_train_df) %in% BD_GSE5388_df_male$ID
#BD_GSE5388_train_df_male <- BD_GSE5388_train_df[index_male,]
#saveRDS(BD_GSE5388_train_df_male,"C:/Users/avive/Documents/GitHub/X_Factor/BD/Male/BD_GSE5388_train_df_male")
BD_GSE5388_train_df_male <- readRDS("C:/Users/avive/Documents/GitHub/X_Factor/BD/Male/BD_GSE5388_train_df_male")

cross.validation.SVM(BD_GSE5388_train_df_male, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/BD/Male/", "BD_GSE5388")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/BD/Male/", "BD_GSE5388", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/BD/Male/", "BD_GSE5388", 10,"Bipolar disorder","Healthy control")

#------------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(BD_GSE5388_train_df_male,BD_GSE5388_train_df_female, 10, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/BD/M_train_F_test/", "BD_GSE5388_M_train_F_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/BD/M_train_F_test/", "BD_GSE5388_M_train_F_test", 10)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/BD/M_train_F_test/", "BD_GSE5388_M_train_F_test", 10,"Bipolar disorder","Healthy control")

#------------------------------------------------------------------------------------------------------------------#
cross.validation.SVM_diff(BD_GSE5388_train_df_female,BD_GSE5388_train_df_male, 5, 1545, "C:/Users/avive/Documents/GitHub/X_Factor/BD/F_train_M_test/", "BD_GSE5388_F_train_M_test")
read.files("C:/Users/avive/Documents/GitHub/X_Factor/BD/F_train_M_test/", "BD_GSE5388_F_train_M_test", 5)
matrix_build("C:/Users/avive/Documents/GitHub/X_Factor/BD/F_train_M_test/", "BD_GSE5388_F_train_M_test", 5,"Bipolar disorder","Healthy control")

Latex_BD_GSE5388 <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/BD/", "BD_GSE5388", "All")
Latex_BD_GSE5388_female <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/BD/Female/", "BD_GSE5388", "Female")
Latex_BD_GSE5388_male <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/BD/Male/", "BD_GSE5388", "Male")
Latex_BD_GSE5388_M_train_F_test <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/BD/M_train_F_test/", "BD_GSE5388_M_train_F_test", "M_train_F_test")
Latex_BD_GSE5388_M_train_F_test <- export_results_latex("C:/Users/avive/Documents/GitHub/X_Factor/BD/F_train_M_test/", "BD_GSE5388_F_train_M_test", "F_train_M_test")

