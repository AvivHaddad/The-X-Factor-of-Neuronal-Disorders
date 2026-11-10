cross.validation.SVM <- function(
  data, x, seed, path, name, donor_id = NULL) {
  acc_list <- list()
  data$group <- as.factor(data$group)


  set.seed(seed = seed)

  if (is.null(donor_id)) {

    folds <- caret::createFolds(
      data$group,
      k = x,
      list = TRUE,
      returnTrain = TRUE
    )

  } else {

    folds <- make.donor.folds(
      labels = data$group,
      donor_id = donor_id,
      k = x
    )

  }

  for (i in 1:x){
    train_idx <- folds[[i]]
    test_idx <- setdiff(seq_len(nrow(data)),train_idx)
    training <- data[train_idx, , drop = F]
    test <- data[test_idx, , drop = F]
    if (!is.null(donor_id)) {
      stopifnot(
        length(intersect(
          donor_id[train_idx],
          donor_id[test_idx]
        )) == 0L
      )
    }
    # Splitting the genes and the groups made it possible for the function to run as it detailed to the SVM what it should predict.
    X_train <- training[
      , setdiff(names(training), "group"),
        drop = F
    ]
    y_train <- training$group

    X_test <- test[
      , setdiff(names(test), "group"),
        drop = F
    ]


    svm_pred <- e1071::svm(
      x = X_train,
      y = y_train,
      kernel = "linear",
      scale = TRUE #This means that the data is scaled by this function automatically
    )
    saveRDS(svm_pred,file = paste0(path,name,"_",i))

    predictions <- predict(svm_pred, newdata = X_test)
    pred_path <- paste0(path, "predictions_", i,"_",name)
    saveRDS(predictions, file = pred_path)

    compare <- data.frame(actual = test$group, predicted = predictions)
    comp_path <- paste0(path, "comparison_", i,"_",name)
    saveRDS(compare, file = comp_path)

    acc <- mean(compare$actual == compare$predicted)
    acc_list[[i]] <- acc
  }
  saveRDS(acc_list, paste0(path,"acc_list","_",name))

}

read.files <- function (path,name,x = 10){
  for (i in 1:x){
    comparison <- readRDS(paste0(path,"comparison_", i,"_",name) )
    comparison_table <- table(comparison)
    saveRDS(comparison_table,paste0(path,"table_",i,"_",name))
    write.csv2(comparison_table,paste0(path,"table_",i,name,"_",".csv"), row.names = T)
    #cat("\nComparison", i, ":\n")
    #print(comparison)

    cat("\nTable", i, ":\n")
    print(comparison_table)
  }

}

matrix_build <- function (path,name,x = 10,positive = "1", control = "0"){
  mat_svm <- lapply(1:x, function(i) {
    table_i <- readRDS(paste0(path,"table_",i,"_",name))
  })
  array_3d <- array(0, dim = c(nrow(mat_svm[[1]]), ncol(mat_svm[[1]]), length(mat_svm)))
  for (i in 1:length(mat_svm)) {
    array_3d[, ,i] <- mat_svm[[i]]
  }
  mean_matrix <- apply(array_3d, c(1, 2), mean)
  mean_matrix <- as.matrix(mean_matrix)
  rownames(mean_matrix) <- rownames(mat_svm[[1]])
  colnames(mean_matrix) <- colnames(mat_svm[[1]])
  print(mean_matrix)
  saveRDS(mean_matrix,paste0(path,"mean_mat_",name))
  write.csv2(mean_matrix,paste0(path,"mean_mat_",name,".csv"))
  
  sum_matrix <- apply(array_3d, c(1, 2), sum)
  sum_matrix <- as.matrix(sum_matrix)
  rownames(sum_matrix) <- rownames(mat_svm[[1]])
  colnames(sum_matrix) <- colnames(mat_svm[[1]])
  print(sum_matrix)
  saveRDS(sum_matrix,paste0(path,"sum_mat_",name))
  precentage_matrix <- prop.table(sum_matrix, margin = 1) * 100
  precentage_matrix <- round(precentage_matrix, 2)
  print(precentage_matrix)
  saveRDS(precentage_matrix,paste0(path,"precentage_mat_",name))
  write.csv2(precentage_matrix,paste0(path,"precentage_mat_",name,".csv"))

  sensitivity <- numeric(x)
  specificity <- numeric(x)
  for (i in seq_len(x)) {
    cm <- mat_svm[[i]]
    TP <- cm[positive,positive]
    FN <- cm[positive,control]
    TN <- cm[control,control]
    FP <- cm[control,positive]
    sensitivity[i] <- TP/(TP + FN)
    specificity[i] <- TN/(TN + FP)
  }
 metrics <- data.frame( Metric = c("Sensitivity", "Specificity"),
 Mean = c( mean(sensitivity, na.rm = TRUE), mean(specificity, na.rm = TRUE)),
 Standard_Deviation = c(sd(sensitivity, na.rm = TRUE),
 sd(specificity, na.rm = TRUE)))
 print(metrics)
 saveRDS(metrics, paste0(path, "metrics_", name))
 write.csv2( metrics, paste0(path, "metrics_", name, ".csv"), row.names = FALSE )
}

get_GEO <- function (path,name,phenoData = "phenoData",sex,diagnosis,assayData = "assayData",
                     exprs = "exprs",geo_accession = "geo_accession"){
  gse <-  readRDS(paste0(path,name))
  gse_counts <- gse@assayData
  gse_counts <- gse_counts$exprs
  saveRDS(gse_counts,paste0(path,"caunts_",name))
  pheno <- gse@phenoData@data
  gse_df <- data.frame(
    ID = pheno[[geo_accession]],
    sex = pheno[[sex]],
    diagnosis = pheno[[diagnosis]]
  )
  gse_df$group <- paste(gse_df$sex,gse_df$diagnosis)
  saveRDS(gse_df,file.path(path, paste0("df_", name)))
  gse_train_df <- as.data.frame(t(gse_counts))
  gse_train_df$group <- gse_df$diagnosis[match(rownames(gse_train_df),gse_df$ID)]
  saveRDS(gse_train_df,file.path(path, paste0("train_df_", name)))

  return(list(
    gse_counts = gse_counts,
    gse_df = gse_df,
    gse_train_df = gse_train_df
  ))
}

cross.validation.SVM_diff <- function (train,test,x,seed,path,name){
  acc_list <- list()
  train$group <- as.factor(train$group)
  test$group <- as.factor(test$group)

  set.seed(seed = seed)

    folds <- caret::createFolds(
      train$group,
      k = x,
      list = TRUE,
      returnTrain = TRUE
    )

  for (i in 1:x){
    train_idx <- folds[[i]]
    training <- train[train_idx,,drop = F]




    X_train <- training[
      , setdiff(names(training), "group"),
        drop = F
    ]
    y_train <- training$group

    X_test <- test[
      , setdiff(names(test), "group"),
        drop = F
    ]

    svm_pred <- e1071::svm(
      x = X_train,
      y = y_train,
      kernel = "linear",
      scale = TRUE
    )
    saveRDS(svm_pred,file = paste0(path,name,"_",i))

    predictions <- predict(svm_pred, newdata = X_test)
    pred_path <- paste0(path, "predictions_", i,"_",name)
    saveRDS(predictions, file = pred_path)

    compare <- data.frame(actual = test$group, predicted = predictions)
    comp_path <- paste0(path, "comparison_", i,"_",name)
    saveRDS(compare, file = comp_path)

    acc <- mean(compare$actual == compare$predicted)
    acc_list[[i]] <- acc
  }
  saveRDS(acc_list, paste0(path,"acc_list","_",name))

}

make.donor.folds <- function(labels, donor_id, k = 10) {

  donors <- unique(donor_id)

  donor_labels <- labels[match(donors, donor_id)]

  if (any(table(donor_labels) < k)) {
    stop("Not enough donors per diagnosis for the requested number of folds.")
  }

  donor_folds <- caret::createFolds(
    factor(donor_labels),
    k = k,
    list = TRUE,
    returnTrain = TRUE
  )

  lapply(donor_folds, function(i) {
    which(donor_id %in% donors[i])
  })
}

export_results_latex <- function(path, name, tag) {

  metrics <- readRDS(paste0(path, "metrics_", name))
  percentage_matrix <- readRDS(paste0(path, "precentage_mat_", name))

  name_clean <- gsub("_", " ", name, fixed = TRUE)

  metrics_latex <- metrics
  metrics_latex$Mean <- round(metrics_latex$Mean * 100, 2)
  metrics_latex$Standard_Deviation <- round(metrics_latex$Standard_Deviation * 100, 2)

  names(metrics_latex) <- c("Metric", "Mean (\\%)", "SD (percentage points)")


  metric_rows <- apply(metrics_latex, 1, function(row) {
    paste(row, collapse = " & ")
  })
  metric_rows <- paste(metric_rows, collapse = "\\\\\n")

  metrics_tex <- paste0(
    "\\begin{table}[H]\n\n",
    "\\caption{Sensitivity and specificity for experiment ", name_clean, "}\n",
    "\\label{tab:metrics_", name, "}\n",
    "\\centering\n",
    "\\vspace{0pt}\n",
    "\\resultlabel{metricblue}{PERFORMANCE METRICS - ", tag, "}\n",
    "\\vspace{-6pt}\n",
    "\\begin{tabular}[t]{lrr}\n",
    "\\toprule\n",
    "Metric & Mean (\\%) & SD (percentage points)\\\\\n",
    "\\midrule\n",
    metric_rows, "\\\\\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
  )

  writeLines(metrics_tex, paste0(path, "metrics_", name, ".tex"))

  col_headers <- paste("Predicted", colnames(percentage_matrix))
  header_row <- paste(c("Actual class", col_headers), collapse = " & ")


  cm_rows <- apply(percentage_matrix, 1, function(row) {
    paste(c(rownames(percentage_matrix)[which(rownames(percentage_matrix) == rownames(percentage_matrix)[1])],
            round(row, 2)), collapse = " & ")
  })


  cm_rows <- sapply(1:nrow(percentage_matrix), function(i) {
    paste(c(rownames(percentage_matrix)[i], round(percentage_matrix[i, ], 2)),
          collapse = " & ")
  })
  cm_rows <- paste(cm_rows, collapse = "\\\\\n")

  n_cols <- ncol(percentage_matrix) + 1
  col_spec <- paste0("l", paste(rep("r", n_cols - 1), collapse = ""))

  cm_tex <- paste0(
    "\\begin{table}[H]\n\n",
    "\\caption{Confusion matrix (\\%) for ", name_clean, "}\n",
    "\\label{tab:cm_", name, "}\n",
    "\\centering\n",
    "\\vspace{-0pt}\n",
    "\\resultlabel{matrixteal}{CONFUSION MATRIX - ", tag, "}\n",
    "\\vspace{-6pt}\n",
    "\\begin{tabular}[t]{", col_spec, "}\n",
    "\\toprule\n",
    header_row, "\\\\\n",
    "\\midrule\n",
    cm_rows, "\\\\\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "\\end{table}\n"
  )

  writeLines(cm_tex, paste0(path, "percentage_matrix_", name, ".tex"))

  invisible(TRUE)
}
