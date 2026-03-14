#' Generate decision tree for a specific smartR recommendation
#'
#' @param object Hasil dari fungsi smart_rec()
#' @return Menampilkan pohon keputusan lengkap dengan nilai statistik
#' @export
#'
#' @examples
#' \dontrun{
#' result <- smart_rec(iris, sample1 = "Sepal.Length", sample2 = "Species", 
#'                     test_type = "comparison", paired = FALSE)
#' smart_dec(result)
#' }
smart_dec <- function(object) {
  if (!inherits(object, "smartR_recommendation")) {
    stop("Object harus dari kelas smartR_recommendation (hasil smart_rec).")
  }
  
  cat("\nsmartR DECISION TREE\n")
  cat("===================\n\n")
  
  # Data summary
  cat("Data Input:\n")
  if (!is.null(object$data_info$dependent)) {
    dep <- object$data_info$dependent
    cat("  • Dependent variable : ", dep$name, " (", dep$type_info$main_type, " - ", dep$type_info$sub_type, ")\n", sep = "")
    cat("      - Unique values : ", dep$n_unique, "\n")
    cat("      - Missing       : ", dep$n_missing, "\n")
  }
  if (!is.null(object$data_info$independent)) {
    ind <- object$data_info$independent
    cat("  • Independent variable : ", ind$name, " (", ind$type_info$main_type, " - ", ind$type_info$sub_type, ")\n", sep = "")
    if (ind$type_info$main_type == "CATEGORICAL") {
      cat("      - Number of groups : ", ind$n_unique, "\n")
      cat("      - Groups : ", paste(head(ind$levels, 5), collapse = ", "), 
          if (length(ind$levels) > 5) " ...", "\n", sep = "")
    } else {
      cat("      - Unique values : ", ind$n_unique, "\n")
    }
    cat("      - Missing        : ", ind$n_missing, "\n")
  }
  cat("  • Complete cases : ", object$data_info$overview$complete_cases, "\n")
  if (object$data_info$overview$missing_cases > 0) {
    cat("  • Missing cases  : ", object$data_info$overview$missing_cases, "\n")
  }
  cat("\n")
  
  # Decision path
  cat("Decision Path:\n")
  
  # Step 1: Tipe data dan tujuan
  cat("  Step 1: Identify variable types and analysis goal\n")
  if (is.null(object$data_info$independent)) {
    cat("    → One-sample analysis\n")
  } else {
    if (object$data_info$dependent$type_info$main_type == "NUMERIC" && 
        object$data_info$independent$type_info$main_type == "NUMERIC") {
      cat("    → Both variables NUMERIC → Association test\n")
    } else if (object$data_info$dependent$type_info$main_type == "NUMERIC" && 
               object$data_info$independent$type_info$main_type == "CATEGORICAL") {
      cat("    → Dependent NUMERIC, Independent CATEGORICAL → Comparison test\n")
    } else if (object$data_info$dependent$type_info$main_type == "CATEGORICAL" && 
               object$data_info$independent$type_info$main_type == "CATEGORICAL") {
      cat("    → Both variables CATEGORICAL → Association test (Chi-square/Fisher)\n")
    } else {
      cat("    → Mixed types → default to comparison\n")
    }
  }
  
  # Step 2: Jumlah kelompok (jika comparison)
  if (!is.null(object$data_info$independent) && 
      object$data_info$independent$type_info$main_type == "CATEGORICAL") {
    n_groups <- object$data_info$independent$n_unique
    cat("  Step 2: Number of groups =", n_groups, "\n")
    if (!is.null(object$paired)) {
      cat("    → Paired =", object$paired, "\n")
    }
  }
  
  # Step 3: Asumsi
  if (!is.null(object$assumptions$normality)) {
    cat("  Step 3: Check normality\n")
    if (!is.null(object$assumptions$normality$group_results)) {
      for (g in names(object$assumptions$normality$group_results)) {
        grp <- object$assumptions$normality$group_results[[g]]
        status <- if (isTRUE(grp$passed)) "PASSED" else if (isFALSE(grp$passed)) "FAILED" else "NA"
        pval <- if (!is.null(grp$p_value) && !is.na(grp$p_value)) sprintf("(p = %.4f)", grp$p_value) else ""
        cat("    • Group", g, ":", status, pval, "\n")
      }
    } else {
      status <- if (isTRUE(object$assumptions$normality$passed)) "PASSED" else if (isFALSE(object$assumptions$normality$passed)) "FAILED" else "NA"
      pval <- if (!is.null(object$assumptions$normality$p_value) && !is.na(object$assumptions$normality$p_value)) 
                sprintf("(p = %.4f)", object$assumptions$normality$p_value) else ""
      cat("    • Overall:", status, pval, "\n")
    }
  }
  
  if (!is.null(object$assumptions$homogeneity) && !is.na(object$assumptions$homogeneity$passed)) {
    cat("  Step 4: Check homogeneity of variance\n")
    status <- if (isTRUE(object$assumptions$homogeneity$passed)) "PASSED" else if (isFALSE(object$assumptions$homogeneity$passed)) "FAILED" else "NA"
    pval <- if (!is.null(object$assumptions$homogeneity$p_value) && !is.na(object$assumptions$homogeneity$p_value)) 
              sprintf("(p = %.4f)", object$assumptions$homogeneity$p_value) else ""
    cat("    •", status, pval, "\n")
  }
  
  # Step 5: Final decision
  cat("\n  Final Decision:\n")
  cat("    → Recommended method :", object$method, "\n")
  cat("    → Confidence level   :", object$confidence, "/5\n")
  
  cat("\n")
  if (length(object$alternatives) > 0) {
    cat("Alternative methods considered:\n")
    for (alt in names(object$alternatives)) {
      cat("  •", alt, "\n")
    }
    cat("\n")
  }
  
  invisible(object)
}
