#' Generate R code for the recommended method
#'
#' @param recommendation Output dari decide_method
#' @param data_info Output dari detect_data_structure
#' @param data_name Nama objek data (string)
#' @param var_dependent Nama variabel dependen (string)
#' @param var_independent Nama variabel independen (string, opsional)
#' @param paired Logical (untuk uji berpasangan)
#' @param ... Argumen tambahan
#' @return Character string berisi kode R
#' @export
generate_code <- function(recommendation, data_info, data_name = "data",
                          var_dependent = NULL, var_independent = NULL,
                          paired = NULL, ...) {
  
  method <- recommendation$method
  args <- list(...)
  
  # Tentukan nama variabel (jika tidak diberikan, coba dari data_info)
  if (is.null(var_dependent) && !is.null(data_info$dependent)) {
    var_dependent <- data_info$dependent$name
  }
  if (is.null(var_independent) && !is.null(data_info$independent)) {
    var_independent <- data_info$independent$name
  }
  
  # Fungsi helper untuk membentuk akses kolom
  col_ref <- function(var) {
    if (is.null(var)) return("")
    # Jika data_name adalah string dan var adalah string nama kolom
    paste0(data_name, "$", var)
  }
  
  code <- ""
  
  # T-test variants
  if (grepl("t-test", method, ignore.case = TRUE)) {
    if (grepl("One-Sample", method)) {
      code <- sprintf("t.test(%s, mu = 0)", col_ref(var_dependent))
    } else if (grepl("Paired", method)) {
      if (!is.null(var_independent)) {
        # Untuk paired, biasanya dua kolom dari data yang sama
        code <- sprintf("t.test(%s, %s, paired = TRUE)", col_ref(var_dependent), col_ref(var_independent))
      } else {
        code <- sprintf("t.test(%s ~ %s, data = %s, paired = TRUE)", var_dependent, var_independent, data_name)
      }
    } else if (grepl("Welch", method)) {
      code <- sprintf("t.test(%s ~ %s, data = %s, var.equal = FALSE)", var_dependent, var_independent, data_name)
    } else if (grepl("Independent", method) || grepl("equal variances", method)) {
      code <- sprintf("t.test(%s ~ %s, data = %s, var.equal = TRUE)", var_dependent, var_independent, data_name)
    } else {
      code <- sprintf("t.test(%s ~ %s, data = %s)", var_dependent, var_independent, data_name)
    }
  }
  
  # Wilcoxon / Mann-Whitney
  else if (grepl("Wilcoxon|Mann-Whitney", method, ignore.case = TRUE)) {
    if (grepl("One-Sample", method) || grepl("Signed-Rank", method)) {
      if (grepl("paired", method, ignore.case = TRUE)) {
        code <- sprintf("wilcox.test(%s, %s, paired = TRUE)", col_ref(var_dependent), col_ref(var_independent))
      } else {
        code <- sprintf("wilcox.test(%s, mu = 0)", col_ref(var_dependent))
      }
    } else {
      code <- sprintf("wilcox.test(%s ~ %s, data = %s)", var_dependent, var_independent, data_name)
    }
  }
  
  # ANOVA
  else if (grepl("ANOVA", method, ignore.case = TRUE)) {
    if (grepl("Welch", method)) {
      code <- sprintf("oneway.test(%s ~ %s, data = %s, var.equal = FALSE)", var_dependent, var_independent, data_name)
    } else {
      code <- sprintf("aov(%s ~ %s, data = %s)", var_dependent, var_independent, data_name)
    }
  }
  
  # Kruskal-Wallis
  else if (grepl("Kruskal-Wallis", method, ignore.case = TRUE)) {
    code <- sprintf("kruskal.test(%s ~ %s, data = %s)", var_dependent, var_independent, data_name)
  }
  
  # Correlation
  else if (grepl("Pearson|Spearman|Kendall", method, ignore.case = TRUE)) {
    meth <- switch(tolower(method),
      "pearson correlation" = "pearson",
      "spearman's rank correlation" = "spearman",
      "kendall's tau" = "kendall",
      "pearson"
    )
    code <- sprintf("cor.test(%s, %s, method = '%s')", col_ref(var_dependent), col_ref(var_independent), meth)
  }
  
  # Chi-square
  else if (grepl("Chi-square", method, ignore.case = TRUE)) {
    if (grepl("Goodness", method)) {
      code <- sprintf("chisq.test(table(%s))", col_ref(var_dependent))
    } else {
      code <- sprintf("chisq.test(table(%s, %s))", col_ref(var_dependent), col_ref(var_independent))
    }
  }
  
  # Binomial
  else if (grepl("Binomial", method, ignore.case = TRUE)) {
    code <- sprintf("binom.test(table(%s))", col_ref(var_dependent))
  }
  
  # Fisher
  else if (grepl("Fisher", method, ignore.case = TRUE)) {
    code <- sprintf("fisher.test(table(%s, %s))", col_ref(var_dependent), col_ref(var_independent))
  }
  
  # Fallback
  else {
    code <- "# Code generation not available for this method."
  }
  
  # Tambahkan komentar
  code <- paste0("# ", method, "\n", code)
  
  # Tambahkan argumen tambahan jika ada
  if (length(args) > 0) {
    extra <- paste(names(args), args, sep = " = ", collapse = ", ")
    code <- sub("\\)$", paste0(", ", extra, ")"), code)
  }
  
  return(code)
}
