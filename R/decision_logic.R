#' Core decision logic for method selection
#'
#' @param data_info Output from detect_data_structure
#' @param assumptions Output from check_assumptions
#' @param test_type "comparison" or "association"
#' @param paired Logical (for two-sample comparison)
#' @return List rekomendasi metode
#' @export
decide_method <- function(data_info, assumptions, test_type, paired = NULL) {
  
  dep <- data_info$dependent
  indep <- data_info$independent
  
  # Inisialisasi
  rec <- list(
    method = NULL,
    r_function = NULL,
    category = NULL,
    confidence = 3,
    alternatives = list(),
    notes = character(0)
  )
  
  # ========== ONE SAMPLE ==========
  if (is.null(indep)) {
    if (dep$type_info$main_type == "NUMERIC") {
      norm_pass <- isTRUE(assumptions$normality$passed)
      if (norm_pass) {
        rec$method <- "One-Sample t-test"
        rec$r_function <- "t.test"
        rec$category <- "Parametric"
        rec$alternatives <- list("Wilcoxon signed-rank" = "wilcox.test")
        rec$notes <- "Compares mean to a hypothesized value (mu)."
      } else {
        rec$method <- "Wilcoxon Signed-Rank Test"
        rec$r_function <- "wilcox.test"
        rec$category <- "Non-parametric"
        rec$alternatives <- list("One-Sample t-test" = "t.test")
        rec$notes <- "Non-parametric alternative; tests median."
      }
    } else if (dep$type_info$main_type == "CATEGORICAL") {
      if (dep$n_unique == 2) {
        rec$method <- "Binomial Test"
        rec$r_function <- "binom.test"
        rec$category <- "Non-parametric"
        rec$notes <- "Tests proportion in a binary category."
      } else {
        rec$method <- "Chi-square Goodness-of-Fit"
        rec$r_function <- "chisq.test"
        rec$category <- "Non-parametric"
        rec$notes <- "Compares observed frequencies to expected."
      }
    }
  } else {
    # ========== TWO OR MORE SAMPLES ==========
    if (test_type == "comparison") {
      if (dep$type_info$main_type == "NUMERIC") {
        if (indep$type_info$main_type == "CATEGORICAL") {
          n_groups <- indep$n_unique
          
          # Determine normality status
          norm_pass <- if (!is.null(assumptions$normality)) {
            if (!is.null(assumptions$normality$group_results)) {
              all(sapply(assumptions$normality$group_results, function(g) isTRUE(g$passed)))
            } else isTRUE(assumptions$normality$passed)
          } else FALSE
          
          homo_pass <- isTRUE(assumptions$homogeneity$passed)
          
          if (n_groups == 2) {
            if (!is.null(paired) && paired) {
              # Paired two-sample
              if (norm_pass) {
                rec$method <- "Paired t-test"
                rec$r_function <- "t.test"
                rec$category <- "Parametric"
                rec$alternatives <- list("Wilcoxon signed-rank (paired)" = "wilcox.test")
                rec$notes <- "Paired t-test for dependent samples."
              } else {
                rec$method <- "Wilcoxon Signed-Rank Test (paired)"
                rec$r_function <- "wilcox.test"
                rec$category <- "Non-parametric"
                rec$alternatives <- list("Paired t-test" = "t.test")
                rec$notes <- "Non-parametric alternative for paired samples."
              }
            } else {
              # Independent two-sample
              if (norm_pass && homo_pass) {
                rec$method <- "Independent t-test (equal variances)"
                rec$r_function <- "t.test"
                rec$category <- "Parametric"
                rec$alternatives <- list("Welch's t-test" = "t.test(var.equal=FALSE)", "Mann-Whitney U" = "wilcox.test")
                rec$notes <- "Classic t-test assuming equal variances."
              } else if (norm_pass && !homo_pass) {
                rec$method <- "Welch's t-test (unequal variances)"
                rec$r_function <- "t.test"
                rec$category <- "Parametric"
                rec$alternatives <- list("Independent t-test" = "t.test(var.equal=TRUE)", "Mann-Whitney U" = "wilcox.test")
                rec$notes <- "t-test without equal variance assumption."
              } else {
                rec$method <- "Mann-Whitney U Test"
                rec$r_function <- "wilcox.test"
                rec$category <- "Non-parametric"
                rec$alternatives <- list("t-test" = "t.test")
                rec$notes <- "Non-parametric alternative for two independent samples."
              }
            }
          } else {
            # More than two groups
            if (norm_pass && homo_pass) {
              rec$method <- "One-Way ANOVA"
              rec$r_function <- "aov"
              rec$category <- "Parametric"
              rec$alternatives <- list("Welch's ANOVA" = "oneway.test", "Kruskal-Wallis" = "kruskal.test")
              rec$notes <- "Use TukeyHSD for post-hoc comparisons."
            } else if (norm_pass && !homo_pass) {
              rec$method <- "Welch's ANOVA"
              rec$r_function <- "oneway.test"
              rec$category <- "Parametric (unequal variances)"
              rec$alternatives <- list("One-Way ANOVA" = "aov", "Kruskal-Wallis" = "kruskal.test")
              rec$notes <- "ANOVA without assuming equal variances."
            } else {
              rec$method <- "Kruskal-Wallis Test"
              rec$r_function <- "kruskal.test"
              rec$category <- "Non-parametric"
              rec$alternatives <- list("One-Way ANOVA" = "aov")
              rec$notes <- "Non-parametric alternative for multiple groups."
            }
          }
        } else {
          # Numeric vs Numeric? seharusnya association, tapi jika test_type=comparison dipaksa, fallback
          rec$method <- "Unknown (check variable types)"
          rec$notes <- "Test type 'comparison' with two numeric variables? Use association."
        }
      } else if (dep$type_info$main_type == "CATEGORICAL" && indep$type_info$main_type == "CATEGORICAL") {
        rec$method <- "Chi-square Test of Independence"
        rec$r_function <- "chisq.test"
        rec$category <- "Non-parametric"
        rec$alternatives <- list("Fisher's Exact Test" = "fisher.test")
        rec$notes <- "If expected counts <5, consider Fisher's exact test."
      } else {
        rec$method <- "Unknown combination"
      }
    } else { # association
      if (dep$type_info$main_type == "NUMERIC" && indep$type_info$main_type == "NUMERIC") {
        norm_pass <- isTRUE(assumptions$normality$passed)
        if (norm_pass) {
          rec$method <- "Pearson Correlation"
          rec$r_function <- "cor.test"
          rec$category <- "Parametric"
          rec$alternatives <- list("Spearman" = "cor.test(method='spearman')", "Kendall" = "cor.test(method='kendall')")
          rec$notes <- "Measures linear correlation; requires bivariate normality."
        } else {
          rec$method <- "Spearman's Rank Correlation"
          rec$r_function <- "cor.test"
          rec$category <- "Non-parametric"
          rec$alternatives <- list("Pearson" = "cor.test(method='pearson')", "Kendall" = "cor.test(method='kendall')")
          rec$notes <- "Monotonic correlation based on ranks."
        }
      } else if (dep$type_info$main_type == "CATEGORICAL" && indep$type_info$main_type == "CATEGORICAL") {
        rec$method <- "Chi-square Test of Independence"
        rec$r_function <- "chisq.test"
        rec$category <- "Non-parametric"
        rec$alternatives <- list("Fisher's Exact" = "fisher.test")
        rec$notes <- "Use Cramer's V for effect size."
      } else if (dep$type_info$main_type == "NUMERIC" && indep$type_info$main_type == "CATEGORICAL" && indep$n_unique == 2) {
        rec$method <- "Point-Biserial Correlation"
        rec$r_function <- "cor.test"
        rec$category <- "Parametric"
        rec$notes <- "Correlation between continuous and binary variable."
      } else {
        rec$method <- "Unknown association test"
      }
    }
  }
  
  # Hitung confidence
  rec$confidence <- calculate_confidence(rec$method, assumptions, data_info)
  
  # Tambahkan referensi (placeholder)
  rec$references <- get_method_references(rec$method)
  
  class(rec) <- "smartR_recommendation"
  return(rec)
}

calculate_confidence <- function(method, assumptions, data_info) {
  score <- 5
  # Jika asumsi tidak terpenuhi untuk metode parametrik, kurangi
  if (grepl("t-test|ANOVA|Pearson", method, ignore.case = TRUE)) {
    if (!is.null(assumptions$normality) && isFALSE(assumptions$normality$passed)) score <- score - 2
    if (!is.null(assumptions$homogeneity) && isFALSE(assumptions$homogeneity$passed) && !grepl("Welch", method)) score <- score - 1
  }
  # Sample size
  n <- data_info$overview$complete_cases
  if (n < 30) score <- score - 1
  if (n < 10) score <- score - 1
  max(1, min(5, score))
}

get_method_references <- function(method) {
  # Placeholder – nanti bisa diperbarui dengan referensi nyata
  c("Reference: See standard statistical textbooks.")
}
