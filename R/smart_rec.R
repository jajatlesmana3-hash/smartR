#' Main function: recommend statistical method based on data
#'
#' @param data Data input (data.frame, vector, dll). Bisa diisi NULL jika menggunakan vektor langsung.
#' @param ... Spesifikasi variabel. Pilih salah satu bentuk:
#'   * Untuk satu sampel: `sample = v` (v adalah vektor atau nama kolom)
#'   * Untuk dua sampel: `sample1 = a, sample2 = b, paired = TRUE/FALSE`
#'   * Untuk banyak sampel (>=2): `samples = list(grup1, grup2, grup3, ...)`
#' @param test_type "comparison" atau "association" atau "auto" (default)
#' @param output "compact" atau "full" (default "compact")
#'
#' @return Objek hasil rekomendasi (invisible), dicetak sesuai output
#' @export
#'
#' @examples
#' \dontrun{
#' # Satu sampel
#' smart_rec(iris, sample = iris$Sepal.Length)
#' 
#' # Dua sampel independen
#' smart_rec(iris, sample1 = "Sepal.Length", sample2 = "Species", 
#'           test_type = "comparison", paired = FALSE)
#' 
#' # Dua sampel berpasangan
#' data_pasangan <- data.frame(sebelum = rnorm(30), sesudah = rnorm(30))
#' smart_rec(data_pasangan, sample1 = "sebelum", sample2 = "sesudah", 
#'           test_type = "comparison", paired = TRUE)
#' 
#' # Banyak sampel (list)
#' smart_rec(NULL, samples = list(iris$Sepal.Length[1:50], 
#'                                 iris$Sepal.Length[51:100], 
#'                                 iris$Sepal.Length[101:150]), 
#'           test_type = "comparison")
#' }
smart_rec <- function(data, ..., test_type = c("auto", "comparison", "association"), 
                       output = c("compact", "full")) {
  
  test_type <- match.arg(test_type)
  output <- match.arg(output)
  
  args <- list(...)
  
  # ========== PARSING INPUT ==========
  
  if ("samples" %in% names(args)) {
    # ---------- Banyak sampel (list) ----------
    samples <- args$samples
    if (!is.list(samples)) stop("Argumen 'samples' harus berupa list.")
    if (length(samples) < 2) stop("Minimal dua sampel untuk 'samples'.")
    
    # Konversi setiap elemen ke vektor numerik (jika perlu)
    samples <- lapply(samples, function(x) {
      if (is.character(x)) x <- as.numeric(as.factor(x))
      as.numeric(x)
    })
    
    # Buat data frame dalam format long
    value_list <- list()
    group_list <- list()
    for (i in seq_along(samples)) {
      value_list[[i]] <- samples[[i]]
      group_list[[i]] <- rep(paste0("Group", i), length(samples[[i]]))
    }
    values <- unlist(value_list)
    groups <- unlist(group_list)
    data <- data.frame(value = values, group = factor(groups))
    var_dependent <- "value"
    var_independent <- "group"
    paired <- NULL  # tidak relevan untuk multi-sampel
    
  } else if ("sample" %in% names(args)) {
    # ---------- Satu sampel ----------
    samp <- extract_vector(data, args$sample)
    data <- data.frame(value = samp)
    var_dependent <- "value"
    var_independent <- NULL
    paired <- NULL
    
  } else if ("sample1" %in% names(args) && "sample2" %in% names(args)) {
    # ---------- Dua sampel ----------
    s1 <- extract_vector(data, args$sample1)
    s2 <- extract_vector(data, args$sample2)
    
    if (test_type == "comparison") {
      if (!"paired" %in% names(args)) {
        stop("Untuk test_type='comparison' dengan dua sampel, argumen 'paired' harus diisi (TRUE/FALSE).")
      }
      paired <- args$paired
      
      if (paired) {
        # Data berpasangan
        if (length(s1) != length(s2)) stop("Untuk data berpasangan, kedua sampel harus sama panjang.")
        id <- rep(1:length(s1), 2)
        val <- c(s1, s2)
        time <- rep(c("before", "after"), each = length(s1))
        data <- data.frame(id = id, value = val, time = factor(time))
        var_dependent <- "value"
        var_independent <- "time"
      } else {
        # Data independen
        data <- data.frame(
          value = c(s1, s2),
          group = factor(rep(c("Group1", "Group2"), c(length(s1), length(s2))))
        )
        var_dependent <- "value"
        var_independent <- "group"
      }
    } else { # test_type == "association" atau "auto"
      # Uji asosiasi (korelasi) antara dua vektor numerik
      if (length(s1) != length(s2)) stop("Untuk uji asosiasi, kedua sampel harus sama panjang.")
      data <- data.frame(x = s1, y = s2)
      var_dependent <- "x"
      var_independent <- "y"
      paired <- NULL
    }
    
  } else {
    stop("Argumen tidak dikenali. Gunakan:\n",
         "  • sample = ...          (satu sampel)\n",
         "  • sample1 = a, sample2 = b, paired = T/F  (dua sampel)\n",
         "  • samples = list(...)   (banyak sampel)")
  }
  
  # ========== DETEKSI DATA ==========
  data_info <- detect_data_structure(data, var_dependent, var_independent)
  
  # ========== CEK ASUMSI ==========
  assumptions <- check_assumptions(data, var_dependent, var_independent, data_info = data_info)
  
  # ========== TENTUKAN JENIS UJI (jika auto) ==========
  if (test_type == "auto") {
    test_type <- infer_test_type(data_info)
  }
  
  # ========== LOGIKA KEPUTUSAN ==========
  recommendation <- decide_method(
    data_info = data_info,
    assumptions = assumptions,
    test_type = test_type,
    paired = paired
  )
  
  # ========== GENERATE KODE ==========
  data_name <- deparse(substitute(data))
  # Hati-hati jika data_name menjadi "NULL" untuk kasus samples
  if (data_name == "NULL" && exists("samples")) data_name <- "data"
  
  code <- generate_code(
    recommendation = recommendation,
    data_info = data_info,
    data_name = data_name,
    var_dependent = var_dependent,
    var_independent = var_independent,
    paired = paired
  )
  recommendation$code <- code
  
  # ========== SIAPKAN OUTPUT ==========
  recommendation$data_info <- data_info
  recommendation$assumptions <- assumptions
  recommendation$output_type <- output
  class(recommendation) <- "smartR_recommendation"
  
  # Cetak sesuai output
  if (output == "compact") {
    print_compact(recommendation)
  } else {
    print_full(recommendation)
  }
  
  invisible(recommendation)
}

# Fungsi internal untuk mengekstrak vektor dari berbagai kemungkinan input
extract_vector <- function(data, x) {
  if (is.null(x)) stop("Nilai sampel tidak boleh NULL.")
  
  # Jika x adalah formula atau sesuatu yang aneh, hentikan
  if (inherits(x, "formula")) stop("Formula tidak didukung langsung. Gunakan vektor atau nama kolom.")
  
  # Jika x adalah vektor langsung
  if (is.vector(x) || is.factor(x)) {
    return(as.vector(x))
  }
  
  # Jika data adalah data.frame dan x adalah string (nama kolom)
  if (is.data.frame(data) && is.character(x) && length(x) == 1) {
    if (x %in% names(data)) {
      return(data[[x]])
    } else {
      stop("Kolom '", x, "' tidak ditemukan dalam data.")
    }
  }
  
  # Jika data adalah data.frame dan x adalah numerik (indeks kolom)
  if (is.data.frame(data) && is.numeric(x) && length(x) == 1) {
    if (x >= 1 && x <= ncol(data)) {
      return(data[[x]])
    } else {
      stop("Indeks kolom ", x, " di luar jangkauan.")
    }
  }
  
  stop("Tidak dapat mengekstrak vektor dari input yang diberikan.")
}

# Infer test type dari data_info
infer_test_type <- function(data_info) {
  dep <- data_info$dependent
  indep <- data_info$independent
  
  if (is.null(indep)) return("comparison")  # satu sampel
  
  if (dep$type_info$main_type == "NUMERIC" && indep$type_info$main_type == "NUMERIC") {
    return("association")
  } else if (dep$type_info$main_type == "NUMERIC" && indep$type_info$main_type == "CATEGORICAL") {
    return("comparison")
  } else if (dep$type_info$main_type == "CATEGORICAL" && indep$type_info$main_type == "CATEGORICAL") {
    return("association")
  } else {
    warning("Tipe data ambigu, menggunakan 'comparison' sebagai default.")
    return("comparison")
  }
}

# Cetak compact (tanpa bingkai)
print_compact <- function(x) {
  cat("\nsmartR RECOMMENDATION\n\n")
  cat("Method:", x$method, "\n")
  cat("Confidence:", x$confidence, "/5\n\n")
  cat("R code:\n")
  cat(x$code, "\n\n")
  if (length(x$notes) > 0) {
    cat("Notes:\n")
    for (n in x$notes) cat("  •", n, "\n")
    cat("\n")
  }
}

# Cetak full (tanpa bingkai)
print_full <- function(x) {
  cat("\nsmartR RECOMMENDATION\n\n")
  cat("Method:", x$method, "\n")
  if (!is.null(x$r_function)) cat("Function:", x$r_function, "\n")
  if (!is.null(x$category)) cat("Category:", x$category, "\n")
  cat("Confidence:", x$confidence, "/5\n\n")
  
  cat("Data Summary:\n")
  dep <- x$data_info$dependent
  cat("  Dependent:", dep$name, "-", dep$type_info$main_type, "(", dep$type_info$sub_type, ")\n")
  if (!is.null(x$data_info$independent)) {
    ind <- x$data_info$independent
    cat("  Independent:", ind$name, "-", ind$type_info$main_type, "(", ind$type_info$sub_type, ")\n")
    if (ind$type_info$main_type == "CATEGORICAL") {
      cat("  Number of groups:", ind$n_unique, "\n")
    }
  }
  cat("  Sample size (complete cases):", x$data_info$overview$complete_cases, "\n")
  if (x$data_info$overview$missing_cases > 0) {
    cat("  Missing cases:", x$data_info$overview$missing_cases, "\n")
  }
  cat("\n")
  
  cat("Assumption Checks:\n")
  if (!is.null(x$assumptions$normality)) {
    if (!is.null(x$assumptions$normality$group_results)) {
      cat("  Normality (per group):\n")
      for (g in names(x$assumptions$normality$group_results)) {
        grp <- x$assumptions$normality$group_results[[g]]
        status <- if (isTRUE(grp$passed)) "PASSED" else if (isFALSE(grp$passed)) "FAILED" else "NA"
        pval <- if (!is.null(grp$p_value) && !is.na(grp$p_value)) sprintf("(p = %.4f)", grp$p_value) else ""
        cat("    ", g, ": ", status, " ", pval, "\n", sep = "")
      }
    } else {
      status <- if (isTRUE(x$assumptions$normality$passed)) "PASSED" else if (isFALSE(x$assumptions$normality$passed)) "FAILED" else "NA"
      pval <- if (!is.null(x$assumptions$normality$p_value) && !is.na(x$assumptions$normality$p_value)) 
                sprintf("(p = %.4f)", x$assumptions$normality$p_value) else ""
      cat("  Normality: ", status, " ", pval, "\n", sep = "")
    }
  }
  if (!is.null(x$assumptions$homogeneity)) {
    status <- if (isTRUE(x$assumptions$homogeneity$passed)) "PASSED" else if (isFALSE(x$assumptions$homogeneity$passed)) "FAILED" else "NA"
    pval <- if (!is.null(x$assumptions$homogeneity$p_value) && !is.na(x$assumptions$homogeneity$p_value)) 
              sprintf("(p = %.4f)", x$assumptions$homogeneity$p_value) else ""
    cat("  Homogeneity of variance: ", status, " ", pval, "\n", sep = "")
  }
  cat("\n")
  
  if (length(x$alternatives) > 0) {
    cat("Alternatives:\n")
    for (alt in names(x$alternatives)) {
      cat("  •", alt, ":", x$alternatives[[alt]], "\n")
    }
    cat("\n")
  }
  
  if (length(x$notes) > 0) {
    cat("Notes:\n")
    for (n in x$notes) cat("  •", n, "\n")
    cat("\n")
  }
  
  cat("R code:\n")
  cat(x$code, "\n\n")
  
  if (length(x$references) > 0) {
    cat("References:\n")
    for (r in x$references) cat("  •", r, "\n")
    cat("\n")
  }
}
