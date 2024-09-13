# Univariate methods for comparing group means
# between 2 groups (e.g., case-control)

# Wrapper around t.test() function
#
# Parameters:
#   x           -- numeric vector of data values OR a formula: numeric ~ 2-level-factor
#   y           -- optional numeric vector (same length as x); default = NULL
#   alternative -- character describing the alternative hypothesis
calc_t <- function(x = NULL, y = NULL,
                   alternative = c("two.sided", "less", "greater"), ...) {
  
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  
  dots <- list(...)
  
  if( ! is.null(x) ) {
    dots$x <- x
    dots$y <- y
  } else if ( "formula" %in% names(dots) ) {
    if( "data" %in% names(dots) ) {
      dots$x <- x
      dots$y <- y
    } else {
      stop("If using a formula object, must provide `data` argument.")
    }
  } else {
    stop("Must provide numeric vector for `x` or formula object for `formula`.")
  }
  
  dots$alternative <- alternative
  
  t.res <- do.call(t.test, dots)
  
  t.res
}

# Wrapper around ks.test() function
#
# Parameters:
#   x           -- numeric vector of data values OR a formula: numeric ~ 2-level-factor
#   y           -- optional numeric vector (same length as x); default = NULL
#   alternative -- character describing the alternative hypothesis
calc_ks <- function(x = NULL, y = NULL,
                    alternative = c("two.sided", "less", "greater"), ...) {
  
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  
  dots <- list(...)
  
  if( ! is.null(x) ) {
    dots$x <- x
    dots$y <- y
  } else if ( "formula" %in% names(dots) ) {
    if( "data" %in% names(dots) ) {
      dots$x <- x
      dots$y <- y
    } else {
      stop("If using a formula object, must provide `data` argument.")
    }
  } else {
    stop("Must provide numeric vector for `x` or formula object for `formula`.")
  }
  
  dots$alternative <- alternative
  
  ks.res <- do.call(ks.test, dots)
  
  ks.res
}


# Wrapper around wilcox.test() function
#
# Parameters:
#   x           -- numeric vector of data values OR a formula: numeric ~ 2-level-factor
#   y           -- optional numeric vector (same length as x); default = NULL
#   alternative -- character describing the alternative hypothesis
calc_wilcox <- function(x = NULL, y = NULL,
                        alternative = c("two.sided", "less", "greater"), ...) {
  
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  
  dots <- list(...)
  
  if( ! is.null(x) ) {
    dots$x <- x
    dots$y <- y
  } else if ( "formula" %in% names(dots) ) {
    if( "data" %in% names(dots) ) {
      dots$x <- x
      dots$y <- y
    } else {
      stop("If using a formula object, must provide `data` argument.")
    }
  } else {
    stop("Must provide numeric vector for `x` or formula object for `formula`.")
  }
  
  dots$alternative <- alternative
  
  wilcox.res <- do.call(wilcox.test, dots)
  
  wilcox.res
}

# Wrapper around kruskal.test() function
#
# Parameters:
#   x           -- numeric vector of data values OR a list of numeric vectors
#   g           -- vector or factor (same length as x)
calc_kw <- function(x = NULL, g = NULL, ...) {
  
  dots <- list(...)
  
  if( ! is.null(x) ) {
    dots$x <- x
    dots$g <- g
  } else if ( "formula" %in% names(dots) ) {
    if( "data" %in% names(dots) ) {
      dots$x <- x
      dots$g <- g
    } else {
      stop("If using a formula object, must provide `data` argument.")
    }
  } else {
    stop("Must provide numeric vector for `x` or formula object for `formula`.")
  }
  
  kw.res <- do.call(kruskal.test, dots)
  
  kw.res
}


# Wrapper around lm() function
#
# Parameters:
#   formula -- object of class "formula" (or can be coerced to that class)
#   data    -- optional data frame, list or environment (or coercible by as.data.frame to data frame) containing variables in model
calc_lm <- function(formula = NULL, data = NULL, ...) {
  
  dots <- list(...)
  
  if( ! is.null(formula) ) {
    dots$formula <- formula
    dots$data <- data
  } else {
    stop("Must provide numeric vector for `x` or formula object for `formula`.")
  }
  
  lm.res <- do.call(lm, dots)
  
  summary(lm.res)
}

# Function for grabbing output from each univariate function
#
#
pretty_uni_res <- function(object, 
                           test.name = c("t-test", 
                                         "ks-test",
                                         "wilcox-test",
                                         "kw-test",
                                         "lm-test")) {
  
  tst.nm <- match.arg(test.name, c("t-test", 
                                   "ks-test",
                                   "wilcox-test",
                                   "kw-test",
                                   "lm-test"))
  
  if ( tst.nm == "t-test" ) {
    out_stuff <- data.frame(estimate    = as.numeric(object$estimate[1] - object$estimate[2]),
                            estimate1   = as.numeric(object$estimate[1]), 
                            estimate2   = as.numeric(object$estimate[2]),
                            statistic   = as.numeric(object$statistic),
                            p.value     = as.numeric(object$p.value),
                            parameter   = as.numeric(object$parameter),
                            conf.low    = as.numeric(object$conf.int[1]),
                            conf.high   = as.numeric(object$conf.int[2]),
                            method      = as.character(object$method),
                            alternative = as.character(object$alternative),
                            formula     = as.character(object$data.name))
  } else if ( tst.nm == "ks-test" ) {
    out_stuff <- data.frame(statistic   = as.numeric(object$statistic),
                            p.value     = as.numeric(object$p.value),
                            method      = as.character(object$method),
                            alternative = as.character(object$alternative),
                            formula     = as.character(object$data.name),
                            exact       = as.logical(object$exact))
  } else if ( tst.nm == "wilcox-test" ) {
    out_stuff <- data.frame(statistic   = as.numeric(object$statistic), 
                            p.value     = as.numeric(object$p.value),
                            method      = as.character(object$method), 
                            alternative = as.character(object$alternative), 
                            formula     = as.character(object$data.name))
  } else if ( tst.nm == "kw-test" ) {
    out_stuff <- data.frame(statistic   = as.numeric(object$statistic), 
                            p.value     = as.numeric(object$p.value), 
                            parameter   = as.numeric(object$parameter), 
                            method      = as.character(object$method), 
                            formula     = as.character(object$data.name))
  } else {
    coef_tab <- object$coefficients[2, ]
    out_stuff <- data.frame(estimate    = as.numeric(coef_tab["Estimate"]), 
                            std.error   = as.numeric(coef_tab["Std. Error"]), 
                            statistic   = as.numeric(coef_tab["t value"]), 
                            p.value     = as.numeric(coef_tab["Pr(>|t|)"]))
  }
  
  out_stuff
}

# Function for making a column from row names
rn2col <- function(object, name = "name") {
  new.name  <- "newcol"
  names(new.name) <- name
  row.nms <- row.names(object)
  object |> 
    mutate(newcol = row.nms, .before = 1) |> 
    rename(new.name)
}

# Function for creating a nice table of univariate results,
# which is ranked in inreasing order of p-value
create_stat_tab <- function(.data, 
                            test.name = c("t-test", "ks-test", "wilcox-test", "kw-test", "lm-test"),
                            response.name = "class"){
  
  
  tst.name <- match.arg(test.name)
  
  uni_fn <- switch(tst.name,
                   "t-test"      = calc_t,
                   "ks-test"     = calc_ks,
                   "wilcox-test" = calc_wilcox,
                   "kw-test"     = calc_kw,
                   "lm-test"     = calc_lm)

  out_res <- lapply(.data |> select(-all_of(c(response.name))) |> colnames(), 
                    function(.v) {
                      my_form <- as.formula(paste0(.v, " ~ ", response.name))
                      uni_res <- uni_fn(formula = my_form, data = .data)
                      pretty_out <- pretty_uni_res(uni_res, test.name = tst.name)
                      data.frame(pretty_out, row.names = .v)
                    }) |> 
    do.call(what = rbind) |> 
    rn2col(name = "Feature") |> 
    arrange(p.value)
  
  out_res <- out_res |>
    mutate(fdr = p.adjust(out_res$p.value, 
                          method = "fdr"),
           p.bonferroni = p.adjust(out_res$p.value, 
                                   method = "bonferroni")) |> 
    mutate(rank = 1:nrow(out_res))
  
  out_res
}

format_stat_tab <- function(object, n.digits = 3){
  pretty_tab <- object |> 
    mutate(across(c("p.value", "fdr", "p.bonferroni"), 
                  ~format(.x, 
                          digits = 3, 
                          nsmall = 3, 
                          scientific = TRUE)
                  )
           ) |> 
    mutate(across(where(is.numeric), 
                  ~format(round(.x, digits = 3), nsmall = 3)
                  )
           )
  pretty_tab
}
