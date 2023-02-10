mod_summary <- function(model) {
    mod_summary_sign <- summary(model)$coefficients[ , 4]  # Pull out p-values
          `mod summary stars` <- NA                             # Named vector with significance stars
          `mod summary stars`[mod_summary_sign < 0.1] <- ". (< 0.1)"
          `mod summary stars`[mod_summary_sign < 0.05] <- "* (< 0.05)"
          `mod summary stars`[mod_summary_sign < 0.01] <- "** (< 0.01)"
          `mod summary stars`[mod_summary_sign < 0.001] <- "*** (< 0.001)"
          `mod summary stars`[is.na(`mod summary stars`)] <- "n.s. (NA)"
          names(`mod summary stars`) <- names(mod_summary_sign)
          return(`mod summary stars`)
          }