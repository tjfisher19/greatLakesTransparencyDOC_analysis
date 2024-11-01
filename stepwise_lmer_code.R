##############################################
## Two Functions are included here
##
##  lmerStepBackward()
##  lmerStepForward()
##
##  They both perform stepwise regression on the fixed
##    effects of a linear mixed effects model
##
##  Neither is coded for efficiency. So it can take some time
##    to run if you include many potential terms.
##
##  Each function expects 4 parameters
##    - data -- the data.frame with the data
##    - fixed.formula - a character string that can be interpreted as the 
##                      formula for the fixed effect, it should include the
##                      response variable and the ~
##    - random.formula - a character string with the random effect, likely
##                       of the form (1|Site) or (Time|Site).
##    - criteria - a character that specifies if BIC or MDL should be used,
##                 if anything else is provided, AIC is used.
##
##  Each of the function returns a list with four elements
##     - a list of all the AIC/BIC/MDL values, called ALL_AIC
##     - a list of corresponding lmer model fits, called ALL_MODELS
##     - the "best" AIC/BIC/MDL value found, called BEST_AIC
##     - the "best" fixed effect terms to include, based on AIC/BIC/MDL,
##       a vector of characters that can be used, called BEST_FIXED_TERMS
##



##########################
##
## Backward Step-Wise Regression
##
################

lmerStepBackward <- function(data, fixed.formula, random.formula, criteria="BIC") {
  
  if(criteria=="BIC") {
    penalty <- log(nrow(data)) 
  } else if(criteria=="MDL") {
    penalty <- log(nrow(data))/2
  } else {  ## Use AIC
    penalty <- 2
  }
  
  response <- str_trim(str_split(fixed.formula, "~", simplify=TRUE)[1])
  lmer_formula <- formula(paste0(fixed.formula, " + ", random.formula))
  
  full_mod <- lmer(lmer_formula, data=data, 
                   control=lmerControl(check.rankX = "silent.drop.cols"))  
  cur_min_aic <- AIC(full_mod, k=penalty)
  
  mod_terms <- rownames(anova(lm(fixed.formula, data=data) ) )
  mod_terms <- mod_terms[-length(mod_terms)]
  
  cur_index <- 1:length(mod_terms)
  cur_int_terms <- max(str_count(mod_terms, ":"))
  
  all_aic_vals <- list(cur_min_aic)
  all_mod_fits <- list(full_mod)
  
  check_again <- TRUE
  while(check_again & cur_int_terms >= 0) {
    check_again <- FALSE
    aic_vals <- NULL
    loop_index <- cur_index[str_count(mod_terms[cur_index], ":")==cur_int_terms]
    for(i in  loop_index) {
      cur_formula <- formula(paste0(response, " ~ ", 
                                    str_flatten(str_replace(mod_terms[cur_index[cur_index != i]], ":", "*"), "+"), 
                                    " + ", random.formula))
      cur_model <- lmer(cur_formula, data=data,
                        control=lmerControl(check.rankX = "silent.drop.cols"))
      aic_vals <- c(aic_vals, AIC(cur_model, k=penalty ) )
      all_aic_vals <- c(all_aic_vals, AIC(cur_model, k=penalty ))
      all_mod_fits <- c(all_mod_fits, cur_model)
    }
    if(min(aic_vals) < cur_min_aic ) {
      cur_min_aic <- min(aic_vals)
      cur_index <- cur_index[-(which(str_count(mod_terms, ":")==cur_int_terms)[which.min(aic_vals)] ) ]
      check_again <- TRUE
    }
    if(length(loop_index)==1 ) {
      cur_int_terms <- cur_int_terms - 1
      check_again <- TRUE
    } else if(check_again==FALSE) {
      cur_int_terms <- cur_int_terms - 1
      check_again <- TRUE
    }
  }
  
  list(ALL_AIC = all_aic_vals,
       ALL_MODELS = all_mod_fits,
       BEST_AIC = cur_min_aic,
       BEST_FIXED_TERMS = mod_terms[cur_index])
}

##########################
##
## Forward Step-Wise Regression
##
################

lmerStepForward <- function(data, fixed.formula, random.formula, criteria="BIC") {

  if(criteria=="BIC") {
    penalty <- log(nrow(data)) 
  } else if(criteria=="MDL") {
    penalty <- log(nrow(data))/2
  } else {  ## Use AIC
    penalty <- 2
  }
  
  response <- str_trim(str_split(fixed.formula, "~", simplify=TRUE)[1])
  lmer_formula <- formula(paste0(response, " ~ ", random.formula))
  
  null_mod <- lmer(lmer_formula, data=data,
                   control=lmerControl(check.rankX = "silent.drop.cols") )

  cur_min_aic <- AIC(null_mod, k=penalty)

  mod_terms <- rownames(anova(lm(fixed.formula, data=data) ) )
  mod_terms <- mod_terms[-length(mod_terms)]

  cur_index <- NULL # Dummy value to start
  max_index <- 1:length(mod_terms)
  max_int_terms <- max(str_count(mod_terms, ":"))
  cur_int_terms <- 0
  
  all_aic_vals <- list(cur_min_aic)
  all_mod_fits <- list(null_mod)

  check_again <- TRUE
  while(check_again  & cur_int_terms <= max_int_terms) {
    check_again <- FALSE
    aic_vals <- NULL
    if(is.null(cur_index) ) {
      loop_index <- max_index[str_count(mod_terms, ":")==cur_int_terms]
    } else {
      loop_index <- max_index[-cur_index][str_count(mod_terms[max_index[-cur_index]], ":")==cur_int_terms]
    }
    for(i in loop_index ) {
      cur_formula <- formula(paste0(response, " ~ ", 
                                    str_flatten(str_replace(mod_terms[c(cur_index,i)], ":", "*"), "+"), 
                                    " + ", random.formula))
      cur_model <- lmer(cur_formula, data=data,
                        control=lmerControl(check.rankX = "silent.drop.cols"))
      aic_vals <- c(aic_vals, AIC(cur_model, k=penalty ) )
      all_aic_vals <- c(all_aic_vals, AIC(cur_model, k=penalty ) )
      all_mod_fits <- c(all_mod_fits, cur_model)
    }
    if(min(aic_vals) < cur_min_aic ) {
      cur_min_aic <- min(aic_vals)
      cur_index <- c(cur_index, loop_index[which.min(aic_vals)])
      check_again <- TRUE
    }
    if(length(loop_index)==1 ) {
      cur_int_terms <- cur_int_terms + 1
      check_again <- TRUE
    } else if(check_again==FALSE) {
      cur_int_terms <- cur_int_terms + 1
      check_again <- TRUE
    }
  }
  
  list(ALL_AIC = all_aic_vals,
       ALL_MODELS = all_mod_fits,
       BEST_AIC = cur_min_aic,
       BEST_FIXED_TERMS = mod_terms[cur_index])
  
}
