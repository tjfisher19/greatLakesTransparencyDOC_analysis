#############################################
### The primary statistical modeling
###
### Fit optimal mixed effect models
###    and plots the predicted trends (in time)

library(tidyverse)
library(lme4)
library(lmerTest)
library(merTools)

load("fully_processed_data.RData")

##################################################################
##################################################################
##################################################################
##
##  Now, some statistical modeling
##
##################################################################


######
## Some Model Selection
##
## Tom Fisher code that does step-wise lmer fitting
source("stepwise_lmer_code.R")

### Backward selection
# uvm_back_aic <- lmerStepBackward(data=UVdata,
#                                  fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
#                                  random.formula = "(1 | SiteID)",
#                                  criteria = "AIC")
# uvm_back_mdl <- lmerStepBackward(data=UVdata,
#                                  fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
#                                  random.formula = "(1 | SiteID)",
#                                  criteria = "MDL")
uvm_back_bic <- lmerStepBackward(data=UVdata,
                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "BIC")

### Forward selection
# uvm_forw_aic <- lmerStepForward(data=UVdata,
#                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
#                                 random.formula = "(1 | SiteID)",
#                                 criteria = "AIC")
# uvm_forw_mdl <- lmerStepForward(data=UVdata,
#                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
#                                 random.formula = "(1 | SiteID)",
#                                 criteria = "MDL")
# uvm_forw_bic <- lmerStepForward(data=UVdata,
#                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
#                                 random.formula = "(1 | SiteID)",
#                                 criteria = "BIC")
# 
# 
# save(uvm_back_aic, uvm_back_mdl, uvm_back_bic,
#      uvm_forw_aic, uvm_forw_mdl, uvm_forw_bic,
#      file="modelSelectionFits_uvm.RData")
# 
# load("modelSelectionFits_uvm.RData")
# 
# uvm_back_aic$BEST_FIXED_TERMS
# uvm_back_mdl$BEST_FIXED_TERMS
# uvm_back_bic$BEST_FIXED_TERMS
# 
# uvm_forw_aic$BEST_FIXED_TERMS
# uvm_forw_mdl$BEST_FIXED_TERMS
# uvm_forw_bic$BEST_FIXED_TERMS





## Backward selection using BIC chosen model
uvm_back_bic$BEST_FIXED_TERMS

uvm.lmer <- lmer(log10(ss.1pc.estimate) ~  log10(DOC) +  Season + WaterBody + RiverPresent +
                   Season:WaterBody + log10(DOC):WaterBody + Season:RiverPresent +
                        (1|SiteID), data=UVdata)  

BIC(uvm.lmer)
anova(uvm.lmer)
## Remember, look at interactions first, the main effects p-values
##   do not mean anything here.

## A quick residuals analysis
plot(fitted(uvm.lmer), residuals(uvm.lmer))
qqnorm(residuals(uvm.lmer))
qqline(residuals(uvm.lmer))
## All looks satisfactory to me


library(emmeans)

##########################
## Some follow-up comparison
##
contrast(emmeans(uvm.lmer, ~ Season | WaterBody ), by="WaterBody", "pairwise" )
plot(contrast(emmeans(uvm.lmer, ~ Season | WaterBody ), by="WaterBody", "pairwise" ) )
## In Embayments, difference in
##   Spring & Summer, Spring & Fall
## In Open Waters, difference in
##    Spring & Summer, Summer & Fall
contrast(emmeans(uvm.lmer, ~ Season | RiverPresent ), by="RiverPresent", "pairwise" )
plot(contrast(emmeans(uvm.lmer, ~ Season | RiverPresent ), by="RiverPresent", "pairwise" ) )
## When River is present, difference
##     From Spring to Fall, and Spring to Summer
## When no river present
##    Difference from summer & fall, summer & spring
## NOTE similar results

## This effectively shows the influence
##    WaterBody has on how DOC effects UV
emmip(uvm.lmer, WaterBody ~ log10(DOC), cov.reduce = range, CIs = TRUE)
## Lines are clearly NOT parallel
##   Showing there is a clear interaction
## So basically, the relationship between log10(DOC)
##   and UVB depends on WaterBody type
emmeans(uvm.lmer, ~ log10(DOC) | WaterBody)
## For Embayment, a 1 unit increase in log10(DOC)
##   will lead to 0.252 more log10(UV) transparency
##   CI: 0.194--0.311 more UVM
## For Open Water, ...
##   will lead to 0.474 more log10(UV) depth
##   CU: 0.416--0.533 more UVM


###############################################
##
## Plot the fitted models -- NEED TO DO
##
##  Now, DOC on the x-axis with constant time
###############################################

min.doc <- min(UVdata$DOC)
max.doc <- max(UVdata$DOC)
UVdata_doc_range <- UVdata |>
  group_by(Season, BlagraveID) |>
  summarize(DOC.min = min(DOC),
            DOC.max = max(DOC),
            WaterBody = first(WaterBody),
            RiverPresent = first(RiverPresent)) |>
 # pivot_longer(c(DOC.min, DOC.max), values_to="DOC" ) |>
  #dplyr::select(-name) |>
  group_by(Season, BlagraveID, WaterBody, RiverPresent) |>
  mutate(DOC = 0) |>
  complete(DOC = seq(DOC.min, DOC.max, 0.02) ) |>
  dplyr::filter(DOC > 0) |>
  ungroup() |>
  dplyr::select(Season, BlagraveID, WaterBody, RiverPresent, DOC) |>
  mutate(SiteID = -1)

predict(uvm.lmer, as.data.frame(UVdata_doc_range),
        re.form=NA, se.fit=TRUE)

UVdata_pred <- UVdata_doc_range |>
  mutate(Pred = predict(uvm.lmer, newdata=as.data.frame(UVdata_doc_range), re.form=NA) ) %>%
  mutate(PredSmooth = 10^Pred) 
 


our_colors = c("#cd5a53",
               "#cd5a53",
               "#2eb5ce",
               "#2eb5ce")

p_log_uvm_fitted <- ggplot() + 
  geom_point(data=UVdata,
             aes(x=log10(DOC), y=log10(ss.1pc.estimate), 
                 color=BlagraveID), alpha=0.3, size=1.15 ) +
  geom_line(data=UVdata_pred, 
            aes(x=log10(DOC), y=Pred,  
                color=BlagraveID,  group=BlagraveID,
                linewidth=BlagraveID, linetype=BlagraveID ) ) +
  #facet_grid(Season ~ Lake) +
  facet_grid(.~Season ) +
  theme_bw() + 
  theme(legend.position="bottom" ) +
  scale_color_manual(name="Habitat", values=our_colors) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_linewidth_manual(values=c(0.75, 1, 0.75, 1), name="Habitat") +
  scale_x_continuous(breaks=seq(0,0.8, 0.2), limits=c(0,0.85) ) +
  #scale_y_continuous(breaks=seq(0, 12, 3), limits=c(0,12) ) +
  labs(title="Predicted 1% UV Depth (m) as a function of DOC by habitat and season",
       subtitle="Points correspond to observed data",       
       y=expression("Logarithm of 1% UV Depth"~~~log[10](m)),
       x=expression(log[10](DOC))) +
  theme(legend.key.width = unit(1, 'cm'))

p_uvm_fitted <- ggplot() + 
  geom_point(data=UVdata,
             aes(x=log10(DOC), y=(ss.1pc.estimate), 
                 color=BlagraveID), alpha=0.3, size=1.15 ) +
  geom_line(data=UVdata_pred, 
            aes(x=log10(DOC), y=PredSmooth,  
            color=BlagraveID,  group=BlagraveID,
            linewidth=BlagraveID, linetype=BlagraveID ) ) +
  #facet_grid(Season ~ Lake) +
  facet_grid(.~Season ) +
  theme_bw() + 
  theme(legend.position="bottom" ) +
  scale_color_manual(name="Habitat", values=our_colors) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_linewidth_manual(values=c(0.75, 1, 0.75, 1), name="Habitat") +
  scale_x_continuous(breaks=seq(0,0.8, 0.2), limits=c(0,0.85) ) +
  scale_y_continuous(breaks=seq(0, 12, 3), limits=c(0,12) ) +
  labs(title="Predicted 1% UV Depth (m) as a function of DOC by habitat and season",
       subtitle="Points correspond to observed data",       
       y="1% UV Depth (m)",
       x=expression(log[10](DOC))) +
  theme(legend.key.width = unit(1, 'cm'))

p_uvm_fitted_doc <- ggplot() + 
  geom_point(data=UVdata,
             aes(x=(DOC), y=(ss.1pc.estimate), 
                 color=BlagraveID, shape=BlagraveID), alpha=0.3, size=1.15 ) +
  geom_line(data=UVdata_pred, 
            aes(x=(DOC), y=PredSmooth,  
                color=BlagraveID,  group=BlagraveID,
                linewidth=BlagraveID, linetype=BlagraveID ) ) +
  #facet_grid(Season ~ Lake) +
  facet_grid(.~Season ) +
  theme_bw() + 
  theme(legend.position="bottom" ) +
  scale_shape_manual(name="Habitat", values=c(16,17,16,17) ) +
  scale_color_manual(name="Habitat", values=our_colors) +
  scale_linetype_manual(values = rev(c("solid","11", "solid", "11")), name="Habitat" ) +
  scale_linewidth_manual(values=rev(c(0.75, 1, 0.75, 1)), name="Habitat") +
  #scale_x_continuous(breaks=seq(0,0.8, 0.2), limits=c(0,0.85) ) +
  scale_y_continuous(breaks=seq(0, 12, 3), limits=c(0,12) ) +
  labs(y="1% UV-B (320 nm) Depth (m)",
       x="DOC (mg/L)") +
  theme(legend.key.width = unit(1, 'cm'))

p_log_uvm_fitted
p_uvm_fitted
p_uvm_fitted_doc

## Figure for manuscript
ggsave(plot=p_uvm_fitted_doc,
       filename="plots/pred_UVlevels_DOC.png", 
       width=8, height=2.95, bg="white")

p_uvm_fitted_doc <- p_uvm_fitted_doc +
  labs(title="Predicted 1% UV Depth (m) as a function of DOC by habitat and season",
       subtitle="Points correspond to observed data")

save(UVdata, UVdata_pred, uvm.lmer,
     p_log_uvm_fitted, p_uvm_fitted, p_uvm_fitted_doc, 
     file="fittedModelUV.RData")
