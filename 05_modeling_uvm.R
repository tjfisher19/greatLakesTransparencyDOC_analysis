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
uvm_back_aic <- lmerStepBackward(data=UVdata,
                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "AIC")
uvm_back_mdl <- lmerStepBackward(data=UVdata,
                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "MDL")
uvm_back_bic <- lmerStepBackward(data=UVdata,
                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "BIC")

### Forward selection
uvm_forw_aic <- lmerStepForward(data=UVdata,
                                fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                random.formula = "(1 | SiteID)",
                                criteria = "AIC")
uvm_forw_mdl <- lmerStepForward(data=UVdata,
                                fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                random.formula = "(1 | SiteID)",
                                criteria = "MDL")
uvm_forw_bic <- lmerStepForward(data=UVdata,
                                fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                random.formula = "(1 | SiteID)",
                                criteria = "BIC")


save(uvm_back_aic, uvm_back_mdl, uvm_back_bic,
     uvm_forw_aic, uvm_forw_mdl, uvm_forw_bic,
     file="modelSelectionFits_uvm.RData")

load("modelSelectionFits_uvm.RData")

uvm_back_aic$BEST_FIXED_TERMS
uvm_back_mdl$BEST_FIXED_TERMS
uvm_back_bic$BEST_FIXED_TERMS

uvm_forw_aic$BEST_FIXED_TERMS
uvm_forw_mdl$BEST_FIXED_TERMS
uvm_forw_bic$BEST_FIXED_TERMS





## Backward selection using BIC chosen model
uvm_back_bic$BEST_FIXED_TERMS

uvm.lmer <- lmer(log10(ss.1pc.estimate) ~  log10(DOC) +  WaterBody +  WaterBody:Season + log10(DOC):WaterBody +
                        (1|SiteID), data=UVdata)  

BIC(uvm.lmer)
anova(uvm.lmer)

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

## This effectively shows the influence
##    WaterBody has on how DOC effects UV
emmip(uvm.lmer, WaterBody ~ log10(DOC), cov.reduce = range, CIs = TRUE)
## Lines are clearly NOT parallel
##   Showing there is a clear interaction
## So basically, the relationship between log10(DOC)
##   and UVB depends on WaterBody type
emmeans(uvm.lmer, ~ log10(DOC) | WaterBody)
## For Embayment, a 1 unit increase in log10(DOC)
##   will lead to 0.205--0.313 more UVM
## FOr Open Water, ...
##    0.419--0.536 more UVM


###############################################
##
## Plot the fitted models -- NEED TO DO
##
##  Now, DOC on the x-axis with constant time
###############################################

min.doc <- min(UVdata$DOC)
max.doc <- max(UVdata$DOC)
UVdata_doc_range <- UVdata %>%
  group_by(WaterBody, Season) %>%
  complete(DOC = seq(min.doc, max.doc, 0.02) ) %>%
  dplyr::select(Season, WaterBody, DOC) |>
  ungroup() |>
  distinct() |>
  drop_na() |>
  mutate(SiteID = -1)

predict(uvm.lmer, as.data.frame(UVdata_doc_range),
        re.form=NA, se.fit=TRUE)

UVdata_pred <- UVdata_doc_range %>%
  mutate(Pred = predict(uvm.lmer, newdata=as.data.frame(UVdata_doc_range), re.form=NA) ) %>%
  mutate(PredSmooth = 10^Pred) 
 # group_by(Lake, Season, DOC, RiverPresent, WaterBody, BelgraveID) %>%
  #summarize(Pred = mean(Pred),
  #          PredSmooth = 10^Pred)


our_colors = c("#cd5a53",
               "#cd5a53",
               "#2eb5ce",
               "#2eb5ce")

ggplot() + 
  geom_point(data=UVdata,
             aes(x=log10(DOC), y=(ss.1pc.estimate), 
                 color=WaterBody), alpha=0.3, size=1.15 ) +
  geom_line(data=UVdata_pred, 
            aes(x=log10(DOC), y=PredSmooth,  
            color=WaterBody,  group=WaterBody),
            linewidth=0.65) +
  #facet_grid(Season ~ Lake) +
  facet_grid(.~Season ) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  scale_color_manual(name="Habitat", values=our_colors[2:3]) +
  labs(title="Predicted 1% UV Depth (m) as a function of DOC by habitat and season",
       subtitle="Points correspond to observed data") +
  theme(legend.key.width = unit(1, 'cm'))

ggsave(filename="plots/pred_UVlevels_DOC.png", width=8, height=3)

