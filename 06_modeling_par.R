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
par_back_aic <- lmerStepBackward(data=PARdata,
                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "AIC")
par_back_mdl <- lmerStepBackward(data=PARdata,
                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "MDL")
par_back_bic <- lmerStepBackward(data=PARdata,
                                 fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "BIC")

### Forward selection
par_forw_aic <- lmerStepForward(data=PARdata,
                                fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                random.formula = "(1 | SiteID)",
                                criteria = "AIC")
par_forw_mdl <- lmerStepForward(data=PARdata,
                                fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                random.formula = "(1 | SiteID)",
                                criteria = "MDL")
par_forw_bic <- lmerStepForward(data=PARdata,
                                fixed.formula = "log10(ss.1pc.estimate) ~ Lake * Year * Season * log10(DOC) * WaterBody*RiverPresent",
                                random.formula = "(1 | SiteID)",
                                criteria = "BIC")

save(par_back_aic, par_back_mdl, par_back_bic,
     par_forw_aic, par_forw_mdl, par_forw_bic,
     file="modelSelectionFits_par.RData")

load("modelSelectionFits_par.RData")

par_back_aic$BEST_FIXED_TERMS
par_back_mdl$BEST_FIXED_TERMS
par_back_bic$BEST_FIXED_TERMS

par_forw_aic$BEST_FIXED_TERMS
par_forw_mdl$BEST_FIXED_TERMS
par_forw_bic$BEST_FIXED_TERMS




##############################################
##############################################
## Using backward selection with BIC

par_back_bic$BEST_FIXED_TERMS

par.lmer <- lmer(log10(ss.1pc.estimate) ~ Lake + Season + log10(DOC) + WaterBody + log10(DOC)*WaterBody + 
                        (1|SiteID), data=PARdata)  
BIC(par.lmer)
anova(par.lmer)

## A quick residuals analysis
plot(fitted(par.lmer), residuals(par.lmer))
qqnorm(residuals(par.lmer))
qqline(residuals(par.lmer))
## All looks satisfactory to me


library(emmeans)

plot(contrast(emmeans(par.lmer, ~ Lake ), "pairwise" ) )
## Superior & Ontario behaving the same
##   All others different

plot(contrast(emmeans(par.lmer, ~ Season ), "pairwise" ) )
contrast(emmeans(par.lmer, ~ Season ), "pairwise" )
## Spring & Fall different, Summer & fall different
##    Spring & Summer technically not different.


emmip(par.lmer, WaterBody ~ log10(DOC), cov.reduce = range)
## Clearly not parallel, so 
##   the interaction is very strong.
emmeans(par.lmer, ~ log10(DOC) | WaterBody)
## In Embayment, the trend on log10(DOC) is
##    between 0.981 and 1.07
## In Open Waters, a stronger trend between
##    1.082 and 1.17
## For both, a 1 unit increase in log10(DOC)
##    would have that effect on PAR



###############################################
##
## Plot the fitted models
##
##  Now, DOC on the x-axis with constant time
###############################################

min.doc <- min(PARdata$DOC)
max.doc <- max(PARdata$DOC)

PARdata_doc_range <- PARdata %>%
  group_by(Season, WaterBody, Lake) %>%
  complete(DOC = seq(min.doc, max.doc, 0.02) ) %>%
  ungroup() |>
  dplyr::select(Lake, Season, DOC, WaterBody) |>
  drop_na() |>
  distinct() |>
  mutate(SiteID = -1)

predict(par.lmer, newdata=as.data.frame(PARdata_doc_range), re.form=NA)

PARdata_pred <- PARdata_doc_range %>%
  mutate(Pred = predict(par.lmer, newdata=as.data.frame(PARdata_doc_range), re.form=NA) ) %>%
  mutate(PredSmooth = 10^Pred) 
 # group_by(Lake, Season, DOC, RiverPresent, WaterBody, BelgraveID) %>%
  #summarize(Pred = mean(Pred),
  #          PredSmooth = 10^Pred)


our_colors = c("#cd5a53",
               "#cd5a53",
               "#2eb5ce",
               "#2eb5ce")

ggplot() + 
  geom_point(data=PARdata,
             aes(x=log10(DOC), y=(ss.1pc.estimate), 
                 color=WaterBody), alpha=0.3, size=1.15 ) +
  geom_line(data=PARdata_pred, 
            aes(x=log10(DOC), y=PredSmooth,  
            color=WaterBody,  group=WaterBody),
            linewidth=0.65) +
  facet_grid(Season ~ Lake) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_color_manual(name="Habitat", values=our_colors[2:3]) +
  labs(title="Predicted PAR Depth (m) as a function of DOC by habitat and season",
       subtitle="Points correspond to observed data") +
  theme(legend.key.width = unit(1, 'cm'))

ggsave(filename="plots/pred_PARlevels_DOC.png", width=8, height=6)

