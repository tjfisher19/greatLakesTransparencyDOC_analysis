##########################################
## Modeling DOC
##
## Here we look at modeling DOC as a function
##   of the variables: Lake, Habitat, Season & Year
##
## This is not the key feature of the analysis
##   but will be a discussion point as it is 
##   well understood that DOC relates to UV-B & PAR
##   in a non-linear relationship.

library(tidyverse)
library(lme4)
library(emmeans)
library(lmerTest)
library(merTools)

load("fully_processed_data.RData")

## Tom Fisher code that does step-wise lmer fitting
source("stepwise_lmer_code.R")

################################################
## As a first step, we explore the results
##    of backward & forward stepwise regression
##    using the AIC, MDL and BIC criteria
##
## Ultimately, we will use backward selection BIC
##    but to see if we can gain any insight,
##    we consider the others as well.

### Backward selection
# doc_back_aic <- lmerStepBackward(data=drop_na(PARdata, DOC),
#                                  fixed.formula = "log10(DOC) ~ Lake * Year * Season * WaterBody*RiverPresent",
#                                  random.formula = "(1 | SiteID)",
#                                  criteria = "AIC")
# doc_back_mdl <- lmerStepBackward(data=drop_na(PARdata, DOC),
#                                  fixed.formula = "log10(DOC) ~ Lake * Year * Season * WaterBody*RiverPresent",
#                                  random.formula = "(1 | SiteID)",
#                                  criteria = "MDL")
doc_back_bic <- lmerStepBackward(data=drop_na(PARdata, DOC),
                                 fixed.formula = "log10(DOC) ~ Lake * Year * Season * WaterBody*RiverPresent",
                                 random.formula = "(1 | SiteID)",
                                 criteria = "BIC")

### Forward selection
# doc_forw_aic <- lmerStepForward(data=drop_na(PARdata, DOC),
#                                 fixed.formula = "log10(DOC) ~ Lake * Year * Season * WaterBody*RiverPresent",
#                                 random.formula = "(1 | SiteID)",
#                                 criteria = "AIC")
# doc_forw_mdl <- lmerStepForward(data=drop_na(PARdata, DOC),
#                                 fixed.formula = "log10(DOC) ~ Lake * Year * Season * WaterBody*RiverPresent",
#                                 random.formula = "(1 | SiteID)",
#                                 criteria = "MDL")
# doc_forw_bic <- lmerStepForward(data=drop_na(PARdata, DOC),
#                                 fixed.formula = "log10(DOC) ~ Lake * Year * Season * WaterBody*RiverPresent",
#                                 random.formula = "(1 | SiteID)",
#                                 criteria = "BIC")

# save(doc_back_aic, doc_back_mdl, doc_back_bic,
#      doc_forw_aic, doc_forw_mdl, doc_forw_bic,
#      file="modelSelectionFits_doc.RData")
# 
# load("modelSelectionFits_doc.RData")
# 
# doc_back_aic$BEST_FIXED_TERMS
# doc_back_mdl$BEST_FIXED_TERMS
# doc_back_bic$BEST_FIXED_TERMS
# 
# doc_forw_aic$BEST_FIXED_TERMS
# doc_forw_mdl$BEST_FIXED_TERMS
# doc_forw_bic$BEST_FIXED_TERMS



############################################
## Our Model
##
## Backward selection using BIC
##
############################################
doc_back_bic$BEST_FIXED_TERMS

doc.lmer <- lmer(log10(DOC) ~ Year + WaterBody + Season +  Season:WaterBody +
                   (1|SiteID), data=drop_na(PARdata,DOC) )

anova(doc.lmer)
summary(doc.lmer)
BIC(doc.lmer)
doc_back_bic$BEST_AIC

## A quick residuals analysis
plot(fitted(doc.lmer), residuals(doc.lmer))
qqnorm(residuals(doc.lmer))
qqline(residuals(doc.lmer))
## All reasonably satisfactory to me
##    Some skewness in there but already taking logs


######################################
## Some follow-up analysis
##   The Year variable is operating on its own
##     so you can practically treat it like SLR

emtrends(doc.lmer, ~ Year, var="Year")
## The trend is significant & positive,
##  For every year that goes by, expect DOC
##     to increase by 0.00234 log10(mg/L)
##  CI: 0.00144-0.00322 log10(mg/L) per year


########################################
## Some sort of interaction between
##   WaterBody & Season

emmeans(doc.lmer, ~ WaterBody | Season)
contrast(emmeans(doc.lmer, ~ WaterBody | Season), by="WaterBody", method="pairwise")
plot(contrast(emmeans(doc.lmer, ~ WaterBody | Season), by="WaterBody", method="pairwise") )
## In Embayments, we see a difference between
##   Spring and Fall
##   Summer and Fall
## No differences in Open Waters by season




####################################
## A plot of the fitted/predicted model
##   Not sure if this is helpful.

DOCdata_pred_range <- PARdata %>%
  group_by(Season, WaterBody) %>%
  complete(Year = seq(min(Year), max(Year)+1, 0.2) ) %>%
  distinct(Season, Year) %>%
  mutate(SiteID = -1) %>%
  ungroup() %>%
  drop_na()

predict(doc.lmer, as.data.frame(DOCdata_pred_range),
        re.form=NA, se.fit=TRUE)

DOCdata_pred <- DOCdata_pred_range %>%
  mutate(Pred = predict(doc.lmer, as.data.frame(DOCdata_pred_range),
                        re.form=NA),
         SE = predict(doc.lmer, as.data.frame(DOCdata_pred_range),
                      re.form=NA, se.fit=TRUE)$se.fit) %>%
  mutate(Lo = Pred - 2*SE,
         Hi = Pred + 2*SE) %>%
  mutate(PredSmooth = 10^Pred,
         LoSmooth = 10^Lo,
         HiSmooth = 10^Hi,
         YearPlot = ymd(paste0(trunc(Year) + 2001, "-01-01")) + days(round((Year-trunc(Year))*365 ) ) )


our_colors = c("#cd5a53",
               "#cd5a53",
               "#2eb5ce",
               "#2eb5ce")

ggplot(DOCdata_pred) + 
  geom_point(data=PARdata, alpha=0.3, size=0.95, aes(x=Date, y=DOC, color=WaterBody) ) +       ## Add data?
  #geom_ribbon(aes(x=YearPlot, ymin=Lo, ymax=Hi, fill=Season, group=Season), alpha=0.2) +               ## Ribbons -- too busy?
  #geom_errorbar(aes(x=YearPlot, ymin=LoSmooth, ymax=HiSmooth, color=Season), linewidth=0.35, show.legend=FALSE ) + ## Error bars -- maybe better?
  #geom_linerange(aes(x=YearPlot, ymin=LoSmooth, ymax=HiSmooth, color=Season), linewidth=3, alpha=0.4 ) +           ## Another version of error bars
  geom_line(aes(x=YearPlot, y=PredSmooth, color=WaterBody),
            linewidth=0.65) +
  #geom_point(aes(x=YearPlot, y=Pred, color=Season), size=2.2 ) +
  #facet_grid(Season ~ Lake) +
  #facet_wrap(~Lake)+ 
  facet_wrap(.~Season) + 
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  #scale_y_continuous(limits=c(-0.5,9), breaks=seq(0,9,3)) +                  ## Turn off if including the data
  scale_x_date(date_breaks="4 years", date_labels="%Y") +
  scale_color_manual(values=our_colors[2:3], name="Habitat") + 
  labs(title="Predicted DOC (mg/L) by season",
       subtitle="From Backward Selection using BIC") +
 # scale_linetype_manual(values = c("solid","dashed","11") ) +
  #scale_linewidth_manual(values=c(0.65, 0.75, 1, 1)) +
  theme(legend.key.width = unit(2, 'cm'))

ggsave(filename="plots/pred_doclevels_trend.png", width=8, height=2.95)

