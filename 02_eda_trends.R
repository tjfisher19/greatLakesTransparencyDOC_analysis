#################################################
##  Here, we do a little EDA 
##     (average profile/trends)
##
## We compare DOC in time
##   UV in time, PAR in time


##########################
## Color scheme for our plotting
##  The moron/brown are for embayments
##     the blue for open waters

our_colors = c("#cd5a53",
               "#cd5a53",
               "#2eb5ce",
               "#2eb5ce")

load("fully_processed_data.RData")

###################################################
## We fit a full interactive non-random effect model
##   just to plot any underlying trends
## They may be insignificant, but we are doing so
##   to visually explore
##
## First, DOC in time

doc_full_lm_model <- lm(log10(DOC) ~ Year*Season*Lake*WaterBody*RiverPresent,
                        data=PARdata)

fake_doc_data <- PARdata %>%
  group_by(Season, Lake, WaterBody, RiverPresent, Year) %>%
  complete(Date = seq(min(Date), max(Date), by="day") ) %>%
  ungroup() %>%
  mutate(Year = year(Date) + day(Date)/365 - 2001 )

fake_doc_data <- fake_doc_data %>%
  mutate(Fitted_Log = predict(doc_full_lm_model, newdata=fake_doc_data),
         Fitted_orig = 10^Fitted_Log) %>%
  drop_na(BlagraveID)

obs_trends_doc <- ggplot(PARdata, aes(color=BlagraveID, group=BlagraveID, 
                                      linetype=BlagraveID, linewidth=BlagraveID)) + 
  geom_line(data=fake_doc_data, aes(x=Date, y=Fitted_orig), alpha=0.6 ) +
  geom_point(alpha=0.6, size=1.5, aes(x=Date, y=DOC, shape=BlagraveID) ) +
  facet_grid(Season ~ Lake) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  labs(title="Observed DOC (mg/L) by lake, season, and habitat",
       subtitle="Underlying average trend provided") +
  scale_y_continuous(limits=c(0,8)) +
  scale_x_date(date_breaks="4 years", date_labels="%Y") +
  scale_color_manual(values=our_colors, name="Habitat") +
  scale_shape_manual(values=c(16,17,16,17), name="Habitat" ) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_linewidth_manual(values=c(0.65, 1, 0.65, 1), name="Habitat") +
  theme(legend.key.width = unit(2, 'cm'))

obs_trends_doc


###############################################
## Now, UV in time


uv_trend_full_lm_model <- lm(log10(ss.1pc.estimate) ~ Year*Season*Lake*WaterBody*RiverPresent,
                        data=UVdata)

fake_uv_trend_data <- UVdata %>%
  group_by(Season, Lake, WaterBody, RiverPresent, Year) %>%
  complete(Date = seq(min(Date), max(Date), by="day") ) %>%
  ungroup() %>%
  mutate(Year = year(Date) + day(Date)/365 - 2001 )

fake_uv_trend_data <- fake_uv_trend_data %>%
  mutate(Fitted_Log = predict(uv_trend_full_lm_model, newdata=fake_uv_trend_data),
         Fitted_orig = 10^Fitted_Log) %>%
  drop_na(BlagraveID)

obs_trends_uv <- ggplot(UVdata, aes(x=YearPlot, y=ss.1pc.estimate, color=BlagraveID, group=BlagraveID, 
                                    linetype=BlagraveID, linewidth=BlagraveID)) + 
  geom_point(alpha=0.6, size=1.5, aes(x=Date) ) +
  geom_line(data=fake_uv_trend_data, aes(x=Date, y=Fitted_orig), alpha=0.6 ) + 
  #stat_summary(fun="mean", geom="line", mapping=aes(x=YearPlot)) + 
  #stat_summary(fun="mean", geom="point", size=2.2, mapping=aes(x=YearPlot)) + 
  facet_grid(Season ~ Lake) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  labs(title="1% UV Depth (m) in time by lake, season and habitat",
       subtitle="Underlying average trend provided") +
  scale_x_date(date_breaks="4 years", date_labels="%Y") +
  scale_color_manual(values=our_colors, name="Habitat") +
  scale_shape_manual(values=c(16,17,16,17), name="Habitat" ) +
  #scale_color_manual(values=c("brown", "brown", "blue", "blue")) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_linewidth_manual(values=c(0.65, 1, 0.65, 1), name="Habitat") +
  theme(legend.key.width = unit(2, 'cm'))

obs_trends_uv


#############################################
##
## Now, PAR in time
##


par_trend_full_lm_model <- lm(log10(ss.1pc.estimate) ~ Year*Season*Lake*WaterBody*RiverPresent,
                             data=PARdata)

fake_par_trend_data <- PARdata %>%
  group_by(Season, Lake, WaterBody, RiverPresent, Year) %>%
  complete(Date = seq(min(Date), max(Date), by="day") ) %>%
  ungroup() %>%
  mutate(Year = year(Date) + day(Date)/365 - 2001 )

fake_par_trend_data <- fake_par_trend_data %>%
  mutate(Fitted_Log = predict(par_trend_full_lm_model, newdata=fake_par_trend_data),
         Fitted_orig = 10^Fitted_Log) %>%
  drop_na(BlagraveID)

obs_trends_par <- ggplot(PARdata, aes(x=YearPlot, y=ss.1pc.estimate, color=BlagraveID, group=BlagraveID, 
                                      linetype=BlagraveID, linewidth=BlagraveID, shape=BlagraveID)) + 
  geom_point(alpha=0.6, size=1.5, aes(x=Date) ) +
  geom_line(data=fake_par_trend_data, aes(x=Date, y=Fitted_orig), alpha=0.6 ) + 
  #stat_summary(fun="mean", geom="line", mapping=aes(x=YearPlot)) + 
  #stat_summary(fun="mean", geom="point", size=2.2, mapping=aes(x=YearPlot)) + 
  facet_grid(Season ~ Lake) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  labs(title="1% PAR Depth (m) in time by lake, season and habitat",
       subtitle="Underlying average trend provided") +
  scale_y_continuous(limits=c(0,60)) +
  scale_x_date(date_breaks="4 years", date_labels="%Y") +
  scale_color_manual(values=our_colors, name="Habitat") +
  scale_shape_manual(values=c(16,17,16,17), name="Habitat" ) +
  #scale_color_manual(values=c("brown", "brown", "blue", "blue")) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_linewidth_manual(values=c(0.65, 1, 0.65, 1), name="Habitat") +
  theme(legend.key.width = unit(2, 'cm'))

obs_trends_par



################################################
## Now, UV as a function of log-DOC



uv_doc_full_lm_model <- lm(log10(ss.1pc.estimate) ~ Season*Lake*WaterBody*RiverPresent*log10(DOC),
                           data=UVdata)

UVdata_doc_range <- UVdata %>%
  group_by(Lake, Season, BlagraveID, WaterBody, RiverPresent) %>%
  complete(DOC = seq(min(DOC), max(DOC), 0.02) ) %>%
  ungroup()

UVdata_pred <- UVdata_doc_range %>%
  mutate(Pred = predict(uv_doc_full_lm_model, newdata=UVdata_doc_range) ) %>%
  mutate(PredSmooth = 10^Pred) 


obs_uv_doc <- ggplot() + 
  geom_point(data=UVdata,
             aes(x=log10(DOC), y=(ss.1pc.estimate), 
                 color=BlagraveID, shape=BlagraveID), size=1.5, alpha=0.6 ) +
  geom_line(data=UVdata_pred, 
            aes(x=log10(DOC), y=PredSmooth,  
                color=BlagraveID,  group=BlagraveID,
                linewidth=BlagraveID, linetype=BlagraveID), alpha=0.6) +
  facet_grid(Season ~ Lake) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  scale_color_manual(values=our_colors, name="Habitat") +
  scale_shape_manual(values=c(16,17,16,17), name="Habitat" ) +
  #scale_color_manual(values=c("brown", "brown", "blue", "blue")) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_linewidth_manual(values=c(0.65, 1, 0.65, 1), name="Habitat") +
  labs(title="1% UV Depth (m) as a function of DOC by lake, season and habitat",
       subtitle="Underlying average trend provided") +
  theme(legend.key.width = unit(1, 'cm'))
obs_uv_doc




################################################
## Now, PAR as a function of log-DOC

par_doc_full_lm_model <- lm(log10(ss.1pc.estimate) ~ Season*Lake*WaterBody*RiverPresent*log10(DOC),
                           data=PARdata)

PARdata_doc_range <- PARdata %>%
  group_by(Lake, Season, BlagraveID, WaterBody, RiverPresent) %>%
  complete(DOC = seq(min(DOC), max(DOC), 0.02) ) %>%
  ungroup()

PARdata_pred <- PARdata_doc_range %>%
  mutate(Pred = predict(par_doc_full_lm_model, newdata=PARdata_doc_range) ) %>%
  mutate(PredSmooth = 10^Pred) 


obs_par_doc <- ggplot() + 
  geom_point(data=PARdata,
             aes(x=log10(DOC), y=(ss.1pc.estimate), 
                 color=BlagraveID, shape=BlagraveID), size=1.5, alpha=0.6 ) +
  geom_line(data=PARdata_pred, 
            aes(x=log10(DOC), y=PredSmooth,  
                color=BlagraveID,  group=BlagraveID,
                linewidth=BlagraveID, linetype=BlagraveID), alpha=0.6) +
  facet_grid(Season ~ Lake) +
  theme_bw() + 
  theme(legend.position="bottom",
        axis.title=element_blank() ) +
  scale_color_manual(values=our_colors, name="Habitat") +
  scale_shape_manual(values=c(16,17,16,17), name="Habitat" ) +
  #scale_color_manual(values=c("brown", "brown", "blue", "blue")) +
  scale_linetype_manual(values = c("solid","11", "solid", "11"), name="Habitat" ) +
  scale_linewidth_manual(values=c(0.65, 1, 0.65, 1), name="Habitat") +
  labs(title="1% PAR Depth (m) as a function of DOC by lake, season and habitat",
       subtitle="Underlying average trend provided") +
  theme(legend.key.width = unit(1, 'cm'))
obs_par_doc




#############################################
## Save the plots as nice png files.

ggsave(filename="plots/eda_DOCtrends.png", plot=obs_trends_doc,
       width=8, height=6)
ggsave(filename="plots/eda_UVtrends.png", plot=obs_trends_uv,
       width=8, height=6)
ggsave(filename="plots/eda_PARtrends.png", plot=obs_trends_par,
       width=8, height=6)
ggsave(filename="plots/eda_UV_DOC.png", plot=obs_uv_doc,
       width=8, height=6)
ggsave(filename="plots/eda_PAR_DOC.png", plot=obs_par_doc,
       width=8, height=6)
