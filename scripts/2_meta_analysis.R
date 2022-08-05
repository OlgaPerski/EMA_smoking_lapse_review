source(here::here("scripts", "0_load_packages_data.R"))

# prepare data for meta-analysis ------------------------------------------

# join with larger EMA review data to pull in quality indicators, study design, etc

data_large_review <- read_rds(here::here("data", "large_review_clean.rds")) %>%
  mutate(id = case_when(str_detect(id, "[abcdef]") ~ as.character(id),
                        TRUE ~ as.character(as.integer(id))))

data_joined <- data_large_review %>%
  filter(id %in% data_descriptives$id)

df_joined <- subset(data_joined, author!="Scholz")

vars <- c("id", "mean_age", "female_sex_percentage", "ethnicity_white_percentage",
          "EMA_study_type", "study_duration_days", "incentive_schedule",
          "quality1", "quality2", "quality3", "quality4")

df_joined_red <- df_joined %>%
  select(any_of(vars))

# join with descriptives data frame

df_desc <- left_join(df_joined_red, data_descriptives)

# join with meta analysis data frame

df_all <- left_join(df_desc, data_meta_analysis, by = "id")

summary(df_all)

median(df_all$mean_age, na.rm = T)
median(df_all$female_sex_percentage, na.rm = T)
median(df_all$ethnicity_white_percentage, na.rm = T)
median(df_all$mean_cpd, na.rm = T)

data <- df_all %>%
  mutate(cessation_support = case_when(is.na(cessation_support_beh) & is.na(cessation_support_pharm) ~ "No support/not reported",
                                       cessation_support_beh == "yes" & cessation_support_pharm == "yes" ~ "Combined support",
                                       cessation_support_beh == "yes" & is.na(cessation_support_pharm) ~ "Behavioural support only",
                                       cessation_support_pharm == "yes" & is.na(cessation_support_beh) ~ "Pharmacological support only",
                                       TRUE ~ "No support/not reported"),
         mean_age = case_when(is.na(mean_age) ~ 42.8, # impute missing using the median value
                              TRUE ~ mean_age),
         female_sex_percentage = case_when(is.na(female_sex_percentage) ~ 56.1,
                                           TRUE ~ female_sex_percentage),
         ethnicity_white_percentage = case_when(is.na(ethnicity_white_percentage) ~ 83,
                                                TRUE ~ ethnicity_white_percentage),
         mean_cpd = case_when(is.na(mean_cpd) ~ 21.4,
                              TRUE ~ mean_cpd))

# retain vars with sufficient variability for moderator analyses

vars1 <- c("author.x", "year", "study_nr.y",
           "mean_age", "female_sex_percentage", 
           "ethnicity_white_percentage",
           "mean_cpd",
           "cessation_support",
           "EMA_study_type",
           "study_duration_days",
           "incentive_schedule", 
           "random_intercept_within",
           "random_slope_within",
           "quality3",
           "ema_psych_context_predictor", 
           "ema_psych_context_predictor_coding",
           "es_id", "logOR_within",
           "logOR_SE_within")

data1 <- data %>%
  select(any_of(vars1)) %>%
  filter(!is.na(logOR_within)) %>%
  mutate(author = as.factor(author.x),
         ema_psych_context_predictor_coding = as.factor(ema_psych_context_predictor_coding),
         random_slope_within = as.factor(random_slope_within),
         study_nr = as.numeric(study_nr.y),
         quality3 = as.factor(quality3),
         mean_age = as.numeric(mean_age),
         female_sex_percentage = as.numeric(female_sex_percentage),
         ethnicity_white_percentage = as.numeric(ethnicity_white_percentage),
         mean_cpd = as.numeric(mean_cpd),
         EMA_study_type = as.factor(EMA_study_type),
         incentive_schedule = as.factor(case_when(incentive_schedule == "NR" ~ "No/not reported",
                                                  incentive_schedule == "Not reported" ~ "No/not reported",
                                                  incentive_schedule == "Payment per EMA" ~ "Payment per EMA",
                                                  incentive_schedule == "Flat payment based on study completion" ~ "Flat payment",
                                                  incentive_schedule == "Muliple" ~ "Multiple",
                                                  incentive_schedule == "Other" ~ "Other",
                                                  TRUE ~ "Not reported")),
         cessation_support = as.factor(cessation_support),
         random_intercept_within = as.factor(case_when(random_intercept_within == "NR" ~ "No/not reported",
                                                       random_intercept_within == "yes" ~ "Yes",
                                                       TRUE ~ "No/not reported")),
         random_slope_within = as.factor(case_when(random_slope_within == "NR" ~ "No/not reported",
                                                   random_slope_within == "yes" ~ "Yes",
                                                   TRUE ~ "No/not reported")),
         quality3 = as.factor(case_when(quality3 == "NR" ~ "Not reported",
                                        quality3 == "Weak" ~ "Weak",
                                        quality3 == "Moderate" ~ "Moderate",
                                        quality3 == "Strong" ~ "Strong",
                                        TRUE ~ "Not reported")))

data1$quality3 <- relevel(data1$quality3, "Weak")
data1$EMA_study_type <- relevel(data1$EMA_study_type, "Observational")
data1$incentive_schedule <- relevel(data1$incentive_schedule, "No/not reported")
data1$cessation_support <- relevel(data1$cessation_support, "No support/not reported")
data1$random_intercept_within <- relevel(data1$random_intercept_within, "No/not reported")
data1$random_slope_within <- relevel(data1$random_slope_within, "No/not reported")

summary(data1)

# count number of studies with data suitable for meta-analysis

data1 %>%
  mutate(study_nr = as.factor(study_nr)) %>%
  count(study_nr)

# if k > 10, run meta-analysis

data1 %>%
  count(ema_psych_context_predictor_coding) %>%
  arrange(-n) # run for 1) negative feeling states, 2) environmental and social cues, 3) cravings

# negative feeling states -------------------------------------------------

df1 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "negative feeling states")

df1.model <- rma.mv(yi = logOR_within, 
                     V = logOR_SE_within, 
                     slab = author,
                     data = df1,
                     random = ~ 1 | author/es_id, 
                     test = "z", 
                     method = "REML")

summary(df1.model)

df1_i2 <- var.comp(df1.model)

summary(df1_i2)

png(here("outputs", "df1.model.forest.png"), width = 600, height = 600)

forest.rma(df1.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-11.7, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                              .(formatC(df1_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# funnel plot

png(here("outputs", "df1.model.funnel.png"), width = 600, height = 600)

funnel(df1.model, main = "Funnel Plot", xlab = "Log Odds Ratio")

dev.off()

if(!file.exists(here("outputs", "df1.model.funnel.png"))) ggsave(df1.model.funnel, filename = here("outputs", "df1.model.funnel.png"), 
                                                          dpi = 320, height = 8, width = 10)

# Egger's test

df1.model.egg <- rma.mv(yi = logOR_within, 
                         V = logOR_SE_within, 
                         slab = author,
                         data = df1,
                         random = ~ 1 | author/es_id, 
                         test = "z", 
                         method = "REML",
                         mods = ~ logOR_SE_within)

summary(df1.model.egg)

# moderator analysis

df1.model.mods <- rma.mv(yi = logOR_within, 
                         V = logOR_SE_within, 
                         slab = author,
                         data = df1,
                         random = ~ 1 | author/es_id, 
                         test = "z", 
                         method = "REML",
                         mods = ~ mean_age + female_sex_percentage + ethnicity_white_percentage +
                           mean_cpd + cessation_support + EMA_study_type + study_duration_days + 
                           incentive_schedule + random_intercept_within + random_slope_within + quality3)

summary(df1.model.mods)

# sensitivity analysis with robust variance estimation

yi = df1$logOR_within
vi = df1$logOR_SE_within

df1.model.rve <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.8,
                      small = TRUE,
                      data = df1)

print(df1.model.rve)

# environmental and social cues -------------------------------------------------

df2 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "environmental and social cues")

df2.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df2,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df2.model)

df2_i2 <- var.comp(df2.model)

summary(df2_i2)

png(here("outputs", "df2.model.forest.png"), width = 600, height = 600)

forest.rma(df2.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-135, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                 .(formatC(df1_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# leave-one-out sensitivity analysis

df2_sens <- data1 %>%
  filter(ema_psych_context_predictor_coding == "environmental and social cues",
         es_id != "id_45",
         es_id != "id_51")

df2.model.sens <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df2_sens,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df2.model.sens)

df2_i2_sens <- var.comp(df2.model.sens)

summary(df2_i2_sens)

png(here("outputs", "df2.model.forest_sens.png"), width = 600, height = 600)

forest.rma(df2.model.sens, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-135, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                .(formatC(df2_i2_sens[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# funnel plot

png(here("outputs", "df2.model.funnel.png"), width = 600, height = 600)

funnel(df2.model, main = "Funnel Plot", xlab = "Log Odds Ratio")

dev.off()

if(!file.exists(here("outputs", "df2.model.funnel.png"))) ggsave(df2.model.funnel, filename = here("outputs", "df2.model.funnel.png"), 
                                                                 dpi = 320, height = 8, width = 10)

# Egger's test

df2.model.egg <- rma.mv(yi = logOR_within, 
                        V = logOR_SE_within, 
                        slab = author,
                        data = df2,
                        random = ~ 1 | author/es_id, 
                        test = "z", 
                        method = "REML",
                        mods = ~ logOR_SE_within)

summary(df2.model.egg)

# moderator analysis

df2.model.mods <- rma.mv(yi = logOR_within, 
                         V = logOR_SE_within, 
                         slab = author,
                         data = df2,
                         random = ~ 1 | author/es_id, 
                         test = "z", 
                         method = "REML",
                         mods = ~ mean_age + female_sex_percentage + ethnicity_white_percentage +
                           mean_cpd + cessation_support + EMA_study_type + study_duration_days + 
                           incentive_schedule + random_intercept_within + random_slope_within + quality3)

summary(df2.model.mods)

df2_mod_i2 <- var.comp(df2.model.mods)

summary(df2_mod_i2)

# sensitivity analysis with robust variance estimation

yi = df2$logOR_within
vi = df2$logOR_SE_within

df2.model.rve <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.8,
                      small = TRUE,
                      data = df2)

print(df2.model.rve)

# cravings ----------------------------------------------------------------

df3 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "cravings")

df3.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df3,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df3.model)

df3_i2 <- var.comp(df3.model)

summary(df3_i2)

png(here("outputs", "df3.model.forest.png"), width = 600, height = 600)

forest.rma(df3.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-5.2, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                .(formatC(df2_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# funnel plot

png(here("outputs", "df3.model.funnel.png"), width = 600, height = 600)

funnel(df3.model, main = "Funnel Plot", xlab = "Log Odds Ratio")

dev.off()

# Egger's test

df3.model.egg <- rma.mv(yi = logOR_within, 
                        V = logOR_SE_within, 
                        slab = author,
                        data = df3,
                        random = ~ 1 | author/es_id, 
                        test = "z", 
                        method = "REML",
                        mods = ~ logOR_SE_within)

summary(df3.model.egg)

# moderator analysis

df3.model.mods <- rma.mv(yi = logOR_within, 
                         V = logOR_SE_within, 
                         slab = author,
                         data = df3,
                         random = ~ 1 | author/es_id, 
                         test = "z", 
                         method = "REML",
                         mods = ~ mean_age + female_sex_percentage + ethnicity_white_percentage +
                           mean_cpd + cessation_support + EMA_study_type + study_duration_days + 
                           incentive_schedule + quality3) # model did not converge with model parameter vars included

summary(df3.model.mods)

df3_mod_i2 <- var.comp(df3.model.mods)

summary(df3_mod_i2)

# sensitivity analysis with robust variance estimation

yi = df3$logOR_within
vi = df3$logOR_SE_within

df3.model.rve <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.8,
                      small = TRUE,
                      data = df3)

print(df3.model.rve)

### plot effect sizes for the other constructs (apart from the 'other' category) to aid the narrative synthesis

# behavioural regulation

df4 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "behavioural regulation")

df4.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df4,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df4.model)

df4_i2 <- var.comp(df4.model)

summary(df4_i2)

png(here("outputs", "df4.model.forest.png"), width = 600, height = 600)

forest.rma(df4.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-5.2, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                .(formatC(df4_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# motivation not to smoke

df5 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "motivation not to smoke")

df5.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df5,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df5.model)

df5_i2 <- var.comp(df5.model)

summary(df5_i2)

png(here("outputs", "df5.model.forest.png"), width = 600, height = 600)

forest.rma(df5.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-5.2, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                .(formatC(df5_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# beliefs about capabilities

df6 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "beliefs about capabilities")

df6.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df6,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df6.model)

df6_i2 <- var.comp(df6.model)

summary(df6_i2)

png(here("outputs", "df6.model.forest.png"), width = 600, height = 600)

forest.rma(df6.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-5.2, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                .(formatC(df6_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# positive feeling states

df7 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "positive feeling states")

df7.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df7,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df7.model)

df7_i2 <- var.comp(df7.model)

summary(df7_i2)

png(here("outputs", "df7.model.forest.png"), width = 600, height = 600)

forest.rma(df7.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-5.2, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                .(formatC(df7_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# positive outcome expectations

df8 <- data1 %>%
  filter(ema_psych_context_predictor_coding == "positive outcome expectations")

df8.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df8,
                    random = ~ 1 | author/es_id, 
                    test = "z", 
                    method = "REML")

summary(df8.model)

df8_i2 <- var.comp(df8.model)

summary(df8_i2)

png(here("outputs", "df8.model.forest.png"), width = 600, height = 600)

forest.rma(df8.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-5.2, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                                .(formatC(df8_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()
