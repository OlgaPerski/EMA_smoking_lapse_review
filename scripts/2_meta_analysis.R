source(here::here("scripts", "0_load_packages_data.R"))

# prepare data for meta-analysis ------------------------------------------

# join with larger EMA review data to pull in quality indicators, study design, etc

df_joined <- read_rds(here::here("data", "updated_review_clean.rds")) %>%
  mutate(id = case_when(str_detect(id, "[abcdef]") ~ as.character(id),
                        TRUE ~ as.character(as.integer(id))),
         across(any_of(c("mean_age", "female_sex_percentage", "ethnicity_white_percentage", "study_duration_days")), as.numeric)) %>%
  filter(author != "Scholz")

vars <- c("id", "mean_age", "female_sex_percentage", "ethnicity_white_percentage",
          "EMA_study_type", "study_duration_days", "incentive_schedule",
          "quality1", "quality2", "quality3", "quality4")

df_joined_red <- df_joined %>%
  select(any_of(vars))

# join with descriptives data frame

df_all <- full_join(df_joined_red, data_meta_analysis, by = "id") %>%
  left_join(data_descriptives %>%
              mutate(id = as.character(id)) %>%
              select(id, mean_cpd, cessation_support_beh, cessation_support_pharm), by = "id")

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
         ethnicity_white_percentage = case_when(is.na(ethnicity_white_percentage) ~ 81.7,
                                                TRUE ~ ethnicity_white_percentage),
         mean_cpd = case_when(is.na(mean_cpd) ~ 21.4,
                              TRUE ~ mean_cpd))

# retain vars with sufficient variability for moderator analyses

vars1 <- c("author", "year", "study_nr",
           "mean_age", "female_sex_percentage", 
           "ethnicity_white_percentage",
           "mean_cpd",
           "cessation_support",
           "EMA_study_type",
           "study_duration_days",
           "incentive_schedule", 
           "random_intercept_within",
           "random_slope_within",
           "quality1", "quality2",
           "quality3", "quality4",
           "ema_psych_context_predictor", 
           "ema_psych_context_predictor_coding",
           "es_id", "logOR_within",
           "logOR_SE_within",
           "time_lag_nr")

data1 <- data %>%
  select(any_of(vars1)) %>%
  filter(!is.na(logOR_within)) %>%
  mutate(author = as.factor(author),
         study_nr = as.factor(study_nr),
         ema_psych_context_predictor_coding = as.factor(ema_psych_context_predictor_coding),
         random_slope_within = as.factor(random_slope_within),
         study_nr = as.numeric(study_nr),
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

# if k > 10, run meta-analysis

data1 %>%
  count(ema_psych_context_predictor_coding) %>%
  arrange(-n) # run for 1) negative feeling states, 2) environmental and social cues, 3) cravings

# create study ID by author var

data2 <- data1 %>%
  mutate(author_study_nr = paste0(author, sep = " ", study_nr))

# negative feeling states -------------------------------------------------

df1 <- data2 %>%
  filter(ema_psych_context_predictor_coding == "negative feeling states")

# fitting simpler two-level (rather than three-level) random-effects model as <1% between-study heterogeneity observed

df1.model <- rma(yi = logOR_within, 
                     vi = logOR_SE_within, 
                     slab = author,
                     data = df1,
                     test = "z", 
                     method = "REML")

summary(df1.model)

df1.model$I2

png(here("outputs", "df1.model.forest.png"), width = 600, height = 600)

forest.rma(df1.model, transf = exp, ilab.xpos = c(-0.25), cex=1,
           refline = 1,
           order = "obs",
           header = "Author.Effect Size ID",
           xlab = "Odds Ratio",
           mlab="")
text(-11.7, - 1, pos = 4, font = 2, bquote(paste("Random Effects Model, I²", " = ",
                                              .(formatC(df1.model$I2, digits = 1, format = "f")), "%")))

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
                         test = "z", 
                         method = "REML",
                         mods = ~ logOR_SE_within)

summary(df1.model.egg)

# sensitivity analysis with robust variance estimation, varying rho (0.2, 0.4, 0.6, 0.8)

yi = df1$logOR_within
vi = df1$logOR_SE_within

df1.model.rve.02 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.2,
                      small = TRUE,
                      data = df1)

df1.model.rve.04 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.4,
                      small = TRUE,
                      data = df1)

df1.model.rve.06 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.6,
                      small = TRUE,
                      data = df1)

df1.model.rve.08 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.8,
                      small = TRUE,
                      data = df1)

df1.rve.table <- tibble(df1.model.rve.02[["reg_table"]]) %>%
  bind_rows(df1.model.rve.04[["reg_table"]]) %>%
  bind_rows(df1.model.rve.06[["reg_table"]]) %>%
  bind_rows(df1.model.rve.08[["reg_table"]])

# environmental and social cues -------------------------------------------------

df2 <- data2 %>%
  filter(ema_psych_context_predictor_coding == "environmental and social cues")

# fit model

df2.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df2,
                    random = ~ 1 | author_study_nr/es_id, 
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
                                                 .(formatC(df2_i2[["totalI2"]] , digits = 1, format = "f")), "%")))

dev.off()

# leave-one-out sensitivity analysis (now removing all O'Connell studies)

df2_sens <- data2 %>%
  filter(ema_psych_context_predictor_coding == "environmental and social cues",
         es_id != "id_42",
         es_id != "id_43",
         es_id != "id_45",
         es_id != "id_46",
         es_id != "id_51")

df2.model.sens <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df2_sens,
                    random = ~ 1 | author_study_nr/es_id, 
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
                        random = ~ 1 | author_study_nr/es_id, 
                        test = "z", 
                        method = "REML",
                        mods = ~ logOR_SE_within)

summary(df2.model.egg)

# prep for moderator analysis

# check cessation support var

df2 %>%
  janitor::tabyl(cessation_support) # remove due to low variability

# check time lag

df2 <- df2 %>%
  mutate(time_lag_nr = as.factor(time_lag_nr)) 

df2 %>%
  janitor::tabyl(time_lag_nr)

# check quality 3

df2 %>%
  janitor::tabyl(quality3) # remove due to low variability

# run moderator analysis

df2.model.mods <- rma.mv(yi = logOR_within, 
                         V = logOR_SE_within, 
                         slab = author,
                         data = df2,
                         random = ~ 1 | author_study_nr/es_id, 
                         test = "z", 
                         method = "REML",
                         mods = ~ mean_age + female_sex_percentage + ethnicity_white_percentage +
                           mean_cpd + EMA_study_type + study_duration_days + 
                           incentive_schedule + random_intercept_within + random_slope_within + time_lag_nr)

summary(df2.model.mods)

df2_mod_i2 <- var.comp(df2.model.mods)

summary(df2_mod_i2)

# generate table for supplementary materials

mod_table_1 <- coef(summary(df2.model.mods))

mod_table_1$moderators <- row.names(mod_table_1)

row.names(mod_table_1) <- NULL

mod_table_1_a <- mod_table_1[,c(7,1,4,5,6)]

mod_table_1_b <- mod_table_1_a %>%
  mutate(moderators = str_replace_all(moderators, "_", " "),
         moderators = str_replace(moderators, "intrcpt", "intercept"),
         moderators = str_replace(moderators, "random slope withinYes", "random slope within"),
         moderators = str_to_sentence(moderators),
         moderators = str_replace(moderators, "cpd", "CPD"),
         moderators = str_replace(moderators, "Ema study typeinterventional", "EMA study type - Interventional"),
         OR = paste0(round(exp(estimate),2), " (", round(exp(ci.lb),2), "-", round(exp(ci.ub),2), ")")) %>%
  select(moderators, OR, pval)

flextable(mod_table_1_b) %>% save_as_docx(path = here("outputs", "mod_table_1_b.docx"))

# sensitivity analysis with robust variance estimation, varying rho (0.2, 0.4, 0.6, 0.8)

yi = df2$logOR_within
vi = df2$logOR_SE_within

df2.model.rve.02 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.2,
                      small = TRUE,
                      data = df2)

df2.model.rve.04 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.4,
                      small = TRUE,
                      data = df2)

df2.model.rve.06 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.6,
                      small = TRUE,
                      data = df2)

df2.model.rve.08 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.8,
                      small = TRUE,
                      data = df2)

df2.rve.table <- tibble(df2.model.rve.02[["reg_table"]]) %>%
  bind_rows(df2.model.rve.04[["reg_table"]]) %>%
  bind_rows(df2.model.rve.06[["reg_table"]]) %>%
  bind_rows(df2.model.rve.08[["reg_table"]])

# cravings ----------------------------------------------------------------

df3 <- data2 %>%
  filter(ema_psych_context_predictor_coding == "cravings")

# fit model

df3.model <- rma.mv(yi = logOR_within, 
                    V = logOR_SE_within, 
                    slab = author,
                    data = df3,
                    random = ~ 1 | author_study_nr/es_id,
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
                                                .(formatC(df3_i2[["totalI2"]], digits = 1, format = "f")), "%")))

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
                        random = ~ 1 | author_study_nr/es_id, 
                        test = "z", 
                        method = "REML",
                        mods = ~ logOR_SE_within)

summary(df3.model.egg)

# prep for moderator analysis

# check cessation support var

df3 %>%
  janitor::tabyl(cessation_support) # remove due to low variability

# check time lag

df3 <- df3 %>%
  mutate(time_lag_nr = as.factor(time_lag_nr))

df3 %>%
  janitor::tabyl(time_lag_nr)

# check quality 3

df3 %>%
  janitor::tabyl(quality3)

# run moderator analyses

df3.model.mods <- rma.mv(yi = logOR_within, 
                         V = logOR_SE_within, 
                         slab = author,
                         data = df3,
                         random = ~ 1 | author_study_nr/es_id, 
                         test = "z", 
                         method = "REML",
                         mods = ~ mean_age + female_sex_percentage + ethnicity_white_percentage +
                           mean_cpd + EMA_study_type + study_duration_days + 
                           incentive_schedule)

summary(df3.model.mods)

df3_mod_i2 <- var.comp(df3.model.mods)

summary(df3_mod_i2)

# generate table for supplementary materials

mod_table_2 <- coef(summary(df3.model.mods))

mod_table_2$moderators <- row.names(mod_table_2)

row.names(mod_table_2) <- NULL

mod_table_2_a <- mod_table_2[,c(7,1,4,5,6)]

mod_table_2_b <- mod_table_2_a %>%
  mutate(moderators = str_replace_all(moderators, "_", " "),
         moderators = str_replace(moderators, "intrcpt", "intercept"),
         moderators = str_to_sentence(moderators),
         moderators = str_replace(moderators, "cpd", "CPD"),
         moderators = str_replace(moderators, "Ema study typeinterventional", "EMA study type - Interventional"),
         moderators = str_replace(moderators, "Incentive scheduleflat payment", "Incentive schedule - Flat payment"),
         OR = paste0(round(exp(estimate),2), " (", round(exp(ci.lb),2), "-", round(exp(ci.ub),2), ")")) %>%
  select(moderators, OR, pval)

flextable(mod_table_2_b) %>% save_as_docx(path = here("outputs", "mod_table_2_b.docx"))

# sensitivity analysis with robust variance estimation, varying rho (0.2, 0.4, 0.6, 0.8)

yi = df3$logOR_within
vi = df3$logOR_SE_within

df3.model.rve.02 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.2,
                      small = TRUE,
                      data = df3)

df3.model.rve.04 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.4,
                      small = TRUE,
                      data = df3)

df3.model.rve.06 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.6,
                      small = TRUE,
                      data = df3)

df3.model.rve.08 <- robu(formula = yi ~ 1, 
                      var.eff.size = vi,
                      studynum = study_nr,
                      modelweights = "CORR",
                      rho = 0.8,
                      small = TRUE,
                      data = df3)

df3.rve.table <- tibble(df3.model.rve.02[["reg_table"]]) %>%
  bind_rows(df3.model.rve.04[["reg_table"]]) %>%
  bind_rows(df3.model.rve.06[["reg_table"]]) %>%
  bind_rows(df3.model.rve.08[["reg_table"]])

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
