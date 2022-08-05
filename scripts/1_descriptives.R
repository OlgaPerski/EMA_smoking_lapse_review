source(here::here("scripts", "0_load_packages_data.R"))

# definition of lapse -----------------------------------------------------

lapse_coding %>%
  filter(!is.na(lapse_coding_final),
         id != "377a" & id!="377b") %>%
  count(lapse_coding_final) %>%
  arrange(-n) %>%
  mutate(prop = n/45*100)

# definition of relapse ---------------------------------------------------

relapse_coding %>%
  filter(!is.na(relapse_coding_final),
         id != "377a" & id!="377b") %>%
  count(relapse_coding_final) %>%
  arrange(-n) %>%
  mutate(prop = n/33*100)

# theoretical underpinning ------------------------------------------------

theory_coding %>%
  filter(!is.na(theory_coding_final),
         id != "377a" & id!="377b") %>%
  count(theory_coding_final) %>%
  arrange(-n) %>%
  mutate(prop = n/31*100)

# join with larger EMA review data ----------------------------------------

data_large_review <- read_rds(here::here("data", "large_review_clean.rds")) %>%
  mutate(id = case_when(str_detect(id, "[abcdef]") ~ as.character(id),
                        TRUE ~ as.character(as.integer(id))))

data_joined <- data_large_review %>%
  filter(id %in% data_descriptives$id)

df_joined <- subset(data_joined, id!="377a" & id!="377b" & author!="Scholz")

# create table 1 ----------------------------------------------------------

table_1_prep <- df_joined %>%
  mutate(unite(across(any_of(starts_with("funder_")), ~case_when(.x == "research" ~ "Yes")), col = "research_funded", na.rm = T),
         research_funded = case_when(str_detect(research_funded, "Yes") ~ "Yes",
                                     TRUE ~ "No"),
         unite(across(any_of(starts_with("funder_")), ~case_when(.x == "society" ~ "Yes")), col = "society_funded", na.rm = T),
         society_funded = case_when(str_detect(society_funded, "Yes") ~ "Yes",
                                    TRUE ~ "No"),
         unite(across(any_of(starts_with("funder_")), ~case_when(.x == "charity" ~ "Yes")), col = "charity_funded", na.rm = T),
         charity_funded = case_when(str_detect(charity_funded, "Yes") ~ "Yes",
                                    TRUE ~ "No"),
         unite(across(any_of(starts_with("funder_")), ~case_when(.x == "university_health_institution" ~ "Yes")), col = "university_health_institution_funded", na.rm = T),
         university_health_institution_funded = case_when(str_detect(university_health_institution_funded, "Yes") ~ "Yes",
                                                          TRUE ~ "No"),
         unite(across(any_of(starts_with("funder_")), ~case_when(.x == "industry" ~ "Yes")), col = "industry_funded", na.rm = T),
         industry_funded = case_when(str_detect(industry_funded, "Yes") ~ "Yes",
                                     TRUE ~ "No"),
         unite(across(any_of(starts_with("funder_")), ~case_when(.x == "none" ~ "Yes")), col = "unfunded", na.rm = T),
         unfunded = case_when(str_detect(unfunded, "Yes") ~ "Yes",
                              TRUE ~ "No"),
         across(where(is.factor), ~fct_infreq(.)))

table_1_prep_recode <- table_1_prep %>%
  mutate(incentive_schedule_recode = recode(incentive_schedule, "NR" = "Not reported"),
         EMA_intervention_level_recode = recode(EMA_intervention_level, "NA" = "Not applicable"),
         country_recode = recode(country, "NR" = "Not reported"),
         sample_size_analytic_sample = as.numeric(sample_size_analytic_sample))

mylabels <- list(country_recode = "Country", research_funded = "Research funding", society_funded = "Society funding", 
                 charity_funded = "Charity funding", university_health_institution_funded = "University/Health institution funding",
                 industry_funded = "Industry funding", unfunded = "No funding",
                 EMA_study_type = "Study design", EMA_intervention_level_recode = "Intervention level", 
                 behaviour1 = "Target behaviour", population_type = "Population Type", 
                 sample_size_analytic_sample = "Sample size", mean_age = "Age", 
                 female_sex_percentage = "% Female", ethnicity_white_percentage = "% White ethnicity", 
                 education_university_percentage = "% University education",
                 incentive_schedule_recode = "Incentive schedule")

mycontrols <- tableby.control(numeric.stats = c("Nmiss", "median", "q1q3"),
                              stats.labels = list(N="Not reported", median = "Median", q1q3 = "IQR"))

t1_total <- tableby(~ country_recode + research_funded + society_funded + charity_funded + university_health_institution_funded + industry_funded + unfunded +
                      EMA_study_type + EMA_intervention_level_recode + behaviour1 + population_type + sample_size_analytic_sample +
                      mean_age + female_sex_percentage + ethnicity_white_percentage + 
                      education_university_percentage + incentive_schedule_recode, data = table_1_prep_recode, control = mycontrols, cat.simplify = F)

t1_total <- summary(t1_total, text = T, labelTranslations = mylabels, digits = 1)

write2word(t1_total,here("outputs","table1_total.doc"))

### add cigarettes per day, number of quit attempts, smoking cessation support

# cpd

data_descriptives %>% 
  filter(id != "377a" & id!="377b") %>%
  count()

data_descriptives %>% 
  filter(!is.na(mean_cpd),
         id != "377a" & id!="377b") %>%
  count()

a <- data_descriptives %>% 
  filter(!is.na(mean_cpd),
         id != "377a" & id!="377b")

median(a$mean_cpd)
q1q3(a$mean_cpd)

# quit attempts

data_descriptives %>% 
  filter(!is.na(mean_nr_quit_attempts),
         id != "377a" & id!="377b") %>%
  count()

b <- data_descriptives %>% 
  filter(!is.na(mean_nr_quit_attempts),
         id != "377a" & id!="377b")

median(b$mean_nr_quit_attempts)
q1q3(b$mean_nr_quit_attempts)

# smoking cessation support

c <- data_descriptives %>% 
  filter(id != "377a" & id!="377b") %>%
  mutate(no_supp = case_when(is.na(cessation_support_beh) & is.na(cessation_support_pharm) ~ 1,
                                   TRUE ~ 0),
         both_supp = case_when(cessation_support_beh == "yes" & cessation_support_pharm == "yes" ~ 1,
                               TRUE ~ 0),
         beh_supp_only = case_when(cessation_support_beh == "yes" & is.na(cessation_support_pharm) ~ 1,
                                   TRUE ~ 0),
         pharm_supp_only = case_when(cessation_support_pharm == "yes" & is.na(cessation_support_beh) ~ 1,
                                     TRUE ~ 0))
table(c$beh_supp_only)
table(c$pharm_supp_only)
table(c$both_supp)
table(c$no_supp)

# create table 2 ----------------------------------------------------------

df_joined <- df_joined %>%
  mutate(adherence = coalesce(percentage_adherence_EMA_general, percentage_adherence_EMA_beh, percentage_adherence_EMA_psych))

table_2_prep_recode <- df_joined %>%
  mutate(burst_design_recode = recode(burst_design, "yes" = "Yes"),
         own_device_EMA_recode = recode(own_device_EMA, "NR" = "Not reported"),
         EMA_delivery_mode_recode = recode(EMA_delivery_mode, "Smartphone app" = "Mobile phone - app"),
         adherence_cutoff_recode = recode(adherence_cutoff, "no" = "No",
                                          "yes" = "Yes",
                                          "NR" = "Not reported"),
         frequency_recode = recode(S_psych_1_frequency, "daily on the EMA days" = "Daily",
                                   "Weekly/Thursday, Friday and Saturday" = "Weekly"))

mylabels <- list(study_duration_days = "Study duration (days)", burst_design_recode = "Burst design", nr_bursts = "Number of bursts", 
                 own_device_EMA_recode = "% Own device",
                 EMA_delivery_mode_recode = "% EMA delivery mode",
                 adherence_cutoff_recode = "Adherence cut-off",
                 frequency_recode = "% EMA sampling frequency",
                 S_EMA_method = "% EMA sampling method",
                 adherence = "% Adherence")

t2_total <- tableby(~ study_duration_days + burst_design_recode + nr_bursts + own_device_EMA_recode + EMA_delivery_mode_recode + adherence + adherence_cutoff_recode + 
                      frequency_recode + S_EMA_method, data = table_2_prep_recode, control = mycontrols, cat.simplify = F)

t2_total <- summary(t2_total, text = T, labelTranslations = mylabels, digits = 1)

write2word(t2_total,here("outputs","table2_total.doc"))

#summarise % multiple items and % precedent (denominator = all studies) 

multiple_items <- df_joined %>%
  select(id, any_of(contains(c("S_psych")))) %>%
  select(id, any_of(contains(c("measure")))) %>%
  pivot_longer(cols = !id) %>%
  filter(!str_detect(name, "measurement_type")) %>%
  drop_na() %>%
  count(value)

precedent <- df_joined %>%
  select(id, any_of(contains(c("S_psych")))) %>%
  select(id, any_of(contains(c("validated")))) %>%
  pivot_longer(cols = !id) %>%
  drop_na() %>%
  count(value)

#summarise % multiple items and % precedent (denominator = total psych/contextual vars per study)

all_measurement <- df_joined %>%
  select(id, any_of(contains(c("S_psych")))) %>%
  select(id, any_of(contains(c("measure")))) %>%
  pivot_longer(cols = !id) %>%
  filter(!str_detect(name, "measurement_type")) %>%
  drop_na() %>% 
  group_by(id) %>%
  mutate(n_in_study = n(),
         prop_multiple = sum(value == "Multiple items", na.rm = T)/n_in_study) %>%
  distinct(id, prop_multiple)

median(all_measurement$prop_multiple)
quantile(all_measurement$prop_multiple)
hist(all_measurement$prop_multiple)
table(all_measurement$prop_multiple)

all_validated <- df_joined %>%
  select(id, any_of(contains(c("S_psych")))) %>%
  select(id, any_of(contains(c("validated")))) %>%
  pivot_longer(cols = !id) %>%
  drop_na() %>% 
  group_by(id) %>%
  mutate(n_in_study = n(),
         prop_precedent = sum(value == "Precedent", na.rm = T)/n_in_study) %>%
  distinct(id, prop_precedent)

median(all_validated$prop_precedent)
quantile(all_validated$prop_precedent)
hist(all_validated$prop_precedent)
table(all_validated$prop_precedent)

#psych construct frequency

S_constructs <- c("S_psych_1_1", "S_psych_1_2", "S_psych_1_3", "S_psych_1_4",
                  "S_psych_1_5", "S_psych_1_6", "S_psych_1_7", "S_psych_1_8", 
                  "S_psych_1_9", "S_psych_1_10", "S_psych_2_1", "S_psych_2_2",
                  "S_psych_2_3", "S_psych_2_4","S_psych_2_5", "S_psych_3_1", 
                  "S_psych_3_2", "S_psych_3_3", "S_psych_4_1", "S_psych_4_2",
                  "S_psych_4_3", "S_psych_5_1", "S_psych_5_2", "S_psych_5_3")

S_plot <- pivot_longer(df_joined %>%
                         select(id, behaviour1, all_of(S_constructs)), cols = all_of(S_constructs)) %>%
  drop_na(value) %>%
  mutate(construct = "Smoking",
         value = str_to_sentence(value))

summary(S_plot)

S_plot %>%
  janitor::tabyl(value)

#summarise median (q1, q3) number of psych/contextual constructs per study

nr_constructs <- S_plot %>%
  group_by(id) %>%
  summarise(n = n())

range(nr_constructs$n)
median(nr_constructs$n)
quantile(nr_constructs$n)

#produce figure 4

colour_palette <- RColorBrewer::brewer.pal(12, "Paired")

fig_S_plot <- S_plot %>%
  ggplot() +
  geom_bar(aes(x = fct_rev(fct_infreq(value)), fill = value)) +
  scale_fill_manual(values = colour_palette) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none", axis.text = element_text(size = 20)) +
  labs(x = NULL,
       y = NULL)

if(!file.exists(here("outputs", "fig_S_plot.png"))) ggsave(fig_S_plot, filename = here("outputs", "fig_S_plot.png"), dpi = 320, height = 12, width = 16)

# quality appraisal -------------------------------------------------------

table_3_prep_recode <- df_joined %>%
  mutate(quality3_recode = recode(quality3, "NA" = "Not reported",
                                  "NR" = "Not reported"),
         quality4_recode = recode(quality4, "n.a." = "Weak",
                                  "NA" = "Weak"))

mylabels <- list(quality1 = "Quality 1", quality2 = "Quality 2", quality3_recode = "Quality 3", quality4_recode = "Quality 4")

t3_total <- tableby(~ quality1 + quality2 + quality3_recode + quality4_recode, data = table_3_prep_recode, control = mycontrols, cat.simplify = F)

t3_total <- summary(t3_total, text = T, labelTranslations = mylabels, digits = 1)

write2word(t3_total,here("outputs","table3_total.doc"))
