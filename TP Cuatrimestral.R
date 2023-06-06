library(tidyverse)
library(dplyr)
mimic_con <- dbConnect(RSQLite::SQLite(), "C:/Users/bauti/Austral/2023_01/AyED/Informe R/mimic3_demo.db")
dbListTables(mimic_con)
####################################################
# ¿Cuáles son los diagnósticos más comunes entre los pacientes que fallecieron en la UCI?
deaths <- tbl(mimic_con, "PATIENTS") %>% filter(!is.na(dod))
diagnoses <- tbl(mimic_con, "DIAGNOSES_ICD") %>% inner_join(tbl(mimic_con, "D_ICD_DIAGNOSES"), by = "icd9_code") %>% 
  select(subject_id, icd9_code, short_title)
deaths_with_diagnoses <- deaths %>% inner_join(diagnoses, by = "subject_id") %>% select(icd9_code, short_title)
# Contar los diagnósticos más comunes
top_diagnoses <- deaths_with_diagnoses %>% group_by(icd9_code, short_title) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(10)
top_diagnoses
####################################################
#¿Cuál es el promedio de readmisiónes por pacientes en la UCI y cuáles son las causas más comunes de estas readmisiones?
icu_stays <- tbl(mimic_con, "ICUSTAYS")
diagnoses_icd <- tbl(mimic_con, "DIAGNOSES_ICD")
d_icd_diagnoses <- tbl(mimic_con, "D_ICD_DIAGNOSES")
icd9_shortnames <- d_icd_diagnoses %>% inner_join(diagnoses_icd, by = "icd9_code") %>% select(subject_id, hadm_id, icd9_code, short_title, seq_num)
n_admissions <- icu_stays %>% group_by(subject_id) %>% 
  summarize(n_admissions = n_distinct(icustay_id))
# calcular el promedio de readmisiones por paciente
readmission_rate <- n_admissions %>% summarize(readmission_rate = mean(n_admissions, na.rm = TRUE))
readmission_rate

# Obtener las causas más comunes de readmisión con el nombre corto del diagnóstico
readmissions <- icu_stays %>% group_by(subject_id) %>% 
  summarize(n_admissions = n_distinct(icustay_id)) %>% 
  filter(n_admissions > 0) %>% 
  left_join(icd9_shortnames, by = "subject_id") %>% 
  filter(seq_num == 1) %>% select(icd9_code, short_title) %>%
  count(icd9_code, short_title, sort = TRUE)
readmissions
####################################################
top_diagnoses %>% collect() %>% ggplot(aes(x = reorder(short_title, -count), y =count)) +
    geom_bar(aes(fill=short_title), stat = "identity") +
    labs(title = "TOP 10 Diseases in dead pacients", x = "Disease title", y = "Number of appearances") +
    scale_x_discrete(labels=c("Hypertension\nNOS","Atrial\nfibrillation","Acute\nkidney\nfailure NOS","Congestive heart\nfailure,
                              unspecified","Diabetes type II\nunspecified","Acute respiratry\nfailure",
                            "Hyperlipidemia\nNEC/NOS","Urin tract\ninfection NOS","Pneumonia, organism\nNOS","Anemia\nNOS ")) +
    theme(legend.position = "none") + geom_text(aes(label = count), position = position_stack(vjust = .5))
####################################################
readmissions %>% 
  collect() %>% 
  mutate(n = ifelse(n >= 4 & n <= 15, as.character(n), "<= 3")) %>%
  mutate(short_title = ifelse(n == "<= 3", "Other Diseases", short_title)) %>%
  ggplot(aes(x = reorder(short_title, -as.numeric(n)), y = as.numeric(n))) +
  geom_bar(aes(fill = short_title), stat = "identity") +
  geom_text(aes(label = ifelse(n == "<= 3", "<= 3", n)), position = position_stack(vjust = 0.5)) +
  geom_bar(data = data.frame(short_title = "Other Diseases", y = 3), 
           fill = "gray", stat = "identity", width = 0.9, aes(x = short_title, y = y)) +
  geom_text(data = data.frame(short_title = "Other Diseases", y = 1.5), 
            aes(x = short_title, y = y, label = "<= 3"), vjust = -0.5) +
  scale_x_discrete(labels=c("Septicemia\nNOS","Pneumonia,\norganism NOS","Acute respiratry\nfailure","Subendo infarct,\ninitial",
                            "Congestive heart\nfailure NOS", "Other Diseases")) +
  labs(title = "Number of readmission caused by a disease in dead pacients", x = "Disease title", y = "Number of readmissions") +
  theme(legend.position = "none")
####################################################
# Cerrar la conexión con la base de datos
dbDisconnect(mimic_con)
