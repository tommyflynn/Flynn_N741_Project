
library(igraph)
library(haven)
library(tidyverse)
library(lubridate)
library(pander)
library(printr)
library(forcats)
library(modelr)
options(na.action = na.warn)

# read in the edges
edges <- haven::read_sas("tommy_edges_2009.sas7bdat")
# read in the node traits: nodes
nodes <- haven::read_sas("tommy_nodes_2009.sas7bdat")
# read in patient characteristics data: patients
patients <- haven::read_sas("tommyflynn_patient_info_2009.sas7bdat")

# There is an issue with dates; each dataset has a different set of dates; figure out what dates to use with:
PTS_d8s <- unique(patients$d8) 
Nodes_d8s <-  unique(nodes$d8)
Edges_D8s <-  unique(edges$D8)
alldates <- c(PTS_d8s, Nodes_d8s , Edges_D8s)
# length(alldates) answer= 131
alldates <- unique(alldates) # There are 49 total unique dates across all three tbls, but only 35 in edges!

# n_nodes <- length(unique(nodes$sid))
# n_pts <- length(unique(patients$sid))
# edges_n <- length(unique(edges$sidi))
# length(edges$sidi)
# length(unique(edges$sidj))
# cc_count <- patients %>% count(Chief_Complaint)
patients_cc <- patients %>% mutate(CC_group = fct_collapse(Chief_Complaint,
                                                           Pain_Abd = c("Pain, Abd General", "Abdominal pain",
                                                                        "Abdominal pain - pregnant", "Abdominal problem",
                                                                        "Pain, Abd LLQ", "Nausea/Vomiting with Abd Pain",
                                                                        "Flank Pain", "Pain, Abd RLQ", "Pain, Epigastric", "Pain, Flank",
                                                                        "Preg w/ Abd Pain", "Pain, Abd RUQ", "Flank pain",
                                                                        "Pain, Abd LUQ"),
                                                           Pain_Chest = c("Chest Pain", "Chest pain", "Pain, Chest", "Chest Pressure",
                                                                          "Chest Wall Pain", "Heartburn or indigestion",
                                                                          "Pain, Chest/Ribs", "Pain, Rib(s)"),
                                                           Resp_ARDS = c("Dyspnea", "Difficulty Breathing", "Asthma", "Respiratory problem",
                                                                         "Wheezing", "Exposure, Chemical, Inhalation"),
                                                           Pain_MSK = c("Back Pain", "Pain, Knee", "Pain, Leg", "Pain, Ankle", 
                                                                        "Pain, Arm", "Pain, Back", "Pain, Foot", "Pain, Hip", 
                                                                        "Pain, Other", "Body aches (general)", "Knee pain - swelling", 
                                                                        "Pain, Shoulder", "Pain, Neck", "Back pain - lumbar", 
                                                                        "Leg pain - swelling", "Pain, Hand", "Arm pain - swelling",
                                                                        "Foot pain - swelling", "Pain, Low Back", "Pain, Wrist", 
                                                                        "Shoulder pain - swelling", "Ankle pain - swelling",
                                                                        "Elbow pain - swelling", "Hip pain - swelling", "Neck pain",
                                                                        "Pain, Breast", "Pain, Buttocks", "Pain, Finger", "Pain, Forearm",
                                                                        "Pain, Joints, Multiple", "Pain, Muscular General", "Pain, Toe",
                                                                        "Back pain - thoracic", "Facial pain", "Hand pain - swelling", 
                                                                        "Lower leg pain - swelling", "Pain, Face", "Pain, Heel",
                                                                        "Multiple musculoskeletal pain/swelling", "Pain, Generalized", 
                                                                        "Rib/Trunk pain - swelling", "Wrist pain - swelling", 
                                                                        "Finger(s) pain - swelling", "Preg w/ Back Pain"),
                                                           Neuro_HA = c("Headache", "Headache, Migraine Hx", "Headache - recurrent",
                                                                        "Headache, Cluster Hx", "Headache, New, No Prior Hx", 
                                                                        "Headache, Post Traumatic", "Pain, Head"),
                                                           Resp_Inf = c("Flu Symptoms", "Cough", "Sore Throat", "URI Sxs", 
                                                                        "Cough, Productive", "Throat pain", "Cough, Cold, Congestion",
                                                                        "Cold Symptoms", "Congestion", "Upper respiratory infection",
                                                                        "Cough, Non Productive"),
                                                           Abscess = c("Abscess - complicated", "Abscess - simple", "Abscess - complicated",
                                                                       "Abscess Skin/Soft Tissue", "Infection, Skin/Soft Tissue, Other"),
                                                           Other = c("Multiple Complaints", "AICD discharge", "Medical Problem - Minor",
                                                                     "Abnormal diagnostic test", "Other Complaint", "Sweats",
                                                                     "Medical Problem - Minor", "Abnormal diagnostic test", "Hiccoughs",
                                                                     "Lab or Test Abnormality", "Assault, Sexual", "Choking",
                                                                     "Wound infection (complicated)", "Cyst", "Sexual assault", "Gout"),
                                                           Neuro_AMS = c("Dizzy", "Syncope", "Altered Mental Status", "Dizziness", 
                                                                         "Altered mental status", "Seizure, Epilepsy Hx", "Fatigue",
                                                                         "Seizure, Non Specific", "Neurological problem", "Near Syncope",
                                                                         "Seizure, New Onset", "Difficulty Speaking", 
                                                                         "Confusion", "Fainting", "Seizure", "Seizure - prior history", 
                                                                         "Vertigo", "Vision Loss or Disturbance"),
                                                           GI_General = c("Vomiting", "Bleeding, Rectal", "Nausea/Vomiting/Diarrhea", 
                                                                          "Nausea/Vomiting", "Diarrhea", "Nausea", "GI bleed",
                                                                          "Constipation", "Nausea/Vomiting/ Abd Pain", "Dehydration", 
                                                                          "Rectal pain", "FB, Rectum", "Hemorrhoids", "Hernia, Abd",
                                                                          "Hernia, Other", "Loss of Appetite", "Failure to Thrive",
                                                                          "Pain, Hemorroids", "Preg w/ Epigastric Pain", 
                                                                          "Preg w/ Nausea,Vomiting &lt; 20 wks", "Regurgitation", 
                                                                          "Vomiting - pregnant"),
                                                           GU = c("Bleeding, Vaginal", "Vaginal Discharge", "Urinary Retention", 
                                                                  "Urinary Burning", "Pain, Pelvic", "Pelvic pain &lt; 20 weeks pregnant",
                                                                  "Preg, Vag Bleed, &lt; 20 wks", "Dysuria", "Hematuria", "Pelvic pain",
                                                                  "STD exposure", "Vaginal bleed", "Vaginal discharge/Itching", 
                                                                  "Dysuria/Frequency", "Genitourinary problem", "Preg w/ Pelvic Pain",
                                                                  "Preg w/ painful vaginal bleeding", "STD Exposure", "Vaginal pain",
                                                                  "Vaginal bleed &lt; 20 weeks pregnant", "FB, Vagina", "Groin pain", 
                                                                  "Incontinent, Urine", "Pain, Groin", "Penile Discharge",
                                                                  "Preg w/ Vag D/C", "Preg w/ Vag Pain", "Preg, Poss Spont Abortion",
                                                                  "Preg, Possible", "Testicular pain", "Urinary Frequency",
                                                                  "Urinary Hesitancy", "STD, Possible"),
                                                           Weak_Numb = c("Weakness, Generalized", "Weakness, All Extremities", 
                                                                         "Weakness or fatigue", "Focal motor weakness",
                                                                         "Weakness, One Side Body", "Numbness, Arm(s)", 
                                                                         "Numbness, One Side Body", "Numbness, Face", 
                                                                         "Numbness, Hand(s)/Finger(s)", "Numbness, Other", "Weakness, Face",
                                                                         "Weakness, Lower Ext", "Weakness, Other", "Weakness, Upper Ext"),
                                                           MVC = c("Motor Vehicle Crash", "MVC - Minor", "Auto-Pedest Crash"),
                                                           Fall = c("Fall", "Fall, ground level"),
                                                           Swelling = c("Swelling, Leg", "Swelling, Face", "Edema, Lower Ext",
                                                                        "Swelling, Hand", "Swelling, Knee", "Swelling, Neck", "Edema, Other",
                                                                        "Swelling, Abd", "Swelling, Arm", "Swelling, Eye", "Swelling, Foot",
                                                                        "Swollen Glands Other", "Leg edema", "Swelling, Airway",
                                                                        "Swelling, General", "Swelling, Ankle"),
                                                           Lytes = c("Hyperglycemia", "Hypoglycemia"),
                                                           Heme_HgSS = c("Sickle Cell crisis", "Sickle Cell, Pain", "Sickle Cell, Joint Pain",
                                                                         "Sickle Cell, Chest Pain", "Sickle Cell, Other"),
                                                           Allergic_Rx = c("Allergic Reaction", "Allergic reaction - minor", 
                                                                           "Allergic reaction - major", "Medication Reaction"),
                                                           Follow_Up = c("Surgical Problem Re-evaluation", "Medical Clearance",
                                                                         "Infection, Incision", "Lab draw", "Medication refill", 
                                                                         "Suture Removal", "Surgical problem re-evaluation",
                                                                         "Wound, Dehiscence", "Wound, Re-eval", "Supplies Only",
                                                                         "Medical problem re-evaluation", "Disloc Hip, Prior Replace"),
                                                           Chronic = c( "Catheter check or insertion", "Dialysis shunt problem", 
                                                                        "Dialysis Access Problem", "Cancer", "Pacemaker Problem",
                                                                        "Diabetes Mellitus, IDDM", "Dialysis Prob, Hemodialysis Access", 
                                                                        "Dialysis Prob, Hemodialysis Graft", "GI tube evaluation", 
                                                                        "ICD Related Problem", "Indwelling line problem",
                                                                        "PEG Tube Problem", "PICC Line Problem"),
                                                           Behavioral = c("Suicidal", "Anxiety/Anxious", "Alcohol Intoxication Possible",
                                                                          "Alcohol Related Problem", "Depression", "Psych, Anxiety/Anxious",
                                                                          "Suicidal ideation", "Anxiety", "Detox Requested", 
                                                                          "Suicide Attempt", "Suicidal Ideation", "Withdrawal, Alcohol",
                                                                          "Psychiatric problem", "Psych, Behavior Disorder"),
                                                           Injury = c("Assault, Physical", "Lac, Finger", "Bleeding, Other", "Lac, Leg",
                                                                      "Inj, Finger(s)", "Lac, Hand", "Hip injury - minor", "Inj, Arm",
                                                                      "Inj, Hand(s)", "Inj, Leg", "Inj, Head", "Inj, Wrist", "Lac, Arm",
                                                                      "Lac, Face", "Lac, Forehead", "Lac, Scalp", "Facial injury - minor",
                                                                      "Extremity injury - minor", "Inj, Ankle", "Inj, Hip", "Lac, Chin",
                                                                      "Knee injury - minor", "Lac, Eyelid", "Lac, Foot", "Lac, Lip",
                                                                      "Laceration - facial", "Laceration - finger", "Laceration - scalp",
                                                                      "Laceration - facial", "Laceration - finger", "Laceration - scalp",
                                                                      "Puncture Wound", "Shoulder injury - minor", "Wrist injury - minor",
                                                                      "Hand injury - major", "Arm injury - minor", "Abrasion minor", 
                                                                      "Abrasion, Abd", "Bite, Insect", "Bite, Dog", "Bite, Human", 
                                                                      "Bite, Spider", "Bite, Animal", "Bite, Unknown", "Sting, Insect"),
                                                           HEENT_oral = c("Eye(s), Complaint, Other", "Pain, Ear", "Earache", "Nosebleed", 
                                                                          "ENT problem", "Pain, Eye", "Pain, Dental", "Pain, Oral", "Eye pain",
                                                                          "Swallowing Problem", "Ear Discharge", "Epistaxis", "Ear pain", 
                                                                          "Eye(s), Swollen", "Hemoptysis", "Dental pain", "Difficulty swallowing",
                                                                          "Ear Wax", "Eye foreign body", "Eye(s), Irritation", "FB, Throat", 
                                                                          "Nasal drainage", "Tongue swelling"),
                                                           CV_Sxs = c("Hypertension", "Hypotension", "Tachycardia", "Heartbeat, Palpitations",
                                                                      "Palpitations", "Heartbeat, Irregular", "Bradycardia", 
                                                                      "Heartbeat, Racing"),
                                                           Fevers = c("Fever, Pediatric", "Fever, Adult", "Fever", "Chills",
                                                                      "Fever, Immunocompromised"),
                                                           Skin = c("Rash", "Itching", "Sores, Skin", "Burn, Thermal",
                                                                    "Jaundice", "Burn, Chemical", "Infection, Hand/Finger", 
                                                                    "Infection, Other", "Poison Ivy/Poison Oak", "Rash, Possible Allergy",
                                                                    "Skin lesion bleed", "Skin problem", "Leg ulcers", 
                                                                    "Ulcer, Lower Extremity", "Infection, Rectal/Perirectal" ),
                                                           Severe = c("Medical Problem - Major", "Wound, Gunshot", 
                                                                      "Cardiac and/or respiratory arrest")
                                                           
)) 

# names(nodes)
# Now clean the patients data to include: acuity recorded, race black or white, date in edges, chief complaint, MinutesInED, 
patients35 <- patients_cc %>% filter(Acuity != "Not Recorded", !is.na(Acuity), Race != "2") %>% 
  mutate(Race = as.factor(Race), Acuity = as.factor(Acuity), Sex = as.factor(Sex)) %>%
  select(sid, d8, Acuity, Race, AGE, Sex, Chief_Complaint, CC_group, MinutesInED, ED_ARRIVAL, 
         ed_departure, Arr_Mode, ED_Disposition, numshift)
# head(patients)
# Now tidy Edges, select only the needed variables (sidi, sidj, participant type i-j, date, am, pm, weight); remove
# all edges between two patients
edges35 <- edges %>% filter(!is.na(i_participant_type), !is.na(j_participant_type), !is.na(i), !is.na(j),
                            (i_participant_type  != "_PAT" | j_participant_type != "_PAT" )) %>%
  mutate(i = i_participant_type, j = j_participant_type, am = (shiftampm == 1), 
         pm = (shiftampm == 2)) %>%
  select(sidi, sidj, i, j, edgeweight, am, pm, D8, idi, idj, numshift)

# Alright; lets get to the nodes.. need to have the ID come first...
nodes35 <- nodes %>% filter(Race != "2") %>%
  mutate(ID = as.character(ID), i = as.character(i), AGE = as.integer(AGE), Acuity = as.factor(Acuity), 
         Race = as.factor(Race), Sex = as.factor(Sex)) %>%
  select(sid, i, d8, ID, participant_type, AGE, Race, Sex, Acuity, Arr_Mode, shift_num,
         MinutesInED,  duration_observed, shift_ampm, ED_Disposition, BLACKyn, participationrate)


# now let's see if this works... finally
results <- nodes35 %>% summarise(n = n_distinct(ID), shifts = n_distinct(shift_num), meanparts = n/35,
                                  participation = mean(participationrate), ties = length(edges35), tieshifts = ties/35)
# column headers
headers <- c("Participants (n)", "Shifts", "Participants/Shift", "Participation Rate (mean%)", "Total Ties",
            "Ties/Shift")
# table for paper
knitr::kable(results, align = "c", caption = "Overall Participation", col.names = headers,
             row.names = FALSE)



# patient demographics
# Race stats
racedf <- data_frame("Race" = c("Black", "Hispanic", "Other", "White"), "Count" = summary(patients35$Race))
# racedf
knitr::kable(racedf, align = "l", caption = "Patient Race",
             row.names = FALSE)
# Acuity stats
acuitydf <- data_frame("Acuity Level" = c("Immediate (1)", "Emergent (2)", "Urgent (3)", "Stable (4)", "Non Urgent (5)"),
                       "Count" = summary(patients35$Acuity))
knitr::kable(acuitydf, align = "l", caption = "Patient Acuity",
             row.names = FALSE)

# Gender Stats
gender <- data_frame("Sex" = c("Female", "Male"), "Count" = summary(patients35$Sex))
# gender

# Age stats
mean_age <- patients35 %>% summarise(Age_mean = mean(AGE, na.rm=TRUE), sd = sd(AGE, na.rm=TRUE))
knitr::kable(gender, align = "l", caption = "Patient Gender",
             row.names = FALSE)











# Chief complaint plotted by age (chief complaint is arranged in ascending order by age)
ggplot(patients35, aes(AGE, fct_reorder(CC_group, AGE), color = Acuity)) +
  geom_point(position = "jitter", alpha = 0.6, shape = 1, size = 4) +
  scale_x_continuous("Patient Age (yrs)", limits = c(1, 100)) + 
  scale_y_discrete(name = "Chief Complaint")

# Edges by shift... this could have been easier; but I spent too much time working my way through the data already; no time to learn how to write a function or loop... :(
Eshift1 <- edges35 %>% filter(D8 == Edges_D8s[1])
Eshift2 <- edges35 %>% filter(D8 == Edges_D8s[2])
Eshift3 <- edges35 %>% filter(D8 == Edges_D8s[3])
Eshift4 <- edges35 %>% filter(D8 == Edges_D8s[4])
Eshift5 <- edges35 %>% filter(D8 == Edges_D8s[5])
Eshift6 <- edges35 %>% filter(D8 == Edges_D8s[6])
Eshift7 <- edges35 %>% filter(D8 == Edges_D8s[7])
Eshift8 <- edges35 %>% filter(D8 == Edges_D8s[8])
Eshift9 <- edges35 %>% filter(D8 == Edges_D8s[9])
Eshift10 <- edges35 %>% filter(D8 == Edges_D8s[10])
Eshift11 <- edges35 %>% filter(D8 == Edges_D8s[11])
Eshift12 <- edges35 %>% filter(D8 == Edges_D8s[12])
Eshift13 <- edges35 %>% filter(D8 == Edges_D8s[13])
Eshift14 <- edges35 %>% filter(D8 == Edges_D8s[14])
Eshift15 <- edges35 %>% filter(D8 == Edges_D8s[15])
Eshift16 <- edges35 %>% filter(D8 == Edges_D8s[16])
Eshift17 <- edges35 %>% filter(D8 == Edges_D8s[17])
Eshift18 <- edges35 %>% filter(D8 == Edges_D8s[18])
Eshift19 <- edges35 %>% filter(D8 == Edges_D8s[19])
Eshift20 <- edges35 %>% filter(D8 == Edges_D8s[20])
Eshift21 <- edges35 %>% filter(D8 == Edges_D8s[21])
Eshift22 <- edges35 %>% filter(D8 == Edges_D8s[22])
Eshift23 <- edges35 %>% filter(D8 == Edges_D8s[23])
Eshift24 <- edges35 %>% filter(D8 == Edges_D8s[24])
Eshift25 <- edges35 %>% filter(D8 == Edges_D8s[25])
Eshift26 <- edges35 %>% filter(D8 == Edges_D8s[26])
Eshift27 <- edges35 %>% filter(D8 == Edges_D8s[27])
Eshift28 <- edges35 %>% filter(D8 == Edges_D8s[28])
Eshift29 <- edges35 %>% filter(D8 == Edges_D8s[29])
Eshift30 <- edges35 %>% filter(D8 == Edges_D8s[30])
Eshift31 <- edges35 %>% filter(D8 == Edges_D8s[31])
Eshift32 <- edges35 %>% filter(D8 == Edges_D8s[32])
Eshift33 <- edges35 %>% filter(D8 == Edges_D8s[33])
Eshift34 <- edges35 %>% filter(D8 == Edges_D8s[34])
Eshift35 <- edges35 %>% filter(D8 == Edges_D8s[35])
# Nodes by shift
Vshift1 <- nodes35 %>% filter(d8 == Edges_D8s[1])
Vshift2 <- nodes35 %>% filter(d8 == Edges_D8s[2])
Vshift3 <- nodes35 %>% filter(d8 == Edges_D8s[3])
Vshift4 <- nodes35 %>% filter(d8 == Edges_D8s[4])
Vshift5 <- nodes35 %>% filter(d8 == Edges_D8s[5])
Vshift6 <- nodes35 %>% filter(d8 == Edges_D8s[6])
Vshift7 <- nodes35 %>% filter(d8 == Edges_D8s[7])
Vshift8 <- nodes35 %>% filter(d8 == Edges_D8s[8])
Vshift9 <- nodes35 %>% filter(d8 == Edges_D8s[9])
Vshift10 <- nodes35 %>% filter(d8 == Edges_D8s[10])
Vshift11 <- nodes35 %>% filter(d8 == Edges_D8s[11])
Vshift12 <- nodes35 %>% filter(d8 == Edges_D8s[12])
Vshift13 <- nodes35 %>% filter(d8 == Edges_D8s[13])
Vshift14 <- nodes35 %>% filter(d8 == Edges_D8s[14])
Vshift15 <- nodes35 %>% filter(d8 == Edges_D8s[15])
Vshift16 <- nodes35 %>% filter(d8 == Edges_D8s[16])
Vshift17 <- nodes35 %>% filter(d8 == Edges_D8s[17])
Vshift18 <- nodes35 %>% filter(d8 == Edges_D8s[18])
Vshift19 <- nodes35 %>% filter(d8 == Edges_D8s[19])
Vshift20 <- nodes35 %>% filter(d8 == Edges_D8s[20])
Vshift21 <- nodes35 %>% filter(d8 == Edges_D8s[21])
Vshift22 <- nodes35 %>% filter(d8 == Edges_D8s[22])
Vshift23 <- nodes35 %>% filter(d8 == Edges_D8s[23])
Vshift24 <- nodes35 %>% filter(d8 == Edges_D8s[24])
Vshift25 <- nodes35 %>% filter(d8 == Edges_D8s[25])
Vshift26 <- nodes35 %>% filter(d8 == Edges_D8s[26])
Vshift27 <- nodes35 %>% filter(d8 == Edges_D8s[27])
Vshift28 <- nodes35 %>% filter(d8 == Edges_D8s[28])
Vshift29 <- nodes35 %>% filter(d8 == Edges_D8s[29])
Vshift30 <- nodes35 %>% filter(d8 == Edges_D8s[30])
Vshift31 <- nodes35 %>% filter(d8 == Edges_D8s[31])
Vshift33 <- nodes35 %>% filter(d8 == Edges_D8s[33])
Vshift34 <- nodes35 %>% filter(d8 == Edges_D8s[34])
Vshift35 <- nodes35 %>% filter(d8 == Edges_D8s[35])

# head(Eshift1)
# head(Vshift1)
# similar <- Eshift1$sidi %in% Vshift1$sid
# table(similar)
# similar2 <- Eshift1$idi %in% Vshift1$ID
# table(similar2)

shift10 <- graph_from_data_frame(d = Eshift10, directed = FALSE, vertices = Vshift10)
plot(shift10)


#just curiour about the dates 
logic <- unique(nodes$d8) %in% unique(edges$D8)
table(logic)
table(nodes$d8[logic == FALSE])
table(edges$D8)
# 2009-07-10, 2009-07-17, 2009-07-26, 2009-07-28, 2009-08-21, 2009-09-28, 2009-10-10, 2009-10-15, 2009-10-19, 2009-10-29,
# 2009-12-04, 2009-12-11, 2009-12-17, 2009-12-22
