knitr::opts_chunk$set(echo = FALSE, tidy=TRUE, warning=FALSE, message=FALSE)
library(igraph)
library(haven)
library(tidyverse)
library(lubridate)
library(pander)
library(printr)
library(forcats)
# read in the edges
edges <- haven::read_sas("tommy_edges_2009.sas7bdat") 
# read in the node traits: nodes
nodes <- haven::read_sas("tommy_nodes_2009.sas7bdat")
# read in patient characteristics data: patients
patients <- haven::read_sas("tommyflynn_patient_info_2009.sas7bdat")
complaints <- levels(factor(patients$Chief_Complaint))

# There is an issue with dates; each dataset has a different set of dates; figure out what dates to use with:
PTS_d8s <- unique(patients$d8) 
Nodes_d8s <-  unique(nodes$d8)
Edges_D8s <-  unique(edges$D8)
alldates <- c(PTS_d8s, Nodes_d8s , Edges_D8s)
# length(alldates) answer= 131
alldates <- unique(alldates) # There are 49 total unique dates across all three tbls, but only 35 in edges!

# Now clean the patients data to include: acuity recorded, race black or white, date in edges, chief complaint, MinutesInED, 
patients35 <- patients %>% filter(Acuity != "Not Recorded", (Race == "Black" | Race == "White" ), 
                                  !is.na(Chief_Complaint), !is.na(Acuity), d8 %in% Edges_D8s) %>%
  select(sid, d8, Acuity, Race, AGE, Sex, Chief_Complaint, MinutesInED, ED_ARRIVAL, ed_departure, Arr_Mode) %>%
  group_by(d8)

# Group EDGES by DATE, select only the needed variables (idi, idj, participant type i-j, date, am, pm, weight)
edges35 <- edges %>% filter(!is.na(i_participant_type), !is.na(j_participant_type), 
                            (i_participant_type  != "_PAT" | j_participant_type != "_PAT" )) %>%
  mutate(i = i_participant_type, j = j_participant_type, am = (shiftampm == 1), pm = (shiftampm == 2)) %>%
  group_by(D8) %>% select(sidi, sidj, i, j, edgeweight, am, pm, D8, idi, idj)

patients35 %>% ungroup(d8) %>% count(Race)

# Plot race in a bar chart
ggplot(patients35, aes(Race, d8, color = Race)) +
  geom_point(position = "jitter", shape = 1, size = 2) + scale_x_discrete(drop = FALSE) 
# (drop = FALSE) = NA values are shown in plot

# Chief complaint plotted by age (chief complaint is arranged in ascending order by age)
ggplot(patients, aes(AGE, fct_reorder(Chief_Complaint, AGE), color = Acuity)) +
  geom_point(position = "jitter", alpha = 0.6, shape = 1, size = 4) +
  scale_x_continuous("Patient Age (yrs)", limits = c(1, 100)) + 
  scale_y_discrete(name = "Chief Complaint")


ggplot(patients35, aes(x = Chief_Complaint, color = Race)) + geom_bar()
names(nodes)
n_nodes <- length(unique(nodes$sid))
n_pts <- length(unique(patients$sid))
edges_n <- length(unique(edges$sidi))
length(edges$sidi)
length(unique(edges$sidj))
cc_count <- patients %>% count(Chief_Complaint)
patients %>% mutate(CC_group = fct_collapse(Chief_Complaint,
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
                                            
)) -> patients_cc
patients_cc %>% count(CC_group, sort = TRUE) -> cc_temp
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   