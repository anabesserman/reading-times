
# This R script deals with reading time data from a self-paced reading (spr) experiment 

# Reading times for each word in the sentence is measured in milliseconds

# Steps:
# 1. cleaning and prepping data
# 2. removing and transforming outlier values
# 3. data viz: generating line graph 
# 4. stats: linear mixed model analyses 

# Experiment Design info:
# DV: reading times
# IVs: (i) verb type (OPT vs. OT), (ii) Antecedent (present vs. absent)
# one additional condition included (Control: Intransitive verb + absent antecedent)
# by-word analyses (e.g. each word position in target sentences must be analyzed individually)

# Loading packages

library(dplyr)
library(plotrix)
library(ggplot2)
library(lme4)
library(lmerTest)

# Reading .csv data frame with results from self-paced reading experiment

spr_raw = read.csv(file.choose( ))
glimpse(spr_raw)
head(spr_raw)

# 1. cleaning and prepping data

# Finding Participants with filler question accuracy lower than 80%

spr_raw %>% 
  filter(WordPosition == "?", ItemType == "filler") %>%
  group_by(Participant) %>%
  summarise(Accuracy = mean(as.numeric(as.character(Answer)))) %>%
  filter(Accuracy < 0.8)

# Excluding participants 28 and 30 for poor accuracy in comprehension questions

`%notin%` <- Negate(`%in%`)  # defining "not in" function: pretty useful!

spr = spr_raw %>% 
  filter(Participant %notin% c(28, 30)) 

unique(spr$Participant)  # checking if right participants were excluded


# Calculating average accuracy for remaining participants

avg_acc = spr %>% 
  filter(WordPosition == "?", ItemType == "filler") %>%
  summarise(Accuracy = mean(as.numeric(as.character(Answer))))

avg_acc  # 94% average accuracy

# Removing practice items, filler items and questions 

spr_targets = spr %>% 
  subset(ItemType != "practice") %>%
  subset(ItemType != "filler") %>%
  subset(WordPosition != "?")

head(spr_targets)
nrow(spr_targets)


# Eliminating extreme reading times: <100ms and >3000ms

extremes = filter(spr_targets, RT <= 100 | RT > 4000)
nrow(extremes)

spr_noExtremes = filter(spr_targets, RT > 100 & RT <= 4000) # creates new variable containing data frame without extreme values
nrow(spr_noExtremes)
nrow(spr_targets)


# Calculating the percentage of extreme values that was removed from full set of data

proportionExtremes = nrow(extremes) * 100 / nrow(spr_targets)
proportionExtremes  # 0.019% of data removed due to extreme values


# 2. removing and transforming outlier values

### Outlier transformation

# This section calculates and transforms outliers for each word position
# Outliers: reading times > 2.5 standard deviations from the mean
# Transformation: change outliers into values == 2.5 SDs from the mean

# Word Positions: det, name, [antec1, antec2, antec3 / intrans] , comma1, [ot_verb / opt_verb / control_verb ],   
# spill1, spill2, spill3, spill4, comma2, conj, verb2, spill_second1, spill_second2, spill_second3, spill_second4
# spillextra


### First, calculate means and standard deviations for each word position
### Then, transform outlier values (> 2.5 SDs from mean) into exactly 2.5 SDs from mean and save in new data frame

spr_MeanAndSDs = spr_noExtremes %>% group_by(Answer) %>% summarise(meanRT = mean(RT), SD = sd(RT), total = n())

spr_MeanAndSDs

########## Word position: det

# starting count of outliers/transformed observations
outliers <- nrow(spr_noExtremes %>% 
                   filter(Answer == "det", RT > Piaget_MeanAndSDs$meanRT[8]+2.5*(Piaget_MeanAndSDs$SD[8])))
outliers


# transforming outlier values for each word position

spr_Transformed <- spr_noExtremes %>% 
  filter(Answer == "det") %>% 
  mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[8]+2.5*(spr_MeanAndSDs$SD[8]), spr_MeanAndSDs$meanRT[8]+2.5*(spr_MeanAndSDs$SD[8]))) 

# sanity check: looks for outlier values, should return 0 rows after outliers have been transformed
spr_Transformed  %>%  filter(Answer == "det", RT > Piaget_MeanAndSDs$meanRT[8]+2.5*(Piaget_MeanAndSDs$SD[8]))

########## Word position: name

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                   filter(Answer == "name", RT > spr_MeanAndSDs$meanRT[10]+2.5*(spr_MeanAndSDs$SD[10]))))
outliers

# from here on now use rbind() to join data to previous data

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% 
                           filter(Answer == "name") %>% 
                           mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[10]+2.5*(spr_MeanAndSDs$SD[10]), spr_MeanAndSDs$meanRT[10]+2.5*(spr_MeanAndSDs$SD[10])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "name", RT > spr_MeanAndSDs$meanRT[10]+2.5*(spr_MeanAndSDs$SD[10]))

########## Word position: antec1

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "antec1", RT > spr_MeanAndSDs$meanRT[1]+2.5*(spr_MeanAndSDs$SD[1]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "antec1") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[1]+2.5*(spr_MeanAndSDs$SD[1]), spr_MeanAndSDs$meanRT[1]+2.5*(spr_MeanAndSDs$SD[1])))) #transforming outlier values

# sanity check
spr_Transformed   %>%  filter(Answer == "antec1", RT > spr_MeanAndSDs$meanRT[1]+2.5*(spr_MeanAndSDs$SD[1]))

########## Word position: antec2

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "antec2", RT > spr_MeanAndSDs$meanRT[2]+2.5*(spr_MeanAndSDs$SD[2]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "antec2") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[2]+2.5*(spr_MeanAndSDs$SD[2]), spr_MeanAndSDs$meanRT[2]+2.5*(spr_MeanAndSDs$SD[2])))) #transforming outlier values

########## sanity check
spr_Transformed  %>%  filter(Answer == "antec2", RT > spr_MeanAndSDs$meanRT[2]+2.5*(spr_MeanAndSDs$SD[2]))

########## Word position: antec3

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "antec3", RT > spr_MeanAndSDs$meanRT[3]+2.5*(spr_MeanAndSDs$SD[3]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "antec3") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[3]+2.5*(spr_MeanAndSDs$SD[3]), spr_MeanAndSDs$meanRT[3]+2.5*(spr_MeanAndSDs$SD[3])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "antec3", RT > spr_MeanAndSDs$meanRT[3]+2.5*(spr_MeanAndSDs$SD[3]))

########## Word position: intrans

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "intrans", RT > spr_MeanAndSDs$meanRT[9]+2.5*(spr_MeanAndSDs$SD[9]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "intrans") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[9]+2.5*(spr_MeanAndSDs$SD[9]), spr_MeanAndSDs$meanRT[9]+2.5*(spr_MeanAndSDs$SD[9])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "intrans", RT > spr_MeanAndSDs$meanRT[9]+2.5*(spr_MeanAndSDs$SD[9]))

########## Word position: comma1

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "comma1", RT > spr_MeanAndSDs$meanRT[4]+2.5*(spr_MeanAndSDs$SD[4]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "comma1") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[4]+2.5*(spr_MeanAndSDs$SD[4]), spr_MeanAndSDs$meanRT[4]+2.5*(spr_MeanAndSDs$SD[4])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "comma1", RT > spr_MeanAndSDs$meanRT[4]+2.5*(spr_MeanAndSDs$SD[4]))

########## Word position: ot_verb

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "ot_verb", RT > spr_MeanAndSDs$meanRT[12]+2.5*(spr_MeanAndSDs$SD[12]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "ot_verb") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[12]+2.5*(spr_MeanAndSDs$SD[12]), spr_MeanAndSDs$meanRT[12]+2.5*(spr_MeanAndSDs$SD[12])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "ot_verb", RT > spr_MeanAndSDs$meanRT[12]+2.5*(spr_MeanAndSDs$SD[12]))

########## Word position: opt_verb

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "opt_verb", RT > spr_MeanAndSDs$meanRT[11]+2.5*(spr_MeanAndSDs$SD[11]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "opt_verb") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[11]+2.5*(spr_MeanAndSDs$SD[11]), spr_MeanAndSDs$meanRT[11]+2.5*(spr_MeanAndSDs$SD[11])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "opt_verb", RT > spr_MeanAndSDs$meanRT[11]+2.5*(spr_MeanAndSDs$SD[11]))

########## Word position: control_verb

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "control_verb", RT > spr_MeanAndSDs$meanRT[7]+2.5*(spr_MeanAndSDs$SD[7]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "control_verb") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[7]+2.5*(spr_MeanAndSDs$SD[7]), spr_MeanAndSDs$meanRT[7]+2.5*(spr_MeanAndSDs$SD[7])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "control_verb", RT > spr_MeanAndSDs$meanRT[7]+2.5*(spr_MeanAndSDs$SD[7]))

########## Word position: spill1

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill1", RT > spr_MeanAndSDs$meanRT[17]+2.5*(spr_MeanAndSDs$SD[17]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill1") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[17]+2.5*(spr_MeanAndSDs$SD[17]), spr_MeanAndSDs$meanRT[17]+2.5*(spr_MeanAndSDs$SD[17])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill1", RT > spr_MeanAndSDs$meanRT[17]+2.5*(spr_MeanAndSDs$SD[17]))

########## Word position: spill2

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill2", RT > spr_MeanAndSDs$meanRT[18]+2.5*(spr_MeanAndSDs$SD[18]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill2") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[18]+2.5*(spr_MeanAndSDs$SD[18]), spr_MeanAndSDs$meanRT[18]+2.5*(spr_MeanAndSDs$SD[18])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill2", RT > spr_MeanAndSDs$meanRT[18]+2.5*(spr_MeanAndSDs$SD[18]))

########## Word position: spill3

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill3", RT > spr_MeanAndSDs$meanRT[19]+2.5*(spr_MeanAndSDs$SD[19]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill3") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[19]+2.5*(spr_MeanAndSDs$SD[19]), spr_MeanAndSDs$meanRT[19]+2.5*(spr_MeanAndSDs$SD[19])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill3", RT > spr_MeanAndSDs$meanRT[19]+2.5*(spr_MeanAndSDs$SD[19]))

########## Word position: spill4

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill4", RT > spr_MeanAndSDs$meanRT[20]+2.5*(spr_MeanAndSDs$SD[20]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill4") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[20]+2.5*(spr_MeanAndSDs$SD[20]), spr_MeanAndSDs$meanRT[20]+2.5*(spr_MeanAndSDs$SD[20])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill4", RT > spr_MeanAndSDs$meanRT[20]+2.5*(spr_MeanAndSDs$SD[20]))

########### ## Word position: comma2

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "comma2", RT > spr_MeanAndSDs$meanRT[5]+2.5*(spr_MeanAndSDs$SD[5]))))
outliers

spr_Transformed <- rbind(piaget_Out_Transformed, spr_noExtremes %>% filter(Answer == "comma2") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[5]+2.5*(spr_MeanAndSDs$SD[5]), spr_MeanAndSDs$meanRT[5]+2.5*(spr_MeanAndSDs$SD[5])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "comma2", RT > spr_MeanAndSDs$meanRT[5]+2.5*(spr_MeanAndSDs$SD[5]))

########## Word position: conj

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "conj", RT > spr_MeanAndSDs$meanRT[6]+2.5*(spr_MeanAndSDs$SD[6]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "conj") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[6]+2.5*(spr_MeanAndSDs$SD[6]), spr_MeanAndSDs$meanRT[6]+2.5*(spr_MeanAndSDs$SD[6])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "conj", RT > spr_MeanAndSDs$meanRT[6]+2.5*(spr_MeanAndSDs$SD[6]))

############ Word position: verb2

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "verb2", RT > spr_MeanAndSDs$meanRT[22]+2.5*(spr_MeanAndSDs$SD[22]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "verb2") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[22]+2.5*(spr_MeanAndSDs$SD[22]), spr_MeanAndSDs$meanRT[22]+2.5*(spr_MeanAndSDs$SD[22])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "verb2", RT > spr_MeanAndSDs$meanRT[22]+2.5*(spr_MeanAndSDs$SD[22]))

########## Word position: spill_second1

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill_second1", RT > spr_MeanAndSDs$meanRT[13]+2.5*(spr_MeanAndSDs$SD[13]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill_second1") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[13]+2.5*(spr_MeanAndSDs$SD[13]), spr_MeanAndSDs$meanRT[13]+2.5*(spr_MeanAndSDs$SD[13])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill_second1", RT > spr_MeanAndSDs$meanRT[13]+2.5*(spr_MeanAndSDs$SD[13]))

########## Word position: spill_second2

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill_second2", RT > spr_MeanAndSDs$meanRT[14]+2.5*(spr_MeanAndSDs$SD[14]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill_second2") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[14]+2.5*(spr_MeanAndSDs$SD[14]), spr_MeanAndSDs$meanRT[14]+2.5*(spr_MeanAndSDs$SD[14])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill_second2", RT > spr_MeanAndSDs$meanRT[14]+2.5*(spr_MeanAndSDs$SD[14]))

########## Word position: spill_second3

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill_second3", RT > spr_MeanAndSDs$meanRT[15]+2.5*(spr_MeanAndSDs$SD[15]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill_second3") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[15]+2.5*(spr_MeanAndSDs$SD[15]), spr_MeanAndSDs$meanRT[15]+2.5*(spr_MeanAndSDs$SD[15])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill_second3", RT > spr_MeanAndSDs$meanRT[15]+2.5*(spr_MeanAndSDs$SD[15]))

########## Word position: spill_second4

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spill_second4", RT > spr_MeanAndSDs$meanRT[16]+2.5*(spr_MeanAndSDs$SD[16]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spill_second4") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[16]+2.5*(spr_MeanAndSDs$SD[16]), spr_MeanAndSDs$meanRT[16]+2.5*(spr_MeanAndSDs$SD[16])))) #transforming outlier values

# sanity check
spr_Transformed  %>%  filter(Answer == "spill_second4", RT > spr_MeanAndSDs$meanRT[16]+2.5*(spr_MeanAndSDs$SD[16]))

########## Word position: spillextra

outliers <- sum(outliers, nrow(spr_noExtremes %>% 
                                 filter(Answer == "spillextra", RT > spr_MeanAndSDs$meanRT[21]+2.5*(spr_MeanAndSDs$SD[21]))))
outliers

spr_Transformed <- rbind(spr_Transformed, spr_noExtremes %>% filter(Answer == "spillextra") %>% mutate(RT = replace(RT, RT > spr_MeanAndSDs$meanRT[21]+2.5*(spr_MeanAndSDs$SD[21]), spr_MeanAndSDs$meanRT[21]+2.5*(spr_MeanAndSDs$SD[21])))) #transforming outlier values

########## sanity check
spr_Transformed  %>%  filter(Answer == "spillextra", RT > spr_MeanAndSDs$meanRT[21]+2.5*(spr_MeanAndSDs$SD[21]))


# Percentage of outliers transformed/removed with >2.5 SD parameter : 2.47 % of total data

outliers*100/nrow(spr_Transformed)

##### Done with outliers detection and transformation!



# 3. generating line graph 


# Building line GRAPH to show differences in reading times across conditions for each word in target sentences

# creating new variable holding data for graph with nem column SentencePosition 
# everything is the same from Answer column, except ot_verb, opt_verb and control_verb all become main_verb

# Positions: det, name, [antec1, antec2, antec3 / intrans] , comma1, [ot_verb / opt_verb / control_verb ],   
# spill1, spill2, spill3, spill4, comma2, conj, verb2, spill_second1, spill_second2, spill_second3, spill_second4
# spillextra

spr_Transformed_GRAPH <- spr_Transformed

spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "det"] <- "det"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "name"] <- "name"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "antec1"] <- "verb1"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "intrans"] <- "verb1"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "antec2"] <- "antec2"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "antec3"] <- "antec3"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "comma1"] <- "comma1"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "ot_verb"] <- "main_verb"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "opt_verb"] <- "main_verb"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "control_verb"] <- "main_verb"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill1"] <- "spill1"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill2"] <- "spill2"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill3"] <- "spill3"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill4"] <- "spill4"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "comma2"] <- "comma2"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "conj"] <- "conj"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "verb2"] <- "verb2"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill_second1"] <- "spill_second1"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill_second2"] <- "spill_second2"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill_second3"] <- "spill_second3"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spill_second4"] <- "spill_second4"
spr_Transformed_GRAPH$SentencePosition[spr_Transformed_GRAPH$Answer == "spillextra"] <- "spillextra"

head(spr_Transformed_GRAPH)

# Some word positions only have data for a subset of all conditions,
# For those positions, need to add the missing conditions as NA so that the ggplot knows they're missing
# Add antec2 and antec3 for conditions c and d

spr_Transformed_GRAPH <- spr_Transformed_GRAPH %>% add_row(Participant = NA, List = NA, ParticipantId = NA, ItemType = NA, ItemNumber = NA, Condition = "c", WordPosition = NA, Word = NA, Answer = NA, RT = NA, Antecedent = NA, VerbType = NA, SentencePosition = "antec2")
spr_Transformed_GRAPH <- spr_Transformed_GRAPH %>% add_row(Participant = NA, List = NA, ParticipantId = NA, ItemType = NA, ItemNumber = NA, Condition = "c", WordPosition = NA, Word = NA, Answer = NA, RT = NA, Antecedent = NA, VerbType = NA, SentencePosition = "antec3")
spr_Transformed_GRAPH <- spr_Transformed_GRAPH %>% add_row(Participant = NA, List = NA, ParticipantId = NA, ItemType = NA, ItemNumber = NA, Condition = "d", WordPosition = NA, Word = NA, Answer = NA, RT = NA, Antecedent = NA, VerbType = NA, SentencePosition = "antec2")
spr_Transformed_GRAPH <- spr_Transformed_GRAPH %>% add_row(Participant = NA, List = NA, ParticipantId = NA, ItemType = NA, ItemNumber = NA, Condition = "d", WordPosition = NA, Word = NA, Answer = NA, RT = NA, Antecedent = NA, VerbType = NA, SentencePosition = "antec3")

# Now make summary with data for graph

spr_summary_GRAPH <- spr_Transformed_GRAPH %>% group_by(Condition, SentencePosition) %>% summarise(meanRT = mean(RT), SE = std.error(RT), total = n())
spr_summary_GRAPH

# removing final word position from graph (only a few items contain this additional word position)

spr_summary_GRAPH = filter(spr_summary_GRAPH, SentencePosition != "spillextra")

# now creating a column ORDER so that word positions can automatically appear in correct order in line graph
# (there are multiple ways to do this, I am using the one that seems more practical now)

spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "det"] <- "1"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "name"] <- "2"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "verb1"] <- "3"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "antec2"] <- "4"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "antec3"] <- "5"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "comma1"] <- "6"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "main_verb"] <- "7"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill1"] <- "8"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill2"] <- "9"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill3"] <- "91"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill4"] <- "92"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "comma2"] <- "93"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "conj"] <- "94"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "verb2"] <- "95"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill_second1"] <- "96"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill_second2"] <- "97"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill_second3"] <- "98"
spr_summary_GRAPH$WordOrder[spr_summary_GRAPH$SentencePosition == "spill_second4"] <- "99"

spr_summary_GRAPH

# now adding columns to summary with relevant conditions for graph 

spr_summary_GRAPH$Antecedent[spr_summary_GRAPH$Condition == "a"] <- "with antecedent"
spr_summary_GRAPH$Antecedent[spr_summary_GRAPH$Condition == "b"] <- "with antecedent"
spr_summary_GRAPH$Antecedent[spr_summary_GRAPH$Condition == "c"] <- "no antecedent"
spr_summary_GRAPH$Antecedent[spr_summary_GRAPH$Condition == "d"] <- "no antecedent"
spr_summary_GRAPH$Antecedent[spr_summary_GRAPH$Condition == "e"] <- "with antecedent"

spr_summary_GRAPH$VerbType[spr_summary_GRAPH$Condition == "a"] <- "OPT verb"
spr_summary_GRAPH$VerbType[spr_summary_GRAPH$Condition == "b"] <- "OT verb"
spr_summary_GRAPH$VerbType[spr_summary_GRAPH$Condition == "c"] <- "OPT verb"
spr_summary_GRAPH$VerbType[spr_summary_GRAPH$Condition == "d"] <- "OT verb"
spr_summary_GRAPH$VerbType[spr_summary_GRAPH$Condition == "e"] <- "Control verb"

head(spr_summary_GRAPH)


# BUILDING GRAPH USING GGPLOT

spr_GRAPH <- ggplot(spr_summary_GRAPH, aes(x=WordOrder, y=meanRT, group=Condition)) +
  geom_errorbar(aes(ymin=meanRT-SE, ymax=meanRT+SE), width=.1) +
  geom_line(aes(color=VerbType,linetype=Antecedent)) +
  geom_point(aes(color=VerbType,shape=Antecedent)) +
  scale_x_discrete(labels = c("A", "Clara", "(entrou/escolheu", "um", "livro)", ",", "(leu/folheou/sentou)", "no", "sofá", "da", "sala", ",", "depois", "guardou", "na", "estante", "e", "saiu")) +
  xlab("") + ylab("Mean Reading Time in ms") +
  theme(text = element_text(size=26)) +
  theme(legend.position="right") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

spr_GRAPH


# 4. linear mixed model analyses 

###### Liner Mixed Models (each word position at a time)

###### start with maximal model and reduce model if needed (e.g., if singularity or convergence issues occur)


# Add columns for factors that represent independent variables (antecedent yes-no, verb type ot/opt/control)

# Antecedent
spr_Transformed$Antecedent[spr_Transformed$Condition == "a"] <- "Antec"
spr_Transformed$Antecedent[spr_Transformed$Condition == "b"] <- "Antec"
spr_Transformed$Antecedent[spr_Transformed$Condition == "c"] <- "NoAntec"
spr_Transformed$Antecedent[spr_Transformed$Condition == "d"] <- "NoAntec"
spr_Transformed$Antecedent[spr_Transformed$Condition == "e"] <- "Antec"

# VerbType : OT (1) vs. OPT (-1) comparison
spr_Transformed$VerbType[spr_Transformed$Condition == "a"] <- "OPT"
spr_Transformed$VerbType[spr_Transformed$Condition == "b"] <- "OT"
spr_Transformed$VerbType[spr_Transformed$Condition == "c"] <- "OPT"
spr_Transformed$VerbType[spr_Transformed$Condition == "d"] <- "OT"
spr_Transformed$VerbType[spr_Transformed$Condition == "e"] <- "Control"

head(spr_Transformed)


# MAIN COMPARISON  (antecedent * verb type) 

# excludes control ("e") - not relevant for this analysis
spr_Transformed_MainComp <- spr_Transformed %>% filter(Condition != "e") # excludes control condition (because I can't figure out sum contrast with it)
tail(spr_Transformed_MainComp)

# set contrasts on relevant conditions
spr_Transformed_MainComp$Antecedent <- as.factor(spr_Transformed_MainComp$Antecedent)
levels(spr_Transformed_MainComp$Antecedent)
contrasts(spr_Transformed_MainComp$Antecedent)
contrasts(spr_Transformed_MainComp$Antecedent) <- c(1, -1)
contrasts(spr_Transformed_MainComp$Antecedent)

spr_Transformed_MainComp$VerbType <- as.factor(spr_Transformed_MainComp$VerbType)
levels(spr_Transformed_MainComp$VerbType)
contrasts(spr_Transformed_MainComp$VerbType)
contrasts(spr_Transformed_MainComp$VerbType) <- c(-1, 1)
contrasts(spr_Transformed_MainComp$VerbType)


### This script contains only Main Comparison analysis for word positions in the critical region (Main Verb, spill1, spill2 and spill3)

# lmer: Main Verb (opt_verb, ot_verb, control_verb) (main comparison) ##################################################################

spr_Transformed_MainComp_MainVerb <- spr_Transformed_MainComp %>% filter(Answer %in% c("ot_verb", "opt_verb")) 
head(spr_Transformed_MainComp_MainVerb)
tail(spr_Transformed_MainComp_MainVerb)

spr_Transformed_MainComp_MainVerb_Model1 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent*VerbType|ItemNumber), spr_Transformed_MainComp_MainVerb)
summary(spr_Transformed_MainComp_MainVerb_Model1)

spr_Transformed_MainComp_MainVerb_Model2 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_MainVerb)
summary(spr_Transformed_MainComp_MainVerb_Model2)

spr_Transformed_MainComp_MainVerb_Model3 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_MainVerb)
summary(spr_Transformed_MainComp_MainVerb_Model3)

spr_Transformed_MainComp_MainVerb_Model4 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_MainVerb)
summary(spr_Transformed_MainComp_MainVerb_Model4)

spr_Transformed_MainComp_MainVerb_Model5 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_MainVerb)
summary(spr_Transformed_MainComp_MainVerb_Model5)

spr_Transformed_MainComp_MainVerb_Model6 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1|ItemNumber), spr_Transformed_MainComp_MainVerb)
summary(spr_Transformed_MainComp_MainVerb_Model6)

spr_Transformed_MainComp_MainVerb_Model7 <- lmer(RT ~ Antecedent*VerbType + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_MainVerb)
summary(spr_Transformed_MainComp_MainVerb_Model7)

# lmer: spill1 (main comparison) ##################################################################

spr_Transformed_MainComp_spill1 <- spr_Transformed_MainComp %>% filter(Answer == "spill1")
head(spr_Transformed_MainComp_spill1)
tail(spr_Transformed_MainComp_spill1)

spr_Transformed_MainComp_spill1_Model1 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent*VerbType|ItemNumber), spr_Transformed_MainComp_spill1)
summary(spr_Transformed_MainComp_spill1_Model1)

spr_Transformed_MainComp_spill1_Model2 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_spill1)
summary(spr_Transformed_MainComp_spill1_Model2)

spr_Transformed_MainComp_spill1_Model3 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_spill1)
summary(spr_Transformed_MainComp_spill1_Model3)

spr_Transformed_MainComp_spill1_Model4 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_spill1)
summary(spr_Transformed_MainComp_spill1_Model4)

spr_Transformed_MainComp_spill1_Model5 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_spill1)
summary(spr_Transformed_MainComp_spill1_Model5)

spr_Transformed_MainComp_spill1_Model6 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill1)
summary(spr_Transformed_MainComp_spill1_Model6)

spr_Transformed_MainComp_spill1_Model7 <- lmer(RT ~ Antecedent*VerbType + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill1)
summary(spr_Transformed_MainComp_spill1_Model7)

# spill1 pairwise : just OPT verbs

spr_Transformed_MainComp_spill1_OPT <- spr_Transformed_MainComp_spill1 %>% filter(VerbType == "OPT")
head(spr_Transformed_MainComp_spill1_OPT)
tail(spr_Transformed_MainComp_spill1_OPT)

spr_Transformed_MainComp_spill1_OPT_Model1 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1+Antecedent|ItemNumber), spr_Transformed_MainComp_spill1_OPT)
summary(spr_Transformed_MainComp_spill1_OPT_Model1)

spr_Transformed_MainComp_spill1_OPT_Model2 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill1_OPT)
summary(spr_Transformed_MainComp_spill1_OPT_Model2)

spr_Transformed_MainComp_spill1_OPT_Model3 <- lmer(RT ~ Antecedent + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill1_OPT)
summary(spr_Transformed_MainComp_spill1_OPT_Model3)

# spill1 pairwise : just OT verbs

spr_Transformed_MainComp_spill1_OT <- spr_Transformed_MainComp_spill1 %>% filter(VerbType == "OT")
head(spr_Transformed_MainComp_spill1_OT)
tail(spr_Transformed_MainComp_spill1_OT)

spr_Transformed_MainComp_spill1_OT_Model1 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1+Antecedent|ItemNumber), spr_Transformed_MainComp_spill1_OT)
summary(spr_Transformed_MainComp_spill1_OT_Model1)

spr_Transformed_MainComp_spill1_OT_Model2 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill1_OT)
summary(spr_Transformed_MainComp_spill1_OT_Model2)

spr_Transformed_MainComp_spill1_OT_Model3 <- lmer(RT ~ Antecedent + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill1_OT)
summary(spr_Transformed_MainComp_spill1_OT_Model3)


# lmer: spill2 (main comparison) ##################################################################

spr_Transformed_MainComp_spill2 <- spr_Transformed_MainComp %>% filter(Answer == "spill2")
head(spr_Transformed_MainComp_spill2)
tail(spr_Transformed_MainComp_spill2)

spr_Transformed_MainComp_spill2_Model1 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent*VerbType|ItemNumber), spr_Transformed_MainComp_spill2)
summary(spr_Transformed_MainComp_spill2_Model1)

spr_Transformed_MainComp_spill2_Model2 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_spill2)
summary(spr_Transformed_MainComp_spill2_Model2)

spr_Transformed_MainComp_spill2_Model3 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_spill2)
summary(spr_Transformed_MainComp_spill2_Model3)

spr_Transformed_MainComp_spill2_Model4 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_spill2)
summary(spr_Transformed_MainComp_spill2_Model4)

spr_Transformed_MainComp_spill2_Model5 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_spill2)
summary(spr_Transformed_MainComp_spill2_Model5)

spr_Transformed_MainComp_spill2_Model6 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill2)
summary(spr_Transformed_MainComp_spill2_Model6)

spr_Transformed_MainComp_spill2_Model7 <- lmer(RT ~ Antecedent*VerbType + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill2)
summary(spr_Transformed_MainComp_spill2_Model7)

# spill2 pairwise : just OPT verbs

spr_Transformed_MainComp_spill2_OPT <- spr_Transformed_MainComp_spill2 %>% filter(VerbType == "OPT")
head(spr_Transformed_MainComp_spill2_OPT)
tail(spr_Transformed_MainComp_spill2_OPT)

spr_Transformed_MainComp_spill2_OPT_Model1 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1+Antecedent|ItemNumber), spr_Transformed_MainComp_spill2_OPT)
summary(spr_Transformed_MainComp_spill2_OPT_Model1)

spr_Transformed_MainComp_spill2_OPT_Model2 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill2_OPT)
summary(spr_Transformed_MainComp_spill2_OPT_Model2)

spr_Transformed_MainComp_spill2_OPT_Model3 <- lmer(RT ~ Antecedent + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill2_OPT)
summary(spr_Transformed_MainComp_spill2_OPT_Model3)

# no effect of antecedent in OPT verbs

# spill2 pairwise : just OT verbs

spr_Transformed_MainComp_spill2_OT <- spr_Transformed_MainComp_spill2 %>% filter(VerbType == "OT")
head(spr_Transformed_MainComp_spill2_OT)
tail(spr_Transformed_MainComp_spill2_OT)

spr_Transformed_MainComp_spill2_OT_Model1 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1+Antecedent|ItemNumber), spr_Transformed_MainComp_spill2_OT)
summary(spr_Transformed_MainComp_spill2_OT_Model1)

spr_Transformed_MainComp_spill2_OT_Model2 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill2_OT)
summary(spr_Transformed_MainComp_spill2_OT_Model2)

spr_Transformed_MainComp_spill2_OT_Model3 <- lmer(RT ~ Antecedent + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill2_OT)
summary(spr_Transformed_MainComp_spill2_OT_Model3)


# lmer: spill3 (main comparison) ##################################################################

spr_Transformed_MainComp_spill3 <- spr_Transformed_MainComp %>% filter(Answer == "spill3")
head(spr_Transformed_MainComp_spill3)
tail(spr_Transformed_MainComp_spill3)

spr_Transformed_MainComp_spill3_Model1 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent*VerbType|ItemNumber), spr_Transformed_MainComp_spill3)
summary(spr_Transformed_MainComp_spill3_Model1)

spr_Transformed_MainComp_spill3_Model2 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent*VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_spill3)
summary(spr_Transformed_MainComp_spill3_Model2)

spr_Transformed_MainComp_spill3_Model3 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+Antecedent+VerbType|ItemNumber), spr_Transformed_MainComp_spill3)
summary(spr_Transformed_MainComp_spill3_Model3)

spr_Transformed_MainComp_spill3_Model4 <- lmer(RT ~ Antecedent*VerbType + (1+Antecedent+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_spill3)
summary(spr_Transformed_MainComp_spill3_Model4)

spr_Transformed_MainComp_spill3_Model5 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1+VerbType|ItemNumber), spr_Transformed_MainComp_spill3)
summary(spr_Transformed_MainComp_spill3_Model5)

spr_Transformed_MainComp_spill3_Model6 <- lmer(RT ~ Antecedent*VerbType + (1+VerbType|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill3)
summary(spr_Transformed_MainComp_spill3_Model6)

spr_Transformed_MainComp_spill3_Model7 <- lmer(RT ~ Antecedent*VerbType + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill3)
summary(spr_Transformed_MainComp_spill3_Model7)


# spill3 pairwise : just OPT verbs

spr_Transformed_MainComp_spill3_OPT <- spr_Transformed_MainComp_spill3 %>% filter(VerbType == "OPT")
head(spr_Transformed_MainComp_spill3_OPT)
tail(spr_Transformed_MainComp_spill3_OPT)

spr_Transformed_MainComp_spill3_OPT_Model1 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1+Antecedent|ItemNumber), spr_Transformed_MainComp_spill3_OPT)
summary(spr_Transformed_MainComp_spill3_OPT_Model1)

spr_Transformed_MainComp_spill3_OPT_Model2 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill3_OPT)
summary(spr_Transformed_MainComp_spill3_OPT_Model2)

spr_Transformed_MainComp_spill3_OPT_Model3 <- lmer(RT ~ Antecedent + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill3_OPT)
summary(spr_Transformed_MainComp_spill3_OPT_Model3)

# no significant effect of antecedent for OPT verbs

# spill3 pairwise : just OT verbs

spr_Transformed_MainComp_spill3_OT <- spr_Transformed_MainComp_spill3 %>% filter(VerbType == "OT")
head(spr_Transformed_MainComp_spill3_OT)
tail(spr_Transformed_MainComp_spill3_OT)

spr_Transformed_MainComp_spill3_OT_Model1 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1+Antecedent|ItemNumber), spr_Transformed_MainComp_spill3_OT)
summary(spr_Transformed_MainComp_spill3_OT_Model1)

spr_Transformed_MainComp_spill3_OT_Model2 <- lmer(RT ~ Antecedent + (1+Antecedent|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill3_OT)
summary(spr_Transformed_MainComp_spill3_OT_Model2)

spr_Transformed_MainComp_spill3_OT_Model3 <- lmer(RT ~ Antecedent + (1|Participant) + (1|ItemNumber), spr_Transformed_MainComp_spill3_OT)
summary(spr_Transformed_MainComp_spill3_OT_Model3)


