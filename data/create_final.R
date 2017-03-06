# Attention: This is not a complete dataset and it may not be used outside this course. There are both variables and observations missing. 
# Any inferences to real life are completely fictional and are only used for the purpose of this course. The dataset in its entirety belongs 
# to the Niilo Mäki Institute and results based on the complete dataset will be presented at the JURE2017 conference for those interested.



#Read the data from SPSS-generated csv-file. Exciting, let's see how it works!

# UPDATE: After looking around a bit, and first trying the "foreign" package and read.spss-function, which didn't work, I found the 
#"memisc"" package with the as.data.set-function, which at first glance seems to work perfectly!

library(dplyr)
library(memisc)
setwd("C:/Users/hehusb/Documents/GitHub/IODS-final")
kesky_all <- as.data.set(spss.system.file("./data/Kesky_luvat_final_tausta_very_short_3.sav"))
str(kesky_all)
summary(kesky_all$utb_grund_mamma)
summary(kesky_all$medicinering)
summary(kesky_all$medievanor1)
summary(kesky_all$skola_nummer)
summary(kesky_all$hur_garna_lasbarn1)
summary(kesky_all$hur_ofta_lasbarn1)
kesky_all$skola_nummer <- as.factor(kesky_all$skola_nummer)
kesky_all <- as.data.frame(kesky_all)
str(kesky_all)
summary(kesky_all$utb_grund_mamma)
summary(kesky_all$medicinering)
summary(kesky_all$medievanor1)
summary(kesky_all$skola_nummer)
summary(kesky_all$hur_garna_lasbarn1)


# So far so good: got the data into a data frame and it looks pretty solid. Above I checked out some of the variables and converted one 
# from integer to categorical (school number). In order to protect the identities of students' and schools I'd removed the school labels prior to importing the data, so R naturlly thought it was a continuous variable. 

# Before I can start analyzing I still need to wrangle the data a bit. As stated above, this dataset is not the final complete dataset, 
# and I will dismember it even more. First of all, I've now decided to look only at my most central variables of interest so I'll remove 
# many of the ones I'd kept when importing.

names(kesky_all)
keep <- c("elev_id","elev_kon","arskurs","skola_nummer","klasstorlek","dist_01","dist_02","dist_03","dist_04","imp_05","imp_06","imp_07","imp_08","imp_09","imp_10","imp_11","imp_12","imp_13","moto_14","moto_15","moto_16","moto_17","moto_18","moto_19","moto_20","rikt_21","rikt_22","rikt_23","rikt_24","rikt_25","uppr_26","uppr_27","uppr_28","uppr_29","uppr_30","uppr_31","skif_32","skif_33","skif_34","skif_35","efig_36","efig_37","efig_38","efig_39","efig_40","efpl_41","efpl_42","efpl_43","efpl_44","efutf_45","efutf_46","efutf_47","efutf_48","efutf_49","efutf_50","efutf_51","efutf_52","efutv_53","efutv_54","efutv_55","beteende","vitsord_ma","vitsord_mo","utb_pabygg_mamma","utb_pabygg_pappa","gatt_om_arskurs","skolform","medicinering","barn_sprak","medievanor1","medievanor2","medievanor3","hur_ofta_lasbarn1","hur_ofta_lasbarn2","hur_ofta_lasbarn3","hur_ofta_lasbarn4","hur_ofta_lasbarn5","hur_ofta_lasbarn6","hur_ofta_lasbarn7","hur_garna_lasbarn1","hur_garna_lasbarn2","hur_garna_lasbarn3","hur_garna_lasbarn4","hur_garna_lasbarn5","hur_garna_lasbarn6","hur_garna_lasbarn7","hur_manga_bocker1","hur_manga_bocker2","borjade_lasforbarn")

kesky_shrt <- dplyr::select(kesky_all, one_of(keep))
names(kesky_shrt)
dim(kesky_shrt)
str(kesky_shrt)

# Whew, that took a bit of searching on the internet. Could not get the dplyr::select()-command to work at first, 
# thought it was the MASS-conflict messing about. However, it turned out that I simply had to use the function as.data.frame() 
# on my imported data before select could actually use the data. Now that I've got my dataset working I still need to mutate around a bit. 
# But first I'll do a listwise deletion just to make it easier to operate on the data. In the final analyses I will probably be imputing 
# some of the variables.

complete.cases(kesky_shrt)
summary(kesky_shrt)

# For now, I have too many variables describing SES, reading habits and reading preferences. I want to aggregate these so I have one variable 
# describing each phenomenon. To do this I'll simply create sum variables. The SES will consist of the mothers and fathers highest education 
# level summed. Similarly, reading habits and preferences will be summed over different categories of litterature. This is going to take some 
# wrangling. The variables I want to look at are:


# Attention & executive function: measured by a 55-item questionnaire (variables 6-61)

which(names(kesky_shrt)%in%c("dist_01", "efutv_55"))
summary(kesky_shrt[, 6:60])

# School achievement: mesured by 2 questions (math/Swedish grade) (variables 63-64)

which(names(kesky_shrt)%in%c("vitsord_ma", "vitsord_mo"))
summary(kesky_shrt[, 62:63])

# Media time: Measured by 3-item questionnaire (variables 71-73)

which(names(kesky_shrt)%in%c("medievanor1", "medievanor3"))
summary(kesky_shrt[, 70:72])

# Reading preferences: Measured by 7 questions (variables 74-80)
# Reading habits: Measured by 7 questions (variables 81-87)


which(names(kesky_shrt)%in%c("hur_garna_lasbarn7", "hur_ofta_lasbarn1"))
summary(kesky_shrt[, 73:86])


# While controlling for SES: Here measured by parents' highest educational level (variables 65-66) and gender: (variable 2)

which(names(kesky_shrt)%in%c("utb_pabygg_mamma", "utb_pabygg_pappa"))
summary(kesky_shrt[, 64:65])

which(names(kesky_shrt)%in%c("elev_kon"))
summary(kesky_shrt[, 2])

# There's a few more variables that I want to keep around for now, but I'll start by subsetting these and checking missing values:

kesky_subset <- kesky_shrt[c(2, 6:60, 62:63, 64:65, 70:72, 73:86)]
names(kesky_subset)
dim(kesky_subset)
complete.cases(kesky_subset)

# print out the data along with a completeness indicator as the last column
data.frame(kesky_subset[-1], comp = complete.cases(kesky_subset))

# Ok, that didn't look quite as catastrophic as the first glance. I'll create two datasets, one with no missing values to use in the
# analysis looking at the relationships beween the background information (i.e. media time and reading habits) and attention (as measured by
# the ATTEX-questionnaire). The other dataset will contain all my cases and will be used to look at the factor structure of the ATTEX-
# questionnaire using confirmatory factor analysis (CFA), because CFA is quite dependent on large sample sizes.

# filter out all rows with NA values
kesky_listwise <- filter(kesky_subset, complete.cases(kesky_subset) == TRUE)
summary(kesky_listwise)
dim(kesky_listwise)
complete.cases(kesky_listwise)

# Right, so now I have a dataset containing my variables of interest and with no missing values. I've lost a lot of 
# information compared to my original dataet of 522 cases, but I won't have to battle the NA:s.

# Next I'll create sum variables of the different variables of the 10 separate dimensions of attention hypothesized in the questionnaire 
summary(kesky_listwise)

dist_columns <- kesky_listwise[2:5]
head(dist_columns)
kesky_listwise$dist <- rowSums(dist_columns)
kesky_listwise$dist

imp_columns <- kesky_listwise[6:14]
head(imp_columns)
kesky_listwise$imp <- rowSums(imp_columns)
kesky_listwise$imp

moto_columns <- kesky_listwise[15:21]
head(moto_columns)
kesky_listwise$moto <- rowSums(moto_columns)
kesky_listwise$moto

rikt_columns <- kesky_listwise[22:26]
head(rikt_columns)
kesky_listwise$rikt <- rowSums(rikt_columns)
kesky_listwise$rikt

uppr_columns <- kesky_listwise[27:32]
head(uppr_columns)
kesky_listwise$uppr <- rowSums(uppr_columns)
kesky_listwise$uppr

skif_columns <- kesky_listwise[33:36]
head(skif_columns)
kesky_listwise$skif <- rowSums(skif_columns)
kesky_listwise$skif

efig_columns <- kesky_listwise[37:41]
head(efig_columns)
kesky_listwise$efig <- rowSums(efig_columns)
kesky_listwise$efig

efpl_columns <- kesky_listwise[42:45]
head(efpl_columns)
kesky_listwise$efpl <- rowSums(efpl_columns)
kesky_listwise$efpl

efutf_columns <- kesky_listwise[46:53]
head(efutf_columns)
kesky_listwise$efutf <- rowSums(efutf_columns)
kesky_listwise$efutf

efutv_columns <- kesky_listwise[54:56]
head(efutv_columns)
kesky_listwise$efutv <- rowSums(efutv_columns)
kesky_listwise$efutv

summary(kesky_listwise)
str(kesky_listwise)
dim(kesky_listwise)

# Now I'll do the same for the dataframe with the missing values:

kesky_nona <- kesky_subset
head(kesky_nona)
dist_columns <- kesky_subset[2:5]
head(dist_columns)
kesky_nona$dist <- rowSums(dist_columns)
kesky_nona$dist

imp_columns <- kesky_subset[6:14]
head(imp_columns)
kesky_nona$imp <- rowSums(imp_columns)
kesky_nona$imp

moto_columns <- kesky_subset[15:21]
head(moto_columns)
kesky_nona$moto <- rowSums(moto_columns)
kesky_nona$moto

rikt_columns <- kesky_subset[22:26]
head(rikt_columns)
kesky_nona$rikt <- rowSums(rikt_columns)
kesky_nona$rikt

uppr_columns <- kesky_subset[27:32]
head(uppr_columns)
kesky_nona$uppr <- rowSums(uppr_columns)
kesky_nona$uppr

skif_columns <- kesky_subset[33:36]
head(skif_columns)
kesky_nona$skif <- rowSums(skif_columns)
kesky_nona$skif

efig_columns <- kesky_subset[37:41]
head(efig_columns)
kesky_nona$efig <- rowSums(efig_columns)
kesky_nona$efig

efpl_columns <- kesky_subset[42:45]
head(efpl_columns)
kesky_nona$efpl <- rowSums(efpl_columns)
kesky_nona$efpl

efutf_columns <- kesky_subset[46:53]
head(efutf_columns)
kesky_nona$efutf <- rowSums(efutf_columns)
kesky_nona$efutf

efutv_columns <- kesky_subset[54:56]
head(efutv_columns)
kesky_nona$efutv <- rowSums(efutv_columns)
kesky_nona$efutv

summary(kesky_nona)
str(kesky_nona)
dim(kesky_nona)
names(kesky_nona)

# In this dataset, I only want to keep the ATTEX sum variables (and the gender variable):

remove5 <- names(kesky_nona[2:77])
remove5
library(dplyr)

kesky_nona <- dplyr::select(kesky_nona, -one_of(remove5))
names(kesky_nona)


# Because one viable way of using the questionnaire data is a single sum variable, I'll go ahead and create a dataframe for that as well:

attention_columns <- kesky_listwise[2:56]
head(attention_columns)
kesky_listwise$attention <- rowSums(attention_columns) 
summary(kesky_listwise$attention)

# I'll be needing a dataset that contains the sums variables but not the original items, because it will make it much easier to
# for example plot the whole dataframe. I'll create the dataframe kesky_sums for that:

remove <- names(kesky_listwise[2:56])
remove

kesky_sums <- dplyr::select(kesky_listwise, -one_of(remove))
str(kesky_sums)
names(kesky_sums)



#And I'll remove the reading variables that I'm not interested in and only leave those related to homework (1 and 2) and literature (3 and 5)

remove2 <- c("hur_ofta_lasbarn4", "hur_ofta_lasbarn6", "hur_ofta_lasbarn7", "hur_garna_lasbarn4", "hur_garna_lasbarn6", "hur_garna_lasbarn7")
remove2
kesky_sums <- dplyr::select(kesky_sums, -one_of(remove2))
names(kesky_sums)



# Last, I'll take the means  for homework related reading and leisure related reading, then remove the original variables:

kesky_sums$rd_hab_hw <- rowMeans(kesky_sums[9:10]) 
kesky_sums$rd_hab_hw

kesky_sums$rd_hab_ls <- rowMeans(kesky_sums[11:12]) 
kesky_sums$rd_hab_ls

kesky_sums$rd_pre_hw <- rowMeans(kesky_sums[13:14]) 
kesky_sums$rd_pre_hw

kesky_sums$rd_pre_ls <- rowMeans(kesky_sums[15:16]) 
kesky_sums$rd_pre_ls

names(kesky_sums)

remove3 <- names(kesky_sums[9:16])
remove3
kesky_sums <- dplyr::select(kesky_sums, -one_of(remove3))
names(kesky_sums)

# And the means for the mothers and fathers education level to form my SES variable, then remove the original variables:

kesky_sums$ses <- rowMeans(kesky_sums[4:5])
kesky_sums$ses

remove4 <- names(kesky_sums[4:5])
remove4
kesky_sums <- dplyr::select(kesky_sums, -one_of(remove4))

names(kesky_sums)


# I'll save this as my new datafiles and use them for my final assignment. All other files will be removed.
dim(kesky_listwise)
head(kesky_listwise)
write.csv2(kesky_listwise, file = "./data/final_cont.csv", row.names = FALSE)
write.csv2(kesky_sums, file = "./data/final_sums_2.csv", row.names=FALSE)
write.csv2(kesky_nona, file = "./data/final_nona.csv", row.names=FALSE)
kesky_read <- read.csv2(file = "./data/final_cont.csv")
sums_read <- read.csv2(file="./data/final_sums_2.csv")
nona_read <- read.csv2(file="./data/final_nona.csv")
dim(kesky_read)
head(kesky_read)
dim(sums_read)
head(sums_read)
dim(nona_read)
library(compare)
compare(kesky_sums, sums_read)
compare(kesky_listwise, kesky_read)
compare(kesky_nona, nona_read)
