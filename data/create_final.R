# Attention: This is not a complete dataset and it may not be used outside this course. There are both variables and observations missing. 
# Any inferences to real life are completely fictional and are only used for the purpose of this course. The dataset in its entirety belongs 
# to the Niilo Mäki Institute and results based on the complete dataset will be presented at the JURE2017 conference for those interested.



#Read the data from SPSS-generated csv-file. Exciting, let's see how it works!

# UPDATE: After looking around a bit, and first trying the "foreign" package and read.spss-function, which didn't work, I found the 
#"memisc"" package with the as.data.set-function, which at first glance seems to work perfectly!

library(dplyr)
library(memisc)
setwd("C:/Users/hehusb/Documents/GitHub/IODS-final")
kesky_all <- as.data.set(spss.system.file("./data/Kesky_luvat_final_tausta_very_short_1.sav"))
str(kesky_all)
summary(kesky_all$utb_grund_mamma)
summary(kesky_all$medicinering)
summary(kesky_all$medievanor1)
summary(kesky_all$skola_nummer)
kesky_all$skola_nummer <- as.factor(kesky_all$skola_nummer)
kesky_all <- as.data.frame(kesky_all)

# So far so good: got the data into a data frame and it looks pretty solid. Above I checked out some of the variables and converted one 
# from integer to categorical (school number). In order to protect the identities of students' and schools I'd removed the school labels prior to importing the data, so R naturlly thought it was a continuous variable. 

# Before I can start analyzing I still need to wrangle the data a bit. As stated above, this dataset is not the final complete dataset, 
# and I will dismember it even more. First of all, I've now decided to look only at my most central variables of interest so I'll remove 
# many of the ones I'd kept when importing.

names(kesky_all)
keep <- c("elev_id","elev_kon","arskurs","skola_nummer","klasstorlek","dist_01","dist_02","dist_03","dist_04","imp_05","imp_06","imp_07","imp_08","imp_09","imp_10","imp_11","imp_12","imp_13","moto_14","moto_15","moto_16","moto_17","moto_18","moto_19","moto_20","rikt_21","rikt_22","rikt_23","rikt_24","rikt_25","uppr_26","uppr_27","uppr_28","uppr_29","uppr_30","uppr_31","skif_32","skif_33","skif_34","skif_35","efig_36","efig_37","efig_38","efig_39","efig_40","efig_sum","efpl_41","efpl_42","efpl_43","efpl_44","efutf_45","efutf_46","efutf_47","efutf_48","efutf_49","efutf_50","efutf_51","efutf_52","efutv_53","efutv_54","efutv_55","beteende","vitsord_ma","vitsord_mo","utb_pabygg_mamma","utb_pabygg_pappa","gatt_om_arskurs","skolform","medicinering","barn_sprak","medievanor1","medievanor2","medievanor3","hur_ofta_lasbarn1","hur_ofta_lasbarn2","hur_ofta_lasbarn3","hur_ofta_lasbarn4","hur_ofta_lasbarn5","hur_ofta_lasbarn6","hur_ofta_lasbarn7","hur_garna_lasbarn1","hur_garna_lasbarn2","hur_garna_lasbarn3","hur_garna_lasbarn4","hur_garna_lasbarn5","hur_garna_lasbarn6","hur_garna_lasbarn7","hur_manga_bocker1","hur_manga_bocker2","borjade_lasforbarn")

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
summary(kesky_shrt[, 6:61])

# School achievement: mesured by 2 questions (math/Swedish grade) (variables 63-64)

which(names(kesky_shrt)%in%c("vitsord_ma", "vitsord_mo"))
summary(kesky_shrt[, 63:64])

# Media time: Measured by 3-item questionnaire (variables 71-73)

which(names(kesky_shrt)%in%c("medievanor1", "medievanor3"))
summary(kesky_shrt[, 71:73])

# Reading preferences: Measured by 7 questions (variables 74-80)
# Reading habits: Measured by 7 questions (variables 81-87)


which(names(kesky_shrt)%in%c("hur_garna_lasbarn7", "hur_ofta_lasbarn1"))
summary(kesky_shrt[, 74:87])


# While controlling for SES: Here measured by parents' highest educational level (variables 65-66) and gender: (variable 2)

which(names(kesky_shrt)%in%c("utb_pabygg_mamma", "utb_pabygg_pappa"))
summary(kesky_shrt[, 65:66])

which(names(kesky_shrt)%in%c("elev_kon"))
summary(kesky_shrt[, 2])

# There's a few more variables that I want to keep around for now, but I'll start by subsetting these and checking missing values:

kesky_subset <- kesky_shrt[c(2, 6:61, 63:64, 65:66, 71:73, 74:87)]
names(kesky_subset)
dim(kesky_subset)
complete.cases(kesky_subset)

# print out the data along with a completeness indicator as the last column
data.frame(kesky_subset[-1], comp = complete.cases(kesky_subset))

# Ok, that didn't look quite as catastrophic as the first glance. I'll just go with a truncated dataset, 
# using the cases that have complete data on the variables I'm actually going to analyze:

# filter out all rows with NA values
kesky_listwise <- filter(kesky_subset, complete.cases(kesky_subset) == TRUE)
summary(kesky_listwise)
dim(kesky_listwise)
complete.cases(kesky_listwise)

# Right, so now I have a dataset containing my variables of interest and with no missing values. I've lost a lot of 
# information compared to my original dataet of 522 cases, but I won't have to battle the NA:s.

# I'll save this as my new datafile and use it for my final assignment. All other files will be removed.
dim(kesky_listwise)
head(kesky_listwise)
write.csv2(kesky_listwise, file = "./data/final.csv", row.names = FALSE)
kesky_read <- read.csv2(file = "./data/final.csv")
dim(kesky_read)
head(kesky_read)
library(compare)
compare(kesky_listwise, kesky_read)

# Why are they not the same? Beats me...