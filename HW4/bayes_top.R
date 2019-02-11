#####################################################################
### PSU ETM 538                        Assignment 4 - Naive Bayes ###
###                                       Last update = 10/3/2018 ###
###                                                               ###
### Top-level file for the Naive Bayes assignment.                ###
### To run the program, simply "select all" in this script        ###
### and run it.                                                   ###
#####################################################################
### -----------------------------------------------------------------

### ------------------ Customization Section ------------------------

### There are two paths to set up:  DATA_PATH to the raw data,
### and R_PATH to any R code files you wish to use.

path_data              <- "C:\Users\Jordan\Documents\ETM 538\ETM538W2019\HW4"
path_out               <- "C:\Users\Jordan\Documents\ETM 538\ETM538W2019\HW4"
path_r                 <- "C:\Users\Jordan\Documents\ETM 538\ETM538W2019\HW4"

### ----------------- Load Heritage Data ----------------------------

path_mem_claims        <- paste(path_data, "Claims_Y1.csv", sep="\\")
path_mem_days          <- paste(path_data, "DayInHospital_Y2.csv", sep="\\")
path_mem_info          <- paste(path_data, "Members_Y1.csv", sep="\\")
path_risk_model        <- paste(path_data, "risk_model_1.csv", sep="\\")

path_out_a_priori      <- paste(path_out, "out_a_priori.csv", sep="\\")
path_out_charlson      <- paste(path_out, "out_charlson.csv", sep="\\")
path_out_stay          <- paste(path_out, "out_stay.csv", sep="\\")
path_out_pcg           <- paste(path_out, "out_pcg.csv", sep="\\")

mem_claims             <- read.csv(path_mem_claims)
mem_days               <- read.csv(path_mem_days)
mem_info               <- read.csv(path_mem_info)
risk_model             <- read.csv(path_risk_model)

### ----------------------- Rename Columns --------------------------

colnames(mem_days) <- c("MemberID", "Days")

### -------------------- Enrich Claims Records ----------------------

mem_to_risk <- merge(mem_days, risk_model, by="Days")

claims_to_risk <- merge(mem_claims, mem_to_risk, by = "MemberID")

### --------------- Calculate A Priori Probabilities ----------------

n_claims <- length(claims_to_risk[,1])     # note that we have to pick a column.

risks <- as.data.frame(as.character(claims_to_risk$RiskLevel))

riskl <- as.list(risks)

risk_count <- aggregate(risks, riskl, FUN=length)

colnames(risk_count) <- c("RiskLevel", "RiskCount")

a_priori <- risk_count

a_priori$Total <- n_claims

a_priori$Prob <- a_priori$RiskCount / n_claims

colnames(a_priori) <- c("RiskLevel", "Prob")

write.csv(a_priori, file = path_out_a_priori, row.names = FALSE)

### -------------------- Condition on Charlson ----------------------

on_charlson <- data.frame(as.character(claims_to_risk$RiskLevel),
                             as.character(claims_to_risk$CharlsonIndex))

colnames(on_charlson) <- c("RiskLevel", "Charlson")

df_char <- as.data.frame(as.character(on_charlson$Charlson))

colnames(df_char) <- c("Charlson")

l_char <- on_charlson$Charlson

l_risk <- on_charlson$RiskLevel

count_char <- aggregate(df_char, by=list(l_char, l_risk), FUN=length)

colnames(count_char) <- c("Charlson", "RiskLevel", "Count")

# check the total to make sure everything is present and accounted for.

n_chars <- sum(count_char$Count)

n_missing <- n_claims - n_chars

print(paste("A Posteriori Charlson -- ", toString(n_missing), " are missing."))

post_char <- merge(count_char, risk_count, by="RiskLevel")

post_char$Prob <- post_char$Count / post_char$RiskCount

post_char$Label <- paste(post_char$Charlson, post_char$RiskLevel, sep="|")

# reorder the columns

post_char <- post_char[c("Label", "Charlson", "RiskLevel", "Count", "RiskCount", "Prob")]


### ----------------- Condition on Length of Stay -------------------

# extract length of stay as a vector

new_stay <- as.character(claims_to_risk$LengthOfStay)

# assign default value to missing columns

new_stay[new_stay == ''] <- '0 days'

on_stay <- data.frame(as.character(claims_to_risk$RiskLevel),
                          as.character(new_stay))

colnames(on_stay) <- c("RiskLevel", "Stay")

df_stay <- as.data.frame(as.character(on_stay$Stay))

colnames(df_stay) <- c("Stay")

l_stay <- on_stay$Stay

l_risk <- on_stay$RiskLevel

count_stay <- aggregate(df_stay, by=list(l_stay, l_risk), FUN=length)

colnames(count_stay) <- c("Stay", "RiskLevel", "Count")

# check the total to make sure everything is present and accounted for.

n_stays <- sum(count_stay$Count)

n_missing <- n_claims - n_stays

print(paste("A Posteriori stay -- ", toString(n_missing), " are missing."))

post_stay <- merge(count_stay, risk_count, by="RiskLevel")

post_stay$Prob <- post_stay$Count / post_stay$RiskCount

post_stay$Label <- paste(post_stay$Stay, post_stay$RiskLevel, sep="|")

# reorder the columns

post_stay <- post_stay[c("Label", "Stay", "RiskLevel", "Count", "RiskCount", "Prob")]


### ------------- Condition on Primary Condition Group --------------


on_pcg <- data.frame(as.character(claims_to_risk$RiskLevel),
                      as.character(claims_to_risk$PrimaryConditionGroup))

colnames(on_pcg) <- c("RiskLevel", "pcg")

df_pcg <- as.data.frame(as.character(on_pcg$pcg))

colnames(df_pcg) <- c("pcg")

l_pcg <- on_pcg$pcg

l_risk <- on_pcg$RiskLevel

count_pcg <- aggregate(df_pcg, by=list(l_pcg, l_risk), FUN=length)

colnames(count_pcg) <- c("pcg", "RiskLevel", "Count")

# check the total to make sure everything is present and accounted for.

n_pcgs <- sum(count_pcg$Count)

n_missing <- n_claims - n_pcgs

print(paste("A Posteriori pcg -- ", toString(n_missing), " are missing."))

post_pcg <- merge(count_pcg, risk_count, by="RiskLevel")

post_pcg$Prob <- post_pcg$Count / post_pcg$RiskCount

post_pcg$Label <- paste(post_pcg$pcg, post_pcg$RiskLevel, sep="|")

# reorder the columns

post_pcg <- post_pcg[c("Label", "pcg", "RiskLevel", "Count", "RiskCount", "Prob")]

### ---------------------- Write Results ----------------------------

write.csv(a_priori, file=path_out_a_priori)
write.csv(post_char, file=path_out_charlson)
write.csv(post_stay, file=path_out_stay)
write.csv(post_pcg, file=path_out_pcg)


### ----------------------- End of File -----------------------------

