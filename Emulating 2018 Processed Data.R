acs_pro_18 <- read.csv("./ACS_2018_Processed.csv")

for (i in 1 : length(acs_pro_18$Label..Grouping.)) {
  if (grepl("%", acs_pro_18$Jefferson.County..Alabama..Total.population..Estimate[i], fixed = T)) {
    acs_pro_18$Label..Grouping.[i] = paste(acs_pro_18$Label..Grouping.[i], "(%)")
  }
}

acs_raw_12 <- read.csv("./ACSSPP1Y2012.S0201-2022-12-31T001101.csv")
acs_raw_14 <- read.csv("./ACSSPP1Y2014.S0201-2022-12-31T000855.csv")
acs_raw_16 <- read.csv("./ACSSPP1Y2016.S0201-2022-12-31T001021.csv")
acs_raw_21 <- read.csv("./ACSSPP1Y2021.S0201-2022-12-31T034739.csv")

write.csv(acs_pro_18, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/ACS_2018_Processed_a.csv", row.names = F)

acs_pro_12 = acs_raw_12[c(acs_pro_18$ï..index) ,]
acs_pro_12$Label..Grouping. = acs_pro_18$Label..Grouping.
write.csv(acs_pro_12, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/ACS_2012_Processed.csv", row.names = F)

acs_pro_14 = acs_raw_14[c(acs_pro_18$ï..index) ,]
acs_pro_14$Label..Grouping. = acs_pro_18$Label..Grouping.
write.csv(acs_pro_14, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/ACS_2014_Processed.csv", row.names = F)

acs_pro_16 = acs_raw_16[c(acs_pro_18$ï..index) ,]
acs_pro_16$Label..Grouping. = acs_pro_18$Label..Grouping.
write.csv(acs_pro_16, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/ACS_2016_Processed.csv", row.names = F)

acs_pro_21 = acs_raw_21[c(acs_pro_18$ï..index) ,]
acs_pro_21$Label..Grouping. = acs_pro_18$Label..Grouping.
write.csv(acs_pro_21, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/ACS_2021_Processed.csv", row.names = F)
