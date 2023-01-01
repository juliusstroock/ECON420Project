acs_2016_cleaned = read.csv("./acs_2016_cleaned.csv")
acs_2018_cleaned = read.csv("./acs_2018_cleaned.csv")
acs_2021_cleaned = read.csv("./acs_2021_cleaned.csv")

election_targets_16 = read.csv("./2016_election_targets.csv")
election_targets_18 = read.csv("./2018_election_targets.csv")
election_targets_20 = read.csv("./2020_election_targets.csv")

acs_cleaned = cbind(acs_2016_cleaned, acs_2018_cleaned, acs_2021_cleaned)
election_targets = rbind(election_targets_16, election_targets_18, election_targets_20)

write.csv(acs_cleaned, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/acs_cleaned.csv", row.names = F)
write.csv(election_targets, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/election_targets.csv", row.names = F)
