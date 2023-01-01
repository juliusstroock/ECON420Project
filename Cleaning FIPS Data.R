fips <- read.csv("./County FIPS.csv")
state_fips <- read.csv("./State FIPS.csv")

colnames(fips) = c("fips", "county")
colnames(state_fips) = c("state", "fips")

for (i in 1 : length(fips$county)) {
  fips$county[i] = strsplit(fips$county[i], "\\[")[[1]][1]
  fips$county[i] = str_remove(fips$county[i], ", City and")
  if (fips$county[i] != "District of Columbia") {
    fips$county[i] = str_remove(fips$county[i], " of")
  }
  fips$county[i] = gsub("'", " ", fips$county[i])
  fips$county[i] = gsub("-", " ", fips$county[i])
  
  for (k in 1 : length(state_fips$state)) {
    if (nchar(fips$fips[i]) == 4 && nchar(state_fips$fips[k]) == 1) {
      if (substr(fips$fips[i], 1, 1) == state_fips$fips[k]) {
        fips$county[i] = paste(fips$county[i], state_fips$state[k], sep = "  ")
      }
    } else if (nchar(fips$fips[i]) == 5 && nchar(state_fips$fips[k]) == 2){
      if (substr(fips$fips[i], 1, 2) == state_fips$fips[k]) {
        fips$county[i] = paste(fips$county[i], state_fips$state[k], sep = "  ")
      }
    }
    
  }
  fips$county[i] = gsub(" ", ".", fips$county[i])
}

write.csv(fips, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/County_FIPS_Cleaned.csv", row.names = F)
