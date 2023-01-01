library(tidyr)
library(stringr)
library(dplyr)

setwd("C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420")

acs_16 <- read.csv("./ACS_2016_Processed.csv")[, -1]

nc = ncol(acs_16)
cn = colnames(acs_16)

for (i in 1 : nc) {
  if (grepl("Margin.of.Error", cn[i], fixed = T)) {
    acs_16 = acs_16[, !(colnames(acs_16) == cn[i])]
  }
}

nc = ncol(acs_16)
cn = colnames(acs_16)

wa = c()

for (i in 1 : nc) {
  if (grepl("White.alone..Estimate", cn[i], fixed = T)) {
    wa = append(wa, i)
  }
}

wp = data.frame(matrix(nrow = 1, ncol = length(wa) + 1))
wp[1, 1] = "prop_white"

counter = 2

for (i in wa) {
  wp[1, counter] = acs_16[1, i]
  wp[1, counter] = gsub(",", "", wp[1, counter])
  counter = counter + 1
}

for (i in 1 : nc) {
  if (grepl("White.alone..Estimate", cn[i], fixed = T)) {
    acs_16 = acs_16[, !(colnames(acs_16) == cn[i])]
  }
}

nc = ncol(acs_16)
nr = nrow(acs_16)
cn = colnames(acs_16)

for (i in 2 : nc) {
  cn[i] = gsub("..Total.population..Estimate", "", cn[i])
  for (k in 1 : nr) {
    if (grepl("%", acs_16[k, i], fixed = T)) {
      acs_16[k, i] = gsub("%", "", acs_16[k, i])
    }
    if (grepl(",", acs_16[k, i], fixed = T)) {
      acs_16[k, i] = gsub(",", "", acs_16[k, i])
    }
  }
}

colnames(acs_16) = cn

for (i in 1 : nr) {
  acs_16[i, 1] = gsub("Â", "", acs_16[i, 1])
  acs_16[i, 1] = gsub(" ", "", acs_16[i, 1])
}

colnames(wp) = colnames(acs_16)
acs_16 = rbind(acs_16, wp)

acs16 = acs_16 %>%
  pivot_longer(!Label..Grouping., names_to = "counties", values_to = "stats") %>%
  pivot_wider(names_from = Label..Grouping., values_from = stats)

for (i in 1 : length(acs16$`Total population`)) {
  tp = as.integer(acs16$`Total population`[i]) / 100
  
  acs16$prop_white[i] = as.integer(acs16$prop_white[i]) / tp
  acs16$Native[i] = as.integer(acs16$Native[i]) / tp
  acs16$`Foreign born; not a U.S. citizen`[i] = as.integer(acs16$`Foreign born; not a U.S. citizen`[i]) / as.integer(acs16$`Foreign born`[i])
  acs16$`Foreign born; naturalized U.S. citizen`[i] = as.integer(acs16$`Foreign born; naturalized U.S. citizen`[i]) / as.integer(acs16$`Foreign born`[i])
  acs16$`Foreign born`[i] = as.integer(acs16$`Foreign born`[i]) / tp
}

colnames(acs16)[colnames(acs16) == "Native"] = paste("Native", "(%)")
colnames(acs16)[colnames(acs16) == "Foreign born"] = paste("Foreign born", "(%)")
colnames(acs16)[colnames(acs16) == "Foreign born; not a U.S. citizen"] = paste("Foreign born; not a U.S. citizen", "(%)")
colnames(acs16)[colnames(acs16) == "Foreign born; naturalized U.S. citizen"] = paste("Foreign born; naturalized U.S. citizen", "(%)")

fips <- read.csv("./County_FIPS_Cleaned.csv")

rn = c()

for (i in 1 : length(acs16$counties)) {
  for (k in 1 : length(fips$county)){
    
    if (grepl(fips$county[k], acs16$counties[i], ignore.case = T)) {
      n = as.integer(fips$fips[k])
      
      if (nchar(n) == 4) {
        n = paste("0", n, sep = "")
      }
      
      rn = append(rn, n)
      break
    }
  }
}

tmp = data.frame("fips" = rn, "counties" = acs16$counties,
                 "tot_pop" = acs16$`Total population`,
                 "prop_white" = acs16$`prop_white`)

rst = acs16[, -(1 : 2)]
rst = rst[, colnames(rst)[colnames(rst) != 'prop_white']]

acs16_o = bind_cols(tmp, rst)

acs16_o$fips = as.integer(acs16_o$fips)

house_results_16 <- read.csv("./2016_HOUSE_precinct_general.csv")

useful_results_16 = subset(house_results_16, county_fips %in% acs16_o$fips)
useful_results_16_d = subset(useful_results_16, party %in% c("democratic", "working families", "Democratic-Farmer-Labor", "democrat, independent", "Dem"))
useful_results_16_r = subset(useful_results_16, party %in% c("republican", "independent, republican", "Rep"))

dem_results = c()
rep_results = c()
prop_dem = c()
fips = c()
rmcnt = c()
rmin = c()

toofew = c()
onlyone = c()

for (i in 1 : length(acs16_o$counties)) {
  dem_vote = 0
  rep_vote = 0
  
  if (acs16_o$fips[i] %in% useful_results_16_d$county_fips && acs16_o$fips[i] %in% useful_results_16_r$county_fips){
    temp_d = subset(useful_results_16_d, useful_results_16_d$county_fips == acs16_o$fips[i])
    temp_r = subset(useful_results_16_r, useful_results_16_r$county_fips == acs16_o$fips[i])
    
    for (k in 1 : length(temp_d$county_fips)) {
      dem_vote = dem_vote + temp_d$votes[k]
    }
    
    for (k in 1 : length(temp_r$county_fips)) {
      rep_vote = rep_vote + temp_r$votes[k]
    }
    
    if (dem_vote < 1000 | rep_vote < 1000) {
      rmcnt = append(rmcnt, acs16_o$counties[i])
      rmin = append(rmin, i)
      
      toofew = append(toofew, acs16_o$counties[i])
      
    } else {
      dem_results = append(dem_results, dem_vote)
      rep_results = append(rep_results, rep_vote)
      
      prop_dem = append(prop_dem, (dem_vote / (dem_vote + rep_vote)))
      
      fips = append(fips, acs16_o$fips[i])
      
    }
  } else {
    rmcnt = append(rmcnt, acs16_o$counties[i])
    rmin = append(rmin, i)
    
    onlyone = append(onlyone, acs16_o$counties[i])
  }
}

rmin_na = c()

for (k in 3 : ncol(acs16_o)) {
  acs16_o[, k] = signif(as.numeric(acs16_o[, k], 3))
  if (NA %in% acs16_o[,k]) {
    print(k)
    print(sum(is.na(acs16_o[,k])))
    
    for (i in 1 : sum(is.na(acs16_o[, k]))) {
      if (!(acs16_o$counties[is.na(acs16_o[, k])][i] %in% rmcnt)) {
        cnt = acs16_o$counties[is.na(acs16_o[, k])]
        rmcnt = append(rmcnt, cnt)
        rmin_na = append(rmin_na, as.integer(row.names(acs16_o)[is.na(acs16_o[, k])]))
      }
    }
  }
}

rmin = append(rmin, rmin_na)
if (!is.null(rmin_na)) {
  prop_dem = prop_dem[-rmin_na]
}


acs16_nl = acs16_o[-rmin, -1]

acs16_nl = acs16_nl %>%
  pivot_longer(cols = c(2 : ncol(acs16_nl)), names_to = "variables", values_to = "values") %>%
  pivot_wider(names_from = counties, values_from = values)

acs16_nl = acs16_nl[, -1]

for (i in 1 : length(colnames(acs16_nl))) {
  colnames(acs16_nl)[i] = paste(colnames(acs16_nl)[i], "16", sep = "_")
}

results16 = data.frame("prop_dem" = prop_dem)

ncol(acs16_nl)
nrow(results16)

write.csv(acs16_nl, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/acs_2016_cleaned.csv", row.names = F)
write.csv(results16, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/2016_election_targets.csv", row.names = F)
