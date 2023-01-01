library(tidyr)
library(stringr)
library(dplyr)

setwd("C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420")

acs_21 <- read.csv("./ACS_2021_Processed.csv")[, -1]

nc = ncol(acs_21)
cn = colnames(acs_21)

for (i in 1 : nc) {
  if (grepl("Margin.of.Error", cn[i], fixed = T)) {
    acs_21 = acs_21[, !(colnames(acs_21) == cn[i])]
  }
}

nc = ncol(acs_21)
cn = colnames(acs_21)

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
  wp[1, counter] = acs_21[1, i]
  wp[1, counter] = gsub(",", "", wp[1, counter])
  counter = counter + 1
}

for (i in 1 : nc) {
  if (grepl("White.alone..Estimate", cn[i], fixed = T)) {
    acs_21 = acs_21[, !(colnames(acs_21) == cn[i])]
  }
}

nc = ncol(acs_21)
nr = nrow(acs_21)
cn = colnames(acs_21)

for (i in 2 : nc) {
  cn[i] = gsub("..Total.population..Estimate", "", cn[i])
  for (k in 1 : nr) {
    if (grepl("%", acs_21[k, i], fixed = T)) {
      acs_21[k, i] = gsub("%", "", acs_21[k, i])
    }
    if (grepl(",", acs_21[k, i], fixed = T)) {
      acs_21[k, i] = gsub(",", "", acs_21[k, i])
    }
  }
}

colnames(acs_21) = cn

for (i in 1 : nr) {
  acs_21[i, 1] = gsub("Â", "", acs_21[i, 1])
  acs_21[i, 1] = gsub(" ", "", acs_21[i, 1])
}

colnames(wp) = colnames(acs_21)
acs_21 = rbind(acs_21, wp)

acs21 = acs_21 %>%
  pivot_longer(!Label..Grouping., names_to = "counties", values_to = "stats") %>%
  pivot_wider(names_from = Label..Grouping., values_from = stats)

for (i in 1 : length(acs21$`Total population`)) {
  tp = as.integer(acs21$`Total population`[i]) / 100
  
  acs21$prop_white[i] = as.integer(acs21$prop_white[i]) / tp
  acs21$Native[i] = as.integer(acs21$Native[i]) / tp
  acs21$`Foreign born; not a U.S. citizen`[i] = as.integer(acs21$`Foreign born; not a U.S. citizen`[i]) / as.integer(acs21$`Foreign born`[i])
  acs21$`Foreign born; naturalized U.S. citizen`[i] = as.integer(acs21$`Foreign born; naturalized U.S. citizen`[i]) / as.integer(acs21$`Foreign born`[i])
  acs21$`Foreign born`[i] = as.integer(acs21$`Foreign born`[i]) / tp
}

colnames(acs21)[colnames(acs21) == "Native"] = paste("Native", "(%)")
colnames(acs21)[colnames(acs21) == "Foreign born"] = paste("Foreign born", "(%)")
colnames(acs21)[colnames(acs21) == "Foreign born; not a U.S. citizen"] = paste("Foreign born; not a U.S. citizen", "(%)")
colnames(acs21)[colnames(acs21) == "Foreign born; naturalized U.S. citizen"] = paste("Foreign born; naturalized U.S. citizen", "(%)")

fips <- read.csv("./County_FIPS_Cleaned.csv")

rn = c()

for (i in 1 : length(acs21$counties)) {
  for (k in 1 : length(fips$county)){
    
    if (grepl(fips$county[k], acs21$counties[i], ignore.case = T)) {
      n = as.integer(fips$fips[k])
      
      if (nchar(n) == 4) {
        n = paste("0", n, sep = "")
      }
      
      rn = append(rn, n)
      break
    }
  }
}

tmp = data.frame("fips" = rn, "counties" = acs21$counties,
                 "tot_pop" = acs21$`Total population`,
                 "prop_white" = acs21$`prop_white`)

rst = acs21[, -(1 : 2)]
rst = rst[, colnames(rst)[colnames(rst) != 'prop_white']]

acs21_o = bind_cols(tmp, rst)

acs21_o$fips = as.integer(acs21_o$fips)

house_results_20 <- read.csv("./2020_HOUSE_precinct_general.csv")

useful_results_20 = subset(house_results_20, county_fips %in% acs21_o$fips)
useful_results_20_d = subset(useful_results_20, party_detailed %in% c("DEMOCRAT", "DEMOCRATIC FARMER LABOR", "WORKING FAMILIES"))
useful_results_20_r = subset(useful_results_20, party_simplified == "REPUBLICAN")

dem_results = c()
rep_results = c()
prop_dem = c()
fips = c()
rmcnt = c()
rmin = c()

toofew = c()
onlyone = c()

for (i in 1 : length(acs21_o$counties)) {
  dem_vote = 0
  rep_vote = 0
  
  if (acs21_o$fips[i] %in% useful_results_20_d$county_fips && acs21_o$fips[i] %in% useful_results_20_r$county_fips){
    temp_d = subset(useful_results_20_d, useful_results_20_d$county_fips == acs21_o$fips[i])
    temp_r = subset(useful_results_20_r, useful_results_20_r$county_fips == acs21_o$fips[i])
    
    for (k in 1 : length(temp_d$county_fips)) {
      dem_vote = dem_vote + temp_d$votes[k]
    }
    
    for (k in 1 : length(temp_r$county_fips)) {
      rep_vote = rep_vote + temp_r$votes[k]
    }
    
    if (dem_vote < 1000 | rep_vote < 1000) {
      rmcnt = append(rmcnt, acs21_o$counties[i])
      rmin = append(rmin, i)
      
      toofew = append(toofew, acs21_o$counties[i])
      
    } else {
      dem_results = append(dem_results, dem_vote)
      rep_results = append(rep_results, rep_vote)
      
      prop_dem = append(prop_dem, (dem_vote / (dem_vote + rep_vote)))
      
      fips = append(fips, acs21_o$fips[i])
      
    }
  } else {
    rmcnt = append(rmcnt, acs21_o$counties[i])
    rmin = append(rmin, i)
    
    onlyone = append(onlyone, acs21_o$counties[i])
  }
}

rmin_na = c()

for (k in 3 : ncol(acs21_o)) {
  acs21_o[, k] = signif(as.numeric(acs21_o[, k], 3))
  if (NA %in% acs21_o[,k]) {
    print(k)
    print(sum(is.na(acs21_o[,k])))
    
    if (!(acs21_o$counties[is.na(acs21_o[, k])] %in% rmcnt)) {
      cnt = acs21_o$counties[is.na(acs21_o[, k])]
      rmcnt = append(rmcnt, cnt)
      rmin_na = append(rmin_na, as.integer(row.names(acs21_o)[is.na(acs21_o[, k])]))
    }
  }
}

rmin = append(rmin, rmin_na)
if (!is.null(rmin_na)) {
  prop_dem = prop_dem[-rmin_na]
}


acs21_nl = acs21_o[-rmin, -1]

acs21_nl = acs21_nl %>%
  pivot_longer(cols = c(2 : ncol(acs21_nl)), names_to = "variables", values_to = "values") %>%
  pivot_wider(names_from = counties, values_from = values)

acs21_nl = acs21_nl[, -1]

for (i in 1 : length(colnames(acs21_nl))) {
  colnames(acs21_nl)[i] = paste(colnames(acs21_nl)[i], "21", sep = "_")
}

results20 = data.frame("prop_dem" = prop_dem)

ncol(acs21_nl)
nrow(results20)

write.csv(acs21_nl, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/acs_2021_cleaned.csv", row.names = F)
write.csv(results20, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/2020_election_targets.csv", row.names = F)
