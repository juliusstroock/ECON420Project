library(tidyr)
library(stringr)
library(dplyr)

setwd("C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420")

acs_18 <- read.csv("./ACS_2018_Processed_a.csv")[, -1]

nc = ncol(acs_18)
cn = colnames(acs_18)

for (i in 1 : nc) {
  if (grepl("Margin.of.Error", cn[i], fixed = T)) {
    acs_18 = acs_18[, !(colnames(acs_18) == cn[i])]
  }
}

nc = ncol(acs_18)
cn = colnames(acs_18)

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
  wp[1, counter] = acs_18[1, i]
  wp[1, counter] = gsub(",", "", wp[1, counter])
  counter = counter + 1
}

for (i in 1 : nc) {
  if (grepl("White.alone..Estimate", cn[i], fixed = T)) {
    acs_18 = acs_18[, !(colnames(acs_18) == cn[i])]
  }
}

nc = ncol(acs_18)
nr = nrow(acs_18)
cn = colnames(acs_18)

for (i in 2 : nc) {
  cn[i] = gsub("..Total.population..Estimate", "", cn[i])
  for (k in 1 : nr) {
    if (grepl("%", acs_18[k, i], fixed = T)) {
      acs_18[k, i] = gsub("%", "", acs_18[k, i])
    }
    if (grepl(",", acs_18[k, i], fixed = T)) {
      acs_18[k, i] = gsub(",", "", acs_18[k, i])
    }
  }
}

colnames(acs_18) = cn

for (i in 1 : nr) {
  acs_18[i, 1] = gsub("Â", "", acs_18[i, 1])
  acs_18[i, 1] = gsub(" ", "", acs_18[i, 1])
}

colnames(wp) = colnames(acs_18)
acs_18 = rbind(acs_18, wp)

acs18 = acs_18 %>%
  pivot_longer(!Label..Grouping., names_to = "counties", values_to = "stats") %>%
  pivot_wider(names_from = Label..Grouping., values_from = stats)

for (i in 1 : length(acs18$`Total population`)) {
  tp = as.integer(acs18$`Total population`[i]) / 100
  
  acs18$prop_white[i] = as.integer(acs18$prop_white[i]) / tp
  acs18$Native[i] = as.integer(acs18$Native[i]) / tp
  acs18$`Foreign born; not a U.S. citizen`[i] = as.integer(acs18$`Foreign born; not a U.S. citizen`[i]) / as.integer(acs18$`Foreign born`[i])
  acs18$`Foreign born; naturalized U.S. citizen`[i] = as.integer(acs18$`Foreign born; naturalized U.S. citizen`[i]) / as.integer(acs18$`Foreign born`[i])
  acs18$`Foreign born`[i] = as.integer(acs18$`Foreign born`[i]) / tp
}

colnames(acs18)[colnames(acs18) == "Native"] = paste("Native", "(%)")
colnames(acs18)[colnames(acs18) == "Foreign born"] = paste("Foreign born", "(%)")
colnames(acs18)[colnames(acs18) == "Foreign born; not a U.S. citizen"] = paste("Foreign born; not a U.S. citizen", "(%)")
colnames(acs18)[colnames(acs18) == "Foreign born; naturalized U.S. citizen"] = paste("Foreign born; naturalized U.S. citizen", "(%)")

fips <- read.csv("./County_FIPS_Cleaned.csv")

rn = c()

for (i in 1 : length(acs18$counties)) {
  for (k in 1 : length(fips$county)){
    
    if (grepl(fips$county[k], acs18$counties[i], ignore.case = T)) {
      n = as.integer(fips$fips[k])
      
      if (nchar(n) == 4) {
        n = paste("0", n, sep = "")
      }
      
      rn = append(rn, n)
      break
    }
  }
}

tmp = data.frame("fips" = rn, "counties" = acs18$counties,
                 "tot_pop" = acs18$`Total population`,
                 "prop_white" = acs18$`prop_white`)

rst = acs18[, -(1 : 2)]
rst = rst[, colnames(rst)[colnames(rst) != 'prop_white']]

acs18_o = bind_cols(tmp, rst)


house_results_18 <- read.csv("./2018_HOUSE_precinct_general.csv")

useful_results_18 = subset(house_results_18, county_fips %in% acs18_o$fips)
useful_results_18_d = subset(useful_results_18, party_simplified == "DEMOCRAT")
useful_results_18_r = subset(useful_results_18, party_simplified == "REPUBLICAN")

dem_results = c()
rep_results = c()
prop_dem = c()
fips = c()
rmcnt = c()
rmin = c()

for (i in 1 : length(acs18_o$counties)) {
  dem_vote = 0
  rep_vote = 0
  
  if (acs18_o$fips[i] %in% useful_results_18_d$county_fips && acs18_o$fips[i] %in% useful_results_18_r$county_fips){
    temp_d = subset(useful_results_18_d, useful_results_18_d$county_fips == acs18_o$fips[i])
    temp_r = subset(useful_results_18_r, useful_results_18_r$county_fips == acs18_o$fips[i])
    
    for (k in 1 : length(temp_d$county_fips)) {
      dem_vote = dem_vote + temp_d$votes[k]
    }
    
    for (k in 1 : length(temp_r$county_fips)) {
      rep_vote = rep_vote + temp_r$votes[k]
    }
    
    if (dem_vote < 1000 | rep_vote < 1000) {
      rmcnt = append(rmcnt, acs18_o$counties[i])
      rmin = append(rmin, i)
      
    } else {
      dem_results = append(dem_results, dem_vote)
      rep_results = append(rep_results, rep_vote)
      
      prop_dem = append(prop_dem, (dem_vote / (dem_vote + rep_vote)))
      
      fips = append(fips, acs18_o$fips[i])
      
    }
  } else {
    rmcnt = append(rmcnt, acs18_o$counties[i])
    rmin = append(rmin, i)
  }
}

rmin_na = c()

for (k in 3 : ncol(acs18_o)) {
  acs18_o[, k] = signif(as.numeric(acs18_o[, k], 3))
  if (NA %in% acs18_o[,k]) {
    print(k)
    print(sum(is.na(acs18_o[,k])))
    
    if (!(acs18_o$counties[is.na(acs18_o[, k])] %in% rmcnt)) {
      cnt = acs18_o$counties[is.na(acs18_o[, k])]
      rmcnt = append(rmcnt, cnt)
      rmin_na = append(rmin_na, as.integer(row.names(acs18_o)[is.na(acs18_o[, k])]))
    }
  }
}

rmin = append(rmin, rmin_na)
if (!is.null(rmin_na)) {
  prop_dem = prop_dem[-rmin_na]
}

acs18_nl = acs18_o[-rmin, -1]

acs18_nl = acs18_nl %>%
  pivot_longer(cols = c(2 : ncol(acs18_nl)), names_to = "variables", values_to = "values") %>%
  pivot_wider(names_from = counties, values_from = values)

acs18_nl = acs18_nl[, -1]

for (i in 1 : length(colnames(acs18_nl))) {
  colnames(acs18_nl)[i] = paste(colnames(acs18_nl)[i], "18", sep = "_")
}

results18 = data.frame("prop_dem" = prop_dem)

ncol(acs18_nl)
nrow(results18)

write.csv(acs18_nl, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/acs_2018_cleaned.csv", row.names = F)
write.csv(results18, "C:/Users/juliu/OneDrive/Desktop/Classes/Fall 2022/ECON 420/2018_election_targets.csv", row.names = F)
