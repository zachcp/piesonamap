#nycounty <- load(url("http://dl.dropbox.com/u/61803503/nycounty.RData"))


pops <-  "http://data.newsday.com/long-island/data/census/county-population-estimates-2012/" %>%
  readHTMLTable(which=1) %>%
  tbl_df() %>%
  select(1:2) %>%
  setNames(c("region", "population")) %>%
  mutate(
    population = {as.numeric(gsub("\\D", "", population))},
    region = tolower(gsub("\\s+[Cc]ounty|\\.", "", region)),
    #weight = ((1 - (1/(1 + exp(population/sum(population)))))/11)
    weight = exp(population/sum(population)),
    weight = sqrt(weight/sum(weight))/3
  )

devtools::use_data(pops,overwrite = TRUE)
