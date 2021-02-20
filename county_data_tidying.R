setwd("~/Desktop/Credit Default Prediction/R data")
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(tidyr)

# load datasets
train = read_csv("final_train_data_county.csv")
test = read_csv("final_test_data_county.csv")

# tidy test and train datasets
train$zip_code <- substr(train$zip_code, 0, 3)
test$zip_code <- substr(test$zip_code, 0, 3)
train$zip_code <- as.factor(as.character(train$zip_code))
test$zip_code <- as.factor(as.character(test$zip_code))
train$issue_d = year(train$issue_d)
test$issue_d = year(test$issue_d)

# tidy dataset that ties county to zip code
zip_county = read_csv("zip_county.csv")
zip_county$COUNTYNAME = str_replace_all(zip_county$COUNTYNAME, ' County', '')
zip_county$county = paste(zip_county$COUNTYNAME, zip_county$STATE, sep = ", ")
zip_county$zip_code <- substr(zip_county$ZIP, 0, 3)
zip_county = zip_county%>%select(zip_code, county)
zip_county$zip_code = as.factor(zip_county$zip_code)
zip_county = zip_county[!duplicated(zip_county),]

# tidy unemployment by county dataset
unemployment = read_csv("Unemployment_county.csv")
unemployment$area_name <- substr(unemployment$area_name, 1, nchar(unemployment$area_name)-11)
unemployment$county = paste(unemployment$area_name, unemployment$Stabr, sep = ", ")
unemployment_vars = colnames(unemployment)
for (i in 4:length(unemployment_vars)){
  print(unemployment_vars[i])
  var = str_sub(unemployment_vars[i], -4, -1)
  print(var)
  var = as.numeric(var)
  print(var)
  if (var < 2008){
    unemployment = unemployment%>%select(-c(unemployment_vars[i]))
  } else if (str_sub(unemployment_vars[i], 0, 17) != "Unemployment_rate"){
    unemployment = unemployment%>%select(-c(unemployment_vars[i]))
  }
}
unemployment = tail(unemployment, -2)
unemployment = unemployment%>%gather("Unemployment_rate_2008", "Unemployment_rate_2009",
                                     "Unemployment_rate_2010","Unemployment_rate_2011",
                                     "Unemployment_rate_2012","Unemployment_rate_2013",
                                     "Unemployment_rate_2014","Unemployment_rate_2015",
                                     "Unemployment_rate_2016","Unemployment_rate_2017",
                                     "Unemployment_rate_2018","Unemployment_rate_2019",
                                     key ="Year", value = "unemployment_rate")
unemployment$Year = str_sub(unemployment$Year, -4, -1)
unemployment = unemployment%>%select(Year, county, unemployment_rate)

# merge unemployment to zip_county
county = left_join(zip_county, unemployment, "county")

# tidy poverty dataset
poverty = read_csv("Poverty_county.csv")
poverty = poverty%>%select(Stabr, Area_name, PCTPOVALL_2018)
poverty$Area_name <- substr(poverty$Area_name, 0, nchar(poverty$Area_name)-7)
poverty$county = paste(poverty$Area_name, poverty$Stabr, sep = ", ")
poverty = tail(poverty, -2)
poverty = poverty%>%mutate(poverty_rate = PCTPOVALL_2018)%>%select(-c(Stabr, PCTPOVALL_2018))

county =  left_join(county, poverty, "county")
county = county%>%filter(Year != "2019") 

# tidy gdp 
gdp = read_csv("gdp_per_county.csv")
gdp = gdp%>%filter(Description == "All industry total")
gdp = tail(gdp, -2)
gdp = gdp%>%gather("2001", '2002','2003','2004',"2005",'2006','2007','2008','2009','2010','2011',
                   '2012','2013','2014','2015','2016','2017','2018', key = 'Year', value =  'gdp')
gdp = gdp%>%mutate(county = GeoName)%>%select(county, Year, gdp)
gdp = gdp%>%filter(as.numeric(Year) > 2007)

county = left_join(county, gdp, by = c("county", "Year"))

# tidy gdp growth
growth = read_csv("gdp_growth_county_year.csv")
growth = growth%>%filter(Description == "All industry total (percent change)")
growth = growth%>%gather('2002','2003','2004',"2005",'2006','2007','2008','2009','2010','2011',
                         '2012','2013','2014','2015','2016','2017','2018', key = 'Year', value =  'growth')
growth = growth%>%mutate(county = GeoName)%>%select(county, Year, growth)
growth = growth%>%filter(as.numeric(Year) > 2007)
county = left_join(county, growth, by = c("county", "Year"))

# economic profile
# population
economic = read_csv("economic_profile_county.csv")
population = economic%>%filter(Description == "Population (persons) 3/")
population = population%>%select(-c(GeoFIPS, LineCode, IndustryClassification, Region, TableName, Unit))
population$Description = as.factor(population$Description)
population = population%>%gather(colnames(population[3:52]), key = 'Year', value = 'Population')
population=  population%>%filter(as.numeric(Year) > 2007)


# per capita personal income
per_capita_net_earnings = economic%>%filter(Description == "Per capita net earnings 4/")
per_capita_net_earnings = per_capita_net_earnings%>%select(-c(GeoFIPS, LineCode, IndustryClassification, Region, TableName, Unit))
per_capita_net_earnings$Description = as.factor(per_capita_net_earnings$Description)
per_capita_net_earnings = per_capita_net_earnings%>%gather(colnames(per_capita_net_earnings[3:52]), key = 'Year', value = 'per_capita_net_earnings')
per_capita_net_earnings=  per_capita_net_earnings%>%filter(as.numeric(Year) > 2007)

# total employment
total_jobs = economic%>%filter(Description == "Total employment (number of jobs)")
total_jobs = total_jobs%>%select(-c(GeoFIPS, LineCode, IndustryClassification, Region, TableName, Unit))
total_jobs$Description = as.factor(total_jobs$Description)
total_jobs = total_jobs%>%gather(colnames(total_jobs[3:52]), key = 'Year', value = 'total_jobs')
total_jobs=  total_jobs%>%filter(as.numeric(Year) > 2007)

# average earnings per job
average_earnings = economic%>%filter(Description == "Average earnings per job (dollars)")
average_earnings = average_earnings%>%select(-c(GeoFIPS, LineCode, IndustryClassification, Region, TableName, Unit))
average_earnings$Description = as.factor(average_earnings$Description)
average_earnings = average_earnings%>%gather(colnames(average_earnings[3:52]), key = 'Year', value = 'average_earnings')
average_earnings=  average_earnings%>%filter(as.numeric(Year) > 2007)

# tidy education data 
edu = read_csv("Education_county.csv")
edu$`Area name` = str_replace_all(edu$`Area name`, ' County', '')
edu$county = paste(edu$`Area name`, edu$State, sep = ", ")
edu = edu[44:48]
edu = edu%>%mutate(less_hs = `Percent of adults with less than a high school diploma, 2014-18`, hs = `Percent of adults with a high school diploma only, 2014-18`, some_college = `Percent of adults completing some college or associate's degree, 2014-18`, bach = `Percent of adults with a bachelor's degree or higher, 2014-18`)
edu = dplyr::select(edu, c(county, hs, less_hs, some_college, bach))


# tidy crime county data
crime = read_csv("crime_county.csv")
crime$county = str_replace_all(crime$county_name, ' County', '')
crime = dplyr::select(crime, county, crime_rate_per_100000)

county = left_join(county, crime, by = "county")
county = left_join(county, edu, by = c("county"))
county = left_join(county, poverty, by = c("county"))
county = left_join(county, population, by = c("county"="GeoName", "Year"))
county = left_join(county, per_capita_net_earnings, by = c("county"="GeoName", "Year"))
county = left_join(county, total_jobs, by = c("county"="GeoName", "Year"))
county = left_join(county, average_earnings, by = c("county"="GeoName", "Year"))
# get rid of county variable and average everything out by three digit zip code
county = county%>%select(crime_rate_per_100000, zip_code,Year, unemployment_rate, gdp, growth, Population, per_capita_net_earnings, total_jobs, average_earnings, poverty_rate.x, hs, less_hs, bach, some_college)
county$growth = as.numeric(county$growth)
county$gdp = as.numeric(county$gdp)
county = county%>%group_by(zip_code, Year)%>%summarize(crime_rate = mean(crime_rate_per_100000, na.rm=T),gdp = mean(gdp, na.rm=T), 
                                                       unemployment_rate = mean(unemployment_rate, na.rm=T), 
                                                       gdp = mean(gdp, na.rm = T), growth = mean(growth, na.rm=T), 
                                                       Population = mean(Population, na.rm=T), 
                                                       per_capita_net_earnings = mean(per_capita_net_earnings, na.rm =T), 
                                                       total_jobs = mean(total_jobs, na.rm=T), 
                                                       average_earnings = mean(average_earnings,na.rm=T), 
                                                       hs = mean(hs, na.rm = T), poverty_rate = mean(poverty_rate.x, na.rm = T), 
                                                       bach = mean(bach, na.rm=T), less_hs = mean(less_hs, na.rm=T), some_college = mean(some_college, na.rm=T))

# divide gdp and total_jobs by population
county = county%>%mutate(gdp_per_capita = gdp/Population, total_jobs_per_capita = total_jobs/Population)

train$issue_d = as.factor(train$issue_d)
train = left_join(train, county, by = c("issue_d"= "Year", "zip_code"))
test$issue_d = as.factor(test$issue_d)
test = left_join(test, county, by = c("issue_d"= "Year", "zip_code"))





