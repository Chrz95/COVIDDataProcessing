library("data.table")
library("ggplot2")
library(xtable)
library(tidyr)
library(lubridate)
library(dplyr)
library(Hmisc)
library(devtools)
library(ggmap)
library(maps)
library(mapdata)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

#Load dataset
#devtools::install_github("RamiKrispin/coronavirus",force = TRUE)
library(coronavirus)
data = covid19_vaccine

# Remove samples that don't have fully_vaccinated_ratio or partially_vaccinated_ratio or population
data = subset(data, !is.na(people_fully_vaccinated))
data = subset(data, !is.na(people_partially_vaccinated))
data = subset(data, !is.na(population))
print(max(data$date))
print(min(data$date))

# Remove info on provinces
data = subset(data, select = -c(province_state,uid,iso2,iso3,combined_key,fips)) 

# Create two new variables
data$fully_vaccinated_ratio = round((data$people_fully_vaccinated / data$population)*100, digits = 1)
data$partially_vaccinated_ratio = round((data$people_partially_vaccinated / data$population)*100, digits = 1) 

# Check number of NA values per attribute
for (i in colnames(data)){
  NA_vals <- sum(is.na(data[[i]]))
  if (NA_vals > 0)
  {
    print(i)
    print(NA_vals)
  }
}

# Convert to data table
data = setDT(data) 
head(data)

# Vaccinated ratio per country (descending)
data_per_country = data[ , .SD[which.max(date)], by = country_region] 

ans <- data_per_country[order(-fully_vaccinated_ratio)]
ans <- head(ans,10)
print(xtable(ans[, .(country_region, fully_vaccinated_ratio,population)], type = "latex"), file = "first_country_full.tex")

df2 <- tidyr::pivot_longer(as.data.frame(ans), cols=c('fully_vaccinated_ratio', 'partially_vaccinated_ratio'), names_to='variable', 
                           values_to="value")

ggplot(df2, aes(x=country_region, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + ggtitle("Fully/partially vaccinated per country")+ xlab("Country") + ylab("Percentage %")  +scale_fill_discrete(name = "Vaccination", labels = c("Fully", "Partially"))

ans <- data_per_country[order(-partially_vaccinated_ratio)]
ans <- head(ans,10)
print(xtable(ans[, .(country_region, partially_vaccinated_ratio,population)], type = "latex"), file = "first_country_part.tex")

# Vaccinated ratio per country (ascending)
data_per_country = data[ , .SD[which.max(date)], by = country_region] 

ans <- data_per_country[order(fully_vaccinated_ratio)]
ans <- head(ans,10)
print(xtable(ans[, .(country_region, fully_vaccinated_ratio,population)], type = "latex"), file = "last_country_full.tex")

df2 <- tidyr::pivot_longer(as.data.frame(ans), cols=c('fully_vaccinated_ratio', 'partially_vaccinated_ratio'), names_to='variable', 
                           values_to="value")

ggplot(df2, aes(x=country_region, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + ggtitle("Fully/partially vaccinated per country")+ xlab("Country") + ylab("Percentage %")  +scale_fill_discrete(name = "Vaccination", labels = c("Fully", "Partially"))

ans <- data_per_country[order(partially_vaccinated_ratio)]
ans <- head(ans,10)
print(xtable(ans[, .(country_region, partially_vaccinated_ratio,population)], type = "latex"), file = "last_country_part.tex")

# Maps
world <- ne_countries(scale = "medium", returnclass = "sf") 
world[c(227),"admin"] <- "US"

names(world)[names(world) == 'admin'] <- "country_region"
total <- merge(x = world, y = as.data.frame(data_per_country), by = "country_region", all = TRUE)

ggplot(data = total) +
  geom_sf(data = total,mapping = aes(fill = partially_vaccinated_ratio)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(data_per_country$country_region)), " countries)")) + theme(axis.title = element_text(size = 20)) + theme(plot.title = element_text(size=20)) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=11)) 

ggplot(data = total) +
  geom_sf(data = total,mapping = aes(fill = fully_vaccinated_ratio)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(data_per_country$country_region)), " countries)"))+ theme(axis.title = element_text(size = 20)) + theme(plot.title = element_text(size=20)) +
  theme(legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=11)) 


# Vaccinated ratio per continent
data_per_country = data[ , .SD[which.max(date)], by = country_region] 
data_per_continent = data_per_country[ ,list(people_fully_vaccinated=sum(people_fully_vaccinated) ,people_partially_vaccinated=sum(people_partially_vaccinated),population=sum(population) ), by=continent_name]
data_per_continent = subset(data_per_continent, !is.na(continent_name))

# Create two new variables
data_per_continent$fully_vaccinated_ratio = round((data_per_continent$people_fully_vaccinated / data_per_continent$population)*100, digits = 1)
data_per_continent$partially_vaccinated_ratio = round((data_per_continent$people_partially_vaccinated / data_per_continent$population)*100, digits = 1) 
head(data_per_continent)

ans <- data_per_continent[order(-fully_vaccinated_ratio)]
print(xtable(ans[, .(continent_name, fully_vaccinated_ratio,population)], type = "latex"), file = "first_continent_full.tex")

df2 <- tidyr::pivot_longer(as.data.frame(ans), cols=c('fully_vaccinated_ratio', 'partially_vaccinated_ratio'), names_to='variable', values_to="value")

ggplot(df2, aes(x=continent_name, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge') + ggtitle("Fully/partially vaccinated per continent")+ xlab("Continent") + ylab("Percentage %")  +scale_fill_discrete(name = "Vaccination", labels = c("Fully", "Partially"))

ans <- data_per_continent[order(-partially_vaccinated_ratio)]
print(xtable(ans[, .(continent_name, partially_vaccinated_ratio,population)], type = "latex"), file = "first_continent_part.tex")

# Time series (Continent)
data_per_continent = data[ ,list(people_fully_vaccinated=sum(people_fully_vaccinated) ,people_partially_vaccinated=sum(people_partially_vaccinated),population=sum(population) ), by=.(date,continent_name)]
data_per_continent = subset(data_per_continent, !is.na(continent_name))
data_per_continent$fully_vaccinated_ratio = round((data_per_continent$people_fully_vaccinated / data_per_continent$population)*100, digits = 1)
data_per_continent$partially_vaccinated_ratio = round((data_per_continent$people_partially_vaccinated / data_per_continent$population)*100, digits = 1) 

Europe = data_per_continent[continent_name == "Europe"][order(date)]
Europe = Europe %>% 
  group_by(month = lubridate::floor_date(date, "month"))  %>% 
  summarise_all(last)

Europe_dt = as.data.frame(Europe)
#latex(Europe_dt[, c("date", "partially_vaccinated_ratio","fully_vaccinated_ratio")], file="Europe.tex")   

North_America = data_per_continent[continent_name == "North America"][order(date)]
North_America = North_America %>% 
  group_by(month = lubridate::floor_date(date, "month"))  %>% 
  summarise_all(last)

North_America_dt = as.data.frame(North_America)
#latex(North_America_dt[, c("date", "partially_vaccinated_ratio","fully_vaccinated_ratio")], file="North_America.tex")   

# plot
theme_set(theme_bw())
# labels and breaks for X axis text
lbls <- paste0(month.abb[lubridate::month(Europe$date)], " ",
               lubridate::year(Europe$date))
brks <- Europe$date
brks <- brks[-length(brks)]
lbls<- lbls[-length(lbls)]

ggplot(Europe, aes(x=date)) +
  geom_line(aes(y=partially_vaccinated_ratio)) +
  labs(title="Europe",
       subtitle="Partially vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid


ggplot(Europe, aes(x=date)) +
  geom_line(aes(y=fully_vaccinated_ratio)) +
  labs(title="Europe",
       subtitle="Fully vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

theme_set(theme_bw())

# labels and breaks for X axis text
lbls <- paste0(month.abb[lubridate::month(North_America$date)], " ",
               lubridate::year(North_America$date))
brks <- North_America$date
brks <- brks[-length(brks)]
lbls<- lbls[-length(lbls)]

ggplot(North_America, aes(x=date)) +
  geom_line(aes(y=partially_vaccinated_ratio)) +
  labs(title="North America",
       subtitle="Partially vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

ggplot(North_America, aes(x=date)) +
  geom_line(aes(y=fully_vaccinated_ratio)) +
  labs(title="North America",
       subtitle="Fully vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

# Time series (Country)
data_per_country = data[ ,list(people_fully_vaccinated=sum(people_fully_vaccinated) ,people_partially_vaccinated=sum(people_partially_vaccinated),population=sum(population) ), by=.(date,country_region)]
data_per_country$fully_vaccinated_ratio = round((data_per_country$people_fully_vaccinated / data_per_country$population)*100, digits = 1)
data_per_country$partially_vaccinated_ratio = round((data_per_country$people_partially_vaccinated / data_per_country$population)*100, digits = 1) 

Greece = data_per_country[country_region == "Greece"][order(date)]
Greece = Greece %>% 
  group_by(month = lubridate::floor_date(date, "month"))  %>% 
  summarise_all(last)

Malta = data_per_country[country_region == "Malta"][order(date)]
Malta = Malta %>% 
  group_by(month = lubridate::floor_date(date, "month"))  %>% 
  summarise_all(last)

Greece_dt = as.data.frame(Greece)
Malta_dt = as.data.frame(Malta)

#latex(Greece_dt[, c("date", "partially_vaccinated_ratio","fully_vaccinated_ratio")], file="Greece.tex")   
#latex(Malta_dt[, c("date", "partially_vaccinated_ratio","fully_vaccinated_ratio")], file="Malta.tex")   

# plot
theme_set(theme_bw())
# labels and breaks for X axis text
lbls <- paste0(month.abb[lubridate::month(Greece$date)], " ",
               lubridate::year(Greece$date))
brks <- Greece$date

ggplot(Greece, aes(x=date)) +
  geom_line(aes(y=partially_vaccinated_ratio)) +
  labs(title="Greece",
       subtitle="Partially vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid


ggplot(Greece, aes(x=date)) +
  geom_line(aes(y=fully_vaccinated_ratio)) +
  labs(title="Greece",
       subtitle="Fully vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

theme_set(theme_bw())


# labels and breaks for X axis text
lbls <- paste0(month.abb[lubridate::month(Malta$date)], " ",
               lubridate::year(Malta$date))
brks <- Malta$date

ggplot(Malta, aes(x=date)) +
  geom_line(aes(y=partially_vaccinated_ratio)) +
  labs(title="Malta",
       subtitle="Partially vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

ggplot(Malta, aes(x=date)) +
  geom_line(aes(y=fully_vaccinated_ratio)) +
  labs(title="Malta",
       subtitle="Fully vaccinated ratio",
       y="Percentage %") +  # title and caption
  scale_x_date(labels = lbls,
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid