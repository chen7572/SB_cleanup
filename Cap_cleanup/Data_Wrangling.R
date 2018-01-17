library(Amelia)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(randomForest)
library(ROCR)

######
# Variables used in this exercise:
# eq: downloaded earthquake data
# eq_date: convert the date in eq from character to date
# country_eq: earthquake data reordered by the countries
# gdp: gdp data from 1960 to 2016, downloaded from World Bank
# world_gdp: gdp data with last 5 rows removed
# gdp_tidy: gdp data gathered by Year and GPD value for that year
# gdp_order: remove the Series.code column in gdp data and arrange data by country
# gdp_spread: gdp data spreaded into columns of four different types of gdp values (constant 2010 US$, current US$, constant 2010 US$, and current US$)
# popul: population data from 1960 to 2016, downloaded from World bank
# world_popu: population data with last 5 rows removed
# popu_tidy: population data gathered by Year and GDP value for that year
# popu_order: remove the Series.code column in population data and arrange data by country
# gdp_popu: gdp and population data joined together
# df_eq_gdp: earthquake, gdp and population data
# df1: data frame with earthquake data, total deaths, total damages in millions of dollars
# f1: saves the plot showing the total deaths VS earthquake magnitude
# f2: saves the plot showing the total damages VS earthquake magnitude
# plot1: saves the plot showing the total deaths VS focal depth, and the color represents earthquake magnitude
# plot2: saves the plot showing the total damage in millions of dollars VS focal depth, similarly, the color represents earthquake magnitude
# a1: saves the plot showing the total deaths VS GDP (in current US$)
# a2: saves the plot showing the total damages VS GDP (in current US$)
# a3: saves the plot showing the total deaths VS Population
# a4: saves the plot showing the total damages VS population
# model1: linear regression of total deaths with the following variables: focal depth, earthquake magnitude, intensity, latitude, longitude.
# model2: linear regression of total deaths with two additional variables, GDP in current US$ and population
# fit: random forest model of total deaths with the following variables: focal depth, earthquake magnitude, intensity, latitude, longitude, GDP in current US$ and population.
# df_NA_removed: data frame  with removed rows that contain NA in EQ_PRIMARY, FOCAL_DEPTH, LATITUDE, LONGITUDE, Population, and GDP_currentUSD.
# fit2: random forest model of a classification problem, death or no death.
# map.world: saves the world map with long, lat and region
# my_map: saves the world map plot
# data_67_77: saves the df_eq_gdp data between 1967 and 1976
# data_77_87: saves the df_eq_gdp data between 1977 and 1986
# data_87_97: saves the df_eq_gdp data between 1987 and 1996
# data_97_07: saves the df_eq_gdp data between 1997 and 2006
# data_07_17: saves the df_eq_gdp data between 2007 and 2017
# p1: map showing the distribution of earthquakes between 1967 and 1976, with total deaths plotted
# p2: map showing the distribution of earthquakes between 1977 and 1986, with total deaths plotted
# p3: map showing the distribution of earthquakes between 1987 and 1996, with total deaths plotted
# p4: map showing the distribution of earthquakes between 1997 and 2006, with total deaths plotted
# p5: map showing the distribution of earthquakes between 2007 and 2017, with total deaths plotted
# d1: map showing the distribution of earthquakes between 1967 and 1976, with total damages plotted
# d2: map showing the distribution of earthquakes between 1977 and 1986, with total damages
# plotted
# d3: map showing the distribution of earthquakes between 1987 and 1996, with total damages plotted
# d4: map showing the distribution of earthquakes between 1997 and 2006, with total damages plotted
# d5: map showing the distribution of earthquakes between 2007 and 2017, with total damages plotted

#######
# Read in and tidy the earthquake data 
eq <- read.table(file = 'results.tsv', sep = '\t', quote="\"",
                      header = TRUE, stringsAsFactors = F)

# visulaize the correlation of missing fields 
empty_cell <- is.na(eq)
cell_cor <- cor(empty_cell)
par( mar = c( 10, 10, 0.5, 0.5 ),cex = 0.5)
image(cell_cor, col = rev(heat.colors(12)), xaxt = "n", yaxt = "n")
axis( 1, at=seq(0,1,length.out=nrow( cell_cor ) ), 
      labels= rownames( cell_cor ), las= 2 )
axis( 2, at=seq(0,1,length.out=ncol( cell_cor ) ), 
      labels= colnames( cell_cor ), las= 2)

# Plot the missing field data using missmap
# missmap(eq,x.cex=0.5,y.cex=0.5,rank.order = F)

# convert NA values in HOUR, MINUTE,SECOND to 0 
# eq_data[c("HOUR","MINUTE","SECOND")][is.na(eq_data[c("HOUR","MINUTE","SECOND")])] <- 0 

# Convert Date from character to date in R
eq_date <- eq %>% unite(DATE,YEAR:DAY,sep="-",remove=T) 
eq_date$DATE <- as.Date(eq_date$DATE)

# Reorder the columns and arrange the data frame by country 
country_eq <- eq_date %>% 
  select(COUNTRY:LONGITUDE,FOCAL_DEPTH,REGION_CODE,DATE:SECOND,everything()) %>% 
  arrange(COUNTRY)


# Next let's import and tidy the world gdp data
gdp <- read.csv(file='world_gdp_Data.csv',sep=",",
                col.names=c("Series.Name","Series.Code",
                "Country.Name","Country.Code",1960:2016),
                stringsAsFactors = F,check.names = F)

# Last 5 rows of gdp data table are explanation, and are excluded in the analysis
world_gdp <- head(gdp,-5)
gdp_tidy <- world_gdp %>% gather("Year","Value",5:61)
gdp_order <- gdp_tidy %>% select(Country.Name,Country.Code,everything(),-Series.Code) %>% 
  arrange(Country.Name)
gdp_spread <- gdp_order %>% spread(key=Series.Name,value = Value)
  
# Last let's tidy the population data 
popul <- read.csv(file="world_population_Data.csv",sep=",",
                       col.names=c("Series.Name","Series.Code",
                                   "Country.Name","Country.Code",1960:2016),
                  stringsAsFactors = F,check.names = F)

world_popu <- head(popul,-5)
popu_tidy <- world_popu %>% gather("Year","Population",5:61)
popu_order <- popu_tidy %>% arrange(Country.Name) %>% 
  select(Country.Name,Country.Code,Year,everything(),-Series.Code,-Series.Name)

# Add population in the gdp data table as a column.
gdp_popu <- left_join(gdp_spread,popu_order)

# Now the Fun Part: Perform data analysis 
gdp_popu[gdp_popu == ".."] = NA

# 1. Total deaths/damage history plot
df1 <- select(country_eq,DATE,TOTAL_DEATHS,TOTAL_DAMAGE_MILLIONS_DOLLARS)
df1 <- gather(df1, "LOSS","VALUE",2:3)

log1psum <- function(y)  {log1p(sum(y))}

ggplot(df1, aes(x = format(DATE,"%Y"), y = VALUE, fill = LOSS)) +
  facet_wrap( ~ LOSS, scale = "free_y",ncol = 1) +
  stat_summary(fun.y=log1psum, geom="line", aes(group=1)) +
  theme(axis.text.x = element_text(angle = 90),legend.position = "none") +
  scale_x_discrete("Year") +
  scale_y_continuous("Log(Loss)")
  
#ggplot(country_eq, aes(x= format(DATE,"%Y"), y = TOTAL_DEATHS)) +
#  stat_summary(fun.y=sum,geom="bar",fill = "red",alpha = 0.2) + 
#  theme(axis.text.x = element_text(angle = 90)) +
#  scale_x_discrete("Year")

#ggplot(country_eq, aes(x= format(DATE,"%Y"), y = log(TOTAL_DAMAGE_MILLIONS_DOLLARS))) +
#  stat_summary(fun.y=sum,geom="bar") + 
#  theme(axis.text.x = element_text(angle = 90)) +
#  scale_x_discrete("Year")

# 2. Total deaths VS Earthquake magnitude
ggplot(country_eq,aes(x=EQ_PRIMARY, y = log1p(TOTAL_DEATHS))) +
  geom_jitter() 

# 3. Total deaths VS Focal depth
ggplot(country_eq,aes(x=FOCAL_DEPTH, y = log1p(TOTAL_DEATHS),color = EQ_PRIMARY)) +
  geom_jitter() +
  scale_color_gradientn(colors=rainbow(4)) +
  scale_x_continuous(limits = c(0, 300))

# 4. Total damage VS Focal depth 
ggplot(country_eq,aes(x=FOCAL_DEPTH, y = log(TOTAL_DAMAGE_MILLIONS_DOLLARS),
                      color = EQ_PRIMARY)) +
  geom_jitter() +
  scale_color_gradientn(colors=rainbow(4)) +
  scale_x_continuous(limits = c(0, 300))

# 5. Total Deaths VS Total Damage 
ggplot(country_eq,aes(x=log(TOTAL_DAMAGE_MILLIONS_DOLLARS),y=log(TOTAL_DEATHS))) + 
  geom_point()


# Combine the earthquake data and the GDP/Population Data
colnames(gdp_popu)[1] <- "COUNTRY"
gdp_popu$COUNTRY <- toupper(gdp_popu$COUNTRY)
country_eq$Year <- format(country_eq$DATE,"%Y")
df_eq_gdp <- left_join(country_eq,gdp_popu)

# convert the NA cells of total deaths and total damages to 0. 

# df_eq_gdp$ANY_DEATHS <- pmax(df_eq_gdp$TOTAL_DEATHS, 1)

df_eq_gdp$TOTAL_DEATHS[is.na(df_eq_gdp$TOTAL_DEATHS)] = 0
df_eq_gdp$TOTAL_DAMAGE_MILLIONS_DOLLARS[is.na(df_eq_gdp$TOTAL_DAMAGE_MILLIONS_DOLLARS)] = 0

df_eq_gdp$DEATHS_EXIST <- 0
df_eq_gdp$DEATHS_EXIST[df_eq_gdp$TOTAL_DEATHS > 0 ] <- 1

df_eq_gdp$DAMAGE_EXIST <- 0
df_eq_gdp$DAMAGE_EXIST[df_eq_gdp$TOTAL_DAMAGE_MILLIONS_DOLLARS >0 ] <- 1

df_eq_gdp$DEATHS_EXIST <- as.factor(df_eq_gdp$DEATHS_EXIST)
df_eq_gdp$Year <- format(df_eq_gdp$DATE,"%Y")
  
colnames(df_eq_gdp)[48] <- "GDP_constant2010USD"
colnames(df_eq_gdp)[49] <- "GDP_currentUSD"
colnames(df_eq_gdp)[50] <- "GDP_percap_2010USD"
colnames(df_eq_gdp)[51] <- "GDP_percap_currentUSD"

# 6. Total Deaths VS GDP per capita
ggplot(df_eq_gdp,aes(x = as.numeric(GDP_percap_currentUSD),y=log(TOTAL_DEATHS))) +
  geom_jitter() +
  scale_x_continuous("GDP per capita current $US",limits = c(0,60000))

# 7. Total Deaths VS GDP 
ggplot(df_eq_gdp,aes(x = as.numeric(GDP_currentUSD),y=log(TOTAL_DEATHS))) +
  geom_jitter() +
  scale_x_continuous("GDP current $US")

# 8. Total Deaths VS Population
ggplot(df_eq_gdp,aes(x = as.numeric(Population),y=log(TOTAL_DEATHS))) +
  geom_jitter() +
  scale_x_continuous("Country Population")

# Random tests that check the data
tail(sort(df_eq_gdp$TOTAL_DEATHS),5)
df_eq_gdp[which.max(df_eq_gdp$TOTAL_DEATHS),]
df_eq_gdp[which(df_eq_gdp$TOTAL_DEATHS == 76213) ,]
df_eq_gdp[which(df_eq_gdp$FOCAL_DEPTH >= 200 & df_eq_gdp$FOCAL_DEPTH <= 300) ,]

write.csv(country_eq, "country_eq.csv")
write.csv(df_eq_gdp, "earthquake_GDP.csv")

# Regression Analysis
df_eq_gdp$GDP_constant2010USD <- as.numeric(df_eq_gdp$GDP_constant2010USD)
df_eq_gdp$GDP_currentUSD <- as.numeric(df_eq_gdp$GDP_currentUSD)
df_eq_gdp$Population <- as.numeric(df_eq_gdp$Population)

subset_eq <- subset(df_eq_gdp, select = c("COUNTRY","FOCAL_DEPTH","EQ_PRIMARY", "INTENSITY", 
                                          "TOTAL_MISSING", "TOTAL_DEATHS", 
                                          "TOTAL_INJURIES", "TOTAL_DAMAGE_MILLIONS_DOLLARS"))
cor(subset_eq, use = "complete.obs")


# Regression Analysis
df_eq_gdp$GDP_constant2010USD <- as.numeric(df_eq_gdp$GDP_constant2010USD)
df_eq_gdp$GDP_currentUSD <- as.numeric(df_eq_gdp$GDP_currentUSD)
df_eq_gdp$Population <- as.numeric(df_eq_gdp$Population)

model1 <- lm(TOTAL_DEATHS ~ FOCAL_DEPTH + EQ_PRIMARY + INTENSITY + LATITUDE + LONGITUDE,
             data = df_eq_gdp)
summary(model1)

model2 <- lm(TOTAL_DEATHS ~ FOCAL_DEPTH + EQ_PRIMARY + INTENSITY + LATITUDE + LONGITUDE
             + GDP_currentUSD + Population,data = df_eq_gdp)
summary(model2)


map.world <- map_data(map = "world")
my_map <- ggplot(map.world, aes(x = long, y = lat, group = group))
my_map <- my_map + geom_path(size = 0.2)
my_map

data_67_77 <- filter(df_eq_gdp, Year < 1977 & Year >= 1967)
data_77_87 <- filter(df_eq_gdp, Year < 1987 & Year >= 1977)
data_87_97 <- filter(df_eq_gdp, Year < 1997 & Year >= 1987)
data_97_07 <- filter(df_eq_gdp, Year < 2007 & Year >= 1997)
data_07_17 <- filter(df_eq_gdp, Year <= 2017 & Year >= 2007)

p1 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log(TOTAL_DEATHS+1)),
                      data = data_67_77, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
p1 <- p1 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
           scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
p1 <- p1 + labs(title = "1967 - 1977")

p2 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log(TOTAL_DEATHS+1)),
                          data = data_77_87, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
p2 <- p2 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
p2 <- p2 + labs(title = "1977 - 1987")

p3 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log(TOTAL_DEATHS+1)),
                          data = data_87_97, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
p3 <- p3 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
p3 <- p3 + labs(title = "1987 - 1997")
p3
p4 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log(TOTAL_DEATHS+1)),
                          data = data_97_07, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
p4 <- p4 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
p4 <- p4 + labs(title = "1997 - 2007")

p5 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log(TOTAL_DEATHS+1)),
                          data = data_07_17, shape=21, stroke=0.5, alpha = 0.6, color="black", inherit.aes = FALSE)
p5 <- p5 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + 
  theme(legend.title = element_text(size = 8),legend.box = "horizontal", 
        legend.position = c(1.75, 0.5), plot.title = element_text(hjust = 0.5))
p5 <- p5 + labs(title = "2007 - 2017" )

#grid.arrange(p1, p2, p3, p4, p5, ncol = 3)
fig <- arrangeGrob(p1, p2, p3, p4, p5, ncol = 3)
ggsave(file = "eq_death_plot.pdf", fig, width = 10, height = 6)


# Plot the total damages every 10 years
d1 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log1p(TOTAL_DAMAGE_MILLIONS_DOLLARS)),
                          data = data_67_77, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
d1 <- d1 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
d1 <- d1 + labs(title = "1967 - 1977")

d2 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log1p(TOTAL_DAMAGE_MILLIONS_DOLLARS)),
                          data = data_77_87, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
d2 <- d2 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
d2 <- d2 + labs(title = "1977 - 1987")

d3 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log1p(TOTAL_DAMAGE_MILLIONS_DOLLARS)),
                          data = data_87_97, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
d3 <- d3 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
d3 <- d3 + labs(title = "1987 - 1997")

d4 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log1p(TOTAL_DAMAGE_MILLIONS_DOLLARS)),
                          data = data_97_07, shape=21, stroke=0.5, alpha = 0.6,color="black", inherit.aes = FALSE)
d4 <- d4 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + theme(legend.position="none", plot.title = element_text(hjust = 0.5))
d4 <- d4 + labs(title = "1997 - 2007")

d5 <- my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, size = EQ_PRIMARY,  fill= log1p(TOTAL_DAMAGE_MILLIONS_DOLLARS)),
                          data = data_07_17, shape=21, stroke=0.5, alpha = 0.6, color="black", inherit.aes = FALSE)
d5 <- d5 + scale_fill_continuous(low='yellow', high='red',na.value = NA) + 
  scale_size(range = c(0.2,3)) + 
  theme(legend.title = element_text(size = 8),legend.box = "horizontal", 
        legend.position = c(1.5, 0.5), plot.title = element_text(hjust = 0.5))
d5 <- d5 + labs(title = "2007 - 2017", fill = "log(tot. damages)")

fig2 <- arrangeGrob(d1, d2, d3, d4, d5, ncol = 3)
ggsave(file = "eq_damage_plot.pdf", fig2, width = 10, height = 6)


set.seed(130)
fit <- randomForest(TOTAL_DEATHS ~ FOCAL_DEPTH + EQ_PRIMARY + INTENSITY + 
                      LATITUDE + LONGITUDE + GDP_currentUSD + Population,
                    data = df_eq_gdp,
                    importance = TRUE,
                    na.action = na.omit,
                    ntree = 2000)
varImpPlot(fit,type=1)
fit

df_NA_removed <- df_eq_gdp[complete.cases(df_eq_gdp[,c(4:6, 14, 52,49)]),]
set.seed(130)
fit2 <- randomForest(DEATHS_EXIST ~ EQ_PRIMARY +
                      FOCAL_DEPTH + LATITUDE + LONGITUDE + Population + GDP_currentUSD,
                    data = df_NA_removed,
                    importance = TRUE,
                    ntree = 1000)
varImpPlot(fit2)

# take a look at how the model fits
fit2

# Plot AUC 
predictions <- as.vector(fit2$votes[,2])
pred <- prediction(predictions, df_NA_removed$DEATHS_EXIST)

perf_AUC <- performance(pred,"auc")
AUC <- perf_AUC@y.values[[1]]
perf_ROC <- performance(pred,"tpr","fpr")
plot(perf_ROC, main="ROC plot", colorize=TRUE,print.cutoffs.at = seq(0,1,by = 0.25),text.adj = c(-0.4,0.8))
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

# Plot accuracy 
pred_acc <- performance(pred,"acc")
plot(pred_acc)

ind = which.max(slot(pred_acc, "y.values")[[1]] )
acc = slot(pred_acc, "y.values")[[1]][ind]
cutoff = slot(pred_acc, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

# Plot F1 score 
pred_f <- performance(pred,"f")
plot(pred_f)

cutoff = 0.500
ind = which(slot(pred_f, "x.values")[[1]] == cutoff )
f_measure = slot(pred_f, "y.values")[[1]][ind]
print(c(f_measure=f_measure , cutoff = cutoff))



# Predictions if deaths exist, based on Latitude and Longitude. 
newdata <- df_NA_removed %>% with(expand.grid(LATITUDE=seq(min(LATITUDE), max(LATITUDE), by = 2),
                                              LONGITUDE=seq(min(LONGITUDE), max(LONGITUDE), by = 2), Population=mean(Population), 
                                              EQ_PRIMARY = 7, GDP_currentUSD=mean(GDP_currentUSD), FOCAL_DEPTH=mean(FOCAL_DEPTH)))
newdata$Predicted_Deaths <- predict(fit2, newdata=newdata)  

map.world <- map_data(map = "world")
my_map <- ggplot(map.world, aes(x = long, y = lat, group = group))
my_map <- my_map + geom_path(size = 0.2)

my_map + geom_point(aes(x = LONGITUDE, y = LATITUDE, color = Predicted_Deaths),
                    data = newdata, size = 1, alpha = 0.2, inherit.aes = FALSE) 


