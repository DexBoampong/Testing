###Session 3:intoduction to dplyr
## Data analysis using tidy datasets
attributes(iris)

rownames(iris)<-paste("Row", 1:150)

ggplot(iris, aes(x=Species,
                 y=Sepal.Length))+
  geom_bar(stat = "identity", fill= "red")


  
hist(mtcars$mpg)

summary(mtcars$mpg)

attributes(mtcars)
ggplot(mtcars, aes(x=factor(vs),
                 y=mpg))+
  geom_boxplot(aes(color=factor(vs)))
#####or lets pull it out############

mympg<-mtcars$mpg
myvs<-factor(mtcars$vs)
mydata<-data.frame(myvs=myvs, mympg=mympg)
ggplot(mydata, aes(x=myvs,
                   y=mympg))+
  geom_boxplot()


attributes(myvs)


remove(mtcars)
mtcars
##moto cars
?mtcars
summary(mtcars)
###gives more like the 5 num summarry
getwd()
View(mtcars)

str(mtcars)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rafalib)
library(readxl)
library(haven)




glimpse(mtcars)
length(mtcars)
head(mtcars, n=10)
##gives first 6 rows
length(mtcars$mpg)
tail(mtcars, n=9)
plot(mtcars)

?co#mgp, y=mtcars#hp, method= "spearman")
###talking about correlation, i will learn it first.

####
head(mtcars)
dim(mtcars)
## remember again that, in a 2d data frame first is a row and second is column

length(mtcars)
remove(mtcars)

nrow(mtcars)
ncol(mtcars)
###this two above are same as saying dim(mtcars)

####
colnames(mtcars)
rownames(mtcars)         


##### $ notation does not work on rows
mtcars[c("Maxda","Rx4",  "Wag"), ]



# Create a vector of row numbers for the car models you want to select
selected_rows <- mtcars[c(1, 6, 8), ]

# Print the selected rows
print(selected_rows)

###u can select the rows by position.



##
?mtcars

###does mgp differ based on engine
boxplot(formula= mpg ~ vs,  data=mtcars)
View(mtcars)
summary(mtcars$mpg)
boxplot(mtcars$mpg)
str(mtcars$mp)

View(mtcars$mpg)

lin
t.test(formula= mpg ~vs,  data=mtcars)
?t.test
  
###


table(mtcars$gear)
class(mtcars$gear)
gear_data<- table(mtcars$gear)
attributes(gear_data)
dim(gear_data)

v<- mtcars$gear


factor(mtcars$gear)
j<- factor(mtcars$gear)
j
fact
table(j)
attributes(j)
table(factor(mtcars$gear))

?aov

boxplot(formula= mpg ~gear,  data=mtcars)
##nb this was done behind the scenes, it picked the mpg and split
## and split them based on diff gears(grouping) and applied a plot func and combined them,
###thats what we se

mpg.model<- aov(formula= mpg ~ factor(gear),  data=mtcars)
summary(mpg.model)
mpg.model

str(mtcars)

TukeyHSD(mpg.model)


### exercise : do horse power vrs carburetors(carb)
boxplot(formula= hp ~vs, data=mtcars)
hp.model<- aov(formula= hp ~ factor(carb),  data=mtcars)
summary(hp.model)

###plot mgp vs hp
plot(x=mtcars$hp, y=mtcars$mpg,
     pch=19)

plot(mpg ~ hp,data = mtcars,
     pch =19, col= gear)

#### average horsepowers for cars with diff gears
mean(mtcars$hp) 

is.gearEq3 <- mtcars$gear==3
mode(is.gearEq3)
##when this was runned, we have a logical vect with gear=3(cases)

mtcars$gear==3

length(is.gearEq3)
mtcars.gear3<- mtcars[ is.gearEq3, ]
mtcars.gear3

dim(mtcars.gear3)
View(mtcars.gear3)

mean(mtcars.gear3$hp)

##the above things done is tedious but there are functions
###split-apply-combine--concept(these fuction will help u do am in a single step)

?aggregate
## this is a base R FUNCTION. BASE R,
##In R, the term "base R" refers to the core set of functions, 
##data structures, and libraries that are included with the R programming language by default. 
###These fundamental components provide essential tools for data manipulation, analysis, and visualization. 
###Base R is the foundation upon which other packages and extensions are built.

aggregate(hp ~  gear, data=mtcars,
          FUN = mean )
##if we want to find standard deviation
aggregate(hp ~  gear, data=mtcars,
          FUN = sd )

###if we want to do sorting(much better in dplyr)

##below is done with basr R
##sort hp
order(mtcars$hp)
mtcars[order(mtcars$hp), ]
View(mtcars[order(mtcars$hp), ])


##sortgear,carb

mtcars[order(mtcars$gear, -mtcars$carb), ]
###if you want descending, just put minus sign
mtcars[order(mtcars$gear, -mtcars$carb), ]
###this sorting is done using the base R, WE WILL USE THE DPLYR SOON

###TIDYVERSE
##its highly stuctured
###underlying philosophy, it provides grammar for coding
##main 5 verbs,
## in dpyly a fuctions name must match as close as the role.

##to use, load it first

library(dplyr)
library(tidyverse)
mtcars


###first we want to arrange our dataset "mtcars" with assending gears and descending carbunators
arrange(mtcars, gear, desc(carb))

### the above operation done using tidy/dplyr,first argument is always the data, u tell it how to arrange it.,
### if you want ascending, we dont say anything, we say for only desc

##next is select. select is for a column, lets say hp and mgp

##the old way,(base R way)
mtcars[ , c("hp" , "mpg")]


###now, with dplyr
select(mtcars, hp, mpg)

###you can do a negative selection
select(mtcars,)
select(mtcars, -hp)
mtwdhp <- mtcars %>%
  select(-hp)
mtwdhp
##next word is filter. this when you want to make a conditional subsetting
###get all cars with gear3
##lets start with the base R way

mtcars[mtcars$gear ==3, ]

##lets use the easier dplyr.##nb filter is for CONDITIONAL SUBSETTINFG IN  rows

filter(mtcars, gear ==3)

##suppose we want to do conditional
##lets say gear=3 and hp>200 (we want to do two comparison)
##lets start with base R below

mtcars[ mtcars$gear == 3 & mtcars$hp > 200, ]

filter(mtcars, gear == 3, hp > 200)

mtcars[3, ]
rownames(mtcars)


##next is summarize
##its same as aggregate

remove(mtcars)

###mutate() ##change observations and mostly used to add new one(additional columns)

###ratio of mpg to hp
##lets start with base R
mtcars$mpg_hp <- mtcars$mpg / mtcars$hp

mutate(mtcars, mpg / hp)
##above only give result , but u can give it a different name

mutate(mtcars, mpg_hp.ratio = mpg / hp)

##u can do multiple mutations also
mutate(mtcars,
       mpg_hp.ratio = mpg / hp,
       mpg_wt = mpg / wt)


###glimpse is just like structure of base R
glimpse(mtcars)


####Pipe operator, pipe analysisworkflow


## %>% %>% (ctrl+shift+m)

log10(mtcars$hp)

mtcars$hp %>% log10
##above means pass mtcars#hp to log10(make it input of log10), it will give same response

sin(log10(mtcars$hp))

mtcars$hp %>% log10 %>% sin

###### lets select two cars
remove(mtcars)

mtcars.new <- mtcars
mtcars.new
glimpse(mtcars.new)
rownames(mtcars.new)

mtcars.new$carname <- rownames(mtcars)
mtcars.new$carname

head(mtcars.new, n=10)
remove(mtcars)
### we want two cars named mazda, we want hp and mgp
mtcars.new[rownames(mtcars), ]

mazda.cars <- filter(mtcars.new, carname == "Mazda RX4")

mazda.cars
##now mazda.cars its now an R object(which we can select our hp and mpg from)

mazda.cars <- select(mazda.cars, hp, mpg)
mazda.cars


###below we use pipe operator
mtcars.new %>% 
  ###only choose car named Mazda RX4
  filter(carname == "Mazda RX4") %>% 
  ### ONLY KEEP hp and mgp
  select(hp, mpg)

mtcars.new %>% filter(carname == "Mazda RX4") %>% select(hp, mpg)
select(filter(mtcars.new, carname == "Mazda RX4"),hp , mpg)

###anoda eg we want info on hp and mpg, but for cars with gear 3 and wt>2
### first we start with data always
### we also want to sort with hp
mtcars %>% 
  filter(gear == 3) %>% 
  filter(wt > 2) %>% 
  select(hp,mpg, gear, wt) %>% 
  arrange(hp, desc(mpg))



### split-apply-combine for dplyr
###group_by + summarize


mtcars %>% 
  group_by(gear) %>% 
  summarise(meanHP = mean(hp))



mtcars %>% 
  group_by(gear) -> data.final

data.final
print(data.final, n = 30)
 
mtcars %>% head(n = 5)
mtcars[1:5, ]

mtcars

print(mtcars)
head(mtcars, n = 5)
?tibble
##above shows data frame for dplyr funct

### eg i want average of all groups

mtcars %>% 
  group_by(gear) %>% 
  summarise_all(mean)
###this gives mean value for each data but split for gear
summarise_all(mtcars, mean)

##two ways of saving data


mtcars %>% 
  group_by(gear) %>% 
  summarise_all(mean) ->
  data.final

###above is the preffered way(we use the assignment operator in oppposite dxn)


###last execise, we want cars with hp / mpg (ratio) greater than 9
###what are the gears, and make a table/pie

remove(mtcars)


mtcars %>% 
  mutate(hp_mpg = hp / mpg) %>% 
  filter(hp_mpg > 9) %>% 
  select(gear) %>% 
  table %>% 
  pie -> data.final



###trying on github









  


    
