library(tidyverse)
library(ggplot2)
library(ggrepel)
library(gghighlight)



## Protein & Fat sources and Obesity Relationship ##

protein_supply <- read.csv("Protein_Supply_Quantity_Data.csv")
protein_supply

fat_supply <- read.csv("Fat_Supply_Quantity_Data.csv")
fat_supply

install.packages("gridExtra")
library("gridExtra")





## Protein and Fat ##

## Animal Products ##
p1 <- ggplot(protein_supply, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Protein Supply",
       x="From Animal Products (by %)",
       y="Obesity (%)")
p2 <- ggplot(fat_supply, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Fat Supply",
       x="From Animal Products (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)






## Animal Fats ##
p1 <- ggplot(protein_supply, aes(x=Animal.fats,y=Obesity)) +
  geom_point() +
  labs(title="Protein Supply",
       x="From Animal Fats (by %)",
       y="Obesity (%)")
p2 <- ggplot(fat_supply, aes(x=Animal.fats,y=Obesity)) +
  geom_point() +
  labs(title="Fat Supply",
       x="From Animal Fats (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)





## Cereals ##
p1 <- ggplot(protein_supply, aes(x=Cereals...Excluding.Beer,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Protein Supply",
       x="From Cereals (by %)",
       y="Obesity (%)")
p2 <- ggplot(fat_supply, aes(x=Cereals...Excluding.Beer,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Fat Supply",
       x="From Cereals (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)






## Meat ##
p1 <- ggplot(protein_supply, aes(x=Meat,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Protein Supply",
       x="From Meat (by %)",
       y="Obesity (%)")
p2 <- ggplot(fat_supply, aes(x=Meat,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Fat Supply",
       x="From Meat (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)






## Vegetal Products ##
p1 <- ggplot(protein_supply, aes(x=Vegetal.Products,y=Obesity)) +
  geom_point() +
  labs(title="Protein Supply",
       x="From Vegetal Products (by %)",
       y="Obesity (%)")
p2 <- ggplot(fat_supply, aes(x=Vegetal.Products,y=Obesity)) +
  geom_point() +
  labs(title="Fat Supply",
       x="From Vegetal Products (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)




## Dairy ##
p1 <- ggplot(protein_supply, aes(x=Milk...Excluding.Butter,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Protein Supply",
       x="From Dairy (by %)",
       y="Obesity (%)")
p2 <- ggplot(fat_supply, aes(x=Milk...Excluding.Butter,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Fat Supply",
       x="From Dairy (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)





## Calorie Intake sources and Obesity Relationships ##

kcal_info <- read.csv("Food_Supply_kcal_Data.csv")
kcal_info

kg_info <- read.csv("Food_Supply_Quantity_kg_Data.csv")

kg_info





## Kilo-calories and Total Weight ##

## Animal Products ##
p1 <- ggplot(kcal_info, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilocalories",
       x="From Animal Products (by %)",
       y="Obesity (%)")
p2 <- ggplot(kg_info, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilograms",
       x="From Animal Products (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)





## Cereals ##
p1 <- ggplot(kcal_info, aes(x=Cereals...Excluding.Beer,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilocalories",
       x="From Cereals (by %)",
       y="Obesity (%)")
p2 <- ggplot(kg_info, aes(x=Cereals...Excluding.Beer,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilograms",
       x="From Cereals (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)





## Dairy ##
p1 <- ggplot(kcal_info, aes(x=Milk...Excluding.Butter,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilocalories",
       x="From Dairy (by %)",
       y="Obesity (%)")
p2 <- ggplot(kg_info, aes(x=Milk...Excluding.Butter,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilograms",
       x="From Dairy (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)





## Sugar ##
p1 <- ggplot(kcal_info, aes(x=Sugar...Sweeteners,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilocalories",
       x="From Sugar (by %)",
       y="Obesity (%)")
p2 <- ggplot(kg_info, aes(x=Sugar...Sweeteners,y=Obesity)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Kilograms",
       x="From Sugar (by %)",
       y="Obesity (%)")
grid.arrange(p1,p2,ncol=2)





## General analysis ##

## Protein Supply ##

protein_avg <- protein_supply %>%
  select(Animal.fats,Animal.Products,Cereals...Excluding.Beer,
         Eggs, Fish..Seafood,Meat,Milk...Excluding.Butter,
         Vegetables, Vegetal.Products,SubRegion) %>%
  summarise(AnimalFat_avag=mean(Animal.fats),
            AnimalProduct_avg=mean(Animal.Products),
            Cereals_avg=mean(Cereals...Excluding.Beer),
            Seafood_avg=mean(Fish..Seafood),
            Meat_avg=mean(Meat),
            Dairy_avg=mean(Milk...Excluding.Butter),
            Vegetables_avg=mean(Vegetables),
            VegProd_avg=mean(Vegetal.Products))


protein_avg




## Fat Supply ##

fat_avg <- fat_supply %>%
  select(Animal.fats,Animal.Products,Cereals...Excluding.Beer,
         Eggs, Fish..Seafood,Meat,Milk...Excluding.Butter,
         Vegetables, Vegetal.Products,SubRegion) %>%
  summarise(AnimFat_avg=mean(Animal.fats),
            AnimalProduct_avg=mean(Animal.Products),
            Cereals_avg=mean(Cereals...Excluding.Beer),
            Seafood_avg=mean(Fish..Seafood),
            Meat_avg=mean(Meat),
            Dairy_avg=mean(Milk...Excluding.Butter),
            Vegetables_avg=mean(Vegetables),
            VegProd_avg=mean(Vegetal.Products))
fat_avg





## Kcal Intake by categories ##

kcal_avg <- kcal_info %>%
  select(Starchy.Roots,Animal.Products,Cereals...Excluding.Beer,
         Eggs, Fish..Seafood,Meat,Milk...Excluding.Butter,
         Vegetables, Vegetal.Products,Vegetable.Oils,Sugar...Sweeteners) %>%
  summarise(StarchyRoots_avg=mean(Starchy.Roots),
            AnimalProduct_avg=mean(Animal.Products),
            Cereals_avg=mean(Cereals...Excluding.Beer),
            Seafood_avg=mean(Fish..Seafood),
            Meat_avg=mean(Meat),
            Dairy_avg=mean(Milk...Excluding.Butter),
            Vegetables_avg=mean(Vegetables),
            VegProd_avg=mean(Vegetal.Products),
            VegOils_avg=mean(Vegetable.Oils),
            Sugar_avg=mean(Sugar...Sweeteners))

kcal_avg





## Intake by weight by categories ##

kg_avg <- kg_info %>%
  select(Starchy.Roots,Animal.Products,Cereals...Excluding.Beer,
         Eggs,Fruits...Excluding.Wine,Starchy.Roots,Meat,Milk...Excluding.Butter,
         Vegetables, Vegetal.Products, Sugar...Sweeteners) %>%
  summarise(StarchyRoots_avg=mean(Starchy.Roots),
            AnimalProduct_avg=mean(Animal.Products),
            Cereals_avg=mean(Cereals...Excluding.Beer),
            Fruits_avg=mean(Fruits...Excluding.Wine),
            Meat_avg=mean(Meat),
            Dairy_avg=mean(Milk...Excluding.Butter),
            Vegetables_avg=mean(Vegetables),
            VegProd_avg=mean(Vegetal.Products),
            Sugar_avg=mean(Sugar...Sweeteners))

kg_avg
kcal_avg
protein_avg
fat_avg





## Analyze all factors by Sub-Regions ##

## Animal Product and Protein,Fat ##

ggplot(protein_supply, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Protein Supply By Sub-Region",
       x="From Animal Products (%)",
       y="Obesity (%)")

ggplot(fat_supply, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply By Sub-Region",
       x="From Animal Products (%)",
       y="Obesity (%)")




## Animal Fat and Protein,Fat ##

ggplot(protein_supply, aes(x=Animal.fats,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Protein Supply from Animal Fat By Sub-Region",
       x="From Animal Fat (%)",
       y="Obesity (%)")

ggplot(fat_supply, aes(x=Animal.fats,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply from Animal Fat By Sub-Region",
       x="From Animal Fat (%)",
       y="Obesity (%)")




## Meat and Protein,Fat ##

ggplot(protein_supply, aes(x=Meat,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Protein Supply from Meat By Sub-Region",
       x="From Meat (%)",
       y="Obesity (%)")

ggplot(fat_supply, aes(x=Meat,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply from Meat By Sub-Region",
       x="From Meat (%)",
       y="Obesity (%)")





## Cereals and Protein,Fat ##

ggplot(protein_supply, aes(x=Cereals...Excluding.Beer,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Protein Supply from Cereals By Sub-Region",
       x="From Cereals (%)",
       y="Obesity (%)")

ggplot(fat_supply, aes(x=Cereals...Excluding.Beer,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply from Cereals By Sub-Region",
       x="From Cereals (%)",
       y="Obesity (%)")




## Vegetal Products and Protein,Fat ##

ggplot(protein_supply, aes(x=Vegetal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Protein Supply from Vegetal Products By Sub-Region",
       x="From Vegetal Products (%)",
       y="Obesity (%)")

ggplot(fat_supply, aes(x=Vegetal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply from Vegetal Products By Sub-Region",
       x="From Vegetal Products (%)",
       y="Obesity (%)")




## Vegetables and Protein,Fat ##

ggplot(protein_supply, aes(x=Vegetables,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Protein Supply from Vegetables By Sub-Region",
       x="From Vegetables (%)",
       y="Obesity (%)")

ggplot(fat_supply, aes(x=Vegetables,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply from Vegetables By Sub-Region",
       x="From Vegetables (%)",
       y="Obesity (%)")




## Vegetable Oil and Fat ##

ggplot(fat_supply, aes(x=Vegetable.Oils,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply from Vegetable Oils By Sub-Region")




## Seafood and Protein,Fat ##

ggplot(protein_supply, aes(x=Fish..Seafood,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Protein Supply from Seafood By Sub-Region")

ggplot(fat_supply, aes(x=Fish..Seafood,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Fat Supply from Seafood By Sub-Region")





## Kilo-calories and Weight Analysis ## 

## Animal Product and kcal, weight ##

ggplot(kcal_info, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Kilocalories from Animal Products by Region",
       x="From Animal Products (%)",
       y="Obesity (%)")

ggplot(kg_info, aes(x=Animal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Weight (kg) from Animal Products by Region",
       x="From Animal Products (%)",
       y="Obesity (%)")





## Meat and kcal, Weight ##

ggplot(kcal_info, aes(x=Meat,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Kilocalories from Meat by Region",
       x="From Meat (%)",
       y="Obesity(%)")

ggplot(kg_info, aes(x=Meat,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Weight from Meat by Region",
       x="From Meat (%)",
       y="Obesity (%)")




## Vegetal Products and kcal,weight ##

ggplot(kcal_info, aes(x=Vegetal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Kilocalories from Vegetal Products by Region",
       x="From Vegetal Products (%)",
       y="Obesity (%)")

ggplot(kg_info, aes(x=Vegetal.Products,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Weight from Vegetal Products by Region",
       x="From Vegetal Products (%)",
       y="Obesity (%)")




## Vegetables and kcal, weight ##

ggplot(kcal_info, aes(x=Vegetables,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Kilocalories from Vegetables by Region",
       x="From Vegetables (%)",
       y="Obesity (%)")

ggplot(kg_info, aes(x=Vegetables,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Weight from Vegetables by Region",
       x="From Vegetables (%)",
       y="Obesity (%)")




## Sugar and kcal, weight ##

ggplot(kcal_info, aes(x=Sugar...Sweeteners,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Kilocalories from Sugar by Region",
       x="From sweetener Sugar (%)",
       y="Obesity (%)")

ggplot(kg_info, aes(x=Sugar...Sweeteners,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Weight from Sugar by Region",
       x="From sweetener Sugar (%)",
       y="Obesity (%)")







## Relationship Between GDP and Obesity ##

ggplot(protein_supply, aes(x=GDP_per_capita,y=Obesity)) +
  geom_point() +
  labs(title="Obesity vs GDP per Capita")

ggplot(protein_supply, aes(x=GDP_per_capita,y=Obesity)) +
  geom_point() +
  facet_wrap(~SubRegion) +
  labs(title="Obesity vs GDP per Capita by Region")







## Protein Sources by Percentage ##

x <- colnames(protein_avg)
y <- c(0.1,21.04,19.14,3.34,9.6,6.01,1.71,28.95)

df_temp <- data.frame(x,y)
df_temp

ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Average Percentage of Protein Sources",
       x="Sources",y="%")

## Fat Sources by Percentage ##

x <- colnames(fat_avg)
y <- c(4.1,20.57,4.4,0.86,9.29,5.21,0.3,29.43)

df_temp <- data.frame(x,y)
df_temp

ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Average Percentage of Fat Sources",
       x="Sources",y="%")

## Kilo-calories Sources by Percentage ##

x <- colnames(kcal_avg)
y <- c(3.22,9.19,20.44,0.62,3.79,2.97,1.07,40.8,4.82,4.77)

df_temp <- data.frame(x,y)
df_temp

ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Average Percentage of Kilo-calories Sources",
       x="Sources",y="%")

## Weight of Sources in Kilograms by Percentage ##

x <- colnames(kg_avg)
y <- c(5.55,12.2,11.89,5.53,3.28,6.68,5.98,37.8,2.75)

df_temp <- data.frame(x,y)
df_temp

ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity') +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Average Percentage of Weight of Sources",
       x="Sources",y="%")







## Sources for Latin America comparison with Asia ##

## Protein ##

la_protein <- protein_supply[protein_supply$SubRegion %in% c("Caribbean","South America","Central America"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Animal_Fats=mean(Animal.fats),
            Cereals=mean(Cereals...Excluding.Beer),
            Eggs=mean(Eggs),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Pulses=mean(Pulses),
            Starches=mean(Starchy.Roots),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetables=mean(Vegetables))


x <- colnames(la_protein)
y <- c(23.74,0.05,16.95,1.39,2.87,1.1,12.47,6.17,3.36,1.26,26.26,1.29)
df_temp1 <- data.frame(x,y)
ggplot(df_temp1, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  gghighlight(x %in% c("Animal_Products","Cereals","Meat","Dairy")) +
  labs(title = "Protein Sources in Latin America",
       x="Sources",y="%")


southasia_protein <- protein_supply[protein_supply$SubRegion %in% c("South-Eastern Asia"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Animal_Fats=mean(Animal.fats),
            Cereals=mean(Cereals...Excluding.Beer),
            Eggs=mean(Eggs),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Pulses=mean(Pulses),
            Starches=mean(Starchy.Roots),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetables=mean(Vegetables))

w <- colnames(southasia_protein)
z <- c(18.90,0.1,22.2,1.28,7.4,0.79,8.05,1.28,1.74,0.53,31.09,2.02)

df_temp2 <- data.frame(w,z)
ggplot(df_temp2, aes(reorder(w,z),z)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  gghighlight(x %in% c("Animal_Products","Cereals","Meat","Dairy")) +
  labs(title = "Protein Sources in South-Eastern Asia",
       x="Sources",y="%")



eastasia_protein <- protein_supply[protein_supply$SubRegion %in% c("Eastern Asia"),] %>%
  group_by(SubRegion) %>%
  summarise(Animal_Products=mean(Animal.Products),
            Animal_Fats=mean(Animal.fats),
            Cereals=mean(Cereals...Excluding.Beer),
            Eggs=mean(Eggs),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Pulses=mean(Pulses),
            Starches=mean(Starchy.Roots),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetables=mean(Vegetables))

x <- colnames(eastasia_protein)
y <- c(29.45,0.08,12.7,2.1,5.9,0.33,14.33,4.5,0.32,0.55,20.55,2.7)
df_temp <- data.frame(x,y)
ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Protein Sources in Eastern Asia",
       x="Sources",y="%")






## Fat ##

la_fat <- fat_supply[fat_supply$SubRegion %in% c("Caribbean","South America","Central America"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Animal_Fats=mean(Animal.fats),
            Cereals=mean(Cereals...Excluding.Beer),
            Eggs=mean(Eggs),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetable_Oils=mean(Vegetable.Oils))

x <- colnames(la_fat)
y <- c(22.53,3.82,3.85,1.13,0.7,0.9,11.53,5.23,27.5,18.34)
df_temp3 <- data.frame(x,y)
ggplot(df_temp3, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  gghighlight(x %in% c("Cereals","Dairy")) +
  labs(title = "Fat Sources in Latin America",
       x="Sources",y="%")

eastasia_fat <- fat_supply[fat_supply$SubRegion %in% c("Eastern Asia"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Animal_Fats=mean(Animal.fats),
            Cereals=mean(Cereals...Excluding.Beer),
            Eggs=mean(Eggs),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetable_Oils=mean(Vegetable.Oils))

x <- colnames(eastasia_fat)
y <- c(26.86,3.78,2.26,1.84,1.55,0.16,15.4,3.9,23.1,16.5)
df_temp <- data.frame(x,y)
ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Fat Sources in Eastern Asia",
       x="Sources",y="%")

southasia_fat <- fat_supply[fat_supply$SubRegion %in% c("South-Eastern Asia"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Animal_Fats=mean(Animal.fats),
            Cereals=mean(Cereals...Excluding.Beer),
            Eggs=mean(Eggs),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetable_Oils=mean(Vegetable.Oils))

x <- colnames(southasia_fat)
y <- c(21.5,2.77,5.69,1.24,2.08,0.58,14.43,0.78,28.51,15.42)
df_temp4 <- data.frame(x,y)
ggplot(df_temp4, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  gghighlight(x %in% c("Cereals","Dairy")) +
  labs(title = "Fat Sources in South-Eastern Asia",
       x="Sources",y="%")





## Kilocalories ##

la_kcal <- kcal_info[kcal_info$SubRegion %in% c("Caribbean","South America","Central America"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Cereals=mean(Cereals...Excluding.Beer),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Starches=mean(Starchy.Roots),
            Sugar=mean(Sugar...Sweeteners),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetable_Oils=mean(Vegetable.Oils),
            Vegetables=mean(Vegetables))

x <- colnames(la_kcal)
y <- c(9.74,18.58,2.6,4.6,2.95,2.11,6.99,40.26,4.83,0.86)
df_temp5 <- data.frame(x,y)
ggplot(df_temp5, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  gghighlight(x %in% c("Animal_Products","Cereals","Sugar","Dairy")) +
  labs(title = "Kilo-calories Sources in Latin America",
       x="Sources",y="%")


eastasia_kcal <- kcal_info[kcal_info$SubRegion %in% c("Eastern Asia"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Cereals=mean(Cereals...Excluding.Beer),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Starches=mean(Starchy.Roots),
            Sugar=mean(Sugar...Sweeteners),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetable_Oils=mean(Vegetable.Oils),
            Vegetables=mean(Vegetables))

x <- colnames(eastasia_kcal)
y <- c(14.01,19.09,0.9,7.18,2.8,1.07,4.01,35.9,4.9,1.73)
df_temp <- data.frame(x,y)
ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Kilo-calories Sources in Eastern Asia",
       x="Sources",y="%")


southasia_kcal <- kcal_info[kcal_info$SubRegion %in% c("South-Eastern Asia"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Cereals=mean(Cereals...Excluding.Beer),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Starches=mean(Starchy.Roots),
            Sugar=mean(Sugar...Sweeteners),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetable_Oils=mean(Vegetable.Oils),
            Vegetables=mean(Vegetables))

x <- colnames(southasia_kcal)
y <- c(6.89,27.86,1.71,3.94,0.55,1.54,4.02,43.11,3.13,1.03)
df_temp6 <- data.frame(x,y)
ggplot(df_temp6, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  gghighlight(x %in% c("Animal_Products","Cereals","Sugar","Dairy")) +
  labs(title = "Kilo-calories Sources in South-Eastern Asia",
       x="Sources",y="%")




## Weight ##

la_kg <- kg_info[kg_info$SubRegion %in% c("Caribbean","South America","Central America"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Cereals=mean(Cereals...Excluding.Beer),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Starches=mean(Starchy.Roots),
            Sugar=mean(Sugar...Sweeteners),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetables=mean(Vegetables))

x <- colnames(la_kg)
y <- c(12.54,10.56,1.19,8.48,4.38,6.03,3.81,3.73,37.45,4.57)
df_temp <- data.frame(x,y)
ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Food Sources by Weight in Latin America",
       x="Sources",y="%")


eastasia_kg <- kg_info[kg_info$SubRegion %in% c("Eastern Asia"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Cereals=mean(Cereals...Excluding.Beer),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Starches=mean(Starchy.Roots),
            Sugar=mean(Sugar...Sweeteners),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetables=mean(Vegetables))

x <- colnames(eastasia_kg)
y <- c(16.6,10.74,2.68,2.96,5.71,5.96,1.92,2.31,33.38,9.4)
df_temp <- data.frame(x,y)
ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Food Sources by Weight in Eastern Asia",
       x="Sources",y="%")


southasia_kg <- kg_info[kg_info$SubRegion %in% c("South-Eastern Asia"),] %>%
  summarise(Animal_Products=mean(Animal.Products),
            Cereals=mean(Cereals...Excluding.Beer),
            Seafood=mean(Fish..Seafood),
            Fruits=mean(Fruits...Excluding.Wine),
            Meat=mean(Meat),
            Dairy=mean(Milk...Excluding.Butter),
            Starches=mean(Starchy.Roots),
            Sugar=mean(Sugar...Sweeteners),
            Vegetal_Products=mean(Vegetal.Products),
            Vegetables=mean(Vegetables))

x <- colnames(southasia_kg)
y <- c(7.86,20.06,3.08,5.52,3.11,0.82,2.84,2.58,42.14,6.21)
df_temp <- data.frame(x,y)
ggplot(df_temp, aes(reorder(x,y),y)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Food Sources by Weight in South-Eastern Asia",
       x="Sources",y="%")




## LA Obesity and GDP ##

la_stats <- kg_info[kg_info$SubRegion %in% c("South America", "Caribbean","Central America"),] %>%
  summarise(obesity_avg=mean(Obesity),
            gdp_capita_avg=mean(GDP_per_capita))

world_stats <- kg_info %>% summarise(obesity_avg=mean(Obesity),
                                     gdp_capita_avg=mean(GDP_per_capita))


la_stats



## Obesity in All Other Regions ##

regions_obesity <- kg_info %>%
  filter(!SubRegion %in% c("South America", "Caribbean","Central America")) %>%
  group_by(SubRegion) %>%
  summarise(obesity_avg=mean(Obesity),
            gdp_capita_avg=mean(GDP_per_capita))

regions_obesity <- regions_obesity %>% add_row(SubRegion="Latin America",
                                               obesity_avg=22.91786,
                                               gdp_capita_avg=8623.081)

regions_obesity

ggplot(regions_obesity, aes(reorder(SubRegion,obesity_avg),obesity_avg)) +
  geom_bar(position = 'dodge',
           stat = 'identity') + 
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  geom_hline(aes(yintercept = mean(obesity_avg)))  +
  coord_flip() +
  labs(title = "Obesity Rate",
       x="Regions",
       y="%")


ggplot(regions_obesity, aes(reorder(SubRegion,gdp_capita_avg),gdp_capita_avg)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fun = 'mean') + 
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "GDP per Capita",
       x="Regions",
       y="US Dollars")





## Obesity vs GDP analysis by region ##

by_SubRegions <- protein_supply %>%
  select(SubRegion,GDP_per_capita,Obesity) %>%
  group_by(SubRegion) %>%
  summarise(gdp_avg=mean(GDP_per_capita),
            obesity_avg=mean(Obesity)) 

ggplot(by_SubRegions, aes(gdp_avg,obesity_avg)) +
  geom_jitter() +
  geom_text_repel(
    aes(label=SubRegion),
    size=3,
    nudge_x = 0.05, nudge_y = 0.75,
    check_overlap=TRUE) +
  labs(title="GDP per capita Average vs Obesity Average by Region",
       x="GDP per Capita",
       y="Obesity Rate")



