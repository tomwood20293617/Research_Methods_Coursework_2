
#Reading the spreadsheet (csv file) into R:
datas <- read.csv("Results_21Mar2022.csv")

#Removing the data not required for my analysis:
datas <- datas[-c(12:20)]
datas$diet_group <- factor(datas$diet_group)

#reordering the diet group factor variable
newdatas <- datas
newdatas$diet_group <- factor(newdatas$diet_group, c("vegan", "veggie", "fish", "meat50", "meat", "meat100"))

#Producing a boxplot to compare acidification potential between males/females
#within each diet group (figure 2):
new_labels <- c("Vegan", "Veggie", "Fish", "Low Meat", "Medium Meat", "High Meat")
library(ggplot2)
library(tidyverse) 
ggplot(newdatas, aes(x = diet_group, y = mean_acid, fill = sex)) +
  geom_boxplot() + 
  theme(text = element_text(size = 14)) +
  ggtitle("Boxplot of Mean Acidification Potential by Diet Group and Sex") + 
  labs(fill = "Sex", x = "Diet Group", y = "Mean Acidification Potential") + 
  scale_x_discrete(labels = new_labels) +
  scale_fill_discrete(labels = c("Female", "Male"))

#Producing a boxplot to compare acidification potential between age groups
#within each diet group (figure 1):
library(ggplot2)
library(tidyverse) 
ggplot(newdatas, aes(x = diet_group, y = mean_acid, fill = age_group)) +
  geom_boxplot()  + 
  theme(text = element_text(size = 14)) +
  ggtitle("Boxplot of Mean Acidification Potential by Diet Group and Age Group") + 
  labs(fill = "Age Group", x = "Diet Group", y = "Mean Acidification Potential") + 
  scale_x_discrete(labels = new_labels)

#Please note that boxplots for all other variables look similar

#Separating the data dietgroup/sex groups:
vegan <- newdatas[newdatas$diet_group == "vegan",]
veggie <- newdatas[newdatas$diet_group == "veggie",]
fish <- newdatas[newdatas$diet_group == "fish",]
lowmeat <- newdatas[newdatas$diet_group == "meat50",]
medmeat <- newdatas[newdatas$diet_group == "meat",]
highmeat <- newdatas[newdatas$diet_group == "meat100",]

veganmal <- vegan[vegan$sex == "male",]
veganfem <- vegan[vegan$sex == "female",]
veggiemal <- veggie[veggie$sex == "male",]
veggiefem <- veggie[veggie$sex == "female",]
fishmal <- fish[fish$sex == "male",]
fishfem <- fish[fish$sex == "female",]
lowmeatmal <- lowmeat[lowmeat$sex == "male",]
lowmeatfem <- lowmeat[lowmeat$sex == "female",]
medmeatmal <- medmeat[medmeat$sex == "male",]
medmeatfem <- medmeat[medmeat$sex == "female",]
highmeatmal <- highmeat[highmeat$sex == "male",]
highmeatfem <- highmeat[highmeat$sex == "female",]

#creating the dataframe for the parallel coordinates plot

diet_types <- rep(c("Vegan", "Veggie", "Fish", "Low Meat", "Med Meat", "High Meat"), each = 2)
sex <- rep(c("Male", "Female"), times = 6)
ghgsmeds <- c(median(veganmal$mean_ghgs),median(veganfem$mean_ghgs),
              median(veggiemal$mean_ghgs),median(veggiefem$mean_ghgs),
              median(fishmal$mean_ghgs),median(fishfem$mean_ghgs),
              median(lowmeatmal$mean_ghgs),median(lowmeatfem$mean_ghgs),
              median(medmeatmal$mean_ghgs),median(medmeatfem$mean_ghgs),
              median(highmeatmal$mean_ghgs),median(highmeatfem$mean_ghgs))
landmeds <- c(median(veganmal$mean_land),median(veganfem$mean_land),
              median(veggiemal$mean_land),median(veggiefem$mean_land),
              median(fishmal$mean_land),median(fishfem$mean_land),
              median(lowmeatmal$mean_land),median(lowmeatfem$mean_land),
              median(medmeatmal$mean_land),median(medmeatfem$mean_land),
              median(highmeatmal$mean_land),median(highmeatfem$mean_land))
watscarmeds <- c(median(veganmal$mean_watscar),median(veganfem$mean_watscar),
                 median(veggiemal$mean_watscar),median(veggiefem$mean_watscar),
                 median(fishmal$mean_watscar),median(fishfem$mean_watscar),
                 median(lowmeatmal$mean_watscar),median(lowmeatfem$mean_watscar),
                 median(medmeatmal$mean_watscar),median(medmeatfem$mean_watscar),
                 median(highmeatmal$mean_watscar),median(highmeatfem$mean_watscar))
eutmeds <- c(median(veganmal$mean_eut),median(veganfem$mean_eut),
             median(veggiemal$mean_eut),median(veggiefem$mean_eut),
             median(fishmal$mean_eut),median(fishfem$mean_eut),
             median(lowmeatmal$mean_eut),median(lowmeatfem$mean_eut),
             median(medmeatmal$mean_eut),median(medmeatfem$mean_eut),
             median(highmeatmal$mean_eut),median(highmeatfem$mean_eut))
ch4meds <- c(median(veganmal$mean_ghgs_ch4),median(veganfem$mean_ghgs_ch4),
             median(veggiemal$mean_ghgs_ch4),median(veggiefem$mean_ghgs_ch4),
             median(fishmal$mean_ghgs_ch4),median(fishfem$mean_ghgs_ch4),
             median(lowmeatmal$mean_ghgs_ch4),median(lowmeatfem$mean_ghgs_ch4),
             median(medmeatmal$mean_ghgs_ch4),median(medmeatfem$mean_ghgs_ch4),
             median(highmeatmal$mean_ghgs_ch4),median(highmeatfem$mean_ghgs_ch4))
n2omeds <- c(median(veganmal$mean_ghgs_n2o),median(veganfem$mean_ghgs_n2o),
             median(veggiemal$mean_ghgs_n2o),median(veggiefem$mean_ghgs_n2o),
             median(fishmal$mean_ghgs_n2o),median(fishfem$mean_ghgs_n2o),
             median(lowmeatmal$mean_ghgs_n2o),median(lowmeatfem$mean_ghgs_n2o),
             median(medmeatmal$mean_ghgs_n2o),median(medmeatfem$mean_ghgs_n2o),
             median(highmeatmal$mean_ghgs_n2o),median(highmeatfem$mean_ghgs_n2o))
biomeds <- c(median(veganmal$mean_bio),median(veganfem$mean_bio),
             median(veggiemal$mean_bio),median(veggiefem$mean_bio),
             median(fishmal$mean_bio),median(fishfem$mean_bio),
             median(lowmeatmal$mean_bio),median(lowmeatfem$mean_bio),
             median(medmeatmal$mean_bio),median(medmeatfem$mean_bio),
             median(highmeatmal$mean_bio),median(highmeatfem$mean_bio))
watusemeds <- c(median(veganmal$mean_watuse),median(veganfem$mean_watuse),
                median(veggiemal$mean_watuse),median(veggiefem$mean_watuse),
                median(fishmal$mean_watuse),median(fishfem$mean_watuse),
                median(lowmeatmal$mean_watuse),median(lowmeatfem$mean_watuse),
                median(medmeatmal$mean_watuse),median(medmeatfem$mean_watuse),
                median(highmeatmal$mean_watuse),median(highmeatfem$mean_watuse))
acidmeds <- c(median(veganmal$mean_acid),median(veganfem$mean_acid),
              median(veggiemal$mean_acid),median(veggiefem$mean_acid),
              median(fishmal$mean_acid),median(fishfem$mean_acid),
              median(lowmeatmal$mean_acid),median(lowmeatfem$mean_acid),
              median(medmeatmal$mean_acid),median(medmeatfem$mean_acid),
              median(highmeatmal$mean_acid),median(highmeatfem$mean_acid))

parallel_df <- data.frame(Diet_Type = diet_types, sex = sex, 
                          Median_Mean_GHG_Emissions = ghgsmeds,
                          Median_Mean_Agricultural_Land_Use = landmeds,
                          Median_Mean_Water_Scarcity = watscarmeds,
                          Median_Mean_Eutrophication_Potential = eutmeds,
                          Median_Mean_GHG_From_CH4 = ch4meds,
                          Median_Mean_GHG_From_N2O = n2omeds,
                          Median_Mean_Biodiversity_Impact = biomeds,
                          Median_Mean_Agricultural_Water_Usage = watusemeds,
                          Median_Mean_Acidification_Potential = acidmeds)

write.csv(parallel_df, "parallel.csv", row.names = FALSE)
#at this point a change was made in excel - diet and sex was combined into one field. 

#importing the new spreadsheet
usethis <- read.csv("parallelforuse.csv")

#normalising data (scaling to proportions of the highest value of each variable)
df_norm <- as.data.frame(lapply(usethis[, -1], function(x) (x - min(x))/(max(x) - min(x))))
df_norm$Group <- usethis$Group

#reshaping the data for ggplot2
library(reshape2)
df_melt <- melt(df_norm, id.vars = "Group")

#custom x axis labels
custom_labels <- c(
  "Median_Mean_GHG_Emissions" = "GHG Emissions",
  "Median_Mean_Agricultural_Land_Use" = "Agricultural Land Use",
  "Median_Mean_Water_Scarcity" = "Water Scarcity",
  "Median_Mean_Eutrophication_Potential" = "Eutrophication Potential",
  "Median_Mean_GHG_From_CH4" = expression(GHG~From~CH[4]),
  "Median_Mean_GHG_From_N2O" = expression(GHG~From~N[2]*O),
  "Median_Mean_Biodiversity_Impact" = "Biodiversity Impact",
  "Median_Mean_Agricultural_Water_Usage" = "Agricultural Water Usage",
  "Median_Mean_Acidification_Potential" = "Acidification Potential"
)


# Plotting
ggplot(df_melt, aes(x = variable, y = value, group = Group, color = Group)) +
  geom_line(alpha = 0.75,size = 1.1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c(
                                "cyan2",
                                "deepskyblue1",
                                "lightpink",
                                "red",
                                "magenta",
                                "purple2",
                                "burlywood2",
                                "chocolate4",
                                "green2",
                                "green4",
                                "yellow",
                                "darkorange")) + #custom colours
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1), # tilting x axis labels
        text = element_text(size = 15)) + #text size
  scale_x_discrete(labels = custom_labels) +  # Change x-axis labels
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "Variables", y = "Normalised Values", color = "Group", title = 
  "Parallel Coordinates Plot for Comparrison of Male and Female Diets for all Diet Groups")
