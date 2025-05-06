# Aaron Tayal
# May 2, 2025
# Yellow pan trap counts

# The goal is to analyse the 2024 yellow pan trap captures. Some pan traps were
# attached to declining ash trees, while others were attached to ash trees
# with a healthy canopy. My goal is to investigate whether the abundance of 
# various insect groups differs between healthy and declining ash trees. I will
# do this by running Mann-Whitney U-tests.

library(ggplot2)
library(dplyr)
library(readxl)

# get info about yellow pan trap locations, etc.
trap_locations0 <- read.csv("Raw_data/EAB_Michigan_2024_trap_locations.csv")
ypts <- trap_locations0 %>% filter(trap_type == "yellow pan")
ypts$trap_number

# Re-use code for canopy decline:
# Create a variable called ash tree decline, which is only equal to 1 if canopy
# condition is 2,3,4, or 5
ypts$ash_tree_decline <- ifelse(ypts$canopy_condition_of_nearest_ash > 1, 1, 0)
ypts$ash_tree_decline <- as.factor(ypts$ash_tree_decline)
table(ypts[,c("ash_tree_decline", "canopy_condition_of_nearest_ash")])
# 6 trees have some degree of canopy decline, while 9 trees have no canopy decline

hist(ypts$diameter_at_137_cm_in_cm_of_nearest_ash) # The yellow pan traps
# vary a lot in diameter of the tree they are set on

# Correlation between canopy decline and DBH for these 15 trees?
ggplot(data=ypts, aes(x=ash_tree_decline, y=diameter_at_137_cm_in_cm_of_nearest_ash)) +
  geom_point(alpha=0.5) + theme_classic()

ypts$crown_class_D_C_I_S <- as.factor(ypts$crown_class_D_C_I_S)
table(ypts$crown_class_D_C_I_S) # 4 trees were in the shade (suppressed),
# while 10 were classified as intermediate and 1 as a canopy tree
table(ypts[,c("ash_tree_decline", "crown_class_D_C_I_S")])

arth0 <- read_excel("Raw_data/EAB_Michigan_2024_raw_data_photos_removed.xlsx",
                    sheet = 8, na = "NA")
# nine weeks

taxa <- colnames(arth0)[5:31]

# subset to only 5 weeks (June 27 to Aug 1, 2025):
arth <- arth0 %>% filter(Completed_sorting_y_n == "y")

# pool by pan trap (only for the 5 weeks)
arthpool0 <- arth %>% group_by(trap_number) %>%
  summarise(across(all_of(taxa), ~ sum(.x, na.rm=T)))

# Combine with trap location info:
arthpool <- full_join(arthpool0, ypts, by="trap_number")

arthpool$Total_arthropods <- arthpool$Total_Hymenoptera +
  arthpool$Diptera + arthpool$Coleoptera + arthpool$Lepidoptera +
  arthpool$Hemiptera + arthpool$Other_arthropods

# Use Mann-Whitney U test (same as Wilcoxon rank sum test) to see if any arthropod
# groups differ in abundance between healthy and declining ash trees

test_taxa <- c("Total_arthropods", "Symphyta","Chrysidoidea_Dryinidae",
               "Formicidae","Pompilloidea","Apoidea","Ichneumonoidea_Ichneumonidae",
               "Ichneumonoidea_Braconidae","Proctotrupoidea_Diapriidae",
               "Ceraphronoidea","Chalcidoidea_Mymaridae","Chalcidoidea_Encyrtidae",
               "Platygastroidea","Diptera","Hemiptera","Lepidoptera",
               "Coleoptera")

p_values <- vector("list", 17)
names(p_values) <- test_taxa

for (i in 1:17){
  group_declining <- unlist(as.vector(arthpool[arthpool$ash_tree_decline == 1, test_taxa[i]]))
  group_healthy <- unlist(as.vector(arthpool[arthpool$ash_tree_decline == 0, test_taxa[i]]))
  test <- stats::wilcox.test(group_healthy, group_declining, alternative = "two.sided")
  p_values[i] <- test$p.value
}

# Only Apoidea appears to differ between healthy and declining ash trees
ggplot(data=arthpool, aes(x=ash_tree_decline, y=Apoidea)) +
  geom_jitter(alpha=0.5, height=0, width=0.05) + theme_bw() +
  xlab("Presence of ash canopy decline") + 
  ylab("Bees and sphecids (count)") + ylim(0,20) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
# It seems more bees/sphecids were found on ash trees that were declining

cor_matrix <- cor(arthpool[, test_taxa])
corrplot::corrplot(cor_matrix, method="ellipse")







