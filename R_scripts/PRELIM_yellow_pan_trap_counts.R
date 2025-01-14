# Jan 10 2025
# Preliminary analyses of yellow pan trap counts for 
# EAB Michigan project
# Aaron Tayal

library(ggplot2)
library(dplyr)

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

plot(ypts$longitude_minutes_part, ypts$latitude_minutes_part)
text(ypts$longitude_minutes_part, ypts$latitude_minutes_part, 
     labels=ypts$trap_number, cex= 0.7)
# I think the GPS coordinates of the trap locations are completely
# useless (not accurate enough)

arth0 <- read.csv("Raw_data/10-i-2025_yellow_pan_trap_counts_preliminary.csv")

# I only want to analyze data from these three weeks:
arth <- arth0 %>% filter(set_date %in% c("06_14_2024", "06_21_2024", "06_27_2024"))

# Variables of interest:

# Total number of Hymenoptera, Diptera, Coleoptera, and Lepidoptera, 
# other arthropods

# Specific groups of Hymenoptera:
# Number of diapriids, dryinids, sawflies, ants, ichneumons, braconids,
# ceraphronids, chalcidoids, 

# Since in the first interval I was not good at IDing chalcidoids or ceraphronids,
# I'm going to remove those groups from consideration. Later I'll go back through
# to get the IDs if I have time. Another group that was extremely common that
# I'm omitting here is platygastrids/scelionids (due to ID difficulties at the 
# beginning)

# Hypotheses:

# Trees with canopy decline (CC > 1) will have higher numbers of 
# woodboring braconids and ichneumonids. Unfourtunately all I have to
# test this is the total counts of braconids and ichneumonids.

# Diapriids are parasitoids of Auchenorrhyncha: cicadas, leafhoppers, 
# treehoppers, planthoppers, and spittlebugs. So I expect their numbers
# to be correlated with counts of Hemiptera

# Common Diptera included phorid flies, tachinids, gall flies, and MANY 
# other kinds. So it's unclear what to expect from their counts

# Common beetles included rove beetles and tumbling flower beetles (Mordellidae)

# Pool across sample intervals:

taxa <- c(
  "Diptera", "Hemiptera", "Lepidoptera", "Coleoptera","Other_arthropods", 
  "Total_Hymenoptera", "Symphyta", "Chrysidoidea", "Proctotrupoidea", "Formicidae", 
  "Ichneumonoidea_Ichneumonidae", "Ichneumonoidea_Braconidae")

arthpool0 <- arth %>% group_by(trap_number) %>%
  summarise(across(all_of(taxa), ~ sum(.x, na.rm=T)))

# Combine with trap location info:
arthpool <- full_join(arthpool0, ypts, by="trap_number")

arthpool$Total_arthropods <- arthpool$Total_Hymenoptera +
  arthpool$Diptera + arthpool$Coleoptera + arthpool$Lepidoptera +
  arthpool$Hemiptera + arthpool$Other_arthropods

# Testing the hypothesis about dryinids (Chrysidoidea):
plot(arthpool$Chrysidoidea, arthpool$Hemiptera)

# Testing the hypotheses about braconids and ichneumonids:
ggplot(data=arthpool, aes(x=ash_tree_decline, y=Ichneumonoidea_Ichneumonidae)) +
  geom_jitter(alpha=0.5, height=0, width=0.05) + theme_classic()

ggplot(data=arthpool, aes(x=ash_tree_decline, y=Ichneumonoidea_Braconidae)) +
  geom_point(alpha=0.5) + theme_classic()

arthpool$Total_Ichneumonoidea <- arthpool$Ichneumonoidea_Braconidae + 
  arthpool$Ichneumonoidea_Ichneumonidae

ggplot(data=arthpool, aes(x=ash_tree_decline, y=Total_Ichneumonoidea)) +
  geom_jitter(alpha=0.5, height=0, width=0.05) + theme_bw() +
  xlab("Presence of ash canopy decline") + 
  ylab("Ichneumon and \nbraconid wasps (count)") + ylim(0,20) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

wilcox.test(arthpool$Total_Ichneumonoidea ~ arthpool$ash_tree_decline,
            exact = FALSE)

testdata <- data.frame(x=c("a", "a", "a", "a", "a", "a", 
                           "b", "b", "b", "b", "b", "b"),
                       y=c(1,2,3,2,1,1,
                           4,5,4,5,4,4))
wilcox.test(testdata$y ~ testdata$x, exact=FALSE) # This nonparametric test
# is sensitive to the ordering, but not to absolute values

# That brings up the question of whether declining ash trees had more 
# Hymenoptera in general.

# I hypothesize that trees in more sunlight will have higher numbers of 
# insects:
ggplot(data=arthpool, aes(x=crown_class_D_C_I_S, y=Total_arthropods)) +
  geom_point() + theme_classic()

# Create a bar graph for the total counts of different taxa:
total_counts_3wks <- colSums(arthpool[,taxa])

total_counts_3wks_df <- data.frame(taxon = names(total_counts_3wks),
                                   count = total_counts_3wks)

total_counts_3wks_df$taxon <- factor(total_counts_3wks_df$taxon, 
                                     levels = total_counts_3wks_df$taxon)

library(forcats)
total_counts_3wks_df$taxon <- fct_rev(total_counts_3wks_df$taxon)
total_counts_3wks_df$taxon <- fct_recode(total_counts_3wks_df$taxon,
  Symphyta_Sawflies="Symphyta", Braconidae = "Ichneumonoidea_Braconidae",
  Ichneumonidae = "Ichneumonoidea_Ichneumonidae", Diptera_flies = "Diptera",
  Hemiptera_true_bugs = "Hemiptera", Lepidoptera_moths = "Lepidoptera",
  Formicidae_ants = "Formicidae", Diapriidae="Proctotrupoidea", 
  Dryinidae = "Chrysidoidea", Coleoptera_beetles = "Coleoptera")

ggplot(data=total_counts_3wks_df, aes(x=taxon, y=count)) + 
  geom_bar(stat="identity") + coord_flip() + theme_classic() +
  xlab("") + ylab("Number of individuals")


# Createarthpool# Create an NMDS plot:
library(vegan)
dis.matrix <- vegdist(arthpool[,taxa], method = "bray")
dis.matrix
nmds.ypts <- metaMDS(dis.matrix, trymax = 500, autotransform = TRUE, k = 2)
stressplot(nmds.ypts)
nmds.ypts
plot(nmds.ypts)

ordiplot(nmds.ypts, disp = "sites", type = "n", xlim = c(-2, 2), ylim = c(-2, 2))
points(nmds.ypts, dis = "sites", select = which(arthpool$ash_tree_decline==0), pch = 15, cex = 1, col = "palegreen4")
points(nmds.ypts, dis = "sites", select = which(arthpool$ash_tree_decline==1), pch = 16, cex = 1, col = "brown4")

ordiellipse(nmds.ypts, arthpool$ash_tree_decline, draw = "lines", col = c("palegreen4", "brown4"), 
            lwd = 3, kind = "sd", conf = 0.90, label = FALSE)

legend("topleft", legend = c("Healthy canopy", "Declining canopy"),
       pch = c(15, 16), cex = 0.8, bty = "n", col = c("palegreen4", "brown4"))


