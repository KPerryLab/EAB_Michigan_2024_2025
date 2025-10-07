# Tree NMDS
# 8/19/2025
# Aaron Tayal
# I'd like to make an NMDS on the 30 hydric plots that incorporates all the 
# information collected in 2024-2025 about canopy trees, understory trees,
# and ground cover.

# The reason I'm doing this is to see what is different about the plots
# where ash regeneration did not occur, versus the plots where ash regeneration
# did occur.

# Variables used to construct the distance matrix:
# Total basal area (>2.5 cm DBH) of each tree *species*
# Percent cover of graminoids, ferns, spicebush, ilex, poison sumac, skunk cabbage

# I think I may want to run the percent cover data separately from the 
# tree basal area. Because they are on different scales.


library(dplyr)
library(ggplot2)
theme_set(theme_classic(base_size = 14))
library(vegan)
library(ggpubr)

BA0 <- read.csv("Cleaned_data/basal_area_big_and_small_trees_spp_level_by_plot.csv")

# Now merge the data to the plot centers data frame:
plots <- read.csv("Cleaned_data/EAB_Michigan_2024_2025_plot_centers_with_hydro.csv")
plots1 <- plots %>% select(center_tree_number, mstrlvl, plotmstr, Transect,
                           Park, Plot_ID)
BA <- right_join(plots1, BA0, by="center_tree_number")

# Information on plot-level percent cover:
cover0 <- read.csv("Cleaned_data/percent_cover_by_plot.csv")
cover <- right_join(plots1, cover0, by="center_tree_number")

# NMDS on tree basal areas: ####################################################

# Now calculate distance between plots in species space. I'm using Bray-Curtis
# dissimilarity because it will ignore double zeros (plots that both lack a
# species should not be considered more similar)

tree_spp <- c("Acer_saccharinum", "Acer_saccharum", "Betula_alleghaniensis",
              "Carpinus_caroliniana", "Frangula_alnus", "Fraxinus_nigra",
              "Fraxinus_pennsylvanica_etc", "Larix_laricina", "Populus",
              "Quercus_white", "Quercus_red", "Tilia_americana", "Ulmus")

BA_matrix <- as.matrix(BA %>% select(all_of(tree_spp)))

#dist_BA <- vegdist(BA_matrix, method = "bray")
#nmds_BA <- metaMDS(dist_BA, trymax = 500, k = 2)



nmds_BA <- metaMDS(BA_matrix, distance = "bray", trymax = 500, k = 2, autotransform = F)
nmds_BA

BA$nmds1 <- nmds_BA$points[,1]
BA$nmds2 <- nmds_BA$points[,2]

ggplot(data=BA, aes(x=nmds1, y=nmds2, color=Acer_saccharinum)) + geom_point(size=2) +
  scale_color_gradientn(colors = rainbow(7))

ggplot(data=BA, aes(x=nmds1, y=nmds2, color=Transect)) + geom_point(size=2)

ggplot(data=BA, aes(x=nmds1, y=nmds2, label=center_tree_number)) + geom_point(size=2) +
  geom_text(vjust=1)

species_scores <- data.frame(scores(nmds_BA, display = "species"))
species_scores$species <- row.names(species_scores)
ggplot() + 
  geom_point(data=BA, aes(x=nmds1, y=nmds2, color=Transect), size=3) +
  theme(legend.box.background = element_rect(), legend.box.margin = margin(3,3,3,3)) +
  coord_fixed() + scale_x_continuous(limits = c(-1.75, 1.75), breaks = seq(-1.5, 1.5, 0.5)) +
  xlab("NMDS1") + ylab("NMDS2") +
  geom_text(data = species_scores, aes(x=NMDS1, y=NMDS2, label = species), alpha=0.5)
spp_graph <- ggplot() + 
  geom_point(data=BA, aes(x=nmds1, y=nmds2), size=2, alpha=0.5) +
  coord_fixed() + scale_x_continuous(limits = c(-1.5, 1.8), breaks = seq(-1, 1, 1)) +
  xlab("NMDS1") + ylab("NMDS2") +
  geom_text(data = species_scores, aes(x=NMDS1, y=NMDS2, label = species), alpha=0.5)
spp_graph

# NMDS on ground-level percentage cover: #######################################

cover_types <- c("graminoids", "skunk_cabbage", "ferns", "spicebush", "ilex",          
                 "poison_sumac", "other_woody_shrub") # omit glossy buckthorn and water cover

cover_matrix <- as.matrix(cover %>% select(all_of(cover_types)))

dist_cover <- vegdist(cover_matrix, method = "bray")

nmds_cover <- metaMDS(dist_cover, trymax = 500, k = 2)

cover$nmds1 <- nmds_cover$points[,1]
cover$nmds2 <- nmds_cover$points[,2]

ggplot(data=cover, aes(x=nmds1, y=nmds2, color=Park)) + geom_point(size=2)

ggplot(data=cover, aes(x=nmds1, y=nmds2, fill=spicebush)) + 
  geom_point(size=3, shape=21) +
  scale_fill_gradient(low = "black", high = "white")

# Try running basal area and ground-level percentage cover in the same ########
# NMDS: for this I will need to standardize by column maximum. This will cause
# Acer saccharinum to become less important in determining the structure. It 
# will also make sure the percent cover variables don't dominate the structure 
# just because they are bigger than the basal areas.

dat <- full_join(BA, cover0, by="center_tree_number")

dat_matrix <- as.matrix(dat %>% select(all_of(c(tree_spp, cover_types))))

dat_matrix_stdz <- decostand(dat_matrix, method = "max", MARGIN = 2) 
# Standardize by column to the max of each column

#dist_dat <- vegdist(dat_matrix_stdz, method = "bray")
#nmds_dat <- metaMDS(dist_dat, trymax = 500, k = 2)

nmds_dat <- metaMDS(dat_matrix_stdz, distance = "bray", trymax = 500, k = 2)

nmds_dat
stressplot(nmds_dat)

dat$nmds1 <- nmds_dat$points[,1]
dat$nmds2 <- nmds_dat$points[,2]

transects <- ggplot(data=dat, aes(x=nmds1, y=nmds2, color=Park)) + geom_point(size=3) +
  theme(legend.box.background = element_rect(), legend.box.margin = margin(3,3,3,3)) +
  coord_fixed() + 
  geom_text(aes(x=nmds1, y=nmds2, color=Park, label=Transect), vjust=1.2) +
  xlab("NMDS1") + ylab("NMDS2")
transects

ggplot(data=dat, aes(x=nmds1, y=nmds2, label=center_tree_number)) + geom_point(size=2) +
  geom_text(vjust=1)

skunk_cabbage <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=skunk_cabbage)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill="Skunk\ncabbage\n% cover") + xlab("") + ylab("") +
  theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
    legend.text = element_text(size = 12), # legend text size
    legend.title = element_text(size = 12)) + theme(
axis.text.x = element_blank(),
axis.text.y = element_blank()
)
skunk_cabbage

ilex <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=ilex)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill=expression(italic("Ilex \nverticillata")))+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                                            legend.text = element_text(size = 12), # legend text size
                                                                                            legend.title = element_text(size = 12))
ilex

graminoids <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=graminoids)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill="Graminoids \n% cover")+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                          legend.text = element_text(size = 12), # legend text size
                                                                          legend.title = element_text(size = 12))+ theme(
                                                                            axis.text.x = element_blank(),
                                                                            axis.text.y = element_blank()
                                                                          )
graminoids

ferns <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=ferns)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill="Ferns \n% cover")+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                    legend.text = element_text(size = 12), # legend text size
                                                                    legend.title = element_text(size = 12))
ferns

carpinus <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=Carpinus_caroliniana)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill=expression(italic("Carpinus \ncaroliniana")))+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                                               legend.text = element_text(size = 12), # legend text size
                                                                                               legend.title = element_text(size = 12))+ theme(
                                                                                                 axis.text.x = element_blank(),
                                                                                                 axis.text.y = element_blank()
                                                                                               )
carpinus

plotmstr <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=plotmstr)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill="plot moisture")+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                  legend.text = element_text(size = 12), # legend text size
                                                                  legend.title = element_text(size = 12))
plotmstr

silver <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=Acer_saccharinum)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill=expression(italic("Acer \nsaccharinum")))+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                                           legend.text = element_text(size = 12), # legend text size
                                                                                           legend.title = element_text(size = 12))+ theme(
                                                                                             axis.text.x = element_blank(),
                                                                                             axis.text.y = element_blank()
                                                                                           )
silver

tilia <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=Tilia_americana)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill=expression(italic("Tilia \namericana")))+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                                          legend.text = element_text(size = 12), # legend text size
                                                                                          legend.title = element_text(size = 12))+ theme(
                                                                                            axis.text.x = element_blank(),
                                                                                            axis.text.y = element_blank()
                                                                                          )
tilia

black_ash <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=Fraxinus_nigra)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill=expression(italic("Fraxinus \nnigra")))+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                                         legend.text = element_text(size = 12), # legend text size
                                                                                         legend.title = element_text(size = 12))+ theme(
                                                                                           axis.text.x = element_blank(),
                                                                                           axis.text.y = element_blank()
                                                                                         )
black_ash

green_ash <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=Fraxinus_pennsylvanica_etc)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill=expression(italic("Fraxinus \npennsylvanica")))+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                                                 legend.text = element_text(size = 12), # legend text size
                                                                                                 legend.title = element_text(size = 12))+ theme(
                                                                                                   axis.text.x = element_blank(),
                                                                                                   axis.text.y = element_blank()
                                                                                                 )
green_ash

elm <- ggplot(data=dat, aes(x=nmds1, y=nmds2, fill=Ulmus)) + 
  geom_point(size=3, shape=21) + coord_fixed() +
  scale_fill_gradientn(colors = c("black", gray(0.75), "white")) +
  labs(fill=expression(italic("Ulmus sp.")))+ xlab("") + ylab("")+theme(legend.key.size = unit(0.5, "cm"),   # size of legend keys (squares/lines)
                                                                                  legend.text = element_text(size = 12), # legend text size
                                                                                  legend.title = element_text(size = 12))+ theme(
                                                                                    axis.text.x = element_blank(),
                                                                                    axis.text.y = element_blank()
                                                                                  )
elm

ggarrange(green_ash, black_ash, silver, tilia, elm, carpinus, graminoids,
          skunk_cabbage, ncol=2, nrow=4, labels = c("A","B","C","D","E","F","G","H")) 




