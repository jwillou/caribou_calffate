library(ggplot2)
library(tidyr)
library(dplyr)
library(FSA)         
library(ggpubr)      

# Read the CSV
setwd("/Users/jannawilloughby/Google Drive/My Drive/Willoughby lab/projects - archive/Caribou/MCH Calf Survival/Calf Fate Data and Manuscript/")

###amount of death####
df <- read.csv("caribou_neonate_mortality.csv")

# Calculate mortality rate
df <- df %>%
  mutate(Mortality = Died / Cap)

# Scale 'Cap' to match the range of 'Mortality' for dual-axis plotting
max_mortality <- max(df$Mortality, na.rm = TRUE)
df <- df %>%
  group_by(Region) %>%
  mutate(
    Cap_scaled = Cap / max(Cap, na.rm = TRUE) * max_mortality
  )

# Plot
ggplot(df, aes(x = Year)) +
  geom_line(aes(y = Mortality, color = Region), size = 1.2) +
  geom_point(aes(y = Mortality, color = Region), size = 2) +
  geom_line(aes(y = Cap_scaled, color = Region), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("East" = "firebrick", "West" = "steelblue")) +
  scale_y_continuous(
    name = "Mortality Rate (Died / Captured)",
    sec.axis = sec_axis(
      trans = ~ . * max(df$Cap, na.rm = TRUE) / max_mortality,
      name = "Number Captured"
    )
  ) +
  scale_x_continuous(breaks = unique(df$Year)) +
  labs(title = "Neonate Caribou Mortality and Captures by Region", x = "Year", color = "Region") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm")
  )


####cause of death####
colorsPred = c("lightsalmon2", "aquamarine2", "skyblue", "orchid", "grey50", "lightgoldenrod")
data = read.table("MCHcalfCODchartsbycalvingarea_data.csv", header=TRUE, sep=",")
east = data[data$Calving=="East",]
west = data[data$Calving=="West",]

plot(-100,-100, xlim=c(0.5,9.5), ylim=c(0,0.6), xlab="", ylab="Proportion of Mortality", xaxt="n")
axis(side=1, labels=c(2011:2014, 2017:2021), tick=T, at=1:9)
width = 0.25
pdata = east #can change to east/west
for(i in 1:9){
  lastone = 0
  for(c in 1:6){
    polygon(x=c((i-width), (i+width), (i+width), (i-width)), 
            y=c((0+lastone), (0+lastone), (pdata[c,(i+1)]+lastone), (pdata[c,(i+1)]+lastone)),
            col=colorsPred[c], border=NA)
    lastone = lastone + pdata[c,(i+1)]
  }
}

#reordered, largest on the bottom

# Choose which region to plot
pdata = west  # change to east/west when needed

# setup
plot(-100, -100, xlim=c(0.5,9.5), ylim=c(0,0.6), xlab="", ylab="Proportion of Mortality", xaxt="n")
axis(side=1, labels=c(2011:2014, 2017:2021), tick=TRUE, at=1:9)
width = 0.25

# Loop over years
for(i in 1:9) {
  # Get proportions for all 6 causes in this year
  values = pdata[1:6, i+1]
  names(values) = 1:6  # index cause rows
  
  # Sort descending
  sorted = sort(values, decreasing=TRUE)
  order_idx = as.numeric(names(sorted))
  
  lastone = 0
  for(j in 1:6) {
    c = order_idx[j]
    height = pdata[c, i+1]
    polygon(
      x = c((i - width), (i + width), (i + width), (i - width)),
      y = c((0 + lastone), (0 + lastone), (height + lastone), (height + lastone)),
      col = colorsPred[c], border = NA
    )
    lastone = lastone + height
  }
}


####cause of death by age####
df <- read.csv("MCH Calf Early COD_May2025.csv")

#predation vs non predation and age
# Convert GENERAL.FATE to predation vs. non-predation
df$Fate_Type <- ifelse(df$GENERAL.FATE %in% c("Bear", "Wolf", "Wolverine", "Eagle", "Unk Pred"), "Predation", "Non-Predation")
wilcox.test(MIN.DAYS.ALIVE ~ Fate_Type, data = df)
tapply(df$MIN.DAYS.ALIVE, df$Fate_Type, median)

colnames(df) <- make.names(colnames(df))
df <- df %>%
  rename(MIN.DAYS.ALIVE = MIN.DAYS.ALIVE,
         GENERAL.FATE = GENERAL.FATE,
         PRED.NONPRED = PRED.NONPRED)

# Filter to predation only
df_pred <- df %>% filter(PRED.NONPRED == "Predation")

# Kruskal-Wallis test
kruskal.test(MIN.DAYS.ALIVE ~ GENERAL.FATE, data = df_pred)

# Post-hoc test (Dunn's test with Benjamini-Hochberg correction)
dunnTest(MIN.DAYS.ALIVE ~ GENERAL.FATE, data = df_pred, method = "bh")

predator_order <- c("Bear", "Wolf", "Eagle", "Wolverine", "Unk Pred")
df_pred$GENERAL.FATE <- factor(df_pred$GENERAL.FATE, levels = predator_order)
my_colors <- c(
  "Bear" = "lightsalmon2",      
  "Wolf" = "orchid",      
  "Eagle" = "skyblue",     
  "Wolverine" = "aquamarine2",
  "Unk Pred" = "grey50"   
)

ggboxplot(df_pred, 
          x = "GENERAL.FATE", 
          y = "MIN.DAYS.ALIVE", 
          color = "black", 
          fill = "GENERAL.FATE") +
  stat_summary(fun = median, 
               geom = "crossbar", 
               width = 0.6, 
               color = "black", 
               fatten = 2) +
  scale_fill_manual(values = my_colors) +
  labs(title = "Age at Death by Predator Type", 
       x = "Predator Type", 
       y = "Age at Death (days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###cause of death age east and west####
# East only
df_sub <- df %>% filter(E.W == "EAST")
wilcox.test(MIN.DAYS.ALIVE ~ Fate_Type, data = df_sub)
tapply(df_sub$MIN.DAYS.ALIVE, df_sub$Fate_Type, median)

df_pred <- df_sub %>% filter(PRED.NONPRED == "Predation")
kruskal.test(MIN.DAYS.ALIVE ~ GENERAL.FATE, data = df_pred)
dunnTest(MIN.DAYS.ALIVE ~ GENERAL.FATE, data = df_pred, method = "bh")

predator_order <- c("Bear", "Wolf", "Eagle", "Wolverine", "Unk Pred")
df_pred$GENERAL.FATE <- factor(df_pred$GENERAL.FATE, levels = predator_order)
my_colors <- c(
  "Bear" = "lightsalmon2",      
  "Wolf" = "orchid",      
  "Eagle" = "skyblue",     
  "Wolverine" = "aquamarine2",
  "Unk Pred" = "grey50"   
)

# Boxplot
ggboxplot(df_pred, 
          x = "GENERAL.FATE", 
          y = "MIN.DAYS.ALIVE", 
          color = "black", 
          fill = "GENERAL.FATE") +
  stat_summary(fun = median, 
               geom = "crossbar", 
               width = 0.6, 
               color = "black", 
               fatten = 2) +
  scale_fill_manual(values = my_colors) +
  labs(title = "Age at Death by Predator Type – East", 
       x = "Predator Type", 
       y = "Age at Death (days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# West only
df_sub <- df %>% filter(E.W == "WEST")
wilcox.test(MIN.DAYS.ALIVE ~ Fate_Type, data = df_sub)
tapply(df_sub$MIN.DAYS.ALIVE, df_sub$Fate_Type, median)

df_pred <- df_sub %>% filter(PRED.NONPRED == "Predation")
kruskal.test(MIN.DAYS.ALIVE ~ GENERAL.FATE, data = df_pred)
dunnTest(MIN.DAYS.ALIVE ~ GENERAL.FATE, data = df_pred, method = "bh")

predator_order <- c("Bear", "Wolf", "Eagle", "Wolverine", "Unk Pred")
df_pred$GENERAL.FATE <- factor(df_pred$GENERAL.FATE, levels = predator_order)
my_colors <- c(
  "Bear" = "lightsalmon2",      
  "Wolf" = "orchid",      
  "Eagle" = "skyblue",     
  "Wolverine" = "aquamarine2",
  "Unk Pred" = "grey50"   
)

# plot
ggboxplot(df_pred, 
          x = "GENERAL.FATE", 
          y = "MIN.DAYS.ALIVE", 
          color = "black", 
          fill = "GENERAL.FATE") +
  stat_summary(fun = median, 
               geom = "crossbar", 
               width = 0.6, 
               color = "black", 
               fatten = 2) +
  scale_fill_manual(values = my_colors) +
  labs(title = "Age at Death by Predator Type – West", 
       x = "Predator Type", 
       y = "Age at Death (days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

