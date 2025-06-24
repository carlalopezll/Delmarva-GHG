# Script for linear regressions


#### Regressions w/ other water chem and physicochemistry ####

f1 <- ggplot(merge, aes(x = NPOC, y = CO2_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE) +
  labs(x = "DOC (mg/L)", y = CO2_lab) +
  scale_y_log10() +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 38, size = 5) +
  theme
f1

summary(lm(formula = CO2_uM~NPOC, data = SW)) # significant
summary(lm(formula = CO2_uM~NPOC, data = GW)) # significant


f2 <- ggplot(merge, aes(x = NPOC, y = CH4_uM, color= Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  labs(x = "DOC (mg/L)", y = CH4_lab, se= FALSE) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 37, size = 5, label.y.npc = 0.4)
f2

summary(lm(formula = CH4_uM~NPOC, data = SW)) # significant
summary(lm(formula = CH4_uM~NPOC, data = GW)) # significant

ggsave("Graphs/CH4 vs DOC_all.jpg")

f <- grid.arrange(f1, f2)
f



# Regressions with DO

d1 <- ggplot(synoptic_clean, aes(x= DO_percent, y = CO2_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "Dissolved oxygen (%)", y = CO2_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 75, size = 5) +
  geom_smooth(method = "lm", se = FALSE)
d1

summary(lm(formula = CO2_uM~DO_percent, data = SW)) # significant
summary(lm(formula = CO2_uM~DO_percent, data = UW)) # almost significant

ggsave("Graphs/CO2 vs DO_all.jpg")

d2 <- ggplot(synoptic_clean, aes(x= DO_percent, y = CH4_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "Dissolved oxygen (%)", y = CH4_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 75, size = 5) +
  geom_smooth(method= "lm", se = FALSE) + 
  scale_y_log10()
d2

ggsave("Graphs/CH4 vs DO_all.jpg")

summary(lm(formula = CH4_uM~DO_percent, data = full_SW)) # significant
summary(lm(formula = CH4_uM~DO_percent, data = full_UW)) # not significant

d <- grid.arrange(d1, d2)
d

# Regressions with temperature

e1 <- ggplot(synoptic_clean, aes(x= Temp_C, y = CO2_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  scale_y_log10() + 
  labs(x = expression("Temperature " ( degree*C)), y = CO2_lab) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 22, size = 5)
e1

summary(lm(formula = CO2_uM~Temp_C, data = full_UW))

ggsave("Graphs/CO2 vs temp_all.jpg")

e2 <- ggplot(synoptic_clean, aes(x= Temp_C, y = CH4_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = expression("Temperature " ( degree*C)), y = CH4_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 22, size = 5)
e2

summary(lm(formula = CH4_uM~Temp_C, data = full_SW))
ggsave("Graphs/CH4 vs temp_all.jpg")

e <- grid.arrange(e1, e2)

fig_8 <- grid.arrange(f, d, e, ncol = 3)

ggsave("Graphs/MS/Regressions.jpg", fig_8, width = 24, height = 12, dpi = 250, units = "in")









f1 <- ggplot(SW, aes(x = NPOC, y = CO2_uM)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE) +
  labs(x = "DOC (mg/L)", y = CO2_lab) +
  scale_y_log10() +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 38, size = 5) +
  theme
f1

summary(lm(formula = CO2_uM~NPOC, data = SW)) # almost significant
summary(lm(formula = CO2_uM~NPOC, data = UW)) # significant


f2 <- ggplot(SW, aes(x = NPOC, y = CH4_uM)) +
  geom_point() +
  theme +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_log10() +
  labs(x = "DOC (mg/L)", y = CH4_lab, se= FALSE) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 50, size = 5, label.y.npc = 0.4)
f2

summary(lm(formula = CH4_uM~NPOC_mgC_L, data = all_SW)) # significant
summary(lm(formula = CH4_uM~NPOC_mgC_L, data = all_UW)) # not significant

ggsave("Graphs/CH4 vs DOC_all.jpg")

f <- grid.arrange(f1, f2)
f

# Regressions with DO

d1 <- ggplot(SW, aes(x= DO_percent, y = CO2_uM)) +
  geom_point() +
  theme +
  labs(x = "Dissolved oxygen (%)", y = CO2_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 75, size = 5) +
  geom_smooth(method = "lm", se = FALSE)
d1

summary(lm(formula = CO2_uM~DO_percent, data = SW)) # significant
summary(lm(formula = CO2_uM~DO_percent, data = UW)) # almost significant

ggsave("Graphs/CO2 vs DO_all.jpg")

d2 <- ggplot(SW, aes(x= DO_percent, y = CH4_uM)) +
  geom_point() +
  theme +
  labs(x = "Dissolved oxygen (%)", y = CH4_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 75, size = 5) +
  geom_smooth(method= "lm", se = FALSE) + 
  scale_y_log10()
d2

ggsave("Graphs/CH4 vs DO_all.jpg")

summary(lm(formula = CH4_uM~DO_percent, data = full_SW)) # significant
summary(lm(formula = CH4_uM~DO_percent, data = full_UW)) # not significant

d <- grid.arrange(d1, d2)
d

# Regressions with temperature

e1 <- ggplot(SW, aes(x= Temp_C, y = CO2_uM)) +
  geom_point() +
  theme +
  scale_y_log10() + 
  labs(x = expression("Temperature " ( degree*C)), y = CO2_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 22, size = 5)
e1

summary(lm(formula = CO2_uM~Temp_C, data = full_UW))

ggsave("Graphs/CO2 vs temp_all.jpg")

e2 <- ggplot(SW, aes(x= Temp_C, y = CH4_uM)) +
  geom_point() +
  theme +
  scale_y_log10() +
  labs(x = expression("Temperature " ( degree*C)), y = CH4_lab) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 22, size = 5)
e2

summary(lm(formula = CH4_uM~Temp_C, data = full_SW))
ggsave("Graphs/CH4 vs temp_all.jpg")

e <- grid.arrange(e1, e2)

fig_8 <- grid.arrange(f, d, e, ncol = 3)

ggsave("Graphs/MS/Regressions.jpg", fig_8, width = 24, height = 12, dpi = 250, units = "in")


install.packages("viridis")  # Install
library("viridis")           # Load


# Regressions with water isotopes

ggplot(SW, aes(x= CO2_uM, y = CH4_uM, color = DO_mgL)) +
  geom_point() +
  theme +
  theme(legend.position = "right") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis()


ggplot(SW, aes(x= dly_mean_wtrlvl, y = CH4_uM)) +
  geom_point()

str(merge)
