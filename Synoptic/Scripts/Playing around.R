# Playing around


ggplot(SW, aes(x= NPOC_mgC_L, y = CO2_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "", y = CO2_lab) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(SW, aes(x= FI, y = CH4_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "", y = CO2_lab) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(SW, aes(x= FI, y = CO2_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "", y = CO2_lab) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(SW, aes(x= BIX, y = CO2_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "", y = CO2_lab) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(SW, aes(x= FI, y = CH4_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "", y = CO2_lab) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(SW, aes(x= BIX, y = CH4_uM, color = Sample_Type, shape = Sample_Type)) +
  geom_point() +
  theme +
  labs(x = "", y = CO2_lab) +
  geom_smooth(method = "lm", se = FALSE)
