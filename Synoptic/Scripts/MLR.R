# Multiple linear regressions

model <- lm(CH4_uM ~ NPOC_mgC_L + SO4_mg_L, data = all_SW)
summary(model)

summary(model)$coefficient

confint(model)
