library("lme4")
library(lattice)
library(ggplot2)
library(gridExtra)

data <- sleepstudy
print(head(data))
# a) LRM Modell
model <- lm(Reaction ~ Days, data = data)
# bo = 251.4, b1 = 10.47 y = 251.4 + 10.47 * x


# b)
xyplot(Reaction ~ Days | Subject,
  data = data,
  xlab = "Days",
  ylab = "Reaction Time",
  type = c("p", "r"),
  aspect = 1,
  index.cond = function(x, y) median(y)
)

# c)
model1 <- lmer(Reaction ~ Days + (1|Subject), data = data)
model2 <- lmer(Reaction ~ Days + (Days|Subject), data = data)
model3 <- lmer(Reaction ~ Days + (0 + Days|Subject), data = data)
model4 <- lmer(Reaction ~ Days + (1 + Days|Subject), data = data)
model5 <- lmer(Reaction ~ Days + (1|Subject) + (0 + Days|Subject), data = data)
model6 <- lmer(Reaction ~ Days + (Days || Subject), data = data)

# d)
anova(model4, model5)

# -> Model 5 is better than Model 4

# e)
confint(model5)
# Konfidenzintervall Days: [7.33, 13.60]

# f)
coef(model5)


# g)
# Schätzung des gemischten Modells
model5 <- lmer(Reaction ~ Days + (1 | Subject) + (0 + Days | Subject), data = data)

# Extraktion der fixen Effekte und zufälligen Effekte
fixed_effects <- fixef(model5)
random_effects <- ranef(model5)
coefficients <- coef(model5)

# Konvertierung der Zufallseffekte in einen Dataframe
random_effects_df <- as.data.frame(random_effects$Subject)
random_effects_df$Subject <- rownames(random_effects$Subject)

plot_fixed <- ggplot(data.frame(Fixed = fixed_effects), aes(x = names(fixed_effects), y = fixed_effects)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Fixe Effekte", x = "Effekt", y = "Wert") +
  theme_minimal()

plot_random_intercept <- ggplot(random_effects_df, aes(x = Subject, y = `(Intercept)`)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Zufällige Effekte - Intercept", x = "Subject", y = "Wert") +
  theme_minimal()

plot_random_slope <- ggplot(random_effects_df, aes(x = Subject, y = `Days`)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Zufällige Effekte - Steigung (Days)", x = "Subject", y = "Wert") +
  theme_minimal()

grid.arrange(plot_fixed, plot_random_intercept, plot_random_slope, ncol = 1)