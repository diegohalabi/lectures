# Load packages
library(tidyverse)
library(car) 
# Normal distribution
df <- data.frame(
  x <- rnorm(100000,mean=1.7,sd=.075)
)
ggplot(df,aes(x=x))+
  geom_histogram(binwidth = .03) +
  scale_x_continuous(name = 'Height (m)',
                     limits = c(1.4, 2)) +
  scale_y_continuous(name = 'Frecuency (n)',
                     breaks = c(5000, 10000, 15000),
                     labels = c('5', '10', '15')) +
  geom_vline(xintercept = 1.7,
             color = 'red',
             linetype = 2) +
  ggsave('normal.pdf', width = 4, height = 3, units = 'in', device = 'pdf')
# Make 2 groups
df <- data.frame(
  ctrl = rnorm(15, mean = 3.5, sd = .9),
  mel = rnorm(15, mean = 4, sd = .9),
  Group = c('treatment')
) %>%
  gather(Group, Proliferation, 1:2)
df$Group <- factor(
  df$Group,
  levels=c('ctrl','mel'),
  labels=c('Control','Melatonin'))
str(df)
  ## Plot
ggplot(df,aes(x = Group, y = Proliferation))+
  geom_violin(trim = F) +
  geom_jitter(height = 0, width = .05, size = .6) +
  scale_x_discrete(name='Group') +
  scale_y_continuous(name='Proliferation marker') +
  theme_light() +
  ggsave('twogroups.pdf', width = 3, height = 3, units = 'in', device = 'pdf')
  ## shapiro test
shapiro.test(df$Proliferation)
  ## Q-Q plot
ggplot(df, aes(sample = Proliferation)) +
  stat_qq() + stat_qq_line(color = 'red') +
  scale_y_continuous(name = 'Proliferation marker') +
  scale_x_continuous(name = 'Teorico') +
  theme_light() +
  ggsave('qqplot.pdf', width = 3, height = 3, units = 'in', device = 'pdf')
## T-test
t.test(df$Proliferation~df$Group, var.eq = T)
# Make 3 groups data
df <- data.frame(
  ctrl = rnorm(15, mean = 3.5, sd = .9),
  mela = rnorm(15, mean = 3.5, sd = .9),
  melb = rnorm(15, mean = 4, sd = .9),
  melc = rnorm(15, mean = 4.5, sd = .9),
  Group = c('treatment')
) %>%
  gather(Group, Proliferation, 1:4)
df$Group <- factor(
  df$Group,
  levels=c('ctrl','mela','melb','melc'),
  labels=c('Control','Mel 0.1nM','Mel 1nM','Mel 10nM'))
str(df)
## Plot
ggplot(df,aes(x = Group, y = Proliferation))+
  geom_violin(trim = F) +
  geom_jitter(height = 0, width = .05, size = .6) +
  scale_x_discrete(name='Group') +
  scale_y_continuous(name='Proliferation marker') +
  theme_light() +
  ggsave('3groups.pdf', width = 5, height = 3, units = 'in', device = 'pdf')
    ## Shapiro test
shapiro.test(df$Proliferation)
    ## Levene test
leveneTest(df$Proliferation~df$Group, center = mean)
    ## Anova
fit1 <- aov(df$Proliferation~df$Group)
summary(fit1)
TukeyHSD(fit1)
# Make 2 groups data and 2 times
before <- data.frame(
  ctrl = rnorm(30, mean = 5.35, sd = .9),
  mel = rnorm(30, mean = 5.35, sd = .9),
  Group = c('treatment'),
  Time = c('before')
) %>%
  gather(Group, HbA1c, 1:2)
after <- data.frame(
  ctrl = rnorm(30, mean = 3.97, sd = .92),
  mel = rnorm(30, mean = 2.89, sd = .81),
  Group = c('treatment'),
  Time = c('after')
) %>%
  gather(Group, HbA1c, 1:2)

df <- bind_rows(before, after)
df$Group <- factor(
  df$Group,
  levels=c('ctrl','mel'),
  labels=c('Metformin','Melatonin')
)
df$Time <- factor(
  df$Time,
  levels=c('before','after'),
  labels=c('Before','After')
)
str(df)
# Plot
ggplot(df,aes(x = Group, y = HbA1c))+
  geom_violin(trim = F) +
  geom_jitter(height = 0, width = .05, size = .6) +
  scale_x_discrete(name='Group') +
  scale_y_continuous(name='Glycated hemoglobin (HbA1c)') +
  facet_wrap(~ Time) +
  theme_light() +
ggsave('two-way-groups.pdf', width = 5, height = 3, units = 'in', device = 'pdf')
## Shapiro test
shapiro.test(df$HbA1c)
## Levene test
leveneTest(df$HbA1c~df$Group, center = mean)
## Anova
fit2 <- aov(HbA1c ~ Group * Time, data = df)
summary(fit2)
TukeyHSD(fit2)