setwd('/Users/Cecile/Documents/MA_speech_pref')
DB<-read.csv('/Users/Cecile/Documents/MA_speech_pref/speech_pref_full_DB.csv')
DB$age_months = DB$mean_age_1/30.44

ggplot(data=DB, aes(x=age_months, y=g_calc))+
  geom_point(aes(size=1/DB[!is.na(DB$test_lang),]$g_var_calc), show.legend = F, na.rm = TRUE)+
  stat_smooth(method = "lm", na.rm = TRUE, color='gray40')+
  scale_x_continuous(name = "Mean age (months)", breaks = seq(0,12,1))+
  scale_y_continuous(name = "Effect size (Hedge's g)", limits = c(-3, 3), breaks = seq(-3,3,1))+
  theme_classic()+
  theme(text = element_text(size = 20))

ggsave("age.pdf")

ggplot(data=DB[!is.na(DB$test_lang),],aes(x=age_months, y=g_calc, colour=test_lang))+
  geom_point(aes(size=1/DB[!is.na(DB$test_lang),]$g_var_calc), show.legend = F, na.rm = TRUE)+
  scale_size(range = c(1, 5))+
  geom_smooth(data=subset(DB, outlier = F), method = "lm", na.rm = TRUE)+
  scale_x_continuous(name = "Mean age (months)", breaks = seq(0,12,1))+
  scale_y_continuous(name = "Effect size (Hedge's g)", limits = c(-3, 3), breaks = seq(-3,3,1))+
  scale_colour_manual(values = c("red3","skyblue"), labels = c("foreign","native"), name = "speech sound")+
  theme_classic()+
  theme(text = element_text(size = 20), legend.position = c(.9, .9))

ggsave("lang-age.pdf")

ggplot(data=DB[!is.na(DB$natural),],aes(x=natural, y=g_calc))+
  geom_jitter(aes(x=natural, y=g_calc, size=1/DB[!is.na(DB$natural),]$g_var_calc), position=position_jitter(w=0.15, h = 0), show.legend = F, na.rm = TRUE)+
  stat_summary(fun.data="mean_cl_normal", colour = c("mediumvioletred","green3"), na.rm=T)+
  scale_x_discrete(name = "other sound", labels=c("artificial","natural"), na.translate = FALSE)+
  scale_y_continuous(name = "Effect size (Hedge's g)", limits = c(-3, 3), breaks = seq(-3,3,1))+
  theme_classic()+
  theme(text = element_text(size = 20))

ggsave("scatter_natural.pdf")

ggplot(data=DB[!is.na(DB$natural),], aes(x=age_months, y=g_calc, colour=natural))+
  geom_point(aes(size=1/DB[!is.na(DB$natural),]$g_var_calc), show.legend = T, na.rm = TRUE)+
  stat_smooth(data=subset(DB, outlier = F), method = "glm", na.rm = TRUE)+
  scale_x_continuous(name = "Mean age (months)", breaks = seq(0,12,1))+
  scale_y_continuous(name = "Effect size (Hedge's g)", limits = c(-3, 3), breaks = seq(-3,3,1))+
  scale_colour_manual(values = c("mediumvioletred","green3"), labels=c("artificial","natural"), name = "other sound")+
  theme_classic()+
  theme(text = element_text(size = 20), legend.position = c(.9, .9))+
  facet_grid(. ~test_lang)

ggsave("natural-age.pdf")

ggplot(data=DB[!is.na(DB$vocal),],aes(x=vocal, y=g_calc))+
  geom_jitter(aes(x=vocal, y=g_calc, size=1/DB[!is.na(DB$vocal),]$g_var_calc), position=position_jitter(w=0.1, h = 0), show.legend = F, na.rm = TRUE)+
  stat_summary(fun.data="mean_cl_normal", colour = c("turquoise2","tomato"), na.rm=T)+
  scale_x_discrete(name = "other sound", na.translate = FALSE, labels = c("non-vocal", "vocal"))+
  scale_y_continuous(name = "Effect size (Hedge's g)", limits = c(-3, 3), breaks = seq(-3,3,1))+
  theme_classic()+
  theme(text = element_text(size = 20))

ggsave("scatter_vocal.pdf")

ggplot(data=DB[!is.na(DB$vocal),],aes(x=age_months, y=g_calc, colour=vocal))+
  geom_point(aes(size=1/DB[!is.na(DB$vocal),]$g_var_calc), show.legend = F, na.rm = TRUE)+
  scale_size(range = c(1, 5))+
  stat_smooth(data=subset(DB, outlier = F), method = "glm", na.rm = TRUE)+
  scale_x_continuous(name = "Mean age (months)", breaks = seq(0,12,1))+
  scale_y_continuous(name = "Effect size (Hedge's g)", limits = c(-3, 3), breaks = seq(-3,3,1))+
  scale_colour_manual(values = c("turquoise2","tomato"), labels = c("non-vocal","vocal"), name = "other sound")+
  theme_classic()+
  theme(text = element_text(size = 20), legend.position = c(.9, .9))

ggsave("vocal-age.pdf")
