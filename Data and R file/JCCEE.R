allbus16t <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
lapply(list('dplyr', 'tibble', 'effects','tidyr', 'ggplot2'), 
       require,character.only = TRUE)

# Table 1#
#Figures for Germany overall#

allbus16t %>% filter(!is.na(pride)) %>% 
  group_by(year) %>% count(pride) %>% 
  mutate(perc=n/sum(n)*100)

# Figures split by east-west #
allbus16t %>% filter(!is.na(pride)) %>% 
  group_by(year, east1) %>% count(pride) %>% 
  mutate(perc=n/sum(n)*100) %>% 
  print(n=Inf)


# Table 2: GLMs#
pb.mod <- glm(pride ~ pvalues + degree + lr + strust + 
                pinterest + econger + econger1 + resecon + resecon1 +
                satdem + trustdi + east + sex1 + agecat, weights=wghtpew, family=binomial(link="logit"))

pbw.mod <- glm(pride ~ pvalues + degree + lr + strust + 
                  pinterest + econger + econger1 + resecon + resecon1 +
                  + satdem + trustdi + sex1 + agecat,family=binomial(link="logit"), data=allbus16t, subset=east==0)
pbe.mod <- glm(pride ~ pvalues + degree + lr + strust + 
                 pinterest + econger + econger1 + resecon + resecon1 +
                 + satdem + trustdi+ sex1 + agecat,family=binomial(link="logit"), data=allbus16t, subset=east==1)

summary(pb.mod)
summary(pbw.mod)
summary(pbe.mod)

library(sjPlot)
sjt.glm(pb.mod, exp.coef=F, show.se=T, show.ci=F, show.r2=T, show.loglik = FALSE, show.aic = FALSE, show.aicc = FALSE)
sjt.glm(pbw.mod, exp.coef=F, show.se=T, show.ci=F, show.r2=T, show.loglik = FALSE, show.aic = FALSE, show.aicc = FALSE)
sjt.glm(pbe.mod, exp.coef=F, show.se=T, show.ci=F, show.r2=T, show.loglik = FALSE, show.aic = FALSE, show.aicc = FALSE)

#Plots for Models#
#All-German#

#Postmaterialism#
myda <- ggeffect(pb.mod, terms="pvalues", ci.lvl=0.95)

a <- tibble(x=1:4, predicted=c(0.423, 0.505, 0.523, 0.567),
            conf.low=c(0.382, 0.475, 0.493, 0.530),
            conf.high=c(0.466, 0.535, 0.553, 0.604))

b <- ggplot(a, aes(x, predicted)) + 
  geom_point() + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), alpha=.8, width=.1)+
  labs(x="", y="Probability Pride", title="Postmaterialism")+
  theme_bw()+
  theme(plot.title = element_text(size=14))

b + scale_x_continuous(labels=c("1" = "materialist", "2" = "materialist-mixed",
                                "3" = "postmaterialist-mixed", "4"="postmaterialist"))
#Ideology#

mydf2 <- ggeffect(pb.mod, terms="lr", ci.lvl=0.95)

a <- ggplot(mydf2, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Ideology")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))
a + scale_x_continuous(breaks=c(1,5), labels=c("1" = "extreme", "5" = "centre"))

#Social Trust#

mydf3 <- ggeffect(pb.mod, terms="strust", ci.lvl=0.95)

d <- tibble(x=1:3, predicted=c(0.488, 0.543, 0.502),
            conf.low=c(0.461, 0.515, 0.464),
            conf.high=c(0.515, 0.570, 0.539))

e <- ggplot(d, aes(x, predicted)) + 
  geom_point() + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), alpha=.8, width=.1)+
  labs(x="", y="Probability Pride", title="Social Trust")+
  theme_bw()+
  theme(plot.title = element_text(size=14))

e + scale_x_continuous(breaks=c(1, 2, 3), labels=c("too careful", "depends", "trusted"))

#Political Interest#
mydf3 <- ggeffect(pb.mod, terms="pinterest", ci.lvl=0.95)

a <- ggplot(mydf3, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymn = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Political Interest")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))
a + scale_x_continuous(breaks=c(0,1,2,3,4), labels=c("0" = "none", "1" = "little","2" = "middle", "3"="strong", "4"="very strong"))

#Evaluation of the future of the National Economy#

mydf4 <- ggeffect(pb.mod, terms="econger1", ci.lvl=0.95)
a <- ggplot(mydf4, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Future National Economy")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))
a + scale_x_continuous(breaks=c(0,1,2,3,4), labels=c("0" = "none", "1" = "little","2" = "middle", "3"="strong", "4"="very strong"))


#Satisfaction with democracy#
mydf4 <- ggeffect(pb.mod, terms="satdem", ci.lvl=0.95)

a <- ggplot(mydf4, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Satisfaction with Democracy")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))
a + scale_x_continuous(breaks=c(0,1,2,3,4,5), labels=c("0" = "very dissatisfied", "1" = "quite","2" = "somewhat", "3"="somewhat", "4"="quite", "5"="very satisfied"))

#Trust in Democratic Institutions#
mydf5 <- ggeffect(pb.mod, terms="trustdi", ci.lvl=0.95)

a <- ggplot(mydf5, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Trust in Democratic Institutions")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))
a + scale_x_continuous(breaks=c(0,24), labels=c("0" = "low trust", "24" = "high trust"))

#East#

mydf7 <- ggeffect(pb.mod, terms="east", ci.lvl=0.95)
h <- ggplot(mydf7, aes(x, predicted)) + 
  geom_point() + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), alpha=.8, width=.1)+
  labs(x="", y="Probability Pride", title="West-East")+
  theme_bw()+
  theme(plot.title = element_text(size=14))

h + scale_x_continuous(breaks=c(0,1), labels=c("0" = "west", "1" = "east"))


#Age#

mydf6 <- ggeffect(pb.mod, terms="agecat", ci.lvl=0.95)

h <- ggplot(mydf6, aes(x, predicted)) + 
  geom_point() + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), alpha=.8, width=.1)+
  labs(x="", y="Probability Pride", title="Age")+
  theme_bw()+
  theme(plot.title = element_text(size=14))

h + scale_x_continuous(labels=c("1" = "18-29", "2" = "30-44","3" = "45-59", "4"="60-74", "5"="75+"))

#Plots for Western and Eastern Models#

library(ggeffects)

#Political interest#

emodpi <- ggeffect(pbe.mod, terms="pinterest", ci.lvl=0.95)
ggplot(emodpi, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Political Interest (East)")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))+ 
  scale_x_continuous(breaks=c(0,1,2,3,4), labels=c("0" = "none", "1" = "little","2" = "middle", "3"="strong", "4"="very strong"))

wmodpi <- ggeffect(pbw.mod, terms="pinterest", ci.lvl=0.95)
ggplot(wmodpi, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Political Interest (West)")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))+ 
  scale_x_continuous(breaks=c(0,1,2,3,4), labels=c("0" = "none", "1" = "little","2" = "middle", "3"="strong", "4"="very strong"))

#Ideology#
emodid <- ggeffect(pbe.mod, terms="lr", ci.lvl=0.95)
ggplot(emodid, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Ideology (East)")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))+
  scale_x_continuous(breaks=c(1,5), labels=c("1" = "extreme", "5" = "centre"))

wmodid <- ggeffect(pbw.mod, terms="lr", ci.lvl=0.95)
ggplot(wmodid, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha=.2)+
  labs(x="", y="Probability Pride", title="Ideology (West)")+ 
  theme_bw() +
  theme(plot.title = element_text(size=14))+
  scale_x_continuous(breaks=c(1,5), labels=c("1" = "extreme", "5" = "centre"))

#Age#
emodag<- ggeffect(pbe.mod, terms="agecat", ci.lvl=0.95)
ggplot(emodag, aes(x, predicted)) + 
  geom_point() + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), alpha=.8, width=.1)+
  labs(x="", y="Probability Pride", title="Age (East)")+
  theme_bw()+
  theme(plot.title = element_text(size=14))+
  scale_x_continuous(labels=c("1" = "18-29", "2" = "30-44","3" = "45-59", "4"="60-74", "5"="75+"))

wmodag<- ggeffect(pbw.mod, terms="agecat", ci.lvl=0.95)
ggplot(wmodag, aes(x, predicted)) + 
  geom_point() + geom_errorbar(aes(ymin=conf.low, ymax=conf.high), alpha=.8, width=.1)+
  labs(x="", y="Probability Pride", title="Age (West)")+
  theme_bw()+
  theme(plot.title = element_text(size=14))+
  scale_x_continuous(labels=c("1" = "18-29", "2" = "30-44","3" = "45-59", "4"="60-74", "5"="75+"))

rm(a,b,d,e,h,myda,mydf2,mydf3,mydf4,mydf5,mydf6,mydf7,pb.mod,pbe.mod,pbw.mod, emodag, emodid, emodpi, wmodag, wmodid, wmodpi)



