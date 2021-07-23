library(Hmisc)
library(rms)
library(lattice)
library(MASS)
stepAIC(m3,direction="backward")
library(ggplot2)

titanic <- read.csv("http://www.math.ku.dk/~susanne/titanic.txt",header = TRUE,colClasses = c("factor", "integer","factor", "numeric", "integer", "integer"))
summary(titanic)

a <- aregImpute(~pclass+survived+sex+age+sibsp+parch,n.impute = 5,nk=0,data = titanic)
f <- fit.mult.impute(survived~pclass+sex+age+sibsp+parch,fitter = function(...) glm(..., family = binomial(link="logit")),xtrans = a,data = titanic)

titanic1 <- f$data

m1<-glm(survived~pclass+sex+age+sibsp+parch,family = binomial(link = "logit"),data = f$data)
print(m1)
step(m1,direction = "backward")
anova(m1)
        
m2<-glm(survived~pclass+sex+age+sibsp+parch,family = binomial(link = "logit"),data = f$data)
m3<-glm(survived~pclass*sex*age*sibsp,family = binomial(link = "logit"),data = f$data)
m4<-glm(survived~sex+pclass*age*sibsp,family = binomial(link = "logit"),data = f$data)
m5<-glm(survived~pclass+sex+age+sibsp+pclass:sex + 
          pclass:age + sex:age + pclass:sibsp + age:sibsp + pclass:age:sibsp,family = binomial(link = "logit"),data = f$data)

#step(m3,direction = "backward")
stepAIC(m3,direction="backward")
anova(m5,m4)

diagmd<-fortify(m6,titanic1)

contVar <- c("pclasee","age","sex","survived","parch","sibsp")
#mf <- melt(titanic1[,contVar])

binScale <- scale_fill_continuous(low="gray80",high="black",trans="log",guide="none")
mdiag <- fortify(m6,titanic1)
p1<-qplot(.fitted,.stdresid,data=mdiag,geom = "hex")+binScale+geom_smooth(size=1)+xlab("fitted values")+ylab("standardized residuals")
#p2<-qplot(survived,.stdresid,data = mdiag,geom="hex")+binScale+stat_binhex(bins = 25)+geom_smooth(size =1)+xlab("survived")+ylab("")
p3<-qplot(sample=.stdresid,data=mdiag,stat="qq")+geom_abline(intercept = 0,slope = 1,color="blue",size = 1)+xlab("theoretibal quantiles")+ylab("")
#grid.arrange(p1,p2,p3,ncol=3)
nsg <- function(x)
form<-survived~nsg(age)+sex+pclass*sibsp
lmm <- glm(form,data=titanic1)
anova(m6,lmm)

titanic1$sibsp[titanic1$sibsp >2] <- 2
titanic1$sibsp <- factor(titanic1$sibsp)
m6<-glm(survived~age+pclass*sex+sibsp,family = binomial(link = "logit"),data = titanic1)
qplot(.fitted,.stdresid,data=mdiag,geom = "hex")+binScale+geom_smooth(size=1)+xlab("fitted values")+ylab("standardized residuals")
qplot(sample=.stdresid,data=mdiag,stat="qq")+geom_abline(intercept = 0,slope = 1,color="blue",size = 1)+xlab("theoretibal quantiles")+ylab("")






  