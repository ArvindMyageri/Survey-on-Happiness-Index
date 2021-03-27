setwd("F:/Statistics/Mini Project/Survey on Happiness Index")
dir()
mydata<-read.csv("Happiness Index(1).csv",header = T ,sep=",")
View(mydata)
attach(mydata)
head(mydata)
str(mydata)
dim(mydata)
names(mydata)
summary(mydata[,1])
summary(mydata[,2])
summary(mydata[,3])
summary(mydata[,4])
Work.Experience.in.Years.[is.na(Work.Experience.in.Years.)]=8
summary(mydata[,5])
Age[is.na(Age)]=32
summary(mydata[,6])
summary(mydata[,7])
Household.Size[is.na(Household.Size)]=4
mean(Household.Size)
mydata[,8][is.na(mydata[,8])]="Married"
summary(mydata[,8])
summary(mydata[,9])
mydata[,9][is.na(mydata[,9])]=4
summary(mydata[,9])
summary(mydata[,10])
summary(mydata[,11])
summary(mydata[,12])
summary(mydata[,13])
summary(mydata[,14])
class(mydata[,15])
summary(mydata[,15])
mydata[,15][is.na(mydata[,15])]=0
summary(mydata[,16])
mydata[,16][is.na(mydata[,16])]=0
summary(mydata[,17])
is.na(mydata[,17])
summary(mydata[,18])
is.na(mydata[,18])
summary(mydata[,19])
is.na(mydata[,19])
summary(mydata[,20])
is.na(mydata[,20])
mydata[,20][is.na(mydata[,20])]=8
summary(mydata[,21])
is.na(mydata[,21])
summary(mydata[,22])
is.na(mydata[,22])
summary(mydata[,23])
is.na(mydata[,23])
summary(mydata[,24])
is.na(mydata[,24])
mydata[,24][is.na(mydata[,24])]="No"
summary(mydata[,25])
mydata[,26][is.na(mydata[,26])]="Always"
summary(mydata[,26])
summary(mydata[,27])
mydata[,28][is.na(mydata[,28])]="Sometimes"
summary(mydata[,28])
mydata[,29][is.na(mydata[,29])]="Sometimes"
summary(mydata[,29])
summary(mydata[,30])
summary(mydata[,31])
mydata[,32][is.na(mydata[,32])]="Good"
summary(mydata[,32])
mydata[,33][is.na(mydata[,33])]="Satisfied"
summary(mydata[,33])
mydata[,34][is.na(mydata[,34])]="Yes"
summary(mydata[,34])
mydata[,35][is.na(mydata[,35])]="Yes"
summary(mydata[,35])
mydata[,36][is.na(mydata[,36])]="No"
summary(mydata[,36])
mydata[,37][is.na(mydata[,37])]="No"
summary(mydata[,37])
mydata[,38][is.na(mydata[,38])]="Yes"
summary(mydata[,38])
summary(mydata)
str(mydata)
names(mydata)
View(mydata)

plot(Age,Work.Experience.in.Years.)
boxplot(mydata$Overall.level.of.Happiness~ mydata$Who.do.you.live.with.)

boxplot(mydata$Overall.level.of.Happiness~mydata$Where.do.you.Live.,las=1,ylab="Happiness.Index.(Responses)")


qqnorm(mydata$On.a.Scale.of.1.to.10..how.would.you.rate.the.following...Overall.level.of.Happiness)
qqline(mydata$On.a.Scale.of.1.to.10..how.would.you.rate.the.following...Overall.level.of.Happiness)

shapiro.test(mydata$On.a.Scale.of.1.to.10..how.would.you.rate.the.following...Overall.level.of.Happiness)

qqnorm(mydata$On.a.Scale.of.1.to.10..how.would.you.rate.the.following...Level.Of.Happiness.With.Students)
qqline(mydata$On.a.Scale.of.1.to.10..how.would.you.rate.the.following...Level.Of.Happiness.With.Students)

qqnorm(Living.Standard.Staisfaction)
qqline(Living.Standard.Staisfaction)

qqnorm(Marriage.life)
qqline(Marriage.life)

qqnorm(Overall.Health)
qqline(Overall.Health)

qqnorm(Satisfaction.with.job)
qqline(Satisfaction.with.job)

class(Do.you.have.family.friends.to.relay.on.in.times.of.trouble.)
as.numeric(Do.you.have.family.friends.to.relay.on.in.times.of.trouble.)
class(Marital.Status)
mean(Age[Household.Income=="7-10 Lakh"])
count=table(Marital.Status)
percent=count/90

pie(percent)
box()
barplot(percent,las=1)
boxplot(mydata$On.a.Scale.of.1.to.10..how.would.you.rate.the.following...Overall.level.of.Happiness[Gender=="Female"])

class(Gender)
class(Do.you.feel.your.life.is.meaningful.)
Tab1<-table(mydata$Gender,mydata$Are.you.Optimistic.about.your.future.)
Tab1
barplot(Tab1,beside=T,legend=T)
chisq.test(Tab1,correct = T) # There is association between gender and meaningfulness in life.
Chi1=chisq.test(Tab1,correct = T)
Chi1
attributes(Chi1)
Chi1$expected

Tab2<-table(Gender,Are.you.Optimistic.about.your.future.)
Tab2
barplot(Tab2,beside=T,legend=T)
chisq.test(Tab2,correct = T)  # Both are independent.
Chi2=chisq.test(Tab2,correct = T)
Chi2
attributes(Chi2)
Chi2$expected

y=Overall.level.of.Happiness
x1<-Marriage.life
x2<-Living.Standard.Staisfaction
x3<-Satisfaction.with.job
x4<-Level.Of.Happiness.With.Students
x5=Overall.Health
summary(Go.out.with.your.friends.for.Trip.Shopping.Dinner)
mydata[,30][is.na(mydata[,30])]="Sometimes"
Marital.Status[is.na(Marital.Status)]="Married"
summary(Marital.Status)
summary(mydata[,10])
mydata[,10][is.na(mydata[,10])]="3-5 Lakh"
x6<-mydata[,10]
x6
summary(mydata[,27])
mydata[,27][is.na(mydata[,27])]="Very Often" 
x7<-mydata[,27]
x7
is.na(Overall.level.of.Happiness)
Marriage.life[is.na(Marriage.life)]=0
summary(Marriage.life)
is.na(Living.Standard.Staisfaction)
is.na(Satisfaction.with.job)
is.na(Level.Of.Happiness.With.Students)
Overall.Health[is.na(Overall.Health)]=0
summary(Overall.Health)

model1<-lm(y~x1+x2+x3+x4+x5+x6+x7,data=mydata)
summary(model1)
model11<-summary(aov(y~x,data=mydata))
model11
391.2+80.6
x<-matrix(c(rep(1,90),x1,x2,x3,x4,x5),ncol=6)
x
cor(x)
par(mfrow=c(2,2))
plot(model1)

par(mfrow=c(1,1))

n=ifelse(mydata$Participate.in.a.spiritual.Community.or.Group.Activites=="Always",4,
         ifelse(Participate.in.a.spiritual.Community.or.Group.Activites=="Very often",3,
                ifelse(Participate.in.a.spiritual.Community.or.Group.Activites=="Sometimes",2,
                       ifelse(Participate.in.a.spiritual.Community.or.Group.Activites=="Rarely",1,
                              ifelse(mydata$Participate.in.a.spiritual.Community.or.Group.Activites=="Never",0
                                     )))))
n

q1
prop.test(Marital.Status,Overall.level.of.Happiness)


boxplot(Age,Participate.in.a.spiritual.Community.or.Group.Activites)
t<-table(Participate.in.a.spiritual.Community.or.Group.Activites,Age)
t
prop.test(Participate.in.a.spiritual.Community.or.Group.Activites,Age)
chisq.test(t) # Taken 2 attributes are independent.
barplot(t,beside = T,legend=T,las=1)

Tab3<-table(Go.out.with.your.friends.for.Trip.Shopping.Dinner,Marital.Status)
Tab3
chisq.test(Tab3,correct = T)
# Outing with friends is associated with their marital status.But not with family
Tab4<-table(Go.out.with.your.family.for.Trip.Shopping.Dinner.etc..,Marital.Status)
Tab4
chisq.test(Tab4,correct = T)
detach(mydata)
