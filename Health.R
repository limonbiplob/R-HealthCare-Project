##Read data file

Hospital <- read.csv(file.choose(),header = T)
View(Hospital)

hist(Hospital$AGE,
     main="Frequency of Patients",
     col ="green",
     xlab = "Age")
attach(Hospital)
AGE <-as.factor(AGE)
summary(AGE)

View(summary(AGE))

##Aggregate function is used to add the expenditure from each age

aggregate(TOTCHG~AGE,FUN = sum,data = Hospital)

##Max
max(aggregate(TOTCHG~AGE,FUN = sum,data = Hospital))


hist(APRDRG,
     col = "green",
     main = "Frequency of Treatments",
     xlab = "Treatment Categories")

APRDRG_fact <-as.factor(Hospital$APRDRG)

summary(APRDRG_fact)

#Max 
which.max(summary(APRDRG_fact))
df<-aggregate(TOTCHG~APRDRG,FUN = sum,data = Hospital)
df

#Total charge
df[which.max(df$TOTCHG),]


#First remove "NA" value
Hospital<-na.omit(Hospital)

#Factorize the Race variable
Hospital$RACE<-as.factor(Hospital$RACE)

#ANOVA function with TOTCHG and RACE Variable.

model_aov<-aov(TOTCHG~RACE,data = Hospital)

#ANOVA RESULTS
model_aov

summary(model_aov)

#Getting max hospital cost per race

summary(Hospital$RACE)

#Analyze the severity of costs
Hospital$FEMALE<-as.factor(Hospital$FEMALE)

#calling Regression function

model_lm4 <-lm(TOTCHG~AGE+FEMALE,data = Hospital)
summary(model_lm4)

#comparing genders
summary(Hospital$FEMALE)

#Linier Regression
Hospital$RACE<-as.factor(Hospital$RACE)

model_lm5<-lm(LOS~AGE+FEMALE+RACE,data = Hospital)

summary(model_lm5)

#Linier Regressin
model_lm6<-lm(TOTCHG~AGE+FEMALE+RACE+LOS+APRDRG,data = Hospital)
summary(model_lm6)

