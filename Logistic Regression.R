data<-read.csv('adult20.csv')
data_1<-data[,c("URBRRL",'REGION','AGEP_A','SEX_A','RACEALLP_A','PHSTAT_A','VISIONDF_A','HEARAID_A',
             'DIFF_A','COMDIFF_A','UPPSLFCR_A','PAYBLL12M_A','LASTDR_A',
             'MARITAL_A','CITZNSTP_A','FAMINCTC_A','NOTCOV_A','VIRAPP12M_A','DLYCARE_A')]
#"URBRRL",'REGION','AGEP_A','SEX_A','RACEALLP_A'
#PHSTAT_A: General health status 
#VISIONDF_A: Difficulty seeing
#HEARAID_A:Difficulty hearing
#DIFF_A:Difficulty walking/steps
#COMDIFF_A: Difficulty communicating
#UPPSLFCR_A: Difficulty with self care 
#PAYBLL12M_A:Problems paying medical bills, past 12m
#LASTDR_A: Time since last saw doctor
#MARITAL_A: Sample adult's current marital status 
#CITZNSTP_A:Citizenship status
#FAMINCTC_A:family income 
#NOTCOV_A: Coverage status as used in Health United States
#DLYCARE_A:Delayed medical care due to COVID-19 
#VIRAPP12M_A: Virtual medical appointment, past 12m
data_1<-data_1[which(data_1$SEX_A==1|data_1$SEX_A==2),]
data_1<-data_1[which(data_1$RACEALLP_A!=7&data_1$RACEALLP_A!=8&data_1$RACEALLP_A!=9),]
data_1<-data_1[which(data_1$PHSTAT_A!=7&data_1$PHSTAT_A!=8&data_1$PHSTAT_A!=9),]
data_1<-data_1[which(data_1$VISIONDF_A!=7&data_1$VISIONDF_A!=8&data_1$VISIONDF_A!=9),]
data_1<-data_1[which(data_1$HEARAID_A!=7&data_1$HEARAID_A!=8&data_1$HEARAID_A!=9),]
data_1<-data_1[which(data_1$DIFF_A!=7&data_1$DIFF_A!=8&data_1$DIFF_A!=9),]
data_1<-data_1[which(data_1$COMDIFF_A!=7&data_1$COMDIFF_A!=8&data_1$COMDIFF_A!=9),]
data_1<-data_1[which(data_1$UPPSLFCR_A!=7&data_1$UPPSLFCR_A!=8&data_1$UPPSLFCR_A!=9),]
data_1<-data_1[which(data_1$PAYBLL12M_A!=7&data_1$PAYBLL12M_A!=8&data_1$PAYBLL12M_A!=9),]
data_1<-data_1[which(data_1$LASTDR_A!=7&data_1$LASTDR_A!=8&data_1$LASTDR_A!=9),]
data_1<-data_1[which(data_1$MARITAL_A!=7&data_1$MARITAL_A!=8&data_1$MARITAL_A!=9),]
data_1<-data_1[which(data_1$CITZNSTP_A!=7&data_1$CITZNSTP_A!=8&data_1$CITZNSTP_A!=9),]
data_1<-data_1[which(data_1$NOTCOV_A!=7&data_1$NOTCOV_A!=8&data_1$NOTCOV_A!=9),]
data_1<-data_1[which(data_1$VIRAPP12M_A!=7&data_1$VIRAPP12M_A!=8&data_1$VIRAPP12M_A!=9),]
data_1<-data_1[which(data_1$DLYCARE_A!=7&data_1$DLYCARE_A!=8&data_1$DLYCARE_A!=9),]


data_1$URBRRL<-factor(data_1$URBRRL)
data_1$REGION<-factor(data_1$REGION)
data_1$SEX_A<-factor(data_1$SEX_A)
data_1$RACEALLP_A<-factor(data_1$RACEALLP_A)
data_1$PHSTAT_A<-factor(data_1$PHSTAT_A)
data_1$VISIONDF_A<-factor(data_1$VISIONDF_A)
data_1$HEARAID_A<-factor(data_1$HEARAID_A)
data_1$DIFF_A<-factor(data_1$DIFF_A)
data_1$COMDIFF_A<-factor(data_1$COMDIFF_A)
data_1$UPPSLFCR_A<-factor(data_1$UPPSLFCR_A)
data_1$PAYBLL12M_A<-factor(data_1$PAYBLL12M_A)
data_1$LASTDR_A<-factor(data_1$LASTDR_A)
data_1$MARITAL_A<-factor(data_1$MARITAL_A)
data_1$CITZNSTP_A<-factor(data_1$CITZNSTP_A)
data_1$NOTCOV_A<-factor(data_1$NOTCOV_A)
data_1$VIRAPP12M_A<-factor(data_1$VIRAPP12M_A)
data_1$DLYCARE_A<-factor(data_1$DLYCARE_A)


summary(data_1)
#data_1<-na.omit(data_1)
data_1[is.na(data_1)] <- 0
summary(data_1)
write.csv(x = data_1,file = "data.csv")
table(data_1$LASTDR_A)
table(data_1$PAYBLL12M_A)


#DLYCARE_A:Delayed medical care due to COVID-19
#X<-data_1[,!grepl("DLYCARE_A",colnames(data_1))]
table(data$DLYCARE_A)
X<-data_1[which(data_1$DLYCARE_A==1|data_1$DLYCARE_A==2),]
X$DLYCARE_A<-ifelse(X$DLYCARE_A==1,1,0)
X<-X[,!grepl("VIRAPP12M_A",colnames(X))]
am.data = glm(formula = DLYCARE_A ~., data = X, family = binomial)
summary(am.data)

#VIRAPP12M_A: Virtual medical appointment, past 12m
table(data$VIRAPP12M_A)
X<-data_1[which(data_1$VIRAPP12M_A==1|data_1$VIRAPP12M_A==2),]
X$VIRAPP12M_A<-ifelse(X$VIRAPP12M_A==1,1,0)
X<-X[,!grepl("DLYCARE_A",colnames(X))]
am.data2 = glm(formula = VIRAPP12M_A ~., data = X, family = binomial)
summary(am.data)
table(X$URBRRL,X$VIRAPP12M_A)
y<-predict(am.data)
library(stargazer)
stargazer(am.data, am.data2, title = "results", align = F, type = "text", no.space = TRUE, out = "fit.html")
