#Check data
dim(VROC_2021_updated2)
library(ggkm)
ggkm(survfit(Surv(OS_days,Death_evemt)~(RPV_catnew),data=subset(VROC_2021_updated2,Grade>2)),table=T) #result same as Fig2A

#Residual disease subgroup survival
##cat by RPV high vs med/lo

VROC_2021_updated2$RPV_Residual4_catbyhi[VROC_2021_updated2$RPV_catnew==3 & VROC_2021_updated2$"Residual_code"==0]=1
VROC_2021_updated2$RPV_Residual4_catbyhi[VROC_2021_updated2$RPV_catnew==3 & VROC_2021_updated2$"Residual_code"==1]=2
VROC_2021_updated2$RPV_Residual4_catbyhi[VROC_2021_updated2$RPV_catnew<3 & VROC_2021_updated2$"Residual_code"==0]=3
VROC_2021_updated2$RPV_Residual4_catbyhi[VROC_2021_updated2$RPV_catnew<3 & VROC_2021_updated2$"Residual_code"==1]=4
table(VROC_2021_updated2$RPV_Residual4_catbyhi)

ggkm(survfit(Surv(OS_days/30.5,Death_evemt)~(RPV_Residual4_catbyhi),data=subset(VROC_2021_updated2,Grade>2)),table=T,ystratalabs=c("RPVhi&noRes","RPVhi&Res","RPVmedlo&noRes","RPVmedlo&Res"),ystrataname="Group",pval=T)

ggkm(survfit(Surv(PFS_days/30.5,Relapse_event)~(RPV_Residual4_catbyhi),data=subset(VROC_2021_updated2,Grade>2)),table=T,ystratalabs=c("RPVhi&noRes","RPVhi&Res","RPVmedlo&noRes","RPVmedlo&Res"),ystrataname="Group",pval=T)


##cat by RPV highmed vs low

VROC_2021_updated2$RPV_Residual4_catbymed[VROC_2021_updated2$RPV_catnew==2 & VROC_2021_updated2$"Residual_code"==0]=1
VROC_2021_updated2$RPV_Residual4_catbymed[VROC_2021_updated2$RPV_catnew==2 & VROC_2021_updated2$"Residual_code"==1]=2
VROC_2021_updated2$RPV_Residual4_catbymed[VROC_2021_updated2$RPV_catnew<2 & VROC_2021_updated2$"Residual_code"==0]=3
VROC_2021_updated2$RPV_Residual4_catbymed[VROC_2021_updated2$RPV_catnew<2 & VROC_2021_updated2$"Residual_code"==1]=4
table(VROC_2021_updated2$RPV_Residual4_catbymed)

ggkm(survfit(Surv(OS_days/30.5,Death_evemt)~(RPV_Residual4_catbymed),data=subset(VROC_2021_updated2,Grade>2)),table=T,ystratalabs=c("RPVhi&noRes","RPVhi&Res","RPVmedlo&noRes","RPVmedlo&Res"),ystrataname="Group",pval=T)

ggkm(survfit(Surv(PFS_days/30.5,Relapse_event)~(RPV_Residual4_catbymed),data=subset(VROC_2021_updated2,Grade>2)),table=T,ystratalabs=c("RPVhi&noRes","RPVhi&Res","RPVmedlo&noRes","RPVmedlo&Res"),ystrataname="Group",pval=T)


#Double check PFS from TABLE 1
ggkm(survfit(Surv(PFS_days/30.5,Relapse_event)~(cohort),data=x1),table=T,pval=T)

