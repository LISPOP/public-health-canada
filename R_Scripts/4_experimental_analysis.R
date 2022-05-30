#Experimental Analysis
#Which groups were correct

full$C12
full %>% 
  mutate(correct=case_when(
    GROUP==1 &C12==2 ~ 1,
    GROUP==1 & C12==1 ~ 0,
    GROUP==2 & C12==1 ~ 0,
    GROUP==2 & C12==2 ~ 1,
    GROUP==3 &C34==2 ~ 1,
    GROUP==3 & C34==1 ~ 0,
    GROUP==4 & C34==1 ~ 0,
    GROUP==4 & C34==2 ~ 1,
    GROUP==5 &C56==1 ~ 1,
    GROUP==5 & C56==2 ~ 0,
    GROUP==6 & C56==1 ~ 0,
    GROUP==6 & C56==2 ~ 1
  ))->full

val_labels(full$correct)<-c(1=correct, 0=incorrect)
#Model 1
#In groups 1:2, skin rash condition, Pr of getting it correct should be related to 
# science literacy, CRt and not related to ideology
full$Sample
full %>% 
  filter(Sample=="General Population")->full
model1<-lm(correct ~ mean_crt, data=subset(full, GROUP>3))
model2<-lm(correct ~ mean_know, data=subset(full, GROUP>3))
summary(model1)
summary(model2)

#IN group 3:4 on harm prescription grade heroin, drugs

lookfor(full, "drugs")
full$harm_reduction<-skpersonal::revScale(as.numeric(full$Q13_2))

#Group3 is the non-harm reduction group
#Group 4 is the harm reduction group
#People agree that reducing harm from drugs is important are threatened by superficial results in group4
#CRT should be related to P of getting it correct in group 4, not group3
model3<-lm(correct ~ harm_reduction*mean_crt, data=subset(full, GROUP==3))
model4<-lm(correct ~ harm_reduction*mean_crt, data=subset(full, GROUP==4))
summary(model3)
summary(model4)
full$seniors_isolation
  full$isolate<-skpersonal::revScale(full$seniors_isolation, reverse=T)
full$isolate
table(full$isolate, full$seniors_isolation)
val_labels(full$seniors_isolation)
# 1 is the tough position, 0 is the relaxed position
full$C56
#Group 5 threatens those with a low score
#group 6 threatens those with a high score
# So the interaction signs should e the opposite
model5<-lm(correct ~ isolate*mean_crt, data=subset(full, GROUP==5))
model6<-lm(correct ~ isolate*mean_crt, data=subset(full, GROUP==6))
summary(model5)
summary(model6)

library(ggeffects)
?ggpredict
ggpredict(model4, terms=c("harm_reduction[0:1]", "mean"))
