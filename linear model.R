
mynew$transactionRevenue
mynew$LogRevenue
mynew=mynew%>% mutate(transactionRevenue=ifelse(is.na(transactionRevenue)
                                                    ,0,transactionRevenue*10^6))
mynew = mynew %>% mutate(LogRevenue=log(transactionRevenue+1))
write.csv(mynew,"2.27_train.csv")
mynew = mynew %>% mutate(is.holiday= ifelse(month=="December",1,0))
class(mynew$is.holiday)
#USA
lreg_USA <- mynew %>% filter(IsTransaction == 1 )
linear1=lm(LogRevenue ~pageviews+pageviews_sqr+log(visitNumber)+as.factor(medium)+as.factor(is.weekend)
           + peakhour+ + isMobile + is.holiday 
           #+ as.factor(isMobile)*as.factor(medium)
           ,data=lreg_USA)
summary(linear1)
pred.revenue_USA=predict.lm(linear1,lreg_USA)
exppred_USA=pred.revenue_USA*predict.logit_USA 
exppred_USA
RMSE=sqrt(mean((exppred_USA-temp_USA$LogRevenue)^2))
RMSE

#NUSA
lreg_NUSA <- mynew %>% filter(IsTransaction == 1 & is.USA==0)
linear2=lm(LogRevenue~pageviews+pageviews_sqr+log(visitNumber)+as.factor(medium)+as.factor(is.weekend)
           ,data=lreg_NUSA)
summary(linear2)
pred.revenue_NUSA=predict.lm(linear2,lreg_NUSA)
exppred_NUSA=pred.revenue_NUSA*predict.logit_NUSA
exppred_NUSA
RMSE=sqrt(mean((exppred_NUSA-temp_NUSA$LogRevenue)^2))
RMSE
