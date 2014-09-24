####Decision Trees####
library(tree)
library(ISLR)
data$High=ifelse (data$avg_trans_per_merchant <=20.00," 0"," 1 ")
tree.data =tree(avg_trans_per_merchant???.-pcnt_trans_utilities- pcnt_trans_travel-pcnt_trans_personal-pcnt_trans_leisure-pcnt_trans_others-pcnt_trans_leisure_travel-pcnt_trans_homedecor-pcnt_trans_healthcare-pcnt_trans_education ,
                data=data)
#tree.data =tree(data$High???.,data )
summary(tree.data)

####Boosting####
library(gbm)
boost.boston =gbm(avg_trans_per_merchant???.-High-pcnt_trans_utilities- pcnt_trans_travel-pcnt_trans_personal-pcnt_trans_leisure-pcnt_trans_others-pcnt_trans_leisure_travel-pcnt_trans_homedecor-pcnt_trans_healthcare-pcnt_trans_education ,
                  data=data, distribution=
                    "gaussian",n.trees =5000 , interaction.depth =4)
summary(boost.boston)

#####Bagging####
library(randomForest)
bag.boston =randomForest(avg_trans_per_merchant???.-High-pcnt_trans_utilities- pcnt_trans_travel-pcnt_trans_personal-pcnt_trans_leisure-pcnt_trans_others-pcnt_trans_leisure_travel-pcnt_trans_homedecor-pcnt_trans_healthcare-pcnt_trans_education ,
                         data=data,
                         mtry=13, importance =TRUE)
summary(bag.boston)

######SUPPORT VECTOR MACHINE#############
library(ISLR)
out=svm(avg_trans_per_merchant???.-High-pcnt_trans_utilities- pcnt_trans_travel-pcnt_trans_personal-pcnt_trans_leisure-pcnt_trans_others-pcnt_trans_leisure_travel-pcnt_trans_homedecor-pcnt_trans_healthcare-pcnt_trans_education ,
        +         data=data, kernel ="linear",cost =10)

summary(out)

########BACKPROPAGATION################
library(neuralnet)
n <- names(data)
#f <- as.formula(paste("avg_trans_per_merchant ~", paste(n[!n %in% "avg_trans_per_merchant"], collapse = " + ")))
#nn<-neuralnet(avg_trans_per_merchant???.-High-pcnt_trans_utilities- pcnt_trans_travel-pcnt_trans_personal-pcnt_trans_leisure-pcnt_trans_others-pcnt_trans_leisure_travel-pcnt_trans_homedecor-pcnt_trans_healthcare-pcnt_trans_education,data=data,hidden=2, err.fct="ce", linear.output=FALSE)
f<-as.formula(avg_trans_per_merchant ~  num_distinct_merchants + num_trans_2012 + 
                            num_trans_2013 + pcnt_trans_q3 + pcnt_trans_q4 + pcnt_trans_q1 + 
                           pcnt_trans_q2 + avg_trans_amt + total_num_trans + persn_primary + 
                          persn_secondary + persn_tertiary + persn_others + pos_methd_sign + 
                          pos_methd_pin + pos_methd_phone + pos_methd_others + 
                          
                           pcnt_trans_luxury +                            
                          pcnt_trans_clothing + pcnt_trans_food + pcn_trans_stores + 
                            frequency + onlin_enrolled + onlin_re_enrolled + deliquent_ind + 
                             balcon_ind + cr_low + cr_med + cr_high + revlv_ind + bill_bal_prn_coll_amt + 
                             high_bill_bal_amt )
> nn<-neuralnet(f,data=data, err.fct="sse",hidden=c(3),linear.output=FALSE )