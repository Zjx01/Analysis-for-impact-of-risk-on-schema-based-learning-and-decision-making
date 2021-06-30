#schema
library(R.matlab)
qdata <- readMat("~/Desktop/PON3/week8/schema/quotes.mat")
pdata <- readMat("~/Desktop/PON3/week8/schema/paintings.mat")

painting_participant_pre <- read.csv("/Users/zhaojingxian/Desktop/PON3/week8/schema/Questionnaire\ answers/Answers\ -\ paintings-Table\ 1.csv")
schema_payoff<-read.csv('/Users/zhaojingxian/Desktop/PON3/week8/schema/Questionnaire\ answers/Payoff\ -\ paintings-Table\ 1.csv',header = F)
schema_score<-schema_payoff[1:15,2]


head(painting_participant_pre)
quote_participant_pre<-read.csv('/Users/zhaojingxian/Desktop/PON3/week8/schema/Questionnaire\ answers/Answers\ -\ quotes-Table\ 1.csv')
head(quote_participant_pre)
p_basic<-data.frame(painting_participant_pre[1:15,1:2])
dim(pdata$Dectimes[[15]][[1]])#24 5
dim(pdata$Dectimes[[15]][[1]])[1]#选择第15个participant decision session的轮数
pdata$Dectimes[[15]][[1]][1,]#第15个participant 的第一轮decision session
pdata$Payoffs[[15]][[1]]
a<-ceiling(pdata$Decpics[[12]][[1]]/36)#participant schema number matrix

library(dplyr)

b<-as.data.frame(table(a[20,]))

#我们要选出每个participant的每个decision phase所得的分数和时间，可以放在两张表里
#我们先选取时间
time_matrix=data.frame()
score_matrix=data.frame()
#这里暂未区分高低risk group：comparable
for (i in 1:15){
  participant_time<-pdata$Dectimes[[i]][[1]]
  participant_schema<-ceiling(pdata$Decpics[[i]][[1]]/36)
  decision_rounds<-dim(participant_time)[1]#总共有几轮decision
  for(j in 1:decision_rounds){
    decision_time<-participant_time[j,5]-participant_time[j,1]
    time_matrix[i,j]=decision_time
    score=0
    decision_schema<-participant_schema[j,]
    decision_schema_table<-as.data.frame(table(decision_schema))#得出每次decision session选择的schema以及频率
    if (decision_schema_table[1,2]==4) {
      #the points needs to be trippled
      schema_num=decision_schema_table[1,1]
      score=3*(4*schema_score[schema_num])
      score_matrix[i,j]=score
    }
    if(decision_schema_table[1,2]==3){
      #the points will be doubled
      schema_num1=decision_schema_table[1,1]
      schema_num2=decision_schema_table[2,1]
      score=2*(3*schema_score[schema_num1]+schema_score[schema_num2])
      score_matrix[i,j]=score
    }
    if(decision_schema_table[1,2]<3){
      #simply add it up
      n=nrow(decision_schema_table)#有几行
      for(t in 1:n){
        score=score+schema_score[decision_schema_table[n,1]]*decision_schema_table[n,2]
        score_matrix[i,j]=score
      }
    }
    
    #输出:两个dataframe分别记录时间、payoff,再间接求取payoff/time
  }
}

#我们开始间接求取payoff/time
judge<-data.frame(score_matrix/time_matrix)
judge<-cbind(p_basic,judge)
#只有low-risk group and high risk grouo without novelty treatment
part1<-judge[which(judge$Group=='L'|judge$Group=='H'),]
library(reshape2)
library(knitr)
#将data处理成longdata进行画图操作
part1<-melt(data=part1,id.vars=c("Participant.ID","Group"),variable.name="Decision_session",value.name="Payoff/time")
library(ggplot2)

#--------------------------------------全部的decision phase for L and H------------------------
ggplot(data=part1[which(part1$Group=="H"),], mapping = aes(x = factor(Decision_session), y = `Payoff/time`, group =Participant.ID)) + geom_smooth(method = "lm") + xlab('Decision_session')
          +geom_point(aes(col=Participant.ID))
ggplot(data=part1[which(part1$Group=="L"),], mapping = aes(x = factor(Decision_session), y = `Payoff/time`, group =Participant.ID)) + geom_line(aes(col=Participant.ID)) + xlab('Decision_session')
#--------------------------------------全部的decision phase for all groups------------------------
judge<-melt(data=judge,id.vars=c("Participant.ID","Group"),variable.name="Decision_session",value.name="Payoff/time")

ggplot(data=judge, mapping = aes(x = factor(Decision_session), y = `Payoff/time`, group =Participant.ID)) + geom_smooth(lm="lm",aes(col=Group),alpha=0.1) + xlab('Decision_session')



#---------------------------------------novelty treatment之前的decision phase------------------------------------

part2<-judge[,1:13]
part2<-melt(data=part2,id.vars=c("Participant.ID","Group"),variable.name="Decision_session",value.name="Payoff/time")
#propably useful figure!!!!,如何画出较好的趋势呢？
ggplot(data=part2, mapping = aes(x = factor(Decision_session), y = `Payoff/time`, group=Group)) + stat_smooth(method = "lm",alpha=0.1,aes(col=Group)) + xlab('Decision_session')+geom_point(aes(col=Group))
#！！！！！折线图表现不佳，lm画出趋势可能会更好一些


ggplot(data=part2[which(part2$Group=="H"),], mapping = aes(x = factor(Decision_session), y = `Payoff/time`, group=Participant.ID)) + stat_smooth(method = "lm",alpha=0.1,aes(col=Participant.ID)) + xlab('Decision_session')+geom_point()



 






#------------------------------------------------将时间和payoff分开计算--------------------------------------
score_matrix<-cbind(p_basic,data.frame(score_matrix))
time_matrix<-cbind(p_basic,data.frame(time_matrix))  
score_matrix<-melt(data=score_matrix,id.vars=c("Participant.ID","Group"),variable.name="Decision_session",value.name = "payoff")
time_matrix<-melt(data=time_matrix,id.vars=c("Participant.ID","Group"),variable.name="Decision_session",value.name = "time")
ggplot(data=score_matrix,mapping=aes(x=factor(Decision_session),y=payoff,group=Group))+geom_smooth(method="lm",aes(col=Group),alpha=0.1)

score_matrix_part1=score_matrix[1:150,]
ggplot(data=score_matrix_part1,mapping=aes(x=factor(Decision_session),y=payoff,group=Participant.ID))+geom_smooth(method="lm",aes(col=Group),alpha=0.1)+geom_point(aes(col=Group))



#--------------------------------------按照每段period中的median payoff值来观察learning-----------------------
score_matrix$period<-rep("period1",nrow(score_matrix))
#对绝大多数人来说，到decision 12结束时，时间过半，开始novelty treatment
score_matrix[61:120,]$period<-"period2"
score_matrix[121:180,]$period<-"period3"
score_matrix[181:240,]$period<-"period4"
score_matrix[241:300,]$period<-"period5"
score_matrix[301:480,]$period<-"period6"

ggplot(data=score_matrix,mapping=aes(x=factor(period),y=payoff),group=Group)+geom_boxplot(aes(col=Group))
#familarity and likebility


#----------------for quote analysis--------------------------------------------------------------

#我们要选出每个participant的每个decision phase所得的分数和时间，可以放在两张表里
#我们先选取时间
qtime_matrix=data.frame()
qscore_matrix=data.frame()
qschema_score=read.csv('/Users/zhaojingxian/Desktop/PON3/week8/schema/Questionnaire\ answers/Payoff\ -\ quotes-Table\ 1.csv')[,2]
#这里暂未区分高低risk group：comparable
for (i in 1:24){
  participant_time<-qdata$Dectimes[[i]][[1]]
  participant_schema<-ceiling(qdata$Decpics[[i]][[1]]/28)
  decision_rounds<-dim(participant_time)[1]#总共有几轮decision 
  #把decision time 改作exploration time
  for(j in 1:decision_rounds){
    decision_time<-participant_time[j,5]-participant_time[j,1]
    qtime_matrix[i,j]=decision_time
    score=0
    decision_schema<-participant_schema[j,]
    decision_schema_table<-as.data.frame(table(decision_schema))#得出每次decision session选择的schema以及频率
    if (decision_schema_table[1,2]==4) {
      #the points needs to be trippled
      schema_num=decision_schema_table[1,1]
      score=3*(4*qschema_score[schema_num])
      qscore_matrix[i,j]=score
    }
    if(decision_schema_table[1,2]==3){
      #the points will be doubled
      schema_num1=decision_schema_table[1,1]
      schema_num2=decision_schema_table[2,1]
      score=2*(3*qschema_score[schema_num1]+qschema_score[schema_num2])
      qscore_matrix[i,j]=score
    }
    if(decision_schema_table[1,2]<3){
      #simply add it up
      n=nrow(decision_schema_table)#有几行
      for(t in 1:n){
        score=score+qschema_score[decision_schema_table[n,1]]*decision_schema_table[n,2]
        qscore_matrix[i,j]=score
      }
    }
  }
}
qscore_matrix<-data.frame(qscore_matrix)
quotes_precondition<-read.csv("/Users/zhaojingxian/Desktop/PON3/week8/schema/Questionnaire\ answers/Answers\ -\ quotes-Table\ 1.csv")
qscore_matrix<-cbind(quotes_precondition[1:24,1:2],qscore_matrix)
qscore_matrix
#按照时间每一个人的具体时间来操作,将whole period分为 6部分；以period 3为界限，Lc和Hc group进行novelty处理
qdata$Expltimes[[1]][[1]]

qscore_matrix<-melt(data=qscore_matrix,id.vars=c("Participant.ID","Group"),variable.name ="Decision_Session",value.name = "payoff")
ggplot(data=qscore_matrix,mapping=aes(x=factor(Decision_Session),y=payoff,group=Group))+geom_smooth(method = "lm",aes(col=Group))
#Notice:here we compare the score for low and high risk group to find out the impact of pressure and detect whether there is learning


qdata$Payoffs
# $Groups indicate experimental groups

# $Explpics indicate which items were selected in exploration

# $Expltimes indicate when these items were selected in exploration (in seconds - if you want relative times from the start of experiment, subtract Expltimes[1] )

# $Explactions indicate where the items where selected from in the exploration phase - positive numbers indicate index on the list on the left and negative numbers indicate 3 buttons on the right.

# $Decmaps indicate which 20 items were presented in each decision screen

# $Decpics indicate which 4 pictures were chosen in each decision screen
# 
# $Dectimes indicate the times (in seconds) when each of the 4 items were chosen in each decision screen (should subtract Dectimes[screen,1] to get time from beginning of the screen)
# 
# $Payoffs indicate which payoff the participant got for each of 4 items (not including bonuses)
#
# $Moneys indicate total money in pounds earned, including bonuses (starting from 5.00)



