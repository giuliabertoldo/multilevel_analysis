library(reshape2)
d <- read.csv('data/mindset_remote.csv')


l <- melt(d, measure.vars=c('neg_time_1','neg_time_2'), variable.name='Week', value.name='NegEmotion')
l2 <- melt(d, measure.vars=c('pos_time_1','pos_time_2'), variable.name='Week', value.name='PosEmotion')
l$PosEmotion <- l2$PosEmotion
l2 <- melt(d, measure.vars=c('pos_time_2','pos_time_3'), variable.name='Week', value.name='PosEmotion_Same')
l$PosEmotion_Same <- l2$PosEmotion_Same
l2 <- melt(d, measure.vars=c('neg_time_2','neg_time_3'), variable.name='Week', value.name='NegEmotion_Same')
l$NegEmotion_Same <- l2$NegEmotion_Same
l2 <- melt(d, measure.vars=c('Productivity_Time2','Productivity_Time3'), variable.name='Week', value.name='Productivity')
l$Productivity <- l2$Productivity
l2 <- melt(d, measure.vars=c('Productivity_Time1','Productivity_Time2'), variable.name='Week', value.name='Productivity_Same')
l$Productivity_Same <- l2$Productivity_Same
l$Week_Num <- as.numeric(l$Week)
l1 <- subset(l, Productivity_Time3 > -3 & Productivity_Time2 > -3 & neg_time_3 > 0 )

ggplot(l1, aes(x=Week_Num, y=NegEmotion)) +
  geom_line(aes(group=SubID))

ggplot(l1, aes(x=Week_Num, y=NegEmotion)) +
  geom_point()
