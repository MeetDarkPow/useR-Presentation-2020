library(ggplot2)

timeline <- seq(as.Date("2008/1/1"), as.Date("2019/1/1"), "years")
question_Count <- c(8,524,2267,5838,12196,22287,30948,40760,44541,54363,52074,54180)
answers_Count <- c(67,20,64,13542,22246,35887,44674,52686,54683,60645,62612,57918)
comment_Count <- c(1,5,27,99,22219,45303,62853,90080,97568,104943,109132,100642)
Queries <- data.frame(Year=timeline, Questions=question_Count,
                      Answers=answers_Count, Comments=comment_Count)
write.csv(Queries,'Queries.csv',row.names = FALSE)

ggplot(Queries, aes(Year)) + 
  geom_line(aes(y = Questions, colour = "Questions")) + 
  geom_line(aes(y = Answers, colour = "Answers")) +
  geom_line(aes(y = Comments, colour = "Comments")) +
  ggtitle("Count of Queries using Stack Overflow Data Explorer (2008-2019)") +
  xlab("Year") + ylab("Count of Queries") + theme_minimal()
