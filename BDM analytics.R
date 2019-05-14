library("dplyr")
library("ggplot2")
sample <- read.csv("C:/Users/Kaushik Avhad/Downloads/ebs-granite-sample.csv")

table(sample$readerid)


sample%>%group_by(mac) %>% count()

table(sample$readerid)
ggplot(sample)+geom_bar(aes(mac))
sample$sensor= ifelse(sample$readerid=="device-id-8451759d-969d-4434-833b-82069da244b7","sensor1",
                      ifelse(sample$readerid=="device-id-8f373a31-a510-4a3f-9472-4a0f5bb56245","sensor2",
                             ifelse(sample$readerid=="device-id-be4eeea7-e2ea-40c3-b070-288224f9023e","sensor3","sensor4")))
head(sample$sensor)

plot(sample$mac,sample$sensor)
sample$readerid<- NULL
sample$samples<- NULL
head(sample)
###2
sample$time <- as.POSIXct(sample$time)
sample$mac = factor(sample$mac)
class(sample_group$mac)
sample1 <- sample[order(sample$mac),]


sample1$tdiff <- unlist(tapply(sample1$time, INDEX = sample1$mac,
                          FUN = function(x) c(0, `units<-`(diff(x), "secs"))))


sample1 %>% group_by(mac) %>% count()

library("sqldf")
 
sqldf('select mac,sum(tdiff) as time_spent from sample1 group by mac')

View(sample)


