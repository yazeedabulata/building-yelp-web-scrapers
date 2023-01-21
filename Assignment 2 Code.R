
###########################Step 1: Import Data

yelp_reviews <- read.csv("milwaukee_ratings.csv")

review_data <- yelp_reviews[,c(3,7,9,11,13,14)]

#Datasetup
review_data$Reviewer.Location=as.factor(review_data$Reviewer.Location)
review_data$Price[is.na(review_data$Price)] <- 0



###################Step 2: Data Pre-processing
library(stm)

processed <- textProcessor(review_data$Review , metadata = review_data, 
                           lowercase = TRUE,   removestopwords = TRUE,
                           removenumbers = TRUE, removepunctuation = TRUE,
                           stem = TRUE, wordLengths = c(4, Inf), 
                           customstopwords=c("bar", "place", "milwaukee", "wisconsin", "like", "just", "dont", "also"))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 2, upper.thresh = 1200)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta


#################3Step 3: Determining the Topic Number

#Bottom-Up
library("ggplot2")

findingk <- searchK(out$documents, out$vocab, K = c(5:20),
                    prevalence =~ s(Price) + LocationB, data = meta, verbose=FALSE)

plot(findingk) #6, 11, 14, 20 best performing


#evaluate using exclusivity by semantic coherence for dominant models
dt.results <- data.frame(findingk$results)
dt.results$K=as.numeric(dt.results$K)
dt.results$semcoh=as.numeric(dt.results$semcoh)
dt.results$exclus=as.numeric(dt.results$exclus)

ggplot(data=dt.results, aes(x=semcoh, y=exclus, label = K)) + 
  geom_point() +geom_text(hjust=0, vjust=0)


###Step 4: Running & Inspecting the Topic Model
## 6 Categories
STM_6 <- stm(documents = out$documents, vocab = out$vocab,
              K = 6, prevalence =~ s(Price) + LocationB,
              max.em.its = 75, data = out$meta,
              init.type = "Spectral", verbose = FALSE)

plot.STM(STM_6, type = "summary")

findThoughts(STM_6, texts = review_data$Review,
             n = 5, topics = 1)

cloud(STM_6, topic = 1, max.words = 50, scale=c(3,0.25))
cloud(STM_6, topic = 2, max.words = 50, scale=c(3,0.25))
cloud(STM_6, topic = 3, max.words = 50, scale=c(3,0.25))
cloud(STM_6, topic = 4, max.words = 50, scale=c(3,0.25))
cloud(STM_6, topic = 5, max.words = 50, scale=c(3,0.25))
cloud(STM_6, topic = 6, max.words = 50, scale=c(3,0.25))

labelTopics(STM_6,n=6)

plot(STM_6, type = "summary")

##1 = General Bar Attribtues
##2 = Bad Customer Experience
##3 = Drinks
##4 = Activities
##5 = Food
##6 = Staff Interaction


#Topic per Price Category relevance graph
prep <- estimateEffect(1:6 ~ s(Price) + LocationB, STM_6, meta = out$meta, uncertainty = "None")

plot(prep, "Price", method = "continuous", topics = 6, model = z, printlegend = FALSE, 
     xaxt = "n", xlab = "Price",main="Topic 6 per Price Category")
axis(1,at = c(0,1,2,3), labels = c("none", "1", "2", "3"))



################################Step 5: Initial Analyses
#Topic Correlaation matrix
topic.corr <- topicCorr(STM_6)
topic.corr$cor
plot(topic.corr)

# Covariate Analysis
STM <- stm(documents = out$documents, vocab = out$vocab,
           K = 6, prevalence =~ s(Price) + LocationB,
           max.em.its = 75, data = out$meta,
           init.type = "Spectral", verbose = FALSE)
prep <- estimateEffect(1:6 ~ s(Price) + LocationB, STM, meta = out$meta, uncertainty = "None")

plot(prep, covariate = "LocationB", topics = c(1,2,3,4,5,6), 
     model = STM, method = "difference", cov.value1 = "0", 
     cov.value2 = "1", xlab = "Out of Town ... In Town", main = "Location Covariate Analysis", 
     xlim = c(-0.1, 0.1), 
     labeltype = "custom", 
     custom.labels = c("Bar Attributes", "Bad Experiences","Drinks", "Activities", "Food", "Bar Logistics"))


#Topic Frequency barchart
topicprop10<-make.dt(STM_6, meta)

topics <- topicprop10[,c(2,3,4,5,6,7)]
topics$allocation <- colnames(topics)[max.col(topics,ties.method="first")]

topicprop10$allocation <- topics$allocation

topicprop10$numeric_rating<-factor(topicprop10$Review.Stars)
topicprop10$restaurant_rating<-factor(topicprop10$Restaurant.Stars)

library("plyr")
barchart1 <- data.frame(count(topicprop10, 'allocation'))
barchart1[1,1] <- "Bar Attributes"
barchart1[2,1] <- "Bad Experiences"
barchart1[3,1] <- "Drinks"
barchart1[4,1] <- "Bar Activities"
barchart1[5,1] <- "Food"
barchart1[6,1] <- "Bar Logistics"


ggplot(data=barchart1, aes(x=allocation, y=freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+coord_cartesian(ylim=c(0,550))+ ggtitle("Frequency Bar Chart") + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Frequency") + ylab("Topic")


#Mean Rating Per Category
barchart2 <- topicprop10[,c(18,17)]
class(barchart2$numeric_rating) = "Numeric"

dt_grp_topic = barchart2 %>% group_by(allocation) %>%
  summarise_at(vars(numeric_rating), list(Mean = mean))

dt_grp_topic[1,1] <- "Bar Attributes"
dt_grp_topic[2,1] <- "Bad Experiences"
dt_grp_topic[3,1] <- "Drinks"
dt_grp_topic[4,1] <- "Bar Activities"
dt_grp_topic[5,1] <- "Food"
dt_grp_topic[6,1] <- "Bar Logistics"


ggplot(data=dt_grp_topic, aes(x=allocation, y=Mean)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()+coord_cartesian(ylim=c(0,5)) + ggtitle("Review Star Bar Chart") + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Topic") + ylab("Mean Review Stars")

###################################Step 6: Regression
require(nnet)
#regression 1, topic on review rating
rslt.logit <- multinom(numeric_rating ~ Topic1 + Topic2 + Topic3 + Topic4 + 
                         Topic5 + Topic6, data=topicprop10)

#regression 2, topic on restaurant rating
rslt.logit2 <- multinom(restaurant_rating ~ Topic1 + Topic2 + Topic3 + Topic4 + 
                         Topic5 + Topic6, data=topicprop10)
require(stargazer)
stargazer(rslt.logit, type="text")
stargazer(rslt.logit2, type="text")



