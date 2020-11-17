df <- read.csv("C:\\Users\\Elyxion\\OneDrive\\Desktop\\imdb_movies.csv",colClasses=c("year"="character"),stringsAsFactors=F,na.strings=c("", "NA"))
df <-as.data.frame(df)

str(df)


df$year <- as.numeric(df$year)
#summary(df$duration)

#boxplot(df$duration, horizontal = TRUE, pch=20, medcol = "blue",col = 'cadetblue3',xlab = 'Minutes', main = "Movie Duration")

#outline = FALSE


#boxplot(df$duration, horizontal = TRUE, pch=20, medcol = "blue",col = 'cadetblue3',xlab = 'Year', main = "Minutes",outline = FALSE)


#factomine r library

#removing text columns

df_new = subset(df, select = -c(title,original_title,director,writer,production_company,actors,description,date_published,metascore,imdb_title_id,budget,
                                usa_gross_income,worlwide_gross_income))

#turning categorical columns to number 

df_new$genre <- as.numeric(as.factor(df_new$genre))
df_new$country <- as.numeric(as.factor(df_new$country))
df_new$language <- as.numeric(as.factor(df_new$language))
df_new <- na.omit(df_new)

summary(df_new$avg_vote)


#making our target categorial variable average vote

df_new$vote_cat[df_new$avg_vote < 6] = "Bad Rating"
df_new$vote_cat[df_new$avg_vote>=6 & df_new$avg_vote<8] = "Average Rating"
df_new$vote_cat[df_new$avg_vote>=8] = "Great Rating"

str(df_new$vote_cat)


df_new$vote_cat <- (factor(df_new$vote_cat))




#split training data
#library caTools
set.seed(123)
split <- sample.split(df_new,SplitRatio = 0.70)
train_set<- subset(df_new,split==T)
test_set<- subset(df_new,split==F)


#balance train with smote
#dmwr package

train_set <- SMOTE(vote_cat ~ ., train_set, perc.over = 1700, perc.under = 300)

summary(train_set$vote_cat)

# seperating target variable
Y_train <- data.frame(train_set$vote_cat)
Y_test <- data.frame(test_set$vote_cat)
train_set <- subset(train_set, select = -c(avg_vote,vote_cat))
test_set <- subset(test_set, select = -c(avg_vote,vote_cat))



#createdatapartition using 
#caret()
#optional


# do pca and dimensionality reduction
pca = prcomp(train_set,scale. = T)

loadings <- as.data.frame(pca$x)
View(loadings)


Matrix <- pca$rotation



std_dev <- pca$sdev
pr_comp_var <- std_dev^2
pr_comp_var


prop_var_ex <- pr_comp_var/sum(pr_comp_var)
prop_var_ex



dev.off()
plot(cumsum(prop_var_ex), xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")


pca_train2 <- cbind(loadings,Y_train)
View(pca_train2)


loadings2 <- loadings[1:5]
pca_train2 <- cbind(loadings2,Y_train)
View(pca_train2)

#transforming the test set with pca

pca_test <- test_set[1:8]
pca_test2 <- predict(pca, newdata = pca_test)

pca_test2 <- as.data.frame(pca_test2)
View(pca_test2)
pca_test3 <- pca_test2[1:5]
pca_test4 <- cbind(pca_test3,Y_test)

#performing svm 


# Fitting SVM to the Training set 
#install.packages('e1071') 
#library(e1071) 

classifier = svm(formula = train_set.vote_cat ~ ., 
                 data = pca_train2, 
                 type = 'C-classification', 
                 kernel = 'linear')
(y_pred = predict(classifier, newdata = pca_test4[1:5])) 


# Making the Confusion Matrix 
cm = table(pca_test4$test_set.vote_cat, y_pred) 
