df <- read.csv("C:\\Users\\Elyxion\\OneDrive\\Desktop\\movies_new.csv",colClasses=c("year"="character"),stringsAsFactors=F,na.strings=c("", "NA"))
summary(df)

df$year <- as.numeric(df$year)

#remove unecessary columns
df_new = subset(df, select = -c(date_published,metascore,budget,X,usa_gross_income,worlwide_gross_income,genre,country,language))

#omit rows with na

df_new <- na.omit(df_new)


#making our target categorial variable average vote

df_new$vote_cat[df_new$avg_vote < 6] = "Bad Rating"
df_new$vote_cat[df_new$avg_vote>=6 & df_new$avg_vote<8] = "Average Rating"
df_new$vote_cat[df_new$avg_vote>=8] = "Great Rating"

str(df_new$vote_cat)


df_new$vote_cat <- (factor(df_new$vote_cat))


#remove average vote

df_new = subset(df_new, select = -c(avg_vote))

#scale numerical features
df_new$year <- scale(df_new$year)

df_new$duration <- scale(df_new$duration)

df_new$votes <- scale(df_new$votes)

df_new$reviews_from_users <- scale(df_new$reviews_from_users)

df_new$reviews_from_critics <- scale(df_new$reviews_from_critics)


#split training data
#library caTools
set.seed(123)
split <- sample.split(df_new,SplitRatio = 0.70)
train_set<- subset(df_new,split==T)
test_set<- subset(df_new,split==F)

#balance train with smote
#dmwr package

train_set <- SMOTE(vote_cat ~ ., train_set, perc.over = 1500, perc.under = 300)

summary(train_set$vote_cat)

#---------------------------------- dont rerun this --------------------------------------------


# seperating target variable
Y_train <- data.frame(train_set$vote_cat)
Y_test <- data.frame(test_set$vote_cat)
train_set <- subset(train_set, select = -c(vote_cat))
test_set <- subset(test_set, select = -c(vote_cat))

# do pca and dimensionality reduction
pca = prcomp(train_set,scale. = F)

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


loadings2 <- loadings[1:8]
pca_train2 <- cbind(loadings2,Y_train)
View(pca_train2)



#transforming the test set with pca

pca_test <- test_set
pca_test2 <- predict(pca, newdata = pca_test)

pca_test2 <- as.data.frame(pca_test2)
View(pca_test2)
pca_test3 <- pca_test2[1:8]
pca_test4 <- cbind(pca_test3,Y_test)

#performing svm 


# Fitting SVM to the Training set 
#install.packages('e1071') 
#library(e1071) 

classifier = svm(formula = train_set.vote_cat ~ ., 
                 data = pca_train2, 
                 type = 'C-classification', 
                 kernel = 'linear')
(y_pred = predict(classifier, newdata = pca_test4[1:8])) 


# Making the Confusion Matrix 
cm = table(pca_test4$test_set.vote_cat, y_pred) 
