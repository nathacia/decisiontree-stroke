#nathacia nathacia
#rscript - dtree on stroke data

dtree <- read.csv("/Users/nathacia/Desktop/r wd/bus315/stroke_data.csv")
class(dtree)
View(dtree)
names(dtree)
head(dtree)
tail(dtree)
hist(dtree$age)
str(dtree)
summary(dtree)

#preprocessing
dtree$bmi <- as.numeric(dtree$bmi)
dtree <- na.omit(dtree)
dtree$gender <- as.factor(dtree$gender)
dtree$ever_married <- as.factor(dtree$ever_married)
dtree$work_type <- as.factor(dtree$work_type)
dtree$Residence_type <- as.factor(dtree$Residence_type)
dtree$smoking_status <- as.factor(dtree$smoking_status)
stroke_likelihood <- ifelse(dtree$stroke>=1, "Yes", "No")
stroke_likelihood <- as.factor(stroke_likelihood)
dtree <- data.frame(dtree, stroke_likelihood)
dtree <- subset(dtree, select=-stroke)
dtree <- subset(dtree, select=-ever_married)
dtree <- subset(dtree, select=-id)
dtree <- subset(dtree, select=-work_type)
dtree <- subset(dtree, select=-Residence_type)
summary(dtree)

#training
dtree.stroke <- tree(stroke_likelihood~., dtree)
summary(dtree.stroke)
plot(dtree.stroke)
text(dtree.stroke,pretty=0)
print(dtree.stroke)

#splitting
set.seed(123)
train.index <- sample(1:nrow(dtree), 300)
train.set <- dtree[train.index,]
class(train.set)

#training
dtree.tree <- tree(stroke_likelihood~., train.set)
plot(dtree.tree)
text(dtree.tree, pretty=0)
summary(dtree.tree)

#testing
test.set <- dtree[-train.index,]
stroke.test <- stroke_likelihood[-train.index]
tree.predict <- predict(dtree.tree, test.set, type="class")
table(tree.predict, stroke.test)
accuracy = (55+55)/155
accuracy

#crossvalidation
crossvalidation.dtree <- cv.tree(dtree.tree, FUN=prune.misclass)
crossvalidation.dtree

#pruning
pruned.tree <- prune.misclass(dtree.tree, best=8)
plot(pruned.tree)
text(pruned.tree, pretty=0)
summary(pruned.tree)

#testing pruned tree
pruned.test.pred <- predict(pruned.tree, test.set, type="class")
table(stroke.test, pruned.test.pred)
(57+57)/155
