---
title: >-
  Prediction of Cardiovascular Disease through Comparative Analysis with Data
  Mining Classification Algorithm Approach
subtitle: Aug 2019 - Feb 2020
date: '2018-12-18'
thumb_image: /images/cardio.jpg
thumb_image_alt: A handheld game console on a yellow background
image: /images/cardio.jpg
image_alt: A handheld game console on a yellow background
seo:
  title: Project Title 6
  description: This is the project 6 description
  extra:
    - name: 'og:type'
      value: website
      keyName: property
    - name: 'og:title'
      value: Project Title 6
      keyName: property
    - name: 'og:description'
      value: This is the project 6 description
      keyName: property
    - name: 'og:image'
      value: images/6.jpg
      keyName: property
      relativeUrl: true
    - name: 'twitter:card'
      value: summary_large_image
    - name: 'twitter:title'
      value: Project Title 6
    - name: 'twitter:description'
      value: This is the project 6 description
    - name: 'twitter:image'
      value: images/6.jpg
      relativeUrl: true
layout: project
---

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Suspendisse faucibus interdum posuere lorem ipsum dolor. Malesuada fames ac turpis egestas sed tempus urna et. Non odio euismod lacinia at quis risus. Nisl vel pretium lectus quam id leo in vitae turpis. Dolor sit amet consectetur adipiscing elit ut. Id donec ultrices tincidunt arcu non sodales neque sodales ut. Risus ultricies tristique nulla aliquet enim. Nulla aliquet porttitor lacus luctus accumsan tortor posuere ac ut. Et leo duis ut diam. Auctor neque vitae tempus quam pellentesque. Scelerisque in dictum non consectetur a erat nam at. Quisque sagittis purus sit amet volutpat consequat. Vulputate enim nulla aliquet porttitor lacus.

## Mengimport library
    {r, echo = TRUE, message = FALSE, warning = FALSE} 
    #install.packages("DataExplorer")
    #install.packages("psych") 
    library(DataExplorer) 
    library(data.table) 
    library(dplyr) 
    library(car) 
    library(psych) 
    library(caret) 
    library(e1071) 
    library(randomForest) 
    library(devtools) library(caret)

## Retrieve Data
    {r, echo = TRUE, message = FALSE, warning = FALSE} 
    library(readr) 
    heart <- read_csv("C:/Users/Tania Ciu/Downloads/DataAnalysis/heart.csv") 
    View(heart) 
    Data<-heart

Variable as factor
`{r, echo = TRUE, message = FALSE, warning = FALSE} Data1 <- copy(Data) Data1$sex <- as.factor(Data1$sex) Data1$cp <- as.factor(Data1$cp) Data1$fbs <- as.factor(Data1$fbs) Data1$restecg <- as.factor(Data1$restecg) Data1$exang <- as.factor(Data1$exang) Data1$ca <- as.factor(Data1$ca) Data1$thal <- as.factor(Data1$thal) Data1$target <- as.factor(Data1$target) describe(Data1) str(Data1)`

Plot histogram \`\`\`{r, echo = TRUE, message = FALSE, warning = FALSE}
library(ggplot2) plot_histogram(Data, ggtheme = theme_bw(),
title="Variables in Data")


    Plot Correlation
    ```{r, echo = TRUE, message = FALSE, warning = FALSE}
    #install.packages("GGally")
    library(GGally)
    ggcorr(Data, nbreaks=8, 
           palette='RdGy', 
           label=TRUE, 
           label_size=5, 
           label_color='black')

Split data \`\`\`{r, echo = TRUE, message = FALSE, warning = FALSE}
set.seed(293) trainIndex\<-createDataPartition(y=Data1\$target , p=0.7,
list=FALSE) train_data\<-Data1\[trainIndex,\] train_data
describe(train_data)

test_data\<-Data1\[-trainIndex,\] test_data describe(test_data)


    Modeling Logistic Regression
    ```{r, echo = TRUE, message = FALSE, warning = FALSE}
    LogisticMod <- glm(target ~ age+sex+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal, data=train_data, family="binomial"(link="logit"))
    LogisticPred <- predict(LogisticMod, test_data, 
                            type='response')
    LogisticPred <- ifelse(LogisticPred > 0.5, 1, 0)
    LogisticPredCorrect <- data.frame(target=test_data$target, 
                                      predicted=LogisticPred, 
                                      match=(test_data$target == LogisticPred))
    summary(LogisticMod)
    plot(LogisticMod)
    LogisticPrediction <- predict(LogisticMod, 
                                  test_data, 
                                  type='response')
    LogisticPrediction
    summary(LogisticPrediction)
    plot(LogisticPrediction)

Modeling K-Nearest Neighbor \`\`\`{r, echo = TRUE, message = FALSE,
warning = FALSE} library(caTools) library(caret) library(class)

set.seed(293) sample\<-sample.split(heart\$target, SplitRatio = 0.7)
sample

train\<-subset(heart, sample==TRUE) train

test\<-subset(heart, sample==FALSE) test

train_x\<-subset(train\[,1:13\]) train_x

test_x\<-subset(test\[,1:13\]) test_x

train_target\<-train\$target train_target

test_target\<-test\$target test_target

\#normalisasi data normalise\<-function(newdataf,dataf) {
normalizeddataf=newdataf for(n in names(newdataf)) {
normalizeddataf\[,n\]=(newdataf\[,n\]-min(dataf\[,n\]))/(max(dataf)-min(dataf))
} return(normalizeddataf) }

train_norm\<-normalise(train_x\[1:13\],train_x\[1:13\]) train_norm

test_norm\<-normalise(test_x\[1:13\],train_x\[1:13\]) test_norm

predictKNN\<-knn(train=train_norm,test = test_norm, cl=train_target,
k=7) predictKNN plot(predictKNN, col = c("darkgreen", "darkblue"))


    Modeling Suppot Vector Machine
    ```{r, echo = TRUE, message = FALSE, warning = FALSE}
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

    svm_Linear <- train(target ~., data = train_data, trControl = trctrl, 
                        preProcess = c("center", "scale"), tuneLength = 10 )
    svm_Linear
    plot(svm_Linear)
    testsvm <- svm(target ~ . , data = train_data,kernel = "radial", gamma = 1, cost = 1, scale = FALSE)
    testsvm
    #Prediction
    SVMpred <- predict(svm_Linear, newdata = test_data)
    SVMpred
    plot(SVMpred, col=c(1,2))

Modeling Naive Bayes
`{r, echo = TRUE, message = FALSE, warning = FALSE} model <- naiveBayes(target~., data = train_data) model #Memprediksi model solusi NBpred <- predict(model, test_data, type = "class") NBpred plot(NBpred, col = c("green", "yellow"))`

Validation Data with K-Fold Cross Validation Logistic regression
`{r, echo = TRUE, message = FALSE, warning = FALSE} library(boot) set.seed(293) glm.fit <- glm(target ~ age+sex+trestbps                +chol+fbs+restecg                +thalach+exang+oldpeak                +slope+ca+thal,                 family = quasibinomial,                 data = Data) cv.err.10 <- cv.glm(data = Data,                      glmfit = glm.fit,                     K = 10) cv.err.10$delta`

Validation Data with K-Fold Cross Validation K-Nearest Neighbor
\`\`\`{r, echo = TRUE, message = FALSE, warning = FALSE} library(klaR)
train.control \<- trainControl(method = "repeatedcv", number = 10) \#
Train the model Knn_cv \<- train(target \~., data = train_data, method =
"knn", trControl = train.control) \# Summarize the results print(Knn_cv)


    Validation Data with K-Fold Cross Validation Support Vector Machine
    ```{r, echo = TRUE, message = FALSE, warning = FALSE}
    set.seed(293)
    tunesvm <- tune(svm, target ~ ., data = train_data, kernel = "sigmoid",
                    ranges = list(cost = 0.001))
    summary(tunesvm)

Validation Data with K-Fold Cross Validation Naive Bayes
`{r, echo = TRUE, message = FALSE, warning = FALSE} library(klaR) train_control <- trainControl(method="cv", number=10) # train the model NB_cv<- train(target~., data=test_data, trControl=train_control, method="nb") # summarize results print(NB_cv)`

Validation Data with Confusion Matrix Logistic Regression
`{r, echo = TRUE, message = FALSE, warning = FALSE} library(tools) conf1<-confusionMatrix(table(LogisticPred,                              test_data$target)) conf1`

Validation Data with Confusion Matrix K-Nearest Neighbor
`{r, echo = TRUE, message = FALSE, warning = FALSE} library(tools) conf2<-confusionMatrix(table(predictKNN,                              test_target)) conf2`
Validation Data with Confusion Matrix Support Vector Machine
`{r, echo = TRUE, message = FALSE, warning = FALSE} library(tools) conf3<-confusionMatrix(table(SVMpred,                              test_data$target)) conf3`
Validation Data with Confusion Matrix Naive Bayes
`{r, echo = TRUE, message = FALSE, warning = FALSE} library(tools) conf4<-confusionMatrix(table(NBpred,                              test_data$target)) conf4`

