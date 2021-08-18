---
title: >-
Logistic Regression Prediction Model for Cardiovascular Disease
subtitle: Aug 2019 - Feb 2020
date: '2018-12-18'
thumb_image: /images/cardio.jpg
thumb_image_alt: cardio
image: /images/cardio.jpg
image_alt: cardio
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

## Variable as factor
    {r, echo = TRUE, message = FALSE, warning = FALSE} 
    Data1 <- copy(Data) 
    Data1$sex <- as.factor(Data1$sex) 
    Data1$cp <- as.factor(Data1$cp) 
    Data1$fbs <- as.factor(Data1$fbs) 
    Data1$restecg <- as.factor
    (Data1$restecg) Data1$exang <- as.factor
    (Data1$exang) Data1$ca <- as.factor
    (Data1$ca) Data1$thal <- as.factor
    (Data1$thal) Data1$target <- as.factor
    (Data1$target) 
    describe(Data1) 
    str(Data1)`

## Plot histogram
    {r, echo = TRUE, message = FALSE, warning = FALSE}
    library(ggplot2) plot_histogram(Data, ggtheme = theme_bw(),
    title="Variables in Data")


## Plot Correlation
    {r, echo = TRUE, message = FALSE, warning = FALSE}
    #install.packages("GGally")
    library(GGally)
    ggcorr(Data, nbreaks=8, 
           palette='RdGy', 
           label=TRUE, 
           label_size=5, 
           label_color='black')

## Split data
    {r, echo = TRUE, message = FALSE, warning = FALSE}
    set.seed(293) 
    trainIndex\<-createDataPartition(y=Data1\$target , p=0.7,
    list=FALSE) 
    train_data\<-Data1\[trainIndex,\] train_data
    describe(train_data)
    test_data\<-Data1\[-trainIndex,\] test_data describe(test_data)

## Modeling Logistic Regression
    {r, echo = TRUE, message = FALSE, warning = FALSE}
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
## LogisticPrediction
    summary(LogisticPrediction)
    plot(LogisticPrediction)

## Validation Data with K-Fold Cross Validation Logistic regression
    {r, echo = TRUE, message = FALSE, warning = FALSE} library(boot) set.seed(293) glm.fit <- glm(target ~ age+sex+trestbps                +chol+fbs+restecg                +thalach+exang+oldpeak                +slope+ca+thal,                 family = quasibinomial,                 data = Data) cv.err.10 <- cv.glm(data = Data,                      glmfit = glm.fit,                     K = 10) cv.err.10$delta`

## Validation Data with Confusion Matrix Logistic Regression
`{r, echo = TRUE, message = FALSE, warning = FALSE} library(tools) conf1<-confusionMatrix(table(LogisticPred,                              test_data$target)) conf1`



