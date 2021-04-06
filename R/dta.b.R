
# This file is a generated template, your changes will not be overwritten

dtaClass <- R6::R6Class(
    "dtaClass",
    inherit = dtaBase,
    private = list(
        .run = function() {
            
            #Defining Data object
            dat2<- self$data
            
            #Options
            dep<- self$options$dep
            indep<- self$options$indep
            split<- self$options$split
            footer<- "Decision Tree classification"
            met <- self$options$method
            minbucket<- self$options$Control
            
            #Initialize Variables 
            d_dep<- data.frame()
            d_indep<- data.frame()
            index<- c()
            
            if(!(is.null(dep) & (is.null(indep))))
            {
                d_dep<- as.factor(dat2[,dep])
                d_indep<- dat2[, indep]
            }
            
            #Creating Data object
            d_data<- cbind(d_dep, d_indep)
            
            
            #Creating index to split data
            size<- floor((split/100)* nrow(d_data))
            index <- sample(seq_len(nrow(d_data)), size = size)
            
            #Splitting data into test and train
            d_train<- as.data.frame(d_data[index,])
            d_test<- as.data.frame(d_data[-index,])
            
            
            #Modelling
            if(met=="Class")
            {
                model1<-rpart::rpart(d_dep~., data = d_train, method = "class", minbucket= minbucket)
            }
            else
            {
                model1<-rpart::rpart(d_dep~., data = d_train, method = "anova", minbucket= minbucket)
            }  
            
            pred1<- predict(model1, d_test)
            
            #Results Variables
            text<- print(model1)
            #text1<- print (table(d_te_class, pred1))
            textResults <- self$results$text
            textResults$content<- text
            
            
            image = self$results$plot
            image$setState(model1)
        },
        .plot = function(image, ...)
        {
            plotData <- image$state
            
            rpart.plot(plotData)
            TRUE
        }
        
    )
)