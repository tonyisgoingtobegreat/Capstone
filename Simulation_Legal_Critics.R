# setwd("~/Dropbox/Research/Personalization/Simulation")
setwd("/Users/tony/Desktop/Capstone/Simulation")  # Tony's Mac set-up

# Installing required packages 
#install.packages(c("readstata13","randomForest","glmnet","ggplot2","reshape2","rpart.plot","tidyr","extrafont"))

# Cleanup: add documentation to code for replicability


# Set a seed to make random draws reproducible
set.seed(1234)


# Load, clean and analyze dataset

# Load
library(readstata13) 
hmda <- read.dta13("HMDA_Boston_Area.dta") 
  
  # Clean
    # Construct missingness as NA
    for(thisname in names(hmda)[sapply(hmda,is.numeric)]) {
     hmda[!is.na(hmda[,thisname]) & hmda[,thisname] > 999999 & hmda[,thisname] < 1000000,thisname] <- NA
    } #hmda has a particular way of coding missing data so this is changed to NA. looping of numeric variables. N like . in stata
    for(thisname in names(hmda)[!sapply(hmda,is.numeric)]) {
      hmda[!is.na(hmda[,thisname]) & hmda[,thisname] == "NA",thisname] <- NA
    }
    # Variable encoding
    numvariables <- c("loanamount","appinc","nounitsinproperty",
                      "nodependents","yeasemployedlineofwork","yearsemployedjob",
                      "appbaseincome","coabaseincome","apptotincome",
                      "coatotincome","housingexpenses","purchaseprice",
                      "otherfinancing","liquidassets","nocreditreports",
                      "nocreditlines","housingdti","totaldti","loanterm",
                      "appraisedvalue","noreviews","netw","uria",
                      "school","chval")
    factorvariables <- setdiff(names(hmda),numvariables) #treat as factor. Need to turn into dummies
    hmda[,factorvariables] <- data.frame(apply(hmda[factorvariables], 2, as.factor)) #turn all other variables into factors
    # Strip of attributes
    hmda <- hmda[,names(hmda)] #some extra data that came in from stata so telling it to ignore it. reducing to only columns.
    
  # Prepare models
    # Group variables
    #drop loan purpose
    loancharacteristics <- c('fixedadjustable','loanterm')
    borrowercharacteristics <- c('appinc','maritalstatus','nodependents','yeasemployedlineofwork','yearsemployedjob','selfemployed','appbaseincome',
                                 'coabaseincome','apptotincome','coatotincome','liquidassets','nocreditreports','nocreditlines',
                                 'mortgagecredithistory','paymentcredithistory','publiccredithistory','housingdti','totaldti','netw','school',
                                 'appsex','coasex','old')
    additionalcharacteristics <- c('specialloanapplication','loanpurchaser','credithistoryok')
    borrowingcharacteristics <- c('unverifiable','noreviews','otherfinancing','giftgrant','cosigner','pmisought','pmidenied')
    propertycharacteristics <- c('occupancy','propertycounty','nounitsinproperty','housingexpenses','purchaseprice','appraisedvalue','propertypurchased','dnotown','dprop')
    neighborhoodcharacteristics <- c('uria','rtdum','bd','mi','vr','chval')
    racecharacteristics <- c('apprace','coarace')
    outcomes <- c('actiontaken')
    # Create clean dataframe
    hmda2 <- hmda[,c(borrowercharacteristics,loancharacteristics,propertycharacteristics,neighborhoodcharacteristics,borrowingcharacteristics)]
    hmda2$race <- factor(hmda$apprace)#create column race, make it a factor. race column in hmda2. use apprace from hmda
    hmda2$approved <- hmda$actiontaken %in% c(1,2)
    # Extend missing values
    for(thisvar in names(hmda2)[sapply(hmda2,is.numeric)]) {
      if(any(is.na(hmda2[,thisvar]))) {
       hmda2[,paste0(thisvar,"M")] <- factor(is.na(hmda2[,thisvar]))
       hmda2[is.na(hmda2[,thisvar]),thisvar] <- 0
      }
    }# if run wihtout this it would only run on lines without missing values. Here we change missing values with 0 and create a missing indicator M.
    for(thisvar in names(hmda2)[sapply(hmda2,is.factor)]) {
      if(any(is.na(hmda2[,thisvar]))) {
        hmda2[,thisvar] <- addNA(hmda2[,thisvar])
        print(levels(hmda2[,thisvar]))
        levels(hmda2[,thisvar])[nlevels(hmda2[,thisvar])] <- "mis"
      }
    } #for factor variables create another level (so an additional indicator)
    hmda2 <- droplevels(hmda2) #drop levels that are not used because R is stupid. 
    hmda2 <- hmda2[,sapply(hmda2, nlevels) != 1] #drop variables with the same value. 
    
    
# Set up and calibrate base model
    
    # Fit logit model
    library(glmnet) #package for GLM models with lasso and ridge (among others)
    hmda3 <-hmda2[complete.cases(hmda2), ]    
    X <- model.matrix(approved ~ . - race, hmda3) #glmnet requires that you form a matrix before running. 
    logitmodel <- glmnet(X, hmda3$approved,
                         family = "binomial",alpha=0,lambda=.05) #logit so the distribution is binomial, alpha is the penalty on the lasson. lambda is the weight of the penalty.
    hmdamodel <- hmda3
    hmdamodel$logitscores <- - predict(logitmodel,X, type = "link") #additioanl column is the prediction (the logit score)
    
    # Calibrate by race to default rate as show
    levels(hmdamodel$race) <- c("B","H","W")#rename the levels of race variable
    targetdefaultrates <- c('B'=.0357,'H'=.0157,'W'=.0108) #as from merged HMDA-Corelogic Dataset
    overallavg <-  sum(targetdefaultrates * table(hmdamodel$race) / nrow(hmdamodel))
    print(overallavg)
    avgrate <- function(scores) mean(1 / (1 + exp(-scores))) #create function 
    offsets <- seq(-5,1,by=.005) #create a grip with increments. 
    hmdamodel$defaultprob <- NA #add a column (for now doesnt have any values_)
    thisoffset <- offsets[which(lapply(offsets,
                                      function(offset) avgrate(hmdamodel$logitscores[hmdamodel$approved == T] + offset))
                               >= overallavg)[1]]
    hmdamodel$defaultprob <- 1 / (1 + exp(-(hmdamodel$logitscores + thisoffset)))
    print(c(mean(hmdamodel$defaultprob[hmdamodel$approved == T]),
            mean(hmdamodel$defaultprob)))

    library(ggplot2) 
    # default probability for different racial groups
    ggplot(hmdamodel, aes(x=defaultprob, fill=race)) +
      xlim(0,0.08) +
      geom_density(alpha=.5) + theme_bw()

    # Prepare data for sampling
    hmdamodel2 <- hmdamodel[,setdiff(names(hmdamodel),c("approved","defaultprob","logitscores"))] #get rid of whether approved, default probability (because only 
    # available to lender).
    
    # Obtain a sampler to draw from population distribution
    popsampler <- function(n=100, replace=T, stratify=F) { #constructing a sample. 
      # Implement stratification -- later
      # Implement oversamling for no replacement -- later
      hmdasample <- hmdamodel2
      hmdasample$default <- factor(runif(nrow(hmdamodel)) < hmdamodel$defaultprob) #create column that is default. This produces 1 with probability p. 
      return(hmdasample[sample(nrow(hmdamodel),n,replace=replace),]) #choosing some of them at random.
    }
    
    #overall default probability
    ggplot(hmdamodel, aes(x=defaultprob)) +
      xlim(0,0.08) +
      geom_density(alpha=.5) + theme_bw()
    
# Tune algorithms
    
    library(rpart) #package for trees
    library(randomForest) 
    
    # Prepare
    loss <- function(x,p) {
      return(sum((as.numeric(x)  - p)^2))
    } # measure the loss.
    train <- popsampler(n=2000, replace=F)
    test <- popsampler(n=2000, replace=F)

    # Tune tree
    treecand <- seq(2,100,by=2) # minimal size in each bucket
    treetuning <- sapply(treecand, function(mb) #for every minimal size bucket, fit a tree and calculate loss
      loss(test$default,predict(rpart(default ~ ., data=train,
                         control = rpart.control(minbucket = mb),method="class"),newdata=test)[,2]))
    plot(treecand,treetuning) #creates cool trees. complexity of the tree v. the loss. 
    treechosen <- treecand[which.min(treetuning)] #picks the tree with the minimum loss.
    print(treechosen)
    
    # Tune rf
    rfcand <- c(1,2,4,6,8,10,20,30,50) #same as earlier- trying out different node sizes as the tuning parameter (on training set alone)
    rftuning <- sapply(rfcand, function(mb) # go through different node size, and for every node size fit a random forest and save the losses
      loss(test$default,predict(randomForest(default ~ ., data=train, nodesize=mb),
                          newdata=test,type="prob")[,2]))
    plot(rfcand,rftuning) #plot the losses and choose the best one 
    rfchosen <- rfcand[which.min(rftuning)]
    print(rfchosen)
    
    # Tune simple LASSO
    #with Lasso you need to create matrixes 
    X <- model.matrix(default ~ .,rbind(train,test))
    Xtrain <- X[1:nrow(train),]
    Xtest <- X[nrow(train)+(1:nrow(test)),]
    lassos <- glmnet(Xtrain, train$default, family="binomial")
    losses <- apply(predict(lassos,newx=Xtest,type="response"),2,function(Yhat) loss(test$default,Yhat))
    plot(log(lassos$lambda),losses)
    lambdas <- lassos$lambda
    lambdaind <- which.min(losses)
    lambda <- lambdas[lambdaind]
    print(lambda)
    

    
# Fit rules 

    trainingsample <- popsampler(n=2000, replace=F)
    holdout <- popsampler(n=2000, replace=F)
    X <- model.matrix(default ~ .,rbind(trainingsample,holdout))
    Xtrain <- X[1:nrow(trainingsample),]
    Xtest <- X[nrow(trainingsample)+(1:nrow(holdout)),]

    holdout0 <- holdout
    
    tree <- rpart(default ~ ., data=trainingsample,
          control = rpart.control(minbucket = treechosen),method="class")
    holdout$tree <- predict(tree,newdata=holdout)[,2]
    
    forest <- randomForest(default ~ ., data=trainingsample, nodesize=rfchosen, importance=T)
    holdout$forest <- predict(forest,newdata=holdout,type="prob")[,2]
    # where shap values can be imported in
    print(sort(forest$importance[,3],decreasing=T))

    smalllassos <- glmnet(Xtrain, trainingsample$default, family="binomial", lambda=lambdas)
    holdout$lasso <- predict(smalllassos,newx=Xtest,type="response")[,lambdaind]
    names(smalllassos$beta[,lambdaind])[!(smalllassos$beta[,lambdaind] == 0)]
    smalllassos$beta[!(smalllassos$beta[,lambdaind] == 0),lambdaind]
    
    toptree <- rpart(default ~ ., data=trainingsample,
                     control = rpart.control(minbucket = treechosen, maxdepth=4),
                     method="class")
    
    library(rpart.plot) #something obscure to get a nice tree
    split.fun <- function(x, labs, digits, varlen, faclen) #takes as input the splits
    {
      gsub(" =.*$", "", labs)
    }
    
    node.fun <- function(x, labs, digits, varlen) { # takes the final nodes
      sprintf("%1.1f%%", 100*as.numeric(labs))
    }
    
    png("Figures2/toptree.png",width=1200,height=1200) #print png
    prp(toptree, digits=0,extra=7,split.fun=split.fun,node.fun=node.fun)
    dev.off()
    

    
    # Analyze stability of these rules by refitting
    
    I <- 15
    
    allvariables <- setdiff(names(trainingsample),"default")
    
    smalllassobarcodes <- matrix(0,nrow=I,ncol=smalllassos$dim[1])
    rfimportances <- matrix(0,nrow=I,ncol=length(forest$importance[,3]))
    rpartchosen <- matrix(0,nrow=I,ncol=length(allvariables))
    
    for(i in 1:I) {
      
      print(i)
      
      thissample <- popsampler(n=2000, replace=F)
      
      thisfullX <- model.matrix(default ~ .,rbind(thissample,holdout0))
      thisX <- thisfullX[1:nrow(thissample),]
      thisXholdout <- thisfullX[nrow(thissample)+(1:nrow(holdout0)),]
      
      thistree <- rpart(default ~ ., data=thissample,
                        control = rpart.control(minbucket = treechosen),method="class")
      holdout[,paste0("tree",i)] <- predict(thistree,newdata=holdout)[,2]
      
      thissmalllassos <- glmnet(thisX, thissample$default, family="binomial", lambda=lambdas)
      
      #print(names(thissmalllassos$beta[,lambdaind])[!(thissmalllassos$beta[,lambdaind] == 0)])
      
      holdout[,paste0("lasso",i)] <- predict(thissmalllassos,newx=thisXholdout,type="response")[,lambdaind]
      
      thisforest <- randomForest(default ~ ., data=thissample, nodesize=rfchosen, importance=T)
      
      #print(sort(thisforest$importance[,3],decreasing=T))
      holdout[,paste0("forest",i)] <- predict(thisforest,newdata=holdout,type="prob")[,2]
      
      thistoptree <- rpart(default ~ ., data=thissample,
                           control = rpart.control(minbucket = treechosen, maxdepth=4),
                           method="class")
      #print(unique(thistoptree$frame$var))
      
      png(paste0("Figures2/toptree",i,".png"),width=1200,height=1200)
      prp(thistoptree, digits=0,extra=7,split.fun=split.fun,node.fun=node.fun)
      dev.off()
      
      smalllassobarcodes[i,as.vector(!(thissmalllassos$beta[,lambdaind] == 0))] <- 1
      rfimportances[i,] <- thisforest$importance[,3]
      rpartchosen[i,] <- as.numeric(allvariables %in% unique(thistoptree$frame$var))
    }
    
    #print(thissample$loanpurpose[1000:1999])
    
    # Fit rules without race
    
    Xnr <- model.matrix(default ~ . - race,rbind(trainingsample,holdout0))
    Xnrtrain <- Xnr[1:nrow(trainingsample),]
    Xnrtest <- Xnr[nrow(trainingsample)+(1:nrow(holdout)),]

    treenr <- rpart(default ~ ., data=trainingsample[,setdiff(names(trainingsample),"race")],
                  control = rpart.control(minbucket = treechosen),method="class")
    holdout$`tree without race` <- predict(treenr,newdata=holdout)[,2]
    
    forestnr <- randomForest(default ~ . - race, data=trainingsample, nodesize=rfchosen, importance=T)
    holdout$`forest without race` <- predict(forestnr,newdata=holdout,type="prob")[,2]

    smalllassosnr <- glmnet(Xnrtrain, trainingsample$default, family="binomial", lambda=lambdas)
    holdout$`lasso without race` <- predict(smalllassosnr,newx=Xnrtest,type="response")[,lambdaind]

    
    

    # Fit rules without race and correlates
    
    covariates <- setdiff(names(trainingsample),c("default","race"))
    correlations <- lapply(covariates, function(cova) cor(trainingsample$race != 'W', as.numeric(trainingsample[,cova])))
    mostcorr <- covariates[order(abs(unlist(correlations)),decreasing = T)[1:10]]
    print(mostcorr)
    
    trainingsample2 <- trainingsample
    trainingsample2$binrace <- trainingsample$race != 'W'
    predictability <- lapply(covariates, function(cova) if(length(unique(trainingsample2[,cova])) > 1) summary(lm(as.formula(paste0("binrace ~ ",cova)),data=trainingsample2))$adj.r.squared else 0)
    mostpred <- covariates[order(abs(unlist(predictability)),decreasing = T)[1:10]]
    print(mostpred)
    
    trainingsampleRED <- trainingsample[,setdiff(names(trainingsample),c("race",mostpred))]
    holdoutRED <- holdout0[,setdiff(names(holdout0),c("race",mostpred))]
    XnrRED <- model.matrix(default ~ .,rbind(trainingsampleRED,holdoutRED))
    XnrREDtrain <- XnrRED[1:nrow(trainingsampleRED),]
    XnrREDtest <- XnrRED[nrow(trainingsampleRED)+(1:nrow(holdoutRED)),]

    treenrRED <- rpart(default ~ ., data=trainingsampleRED,
                    control = rpart.control(minbucket = treechosen),method="class")
    holdout$`tree without race, correlates` <- predict(treenrRED,newdata=holdoutRED)[,2]
    
    forestnrRED <- randomForest(default ~ ., data=trainingsampleRED, nodesize=rfchosen, importance=T)
    holdout$`forest without race, correlates` <- predict(forestnrRED,newdata=holdoutRED,type="prob")[,2]
    
    smalllassosnrRED <- glmnet(XnrREDtrain, trainingsampleRED$default, family="binomial", lambda=lambdas)
    holdout$`lasso without race, correlates` <- predict(smalllassosnrRED,newx=XnrREDtest,type="response")[,lambdaind]
    

    
    
    library(reshape2)
    # Barcode plot
    
    getbarcodeplot <- function(barcodes) {
      barcodeplotdata <- melt(barcodes)
      names(barcodeplotdata) <- c("Iteration","Coefficient","Selected")
      barcodeplotdata$Selected <- as.factor(barcodeplotdata$Selected)
      barcodeplotdata$Iteration <- as.factor(barcodeplotdata$Iteration)
      barcodeplot <- ggplot(data = barcodeplotdata, aes(x=Iteration, y=Coefficient, fill=Selected)) + 
        geom_tile() + scale_fill_manual(values=c('white','black')) + theme_bw()
      return(barcodeplot)
    }
    
    getimportanceplot <- function(importances) {
      barcodeplotdata <- melt(importances)
      names(barcodeplotdata) <- c("iteration","coefficient","importance")
      barcodeplotdata$tteration <- as.factor(barcodeplotdata$iteration)
      barcodeplot <- ggplot(data = barcodeplotdata, aes(x=iteration, y=coefficient, fill=importance)) + 
        geom_raster() + 
        scale_fill_gradient(low="white", high="red") + theme_bw()
      return(barcodeplot)
    }
    
    ggsave(getbarcodeplot(smalllassobarcodes),file="Figures2/smalllasso.png")
    ggsave(getbarcodeplot(rpartchosen),file="Figures2/treechosen.png")
    ggsave(getimportanceplot(rfimportances),file="Figures2/rfimportances.png")
    
    
    
    
    
    # Analyze rules y-hat
    
    plotbygroup <- function(groups,type="density",df=holdout,addmean=F) {
      plotdata <- reshape(data=df[,c("race",groups)],varying=groups,v.names="riskprediction",
                          direction="long",timevar="model",times=groups)
      
      mediandata <- aggregate(riskprediction ~ model + race, plotdata, median)
      meandata <- aggregate(riskprediction ~ model + race, plotdata, mean)
      
      if(type == "cumm") {
        return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                 facet_wrap(~model) +
                 xlim(0,.5) +
                 stat_ecdf() +
                 xlab("risk prediction") +
                 ylab("cummulative proportion in hold-out") + theme_bw())
      }
      else if(type == "hist") {
        return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                 facet_wrap(~model) +
                 xlim(0,.08) +
                 #    geom_histogram(alpha=.2,binwidth=.002,position="dodge",aes(y=..count../sum(..count..))) +
                 geom_histogram(alpha=.3,binwidth=.002,position="identity",aes(y=.002*..density..),size=.5) +
                 xlab("risk prediction") +
                 ylab("proportion per bin in hold-out") + theme_bw())
      }
      else if(type == "freq") {
        return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                 facet_wrap(~model) +
                 xlim(0,.08) +
                 #    geom_histogram(alpha=.2,binwidth=.002,position="dodge",aes(y=..count../sum(..count..))) +
                 geom_freqpoly(binwidth=.002,position="identity",aes(y=.002*..density..)) +
                 geom_histogram(alpha=.2,binwidth=.002,position="identity",aes(y=.002*..density..),size=0) +
                 xlab("risk prediction") +
                 ylab("proportion per bin in hold-out") + theme_bw())
      }
      else {
        if(addmean) {
          return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                   facet_wrap(~model) +
                   xlim(0,.08) +
                   xlab("risk prediction") +
                   ylab("density in hold-out") +
                   geom_vline(data=mediandata,aes(xintercept=riskprediction,color=race)) +
                   geom_vline(data=meandata,aes(xintercept=riskprediction,color=race),linetype="dashed") +
                   geom_density(alpha=.2) + theme_bw())   # ,bw=.003
        }
        else {
          return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                   facet_wrap(~model) +
                   xlim(0,.08) +
                   xlab("risk prediction") +
                   ylab("density in hold-out") +
                   geom_vline(data=mediandata,aes(xintercept=riskprediction,color=race)) +
                   geom_density(alpha=.2) + theme_bw())    # ,bw=.003
          }
      }
    }
    

    ggsave("Figures2/densities.png",
           plotbygroup(c("forest","lasso"),type="dens"),width=10,height=5)
    ggsave("Figures2/hist.png",
           plotbygroup(c("forest","lasso"),type="hist"),width=10,height=5) 
    ggsave("Figures2/freq.png",
           plotbygroup(c("forest","lasso"),type="freq"),width=10,height=5)
    ggsave("Figures2/cummulative.png",
           plotbygroup(c("forest","lasso"),type="cumm"),width=10,height=5)    
    
    ggsave("Figures2/rf-densities.png",
           plotbygroup(c("forest"),type="dens"),width=6,height=5)
    ggsave("Figures2/rf-hist.png",
           plotbygroup(c("forest"),type="hist"),width=6,height=5) 
    ggsave("Figures2/rf-freq.png",
           plotbygroup(c("forest"),type="freq"),width=6,height=5)
    ggsave("Figures2/rf-cummulative.png",
           plotbygroup(c("forest"),type="cumm"),width=6,height=5)    
    
    # Adding makes better

      holdout3 <- holdout[,c("race","default")]
     
          # for(thisvar in setdiff(allvariables,c("coasex","nocreditlinesM","race"))) {
          #   thiswithout <- glm(as.formula(paste0("default ~ ",thisvar)), data=trainingsample, family="binomial")
          #   thiswith <- glm(as.formula(paste0("default ~ race + ",thisvar)), data=trainingsample, family="binomial")
          #   holdout3[,thisvar] <- predict(thiswithout,newdata=holdout0,type="response")
          #   holdout3[,paste0(thisvar,"andrace")] <- predict(thiswith,newdata=holdout0,type="response")
          # 
          #   thiswithoutforest <- randomForest(as.formula(paste0("default ~ ",thisvar)), data=thissample, nodesize=rfchosen)
          #   thiswithforest <- randomForest(as.formula(paste0("default ~ race + ",thisvar)), data=thissample, nodesize=rfchosen)
          #   holdout3[,paste0(thisvar,"forest")] <- predict(thiswithoutforest,newdata=holdout0,type="prob")[,2]
          #   holdout3[,paste0(thisvar,"andraceforest")] <- predict(thiswithforest,newdata=holdout0,type="prob")[,2]
          # 
          #   print(plotbygroup(c(thisvar,paste0(thisvar,"andrace"),paste0(thisvar,"forest"),paste0(thisvar,"andraceforest")),df=holdout3))
          # }

      thisvar <- "school"
      thiswithoutforest <- randomForest(as.formula(paste0("default ~ ",thisvar)), data=thissample, nodesize=rfchosen)
      thiswithforest <- randomForest(as.formula(paste0("default ~ race + ",thisvar)), data=thissample, nodesize=rfchosen)
      holdout3[,paste0("forest-school")] <- predict(thiswithoutforest,newdata=holdout0,type="prob")[,2]
      holdout3[,paste0("forest-schoolandrace")] <- predict(thiswithforest,newdata=holdout0,type="prob")[,2]
      ggsave("Figures2/addrace.png",
             plotbygroup(c(paste0("forest-school"),paste0("forest-schoolandrace")),df=holdout3,addmean=T),width=10,height=5)
    
    ggsave("Figures2/substitutability-lasso.png",
      plot=plotbygroup(c("lasso","lasso without race")),width=10,height=5)
    
    ggsave("Figures2/substitutability2-lasso.png",
      plot=plotbygroup(c("lasso","lasso without race","lasso without race, correlates")),width=15,height=5)
    
    ggsave("Figures2/substitutability-forest.png",
      plot=plotbygroup(c("forest","forest without race")),width=10,height=5)

    ggsave("Figures2/substitutability2-forest.png",
      plot=plotbygroup(c("forest","forest without race","forest without race, correlates")),width=15,height=5)
    

    plotdata <- holdout[,c("race","forest","propertycounty")]
    names(plotdata) <- c("race","riskprediction","propertycounty")
    
    mediandata <- aggregate(riskprediction ~ propertycounty + race, plotdata, median)
    meandata <- aggregate(riskprediction ~ propertycounty + race, plotdata, mean)
    
    ggsave("Figures2/distribution.png",
           ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
             facet_wrap(~propertycounty,labeller=label_both) +
             xlim(0,.08) +
             xlab("risk prediction") +
             ylab("density in hold-out") +
             geom_vline(data=mediandata,aes(xintercept=riskprediction,color=race)) + theme_bw() +
             geom_density(alpha=.2),width=10,height=5)

    
    
    # Stability
    ggsave("Figures2/lassostability.png",
           plotbygroup(c("lasso1","lasso2","lasso3")),width=15,height=5)
    
  
    
# Add diagnostic plots for barcode draws
    
      diagnosticgroups <- c("lasso1","lasso2","lasso3","forest","forest without race","forest without race, correlates")
      diagnosticholdout <- holdout[,c("race",diagnosticgroups,"default")]
      diagnosticholdout$default <- as.numeric(diagnosticholdout$default) - 1
      
      # Scatter plots
      
      diagnosticscatter <- ggplot(diagnosticholdout, aes(x=lasso1, y=lasso2, color=race)) +
              geom_point(alpha=.7,size=1,shape=20) +
              # geom_density2d(aes(alpha=..level..), geom="polygon") +
              xlim(0,.08) + ylim(0,.1) + theme_bw()    # ,bw=.003
      ggsave("Figures2/scatter12.png",diagnosticscatter,width=6,height=5)
      
      diagnosticscatter <- ggplot(diagnosticholdout, aes(x=lasso2, y=lasso3, color=race)) +
        geom_point(alpha=.7,size=1,shape=20) +
        # geom_density2d(aes(alpha=..level..), geom="polygon") +
        xlim(0,.08) + ylim(0,.1) + theme_bw()    # ,bw=.003
      ggsave("Figures2/scatter23.png",diagnosticscatter,width=6,height=5)
      
      diagnosticscatter <- ggplot(diagnosticholdout, aes(x=lasso3, y=lasso1, color=race)) +
        geom_point(alpha=.7,size=1,shape=20) +
        # geom_density2d(aes(alpha=..level..), geom="polygon") +
        xlim(0,.08) + ylim(0,.1) + theme_bw()    # ,bw=.003
      ggsave("Figures2/scatter31.png",diagnosticscatter,width=6,height=5)
      
      
      # ROC
      
      rocxy <- function(predictions,truth,steps,thisname) {
        xydata <- t(sapply(seq(from=0,to=1,length.out=steps),function(a) { c(sum((predictions >= a) * (1 - truth)) / sum(1 - truth), sum((predictions >= a) * truth) / sum(truth)) }))
        data.frame(FPR=xydata[,1],TPR=xydata[,2],method=thisname)
      }
      
      lassorocdata <- rbind(rocxy(diagnosticholdout$lasso1,diagnosticholdout$default,1000,"lasso1"),rocxy(diagnosticholdout$lasso2,diagnosticholdout$default,1000,"lasso2"),rocxy(diagnosticholdout$lasso3,diagnosticholdout$default,1000,"lasso3"))
      
      lassoroc <- ggplot(lassorocdata, aes(x=FPR, y=TPR)) +
        geom_line() +
        xlim(0,1) + ylim(0,1) + theme_bw() +    # ,bw=.003 +
        facet_wrap(~method)
      
      ggsave("Figures2/lassoroc.png",lassoroc,width=15,height=5)
 

smalllassos <- glmnet(Xtrain, trainingsample$default, family="binomial", lambda=lambdas)
holdout$lasso <- predict(smalllassos,newx=Xtest,type="response")[,lambdaind]
      
      
lasso_marital <- (rocxy(holdout[,-2],holdout[,2]))   


      forestrocdata <- rbind(rocxy(diagnosticholdout$forest,diagnosticholdout$default,1000,"forest"),
                             rocxy(diagnosticholdout[,"forest without race"],diagnosticholdout$default,1000,"forest without race"),
                             rocxy(diagnosticholdout[,"forest without race, correlates"],diagnosticholdout$default,1000,"forest without race, correlates"))
      
      forestroc <- ggplot(forestrocdata, aes(x=FPR, y=TPR)) +
        geom_line() +
        xlim(0,1) + ylim(0,1) + theme_bw() +    # ,bw=.003 +
        facet_wrap(~method)
      
      ggsave("Figures2/forestroc.png",forestroc,width=15,height=5)
      

      
        
# Obtain distribution of variables by county, and by county and group
  library(tidyr)
  mymeans <- aggregate(. ~ propertycounty1, data.frame(model.matrix(~ . - 1, holdout[,1:(which(names(holdout)=="default")[1])])), mean)
  mymeans$propertycounty1 <- paste0(mymeans$propertycounty1,"-mean")
  mysds <- aggregate(. ~ propertycounty1, data.frame(model.matrix(~ . - 1, holdout[,1:(which(names(holdout)=="default")[1])])), sd)
  mysds$propertycounty1 <- paste0(mysds$propertycounty1,"-sd")
  
  write.table(spread(rbind(gather(mymeans,"variable","value",appinc:defaultTRUE),
                           gather(mysds,"variable","value",appinc:defaultTRUE)), "propertycounty1", "value"), "variablesbycounty.csv", row.names=FALSE, sep=",") 
  
  mymeans <- aggregate(. ~ propertycounty1 + raceH + raceW, data.frame(model.matrix(~ . - 1, holdout[,1:(which(names(holdout)=="default")[1])])), mean)
  mymeans$propertycounty1 <-
    c("0-B","1-B","0-H","1-H","0-W","1-W")
  mymeans <- mymeans[ -c(2,3) ]
  mymeans$propertycounty1 <- paste0(mymeans$propertycounty1,"-mean")
  mysds <- aggregate(. ~ propertycounty1 + raceH + raceW, data.frame(model.matrix(~ . - 1, holdout[,1:(which(names(holdout)=="default")[1])])), sd)
  mysds$propertycounty1 <-
    c("0-B","1-B","0-H","1-H","0-W","1-W")
  mysds <- mysds[ -c(2,3) ]
  mysds$propertycounty1 <- paste0(mysds$propertycounty1,"-sd")
  
  write.table(spread(rbind(gather(mymeans,"variable","value",appinc:defaultTRUE),
                           gather(mysds,"variable","value",appinc:defaultTRUE)), "propertycounty1", "value"), "variablesbycountyandrace.csv", row.names=FALSE, sep=",") 
  
  table(holdout$propertycounty,holdout$race)
  
  
  


# Add downsampled, reduced-predictor, and recalibrated rules

  # Downsampled random forest
  
  smalln <- 200
  
  # Tune rf
  rftuning <- sapply(rfcand, function(mb) # go through different node size, and for every node size fit a random forest and save the losses
    loss(test$default,predict(randomForest(default ~ ., data=train[1:smalln,], nodesize=mb),
                              newdata=test,type="prob")[,2]))
  plot(rfcand,rftuning) #plot the losses and choose the best one 
  rfchosensmall <- rfcand[which.min(rftuning)]
  print(rfchosensmall)
  # Fit forest
  forestsmall <- randomForest(default ~ ., data=trainingsample[1:smalln,], nodesize=rfchosensmall)
  holdout$`downsampled forest` <- predict(forestsmall,newdata=holdout,type="prob")[,2]
  
  # Add reduced-predictor random forest
  
  # S45 and S46 (debt to income ratio)
  # S47 (fixed or adjustable loan)
  # S48 (term of loan)
  simsitvars1 <- c("housingdti","totaldti",'fixedadjustable','loanterm')
  
  #	Similarly situated is the 10 variables most correlated with race (used for the "exclusion" section of the paper).
  simsitvars2 <- mostcorr   
  
  simsitvars <- c(simsitvars1,simsitvars2)
  relvars <- c("default",simsitvars)
  # Tune rf
  rftuning <- sapply(rfcand, function(mb) # go through different node size, and for every node size fit a random forest and save the losses
    loss(test$default,predict(randomForest(default ~ ., data=train[,relvars], nodesize=mb),
                              newdata=test,type="prob")[,2]))
  plot(rfcand,rftuning) #plot the losses and choose the best one 
  rfchosensimsit <- rfcand[which.min(rftuning)]
  print(rfchosensimsit)
  # Fit forest
  forestsimsit <- randomForest(default ~ ., data=trainingsample[,relvars], nodesize=rfchosensimsit)
  holdout$`forest from sim sit` <- predict(forestsimsit,newdata=holdout,type="prob")[,2]
  
  # Add recalibrated random forest
  targetavg <- mean(holdout$forest)
  print(targetavg)
  offsets <- seq(-3,3,by=.01) #create a grid with increments. 
  logit <- function(p) log(p / (1 - p))
  logistic <- function(s) 1 / (1 + exp(-s))
  scores <- logit(holdout$forest)
  for(thisrace in levels(holdout$race)) {
    thisoffset <- offsets[which(lapply(offsets,
                                       function(offset) avgrate(scores[holdout$race == thisrace] + offset)) >= targetavg)[1]]
    scores[holdout$race == thisrace] <- scores[holdout$race == thisrace] + thisoffset }
  holdout$`recalibrated forest` <- logistic(scores)
  
  
  # Analyze rules y-hat

   plotbygroup <- function(groups,type="density",df=holdout,addmean=F) {
     plotdata <- reshape(data=df[,c("race",groups)],varying=groups,v.names="riskprediction",
                         direction="long",timevar="model",times=groups)
  
     mediandata <- aggregate(riskprediction ~ model + race, plotdata, median)
     meandata <- aggregate(riskprediction ~ model + race, plotdata, mean)
  
     if(type == "cumm") {
       return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                facet_wrap(~model) +
                xlim(0,.5) +
                stat_ecdf() +
                xlab("risk prediction") +
                ylab("cummulative proportion in hold-out") + theme_bw())
     }
     else if(type == "hist") {
       return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                facet_wrap(~model) +
                xlim(0,.1) +
                #    geom_histogram(alpha=.2,binwidth=.002,position="dodge",aes(y=..count../sum(..count..))) +
                geom_histogram(alpha=.3,binwidth=.002,position="identity",aes(y=.002*..density..),size=.5) +
                xlab("risk prediction") +
                ylab("proportion per bin in hold-out") + theme_bw())
     }
     else if(type == "freq") {
       return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                facet_wrap(~model) +
                xlim(0,.1) +
                #    geom_histogram(alpha=.2,binwidth=.002,position="dodge",aes(y=..count../sum(..count..))) +
                geom_freqpoly(binwidth=.002,position="identity",aes(y=.002*..density..)) +
                geom_histogram(alpha=.2,binwidth=.002,position="identity",aes(y=.002*..density..),size=0) +
                xlab("risk prediction") +
                ylab("proportion per bin in hold-out") + theme_bw())
     }
     else {
       if(addmean) {
         return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                  facet_wrap(~model) +
                  xlim(0,.1) +
                  xlab("risk prediction") +
                  ylab("density in hold-out") +
                  geom_vline(data=mediandata,aes(xintercept=riskprediction,color=race)) +
                  geom_vline(data=meandata,aes(xintercept=riskprediction,color=race),linetype="dashed") +
                  geom_density(alpha=.2) + theme_bw())   # ,bw=.003
       }
       else {
         return(ggplot(plotdata, aes(x=riskprediction, fill=race, color=race)) +
                  facet_wrap(~model) +
                  xlim(0,.1) +
                  xlab("risk prediction") +
                  ylab("density in hold-out") +
                  geom_vline(data=mediandata,aes(xintercept=riskprediction,color=race)) +
                  geom_density(alpha=.2) + theme_bw())    # ,bw=.003
       }
     }
   }


   ggsave("Figures_extended_V2/forest-distributions.png",
          plot=plotbygroup(c("forest","downsampled forest","forest from sim sit","recalibrated forest")),width=10,height=10)
  
  
  # ROC
  
  diagnosticholdout <- holdout
  diagnosticholdout$default <- as.numeric(holdout$default) - 1
  
  rocxy <- function(predictions,truth,steps,thismethod,thisrace) {
    xydata <- t(sapply(seq(from=0,to=1,length.out=steps),function(a) { c(sum((predictions >= a) * (1 - truth)) / sum(1 - truth), sum((predictions >= a) * truth) / sum(truth)) }))
    data.frame(FPR=xydata[,1],TPR=xydata[,2],method=thismethod,race=thisrace)
  }
  
  rocdata <- function(thesemethods) {
    do.call("rbind", lapply(thesemethods,
                            function(thismethod) {
                              do.call("rbind",lapply(levels(diagnosticholdout$race), function(thisrace) { 
                                rocxy(diagnosticholdout[diagnosticholdout$race == thisrace,thismethod],
                                      diagnosticholdout[diagnosticholdout$race == thisrace,"default"],
                                      2000,thismethod,thisrace) } ))
                            })
    )
  }
  
  rocxy4 <- function(predictions,truth,steps,thismethod) {
    xydata <- t(sapply(seq(from=0,to=1,length.out=steps),function(a) { c(sum((predictions >= a) * (1 - truth)) / sum(1 - truth), sum((predictions >= a) * truth) / sum(truth)) }))
    data.frame(FPR=xydata[,1],TPR=xydata[,2],method=thismethod)
  }
  
  rocdata4 <- function(thesemethods) {
    do.call("rbind", lapply(thesemethods,
                            function(thismethod) {
                                rocxy4(diagnosticholdout[,thismethod],
                                      diagnosticholdout[,"default"],
                                      2000,thismethod) } ))
                            }
 
  
  forestrocdata <- rocdata(c("forest","forest from sim sit"))
  forestrocdata4 <- rocdata4(c("forest","forest from sim sit"))
  

# Produce extended graphs
  
  forestrocbyrace <- ggplot(forestrocdata, aes(x=FPR, y=TPR, color=race)) +
    geom_line() +
    xlim(0,1) + ylim(0,1) + theme_bw() +    # ,bw=.003 +
    facet_wrap(~method)
  
  ggsave("Figures_extended_V2/forestrocbyrace.png",forestrocbyrace,width=10,height=5)
  
  forestroc <- ggplot(forestrocdata4, aes(x=FPR, y=TPR)) +
    geom_line() +
    xlim(0,1) + ylim(0,1) + theme_bw() +    # ,bw=.003 +
    facet_wrap(~method)
  
  ggsave("Figures_extended_V2/forestroc.png",forestroc,width=10,height=5)