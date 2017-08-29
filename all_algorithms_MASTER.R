# printnames = function(x)
# {
#   many =ncol(x)
#   for (i in 1:many)
#   {
#     cat(i,"-",names(x)[i],"\n")
#   }
# }
# accuracy_function <- function(testedd, predicted)
# {
#   t = as.character(testedd)
#   p = as.character(predicted)
#   lenn = length(testedd)
#   count = 0
#   for (i in 1:lenn)
#   {
#     if (t[i] == p[i])
#     {
#       count = count + 1
#     }
#   }
#   return (count/lenn)
# }
#
# normalize <- function(x){
#   return ((x - min(x)) / (max(x) - min(x)))
# }

library(NbaFunctions)

library(caret)
library(vcd)
library(dplyr)

library(rBayesianOptimization)

##algortihms
library(RSNNS)
library(class) ##33 is sqrt of 110
library(ipred)
library(e1071)
library(randomForest)
library(kernlab)




##what is master folder????
root_folder = choose.dir(default = "", caption = "Select folder")
FeatureSets_folder = paste0(root_folder,"/FeatureSets")
setwd(FeatureSets_folder)


feature_set = dir()
algorithm_choices = c("ann","bag","knn","log","nab","raf","svm")

Results <- function(seasons, featureSets, algorithms)
{
the_results = data.frame(memo              =  character(),
                       label               =  character(),
                       season              =  integer(),
                       part_of_season      =  character(),
                       date                =  character(),
                       home_team           =  character(),
                       away_team           =  character(),
                       predicted_L         =  character(),
                       probability_L       =  numeric(),
                       parameter_opt       =  character(),
                       cv_accuracy         =  numeric(),
                       feature_set         =  character(),
                       number_of_features  =  integer(),
                       algorithm           =  character())



for(fs_iter in featureSets)
{

current_feature_set = read.csv(feature_set[fs_iter],stringsAsFactors = F)

for(alg_iter in algorithms)
{
##gets results for each season
for(iii in seasons)
{
  print(paste0("Start Season: ",iii,"           FeatureSet: ",fs_iter," - ",feature_set[fs_iter]))

  ##temp is current season data
  temp = current_feature_set[current_feature_set$season_year==iii,]


  current_train = temp[temp$train_test=="Train" ,c(9:ncol(temp))]
  current_test = temp[temp$train_test=="Test" ,c(9:ncol(temp))]

  current_train_labels = temp[temp$train_test=="Train" ,c(3:8,1:2)]
  current_test_labels = temp[temp$train_test=="Test" ,c(3:8,1:2)]

  set.seed(666)
  folds <- createFolds(current_train_labels[,1], k=5, list = TRUE, returnTrain = FALSE)
  ##########################################
  ##########################################
  ##                                      ##
  ##     AAAA       NN    NN    NN    NN  ##
  ##    AA  AA      NNN   NN    NNN   NN  ##
  ##    AA  AA      NNNN  NN    NNNN  NN  ##          Start
  ##   AAAAAAAA     NN NN NN    NN NN NN  ##
  ##   AA    AA     NN  NNNN    NN  NNNN  ##
  ##  AA      AA    NN   NNN    NN   NNN  ##
  ##  AA      AA    NN    NN    NN    NN  ##
  ##                                      ##
  ##########################################
  ##########################################
  if(alg_iter == 1)
  {
  print("             Start ANN")

    if(ncol((current_train-8)) > 10)
    {

      Alg_for_opt <- function(hl_1,hl_2,the_iterations,learn_rate)
      {
        cv_results <- lapply(folds, function(zeta)
        {
          ## creating training and validation!!
          train = current_train[-zeta,]
          validation = current_train[zeta,]
          train_labels = current_train_labels[-zeta,]
          validation_labels = current_train_labels[zeta,]

          ##performing ANN
          m = mlp(train, ifelse(train_labels[,1]=="W",1,0),size=c(hl_1,hl_2), maxit = the_iterations,
                  initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
                  learnFunc = "Std_Backpropagation", learnFuncParams = c(learn_rate),
                  updateFunc = "Topological_Order", hiddenActFunc = "Act_Logistic")

          p_prob = predict(m, validation)
          #p_prob = as.data.frame(p_prob)
          #p_prob = p_prob$W
          p_class = ifelse(p_prob >= .5, "W","L")
          acc =  accuracy_function(p_class,validation_labels[,1])

          return (acc)
        })
        list(Score = mean(unlist(cv_results)), Pred = 0)
      }

      ##Bayesian optimisation!!
      OPT_Res <- BayesianOptimization(Alg_for_opt,
                                      bounds = list(hl_1 = c(15L,25L),
                                                    hl_2 = c(2L,10L),
                                                    the_iterations =c(500L,1500L),
                                                    learn_rate=c(.1,1)),
                                      init_points = 6, n_iter = 6,
                                      acq = "ucb", kappa = 2.576, eps = 0.0,
                                      verbose = TRUE)
      best_cv_accuracy = OPT_Res$Best_Value
      best_parameters = paste("hl_1-",OPT_Res$Best_Par[[1]],
                                " hl_2-",OPT_Res$Best_Par[[2]],
                                " iter-",OPT_Res$Best_Par[[3]],
                                " lnrt-",OPT_Res$Best_Par[[4]],
                                sep="")
    } else  ##10 features or less
    {
      Alg_for_opt <- function(hl_1,the_iterations,learn_rate)
      {
        cv_results <- lapply(folds, function(zeta)
        {
          ## creating training and validation!!
          train = current_train[-zeta,]
          validation = current_train[zeta,]
          train_labels = current_train_labels[-zeta,]
          validation_labels = current_train_labels[zeta,]

          ##performing ANN
          m = mlp(train, ifelse(train_labels[,1]=="W",1,0),size=c(hl_1), maxit = the_iterations,
                  initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
                  learnFunc = "Std_Backpropagation", learnFuncParams = c(learn_rate),
                  updateFunc = "Topological_Order", hiddenActFunc = "Act_Logistic")

          p_prob = predict(m, validation)
          #p_prob = as.data.frame(p_prob)
          #p_prob = p_prob$W
          p_class = ifelse(p_prob >= .5, "W","L")
          acc =  accuracy_function(p_class,validation_labels[,1])

          return (acc)
        })
        list(Score = mean(unlist(cv_results)), Pred = 0)
      }

      ##Bayesian optimisation!!
      OPT_Res <- BayesianOptimization(Alg_for_opt,
                                      bounds = list(hl_1 = c(1L,8L),
                                                    the_iterations =c(500L,1500L),
                                                    learn_rate=c(.1,1)),
                                      init_points = 6, n_iter = 6,
                                      acq = "ucb", kappa = 2.576, eps = 0.0,
                                      verbose = TRUE)
      best_cv_accuracy = OPT_Res$Best_Value
      best_parameters = paste("hl_1-",OPT_Res$Best_Par[[1]],
                              " iter-",OPT_Res$Best_Par[[2]],
                              " lnrt-",OPT_Res$Best_Par[[3]],
                              sep="")
    }


    #########################################
    ######## averages over 5 trials start  ##
    #########################################
    runs = 5
    ensemble_voting = data.frame(date_performes=rep(Sys.Date(),nrow(current_test)))
    for(i in 1:runs)
    {
      if(ncol((current_train-8)) > 10)
      {
        m = mlp(current_train, ifelse(current_train_labels[,1]=="W",1,0),
                size=c(OPT_Res$Best_Par[[1]],OPT_Res$Best_Par[[2]]),
                maxit = OPT_Res$Best_Par[[3]],
                initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
                learnFunc = "Std_Backpropagation", learnFuncParams = c(OPT_Res$Best_Par[[4]]),
                updateFunc = "Topological_Order", hiddenActFunc = "Act_Logistic")

        p_prob = predict(m, current_test)#, type = "prob")
        #p_prob = as.data.frame(p_prob)
        #p_prob = p_prob$V1
        p_class = ifelse(p_prob >= .5, "W","L")
        } else
      {
        m = mlp(current_train, ifelse(current_train_labels[,1]=="W",1,0),
                size=c(OPT_Res$Best_Par[[1]]), maxit = OPT_Res$Best_Par[[2]],
                initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
                learnFunc = "Std_Backpropagation", learnFuncParams = c(OPT_Res$Best_Par[[3]]),
                updateFunc = "Topological_Order", hiddenActFunc = "Act_Logistic")

        p_prob = predict(m, current_test)#, type = "prob")
        #p_prob = as.data.frame(p_prob)
        #p_prob = p_prob$V1
        p_class = ifelse(p_prob >= .5, "W","L")
      }


      ensemble_voting = cbind(ensemble_voting,p_class)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("label_",i,sep="")
      ensemble_voting = cbind(ensemble_voting,p_prob)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("prob_of_label_",i,sep="")
      ##end ensembling
    }


    ##now the ensembline
    ensemble_voting$predicted_label = "L"
    ensemble_voting$prob_of_label = 0

    for(i in 1:nrow(current_test))
    {
      count = 0

      for(j in seq(2,runs*2+1,2))
      {
        if(ensemble_voting[i,j] == "W")
        {
          count = count + 1
        }
      }
      if (count > runs %/% 2)
      {
        ensemble_voting$predicted_label[i] = "W"
      }

      sum = 0
      for(j in seq(3,runs*2+1,2))
      {
        sum = sum + ensemble_voting[i,j]
      }
      ensemble_voting$prob_of_label[i] = sum / runs
    }

    #########################################
    ######## averages over 5 trials end    ##
    #########################################

    pred_temp = data.frame(memo                =  current_test_labels[,7],
                           label               =  current_test_labels[,1],
                           season              =  current_test_labels[,2],
                           part_of_season      =  current_test_labels[,3],
                           date                =  current_test_labels[,4],
                           home_team           =  current_test_labels[,5],
                           away_team           =  current_test_labels[,6],
                           predicted_L         =  ensemble_voting$predicted_label,
                           probability_L       =  ensemble_voting$prob_of_label,
                           parameter_opt       =  rep(best_parameters,nrow(current_test_labels)),
                           cv_accuracy         =  best_cv_accuracy,
                           feature_set         =  rep(feature_set[fs_iter],nrow(current_test_labels)),
                           number_of_features  =  ncol(temp) - 9,
                           algorithm           =  algorithm_choices[alg_iter])

    print(paste("CV_acc: ", round(unique(pred_temp$cv_accuracy),3),
                ". Test_acc: ",round(accuracy_function(pred_temp$label,pred_temp$predicted_L),3),
                ". Difference = ", round(accuracy_function(pred_temp$label,pred_temp$predicted_L)
                                         - unique(pred_temp$cv_accuracy),3)))

    the_results =  rbind(the_results, pred_temp)
  }

  ##########################################
  ##########################################
  ##                                      ##
  ##     AAAA       NN    NN    NN    NN  ##
  ##    AA  AA      NNN   NN    NNN   NN  ##
  ##    AA  AA      NNNN  NN    NNNN  NN  ##          End
  ##   AAAAAAAA     NN NN NN    NN NN NN  ##
  ##   AA    AA     NN  NNNN    NN  NNNN  ##
  ##  AA      AA    NN   NNN    NN   NNN  ##
  ##  AA      AA    NN    NN    NN    NN  ##
  ##                                      ##
  ##########################################
  ##########################################



  ##############################################
  ##############################################
  ##                                          ##
  ##  BBBBBB        AAAA          GGGGGGG     ##
  ##  BB   BB      AA  AA        GG     GG    ##
  ##  BB  BB       AA  AA       GG            ##          Start
  ##  BBBB        AA    AA     GG             ##
  ##  BB  BB      AAAAAAAA      GG    GGGGGG  ##
  ##  BB   BB    AA      AA      GG      GG   ##
  ##  BBBBBB     AA      AA       GGGGGGGG    ##
  ##                                          ##
  ##############################################
  ##############################################


  if(alg_iter == 2)
  {

    print("             Start Bag")

    # set.seed(666)
    # folds <- createFolds(current_train_labels[,1], k=5, list = TRUE, returnTrain = FALSE)

    Alg_for_opt <- function(number_of_bags)
    {
      cv_results <- lapply(folds, function(zeta)
      {
        ## creating training and validation!!
        train = current_train[-zeta,]
        validation = current_train[zeta,]
        train_labels = current_train_labels[-zeta,]
        validation_labels = current_train_labels[zeta,]


        bag_train = cbind(train_labels[,1],train)
        names(bag_train)[1] = "W.L"
        bag_validation = cbind(validation_labels[,1],validation)
        names(bag_validation)[1] = "W.L"

        ##performing bag
        predictors = paste(names(bag_train)[2:length(bag_train)],collapse = " + ")
        form = as.formula(paste("W.L ~ ",predictors,collapse = " + "))

        m <- bagging(form, data = bag_train, nbagg=number_of_bags)
        #m <- bagging(form, data=bag_train, nbagg=number_of_bags, method=c("standard","double"))
        p <- predict(m,bag_validation,type = "class")
        #p <-p$class
        print(accuracy_function(bag_validation[,1],p))
        acc =  accuracy_function(bag_validation[,1],p)

        return (acc)
      })
      list(Score = mean(unlist(cv_results)), Pred = 0)
    }


    ##Bayesian optimisation!!
    OPT_Res <- BayesianOptimization(Alg_for_opt,
                                    bounds = list(number_of_bags = c(25L,100L)),
                                    init_points = 5, n_iter = 5,
                                    acq = "ucb", kappa = 2.576, eps = 0.0,
                                    verbose = TRUE)
    best_cv_accuracy = OPT_Res$Best_Value
    ##### ensemblin
    best_parameters = paste("number_of_bags-",OPT_Res$Best_Par[[1]],sep="")

    #########################################
    ######## averages over 5 trials start  ##
    #########################################
    runs = 5
    ensemble_voting = data.frame(date_performes=rep(Sys.Date(),nrow(current_test)))
    for (i in 1:runs)
    {
      bag_train = cbind(current_train_labels[,1],current_train)
      names(bag_train)[1] = "W.L"
      bag_test = cbind(current_test_labels[,1],current_test)
      names(bag_test)[1] = "W.L"

      predictors = paste(names(bag_train)[2:length(bag_train)],collapse = " + ")
      form = as.formula(paste("W.L ~",predictors,collapse = " + "))

      m <- bagging(form, data = bag_train, nbagg=OPT_Res$Best_Par[[1]])
      #m <- bagging(form, data=bag_train, nbagg=number_of_bags, method=c("standard","double"))
      p <- predict(m,bag_test,type = "class")
      #p <-p$class

      ensemble_voting = cbind(ensemble_voting,p)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("label_",i,sep="")

      p <- predict(m,bag_test,type = "prob")
      p_prob =as.data.frame(p)
      p_prob = p_prob$W
      ensemble_voting = cbind(ensemble_voting,p_prob)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("prob_of_label_",i,sep="")
      ##end ensembling
    }

    ##now the ensembline
    ensemble_voting$predicted_label = "L"
    ensemble_voting$prob_of_label = 0

    for(i in 1:nrow(current_test))
    {
      count = 0

      for(j in seq(2,runs*2+1,2))
      {
        if(ensemble_voting[i,j] == "W")
        {
          count = count + 1
        }
      }
      if (count > runs %/% 2)
      {
        ensemble_voting$predicted_label[i] = "W"
      }

      sum = 0
      for(j in seq(3,runs*2+1,2))
      {
        sum = sum + ensemble_voting[i,j]
      }
      ensemble_voting$prob_of_label[i] = sum / runs
    }
    #########################################
    ######## averages over 5 trials end    ##
    #########################################


    pred_temp = data.frame(memo                =  current_test_labels[,7],
                           label               =  current_test_labels[,1],
                           season              =  current_test_labels[,2],
                           part_of_season      =  current_test_labels[,3],
                           date                =  current_test_labels[,4],
                           home_team           =  current_test_labels[,5],
                           away_team           =  current_test_labels[,6],
                           predicted_L         =  ensemble_voting$predicted_label,
                           probability_L       =  ensemble_voting$prob_of_label,
                           parameter_opt       =  rep(best_parameters,nrow(current_test_labels)),
                           cv_accuracy         =  best_cv_accuracy,
                           feature_set         =  rep(feature_set[fs_iter],nrow(current_test_labels)),
                           number_of_features  =  ncol(temp) - 9,
                           algorithm           =  algorithm_choices[alg_iter])

    print(paste("CV_acc: ", round(unique(pred_temp$cv_accuracy),3),
                ". Test_acc: ",round(accuracy_function(pred_temp$label,pred_temp$predicted_L),3),
                ". Difference = ", round(accuracy_function(pred_temp$label,pred_temp$predicted_L)
                                         - unique(pred_temp$cv_accuracy),3)))

    the_results =  rbind(the_results, pred_temp)
  }


  ##############################################
  ##############################################
  ##                                          ##
  ##  BBBBBB        AAAA          GGGGGGG     ##
  ##  BB   BB      AA  AA        GG     GG    ##
  ##  BB  BB       AA  AA       GG            ##          END
  ##  BBBB        AA    AA     GG             ##
  ##  BB  BB      AAAAAAAA      GG    GGGGGG  ##
  ##  BB   BB    AA      AA      GG      GG   ##
  ##  BBBBBB     AA      AA       GGGGGGGG    ##
  ##                                          ##
  ##############################################
  ##############################################


  #######################################
  #######################################
  ##                                   ##
  ##  KK   KK    NN    NN    NN    NN  ##
  ##  KK  KK     NNN   NN    NNN   NN  ##
  ##  KK KK      NNNN  NN    NNNN  NN  ##          sTART
  ##  KKKK       NN NN NN    NN NN NN  ##
  ##  KK KK      NN  NNNN    NN  NNNN  ##
  ##  KK  KK     NN   NNN    NN   NNN  ##
  ##  KK   KK    NN    NN    NN    NN  ##
  ##                                   ##
  #######################################
  #######################################



   if(alg_iter == 3)
   {

      print("             Start kNN")

    # set.seed(666)
    # folds <- createFolds(current_train_labels[,1], k=5, list = TRUE, returnTrain = FALSE)

    Alg_for_opt <- function(x)
    {
      cv_results <- lapply(folds, function(zeta)
        {
        ## creating training and validation!!
        train = current_train[-zeta,]
        validation = current_train[zeta,]
        train_labels = current_train_labels[-zeta,]
        validation_labels = current_train_labels[zeta,]

        ##performing knn
        model = knn(train = train, test = validation, cl = train_labels[,1], k = x)
        acc = accuracy_function(validation_labels[,1], model)

        return (acc)
        })
      list(Score = mean(unlist(cv_results)), Pred = 0)
    }


    ##Bayesian optimisation!!
    OPT_Res <- BayesianOptimization(Alg_for_opt,
                                    bounds = list(x = c(1L,100L)),
                                    init_points = 10, n_iter = 10,
                                    acq = "ucb", kappa = 2.576, eps = 0.0,
                                    verbose = F)
    ## get non even to prevent ties
    k_getter = as.data.frame(OPT_Res$History)
    k_getter = k_getter[order(k_getter$Value,decreasing = T),]
    k_i = 1
    while (k_getter$x[k_i]%%2==0)
    {
      k_i = k_i + 1
    }
    k_getter$x[k_i]

    best_cv_accuracy = k_getter$Value[k_i]
    best_k = paste0("k-", k_getter$x[k_i])
    print("             opt")

#########################################
######## averages over 5 trials start  ##
#########################################
    runs = 5
    ensemble_voting = data.frame(date_performes=rep(Sys.Date(),nrow(current_test)))
    for (i in 1:runs)
    {
      model = knn(train=current_train,test=current_test,cl=current_train_labels[,1],
                  k=k_getter$x[k_i], prob = TRUE)
      m = as.data.frame(model)
      m = cbind(m,      attr(model,"prob"))
      names(m)= c("label","prob")
      ##changes to represent prob of home team
      m$prob = ifelse(m$label=="W",m$prob,1-m$prob)
      ensemble_voting = cbind(ensemble_voting,m$label)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("label_",i,sep="")
      ensemble_voting = cbind(ensemble_voting,m$prob)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("prob_of_label_",i,sep="")
    }

    ##now the ensembline
    ensemble_voting$predicted_label = "L"
    ensemble_voting$prob_of_label = 0

  for(i in 1:nrow(current_test))
  {
    count = 0

    for(j in seq(2,runs*2+1,2))
    {
      if(ensemble_voting[i,j] == "W")
      {
        count = count + 1
      }
    }
    if (count > runs %/% 2)
    {
      ensemble_voting$predicted_label[i] = "W"
    }

    sum = 0
    for(j in seq(3,runs*2+1,2))
    {
      sum = sum + ensemble_voting[i,j]
    }
    ensemble_voting$prob_of_label[i] = sum / runs
  }
#########################################
######## averages over 5 trials end    ##
#########################################


      pred_temp = data.frame(memo                =  current_test_labels[,7],
                           label               =  current_test_labels[,1],
                           season              =  current_test_labels[,2],
                           part_of_season      =  current_test_labels[,3],
                           date                =  current_test_labels[,4],
                           home_team           =  current_test_labels[,5],
                           away_team           =  current_test_labels[,6],
                           predicted_L         =  ensemble_voting$predicted_label,
                           probability_L       =  ensemble_voting$prob_of_label,
                           parameter_opt       =  rep(best_k,nrow(current_test_labels)),
                           cv_accuracy         =  best_cv_accuracy,
                           feature_set         =  rep(feature_set[fs_iter],nrow(current_test_labels)),
                           number_of_features  =  ncol(temp) - 9,
                           algorithm           =  algorithm_choices[alg_iter])

    print(paste("CV_acc: ", round(unique(pred_temp$cv_accuracy),3),
                ". Test_acc: ",round(accuracy_function(pred_temp$label,pred_temp$predicted_L),3),
                ". Difference = ", round(accuracy_function(pred_temp$label,pred_temp$predicted_L)
                - unique(pred_temp$cv_accuracy),3)))

    the_results =  rbind(the_results, pred_temp)
   }

      #######################################
      #######################################
      ##                                   ##
      ##  KK   KK    NN    NN    NN    NN  ##
      ##  KK  KK     NNN   NN    NNN   NN  ##
      ##  KK KK      NNNN  NN    NNNN  NN  ##          End
      ##  KKKK       NN NN NN    NN NN NN  ##
      ##  KK KK      NN  NNNN    NN  NNNN  ##
      ##  KK  KK     NN   NNN    NN   NNN  ##
      ##  KK   KK    NN    NN    NN    NN  ##
      ##                                   ##
      #######################################
      #######################################


  ##############################################
  ##############################################
  ##                                          ##
  ##  LL         OOOOOOOOOO        GGGGGGGG   ##
  ##  LL         OOOOOOOOOO      GG     GGG  ##
  ##  LL         OO      OO     GG            ##          START
  ##  LL         OO      OO    GG             ##
  ##  LL         OO      OO     GG    GGGGGG  ##
  ##  LLLLLL     OOOOOOOOOO      GG      GG   ##
  ##  LLLLLL     OO000000O0       GGGGGGGG    ##
  ##                                          ##
  ##############################################
  ##############################################
  if(alg_iter == 4)
  {
    link_function = c("logit", "probit", "cloglog")  ##### Logistic #######

    print("             Start LOGISTIC")

    # set.seed(666)
    # folds <- createFolds(current_train_labels[,1], k=5, list = TRUE, returnTrain = FALSE)
    link_accuracies =c(-666,-666,-666)
    for(i in 1:3){
      cv_results <- lapply(folds, function(xxxxx){
        train = current_train[-xxxxx,]
        test = current_train[xxxxx,]
        train_labels = current_train_labels[-xxxxx,]
        test_labels = current_train_labels[xxxxx,]

        log_train = cbind(train_labels[,1],train)
        names(log_train)[1] = "W.L"
        log_test = cbind(test_labels[,1],test)
        names(log_test)[1] = "W.L"

        predictors = paste(names(log_train)[2:length(log_train)],collapse = " + ")
        form = as.formula(paste("W.L ~",predictors,collapse = " + "))

        m <- glm(form, data = log_train, family = binomial(link=link_function[i]))
        p_prob <- predict(m,log_test,type = "response")
        p_class <- as.character(ifelse(p_prob>0.5,"W","L"))
        acc = accuracy_function(log_test[,1],p_class)
        return (acc)
      })
      link_accuracies[i] = mean(unlist(cv_results))
    }
    # print(link_accuracies)
    # print(link_function[which.max(link_accuracies)])
    # print(max(link_accuracies))
    #

    best_cv_accuracy = max(link_accuracies)
    ##### ensemblin
    best_parameters = link_function[which.max(link_accuracies)]



    # log_train = cbind(train_labels[,1],train)
    # names(log_train)[1] = "W.L"
    log_train = cbind(current_train_labels[,1],current_train)
    names(log_train)[1] = "W.L"
    log_test = cbind(current_test_labels[,1],current_test)
    names(log_test)[1] = "W.L"

    predictors = paste(names(log_train)[2:length(log_train)],collapse = " + ")
    form = as.formula(paste("W.L ~",predictors,collapse = " + "))


    m <- glm(form, data = log_train, family = binomial(link=best_parameters))
    #summary(m)
    p_prob <- predict(m,log_test,type = "response")
    p_class <- as.character(ifelse(p_prob>0.5,"W","L"))



    pred_temp = data.frame(memo                =  current_test_labels[,7],
                           label               =  current_test_labels[,1],
                           season              =  current_test_labels[,2],
                           part_of_season      =  current_test_labels[,3],
                           date                =  current_test_labels[,4],
                           home_team           =  current_test_labels[,5],
                           away_team           =  current_test_labels[,6],
                           predicted_L         =  p_class,
                           probability_L       =  p_prob,
                           parameter_opt       =  rep(best_parameters,nrow(current_test_labels)),
                           cv_accuracy         =  best_cv_accuracy,
                           feature_set         =  rep(feature_set[fs_iter],nrow(current_test_labels)),
                           number_of_features  =  ncol(temp) - 9,
                           algorithm           =  algorithm_choices[alg_iter])

    print(paste("CV_acc: ", round(unique(pred_temp$cv_accuracy),3),
                ". Test_acc: ",round(accuracy_function(pred_temp$label,pred_temp$predicted_L),3),
                ". Difference = ", round(accuracy_function(pred_temp$label,pred_temp$predicted_L)
                                         - unique(pred_temp$cv_accuracy),3)))

    the_results =  rbind(the_results, pred_temp)
  }


  ##############################################
  ##############################################
  ##                                          ##
  ##  LL         OOOOOOOOOO        GGGGGGGG   ##
  ##  LL         OOOOOOOOOO      GG     GGG  ##
  ##  LL         OO      OO     GG            ##          END
  ##  LL         OO      OO    GG             ##
  ##  LL         OO      OO     GG    GGGGGG  ##
  ##  LLLLLL     OOOOOOOOOO      GG      GG   ##
  ##  LLLLLL     OO000000O0       GGGGGGGG    ##
  ##                                          ##
  ##############################################
  ##############################################



  ##########################################
  ##########################################
  ##                                      ##
  ##  NN    NN       AAAA       BBBBBB    ##
  ##  NNN   NN      AA  AA      BB   BB   ##
  ##  NNNN  NN      AA  AA      BB  BB    ##          START
  ##  NN NN NN     AA    AA     BBBB      ##
  ##  NN  NNNN     AAAAAAAA     BB  BB    ##
  ##  NN   NNN    AA      AA    BB   BB   ##
  ##  NN    NN    AA      AA    BBBBBB    ##
  ##                                      ##
  ##########################################
  ##########################################
  if(alg_iter == 5)
  {

    print("             Start NAIVE BAYES")

    # set.seed(666)
    # folds <- createFolds(current_train_labels[,1], k=5, list = TRUE, returnTrain = FALSE)

    cv_results <- lapply(folds, function(xxxxx){
        train = current_train[-xxxxx,]
        test = current_train[xxxxx,]
        train_labels = current_train_labels[-xxxxx,]
        test_labels = current_train_labels[xxxxx,]

        m = naiveBayes(train, train_labels[,1])
        p = as.data.frame(predict(m, test,type="raw",threshold = 0.001))
        p_labels =ifelse(p$W>=.5,"W","L")

        acc = accuracy_function(p_labels,test_labels[,1])

        return (acc)
      })
      best_cv_accuracy = mean(unlist(cv_results))


      m = naiveBayes(current_train, current_train_labels[,1])
      p = as.data.frame(predict(m, current_test,type="raw",threshold = 0.001))
      p_labels =ifelse(p$W>=.5,"W","L")



      current_test

    pred_temp = data.frame(memo                =  current_test_labels[,7],
                           label               =  current_test_labels[,1],
                           season              =  current_test_labels[,2],
                           part_of_season      =  current_test_labels[,3],
                           date                =  current_test_labels[,4],
                           home_team           =  current_test_labels[,5],
                           away_team           =  current_test_labels[,6],
                           predicted_L         =  p_labels,
                           probability_L       =  p$W,
                           parameter_opt       =  rep("NB_none",nrow(current_test_labels)),
                           cv_accuracy         =  best_cv_accuracy,
                           feature_set         =  rep(feature_set[fs_iter],nrow(current_test_labels)),
                           number_of_features  =  ncol(temp) - 9,
                           algorithm           =  algorithm_choices[alg_iter])

    print(paste("CV_acc: ", round(unique(pred_temp$cv_accuracy),3),
                ". Test_acc: ",round(accuracy_function(pred_temp$label,pred_temp$predicted_L),3),
                ". Difference = ", round(accuracy_function(pred_temp$label,pred_temp$predicted_L)
                                         - unique(pred_temp$cv_accuracy),3)))

    the_results =  rbind(the_results, pred_temp)
  }


  ##########################################
  ##########################################
  ##                                      ##
  ##  NN    NN       AAAA       BBBBBB    ##
  ##  NNN   NN      AA  AA      BB   BB   ##
  ##  NNNN  NN      AA  AA      BB  BB    ##          End
  ##  NN NN NN     AA    AA     BBBB      ##
  ##  NN  NNNN     AAAAAAAA     BB  BB    ##
  ##  NN   NNN    AA      AA    BB   BB   ##
  ##  NN    NN    AA      AA    BBBBBB    ##
  ##                                      ##
  ##########################################
  ##########################################


  ##########################################
  ##########################################
  ##                                      ##
  ##  RRRRRR        AAAA       FFFFFFFF   ##
  ##  RR   RR      AA  AA      FFFFFFFF   ##
  ##  RR  RR       AA  AA      FF         ##          START
  ##  RRRR        AA    AA     FFFFF      ##
  ##  RR  RR      AAAAAAAA     FFFFF      ##
  ##  RR   RR    AA      AA    FF         ##
  ##  RR    RR   AA      AA    FF         ##
  ##                                      ##
  ##########################################
  ##########################################


  if(alg_iter == 6)
  {

    print("             Start RANDOM FOREST")

    # set.seed(666)
    # folds <- createFolds(current_train_labels[,1], k=5, list = TRUE, returnTrain = FALSE)

    Alg_for_opt <- function(number_of_trees)
    {
      cv_results <- lapply(folds, function(zeta)
      {
        ## creating training and validation!!
        train = current_train[-zeta,]
        test = current_train[zeta,]
        train_labels = current_train_labels[-zeta,]
        test_labels = current_train_labels[zeta,]

##fix validation
        rf_train = cbind(train_labels[,1],train)
        names(rf_train)[1] = "W.L"
        rf_test = cbind(test_labels[,1],test)
        names(rf_test)[1] = "W.L"

        ##performing bag
        predictors = paste(names(rf_train)[2:length(rf_train)],collapse = " + ")
        form = as.formula(paste("W.L ~ ",predictors,collapse = " + "))

        rf <- randomForest(form, data = rf_train, ntree=number_of_trees)

        p <- predict(rf, rf_test,type = "response")

        acc = accuracy_function(p,rf_test[,1])

        return (acc)
      })
      list(Score = mean(unlist(cv_results)), Pred = 0)
    }


    ##Bayesian optimisation!!
    OPT_Res <- BayesianOptimization(Alg_for_opt,
                                    bounds = list(number_of_trees = c(50L,200L)),
                                    init_points = 7, n_iter = 8,
                                    acq = "ucb", kappa = 2.576, eps = 0.0,
                                    verbose = TRUE)
    best_cv_accuracy = OPT_Res$Best_Value
    ##### ensemblin
    best_parameters = paste("number_of_trees-",OPT_Res$Best_Par[[1]],sep="")

    #########################################
    ######## averages over 5 trials start  ##
    #########################################
    runs = 5
    ensemble_voting = data.frame(date_performes=rep(Sys.Date(),nrow(current_test)))
    for (i in 1:runs)
    {
      rf_train = cbind(current_train_labels[,1],current_train)
      names(rf_train)[1] = "W.L"
      rf_test = cbind(current_test_labels[,1],current_test)
      names(rf_test)[1] = "W.L"

      predictors = paste(names(rf_train)[2:length(rf_train)],collapse = " + ")
      form = as.formula(paste("W.L ~",predictors,collapse = " + "))

      rf <- randomForest(form, data = rf_train, trees=OPT_Res$Best_Par[[1]])

      p <- predict(rf,rf_test,type = "response")
      ensemble_voting = cbind(ensemble_voting,p)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("label_",i,sep="")


      p <- as.data.frame(predict(rf,rf_test,type = "prob"))
      ensemble_voting = cbind(ensemble_voting,p$W)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("prob_of_label_",i,sep="")
      ##end ensembling
    }

    ##now the ensembline
    ensemble_voting$predicted_label = "L"
    ensemble_voting$prob_of_label = 0

    for(i in 1:nrow(current_test))
    {
      count = 0

      for(j in seq(2,runs*2+1,2))
      {
        if(ensemble_voting[i,j] == "W")
        {
          count = count + 1
        }
      }
      if (count > runs %/% 2)
      {
        ensemble_voting$predicted_label[i] = "W"
      }

      sum = 0
      for(j in seq(3,runs*2+1,2))
      {
        sum = sum + ensemble_voting[i,j]
      }
      ensemble_voting$prob_of_label[i] = sum / runs
    }
    #########################################
    ######## averages over 5 trials end    ##
    #########################################


    pred_temp = data.frame(memo                =  current_test_labels[,7],
                           label               =  current_test_labels[,1],
                           season              =  current_test_labels[,2],
                           part_of_season      =  current_test_labels[,3],
                           date                =  current_test_labels[,4],
                           home_team           =  current_test_labels[,5],
                           away_team           =  current_test_labels[,6],
                           predicted_L         =  ensemble_voting$predicted_label,
                           probability_L       =  ensemble_voting$prob_of_label,
                           parameter_opt       =  rep(best_parameters,nrow(current_test_labels)),
                           cv_accuracy         =  best_cv_accuracy,
                           feature_set         =  rep(feature_set[fs_iter],nrow(current_test_labels)),
                           number_of_features  =  ncol(temp) - 9,
                           algorithm           =  algorithm_choices[alg_iter])

    print(paste("CV_acc: ", round(unique(pred_temp$cv_accuracy),3),
                ". Test_acc: ",round(accuracy_function(pred_temp$label,pred_temp$predicted_L),3),
                ". Difference = ", round(accuracy_function(pred_temp$label,pred_temp$predicted_L)
                                         - unique(pred_temp$cv_accuracy),3)))

    the_results =  rbind(the_results, pred_temp)
  }


  ##########################################
  ##########################################
  ##                                      ##
  ##  RRRRRR        AAAA       FFFFFFFF   ##
  ##  RR   RR      AA  AA      FFFFFFFF   ##
  ##  RR  RR       AA  AA      FF         ##          END
  ##  RRRR        AA    AA     FFFFF      ##
  ##  RR  RR      AAAAAAAA     FFFFF      ##
  ##  RR   RR    AA      AA    FF         ##
  ##  RR    RR   AA      AA    FF         ##
  ##                                      ##
  ##########################################
  ##########################################




  ##############################################
  ##############################################
  ##                                          ##
  ##    SSS   VV          VV  MM       MM     ##
  ##   S   S   VV        VV   MMM     MMM     ##
  ##  SS        VV      VV    MM MM MM MM     ##          START
  ##   SSSS      VV    VV     MM  MMM  MM     ##
  ##      SS      VV  VV      MM   M   MM     ##
  ##  S    S       VVVV       MM       MM     ##
  ##   SSSS         VV        MM       MM     ##
  ##                                          ##
  ##############################################
  ##############################################


  if(alg_iter == 7)
  {
    svm_functions = c("rbfdot","polydot","vanilladot")

    print("             Start SVM")

    # set.seed(666)
    # folds <- createFolds(current_train_labels[,1], k=5, list = TRUE, returnTrain = FALSE)

    Alg_for_opt <- function(the_cost, the_kernel)
    {
      cv_results <- lapply(folds, function(zeta)
      {
        ## creating training and validation!!
        train = current_train[-zeta,]
        validation = current_train[zeta,]
        train_labels = current_train_labels[-zeta,]
        validation_labels = current_train_labels[zeta,]


        svm_train = cbind(train_labels[,1],train)
        names(svm_train)[1] = "W.L"
        svm_test = cbind(validation_labels[,1],validation)
        names(svm_test)[1] = "W.L"

        ##performing svm
        predictors = paste(names(svm_train)[2:length(svm_train)],collapse = " + ")
        form = as.formula(paste("W.L ~ ",predictors,collapse = " + "))

        m <- ksvm(form, data = svm_train, kernel = svm_functions[the_kernel], C = the_cost,prob.model=TRUE)
        p <- predict(m,svm_test,type = "response")
        acc = accuracy_function(svm_test[,1],p)

        return (acc)
      })
      list(Score = mean(unlist(cv_results)), Pred = 0)
    }


    ##Bayesian optimisation!!
    OPT_Res <- BayesianOptimization(Alg_for_opt,
                                    bounds = list(the_cost = c(0,10),
                                                  the_kernel = c(1L,3L)),
                                    init_points = 10, n_iter = 5,
                                    acq = "ucb", kappa = 2.576, eps = 0.0,
                                    verbose = TRUE)
    best_cv_accuracy = OPT_Res$Best_Value
    ##### ensemblin
    best_parameters = paste("cost-",OPT_Res$Best_Par[[1]]," kernel-",svm_functions[OPT_Res$Best_Par[[2]]],sep="")

    #########################################
    ######## averages over 5 trials start  ##
    #########################################
    runs = 5
    ensemble_voting = data.frame(date_performes=rep(Sys.Date(),nrow(current_test)))
    for (i in 1:runs)
    {
      svm_train = cbind(current_train_labels[,1],current_train)
      names(svm_train)[1] = "W.L"
      svm_test = cbind(current_test_labels[,1],current_test)
      names(svm_test)[1] = "W.L"

      predictors = paste(names(svm_train)[2:length(svm_train)],collapse = " + ")
      form = as.formula(paste("W.L ~",predictors,collapse = " + "))

      m <- ksvm(form, data = svm_train, kernel = svm_functions[OPT_Res$Best_Par[[2]]], C = OPT_Res$Best_Par[[1]],prob.model=TRUE)
      p <- predict(m,svm_test,type = "response")


      ensemble_voting = cbind(ensemble_voting,p)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("label_",i,sep="")

      p <- predict(m,svm_test,type = "prob")
      p_prob =as.data.frame(p)
      ensemble_voting = cbind(ensemble_voting,p_prob$W)
      names(ensemble_voting)[ncol(ensemble_voting)]= paste("prob_of_label_",i,sep="")
      ##end ensembling
    }

    ##now the ensembline
    ensemble_voting$predicted_label = "L"
    ensemble_voting$prob_of_label = 0

    for(i in 1:nrow(current_test))
    {
      count = 0

      for(j in seq(2,runs*2+1,2))
      {
        if(ensemble_voting[i,j] == "W")
        {
          count = count + 1
        }
      }
      if (count > runs %/% 2)
      {
        ensemble_voting$predicted_label[i] = "W"
      }

      sum = 0
      for(j in seq(3,runs*2+1,2))
      {
        sum = sum + ensemble_voting[i,j]
      }
      ensemble_voting$prob_of_label[i] = sum / runs
    }
    #########################################
    ######## averages over 5 trials end    ##
    #########################################


    pred_temp = data.frame(memo                =  current_test_labels[,7],
                           label               =  current_test_labels[,1],
                           season              =  current_test_labels[,2],
                           part_of_season      =  current_test_labels[,3],
                           date                =  current_test_labels[,4],
                           home_team           =  current_test_labels[,5],
                           away_team           =  current_test_labels[,6],
                           predicted_L         =  ensemble_voting$predicted_label,
                           probability_L       =  ensemble_voting$prob_of_label,
                           parameter_opt       =  rep(best_parameters,nrow(current_test_labels)),
                           cv_accuracy         =  best_cv_accuracy,
                           feature_set         =  rep(feature_set[fs_iter],nrow(current_test_labels)),
                           number_of_features  =  ncol(temp) - 9,
                           algorithm           =  algorithm_choices[alg_iter])

    print(paste("CV_acc: ", round(unique(pred_temp$cv_accuracy),3),
                ". Test_acc: ",round(accuracy_function(pred_temp$label,pred_temp$predicted_L),3),
                ". Difference = ", round(accuracy_function(pred_temp$label,pred_temp$predicted_L)
                                         - unique(pred_temp$cv_accuracy),3)))

    the_results =  rbind(the_results, pred_temp)
  }



  ##############################################
  ##############################################
  ##                                          ##
  ##    SSS   VV          VV  MM       MM     ##
  ##   S   S   VV        VV   MMM     MMM     ##
  ##  SS        VV      VV    MM MM MM MM     ##          END
  ##   SSSS      VV    VV     MM  MMM  MM     ##
  ##      SS      VV  VV      MM   M   MM     ##
  ##  S    S       VVVV       MM       MM     ##
  ##   SSSS         VV        MM       MM     ##
  ##                                          ##
  ##############################################
  ##############################################


    }
  }
}
  return(the_results)
}







print(feature_set)




## algorithm_choices
## 1 - ann
## 2 - bag
## 3 - knn
## 4 - log
## 5 - nab
## 6 - raf
## 7 - svm

  x_ann_knn =Results(c(2000:2016),c(1),c(1,3))
  x_Bag =Results(c(2000:2016),c(1),c(2))
  x_Log =Results(c(2000:2016),c(1),c(4))
  x_nab =Results(c(2000:2016),c(1),c(5))

  x_raf =Results(c(2000:2016),c(1),c(6))
  x_svm =Results(c(2000:2016),c(1),c(7))
  
  ## get all results   very time consuming.. very very very time consuming
  xxx = Results(c(2000:2016),c(1:4),c(1:7))
