#' An import function that will
#' Reformat data into a usable form
#' from Mplus .out files. This one
#' is used to extract all the parameters
#' from the .out file when the model is a 2-class bifactor
#' model with 8 indicators. There is one general factor
#' and two specific factors.
#' FileName: the file name of the first simulation

#' This has been used for multiple simulation scenarios that have
#'5 different sample sizes (500,1000,2000,3500,5000) and 500 nrep
#' per sample size. The naming convention for the different nrep
#' is to change the number at the end. For example:
#'prac_1.out
#'prac_2.out
#'prac_3.out
#'....
#'prac_500.out
#' This made it easier to create a loop to change the
#'nrep number at the end of the file name.
#'
#' c1: used to combine path and file name
#' @nreps: the number simulations run for the scenario condition. 
#' @Filename: name of the Mplus .out file. The Filename pattern for this script is prac_#.out where # is the nrep number.
#' @path: directory path that the files are saved and the output is saved
#' @return is a usable format of the parameter loadings
#'
#'@export
#'
#'@importFrom utils write.csv
#'
#'




Import1<-function(nreps,path,FileName){
  library(MplusAutomation)
  library(rio)

  c1<-paste0(path,FileName)

  c2<-readModels(
    c1, recursive=TRUE)

  P2<-data.frame(c2$parameters)


  #build variable names

  #create three columns with the paramHeaders
  P2header<-data.frame(c2$parameters$unstandardized$paramHeader,
                       c2$parameters$unstandardized$paramHeader,
                       c2$parameters$unstandardized$paramHeader)


  #create three columns with the Parameter Names
  P2Names<-data.frame(c2$parameters$unstandardized$param,
                      c2$parameters$unstandardized$param,
                      c2$parameters$unstandardized$param)

  P2LC<-data.frame(c2$parameters$unstandardized$LatentClass,
                   c2$parameters$unstandardized$LatentClass,
                   c2$parameters$unstandardized$LatentClass)


  # create one long column with the parameter header
  P2headerCol<-data.frame(VarHeader=unlist(P2header))
  P2NamesCol<-data.frame(VarNam=unlist(P2Names))
  P2LatClass<-data.frame(VarLC=unlist(P2LC))

  # make into one dataset
  P2a<-cbind(P2headerCol,P2NamesCol,P2LatClass)


  #  add one long column for each estimate
  #P2header$est<-rep(c("_est"),times=5)
  P2a$estimNames<-rep(c("_est","_se","pvalu"),each=83)


  #combine the two columns
  P2a$VarName<-paste(P2a$VarHeader,P2a$VarNam,P2a$VarLC,P2a$estimNames)


  #  make data set with only est, se, pvalue, and latent class
  P2b<-data.frame(P2$unstandardized.est,P2$unstandardized.se,P2$unstandardized.pval)


  # make P2b into one column
  P2c<-data.frame(estimates=unlist(P2b))


  ## combine variable names and estimate values
  P2d<-data.frame(P2a$VarName,P2c$estimates)


  # transposing first column to variable names and second column to row one
  P2e<-setNames(data.frame(t(P2d[,-1])), P2d[,1])

  ### Create nrep variable and make first one 1.
  ## keep track of nrep number to go back
  ## and do a quality control check
  ## on random nrep in the data set
  P2e$nrep<-1

  ##create new data set to find the mean of the loadings
  ## P2e$Conv is used to count the number of
  ## nrep that converge
  P2e$Conv<-c("Yes")


  ##############################################################################

  ###   End of first file read in ############
  b<-c(1:500)


  for (i in 2:nreps){

    ##   2 class model ####
    c1<-paste0(path,"/prac_",b[i],".out")


    c2<-readModels(
      c1 , recursive=TRUE)


    P2<-data.frame(c2$parameters)


    #build variable names

    #create four columns with the paramHeaders
    P2header<-data.frame(c2$parameters$unstandardized$paramHeader,
                         c2$parameters$unstandardized$paramHeader,
                         c2$parameters$unstandardized$paramHeader)


    #create four columns with the Parameter Names
    P2Names<-data.frame(c2$parameters$unstandardized$param,
                        c2$parameters$unstandardized$param,
                        c2$parameters$unstandardized$param)

    P2LC<-data.frame(c2$parameters$unstandardized$LatentClass,
                     c2$parameters$unstandardized$LatentClass,
                     c2$parameters$unstandardized$LatentClass)


    # create one long column with the parameter header
    P2headerCol<-data.frame(VarHeader=unlist(P2header))
    P2NamesCol<-data.frame(VarNam=unlist(P2Names))
    P2LatClass<-data.frame(VarLC=unlist(P2LC))



    # make into one data set
    P2a<-cbind(P2headerCol,P2NamesCol,P2LatClass)


    #  add one long column for each estimate
    #P2header$est<-rep(c("_est"),times=5)
    P2a$estimNames<-rep(c("_est","_se","pvalu"),each=83)

    #combine the two columns
    P2a$VarName<-paste(P2a$VarHeader,P2a$VarNam,P2a$VarLC ,P2a$estimNames)

    nc<-ncol(P2)

    if(nc==4){


      #  make data set with only est, se, pvalue, and latent class
      P2b1<-data.frame(P2$unstandardized.est)

      P2b1$understandardized.se<-rep(c("NA"),times=83)
      P2b1$understandardized.pval<-rep(c("NA"),times=83)

      P2b<-data.frame(P2b1$P2.unstandardized.est,
                      P2b1$understandardized.se,
                      P2b1$understandardized.pval)


      # make P2b into one column
      P2c<-data.frame(estimates=unlist(P2b))


      ## combine variable names and estimate values
      P2d<-data.frame(P2a$VarName,P2c$estimates)


      # transposing first column to variable names and second column to row one
      P2e_L<-setNames(data.frame(t(P2d[,-1])), P2d[,1])

      P2e_L$nrep<-b[i]

      ## if ncol==4, the model did not converge
      ## P2e_L$Conv counts how many of the nreps
      ## did not converge
      P2e_L$Conv<-c("No")

    }#end of nc==4

    else if (nc==7){

      #  make data set with only est, se, pvalue, and latent class
      P2b<-data.frame(P2$unstandardized.est,
                      P2$unstandardized.se,
                      P2$unstandardized.pval)


      # make P2b into one column
      P2c<-data.frame(estimates=unlist(P2b))


      ## combine variable names and estimate values
      P2d<-data.frame(P2a$VarName,P2c$estimates)


      # transposing first column to variable names and second column to row one
      P2e_L<-setNames(data.frame(t(P2d[,-1])), P2d[,1])

      P2e_L$nrep<-b[i]

      ## P2e_L$Conv counts
      ## how many converge
      P2e_L$Conv<-c("Yes")

    }#end of n==7

    P2e<-rbind(P2e,P2e_L)

  }#end of i loop
  Save1<-paste0(path,"Parameter Estimates.csv")
rio::export(P2e,Save1)

  return(P2e)
  save(P2e,file="P2e.RData")

} ## end of function
