

library(stats)




maddy<-function(x) { out<- mad(x, center = median(x), constant = 1.4826,na.rm = FALSE, low = FALSE, high = FALSE)    }

cccv<-function(x) { out<- cv(x)    }

s<-function(x) { out<- sd(x)    }
m<-function(x) { out <- mean(x) }


pf <- function(x,y){
  test <- t.test(x,y, paired=TRUE)
  
  out <- data.frame(
    pval = as.numeric(test$p.value)                                  
  )
}


fold <- function(x, y) {
  
  fc<-foldchange(median(x,na.rm = TRUE),median(y,na.rm = TRUE))
  
  out <- data.frame(
    fc1 <-log2((abs(fc))^(sign(fc)) )
    #fc1 = round(as.numeric(fc),digits=4)                                  
    
  )
  
  
  
  
}
pw <- function(x,y){
  
  test <- wilcox.test(x,y, paired=TRUE)
  out <- data.frame(
    pval = as.numeric(test$p.value)                                  
    
  )
  
  return(out)
}





pairedtt<-
  function (myMatrix,tp,tpv1, tpv2 ,row_list,col_list,f3,f4){
     sub_myMatrix<-myMatrix %>% dplyr::select(c("Subject","TimePoint",col_list))
 
     sub_myMatrix<-sub_myMatrix [rowSums(is.na(sub_myMatrix )) != ncol(sub_myMatrix ),] 

  pt2 <- sub_myMatrix %>% filter(sub_myMatrix[,tp] ==tpv1 ) %>% arrange(Subject) %>% collect()
  pt1 <- sub_myMatrix %>% filter(sub_myMatrix[,tp] ==tpv2 ) %>% arrange(Subject) %>% collect()

  rownames(pt2) <-
    make.names(paste(pt2$Subject, sep = "_"), unique = TRUE)
  
  
  rownames(pt1) <-
    make.names(paste(pt1$Subject, sep = "_"), unique = TRUE)
  
  
  
  list_of_data <- list(pt1,pt2)
  common_names <- Reduce(intersect, lapply(list_of_data, row.names))
  cn<<-common_names
  list_of_data <- lapply(list_of_data, function(x) { x[row.names(x) %in% common_names,] })
  
  xa <-list_of_data[[1]]
  xb <-list_of_data[[2]]
 xxxaaa<<-xa
 ccclll<<-col_list
     #select_xa <-xa[,grepl(paste0("^",col_list,"$",  collapse='|'), colnames(xa), ignore.case=TRUE),]
 select_xa <- xa %>% select(any_of(col_list))     
 #select_xb <-xb[,grepl(paste0("^",col_list,"$",  collapse='|'), colnames(xb), ignore.case=TRUE),]
 select_xb <-xb %>% select(any_of(col_list))  
   
  pttestmat1<-sapply(seq(ncol(select_xa) ), function(x) pf(select_xa[,x], select_xb[,x]) )   
  
  
  foldchange1<-sapply(seq(ncol(select_xa) ), function(x) fold(select_xa[,x], select_xb[,x]) ) 
  
  pwilcoxmat1<-sapply(seq(ncol(select_xa) ), function(x) pw(select_xa[,x], select_xb[,x]) )   
  
  
  
  tp1<-sapply(seq(ncol(select_xa)), function(x) paste(tpv2)  )
  #tp1<-sapply(seq(ncol(select_xa)), function(x) paste(tpv2 , "-" , tpv1, sep ="")  )
  tp1<-unlist(tp1)
  foldchange1<-unlist(foldchange1)
  pttestmat1<-unlist(pttestmat1)
  pwilcoxmat1<-unlist(pwilcoxmat1)
  
  pttestpadj<-p.adjust(sapply(pttestmat1, function(x) paste(unlist(x),collapse="")), method="fdr" )    
  
  pwilpadj<-p.adjust(sapply(pwilcoxmat1, function(x) paste(unlist(x),collapse="")), method="fdr" ) 
  
  cccc <-colnames(select_xb)
  somamers2<-unlist(colnames(select_xb))
  
  
  foo2<- list(df7=data_frame(tp1),df6=data_frame(foldchange1),df2=data_frame(pttestmat1),df4=data_frame(pwilcoxmat1),  df1 = data.frame(pttestpadj), df5 = data.frame(pwilpadj), ff3=data.frame(somamers2) )
  
  pttestmat<- do.call("cbind", foo2)
  colnames(pttestmat) <- c('TimePoint',   'Fold Change', 'paired ttest', 'Wilcoxin Rank Sum Test' , 'paired t-test (fdr)', 'Wilcoxin Rank Sum Test (fdr)', 'id')
  
  #fil_pttest <-pttestmat %>% filter(.[[as.numeric(input$tfilt3)]] < input$tfilt4)
  fil_pttest <-pttestmat
  
  #fil_pttest <-pttestmat %>% filter(.[[as.numeric(f3)]] < f4)
  
  return(list("pttestmat"=pttestmat, "select_xa"=select_xa, "select_xb"=select_xb, "fil_pttest"=fil_pttest)) 
#  return(list("fil_pttest"=fil_pttest))
  
}

  
  
  
 



yab<- function(data,data2,d3){
  
  myData <- data2
  
  a  <- myData
  a<-a[!apply(a == "", 1, all),]
  a<-na.omit(a)
  b<-a[,4:ncol(a)]
  
  # chi_samples <- aaa %>% filter(aaa[,1] != "CHI-010") %>% filter(aaa[,2] == "d0")  %>% collect()
  
  
  con_sample<-"CHI-010"
  
  chi_controls <- a %>% filter(a[,1] == con_sample)  %>% collect()
  chi_controls_data<-chi_controls[,4:ncol(a)]
  chi_controls_m_1 <- sapply(seq(ncol(chi_controls_data)), function(x)
    m(chi_controls_data[, x]) ) 
  chi_controls_data_1 <- sapply(seq(ncol(chi_controls_data)), function(x)
    maddy(chi_controls_data[, x]) ) 
  chi_controls_sd_1 <- sapply(seq(ncol(chi_controls_data)), function(x)
    s(chi_controls_data[, x]) ) 
  chi_controls_cv_1 <- sapply(seq(ncol(chi_controls_data)), function(x)
    cccv(chi_controls_data[, x]) ) 
  
  
  
  
  somamers <- unlist(colnames(b))
  
  if (is.null(input$showtp) || input$showtp  == 'FALSE'){
    
    foo <-
      list(
        df1 <- data_frame(somamers),
        df2 <- data_frame(chi_controls_data_1),
        df4 <- data_frame(chi_controls_sd_1),
        df6 <- data_frame(chi_controls_cv_1),
        df8 <- data_frame(chi_controls_m_1)
      )
    
    
    summat <- do.call("cbind", foo)
    
    colnames(summat) <-
      cbind(
        'Population',
        paste ('mad (',input$con,")" ,sep=""),
        paste ('sd(',input$con,")", sep=""),
        paste ('cv (',input$con,")", sep=""),
        paste ('mean (',input$con,")", sep="")
      )
    
    dff2 <- data.frame(summat[c(1,2,3,4,5)])
    rownames(dff2)<-(make.names(paste(dff2$Population,sep="_"),unique=TRUE))  
    
  }
  
  else{
    
    chi_samples <- a %>% filter(a[,2] == input$tp)  %>% collect()
    chi_samples_data<-chi_samples[,4:ncol(a)]
    chi_samples_m_1 <- sapply(seq(ncol(chi_samples_data)), function(x)
      m(chi_samples_data[, x]) )
    chi_samples_data_1 <- sapply(seq(ncol(chi_samples_data)), function(x)
      maddy(chi_samples_data[, x]) )
    chi_samples_sd_1 <- sapply(seq(ncol(chi_samples_data)), function(x)
      s(chi_samples_data[, x]) )
    chi_samples_cv_1 <- sapply(seq(ncol(chi_samples_data)), function(x)
      cccv(chi_samples_data[, x]) )
    
    
    
    
    foo <-
      list(
        df1 <- data_frame(somamers),
        df2 <- data_frame(chi_controls_data_1),
        df3 <- data_frame(chi_samples_data_1),
        
        df4 <- data_frame(chi_controls_sd_1),
        df5 <- data_frame(chi_samples_sd_1),
        df6 <- data_frame(chi_controls_cv_1),
        df7 <- data_frame(chi_samples_cv_1),
        df8 <- data_frame(chi_controls_m_1),
        df9 <- data_frame(chi_samples_m_1)
      )
    
    
    summat <- do.call("cbind", foo)
    
    
    
    
    colnames(summat) <-
      cbind(
        'Population',
        paste ('mad (',input$con,")" ,sep=""),
        paste ('mad (',input$tp,")" ,sep=""),
        
        paste ('sd(',input$con,")", sep=""),
        paste ('sd(',input$tp,")", sep=""),
        paste ('cv (',input$con,")", sep=""),
        paste ('cv (',input$tp,")", sep=""),
        paste ('mean (',input$con,")", sep=""),
        paste ('mean (',input$tp,")", sep="")
      )
    
    dff2 <<- data.frame(summat[c(1,2,4,6,8)])
    rownames(dff2)<-(make.names(paste(dff2$Population,sep="_"),unique=TRUE))
  }
  
  
  
  dff2
  
  #  return(summat)
  # return(list("summat"= summat, "dff2"=dff2))    
  
}
#)



















# if (input$controls == "CHI-002-H5N1-Treg"){
 # cells<- bcells
 
 
# }


dutab2<- function(x){
cells<-x
cells<-na.omit(cells)

cells_data<-cells[,3:ncol(cells)]

cells_m_1 <<- sapply(seq(ncol(cells_data)), function(x)
  m(cells_data[, x]) )

cells_mad_1 <- sapply(seq(ncol(cells_data)), function(x)
  maddy(cells_data[, x]) )

cells_sd_1 <- sapply(seq(ncol(cells_data)), function(x)
  s(cells_data[, x]) )

cells_cv_1 <- sapply(seq(ncol(cells_data)), function(x)
  cccv(cells_data[, x]) )

somamers <- unlist(colnames(cells_data))



foo2 <-
  list(
    dfc1 <- data_frame(somamers),
    dfc2 <- data_frame(cells_mad_1),
    dfc3 <- data_frame(cells_sd_1),
    
    dfc4 <- data_frame(cells_cv_1),
    dfc5 <- data_frame(cells_m_1)
  )


datab <- do.call("cbind", foo2)


}


