###############
###############v is odd (Series 1)
#' Structurally Complete Generalized Row Column Designs of Series-I
#'
#' @param v Odd number(>3)
#'
#' @return This function generates structurally complete GRC designs for odd number of treatment as well as the information matrix for estimating elementary treatment contrast.
#' @export
#'@description
#' This series generated through initial columns. The resulting GRC design is a row-column design with two units per cell and with p = t (>1) rows of size 2(2t+1), q = (2t+1) columns of size 2t, k = 2 and r = 2t replications.
#'@references 1) Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2014). Structurally incomplete row-column designs with multiple units per cell. Statistics and Applications, 12(1&2), 71-79.
#'
#'2)Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2015). Some series of row-column designs with multiple units per cell. Calcutta Statistical Association Bulletin, 67, ( 265-266), 89-99.
#'
#'3)Datta, A., Jaggi, S., Varghese, C. and Varghese, E.  (2016). Series of incomplete row-column designs with two units per cell. Advances in Methodology and Statistics. 13(1), 17-25.

#' @examples
#' library(GRCdesigns)
#' SCGRC_I(5)
SCGRC_I<-function(v){
  if(v%%2==0 || v<4){
    return(message("Please enter any odd number (>3)"))
  }
  tupple.matrix<-function(matrix,n){
    tuple<-function(vec){
      z<-(c("(",vec,")"))
      return(noquote(z))
    }
    final=NULL
    matrix=as.matrix(matrix)
    allinone=c(t(matrix))
    for(i in 1:(length(allinone)/n)){
      final=c(final,tuple(c(allinone[((n*i)-(n-1)):(n*i)])))
    }
    final=matrix(final,nrow=nrow(matrix),byrow=T)
    final=noquote(final)
    prmatrix(final,rowlab=,collab=rep("",ncol(final)), quote = FALSE)
  }
  t=(v-1)/2
  col1=seq(1:t)
  col2=seq(((2*t)+1),((2*t)-(t-2)))
  main=cbind(col1,col2)
  y=main
  for(i in 1:(v-1)){
    main=cbind(main,((main[,1:2]+i)))
  }
  main=main%%v
  main[main==0]<-v
  colnames(main)=NULL
  #################
  message("Structually Complete Row-Column Design With Multiple Units Per Cell When v is an Odd Number")
  tupple.matrix(main,2)
  cat("\n")
  list=list("Number of Treatments"=v, "Number of Rows" = nrow(main), "Number of Columns" = ncol(main)/2, "Number of Replication" = length(which(main==max(main))), "Number of Units per Cell"=2)
  print(list)
  cat("\n")
  ######C matrix
  message("C matrix")
  c=(t+0.5)*diag(1,nrow=v,ncol=v)-0.5*matrix(1,nrow=v,ncol=v)
  print(round(c,digits=4))
}
###########################Check
#series1(3)
#############################################################
#########v is even (Series 2)
#' Structurally Complete Generalized Row Column Designs of Series-II
#'
#' @param v Even number(>3)
#'
#' @return This function generates structurally complete GRC designs for even number of treatment as well as the information matrix for estimating elementary treatment contrast.
#' @export
#'@description
#' This series generated through initial columns. The parameters of the design are v, p = (v-1) rows of size v, q =v/2 columns of size 2(v-1), k = 2 and r = (v-1).
#' @examples
#' library(GRCdesigns)
#' SCGRC_II(6)
#'@references 1) Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2014). Structurally incomplete row-column designs with multiple units per cell. Statistics and Applications, 12(1&2), 71-79.
#'
#'2)Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2015). Some series of row-column designs with multiple units per cell. Calcutta Statistical Association Bulletin, 67, ( 265-266), 89-99.
#'
#'3)Datta, A., Jaggi, S., Varghese, C. and Varghese, E.  (2016). Series of incomplete row-column designs with two units per cell. Advances in Methodology and Statistics. 13(1), 17-25.
SCGRC_II<-function(v){
  if(v%%2!=0 || v<4){
    return(message("Please enter any even number (>3)"))
  }
  tupple.matrix<-function(matrix,n){
    tuple<-function(vec){
      z<-(c("(",vec,")"))
      return(noquote(z))
    }
    final=NULL
    matrix=as.matrix(matrix)
    allinone=c(t(matrix))
    for(i in 1:(length(allinone)/n)){
      final=c(final,tuple(c(allinone[((n*i)-(n-1)):(n*i)])))
    }
    final=matrix(final,nrow=nrow(matrix),byrow=T)
    final=noquote(final)
    prmatrix(final,rowlab=,collab=rep("",ncol(final)), quote = FALSE)
  }
  col1=seq(1:(v/2))
  col2=seq(v,(v-((v/2)-2)))
  col2=c(col2,0)
  newcol=c()
  for(i in 1:length(col1)){
    x=rbind(col1[i],col2[i])
    newcol=c(newcol,x)
  }
  newcol=newcol[-length(newcol)]
  rem=setdiff(1:v,c(col1,col2))
  newcol2=c(newcol[-1],rem)
  main=cbind(newcol,newcol2)
  y=main
  for(i in 1:((v/2)-1)){
    main=cbind(main,((main[,1:2]+i)))
  }
  main=main%%v
  main[main==0]<-v
  colnames(main)=NULL
  message("Structually Complete Row-Column Design With Multiple Units Per Cell When v is an Even Number")
  tupple.matrix(main,2)
  cat("\n")
  list=list("Number of Treatments"=v, "Number of Rows" = nrow(main), "Number of Columns" = ncol(main)/2, "Number of Replication" = length(which(main==max(main))), "Number of Units per Cell"=2)
  print(list)
  ######C matrix
  message("C matrix")
  c=(v/2)*diag(1,nrow=v,ncol=v)-0.5*matrix(1,nrow=v,ncol=v)
  print(round(c,digits=4))
}
###############################Check
#series2(8)
##################################################################
####Series 3 (v prime)
#' Structurally Complete Generalized Row Column Designs of Series-III
#'
#' @param v Prime number(>3)
#'@param k Number of units per cell
#' @return This function generates structurally complete GRC designs for prime number of treatment as well as the information matrix for estimating elementary treatment contrast.
#' @export
#'@description
#' The resulting design is a GRC designs with v (prime number) treatments in p = 2 rows, q = v(v-1) /2 columns and each cell of size k (2 <= k <= v-1).
#' @examples
#' library(GRCdesigns)
#' SCGRC_III(7,2)
#'@references 1) Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2014). Structurally incomplete row-column designs with multiple units per cell. Statistics and Applications, 12(1&2), 71-79.
#'
#'2)Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2015). Some series of row-column designs with multiple units per cell. Calcutta Statistical Association Bulletin, 67, ( 265-266), 89-99.
#'
#'3)Datta, A., Jaggi, S., Varghese, C. and Varghese, E.  (2016). Series of incomplete row-column designs with two units per cell. Advances in Methodology and Statistics. 13(1), 17-25.

SCGRC_III<-function(v,k){
  tupple.matrix<-function(matrix,n){
    tuple<-function(vec){
      z<-(c("(",vec,")"))
      return(noquote(z))
    }
    final=NULL
    matrix=as.matrix(matrix)
    allinone=c(t(matrix))
    for(i in 1:(length(allinone)/n)){
      final=c(final,tuple(c(allinone[((n*i)-(n-1)):(n*i)])))
    }
    final=matrix(final,nrow=nrow(matrix),byrow=T)
    final=noquote(final)
    prmatrix(final,rowlab=,collab=rep("",ncol(final)), quote = FALSE)
  }
  is.prime=function(v){
    if(v==2){
      return(TRUE)
    }
    if(v>2){
      a=c(2:as.integer(v/2))
      b=rep(v,time=length(a))
      c=c(b%%a)
      d=length(c[c==0])

      if(d==0){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }
  }
  ##############
  if(is.prime(v)==FALSE || v<4){
    return(message("Please enter any prime number (>3)"))
  }
  #####################
  vec=seq(1:((v-1)/2))
  ##generation of MOLS

  #################

  vec=seq(1:((v-1)/2))
  array=NULL
  for(i in vec){
    array1=NULL
    ini=matrix(1:v,nrow=v,ncol=v,byrow=T)
    newvec=c(0,seq(i,(i*(v-1)),by=i))
    array1=cbind(array1,(ini+newvec))
    array=cbind(array,array1)
  }
  array=array%%v
  array[array==0]=v
  #############
  finalarray=NULL
  for(i in 1:2){
    store<-NULL
    for(j in 1:ncol(array)){
      store<-cbind(store,t(array[i:(k+i-1),j]))
    }
    finalarray<-rbind(finalarray,store)
  }

  ########################
  ######################
  message("Structually Complete Row-Column Design With Multiple Units Per Cell When v is a Prime Number")
  tupple.matrix(finalarray,k)
  cat("\n")
  list=list("Number of Treatments"=v, "Number of Rows" = 2, "Number of Columns" = ncol(array), "Number of Replication" = length(which(finalarray==max(finalarray))), "Number of Units per Cell"=k)
  print(list)
  cat("\n")
  ######C matrix
  message("C matrix")
  c=(k-1)*v*diag(1,nrow=v,ncol=v)-(k-1)*matrix(1,nrow=v,ncol=v)
  print(round(c,digits=4))
}
####################################Incomplete series
#' Structurally Incomplete Generalized Row Column Designs of Series-I
#'
#' @param v Odd number(>3)
#'
#' @return This function generates structurally incomplete GRC designs for odd number of treatment with differential replication as well as the information matrix for estimating elementary treatment contrast.
#' @export
#'@description
#' The parameter of the design are v (odd), p = (v-1) rows of size 2(v-1) each, q = v columns [one column of size 2(v-1) and remaining of size 2(v- 2) each], k = 2, r_1 (replication of first v-1 treatments) = 2v-3 and r_2 (replication of the v th treatment) = v-1.
#' @examples
#' library(GRCdesigns)
#' SIGRC_I(5)
#'@references 1) Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2014). Structurally incomplete row-column designs with multiple units per cell. Statistics and Applications, 12(1&2), 71-79.
#'
#'2)Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2015). Some series of row-column designs with multiple units per cell. Calcutta Statistical Association Bulletin, 67, ( 265-266), 89-99.
#'
#'3)Datta, A., Jaggi, S., Varghese, C. and Varghese, E.  (2016). Series of incomplete row-column designs with two units per cell. Advances in Methodology and Statistics. 13(1), 17-25.
SIGRC_I<-function(v){
  tupple.matrix<-function(matrix,n){
    tuple<-function(vec){
      z<-(c("(",vec,")"))
      return(noquote(z))
    }
    final=NULL
    matrix=as.matrix(matrix)
    allinone=c(t(matrix))
    for(i in 1:(length(allinone)/n)){
      final=c(final,tuple(c(allinone[((n*i)-(n-1)):(n*i)])))
    }
    final=matrix(final,nrow=nrow(matrix),byrow=T)
    final=noquote(final)
    prmatrix(final,rowlab=,collab=rep("",ncol(final)), quote = FALSE)
  }
  if(v%%2==0 || v<4){
    return(message("Please enter any odd number (>3)"))
  }
  final_column<-NULL
  for(i in 1:v){
    two_columns<-NULL
    a<-c(i,(i+1))
    seq1<-seq(a[1],a[1]+(v-2),1)%%v
    seq1[seq1==0]<-v
    seq2<-seq(a[2],(a[2]+2*(v-2)),2)%%v
    seq2[seq2==0]<-v
    two_columns<-cbind(seq1,seq2)
    final_column<-cbind(final_column,two_columns)
  }
  #####################################
  blank_of_col<-matrix(ncol(final_column):3,ncol=2,byrow=T)
  ################
  final_column
  for(i in 1:nrow(blank_of_col)){
    final_column[i,c(blank_of_col[i,])]<-"-"
  }
  #######################


  message("Structually Incomplete Row-Column Design With Multiple Units Per Cell When v is Odd")
  tupple.matrix(final_column,2)
  ##########C matrix
  C=rbind(cbind((v-0.5)*diag(1,v,v)-matrix(1,v,v), -0.5*matrix(1,v,1)),cbind(-0.5*matrix(1,1,v),((v-1)/2)))
  #prmatrix(final_column,rowlab=,collab=rep("",ncol(final_column)*4), quote = FALSE)
  cat("\n")
  list=list("Number of Treatments"=v, "Number of Rows" = (v-1) ,"Number of Columns" = v,"Cell size"=2, "Replication-1 (r1)"=(2*v-3),"Replication-2 (r2)"=v-1)
  print(list)
  cat("\n")
  message("C matrix")
  print(round(C,digits=4))
}
################Check
#incomplete_series1(3)
########################################incomplete series 2

########################### v=s^2
#' Structurally Incomplete Generalized Row Column Designs of Series-II
#'
#' @param v = s^2 where s is a prime number
#'
#' @return This function generates structurally incomplete GRC designs from resolvable (Balanced Incomplete Block) BIB designs as well as the information matrix for estimating elementary treatment contrast.
#' @export
#'@description
#' This series generates using resolvable balanced incomplete block designs for a given v. The blocks are arranged in the row-column set up such that there should not be more than one blank cell in each row and column.
#' @examples
#' library(GRCdesigns)
#' SIGRC_II(4)
#'@references 1) Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2014). Structurally incomplete row-column designs with multiple units per cell. Statistics and Applications, 12(1&2), 71-79.
#'
#'2)Datta, A., Jaggi, S., Varghese, C. and Varghese, E. (2015). Some series of row-column designs with multiple units per cell. Calcutta Statistical Association Bulletin, 67, ( 265-266), 89-99.
#'
#'3)Datta, A., Jaggi, S., Varghese, C. and Varghese, E.  (2016). Series of incomplete row-column designs with two units per cell. Advances in Methodology and Statistics. 13(1), 17-25.

SIGRC_II<-function(v){
  tupple.matrix<-function(matrix,n){
    tuple<-function(vec){
      z<-(c("(",vec,")"))
      return(noquote(z))
    }
    final=NULL
    matrix=as.matrix(matrix)
    allinone=c(t(matrix))
    for(i in 1:(length(allinone)/n)){
      final=c(final,tuple(c(allinone[((n*i)-(n-1)):(n*i)])))
    }
    final=matrix(final,nrow=nrow(matrix),byrow=T)
    final=noquote(final)
    prmatrix(final,rowlab=,collab=rep("",ncol(final)), quote = FALSE)
  }
  is.prime<-function(v){
    if(v<=2){
      return(TRUE)
    }
    if(v>2){
      for(i in 2:as.integer(v/2)){
        if(v%%i==0){
          return(FALSE)
        }
      }
      if(i==as.integer(v/2)){
        return(TRUE)
      }
    }
  }
  #is.prime(11)
  ###########
  factor<-function(v){
    if(is.prime(v)==T){
      return(1)
    }
  for(i in 2:(v/2)){
    if(v%%i==0){
      return(i)
    }
  }
  }

  proceed=factor(v)-sqrt(v)
  ############
  if(proceed!=0 || v<4){
    return(message("Please enter v = s^2 where, s(>=3) is a prime number"))
  }
  #############generate mols
  list_of_MOLS<-list()
  for(i in 1:(sqrt(v)-1)){
    store_mat<-c(1:sqrt(v))
    a<-c(1:sqrt(v))
    for(k in 1:(sqrt(v)-1)){
      a<-a+i
      store_mat<-rbind(store_mat,a)
    }
    store_mat<-store_mat%%sqrt(v)
    store_mat[store_mat==0]<-sqrt(v)
    #############
    list_of_MOLS<-append(list_of_MOLS,list(t(store_mat)))
  }

  #################################################
  positions_in_MOLS<-list()
  for(i in 1:length(list_of_MOLS)){
    store_position<-NULL
    for(k in 1:sqrt(v)){
      store_position<-rbind(store_position,which(list_of_MOLS[[i]]==k))
    }
    positions_in_MOLS<-append(positions_in_MOLS,list(store_position))
  }
  ################final matrix=rowwise+columnwise+MOLS
  ##############show replications by partitions
  positions_in_MOLS<-lapply(positions_in_MOLS,function(mat) rbind(mat,NA))
  #new_positions_in_MOLS=do.call(rbind,positions_in_MOLS)
  new_list<-append(list(rbind(t(matrix(1:v,sqrt(v),sqrt(v))),NA),rbind((matrix(1:v,sqrt(v),sqrt(v))),NA)),positions_in_MOLS)
  #################now construct design
  #############design
  ###############
  ###matrix rotation
  rotation_mat<-function(matrix){
    a=as.matrix(matrix)
    ini=c(1:ncol(a))
    final=NULL
    for(i in 1:(ncol(a)-1)){
      x=ini-i
      final=rbind(final,x)
    }
    final=rbind(ini,final)
    final=final%%ncol(a)
    final[final==0]<-ncol(a)
    fmat=NULL
    for(j in 1:ncol(a)){
      output=NULL
      for(k in 1:ncol(a)){
        output=cbind(output,(a[,final[j,k]]))
      }
      fmat=rbind(fmat,output)
    }
    return(fmat)
  }
  ##################################
  blank_position_matrix<-rotation_mat(matrix(1:(sqrt(v)+1),nrow=1,ncol=sqrt(v)+1,byrow=T))
  ##############
  design<-list()
  for(i in 1:length(new_list)){
    design<-append(design,list(new_list[[i]][c(blank_position_matrix[i,]),]))
  }
  ###################
  final_design<-do.call(rbind,design)
  resultant_design<-matrix(c(t(final_design)),nrow=(sqrt(v)+1),byrow=T)
  resultant_design[is.na(resultant_design)]<-"-"
  ##########################
  message("Structually Incomplete Row-Column Design With Multiple Units Per Cell When v = s^2 and s is a Prime Number")
  tupple.matrix(resultant_design,sqrt(v))
  C=sqrt(v)*diag(1,v,v)-(1/(sqrt(v)))*matrix(1,v,v)
  cat("\n")
  list=list("Number of Treatments"=v, "Number of Rows" = (sqrt(v)+1),"Number of Columns" = sqrt(v)+1,"Cell size"=sqrt(v))
  print(list)
  cat("\n")
  message("C matrix")
  print(round(C,digits=4))
}

