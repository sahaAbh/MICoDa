compute_x_D <- function(orig,exp_theta, D=NULL, known_ind=NA,method='direct', x_D_init=4, Constant=24, ZeroAsMissing=T){
  if(is.null(D)){D=length(orig)+1}
  if(!is.null(known_ind) & ZeroAsMissing){if(is.na(known_ind)) known_ind = which(!is.na(orig) & orig >0) }
  if(!is.null(known_ind) & !ZeroAsMissing){if(is.na(known_ind)) known_ind = which(!is.na(orig))}
  #print("check ")
exp_sm <- sum(exp_theta[setdiff(1:(D-1),known_ind)])  ; K = sum(as.numeric(orig[known_ind]),na.rm=T);

if(method=='direct'){
  if(K <= Constant){x_D <- (Constant- K)/(1+exp_sm);}else{x_D = 0}
}else{
  x_D <- (Constant*x_D_init)/(x_D_init+K+x_D_init*exp_sm)
}
return(x_D)
}



ALR_reg <- function(data_tmp,act_ind, ref_ind,varlist, out="predict")
{
  data_tmp$var <-data_tmp[,act_ind]
  # removing NA's in Y as well as in reference category
  data_tmp1 <-  data_tmp[(!is.na(data_tmp$var)) & (!is.na(data_tmp[,ref_ind])),]
  # removing 0's from Y as well as ref ind
  data_tmp1 <- data_tmp1[(data_tmp1$var > 0) & (data_tmp1[,ref_ind]  >0) ,]
  data_tmp1$var = log(data_tmp1$var/data_tmp1[,ref_ind])
  ALR_fit <-  lm(formula =as.formula(paste0( "var ~ ", paste0(varlist,collapse="+ "))) , data = data_tmp1)
  if(out=="predict"){r_pred = predict(ALR_fit, data_tmp);return(r_pred);}else{theta_train = summary(ALR_fit)$coef[,1];return(theta_train)}
}



row_partial <- function(data_init,r_est,NArows,D=NULL,act_index_start=10,rescaleAll=T,...){
  if(is.null(D)){D=ncol(r_est)+1}
    data_init0 <- data_init
      for(i in NArows)
      { if(rescaleAll){
        for(k in 1:(D-1)){
        orig<- data_init[i,act_index_start: (act_index_start+(D-1)-1)];orig[k]=NA
        x_D <- compute_x_D(orig, exp_theta = exp(r_est[i,]),...)
        data_init[i,(act_index_start+D-1)]= x_D;
        data_init[i,(act_index_start+k-1)]= x_D* exp(r_est[i,k]);
        }}else{
          orig<- data_init0[i,act_index_start: (act_index_start+(D-1)-1)];                  
          x_D <- compute_x_D(orig, exp_theta = exp(r_est[i,]),...)    
          data_init[i,(act_index_start+D-1)]= x_D;
          data_init[i,(act_index_start+which(is.na(orig))-1)]= x_D* exp(r_est[i,which(is.na(orig))]);
          }
      }
  return(data_init)            
}

MICoDa<- function(train, D, varlist, iter=15,rescaleAll=T,...)
{ 
  NA_ind_com<- sapply(1:nrow(train), function(x)sum(is.na(train[x,1:D])))
  ind <- which(NA_ind_com > 0)
  # Finding regression estimates based on complete data
  r_pred_0 <- sapply(1:(D-1), function(x){ALR_reg(train,act_ind=x, ref_ind=D, varlist=varlist)})
  
  diff <- rep(1,iter)
  data_init <- train
  r_est<- r_pred_0
  for(j in 1:iter){
    if(rescaleAll){
      data_init <- row_partial(data_init, r_est, NArows=ind, act_index_start=1,rescaleAll=rescaleAll,...)
    }else{data_init <- row_partial(train, r_est, NArows=ind, act_index_start=1,rescaleAll=rescaleAll,...)}
    r_est <- sapply(1:(D-1), function(x){ALR_reg(data_init,act_ind=x, ref_ind=D, varlist=varlist)}) 
  #  print(data_init[c(1,3,8),1:6])
    
    #Computinng relative RMSPE on the imputed part  
    if(j ==1 ){data_old <- data_init[ind,1:D]  }else{
      data_new <- data_init[ind,1:D];
      diff[j] <- sqrt(mean(as.matrix((data_new-data_old)^2)))/ sqrt(mean(as.matrix(data_old^2)))
      data_old <- data_new;
    }
    if(j==iter){Theta_est <- sapply(1:(D-1), function(x){ALR_reg(data_init,act_ind=x, ref_ind=D, varlist=varlist, out="coef")}); colnames(Theta_est)=colnames(train)[1:(D-1)] }
  }
  
  return(list(diff=diff,data_Imp=data_init,Theta_est=Theta_est, NArows=ind))
}


