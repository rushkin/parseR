#' Spelling settings
#'
#' Generate the default spelling error weights and some other spelling settings
#'
#' @return a list of spelling settings:
#' \item{characters}{Vector of characters. Some character combinations are represented as special characters, so they can be treated as a single character}
#' \item{combinations}{Vector of character combinations included in \code{characters}.}
#' \item{ins_weights}{A named vector of weights for inserting a character. E.g. ins_weights["a"] is the penalty for the scribe inserting an "a".}
#' \item{del_weights}{A named vector of weights for deleting (omitting) a character. E.g. del_weights["a"] is the penalty for the scribe omitting an "a".}
#' \item{sub_weights}{A named matrix of weights for substituting one character instead of another. E.g. sub_weights["a","b"] is the penalty for the scribe writing "a" instead of "b".}
#' \item{maxnchardiff}{The maximum difference in the character counts of two words for which distance is computed, if exceeded, distance Inf is assigned. Used for speed, to avoid calculating distances between clearly different words.}
spelling_settings_default=function(){
spelling_settings=list()

spelling_settings$characters=c('ss'='\002','cc'='\003','ll'='\004',' ','-',letters)
##Some combinations of characters may be treated as one special character, e.g. ss and cc.
spelling_settings$combinations=spelling_settings$characters[names(spelling_settings$characters)!='']

spelling_settings$ins_weights=rep(1,length(spelling_settings$characters))
names(spelling_settings$ins_weights)=spelling_settings$characters
spelling_settings$del_weights=rep(1,length(spelling_settings$characters))
names(spelling_settings$del_weights)=spelling_settings$characters
spelling_settings$ins_weights['e']=0.15 ##Scribe inserted "e"
spelling_settings$ins_weights['i']=0.15 ##Scribe inserted "i"
spelling_settings$del_weights['e']=0.15 ##Scribe omitted "e"
spelling_settings$ins_weights['h']=0.15 ##Scribe inserted "h"
spelling_settings$del_weights['h']=0.15 ##Scribe omitted "h"

spelling_settings$sub_weights=matrix(1,nrow=length(spelling_settings$characters),ncol=length(spelling_settings$characters))
diag(spelling_settings$sub_weights)=0
rownames(spelling_settings$sub_weights)=spelling_settings$characters
colnames(spelling_settings$sub_weights)=spelling_settings$characters
spelling_settings$sub_weights['i','y']=0.1 ##Scribe wrote "i" instead of "y"
spelling_settings$sub_weights['y','i']=0.1 ##Scribe wrote "y" instead of "i"
spelling_settings$sub_weights['j','i']=0
spelling_settings$sub_weights['i','j']=0
spelling_settings$sub_weights['u','v']=0
spelling_settings$sub_weights['v','u']=0

spelling_settings$sub_weights['\002','s']=0.1
spelling_settings$sub_weights['s','\002']=0.1

spelling_settings$sub_weights['\003','c']=0.1
spelling_settings$sub_weights['c','\003']=0.1

spelling_settings$sub_weights['\004','l']=0.1
spelling_settings$sub_weights['l','\004']=0.1
spelling_settings$maxnchardiff=4
return(spelling_settings)
}

spelldist_elem=function(x,y,spelling_settings){

  m=length(x)
  n=length(y)
  if(abs(m-n)>spelling_settings$maxnchardiff){
    return(Inf)
  }
  if(m==n){
    if(all(x==y)){return(0)}
  }

  if(m==0){
    if(n==0){
      return(0)
    }else{
      return(sum(spelling_settings$del_weights[y]))
    }
  }else{
    if(n==0){
      return(sum(spelling_settings$ins_weights[x]))
    }
  }



  v0=c(0,cumsum(spelling_settings$del_weights[y]))
  v10=cumsum(spelling_settings$ins_weights[x])
  v1=v0

  for(i in 1:m){
    v1[1]=v10[i]

    for(j in 1:n){
      subst=v0[j]+spelling_settings$sub_weights[x[i],y[j]]
      ins=v0[j+1]+spelling_settings$ins_weights[x[i]]
      del=v1[j]+spelling_settings$del_weights[y[j]]

      v1[j+1]=min(del,ins,subst)
    }
    v0=v1
  }

  return(v0[n+1])

}




#' Spelling distance matrix
#'
#'Calculate spelling distance matrix according to weights of insertion, deletion and substitution of characters of character combinations
#'
#' @param real a vector of word forms as the scribe wrote them, case-insensitive.
#' @param ideal a vector of word forms in correct spelling, case-insensitive. If NULL, will use \code{real}, thus returning a symmetric distance matrix.
#' @param spelling_settings a list of spelling settings, such as spelling weights (=penalties). If NULL, will use the default settings.
#'
#' @return a matrix of spelling distances.
#' @details Due to different spelling weights, the distance matrix may not by symmetric. Zero weights (penalties) for insertion or substitution do not require the spelling-distance algorithm (it is simply spelling normalization). Hence, setting some of these weights to zero improves the speed of calculation compared to however small but non-zero weights.
spelldist_old=function(real,ideal=NULL, spelling_settings=NULL){

  if(is.null(spelling_settings)){
    spelling_settings=spelling_settings_default()
  }

  if(is.null(ideal)){
    ideal=real
  }

  ideal=tolower(ideal)
  real=tolower(real)

  rownms=real
  colnms=ideal


    for(i in 1:length(spelling_settings$combinations)){
      real=gsub(names(spelling_settings$combinations)[i],spelling_settings$combinations[i],real)
    ideal=gsub(names(spelling_settings$combinations)[i],spelling_settings$combinations[i],ideal)
    }

  #Convert strings to numeric vectors
  text1=lapply(strsplit(real,''),function(x){match(x,spelling_settings$characters)})
  text2=lapply(strsplit(ideal,''),function(x){match(x,spelling_settings$characters)})

  #If there are zero penalties for insertion or substitution, we can simply gsub, which is much quicker. This is "spelling normalization
  free_insertions=names(spelling_settings$ins_weights)[spelling_settings$ins_weights==0]
  text1=lapply(text1,function(str){str=str[!(str %in% free_insertions)]})

  free_substitutions=which(spelling_settings$sub_weights==0,arr.ind=TRUE)
  free_substitutions=free_substitutions[free_substitutions[,1]>free_substitutions[,2],]

  text1=lapply(text1,function(str){
    i=free_substitutions[match(str,free_substitutions[,1]),2]
    ind=which(!is.na(i))
    str[ind]=i[ind]
    return(str)
    })

  text2=lapply(text2,function(str){
    i=free_substitutions[match(str,free_substitutions[,1]),2]
    ind=which(!is.na(i))
    str[ind]=i[ind]
    return(str)
  })




  temp=lapply(text1,function(y,spelling_settings){
    dd=sapply(text2,function(x,spelling_settings){
      spelldist_elem(y,x,spelling_settings=spelling_settings)
    },spelling_settings=spelling_settings)
  },spelling_settings=spelling_settings)
  mat=do.call(rbind,temp)


  rownames(mat)=rownms
  colnames(mat)=colnms
  return(mat)

}

