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
#' @export
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
  x=match(x,spelling_settings$characters)
  y=match(y,spelling_settings$characters)
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
#' @param ideal a vector of word forms in correct spelling, case-insensitive. If NULL, will use \code{real}, thus returning a square distance matrix.
#' @param spelling_settings a list of spelling error weights. If NULL, will use the default settings.
#'
#' @return a matrix of spelling distances.
#' @export
spelldist=function(real,ideal=NULL, spelling_settings=NULL){

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


    for(i in 1:length(spelling_settings$combinations))
      real=gsub(names(spelling_settings$combinations)[i],spelling_settings$combinations[i],real)
    ideal=gsub(names(spelling_settings$combinations)[i],spelling_settings$combinations[i],ideal)


  text1=strsplit(real,'')
  text2=strsplit(ideal,'')

  temp=lapply(text1,function(y){
    dd=sapply(text2,function(x){
      spelldist_elem(y,x,spelling_settings=spelling_settings)
    })
  })
  mat=do.call(rbind,temp)


  rownames(mat)=rownms
  colnames(mat)=colnms
  return(mat)

}

