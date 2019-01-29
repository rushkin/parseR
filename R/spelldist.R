
#' Spelling distance matrix
#'
#'Calculate spelling distance matrix according to costs of insertion-deletion and substitution of characters of character combinations
#'
#' @param words a vector of word forms, case-insensitive.
#' @param standard a vector of word forms in standard spelling, case-insensitive. If NULL, will use \code{real}, thus returning a symmetric distance matrix.
#' @param asdist logical, whether to return the spelling distance as a dist object rather than a matrix. If \code{standard} on a distance matrix can be returned, ignoring \code{asdist}.
#' @param indel an optional named numeric vector of indel costs. The names are what is being inserted or deleted, e.g. c('a'=1,'bb'=0.2). The names do not have to be single-character.
#' @param sm an optional numeric matrix with rownames and colnames as the characters that are being substituted. An element \code{sm[i,j]} is the cost of substituting \code{rownames(sm)[i]} with \code{colnames(sm)[j]}. The names do not have to be single-character
#' @param cost_method method for calculating those indel and substitution costs that are not explicitly provided by \code{indel} and \code{sm}. See TraMineR::seqcost for descriptions.
#' @param dist_method method for calculating spelling distance. See TraMineR::seqdist for descriptions
#' @param ... Optional arguments to be passed to TraMineR::seqdist (all other than the seqdata, method, indel and sm)
#'
#' @return a list of: \code{m} - matrix (or dist object) of spelling distances, with rows corresponding to \code{words}; \code{indel} and \code{sm} - as were used in the calculation; \code{elapsed} - elapsed time in seconds.
#' @details The calculation is case-insensitive: \code{words}, \code{standard}, as well as the dimnames of \code{indel} and \code{sm} will be coerced to lower case. \code{indel, sm} provide a way to deal with multi-character rules. E.g. presence of an element \code{sm['ll','l']} indicates that "ll" will be treated as a single character. For technical reasons, however, the number of such distinct multi-character names (in indel and sm in total) must not exceed 26.
#' @export
spelldist=function(words,standard=NULL, asdist=FALSE,
                   indel=NULL,
                   sm=NULL,
                   cost_method=c('INDELS','CONSTANT','TRATE','FUTURE','FEATURES','INDELSLOG'),
                   dist_method=c("OM", "OMloc", "OMslen", "OMspell", "OMstran", "HAM", "DHD", "CHI2", "EUCLID", "LCS", "LCP", "RLCP", "NMS", "NMSMST", "SVRspell", "TWED"),
               ...


                   ){

  tic=proc.time()[3]
  words=tolower(words)
  standard=tolower(standard)
  if(length(indel)) names(indel)=tolower(names(indel))
  if(length(sm)) rownames(sm)=tolower(rownames(sm)); colnames(sm)=rownames(sm)

  words=c(standard,words)


  #Replace multi-character entities (if any are referenced in indel or sm) with some capital letters.
  multichar_entities=(c(names(indel), rownames(sm)))
  multichar_entities=sort(unique(multichar_entities[nchar(multichar_entities)>1]))
  if(length(multichar_entities)){
    names(multichar_entities)=LETTERS[1:length(multichar_entities)]
    for(i in 1:length(multichar_entities)){
      words=gsub(multichar_entities[i],names(multichar_entities)[i],words)
    }
    if(length(indel)) names(indel)[nchar(names(indel))>1]=names(multichar_entities)[match(names(indel)[nchar(names(indel))>1], multichar_entities)]
    if(length(sm)){
      rownames(sm)[nchar(rownames(sm))>1]=names(multichar_entities)[match(rownames(sm)[nchar(rownames(sm))>1], multichar_entities)]
      rownames(sm)=paste0(rownames(sm),'->')
      colnames(sm)=rownames(sm)
    }
  }


  temp=strsplit(words, split='')

  ll=lengths(temp)
  nw=max(lengths(temp))

  temp=lapply(temp,function(x){x=c(x,rep(NA,nw-length(x)))})

  m=do.call(rbind,temp)
  suppressWarnings(suppressMessages({s=TraMineR::seqdef(m)}))
  costs=TraMineR::seqcost(s, method = cost_method[1])

  alphbt=TraMineR::alphabet(s)
  i=match(names(indel),alphbt)
  inna=!is.na(i)
  i=i[inna]
  costs$indel[i]=indel[inna]
  i=match(rownames(sm),rownames(costs$sm))
  inna=!is.na(i)
  i=i[inna]
  costs$sm[i,i]=sm[inna,inna]
  diag(costs$sm)=0

  names(alphbt)=alphbt
  alphbt[names(multichar_entities)]=multichar_entities



  suppressMessages({m=TraMineR::seqdist(s, method = dist_method[1],
             indel = costs$indel, sm = costs$sm
             ,...
  )
  })

  rownames(m)=words
  colnames(m)=words

  if(length(standard)){
    m=m[(length(standard)+1):nrow(m),1:length(standard),drop=FALSE]
  }else{
    if(asdist) m=as.dist(m)
  }

  names(costs$indel)=alphbt
  rownames(costs$sm)=alphbt
  colnames(costs$sm)=alphbt

  return(list(m=m, indel=costs$indel, sm=costs$sm, elapsed=proc.time()[3]-tic))

}

