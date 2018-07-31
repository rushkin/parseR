cleanText=function(text
               ,lowercase=TRUE
               ,replacechars=c('j'='i','v'='u')
               ,removeangbracketed=TRUE
               ,removeparensed=TRUE
               ,onlycharacter=TRUE ##In particular, will remove punctuation
               ,removepunctuation=FALSE ##Matters only if onlycharacter=FALSE
               ,removestopwords=FALSE, language='lat' ##Currently will not work, no data
               ,customstopwords=NULL
               ,removenumbers=FALSE
               ,removeextraspaces=TRUE){
  library(tm)
  library(stringr)
  if (removeangbracketed) {
    text=gsub("<.*?>", " ", text)
  }
  if (removeparensed) {
    text=gsub("\\(.*?\\)", " ", text)
  }
  if(lowercase){
    text=tolower(text)
  }

  if (onlycharacter) {
    text=gsub('\\^a\\^','',text)
    text=gsub('\\^or\\^','',text)
    text=gsub("[^[:alnum:]///' ]", "", text)
  }
  if(removepunctuation){
    if (verbose){cat("Removing punctuation... \n")}
    text=gsub("[[:punct:]]", " ",text)
    # text=tm::removePunctuation(text,preserve_intra_word_dashes=TRUE)
  }
  if (removestopwords) {
    if (verbose){cat("Removing stopwords... \n")}
    text=tm::removeWords(text, tm::stopwords(language))
  }
  if (!is.null(customstopwords)) {
    if (verbose){cat("Removing custom stopwords...\n")}
    text=tm::removeWords(text,customstopwords)
  }
  if (removenumbers) {
    text=tm::removeNumbers(text)
  }

  if (removeextraspaces){
    text=gsub("\\s+", " ", str_trim(text))
  }

  if(length(replacechars)>0){
    for(ch in 1:length(replacechars)){
      text=gsub(names(replacechars)[ch],replacechars[ch],text)
    }
  }

  return(text)
}


#' Parse a word
#'
#'Parse a single word by making a call to a lookup parsing service
#'
#' @param w the word
#' @param URLPrefix the url of the XML parsing results for each word is constructed by prefixing URLPrefix to the word. The default value will query Perseus Latin word tool database.
#'
#' @return A list of the same structure as a \code{word} element in a \code{parsed} data object, such as produced by the function \code{iparse}.
iparseWord=function(w,URLPrefix='http://www.perseus.tufts.edu/hopper/xmlmorph?lang=la&lookup='){

  xmlfile=XML::xmlTreeParse(paste0(URLPrefix,w))
  xmltop=XML::xmlRoot(xmlfile)
  m=XML::xmlApply(xmltop,function(x){
    XML::xmlApply(x,XML::xmlValue)
  })





if(length(m)==0){
  m=list(list(form=w,standardForm=w,lemma='UNKNOWN',translation=''))
}else{
  m=lapply(m,function(x){
    x$translation=''
    x$standardForm=w
    return(x)
  })
}

return(m)
}

#' Parse text
#'
#' Parse a vector of texts by making calls to a lookup parsing service
#'
#' @param text a vector of character strings (“documents”), each of which may contain one or several words. Each element will be split into words. It is expected that the text has already been stripped of punctuation and special characters.
#' @param split the character to be used as the split marker in splitting the elements of text into words.
#' @param URLPrefix the url of the XML parsing results for each word is constructed by prefixing URLPrefix to the word. The default value will query Perseus Latin word tool database.
#' @param lineIDs if a vector of same length as text, will be used as the names of the output list.
#' @return a list with the structure: \code{documents} >> contain>> \code{words} >>contain>> \code{analyses} >>contain>> \code{fields}
#'
#' Each element of this list corresponds to an element of text, in the same order. We can call these elements \code{documents}.
#'
#' A \code{document} is a list, where each element corresponds to a word, in the same order as in the text. We call these elements \code{words}. They are named (the names are the words from the text), however one should be careful about using these names for lookup, since there may be duplicate words in a document. If no words are detected (e.g. the string in the text was empty), a NULL will be returned.
#'
#' A \code{word} is a list of one or several elements called \code{analyses}, each of which represents a parsing.
#'
#' An \code{analysis} is a list of elements called \code{fields}, such as \code{pos} (part of speech), \code{lemma}, \code{case}, etc. The inventory of fields varies from word to word (e.g. \code{tense} will be present for a verb but not for a noun). They all, however, contain a field \code{form} (the word form repeated) and \code{lemma}.
#' @details In case the word form is not recognized, lemma is UNKNOWN. In case the query failed for some reason, lemma is ERROR. Numbers, both modern digits and roman, are parsed with the lemma which is the number, converted to character (e.g. "2018"), and with \code{pos=numeral_digits}.
#' After cycling through all the words in the text, this function gives a second try to any cases with lemma ERROR and prints out how many errors there were and how many got fixed. It does it by calling the function iremoveLookupErrors(). In the unlikely case the same errors persist, use this function again.
#' @export
iparse=function(text, split=' ', URLPrefix='http://www.perseus.tufts.edu/hopper/xmlmorph?lang=la&lookup=', lineIDs=NULL){

  words=strsplit(text,split=split)
  words=words[words!='']

  ans=lapply(words,function(wordsinline){

    if(length(wordsinline)==0){
      return(NULL)
    }
    temp=lapply(wordsinline,function(w){

      #Check if it is a numeral
      suppressWarnings({n=as.numeric(as.roman(w))})
      if(w=='0'){
        n=0
      }
      if(is.na(n)){

        m=list(list(form=w,lemma='ERROR'))
        try({
          xmlfile=XML::xmlTreeParse(paste0(URLPrefix,w))
          xmltop=XML::xmlRoot(xmlfile)
          m=XML::xmlApply(xmltop,function(x){
            XML::xmlApply(x,XML::xmlValue)
          })
        })
        if(length(m)==0){
          m=list(list(form=w,standardForm=w,lemma='UNKNOWN',translation=''))
        }else{
          m=lapply(m,function(x){
            x$translation=''
            x$standardForm=w

            return(x)
          })
        }
      }else{
        m=list(list(form=w,standardForm=w,lemma=as.character(n),pos='numeral_digits',translation=''))
      }
      return(m)
    })
    names(temp)=wordsinline

    return(temp)

  })

  ans=iremoveLookupErrors(ans,URLPrefix = URLPrefix)

  ##Add line ids of the data as list names:
  if(!is.null(lineIDs)){
    if(length(lineIDS)==length(text)){
      names(ans)=lineIDs
    }
  }

  return(ans)
}

#' Query a \code{parsed} object
#'
#'This function is to perform queries on the data object \code{parsed} , such as produced by \code{iparse}.
#'
#' @param parsed a data object, such as produced by the function \code{iparse}
#' @param where a character string containing a logical expression. The analysis list should be named \code{x}. E.g., if looking for all analyses with lemma "dog", the argument should be 'x$lemma=="dog"'
#' @param absent a logical, controlling what happens when the queried field is absent: should it be treated as satisfying (TRUE) or not satisfying (FALSE) the query.
#'
#' @return a matrix with columns \code{document}, \code{word}, \code{analysis}. Each row contains the number of the document, word, analysis where the search result was found. If no results were found the output is NULL.
#' @export
iwhich=function(parsed,where='x$lemma=="UNKNOWN"',absent=FALSE){

  where=parse(text=where)

  temp1=lapply(1:length(parsed),function(i1){

    line=parsed[[i1]]
    if(!is.null(line)){
      temp2=lapply(1:length(line),function(i2){

        word=line[[i2]]

        temp3=which(as.logical(sapply(1:length(word),function(i3){

          x=word[[i3]]

          ans=eval(where)
          if(length(ans)==0){
            return(absent)
          }else{
            return(ans)
          }

        })))
        if(length(temp3)==0){
          return(NULL)
        }else{
          return(matrix(c(rep(i2,length(temp3)),temp3),nrow=length(temp3)))
        }
      })
      temp2=do.call(rbind,temp2)
      if(!is.null(temp2)){
        return(cbind(rep(i1,nrow(temp2)),temp2))
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  })

  temp2=do.call(rbind,temp1)
  if(!is.null(temp2)){
    colnames(temp2)=c('document','word','analysis')
  }
  return(temp2)

}

#' Remove lookup errors
#'
#' Takes a data object \code{parsed}, such as produced by the function \code{iparse}, finds all lookup errors (instances where the call to the parsing database failed, resulting in \code{lemma=’ERROR’}), makes the call for these instances and updates the data object.
#'
#' @param parsed an object as produced by \code{iparse}.
#' @param URLPrefix the url of the XML parsing results for each word is constructed by prefixing URLPrefix to the word. The default value will query Perseus Latin word tool database.
#'
#' @return the updated version of the object \code{parsed}.
#' @export
iremoveLookupErrors=function(parsed,URLPrefix='http://www.perseus.tufts.edu/hopper/xmlmorph?lang=la&lookup='){

  m=iwhich(parsed,where='x$lemma=="ERROR"',absent=FALSE)

  if(is.null(m)){
    cat('No errors found.\n')
    return(parsed)
  }

  errorsfound=nrow(m)
  cat('Errors found:',errorsfound,'\n')

  for(i in 1:nrow(m)){
  try({
    parsed[[m[i,1]]][[m[i,2]]]=iparseWord(names(parsed[[m[i,1]]])[m[i,2]],URLPrefix = URLPrefix)
  })
  }
  m=iwhich(parsed,where='x$lemma=="ERROR"',absent=FALSE)
  if(is.null(m)){
    errorsleft=0
  }else{
    errorsleft=nrow(m)
  }

  cat('Errors removed:',errorsfound-errorsleft,'\n')
  cat('Errors left:',errorsleft,'\n')
  return(parsed)
}

ialtForm=function(parsed,wordforms, newparsing=NULL, URLPrefix='http://www.perseus.tufts.edu/hopper/xmlmorph?lang=la&lookup='){

  t=which(duplicated(names(wordforms)))
  if(length(t)>0){
    cat('Error. Duplicate wordforms discovered:',names(wordforms)[t],'\n')
    return(parsed)
  }

  makecall=is.null(newparsing)
  if(!makecall){
    newparsing=list(newparsing)
    names(newparsing)='1'
  }

  for(iw in 1:length(wordforms)){

    form=names(wordforms)[iw]
    standardForm=as.character(wordforms)[iw]
    # m=iwhich(parsed,where=c('form'=form),absent=FALSE)
    m=iwhich(parsed,where=paste0('x$form=="',form,'"'),absent=FALSE)
    if(!is.null(m)){
      if(makecall){
        newparsing=lapply(iparseWord(standardForm,URLPrefix = URLPrefix),function(x){
          x$form=form
          x$standardForm=standardForm
          return(x)
        })
      }

      for(i in 1:nrow(m)){
        parsed[[m[i,1]]][[m[i,2]]]=newparsing
      }
    }
  }
  return(parsed)
}

#' List word forms found by a logical condition
#'
#' List all word forms from a \code{parsed} object that sarisfy a logical condition
#'
#' @param parsed a \code{parsed} object
#' @param where a character string containing a logical expression. The analysis list should be named \code{x}. E.g., if looking for all analyses with lemma "dog", the argument should be 'x$lemma=="dog"'
#'
#' @return a sorted string vector of all distinct word forms found. If the argument \code{where} is an empty string it is interpreted as no condition, and the output will contain all word forms present in \code{parsed}.
#' @export
ilistForms=function(parsed,where='x$lemma=="UNKNOWN"'){

  if(where!=''){

  m=iwhich(parsed,where=where,absent=FALSE)

  if(is.null(m)){
    return(NULL)
  }

  m1=sapply(1:nrow(m),function(i){
    return(parsed[[m[i,1]]][[m[i,2]]][[1]]$form)
  })

  }else{

  m1=unlist(sapply(parsed,function(x1){
    temp1=sapply(x1,function(x2){
      temp2=sapply(x2,function(x3){
        return(x3$form)
      })
      return(temp2)
    })
    return(temp1)
  }))

  }

  return(sort(unique(m1)))
}
