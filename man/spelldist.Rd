% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spelldist.R
\name{spelldist}
\alias{spelldist}
\title{Spelling distance matrix}
\usage{
spelldist(words, standard = NULL, asdist = FALSE, indel = NULL,
  sm = NULL, cost_method = c("INDELS", "CONSTANT", "TRATE", "FUTURE",
  "FEATURES", "INDELSLOG"), dist_method = c("OM", "OMloc", "OMslen",
  "OMspell", "OMstran", "HAM", "DHD", "CHI2", "EUCLID", "LCS", "LCP",
  "RLCP", "NMS", "NMSMST", "SVRspell", "TWED"), ...)
}
\arguments{
\item{words}{a vector of word forms, case-insensitive.}

\item{standard}{a vector of word forms in standard spelling, case-insensitive. If NULL, will use \code{real}, thus returning a symmetric distance matrix.}

\item{asdist}{logical, whether to return the spelling distance as a dist object rather than a matrix. If \code{standard} on a distance matrix can be returned, ignoring \code{asdist}.}

\item{indel}{an optional named numeric vector of indel costs. The names are what is being inserted or deleted, e.g. c('a'=1,'bb'=0.2). The names do not have to be single-character.}

\item{sm}{an optional numeric matrix with rownames and colnames as the characters that are being substituted. An element \code{sm[i,j]} is the cost of substituting \code{rownames(sm)[i]} with \code{colnames(sm)[j]}. The names do not have to be single-character}

\item{cost_method}{method for calculating those indel and substitution costs that are not explicitly provided by \code{indel} and \code{sm}. See TraMineR::seqcost for descriptions.}

\item{dist_method}{method for calculating spelling distance. See TraMineR::seqdist for descriptions}

\item{...}{Optional arguments to be passed to TraMineR::seqdist (all other than the seqdata, method, indel and sm)}
}
\value{
a list of: \code{m} - matrix (or dist object) of spelling distances, with rows corresponding to \code{words}; \code{indel} and \code{sm} - as were used in the calculation; \code{elapsed} - elapsed time in seconds.
}
\description{
Calculate spelling distance matrix according to costs of insertion-deletion and substitution of characters of character combinations
}
\details{
The calculation is case-insensitive: \code{words}, \code{standard}, as well as the dimnames of \code{indel} and \code{sm} will be coerced to lower case. \code{indel, sm} provide a way to deal with multi-character rules. E.g. presence of an element \code{sm['ll','l']} indicates that "ll" will be treated as a single character. For technical reasons, however, the number of such distinct multi-character names (in indel and sm in total) must not exceed 26.
}
