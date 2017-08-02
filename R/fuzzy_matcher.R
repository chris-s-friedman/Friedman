#' Fuzzy string matching
#'
#' Produce a matrix showing the results of one or more fuzzy string matches
#' 
#' @param comparand1 word or set of words to make a comparison with. Must be a 
#' character Vector
#' 
#' @param comparand2 word or set of words to make a comparison with. Must be a 
#' character Vector
#' 
#' @param methods String matching method(s) to use. By default, Jaro, Jaccard, 
#' Levenshtein, and cosine methods are used. 
#' \code{"osa"} Optimal string aligment, (restricted Damerau-Levenshtein distance).
#' \code{"lv"} Levenshtein distance (as in R’s native adist).
#' \code{"dl"} Full Damerau-Levenshtein distance.
#' \code{"hamming"} Hamming distance (a and b must have same nr of characters).
#' \code{"lcs"} Longest common substring distance.
#' \code{"qgram"} q-gram distance.
#' \code{"cosine"} cosine distance between q-gram profiles
#' \code{"jaccard"} Jaccard distance between q-gram profiles
#' \code{"jw"} Jaro, or Jaro-Winker distance.
#' 
#' @param ... other parameters passed onto methods
#' 
#' @return A matrix showing degree of matches for each method chosen, for each 
#' comparand
#' 
#' @author Chris Friedman, \email{chris.s.friedman@@gmail.com}
#' 
#' @references \url{http://bigdata-doctor.com/fuzzy-string-matching-survival-skill-tackle-unstructured-information-r/}
#' 
#' @examples fuzzy_matcher(c("PECS book", "PECS activity book", "PECS"), "PECS")
#' 
#' @import stringdist reshape2
#' 
#' @export

fuzzy_matcher <- function(comparand1, comparand2, 
                          distance.methods = c('lv', 'cosine', 'jaccard', 
                                               'jw')){
dist.methods<-list()
for(m in 1:length(distance.methods))
{
  dist.name.enh <- matrix(NA, ncol = length(comparand2), 
                          nrow = length(comparand1))
  for(i in 1:length(comparand2)) {
    for(j in 1:length(comparand1)) { 
      dist.name.enh[j,i] <- stringdist(tolower(comparand2[i]),
                                       tolower(comparand1[j]),
                                       method = distance.methods[m])      
      #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
}

match.s1.s2.enh <- NULL
for(m in 1:length(dist.methods))
{
  
  dist.matrix <- as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh <- apply(dist.matrix, 1, base::min)
  for(i in 1:nrow(dist.matrix))
  {
    s2.i <- match(min.name.enh[i], dist.matrix[i,])
    s1.i <- i
    match.s1.s2.enh <- rbind(data.frame(s2.i=s2.i, s1.i=s1.i,
                                        s2name=comparand2[s2.i], 
                                        s1name=comparand1[s1.i], 
                                        adist=min.name.enh[i],
                                        method=distance.methods[m]),
                             match.s1.s2.enh)
  }
}
# Put the results in a nice df
matched.names.matrix <- dcast(match.s1.s2.enh, s2.i + s1.i + s2name + 
                                s1name~method, 
                              value.var = "adist")
}
