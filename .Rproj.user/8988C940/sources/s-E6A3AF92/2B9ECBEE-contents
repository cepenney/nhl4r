#' @export
.drillDown <- function(x, classObj){
  if (!inherits(x, classObj)) lapply(x, .drillDown, classObj) else TRUE}

#' @export
# drills down a list for a certain level
.drillDownStop <- function(){
  
  if (!inherits(x, classObj)) lapply(x, .drillDown, classObj) else TRUE
  
  cols <- names(unlist(.drillDown(obs, "character"))) # vectors must be character, else drill down
  
  els <- strsplit(cols, "\\.") # list with vectors of drill down path
  
  dObs <- do.call(cbind, lapply(els, function(x) obs[[x]])) # n x m matrix -input to final frame
}

#' @export
.fillCols <- function(data, cols){
  dfCols <- names(data)
  if(identical(dfCols, cols)) return(data)
  missCols <- setdiff(cols, dfCols)
  out <- cbind(data, 
        matrix(NA, nrow=nrow(data), ncol=length(missCols), dimnames = list(NULL, missCols)))
  
  return(out[, cols])
}
