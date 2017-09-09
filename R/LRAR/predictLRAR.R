crank <- function(rulz, xs, ys, std, m2, mt)
{
  #Order the rules by "confidence" and "support"
  rulz <- rulz[order(-sapply(rulz, "[[" ,"sup" ))]
  rulz <- rulz[order(-sapply(rulz, "[[" ,"conf" ))]
  
  if (mt==0)
  { 
    i <- TRUE
  } else
  {
    i <- 1:mt
  }
  
  ##Match rules whith examples
  rs <- apply(xs, 1, function(row)
  {
    a <- sapply(rulz, function(l) { all(l$a %in% row) })
    
    if (any(a))
    {
      if (sum(a)<mt) {
        ii <- 1:sum(a)
      } else {
        ii <- i
      }
      
      rt <- rank( rowMeans( sapply(rulz[a][ii], "[[", "c") ) )
      
      if (length(unique(rt))==1 )
      {
        #The method uses the baseline ranking the first rule if the avg ranking is constant
        rt <- -rank(-std)
      }
    } else
    {
      #NOTE: -rank(-c(1,2,3)) is equivalent to c(1,2,3) 
      rt <- -rank(-std)
    }
    rt
  })
  
  rs
}
