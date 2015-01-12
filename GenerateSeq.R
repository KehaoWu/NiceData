randomOne = function()
{
  repo = c("A","B","C","D","E","F","G","H",
           "I","J","K","L","M","N","O","P",
           "Q","R","S","T","U","V","W","X",
           "Y","Z")
  return(repo[sample(1:26,size = 1)])
}

generateSeq = function(destSeq="TOBEORNOTTOBE",Niter=5000)
{
  timesSet = vector(mode = "numeric",length = Niter)
  pb = txtProgressBar(min = 1,max = Niter,style = 3)
  destSeq = strsplit(destSeq,split = "")[[1]]
  for(i in 1:Niter)
  {
    times = 0  ## Times of random
    seq = vector(mode = "character",length = length(destSeq))
    while(!all(destSeq==seq))
    {
      times = times + 1
      unequalIndex = (1:length(destSeq))[destSeq!=seq]
      lapply(unequalIndex,FUN = function(x)
      {
        seq[x] <<- randomOne()
      })
    }
    timesSet[i] = times
    setTxtProgressBar(pb = pb,value = i)
  }
  cat("\n")
  averageTimes = mean(timesSet)
  sdTimes = sd(timesSet)
  cat("Average times:",averageTimes," (",sdTimes,")\n")
  return(averageTimes)
}
generateSeq()