convergenceTable <- read.csv( "LogFilesMGH.csv" )

numberOfIterationsPerLevel <- c( 100, 100, 70, 20 )

for( i in 1:length( numberOfIterationsPerLevel ) )
  {
  averageConvergence <- colMeans( convergenceTable[3:( 2+numberOfIterationsPerLevel[i] )] )
  }
