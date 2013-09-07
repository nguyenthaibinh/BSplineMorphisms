library( ggplot2 )

stats <- list()
volumes <- list()

cohortNames <- c( 'CUMC', 'IBSR', 'LPBA', 'MGH', 'MICCAI' )

stats[[1]] <- read.csv( "OverlapMeasures/overlapMeasuresCUMC.csv" )
volumes[[1]] <- read.csv( "OverlapMeasures/labelVolumesCUMC.csv" )
stats[[2]] <- read.csv( "OverlapMeasures/overlapMeasuresIBSR.csv" )
volumes[[2]] <- read.csv( "OverlapMeasures/labelVolumesIBSR.csv" )
stats[[3]] <- read.csv( "OverlapMeasures/overlapMeasuresLPBA.csv" )
volumes[[3]] <- read.csv( "OverlapMeasures/labelVolumesLPBA.csv" )
stats[[4]] <- read.csv( "OverlapMeasures/overlapMeasuresMGH.csv" )
volumes[[4]] <- read.csv( "OverlapMeasures/labelVolumesMGH.csv" )
stats[[5]] <- read.csv( "OverlapMeasures/overlapMeasuresMICCAI.csv" )
volumes[[5]] <- read.csv( "OverlapMeasures/labelVolumesMICCAI.csv" )


#
# Render box plots (syn vs. bsyn) for each of the 5 cohorts
#

for( i in 1:length( cohortNames ) )
  {
  diceSyN <- stats[[i]]$AllLabels[stats[[i]]$Regularization == 'syn']
  diceBSplineSyN <- stats[[i]]$AllLabels[stats[[i]]$Regularization == 'bsyn']

  plotLabels <- c( rep.int( "SyN", length( diceSyN ) ), rep.int( "BSplineSyN", length( diceBSplineSyN ) ) )

  plotData <- data.frame( Type = plotLabels, Dice = c( diceSyN, diceBSplineSyN ) )

  myPlot <- ggplot( plotData,  aes( x = factor( Type ), y = Dice ) ) +
            geom_boxplot( aes( fill = Type ) ) +
            ggtitle( cohortNames[i] ) +
            scale_x_discrete( "" )

  ggsave( filename = paste0( "synComparison", cohortNames[i], ".pdf" ), plot = myPlot, width = 8, height = 5, units = 'in' )
  }


#
# Determine linear model:  dicePerLabel ~ regularization + volumesPerLabel + cohort
#

allDice <- c()
allVolumes <- c()
allRegularization <- c()
allCohorts <- c()

for( i in 1:length( cohortNames ) )
  {
  cat( "\nProcessing ", cohortNames[i], ".\n", sep = "" )

  for( j in 1:nrow( volumes[[i]] ) )
    {
    currentSyNDice <- stats[[i]][2*j,]
    currentBSyNDice <- stats[[i]][2*j-1,]
    currentVolume <- volumes[[i]][j,]

    indices <- which( currentVolume > 0 )

    allRegularization <- append( allRegularization, c( rep.int( 'SyN', length( indices ) ), rep.int( 'BSplineSyN', length( indices ) ) ) )
    allDice <- append( allDice, c( currentSyNDice[2+indices], currentBSyNDice[2+indices] ) )
    allVolumes <- append( allVolumes, c( currentVolume[indices], currentVolume[indices] ) )
    allCohorts <- append( allCohorts, rep.int( cohortNames[i], 2 * length( indices ) ) )
    }
  }


indices <- 1:length( allDice )
sampleIndices <- sample( indices, size = 50000, replace = FALSE )

myDiceDF <- data.frame( Dice = as.numeric( allDice[sampleIndices] ),
                        Regularization = allRegularization[sampleIndices],
                        Volumes = as.numeric( allVolumes[sampleIndices] ),
                        Cohorts = allCohorts[sampleIndices] )
myDiceLM <- lm( Dice ~ Regularization + Volumes + Cohorts, data = myDiceDF )
summary( myDiceLM )









