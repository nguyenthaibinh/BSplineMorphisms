library( ggplot2 )
library( gridExtra )

logFiles <- c( "CUMC", "IBSR", "LPBA", "MGH", "MICCAI" )
cohortNames <- c( 'CUMC', 'IBSR', 'LPBA', 'MGH', 'MICCAI' )

stats <- list()
volumes <- list()

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


numberOfIterationsPerLevel <- c( 100, 100, 70, 20 )
iterationCumSum <- cumsum( numberOfIterationsPerLevel )

for( f in 1:length( logFiles ) )
  {
  cat( "Processing ", logFiles[f], "\n", sep = "" )
  convergenceTable <- read.csv( paste0( "LogFiles", logFiles[f], ".csv" ) )

  convergencePlots <- list();
		for( i in 1:length( numberOfIterationsPerLevel ) )
				{
				colIndices <- 4:( 3+numberOfIterationsPerLevel[i] )
				if( i > 1 )
						{
						colIndices <- ( 4 + iterationCumSum[i-1] ):( 3 + iterationCumSum[i] )
						}

				synConvergence <- convergenceTable[which( convergenceTable$REGULARIZATION == 'syn'), colIndices]
				bsynConvergence <- convergenceTable[which( convergenceTable$REGULARIZATION == 'bsyn'), colIndices]

				avgSyNConvergence <- as.vector( colMeans( synConvergence, na.rm = TRUE ) )
				avgBSyNConvergence <- as.vector( colMeans( bsynConvergence, na.rm = TRUE ) )

				sdSyNConvergence <- as.vector( apply( synConvergence, 2, sd, na.rm = TRUE ) )
				sdBSyNConvergence <- as.vector( apply( bsynConvergence, 2, sd, na.rm = TRUE ) )

				plusSyNConvergence <- avgSyNConvergence + 1 * sdSyNConvergence
				minusSyNConvergence <- avgSyNConvergence - 1 * sdSyNConvergence

				plusBSyNConvergence <- avgBSyNConvergence + 1 * sdBSyNConvergence
				minusBSyNConvergence <- avgBSyNConvergence - 1 * sdBSyNConvergence

				plotData <- data.frame( Iteration = c( 1:numberOfIterationsPerLevel[i], 1:numberOfIterationsPerLevel[i] ),
																												AverageConvergence = c( avgSyNConvergence, avgBSyNConvergence ),
																												LowerBoundConvergence = c( minusSyNConvergence, minusBSyNConvergence ),
																												UpperBoundConvergence = c( plusSyNConvergence, plusBSyNConvergence ),
																												Regularization = c( rep( 'SyN', numberOfIterationsPerLevel[i] ), rep( 'BSplineSyN', numberOfIterationsPerLevel[i] ) )
																										)
				plotData <- transform( plotData, Regularization = factor( Regularization ) )

				convergencePlots[[i]] <-
				             ggplot( data = plotData, aes( x = Iteration, y = AverageConvergence, group = Regularization ) ) +
				             geom_ribbon( aes( x = Iteration, y = AverageConvergence , ymin = LowerBoundConvergence, ymax = UpperBoundConvergence, fill = Regularization ), alpha = 0.25 ) +
																	geom_line( aes( colour = Regularization ), size = 2 ) +
																	scale_x_continuous( "Iteration", breaks = seq( 0, numberOfIterationsPerLevel[i], by = 10 ), labels = seq( 0, numberOfIterationsPerLevel[i], by = 10 ), limits = c( 1, numberOfIterationsPerLevel[i] ) ) +
		#               scale_y_continuous( "CC metric value", breaks = seq( -0.95, -0.5, by = 0.1 ), labels = seq( -0.95, -0.5, by = 0.1 ), limits = c( -0.95, -0.5 ) ) +
																	scale_y_continuous( "CC metric value" ) +
																	scale_colour_manual( values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
       										scale_fill_manual( values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
																	ggtitle( paste( "Convergence (Level ", i, ")", sep = "" ) )
# 				ggsave( filename = paste0( logFiles[f], "ConvergenceLevel", i, ".pdf" ), plot = myPlot, width = 8, height = 6, units = 'in' )
				}

  timeSyN <- convergenceTable$ELAPSED_TIME[convergenceTable$REGULARIZATION == 'syn'] / 3600
  timeBSplineSyN <- convergenceTable$ELAPSED_TIME[convergenceTable$REGULARIZATION == 'bsyn'] / 3600

  cat( "  SyN time:  ", mean( timeSyN ), ' +/- ', sd( timeSyN ), "\n" );
  cat( "  BSyN time:  ", mean( timeBSplineSyN ), ' +/- ', sd( timeBSplineSyN ), "\n" );

  plotLabels <- c( rep.int( "SyN", length( timeSyN ) ), rep.int( "BSplineSyN", length( timeBSplineSyN ) ) )

  timeData <- data.frame( Type = plotLabels, Time = c( timeSyN, timeBSplineSyN ) )

  timePlot <- ggplot( timeData,  aes( x = factor( Type ), y = Time ) ) +
            geom_boxplot( aes( fill = Type ), alpha = 0.5 ) +
  										scale_fill_manual( values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
            ggtitle( logFiles[f] ) +
 											scale_y_continuous( "Time (hours)" ) +
            scale_x_discrete( "" )

# ggsave( filename = paste0( "timeComparison", logFiles[f], ".pdf" ), plot = timePlot, width = 8, height = 5, units = 'in' )

  # do overlap measures

  diceSyN <- stats[[f]]$AllLabels[stats[[f]]$Regularization == 'syn']
  diceBSplineSyN <- stats[[f]]$AllLabels[stats[[f]]$Regularization == 'bsyn']

  plotLabels <- c( rep.int( "SyN", length( diceSyN ) ), rep.int( "BSplineSyN", length( diceBSplineSyN ) ) )

  plotData <- data.frame( Type = plotLabels, Dice = c( diceSyN, diceBSplineSyN ) )

  dicePlot <- ggplot( plotData,  aes( x = factor( Type ), y = Dice ) ) +
														geom_boxplot( aes( fill = Type ), alpha = 0.5 ) +
														scale_fill_manual( values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
														ggtitle( cohortNames[f] ) +
														scale_x_discrete( "" )

		pdf( paste0( "all", logFiles[f], ".pdf" ), width = 20, height = 8 )

		plist <- list( convergencePlots[[1]], convergencePlots[[2]], dicePlot,
    		           convergencePlots[[3]], convergencePlots[[4]], timePlot )

  do.call("grid.arrange", c(plist, ncol=3))
		dev.off()

  }
