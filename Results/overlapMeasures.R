library( ggplot2 )
library( reshape )

stats <- list()
volumes <- list()



cohortNames <- c( 'CUMC', 'IBSR', 'LPBA', 'MGH', 'MAL' )

stats[[1]] <- read.csv( "./overlapMeasuresCUMCTemplateMALF.csv" )
volumes[[1]] <- read.csv( "./labelVolumesCUMCTemplateMALF.csv" )
stats[[2]] <- read.csv( "./overlapMeasuresIBSRTemplateMALF.csv" )
volumes[[2]] <- read.csv( "./labelVolumesIBSRTemplateMALF.csv" )
stats[[3]] <- read.csv( "./overlapMeasuresLPBATemplateMALF.csv" )
volumes[[3]] <- read.csv( "./labelVolumesLPBATemplateMALF.csv" )
stats[[4]] <- read.csv( "./overlapMeasuresMGHTemplateMALF.csv" )
volumes[[4]] <- read.csv( "./labelVolumesMGHTemplateMALF.csv" )
stats[[5]] <- read.csv( "./overlapMeasuresMALTemplateMALF.csv" )
volumes[[5]] <- read.csv( "./labelVolumesMALTemplateMALF.csv" )

# stats[[1]] <- read.csv( "./overlapMeasuresCUMCTemplate.csv" )
# volumes[[1]] <- read.csv( "./labelVolumesCUMCTemplate.csv" )
# stats[[2]] <- read.csv( "./overlapMeasuresIBSRTemplate.csv" )
# volumes[[2]] <- read.csv( "./labelVolumesIBSRTemplate.csv" )
# stats[[3]] <- read.csv( "./overlapMeasuresLPBATemplate.csv" )
# volumes[[3]] <- read.csv( "./labelVolumesLPBATemplate.csv" )
# stats[[4]] <- read.csv( "./overlapMeasuresMGHTemplate.csv" )
# volumes[[4]] <- read.csv( "./labelVolumesMGHTemplate.csv" )
# stats[[5]] <- read.csv( "./overlapMeasuresMALTemplate.csv" )
# volumes[[5]] <- read.csv( "./labelVolumesMALTemplate.csv" )


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
            geom_boxplot( aes( fill = Type ), alpha = 0.75, notch = TRUE ) +
  										scale_fill_manual( name = "", values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
												scale_y_continuous( "", breaks = seq( 0.5, 0.9, by = 0.1 ), labels = seq( 0.5, 0.9, by = 0.1 ), limits = c( 0.45, 0.9 ) ) +
            ggtitle( cohortNames[i] ) +
            scale_x_discrete( "" ) +
  								theme( legend.position = "none" )

#   ggsave( filename = paste0( "synComparison", cohortNames[i], ".pdf" ), plot = myPlot, width = 4, height = 5, units = 'in' )
  ggsave( filename = paste0( "synComparison", cohortNames[i], "MALF.pdf" ), plot = myPlot, width = 4, height = 5, units = 'in' )

  ##############

  synDifference <- melt( stats[[i]][stats[[i]]$Regularization == 'bsyn',4:ncol(stats[[i]])] -
                         stats[[i]][stats[[i]]$Regularization == 'syn',4:ncol(stats[[i]])] )
  labelVolumes <- melt( volumes[[i]][,2:ncol( volumes[[i]] )] )

  synDifference <- synDifference[labelVolumes$value > 1000,]
  labelVolumes <- labelVolumes[labelVolumes$value > 1000,]

  plot2Data <- data.frame( Volumes = labelVolumes$value, Difference = synDifference$value );

  myPlot2 <- ggplot( plot2Data, aes( x = Volumes, y = Difference ) ) +
															geom_point( colour = "darkred", size = 2, alpha = 0.25 ) +
															geom_hline( yintercept = 0, colour="navyblue", linetype = "longdash" ) +
															scale_x_continuous( "Label volume (mm)" ) +
															scale_y_continuous( "BSyN - SyN (Dice)", breaks = seq( -0.25, 0.25, by = 0.25 ), labels = seq( -0.25, 0.25, by = 0.25 ), limits = c( -0.3, 0.3 ) ) +
               ggtitle( cohortNames[i] ) +
															theme( legend.position = "none" )

#   ggsave( filename = paste0( "versus", cohortNames[i], ".pdf" ), plot = myPlot2, width = 6, height = 5, units = 'in' )
  ggsave( filename = paste0( "versus", cohortNames[i], "MALF.pdf" ), plot = myPlot2, width = 6, height = 5, units = 'in' )
  }

