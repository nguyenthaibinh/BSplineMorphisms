library( ggplot2 )
library( reshape )

stats <- list()

# cohortNames <- c( 'CUMC', 'IBSR', 'LPBA', 'MGH', 'MAL' )
cohortNames <- c( 'CUMC12', 'IBSR18', 'LPBA40', 'MGH10', 'MAL35' )

stats[[1]] <- read.csv( "./logJacobianMeasuresCUMCTemplate.csv" )
stats[[2]] <- read.csv( "./logJacobianMeasuresIBSRTemplate.csv" )
stats[[3]] <- read.csv( "./logJacobianMeasuresLPBATemplate.csv" )
stats[[4]] <- read.csv( "./logJacobianMeasuresMGHTemplate.csv" )
stats[[5]] <- read.csv( "./logJacobianMeasuresMALTemplate.csv" )

jacobianRange <- c()
regularizationCohort <- c()
factorLevels <- c()

for( i in 1:length( cohortNames ) )
  {
  jacSyN5th <- stats[[i]]$X5th.[stats[[i]]$Regularization == 'syn']
  jacBSplineSyN5th <- stats[[i]]$X5th.[stats[[i]]$Regularization == 'bsyn']

  jacSyN95th <- stats[[i]]$X95th.[stats[[i]]$Regularization == 'syn']
  jacBSplineSyN95th <- stats[[i]]$X95th.[stats[[i]]$Regularization == 'bsyn']

  jacSyNRange <- jacSyN95th - jacSyN5th
  jacBSplineSyNRange <- jacBSplineSyN95th - jacBSplineSyN5th

  regCohort <- c( rep.int( paste0( "SyN\n(", cohortNames[i], ")" ), length( jacSyNRange ) ),
                  rep.int( paste0( "BSplineSyN\n(", cohortNames[i], ")" ), length( jacBSplineSyNRange ) ) )

  factorLevels <- append( factorLevels, c( paste0( "BSplineSyN\n(", cohortNames[i], ")" ), paste0( "SyN\n(", cohortNames[i], ")" ) ) )

  regularizationCohort <- append( regularizationCohort, regCohort )
  jacobianRange <- append( jacobianRange, c( jacSyNRange, jacBSplineSyNRange ) )
  }

plotData <- data.frame( Cohort = factor( regularizationCohort, levels = factorLevels ), Range = jacobianRange )

myPlot <- ggplot( plotData,  aes( x = Cohort, y = Range ) ) +
										geom_violin( aes( fill = Cohort ), alpha = 0.75 ) +
										scale_fill_manual( name = "",
										  values = c( "darkred", "navyblue", "darkred", "navyblue", "darkred", "navyblue", "darkred", "navyblue", "darkred", "navyblue" ),
										  labels = c( "BSplineSyN", "SyN" ) ) +
          geom_vline( xintercept = c( 2.5, 4.5, 6.5, 8.5 ), linetype = "longdash", alpha = 0.5 ) +
										scale_x_discrete( "" ) +
										scale_y_continuous( "Log Jacobian range (95th% - 5th%)" ) +
  								theme( legend.position = "none" )

ggsave( filename = "jacobianRange.pdf", plot = myPlot, width = 13, height = 5, units = 'in' )

#
# Render box plots (syn vs. bsyn) for each of the 5 cohorts
#

# 	for( i in 1:length( cohortNames ) )
# 			{
# 			subjectID <- gsub( "T_Template0x", "", stats[[i]]$SubjectID )
#
# 			jacSyNMean <- stats[[i]]$Mean[stats[[i]]$Regularization == 'syn']
# 			jacBSplineSyNMean <- stats[[i]]$Mean[stats[[i]]$Regularization == 'bsyn']
#
# 			jacSyN5th <- stats[[i]]$X5th.[stats[[i]]$Regularization == 'syn']
# 			jacBSplineSyN5th <- stats[[i]]$X5th.[stats[[i]]$Regularization == 'bsyn']
#
# 			jacSyN95th <- stats[[i]]$X95th.[stats[[i]]$Regularization == 'syn']
# 			jacBSplineSyN95th <- stats[[i]]$X95th.[stats[[i]]$Regularization == 'bsyn']
#
# 			plotLabels <- c( rep.int( "SyN", length( jacSyNMean ) ), rep.int( "BSplineSyN", length( jacBSplineSyNMean ) ) )
#
# 			jacSyNRange <- jacSyN95th - jacSyN5th
# 			jacBSplineSyNRange <- jacBSplineSyN95th - jacBSplineSyN5th
#
# 			plotData <- data.frame( Subject = factor( subjectID ), Type = factor( plotLabels ), Mean = c( jacSyNMean, jacBSplineSyNMean ), Range = c( jacSyNRange, jacBSplineSyNRange ) )
#
# 			myPlot <- ggplot( plotData,  aes( x = Mean, y = Range ) ) +
# 													geom_point( aes( colour = Type ), size = 5, alpha = 0.75 ) +
# 													scale_colour_manual( name = "", values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
# 													scale_fill_manual( values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
# 													scale_x_continuous( "Mean log Jacobian" ) +
# 													scale_y_continuous( "Log Jacobian range" ) +
# 													ggtitle( cohortNames[i] )
#
# 	#   subjectID <- gsub( "T_Template0x", "", stats[[i]]$SubjectID )
# 	#
# 	#   plotData <- data.frame( Regularization = stats[[i]]$Regularization,
# 	#                           Mean = stats[[i]]$Mean,
# 	#                           Min = stats[[i]]$Min,
# 	#                           Max = stats[[i]]$Max )
# 	#
# 	#   myPlot <- ggplot( plotData ) +
# 	#             geom_point( aes( x = factor( subjectID ), y = Mean, colour = factor( Regularization ), shape = factor( Regularization ) ), alpha = 1.0, size = 4 ) +
# 	#   										scale_colour_manual( name = "Regularization", values = c( "darkred", "navyblue" ), labels = c( "BSplineSyN", "SyN" ) ) +
# 	#   										scale_shape_manual( name = "Regularization", labels = c( "BSplineSyN", "SyN" ), values = c( 21, 22 ) ) +
# 	# #   										scale_fill_manual( name = "Regularization", labels = c( "BSplineSyN", "SyN" ), values = c( "darkred", "blue" ) ) +
# 	# 												scale_x_discrete( "Subject" ) +
# 	# 												scale_y_continuous( "Mean log Jacobian" ) +
# 	# # 												scale_y_continuous( "Mean log Jacobian", breaks = seq( -0.25, 0.25, by = 0.25 ), labels = seq( -0.25, 0.25, by = 0.25 ), limits = c( -0.3, 0.3 ) ) +
# 	#             ggtitle( cohortNames[i] )
#
# 			ggsave( filename = paste0( "logJacobian", cohortNames[i], "MALF.pdf" ), plot = myPlot, width = 6, height = 5, units = 'in' )
#
# 			}
