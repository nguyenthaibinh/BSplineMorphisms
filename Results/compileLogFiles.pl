#! /usr/bin/perl -w

use strict;

use Cwd 'realpath';
use File::Find;
use File::Basename;
use File::Path;
use File::Spec;
use FindBin qw($Bin);

my $usage = qq{
  Usage: compileLogFiles.pl <inputDir> <outputFile>
 };

my ( $inputDirectory, $outputFile ) = @ARGV;

my @logFiles = <${inputDirectory}/*syn.o*>;

my @numberOfIterationsPerLevel = ( 100, 100, 70, 20 );

open( CSV_FILE, ">${outputFile}" );
print CSV_FILE "WHICH,REGULARIZATION,";
for( my $i = 0; $i < @numberOfIterationsPerLevel; $i++ )
  {
  for( my $j = 0; $j < $numberOfIterationsPerLevel[$i]; $j++ )
    {
    print CSV_FILE "LEVEL${i}_ITERATION${j}";
    if( $i == @numberOfIterationsPerLevel-1 && $j == $numberOfIterationsPerLevel[$i]-1 )
      {
      print CSV_FILE "\n";
      }
    else
      {
      print CSV_FILE ",";
      }
    }
  }

for( my $i = 0; $i < @logFiles; $i++ )
  {
  open( FILE, "<${logFiles[$i]}" );
  my @fileContents = <FILE>;
  close( FILE );

  # Check to see if this particular registration ran to
  # completion successfully.

  my @lastLineTokens = split( ': ', $fileContents[-1] );
  if( ${lastLineTokens[0]} !~ m/Output\ warped\ image/ )
    {
    print "Bad file:  ${logFiles[$i]}\n";
    }

  chomp( ${lastLineTokens[1]} );
  my ( $filename, $directories, $suffix ) = fileparse( ${lastLineTokens[1]}, ".nii.gz" );

  my $regularization = 'syn';
  if( $directories =~ /BSyN_DOUBLE/ )
    {
    $regularization = 'bsyn';
    }
  print CSV_FILE "${filename},${regularization},";

  # get the iteration index where each level begins

  my $keepTrack = 0;
  my @iterationIndices = ();
  for( my $j = 0; $j < @fileContents; $j++ )
    {
    if( $fileContents[$j] =~ /^Stage\ 2/ )
      {
      $keepTrack = 1;
      }
    if( $keepTrack == 1 )
      {
      if( $fileContents[$j] =~ m/^XDIAGNOSTIC/ || $fileContents[$j] =~ m/^XXDIAGNOSTIC/ )
        {
        push( @iterationIndices, $j );
        }
      }
    }

  if( @iterationIndices >= 4 )
    {
    @iterationIndices = ( $iterationIndices[-4], $iterationIndices[-3], $iterationIndices[-2], $iterationIndices[-1] )
    }
  else
    {
    print "Bad file:  ${logFiles[$i]}\n";
    }

  # Get all iteration metric values for all levels

#   print "${logFiles[$i]}\n";
#   print "@iterationIndices\n";

  for( my $j = 0; $j < 4; $j++ )
    {
    my @metricValues = ( 'NA' ) x $numberOfIterationsPerLevel[$j];

    my $startIndex = $iterationIndices[$j] + 1;
    my $endIndex = scalar( @fileContents ) - 1;
    if( $j < 3 )
      {
      $endIndex = $iterationIndices[$j+1] - 5;
      }
    else
      {
      $endIndex = scalar( @fileContents ) - 15;
      }

    for( my $k = $startIndex; $k < $endIndex; $k++ )
      {
      my @lineTokens = split( ',', $fileContents[$k] );
      $metricValues[$k-$startIndex] = $lineTokens[2];
      }

    my $metricValueString = join( ",", @metricValues );
    if( $j < 3 )
      {
      print CSV_FILE "${metricValueString},";
      }
    else
      {
      print CSV_FILE "${metricValueString}\n";
      }
    }

#   print "${logFiles[$i]}\n";
#   print "@iterationIndices\n";

  }

close( CSV_FILE );


