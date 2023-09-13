##################################
### Photo Randomizer Functions ###
##################################

################################################################################
#
### File Description ###
#
# Filename: PhotoRandomizer-Functions.R
# Author: Sam McNeely
# Contributors: 
# Date Created: 09/13/2023
# Date Modified: 09/13/2023
#
# Purpose: 
# The purpose of this script is to organize the GoPro sampling metadata,
# randomize the image site numbers to minimize scoring bias, and combine it all 
# into an Excel workbook to be scored.
#
# Difference from PhotoRandomizer.R:
# This script is different from PhotoRandomizer.R because here we employ
# functions to do the work for us. This makes the process simpler to read and
# input necessary data because the data is input in one location of the script
# and the functions take care of the rest for us. It minimizes the editing and
# run time to get the same output.
#
# Place in the data collection process:
# This R script is meant to be run after images have been extracted from the 
# GoPro videos and before the images are qualitatively scored.
#
################################################################################

### R Script ###