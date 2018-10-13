#!/bin/bash
# $Id$
#===============================================================================
# Description:
# Script to call run_Forecast.bash by using the cron scheduler.
#===============================================================================

# Figure out the directory where the script is located.
run_dir=`dirname $0`

# Change the directory to the one in which the script is located.
cd $run_dir

# Run the WRF-CLUBB forecast.
source /etc/profile.d/larson-group.sh
./run_Forecast.bash --plot-output &> wrfoutput.txt
