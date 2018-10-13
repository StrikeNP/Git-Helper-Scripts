#!/bin/sh
#
# $Id: run_real.hd1.sbatch 648 2014-05-09 02:40:29Z charlass@uwm.edu $
#
#==============================================================================
# THE FOLLOWING IS A SUBMISSION SCRIPT USED TO SUBMIT WRF-CLUBB JOBS ON HD1
# TO SUBMIT:
# % sbatch run_real.hd1.sbatch
# 
#==============================================================================


#============================Scheduling information============================

#Specify the job name, appears in the output of squeue
#SBATCH -J run_real

#Specify the compiler output filename
#SBATCH -o wrf.log.output.%j
#SBATCH -e wrf.log.error.%j

#THESE TWO WILL SEND AN EMAIL TO YOUR PANTHER EMAIL WHEN THE JOB STARTS, ENDS, 
#EXITS IN ERROR, OR IS REQUEUED.
#SBATCH --mail-type=ALL

#SPECIFY THE RECIPIENT OF THE NOTIFICATIONS
#SBATCH --mail-user=$(whoami)@uwm.edu

#MAKE SURE THE NUMBER IN '-n #' IS EQUAL TO THE NUMBER OF CORES YOU ARE USING.
#THIS MUST ALSO MATCH THE RULES LAID OUT IN SAM's USER GUIDE.
#SBATCH -n 4

#SBATCH --mem-per-cpu=2000mb

#==============================================================================



#=================SHOULD NOT HAVE TO EDIT BELOW THIS LINE=====================

# Load LIBRARIES
module use  /sharedapps/ICC/modulefiles
module load icc/11.1
module load openmpi/1.6.5
module load zlib/1.2.8
module load hdf5/1.8.12
module load netcdf/4.3.0
module load libpng/1.6.8
export LD_LIBRARY_PATH=/sharedapps/ICC/11.1/netcdf/4.3.0/lib:$LD_LIBRARY_PATH

#==============================================================================



#===============================RUN SIMULATION=================================

mpirun -n 4 ./real.exe

#==============================================================================



















