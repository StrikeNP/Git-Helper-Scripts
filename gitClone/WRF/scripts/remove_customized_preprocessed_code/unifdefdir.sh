#!/bin/bash
#
#==============================================================================
##
# $Id$
#
# THE FOLLOWING SCRIPT REMOVES ALL SOURCE CODE INCLUDED IN ANY OF THE 
# PREPROCESSOR FLAGS GIVEN IN "MACROS" FROM ALL FORTRAN SOURCE FILES
# (*.f, *.F, *.f90, *.F90) IN A GIVEN DIRECROTY <in_dir>.
# 
# % ./unifdefdir <in_dir>
#
# Note:
# It might be necessary to compile unifdef in 
# WRF/scripts/remove_customized_preprocessed_code/unifdef-2.9.
# 
#==============================================================================

# DIRECTORY
DIR=$1

# PATH TO unifdef DIRECTORY
UNIFDEFDIR="./unifdef-2.9"

# MACROS WHICH WILL BE DELETED
MACROS="-UWRFLES -UWRFSTAT -UWRFLPT -UCLUBBSTATS -UTESTCASES -UESRL -UMORRF2M"

# SET TO "T" FOR DEBUG OUTPUT
VERBOSE="T"

if [ ! -d $DIR ]; then
   echo "Error: Directory $DIR not found!"
   exit
fi

# LOOP OVER ALL FORTRAN SOURCE FILES
FILELIST=( $(find $DIR -name "*.[fF]") $(find $DIR -name "*.[fF]90") )
for i in "${FILELIST[@]}"
do
   if [ $VERBOSE = "T" ]; then
      echo "Removing unwanted preprocessor flags from file: $i"
   fi

   # USE unifdef TO REMOVE PREPROCESSOR FLAGS FROM SPECIFIC FILE
   $UNIFDEFDIR/unifdef -B -t -m $MACROS $i

done

exit

