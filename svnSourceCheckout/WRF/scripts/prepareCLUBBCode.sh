#!/bin/bash
#
# $Id: prepareCLUBBCode.sh 633 2014-04-22 22:37:08Z charlass@uwm.edu $
#
#==============================================================================
# 
# Description:
# This script prepares for the CLUBB source code checked out from the CLUBB
# repository into the directories ./phys/clubb, ./phys/silhs and./phys/microutils
# to be compiled with WRF-CLUBB.
# The script does the following things:
#   - Renaming all CLUBB sourcecode files to *.F90 to *.f90
#   - Include all the sourceode included in the preprocessor flags listed in 
#     CPPDEFSCLUBB.
#   - Exclude all the code inside the preprocessor flags which are not listed in
#     CPPDEFSCLUBB.
#
# Run:
#     % cd WRF/scripts
#     % ./prepareCLUBBCode.sh
#
#
# Notes:
# Performing these changes to the source code files from CLUBB allows us to compile
# these source files using targets in the Makefiles which came which the vendor
# version of WRF. This basically means we do not have to make changes in the 
# configure.wrf for this.

#==============================================================================

# PATH TO unifdef DIRECTORY
# UNIFDEFDIR="./remove_customized_preprocessed_code/unifdef-2.9"

# Relative path to WRF-CLUBB's main directory (starting at WRF/scripts)
MAINDIR=".."

CPPDEFSCLUBB="-DNETCDF \
              -Dnooverlap \
              -Dradoffline \
              -DCLUBB_REAL_TYPE=4 \
              -DCOAMPS_MICRO \
              -DTUNER \
              -DUNRELEASED_CODE \
              -DSILHS \
              -DBYTESWAP_IO \
             "

# RenameCLUBB files from *.F90 to .f90 and preprocess files
cd $MAINDIR/phys/clubb
echo "$MAINDIR/phys/clubb"
for file in *.F90; do
   #mv "$file" "`basename $file .F90`.f90" 
   #../../remove_customized_preprocessed_code/$UNIFDEFDIR/unifdef -B -t -m $CPPDEFSCLUBB "`basename $file .F90`.f90"
   gcc -E -CC -cpp -P -nostdinc -CC -o "`basename $file .F90`.f90" $CPPDEFSCLUBB $file
done

cd ../silhs
echo "$MAINDIR/phys/silhs"
for file in *.F90; do
   #mv "$file" "`basename $file .F90`.f90"
   #../../remove_customized_preprocessed_code/$UNIFDEFDIR/unifdef -B -t -m $CPPDEFSCLUBB "`basename $file .F90`.f90"
   gcc -E -CC -cpp -P -nostdinc -CC -o "`basename $file .F90`.f90" $CPPDEFSCLUBB $file
done

cd ../microutils
echo "$MAINDIR/phys/microutils"
for file in *.F90; do
   #mv "$file" "`basename $file .F90`.f90"
   #../../remove_customized_preprocessed_code/$UNIFDEFDIR/unifdef -B -t -m $CPPDEFSCLUBB "`basename $file .F90`.f90"
   gcc -E -CC -cpp -P -nostdinc -CC -o "`basename $file .F90`.f90" $CPPDEFSCLUBB $file
done
cd ../..

# Reflect the changes of the names of the source files in the list of dependencies
sed -i 's/.F90/.f90/g' ./main/depend.common
