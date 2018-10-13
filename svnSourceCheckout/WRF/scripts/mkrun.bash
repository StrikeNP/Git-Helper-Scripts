#!/bin/sh
#
# $Id: mkrun.bash 678 2014-05-22 04:34:30Z charlass@uwm.edu $
#
#==============================================================================
#
# THE FOLLOWING IS A SCRIPT TO COLLECT ALL IMPORTANT INFORMATION AFTER RUNNING 
# A WRF-CLUBB SIMULATION AND PLACING THEM IN A DIRECTORY WHICH CAN BE SPECIFIED
# COMMAND LINE.
# 
# % ./mkrun -n <name_of_the_run>
# 
#==============================================================================



#====================USER SPECIFIED VARIABLES AND PATHS========================

# GIVE YOUR CASE A NAME. THIS WILL BE THE NAME OF THE DIRECTORY THE OUTPUT FILE
# AND STANDARD OUTPUT WILL BE LOCATED
# MAKE SURE THAT THE DIRECTORY $OUTPUTDIR/$CASE DOES NOT EXIST!

# OUTPUT DIRECTORY
OUTPUTDIR="."

# WRF MAIN DIRECTORY
WRFMAIN="../../"

#==============================================================================

### Switch to current directory
cd $PWD

myhelp()
{ 
echo "------------------------------------------------------------------------------------"
echo "This script collects all important information after running a WRF-CLUBB simulation"
echo "such as Input, Configure, Output and Log files. Furthermore it creates files"
echo "containing information gained from svn info and svn diff for the checkout and its"
echo "externals".
echo "" 
echo "Usage: ./mkrun -n <name_of_the_run>"
echo ""
echo "Options:"
echo "-n -- name of the run  <name_of_the_run>"
echo "-h -- show this page"
echo ""
echo "------------------------------------------------------------------------------------"
}

CASE=""


# FILTER OPTIONS
while getopts n:h opt
do
  case "$opt" in
    n) CASE="$OPTARG";;
    h) myhelp; exit;;
    \?) echo "Error: Unknown option."; myhelp; exit 1;;
  esac
done


# CASENAME MUST BE SPECIFIED
if [ "$CASE" = "" ]; then
  echo ""
  echo "Error: You have to specify a name for this Simulation!"
  echo ""
  myhelp 
  exit 1
fi

CASEDIR="run$CASE"

# EXIT SIMULATION IF OUTPUT DIRECTORY ALREADY EXISTS
if [ -d "$OUTPUTDIR"/"$CASEDIR" ]; then
  echo ""
  echo "Error: The directory \"$OUTPUTDIR/$CASEDIR\" already exists!"
  echo "Choose a different case name or move the other directory!"
  exit 1
fi


#================GATHERING ALL INFORMATION IN ONE DIRECTORY====================


# CREATE CASE DIRECTORY
mkdir $OUTPUTDIR/$CASEDIR
mkdir $OUTPUTDIR/$CASEDIR/wrfout
echo $CASE > $OUTPUTDIR/$CASEDIR/wrfout/run.nfo



# COPY SOURCE CODE INFORMATION of WRF-CLUBB and CLUBB
# SVN INFO OF WRF-CLUBB CHECKOUT ITSELF
svn info $WRFMAIN > wrf.base.svn.info
svn diff $WRFMAIN > wrf.base.svn.diff

# SVN INFO OF EXTERNALS OF WRF-CLUBB CHECKOUT
echo "svn info phys/clubb:" > wrf.externals.svn.info
svn info $WRFMAIN/phys/clubb >> wrf.externals.svn.info
echo "" >> wrf.externals.svn.info 
echo "svn info phys/silhs:" >> wrf.externals.svn.info
svn info $WRFMAIN/phys/silhs >> wrf.externals.svn.info
echo "" >> wrf.externals.svn.info 
echo "svn info phys/microutils:" >> wrf.externals.svn.info
svn info $WRFMAIN/phys/microutils >> wrf.externals.svn.info
echo "" >> wrf.externals.svn.info
echo "svn info test/clubb_input/stats:" >> wrf.externals.svn.info
svn info $WRFMAIN/test/clubb_input/stats >> wrf.externals.svn.info
echo "" >> wrf.externals.svn.info
echo "svn info test/clubb_input/tunable_parameters:" >> wrf.externals.svn.info
svn info $WRFMAIN/test/clubb_input/tunable_parameters/ >> wrf.externals.svn.info

echo "svn diff phys/clubb:" > wrf.externals.svn.diff
svn diff $WRFMAIN/phys/clubb >> wrf.externals.svn.diff
echo "" >> wrf.externals.svn.diff 
echo "svn diff phys/silhs:" >> wrf.externals.svn.diff
svn diff $WRFMAIN/phys/silhs >> wrf.externals.svn.diff
echo "" >> wrf.externals.svn.diff 
echo "svn diff phys/microutils:" >> wrf.externals.svn.diff
svn diff $WRFMAIN/phys/microutils >> wrf.externals.svn.diff
echo "" >> wrf.externals.svn.diff
echo "svn diff test/clubb_input/stats:" >> wrf.externals.svn.diff
svn diff $WRFMAIN/test/clubb_input/stats >> wrf.externals.svn.diff
echo "" >> wrf.externals.svn.diff
echo "svn diff test/clubb_input/tunable_parameters:" >> wrf.externals.svn.diff
svn diff $WRFMAIN/test/clubb_input/tunable_parameters/ >> wrf.externals.svn.diff

mv wrf.base.svn.info $OUTPUTDIR/$CASEDIR/
mv wrf.base.svn.diff $OUTPUTDIR/$CASEDIR/
mv wrf.externals.svn.info $OUTPUTDIR/$CASEDIR/
mv wrf.externals.svn.diff $OUTPUTDIR/$CASEDIR/



# COPYING OUTPUT, CONFIGURATION AND LOG FILES

# INPUT FILES
cp wrfbdy_d01         $OUTPUTDIR/$CASEDIR/
cp wrfinput_d01       $OUTPUTDIR/$CASEDIR/
cp wrflowinp_d01      $OUTPUTDIR/$CASEDIR/

# CONFIGURE FILES
mv namelist.output $OUTPUTDIR/$CASEDIR/
cp namelist.input     $OUTPUTDIR/$CASEDIR/
cp run_wrf.hd1.sbatch $OUTPUTDIR/$CASEDIR/
cp ../../configure.wrf $OUTPUTDIR/$CASEDIR/
mv wrf.run.bsub $OUTPUTDIR/$CASEDIR/

# OUTPUT FILES
mv wrfout*         $OUTPUTDIR/$CASEDIR/wrfout
mv wrfrst*         $OUTPUTDIR/$CASEDIR/
mv *.ctl           $OUTPUTDIR/$CASEDIR/
mv *.dat           $OUTPUTDIR/$CASEDIR/

# LOG FILES 
mv wrf.log.*   $OUTPUTDIR/$CASEDIR/
mv wrf.log.*  $OUTPUTDIR/$CASEDIR/
mv rsl.*           $OUTPUTDIR/$CASEDIR/
mv core*           $OUTPUTDIR/$CASEDIR/
mv *.btr           $OUTPUTDIR/$CASEDIR/

#==============================================================================
