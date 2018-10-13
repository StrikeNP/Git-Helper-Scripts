#!/bin/bash
#
# $Id: publicRelease.sh 633 2014-04-22 22:37:08Z charlass@uwm.edu $
#
#==============================================================================
# 
# Description:
# This script prepares the WRF-CLUBB checkout for public release
# The script does the following things:
#   - delete all source code which is contained in the preprocessor flags
#     specified in remove_customized_preprocessed_code/unifdefdir.sh
#   - delete CLUBB related source code from WRF's microphysics routines
#   - remove Id tag from all files
#   - remove leftovers of Tak's comments (after unifdefdir.sh, see comment below)
#   - delete unused files (files related to the preprocessor flags we excluded)
#
# Notes:
# - Deletion of unwanted preprocessor flags in registry files does not work yet
# - It might be necessary to recompile unifdef in 
#   WRF/scripts/remove_customized_preprocessed_code/unifdef-2.9
#   before you are able to successfully run this script.
#==============================================================================


# General setup
VERBOSE="T"
UNIFDEFDIR="./unifdef-2.9"
MACROS="-UWRFLES -UWRFSTAT -UWRFLPT -UCLUBBSTATS -UTESTCASES -UESRL -UMORRF2M"

# Relative path to WRF-CLUBB's main directory (starting at WRF/scripts)
MAINDIR=".."



#===============Remove code inside certain preprocessor flags==================
#
cd remove_customized_preprocessed_code
./unifdefdir.sh ../$MAINDIR

# Also remove flags from registry files
# THis does not work correctly right now
$UNIFDEFDIR/unifdef -B -t -m $MACROS ../$MAINDIR/Registry/registry.clubb
$UNIFDEFDIR/unifdef -B -t -m $MACROS ../$MAINDIR/Registry/Registry.EM_COMMON
cd ..
#==============================================================================




#=============================Remove Id tags===================================
#
FILELIST=( $(find $MAINDIR -type f -name "*" | grep  -v .svn))

for i in "${FILELIST[@]}"
do
   if [ $VERBOSE = "T" ]; then
      echo "Removing Id-tag from file: $i"
   fi
   sed -i '/^! $Id:/d' $i
done

sed -i '/^# $Id:/d' $MAINDIR/Registry/Registry.EM
sed -i '/^# $Id:/d' $MAINDIR/phys/Makefile
#==============================================================================




#=======================Remove Tak's code commands=============================
#
# Remove leftovers of Tak's comments
# Because it is not allways possible to include Tak's comments inside the
# preprocessor flags we must delete what ever is over after using unifdefdir.sh
sed -i '/^! Tak Yamaguchi/d' $MAINDIR/dyn_em/module_em.F
sed -i '/^! Tak Yamaguchi/d' $MAINDIR/phys/module_physics_addtendc.F
sed -i '/^! Tak Yamaguchi/d' $MAINDIR/phys/module_physics_init.F
sed -i '/^! Tak Yamaguchi/d' $MAINDIR/phys/module_ra_rrtm.F
sed -i '/^! Tak Yamaguchi/d' $MAINDIR/phys/module_radiation_driver.F
#==============================================================================




#=====================Remove unnecessary dependencies==========================
#
# Be very careful when changing the following section! 
# Every whitespace character inside the patter has to to match exactly its 
# counter part in the original file.


# Remove dependencies for unused files in dyn_em/depend.dyn_em
sed -n -i '1h;1!H;${;g
s~../frame/module_wrf_error.o \\\n		../frame/module_statistics.o~../frame/module_wrf_error.o~
p}' $MAINDIR/dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${;g
s~../frame/module_wrf_error.o \\\n		../frame/module_statistics.o \\\n		../frame/module_wrfles.o~../frame/module_wrf_error.o~
p}' $MAINDIR/dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${g;s~../share/module_model_constants.o \\\n\s*../frame/module_statistics.o~../share/module_model_constants.o~; p}' $MAINDIR/dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${;g
s~module_init_utilities.o \\\n		../frame/module_statistics.o \\\n		../frame/module_wrfles.o~module_init_utilities.o~
p}' $MAINDIR/dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${;g
s~../phys/module_physics_addtendc.o \\\n		../frame/module_statistics.o \\\n		../frame/module_wrfles.o~../phys/module_physics_addtendc.o~
p}' $MAINDIR/dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${;g
s~../phys/module_fddagd_driver.o \\\n		../frame/module_wrfles.o~../phys/module_fddagd_driver.o~
p}' $MAINDIR//dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${;g
s~frame/module_statistics.o: \\\n.*../frame/module_nesting.o \\\n		../share/module_model_constants.o~~
p}' $MAINDIR/dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${;g
s~frame/module_wrfles.o : \\\n.*../frame/module_statistics.o \\\n		../share/module_model_constants.o~~
p}' $MAINDIR/dyn_em/depend.dyn_em;

sed -n -i '1h;1!H;${;g
s~\n\n\n\n# End of DEPENDENCIES for dyn_em~# End of DEPENDENCIES for dyn_em~
p}' $MAINDIR/dyn_em/depend.dyn_em;


## Remove modules for unused files in frame/Makefile
sed -n -i '1h;1!H;${;g
s~module_intermediate_nmm.o  \\\n.*module_statistics.o.*\\\n.*module_wrfles.o~module_intermediate_nmm.o~
p}' $MAINDIR/frame/Makefile;

sed -n -i '1h;1!H;${;g
s~module_intermediate_nmm.o\n\nALOBJS =\\~module_intermediate_nmm.o\nALOBJS =\\~
p}' $MAINDIR/frame/Makefile;


# Remove dependencies for unused files in main/depend.common
sed -i '/For module_statistics we need two entries/,+31d' $MAINDIR/main/depend.common

sed -n -i '1h;1!H;${g;s~../frame/module_wrf_error.o \\\n\s*../frame/module_statistics.o~../frame/module_wrf_error.o~; p}'  $MAINDIR/main/depend.common;

sed -n -i '1h;1!H;${g;s~module_ra_rrtmg_sw.o: module_ra_rrtmg_lw.o \\\n\s*../frame/module_statistics.o~module_ra_rrtmg_sw.o: module_ra_rrtmg_lw.o~; p}'  $MAINDIR/main/depend.common;

sed -n -i '1h;1!H;${g;s~module_ra_clWRF_support.o \\\n\s*../frame/module_statistics.o~module_ra_clWRF_support.o~; p}'  $MAINDIR/main/depend.common;

sed -n -i '1h;1!H;${g;s~../share/module_model_constants.o \\\n\s*module_ra_dyclw.o \\\n\s*module_mp_feingold2m.o~../share/module_model_constants.o ~; p}'  $MAINDIR/main/depend.common;

sed -n -i '1h;1!H;${g;s~../share/module_model_constants.o \\\n\s*module_mp_feingold2m.o~../share/module_model_constants.o~; p}'  $MAINDIR/main/depend.common;

sed -n -i '1h;1!H;${g;s~../share/module_model_constants.o \\\n\s*../frame/module_wrfles.o~../share/module_model_constants.o ~; p}'  $MAINDIR/main/depend.common;

sed -n -i '1h;1!H;${g;s~../frame/module_configure.o \\\n\s*../share/module_model_constants.o \\\n\s*../frame/module_wrfles.o~../frame/module_configure.o \\\n\t\t../share/module_model_constants.o  ~; p}'  $MAINDIR/main/depend.common;


sed -i '/module_cu_TestCases.o: \\/,+17d' $MAINDIR/main/depend.common

sed -n -i '1h;1!H;${g;s~../frame/module_state_description.o \\\n\s*../frame/module_statistics.o \\\n\s*../frame/module_wrfles.o~../frame/module_state_description.o~; p}'  $MAINDIR/main/depend.common;

sed -i '/		module_statistics.o \\/,+0d' $MAINDIR/main/depend.common

# Remove modules for unused files in phys/Makefile
sed -i '/module_quadratic.o/,+0d' $MAINDIR/phys/Makefile
sed -i '/module_cu_TestCases.o/,+0d' $MAINDIR/phys/Makefile
sed -i '/module_mp_feingold2m.o/,+0d' $MAINDIR/phys/Makefile
sed -i '/module_ra_dyclw.o/,+0d' $MAINDIR/phys/Makefile
sed -i '/module_sf_TestCases.o/,+0d' $MAINDIR/phys/Makefile

#==============================================================================




#============Remove unwanted definitions in registry files=====================
#
# Remove unwanted definitions from registry.clubb
sed -i 's~ifdef~#ifdef~g' $MAINDIR/Registry/registry.clubb
sed -i 's~endif~#endif~g' $MAINDIR/Registry/registry.clubb
./remove_customized_preprocessed_code/$UNIFDEFDIR/unifdef -B -t -m $MACROS  $MAINDIR/Registry/registry.clubb
sed -i 's~#ifdef~ifdef~g' $MAINDIR/Registry/registry.clubb
sed -i 's~#endif~endif~g' $MAINDIR/Registry/registry.clubb


# Remove unwanted definitions from Registry.EM_COMMON
sed -i 's~#~!!!foo!!~g' $MAINDIR/Registry/Registry.EM_COMMON
sed -i 's~ifdef~#ifdef~g' $MAINDIR/Registry/Registry.EM_COMMON
sed -i 's~ifndef~#ifndef~g' $MAINDIR/Registry/Registry.EM_COMMON
sed -i 's~endif~#endif~g' $MAINDIR/Registry/Registry.EM_COMMON
./remove_customized_preprocessed_code/$UNIFDEFDIR/unifdef -B -t -m $MACROS  $MAINDIR/Registry/Registry.EM_COMMON
sed -i 's~#ifdef~ifdef~g' $MAINDIR/Registry/Registry.EM_COMMON
sed -i 's~#ifndef~ifndef~g' $MAINDIR/Registry/Registry.EM_COMMON
sed -i 's~#endif~endif~g' $MAINDIR/Registry/Registry.EM_COMMON
sed -i 's~!!!foo!!~#~g' $MAINDIR/Registry/Registry.EM_COMMON
sed -i '/# Original WRF code:/,+0d' $MAINDIR/Registry/Registry.EM_COMMON
#==============================================================================




#==================Prepare source code pulled from CLUBB=======================
#
# Change file extension of CLUBB's source code files and preprocess them
./prepareCLUBBCode.sh
rm $MAINDIR/phys/clubb/*.F90
rm $MAINDIR/phys/silhs/*.F90
rm $MAINDIR/phys/microutils/*.F90


# Copy configure.wrf files for Yellowstone so that they could be commited too
cp $MAINDIR/custom_config_files/*yellowstone* $MAINDIR/


# Delete unused files (files related to the preprocessor flags we excluded)
rm -rf $MAINDIR/custom_config_files

rm $MAINDIR/frame/module_statistics.F
rm $MAINDIR/frame/module_wrfles.F
rm $MAINDIR/phys/module_cu_TestCases.F
rm $MAINDIR/phys/module_mp_feingold2m.F
rm $MAINDIR/phys/module_quadratic.F
rm $MAINDIR/phys/module_ra_dyclw.F
rm $MAINDIR/phys/module_sf_TestCases.F
rm $MAINDIR/Registry/Registry.EM.voca.diurnalOutput
rm $MAINDIR/Registry/Registry.EM.voca.reducedOutput
rm $MAINDIR/Registry/Registry.EM_lessout
rm $MAINDIR/Registry/Registry.EM_moreout
rm $MAINDIR/Arakawa_C_grid.txt

# Remove our files from the test directory
rm -rf $MAINDIR/test/em_les/AM3_14S80W_JDAY552
rm -rf $MAINDIR/test/em_les/DYCOMS_RF01
rm -rf $MAINDIR/test/em_les/DYCOMS_RF02
rm -rf $MAINDIR/test/em_les/RICO
rm -rf $MAINDIR/test/em_les/lookuptables
rm -rf $MAINDIR/test/em_les/stat
rm -rf $MAINDIR/test/em_les/temp
rm $MAINDIR/test/em_les/README.wrfles
rm $MAINDIR/test/em_les/newcoll_1
rm $MAINDIR/test/em_les/newcoll_2 
rm $MAINDIR/test/em_les/sc12_0
rm $MAINDIR/test/em_les/sc12_3
rm $MAINDIR/test/em_les/sd12_0
rm $MAINDIR/test/em_les/sd12_3

rm $MAINDIR/test/em_quarter_ss/*.txt
rm $MAINDIR/test/em_quarter_ss/*.cdl
rm $MAINDIR/test/em_quarter_ss/input_sounding_*
rm $MAINDIR/test/em_quarter_ss/make*
rm $MAINDIR/test/em_quarter_ss/namelist.input.*
rm $MAINDIR/test/em_quarter_ss/run_WRF_CLUBB.bash

rm $MAINDIR/test/em_real/namelist.input.summer
rm $MAINDIR/test/em_real/namelist.input.winter
#==============================================================================


exit
