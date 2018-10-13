#!/bin/bash
#
# $Id$
#

#################  Function Definitions  #################

myhelp()
{ 
echo ""
echo "=============================================================================="
echo " THE FOLLOWING IS A BASH SCRIPT TO COMPILE WRF-CLUBB ON YELLOWSTONE."
echo ""
echo " Note:"
echo " You have to copy/ provide an appropriate configure.wrf file if -m is specified!"
echo ""
echo " You also have to specify one of the following options:"
echo "       -G -- group computers"
echo "       -Y -- Yellowstone"
echo "       -H -- HD1"
echo ""
echo " Usage:"
echo " Checkout WRF-CLUBB and change to the main WRF directory (WRF), copy this script"
echo " build_wrf.yellowstone.bash:"
echo ""
echo "      cp scripts/build_wrf.yellowstone.bash ."
echo ""
echo " and copy an appropriate configure.wrf file, e.g."
echo ""
echo "     cp custom_config_files/configure.wrf.intel64.dmpar.yellowstone.clubb configure.wrf"
echo ""
echo "To actually run the script type"
echo ""
echo "     ./build_wrf.yellowstone.bash [-c <compiler>] [-t <testcase>]"
echo ""
echo "Options:"
echo "-c -- Compiler used for compilation (intel, pgi, gfortran)"
echo "-t -- Test case yo uwant to run, e.g.: em_real, em_quarter_ss, ..."
echo "      Type \"./compile\" to see all available test cases."
echo "-d -- Compile with debug flags (only effective if not using custom configure.wrf)"
echo "-m -- Use the configure.wrf file from the current directory instead a prepared one."
echo ""
echo "Default:"
echo "     ./build_wrf.yellowstone.bash -c intel64 -t em_real"
echo ""
echo "If -c and -t are not specified Intel is used as default compiler and em_real"
echo "as the default test case. By default WRF-CLUBB is compiled with debug flags."
echo ""
echo "To watch the progress type"
echo ""
echo "     tail -f compile.log"
echo ""
echo "The user is responsible to specify a valid test case (e.g. em_real, ...)!"
echo ""
echo "=============================================================================="
echo ""
}

error_msg_1()
{
echo "------------------------------------------------------------------------------------"
echo "Error: You have to specify one of the following options:"
echo "       -G -- group computers"
echo "       -Y -- Yellowstone"
echo "       -H -- HD1"
echo ""
echo "Use the -h option for help"
echo "------------------------------------------------------------------------------------"
}

write_compiler_info()
{
   echo '#!/bin/bash' > $COMPILER_INFO
   echo '### WRF Scripts Compiler Info ###' >> $COMPILER_INFO
   echo 'export WRF_SCRIPTS_COMPILER='$compiler >> $COMPILER_INFO
   chmod 755 $COMPILER_INFO
}

read_install_info()
{
   source $INSTALL_INFO
   comp_type=$WRF_SCRIPTS_CT
   bin_dir=$WRF_SCRIPTS_BIN
   script_dir=$WRF_SCRIPTS_SCR

   if [ $verbose = true ]; then

      echo "====== WRF scripts installation details:"
      echo ""
      echo "==== comp_type:"$comp_type
      echo "==== bin_dir:"$bin_dir
      echo "==== script_dir:"$script_dir

   fi

   # Set flags corresponding to comp_type
   groupcomp=false
   yellowstone=false
   hd1=false

   if [ "$comp_type" = "group_comp" ]; then
      groupcomp=true
   elif [ "$comp_type" = "yellowstone" ]; then
      yellowstone=true
   elif [ "$comp_type" = "hd1" ]; then
      hd1=true
   fi
   
}

config_group_comp()
{
   if [ $verbose = true ]; then
      echo "==== Group computer configuration ===="
   fi

   ### Group computer specific config 
   comp_name=$(hostname -s)
   
   if [ "$compiler" = "intel64" ]; then

      MPICH_PATH=$IFORT_MPICH
      export PATH=$MPICH_PATH:$PATH

   elif [ "$compiler" = "pgi" ]; then

      MPICH_PATH=$PGIFORTRAN_MPICH
      export PATH=$MPICH_PATH:$PATH

   elif [ "$compiler" = "gfortran" ]; then

      echo "The use of gfort on our group computers is not yet supported!"
      exit;

   fi

} # config_group_comp()

compile_hd1()
{

  ### Replace the placeholders in the templates by actual parameters
  sed 's/<CASE>/'$wrf_case'/' < $script_dir/templates/build_wrf.hd1.sbatch.temp > $script_dir/build_wrf.hd1.sbatch

  ### Compile WRF
  sbatch $script_dir/build_wrf.hd1.sbatch

  ### Cleanup
  rm -f $script_dir/build_wrf.hd1.sbatch

} # compile_hd1()

#################  Defaults and Parameters  #################

### Defaults
parallel=true
compiler="intel64"
wrf_case="em_real"
debug=false
manual=false
groupcomp=false
yellowstone=false
hd1=false
verbose=false

### Pieces of the configure.wrf preset file name
CONFIG_PREFIX="configure.wrf"
CONFIG_DEBUG=".debug"
CONFIG_CLUBB=".clubb"
CONFIG_PAR=".dmpar"
CONFIG_CT=""

### This variable must be consistent with the ones in install_wrf_scripts.bash, build_wrf.bash 
### and run_wrf.bash
INSTALL_INFO_DIR=~/.wrf_scripts_info
INSTALL_INFO=$INSTALL_INFO_DIR/wrf_scripts_info.bash
COMPILER_INFO=$INSTALL_INFO_DIR/wrf_scripts_compile_info.bash

### Defaults for the scripts and the bin directory (overwritten by read_install_info())
bin_dir=~/bin
script_dir=~/wrf_scripts



#################  Parse Options  #################
while getopts hvGYHsmdc:t: opt
do
  case "$opt" in
    G) groupcomp=true;;
    Y) yellowstone=true;;
    H) hd1=true;;
    s) parallel=false;;
    c) compiler="$OPTARG";;
    t) wrf_case="$OPTARG";;
    d) debug=true;;
    m) manual=true;; 
    v) verbose=true;;
    h) myhelp; exit;;
    \?) echo "Error: Unknown option."; myhelp; exit;;
  esac
done

#################  Begin Code  #################

### Switch to current directory
cd $PWD

echo "========   Building WRF ...   ========"

### Load setup from INSTALL_INFO
if [ -f $INSTALL_INFO ]; then
   read_install_info
fi

#################  Sanity Checks  #################

if [ "$compiler" != "intel64" ] && [ "$compiler" != "pgi" ] && [ "$compiler" != "gfortran" ]; then 
  echo ""
  echo "Error: The compiler option $compiler not valid!"
  echo ""
  echo "Choose one of the compiler options:"
  echo "    * intel64"
  echo "    * pgi"
  echo "    * gfortran"
  echo ""
  myhelp
  exit 1
fi

if [[ -f configure.wrf ]] && [ $manual = false ]; then
  echo ""
  echo "Error: An existing configure.wrf was found!"
  echo ""
  echo "You have chosen not to use a custom configure.wrf file but the current"
  echo "directory contains a configure.wrf file. To prevent accidentally overwriting"
  echo "this file this script stops execution."
  echo ""
  echo "If you intent to compile with the existing custom configure.wrf file run this"
  echo "this script with the -m option:"
  echo ""
  echo "    ./build_wrf.yellowstone.bash -m"
  echo ""
  echo "If you do not intent to run with the custom configure.wrf file just delete"
  echo "the configure.wrf file and rerun this script."
  echo ""
  exit 1
fi 

if [ $groupcomp = false ] && [ $yellowstone = false ] && [ $hd1 = false ]; then
   error_msg_1
   exit;
fi


#################  Prepare configure.wrf  #################

# Use a prepared configure.wrf file
if [ $manual = false ]; then

   ### Figure out which config file to use
   if [ $groupcomp = true ]; then
      CONFIG_CT=""
   elif [ $yellowstone = true ]; then
      CONFIG_CT=".yellowstone"
   elif [ $hd1 = true ]; then
      CONFIG_CT=".hd1"
   fi

   if [ $parallel = true ]; then
      CONFIG_PAR=".dmpar"
   else
      CONFIG_PAR=".serial"
   fi

   if [ $debug = true ]; then
      CONFIG_DEBUG=".debug"
   else
      CONFIG_DEBUG=""
   fi

   CONFIG_FILE=$CONFIG_PREFIX"."$compiler""$CONFIG_PAR""$CONFIG_CT""$CONFIG_CLUBB""$CONFIG_DEBUG

   ### copy selcted config file to configure.wrf if existent
   if [ ! -f custom_config_files/$CONFIG_FILE ]; then
      echo "Error: Cannot find configuration: custom_config_files/"$CONFIG_FILE
   else
      echo "Using config file: custom_config_files/"$CONFIG_FILE
      cp custom_config_files/$CONFIG_FILE configure.wrf
   fi

else # Use the existing configure.wrf file

  #Check if configure.wrf file exists
  if [ ! -f configure.wrf ]; then
    echo ""
    echo "Error: No configure.wrf found!!!"
    echo ""
    echo "Provide a suitable configure.wrf! You can copy one from custom_config_files. Or run"
    echo "this script without the -m option if you do not intent to compile with a custom"
    echo "configure.wrf file."  
    echo ""
    exit 1;
  fi
fi # Use existing configure.wrf or not

#################  Computer specific configurations  #################

if [ $groupcomp = true ]; then
   
   config_group_comp

elif [ $yellowstone = true ]; then


   if [ $verbose = true ]; then
      echo "==== Yellowstone configuration ===="
   fi

   # Intel
   if [ "$compiler" = "intel64" ]; then
      module reset
      module load intel
      module load mkl
   fi

   # PGI
   if [ "$compiler" = "pgi" ]; then
      # Since the Intel compiler is loaded by default we have to swap the module
      module reset
      module swap intel pgi
   fi

   # GFortran
   if [ "$compiler" = "gfortran" ]; then
     # Since the Intel compiler is loaded by default we have to swap the module
     module reset
     module swap intel gnu
     module load lapack
   fi

elif [ $hd1 = true ]; then

   if [ $verbose = true ]; then
      echo "==== HD1 configuration ===="
   fi

   compile_hd1

fi 


if [ $hd1 = false ]; then
   #################  Parallel Compiling  #################
   # Yellowstone's manual suggest to not use more then 8 core for compilation on 
   # the head node. This pretty sufficient for us.
   if [ $parallel = true ]; then

      if [ $verbose = true ]; then
         echo "== Compiling in parallel using 8 cores."
      fi

      export J='-j 8'

   else # compiling in serial

      if [ $verbose = true ]; then
         echo "== Compiling in serial."
      fi

   fi # $parallel = true

   ################# Compiling  #################
   ./compile $wrf_case &> compile.log &

fi # $hd1 = false   

# Write the compiler info to file (needed for run_wrf.bash)
write_compiler_info

echo ""
echo "== Compilation started in background ..."
echo ""
echo "== To monitor the progress type:"
echo "   tail -f compile.log"
echo ""

#==============================================================================


