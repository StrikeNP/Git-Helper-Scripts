#!/bin/bash
#
# $Id$
#

COMPILER="intel"
CASE="em_real"
DEBUG=true
MANUAL=false
PARA=false


### parse options
while getopts hmnpc:t: opt
do
  case "$opt" in
    c) COMPILER="$OPTARG";;
    t) CASE="$OPTARG";;
    n) DEBUG=false;;
    m) MANUAL=true;;
    p) PARA=true;;
    h) myhelp; exit;;
    \?) echo "Error: Unknown option."; myhelp; exit;;
  esac
done

#=======================Check compiler option is valid=========================

if [ "$COMPILER" != "intel" ] && [ "$COMPILER" != "pgi" ] && [ "$COMPILER" != "gfortran" ]; then 
  echo ""
  echo "Error: The compiler option $COMPILER not valid!"
  echo ""
  echo "Choose one of the compiler options intel, pgi, gfortran."
  echo ""
  myhelp
  exit 1
fi


# Switch to current directory
cd $PWD


if [[ -f configure.wrf ]] && [ $MANUAL = false ]; then
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
  echo "    ./build_wrf.group.bash -m"
  echo ""
  echo "If you do not intent to run with the custom configure.wrf file just delete"
  echo "the configure.wrf file and rerun this script."
  echo ""
  exit 1
fi




#=======================Copy prepared configure.wrf file=======================
# Use a prepared configure.wrf file
if [ $MANUAL = false ]; then
  
  #======================================Serial================================
  if [ $PARA = false ]; then

    # Intel
    if [ "$COMPILER" = "intel" ]; then
      if [ $DEBUG = false ]; then
        cp custom_config_files/configure.wrf.intel64.serial.clubb configure.wrf 
      else
        cp custom_config_files/configure.wrf.intel64.serial.clubb.debug configure.wrf
      fi

    # PGI
    elif [ "$COMPILER" = "pgi" ]; then
      if [ $DEBUG = false ]; then
        cp custom_config_files/configure.wrf.pgi.serial.clubb configure.wrf 
      else
        cp custom_config_files/configure.wrf.pgi.serial.clubb.debug configure.wrf
      fi

   # GFortran
   elif [ "$COMPILER" = "gfortran" ]; then
      if [ $DEBUG = false ]; then
        cp custom_config_files/configure.wrf.gfortran.serial.clubb configure.wrf 
      else
        cp custom_config_files/configure.wrf.gfortran.serial.clubb.debug configure.wrf
      fi
   fi # Intel, PGI or Gfortran
 

  #=====================================Parallel==============================
  else 

    # Intel
    if [ "$COMPILER" = "intel" ]; then

      if [ $DEBUG = false ]; then
        cp custom_config_files/configure.wrf.intel64.dmpar.clubb configure.wrf 
      else
        cp custom_config_files/configure.wrf.intel64.dmpar.clubb.debug configure.wrf
      fi

    # PGI
    elif [ "$COMPILER" = "pgi" ]; then
      if [ $DEBUG = false ]; then
        cp custom_config_files/configure.wrf.pgi.dmpar.clubb configure.wrf 
      else
        cp custom_config_files/configure.wrf.pgi.dmpar.clubb.debug configure.wrf
      fi

   # GFortran
   elif [ "$COMPILER" = "gfortran" ]; then
      if [ $DEBUG = false ]; then
        cp custom_config_files/configure.wrf.gfortran.dmpar.clubb configure.wrf 
      else
        cp custom_config_files/configure.wrf.gfortran.dmpar.clubb.debug configure.wrf
      fi
   fi # Intel, PGI or Gfortran

  fi # Serial or parallel

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




#==================================Load compiler===============================

# Only load specific compiler if compiling for parallel execution
if [ $PARA = true ]; then
  ./loadCompiler.group.bash -c $COMPILER
  source compiler.source
  rm compiler.source
fi


#=============================Parallel compiling===============================
export J='-j 8'
#==============================================================================


#=================================Compiling====================================
./compile $CASE &> compile.log &

echo ""
echo "Compilation started in background ..."
echo ""
echo "To monitor the progress type:"
echo "  tail -f compile.log"
echo ""

#==============================================================================



