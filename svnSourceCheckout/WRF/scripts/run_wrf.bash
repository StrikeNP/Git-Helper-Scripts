#!/bin/bash

#$Id: run_wrf.bash 693 2014-06-09 23:53:59Z janhft@uwm.edu $

#################  Function Definitions  #################
myhelp()
{ 
echo "------------------------------------------------------------------------------------"
echo "This bash script runs wrf.exe in parallel on machines that are using bsub."
echo "Right now it is only suited for Yellowstone. But soon to be generalized..."
echo ""
echo "You have to specify one of the following options:"
echo "       -G -- group computers"
echo "       -Y -- Yellowstone"
echo "       -H -- HD1"
echo ""
echo "Notes: If neither of -q or -Q are used, the job will be submitted via the regular queue"
echo "(see http://www2.cisl.ucar.edu/resources/yellowstone/using_resources/queues_charges). "
echo "The wall clock time <numproc> has to be in the format [h]h:mm where the hours can range from "
echo "1 up to 12 (see link above for more details)."
echo ""
echo "Dependencies: bsub"
echo "" 
echo "Usage: ./run_wrf_bsub.bash [-c <compiler>] [-n <jobname>] [-p <numproc>] [-t <runtime>] [-q|Q] [-h]"
echo ""
echo "Options:"
echo "-h -- show this page"
echo "-n -- use job name <jobname>"
echo "-p -- use <numproc> processors"
echo "-c -- use libraries corresponding to <compiler>"
echo "-t -- set wall clock time to <runtime>"
echo "-q -- use economy queue (Yellowstone only)"
echo "-Q -- use premium queue (Yellowstone only)"
echo "-Y -- run the script on Yellowstone"
echo "-H -- run the script on HD1"
echo "-G -- run the script on group computer (default)"
echo "------------------------------------------------------------------------------------"
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
      echo "==== comp_type: "$comp_type
      echo "==== bin_dir: "$bin_dir
      echo "==== script_dir: "$script_dir

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

read_compiler_info()
{
   source $COMPILER_INFO
   compiler=$WRF_SCRIPTS_COMPILER
   echo "==== compiled with: "$compiler

   
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

run_yellowstone()
{

   # Replace placeholders within the templates by the actual parameters
   sed 's/<PNR>/'$pjnumber'/' < $script_dir/templates/run_wrf.yellowstone.bsub.temp > $script_dir/run_wrf.yellowstone.bsub.tmp
  
   sed 's/<NUMPROC>/'$numproc'/' < $script_dir/run_wrf.yellowstone.bsub.tmp > $script_dir/run_wrf.yellowstone.bsub
   mv $script_dir/run_wrf.yellowstone.bsub $script_dir/run_wrf.yellowstone.bsub.tmp

   sed 's/<RUNTIME>/'$runtime'/' < $script_dir/run_wrf.yellowstone.bsub.tmp > $script_dir/run_wrf.yellowstone.bsub
   mv $script_dir/run_wrf.yellowstone.bsub $script_dir/run_wrf.yellowstone.bsub.tmp

   sed 's/<QUEUE>/'$queue'/' < $script_dir/run_wrf.yellowstone.bsub.tmp > $script_dir/run_wrf.yellowstone.bsub

   # Run WRF
   bsub < $script_dir/run_wrf.yellowstone.bsub

   # Cleanup
   rm -f $script_dir/run_wrf.yellowstone.bsub

   exit;

} # run_yellowstone

run_hd1()
{
   # Replace placeholders within the templates by the actual parameters
   sed 's/<NUMPROC>/'$numproc'/' < $script_dir/templates/run_wrf.hd1.sbatch.temp > $script_dir/run_wrf.hd1.sbatch

   # Run WRF
   sbatch $script_dir/run_wrf.hd1.sbatch

   # Cleanup
   rm -f $script_dir/run_wrf.hd1.sbatch

}

#################  Defaults and Parameters  #################
jobname="wrf_run"
numproc=0 # The defaults will be set depending on the machine
runtime="6:00"
runfilename="wrf.run.bsub"
queue="regular"
yellowstone=false
groupcomp=false
hd1=false
pjnumber="P36741010"
verbose=false
compiler="intel64"

script_dir=~/wrf_scripts
bin_dir=~/bin

### This variable must be consistent with the ones in install_wrf_scripts.bash, build_wrf.bash 
### and run_wrf.bash
INSTALL_INFO_DIR=~/.wrf_scripts_info
INSTALL_INFO=$INSTALL_INFO_DIR/wrf_scripts_info.bash
COMPILER_INFO=$INSTALL_INFO_DIR/wrf_scripts_compile_info.bash


#################  Parse Options  #################
while getopts hqQYGHn:p:t:c: opt
do
  case "$opt" in
    Y) yellowstone=true;;
    G) group_comp=true;;
    H) hd1=true;;
    n) jobname="$OPTARG";;
    c) compiler="$OPTARG";;
    p) numproc=$OPTARG;;
    t) runtime="$OPTARG";;
    q) queue="economy";;
    Q) queue="premium";; 
    v) verbose=true;;
    h) myhelp; exit;;
    \?) echo "Error: Unknown option."; myhelp; exit;;
  esac
done

#################  Begin Code  #################

### Switch to current directory
cd $PWD

### Load setup from INSTALL_INFO
if [ -f $INSTALL_INFO ]; then
   read_install_info
fi

### Load compiler info
if [ -f $COMPILER_INFO ]; then
   read_compiler_info
fi

### Assertion Checks
if [ ! -f wrf.exe ]; then
   echo "Error: wrf.exe file not found in current directory! Please (re-)compile your code or "
   echo "switch to the correct directory!"
   exit;
fi

if [ ! -f wrfinput_d01 ] || [ ! -f wrflowinp_d01 ] || [ ! -f wrfbdy_d01 ]; then
   echo "Error: Missing boundary data in current directory! One or more of the following files are"
   echo "missing:"
   echo " *wrfinput_d01"
   echo " *wrflowinp_d01"
   echo " *wrfbdy_d01"
   exit;
fi

if [ ! -f namelist.input ]; then
   echo "Error: namelist.input file not found in current directory!"
   exit;
fi

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

### Prepare and run 
if [ $groupcomp = true ]; then

   runfilename="run_wrf_atscript.bash"

   config_group_comp

   echo "==== Group computer run ===="
   echo ""
   echo "== Details: =="

   # Default (group computers) for numproc
   if [ $numproc = 0 ]; then
      numproc=1
      echo ""
      echo "Hint: Use -p <numproc> option to specify the # of procs."
      echo ""

   elif [ "$numproc" -gt "16" ]; then
     
      echo ""
      echo "Error: Can't set # of procs. greater than 16 on group computers."
      echo ""

      numproc=16
   fi

   echo "== # of procs: $numproc"

   if [ $numproc = 1 ]; then

      echo "== Running WRF_CLUBB in serial (see wrf.log) =="

      ./wrf.exe >& wrf.log &

   else # numproc in interval [2,16]

      echo "== Running WRF_CLUBB in parallel =="   

      mpiexec -n $numproc ./wrf.exe >& /dev/null &

   fi

   echo "To monitor the progress run:"
   echo "tail -f rsl.*"   
   
elif [ $yellowstone = true ]; then
  
   echo "==== Yellowstone run ===="
   echo ""
   echo "== Details: =="
   echo "== jobname: $jobname"
   echo "== project#: $pjnumber"

   # Default (yellowstone) for numproc
   if [ $numproc = 0 ]; then
      numproc=128
      echo "== # of procs. (default): $numproc"
      echo "Hint: Use -p <numproc> option to specify the # of procs."
   else
      echo "== # of procs: $numproc"
   fi

   echo "== runtime: $runtime"
   echo "== queue: $queue"

   run_yellowstone

   echo "To check the job queue use: bjobs "
   echo "To monitor the progress use:"
   echo "tail -f rsl.*"   

elif [ $hd1 = true ]; then

   echo "====    HD1 run    ===="
   echo ""
   echo "== Details: =="

   # Default (HD1) for numproc
   if [ $numproc = 0 ]; then
      numproc=64
      echo "== # of procs. (default): $numproc"
      echo "Hint: Use -p <numproc> option to specify the # of procs."
   else
      echo "== # of procs: $numproc"
   fi

   run_hd1

   echo "To check the job queue use: squeue "
   echo "To monitor the progress use:"
   echo "tail -f rsl.*"

fi
