#!/bin/bash

#$Id: install_wrf_scripts.bash 688 2014-06-04 22:31:55Z janhft@uwm.edu $

#################  Function Definitions  #################

myhelp()
{ 
echo "------------------------------------------------------------------------------------"
echo "This bash script was created to install a variety of useful scripts to work with WRF."
echo "You need to have a .profile file in your home directory, which includes the following line:"
echo "export "'$PATH'"=~/bin:.:"'$PATH'
echo "See http://www2.cisl.ucar.edu/resources/yellowstone/access_userenvironment/startfiles for help."
echo ""
echo "You have to specify one of the following options:"
echo "       -G -- group computers"
echo "       -Y -- Yellowstone"
echo "       -H -- HD1"
echo ""
echo "However this script is not really generic and might need adjustment, when you try to install"
echo "the scripts on another machine than Yellowstone. Also if there are new scripts developed for"
echo "the usage of WRF, they should be included in this script."
echo ""
echo "NOTE: To update the scripts switch to your <wrf_scripts> directory (default: ~/wrf_scripts)"
echo "and run the command:"
echo "svn update"
echo ""
echo "NOTE: This script only works on Yellowstone right now."
echo ""
echo "Dependencies: none"
echo "" 
echo "Usage: ./install_wrf_scripts_yellowstone.bash -H|G|Y [-hf] [-s <script_dir>] [-b <bin_dir>]"
echo ""
echo "Options:------------------"
echo "-h -- show this page"
echo "-f -- overwrite script directory"
echo "-s -- set script directory to <script_dir> (script_dir w/o " '"/"' " in the end)"
echo "-b -- set bin directory to <bin_dir> (bin_dir w/o " '"/"' " in the end)"
echo "---------------------------"
echo "-G -- group computer install"
echo "-Y -- Yellowstone install"
echo "-H -- HD1 install"
echo "---------------------------"
echo "-u -- Update the WRF scripts (svn update)"
echo "-c -- Clean up the WRF scripts (uninstall)"
echo ""
echo "------------------------------------------------------------------------------------"
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

update()
{
## read script dir etc
echo "== Updating $script_dir .."
svn update $script_dir
echo "You might want to run: install_wrf_scripts again!"
}

clean_up()
{
echo "== Cleaning up $script_dir ..."
rm -rf $script_dir

echo "== Cleaning up $bin_dir/$BUILDWRF_NAME ..."
rm -f $bin_dir/$BUILDWRF_NAME

echo "== Cleaning up $bin_dir/$RUNWRF_NAME ..."
rm -f $bin_dir/$RUNWRF_NAME

echo "== Cleaning up $bin_dir/$MKRUN_NAME ..."
rm -f $bin_dir/$MKRUN_NAME

echo "== Cleaning up $bin_dir/$CO_WRF_NAME ..."
rm -f $bin_dir/$CO_WRF_NAME

echo "== Cleaning up $bin_dir/$INSTALL_WRF_SCRIPTS_NAME ..."
rm -f $bin_dir/$INSTALL_WRF_SCRIPTS_NAME

INSTALL_INFO_DIR=${INSTALL_INFO%/*}"/"
echo "== Cleaning up $INSTALL_INFO_DIR ..."
rm -rf $INSTALL_INFO_DIR

## (5) ## If you added more scripts, add the clean up here.
}

link_scripts()
{
### Create softlinks that will be included in the PATH variable
echo "== Installing $bin_dir/$INSTALL_WRF_SCRIPTS_NAME ..."
ln -s $script_dir/$INSTALL_WRF_SCRIPTS $bin_dir/$INSTALL_WRF_SCRIPTS_NAME

echo "== Installing $bin_dir/$BUILDWRF_NAME ..."
ln -s $script_dir/$BUILDWRF $bin_dir/$BUILDWRF_NAME

echo "== Installing $bin_dir/$RUNWRF_NAME ..."
ln -s $script_dir/$RUNWRF $bin_dir/$RUNWRF_NAME

echo "== Installing $bin_dir/$MKRUN_NAME ..."
ln -s $script_dir/$MKRUN $bin_dir/$MKRUN_NAME

echo "== Installing $bin_dir/$CO_WRF_NAME ..."
ln -s $script_dir/$CO_WRF $bin_dir/$CO_WRF_NAME

## (1) ## Additional scripts go here. Config in lines (2) and (3). Setup in line (4). 
       ## Clean up in line (5). You'll get the idea.
}

write_install_info()
{

   if [ ! -d ~/.wrf_scripts_info ]; then
      mkdir ~/.wrf_scripts_info
   fi 

   if [ $groupcomp = true ]; then
      comp_type="group_comp"
   elif [ $yellowstone = true ]; then
      comp_type="yellowstone"
   elif [ $hd1 = true ]; then
      comp_type="hd1"
   fi

   echo '#!/bin/bash' > $INSTALL_INFO
   echo '### WRF Scripts Installation ###' >> $INSTALL_INFO
   echo 'export WRF_SCRIPTS_BIN='$bin_dir >> $INSTALL_INFO
   export WRF_SCRIPTS_BIN=$bin_dir
   echo 'export WRF_SCRIPTS_SCR='$script_dir >> $INSTALL_INFO
   export WRF_SCRIPTS_SCR=$script_dir
   echo 'export WRF_SCRIPTS_CT='$comp_type >> $INSTALL_INFO
   export WRF_SCRIPTS_CT=$comp_type
   chmod 755 $INSTALL_INFO
}

read_install_info()
{
   source $INSTALL_INFO
   comp_type=$WRF_SCRIPTS_CT
   bin_dir=$WRF_SCRIPTS_BIN
   script_dir=$WRF_SCRIPTS_SCR
   echo "====== WRF scripts installation details:"
   echo ""
   echo "==== comp_type:"$comp_type
   echo "==== bin_dir:"$bin_dir
   echo "==== script_dir:"$script_dir

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

update_profile()
{
   ### Add bin_dir to PATH variable (in ~/.profile) if not already included
   if grep -q 'export PATH='$bin_dir ~/.profile; then
      echo "bin_dir is already included in PATH variable."
   else
      echo 'export PATH='$bin_dir':$PATH' >> ~/.profile   
   fi

   echo "====== PATH variable updated (in ~/.profile)... ======"
   echo ""

   export PATH=$bin_dir/":$PATH"
}

#################  Defaults and Parameters  #################

### Defaults
script_dir=~/wrf_scripts
bin_dir=~/bin
yellowstone=false
groupcomp=false
hd1=false
force=false
cleanup=false
update=false
comp_type="group_comp"

### This variable must be consistent with the ones in install_wrf_scripts.bash, build_wrf.bash 
### and run_wrf.bash
INSTALL_INFO=~/.wrf_scripts_info/wrf_scripts_info.bash

### Scripts configurations for different architectures 
### TODO: This part might be unnecessary!
RUNWRF_GC=run_wrf.bash
BUILDWRF_GC=build_wrf.bash
RUNWRF_HD1=run_wrf.bash
BUILDWRF_HD1=build_wrf.bash
RUNWRF_YS=run_wrf.bash
BUILDWRF_YS=build_wrf.bash
## (2) ## Put the name of the scripts on the specific compurters here.
       ## The script configurations are the same forall computers right now,
       ## since all those scripts are generic enough. But it is possible to link
       ## different scripts on different computers by setting it up here.


### Scripts to be linked
CO_WRF=checkout_wrf_clubb.sh
MKRUN=mkrun.bash
RUNWRF=$RUNWRF_GC
BUILDWRF=$BUILDWRF_GC
INSTALL_WRF_SCRIPTS=install_wrf_scripts.bash
## (3) ## Here go all the script names of the scripts in $script_dir that you want to link..

### Link target names
CO_WRF_NAME=checkout_wrf_clubb
BUILDWRF_NAME=build_wrf
RUNWRF_NAME=run_wrf
MKRUN_NAME=mkrun
INSTALL_WRF_SCRIPTS_NAME=install_wrf_scripts
## (3) ## .. and the here all the link target names for the scripts.

#################  Begin Code  #################

### parse options
while getopts hfcuGYHs:b: opt
do
  case "$opt" in
    G) groupcomp=true;;
    Y) yellowstone=true;;
    H) hd1=true;;
    f) force=true;;
    s) script_dir="$OPTARG";;
    b) bin_dir="$OPTARG";;
    c) cleanup=true;;
    u) update=true;;
    h) myhelp; exit;;
    \?) echo "Error: Unknown option."; myhelp; exit;;
  esac
done

### Install scripts
if [ $cleanup = false ] && [ $update = false ]; then 

   echo "======== WRF Scripts Installation started.. ========"
   echo ""

   ### Check if WRF scripts already installed
   if [ -f $INSTALL_INFO ]; then
      echo "Error: WRF scripts already installed. Run install_wrf_scripts -c for cleanup."
      exit;
   fi

   ### Check if a computer type has been specified
   if [ $groupcomp = false ] && [ $yellowstone = false ] && [ $hd1 = false ]; then
      error_msg_1
      exit;
   fi

   ### Check out the WRF scripts
   if [ -d $script_dir ]; then
   
      if [ $force = true ]; then
         rm -rf $script_dir
         svn co http://carson.math.uwm.edu/repos/wrf/trunk/WRF/scripts $script_dir >& wrf_scripts_co.log
      else
         echo "Error: $script_dir already exists. Specify -f to overwrite the existing directory."
         exit;
      fi

   else # script_dir does not exist
      svn co http://carson.math.uwm.edu/repos/wrf/trunk/WRF/scripts $script_dir >& wrf_scripts_co.log
   fi # -d $script_dir

   echo "====== The WRF scripts have been checked out to the following location: $script_dir ======"
   echo ""

   ### Create private bin directory if not existent
   if [ ! -d $bin_dir ]; then
      mkdir $bin_dir
   fi
   echo "====== Installing executables to: $bin_dir ======="
   echo ""

   ### Update ~/.profile
   update_profile

   ### Write installation info
   write_install_info
   
### Clean up scripts
elif [ $cleanup = true ] && [ $update = false ]; then 

   echo "======== WRF Scripts Clean up started.. ========"
   echo ""

   if [ -f $INSTALL_INFO ]; then
      read_install_info
   else
      echo "Error: WRF scripts installation not found!"
      exit;
   fi


### Update scripts
elif [ $cleanup = false ] && [ $update = true ]; then 

   echo "======== WRF Scripts Update started.. ========"
   echo ""

   if [ -f $INSTALL_INFO ]; then
      read_install_info
   else
      echo "Error: WRF scripts installation not found!"
      exit;
   fi

else

   echo "Error: You can not specify the -c and -u option together!"
   exit;

fi # [ $cleanup = false ] && [ $update = false ]



#################  Setup for different computer types  #################

## (4) ## Setup for all scripts on the specified computer type
if [ $groupcomp = true ]; then
   
   echo "==== Group Computer installation ===="
   echo ""
   comp_type="group_comp"
   RUNWRF=$RUNWRF_GC
   BUILDWRF=$BUILDWRF_GC

elif [ $yellowstone = true ]; then

   echo "==== Yellowstone installation ===="
   echo ""
   comp_type="yellowstone"
   RUNWRF=$RUNWRF_YS
   BUILDWRF=$BUILDWRF_YS

elif [ $hd1 = true ]; then

   echo "==== HD1 installation ===="
   echo ""
   comp_type="hd1"
   RUNWRF=$RUNWRF_HD1
   BUILDWRF=$BUILDWRF_HD1

fi # $groupcomp = true



#################  Installation / Clean up / Update  #################

### Clean up if -c was specified
if [ $cleanup = true ]; then 
   clean_up
   echo ""
   echo "======== WRF Scripts Clean up finished. ========"
   exit;

### Update if -u was specified
elif [ $update = true ]; then
   update
   echo ""
   echo "======== WRF Scripts Update finished. ========"
   exit;

### Otherwise install
else
   link_scripts
   echo '== ls -lisa '$bin_dir
   ls -lisa $bin_dir
   # Here maybe call a little cleanup routine for unused scripts in $script_dir
   echo ""
   echo "======== WRF Scripts Installation finished. ========"
fi

exit;
