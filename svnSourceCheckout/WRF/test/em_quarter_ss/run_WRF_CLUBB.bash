#!/bin/bash

###############################################################################
# run_WRF_CLUBB.bash
#
# This script runs WRF_CLUBB cases.  You simply specify what cases and it will
# run them and move the clubb output into clubb_output.  This script can also
# be used for the nightly test to generate the output for WRF_CLUBB.
#
# Notes:
# Arguments
# -arm, -atex, -bomex, -fire, -wangara, -all depending on which cases you want
# to run.
# -nightly to run in nightly mode
###############################################################################


#Set up all of the booleans to be false
ARM=false
ATEX=false
BOMEX=false
FIRE=false
WANGARA=false
ALL=false


NIGHTLY_OUT="/usr/nightly_tests/nightly_tests/output"
OUTPUT_FOLDER="clubb_output"

# This function reads all the arguments and sets variables that will be used
# later in the script and also passed to other scripts.
set_args()
{
        # Loop through the list of arguments ($1, $2...). This loop ignores
        # anything not starting with '-'.
        while [ -n "$(echo $1 | grep "-")" ]; do
                case $1 in
                        -nightly ) NIGHTLY=true ;;
			 -arm ) ARM=true ;;
			 -atex ) ATEX=true ;;
			 -bomex ) BOMEX=true ;;
			 -fire ) FIRE=true ;;
			 -wangara ) WANGARA=true ;;
			 -all ) ALL=true ;;
                        # Handles help, and any unknown argument
                        -help | -h | -? | * ) echo "usage: script_name [ARGS]"
                                              echo "Arguments:"
					      echo -e " -arm    \t to run the ARM case."
					      echo -e " -atex   \t to run the ATEX case."
					      echo -e " -bomex  \t to run the BOMEX case."
					      echo -e " -fire   \t to run the FIRE case."
					      echo -e " -wangara\t to run the Wangara case."
					      echo -e " -all    \t to run all of the cases."
					      echo -e " -nightly\t to run in nightly_test mode."
                                              exit 1 ;;
                esac
                # Shift moves the parameters up one. Ex: $2 -> $1 and so on.
                # This is so we only have to check $1 on each iteration.
                shift
        done

}

if [ $# == 0 ] ; then
	echo
	echo "Whoops, you must have at least one argument!"
	echo " Type  ./run_WRF_CLUBB.bash -help  for a list of valid arguments."
	exit -1
fi

# Call set_args() and pass all the arguments that were set when the script was run
set_args $*

# Start logging
echo "[`date +"%d %b %Y"` `date +%r`] *** Script started (run_WRF_CLUBB.bash)" 

if [ "$NIGHTLY" = true ] ; then
        OUTPUT_FOLDER="WRF_CLUBB_curr"
	#Try making the directories as a failsafe, no harm if they already exist
        mkdir WRF_CLUBB_prev
        mkdir $NIGHTLY_OUT/WRF_CLUBB_prev
        mkdir $NIGHTLY_OUT/WRF_CLUBB_curr

        # Remove any previous data to prevent spurious profiles
        rm WRF_CLUBB_prev/*
        rm $NIGHTLY_OUT/WRF_CLUBB_prev/*

        mv $NIGHTLY_OUT/WRF_CLUBB_curr/* $NIGHTLY_OUT/WRF_CLUBB_prev
fi

mkdir $OUTPUT_FOLDER

# Remove WRF_CLUBB.output if it already exists.  Otherwise if this script is run multiple times,
# the output will just keep getting added to the end of the previous WRF_CLUBB.output file.
if [ -e WRF_CLUBB.output ] ; then
	rm WRF_CLUBB.output
fi

if [[ "$ARM" = true || "$ALL" = true ]] ; then
	# Make the forcing files we need
	ncl make_ARM_forcing.ncl >> WRF_CLUBB.output

	# Copy namelist and input souding for ARM
	cp -f namelist.input.arm_clubb namelist.input
	cp -f input_sounding_arm input_sounding

	# RUN WRF
	./ideal.exe >> WRF_CLUBB.output
	echo "Running ARM"
	./wrf.exe >> WRF_CLUBB.output

	# Move output and change the name, modify ctl files to reflect the changed name

	mv clubb_sfc.ctl $OUTPUT_FOLDER/arm_sfc_wrf.ctl
	mv clubb_sfc.dat $OUTPUT_FOLDER/arm_sfc_wrf.dat
	mv clubb_zm.ctl $OUTPUT_FOLDER/arm_zm_wrf.ctl
	mv clubb_zm.dat $OUTPUT_FOLDER/arm_zm_wrf.dat
	mv clubb_zt.ctl $OUTPUT_FOLDER/arm_zt_wrf.ctl
	mv clubb_zt.dat $OUTPUT_FOLDER/arm_zt_wrf.dat

	sed -i 's|DSET ^clubb_sfc.dat|DSET ^arm_sfc_wrf.dat|' $OUTPUT_FOLDER/arm_sfc_wrf.ctl
	sed -i 's|DSET ^clubb_zm.dat|DSET ^arm_zm_wrf.dat|' $OUTPUT_FOLDER/arm_zm_wrf.ctl
	sed -i 's|DSET ^clubb_zt.dat|DSET ^arm_zt_wrf.dat|' $OUTPUT_FOLDER/arm_zt_wrf.ctl
fi

if [[ "$ATEX" = true || "$ALL" = true ]] ; then
	# Make the forcing files we need
	ncl make_ATEX_forcing.ncl >> WRF_CLUBB.output

	# Copy namelist and input souding for ATEX
	cp -f namelist.input.atex_clubb namelist.input
	cp -f input_sounding_atex input_sounding

	# RUN WRF
	./ideal.exe >>WRF_CLUBB.output
	echo "Running ATEX"
	./wrf.exe >> WRF_CLUBB.output

	# Move output and change the name, modify ctl files to reflect the changed name

	mv clubb_sfc.ctl $OUTPUT_FOLDER/atex_sfc_wrf.ctl
	mv clubb_sfc.dat $OUTPUT_FOLDER/atex_sfc_wrf.dat
	mv clubb_zm.ctl $OUTPUT_FOLDER/atex_zm_wrf.ctl
	mv clubb_zm.dat $OUTPUT_FOLDER/atex_zm_wrf.dat
	mv clubb_zt.ctl $OUTPUT_FOLDER/atex_zt_wrf.ctl
	mv clubb_zt.dat $OUTPUT_FOLDER/atex_zt_wrf.dat

	sed -i 's|DSET ^clubb_sfc.dat|DSET ^atex_sfc_wrf.dat|' $OUTPUT_FOLDER/atex_sfc_wrf.ctl
	sed -i 's|DSET ^clubb_zm.dat|DSET ^atex_zm_wrf.dat|' $OUTPUT_FOLDER/atex_zm_wrf.ctl
	sed -i 's|DSET ^clubb_zt.dat|DSET ^atex_zt_wrf.dat|' $OUTPUT_FOLDER/atex_zt_wrf.ctl
fi

if [[ "$BOMEX" = true || "$ALL" = true ]] ; then
	# Make the forcing files we need
	ncl make_BOMEX_forcing.ncl >> WRF_CLUBB.output

	# Copy namelist and input souding for BOMEX
	cp -f namelist.input.bomex_clubb namelist.input
	cp -f input_sounding_bomex input_sounding

	# RUN WRF
	./ideal.exe >> WRF_CLUBB.output
	echo "Running BOMEX"
	./wrf.exe >> WRF_CLUBB.output

	# Move output and change the name, modify ctl files to reflect the changed name

	mv clubb_sfc.ctl $OUTPUT_FOLDER/bomex_sfc_wrf.ctl
	mv clubb_sfc.dat $OUTPUT_FOLDER/bomex_sfc_wrf.dat
	mv clubb_zm.ctl $OUTPUT_FOLDER/bomex_zm_wrf.ctl
	mv clubb_zm.dat $OUTPUT_FOLDER/bomex_zm_wrf.dat
	mv clubb_zt.ctl $OUTPUT_FOLDER/bomex_zt_wrf.ctl
	mv clubb_zt.dat $OUTPUT_FOLDER/bomex_zt_wrf.dat

	sed -i 's|DSET ^clubb_sfc.dat|DSET ^bomex_sfc_wrf.dat|' $OUTPUT_FOLDER/bomex_sfc_wrf.ctl
	sed -i 's|DSET ^clubb_zm.dat|DSET ^bomex_zm_wrf.dat|' $OUTPUT_FOLDER/bomex_zm_wrf.ctl
	sed -i 's|DSET ^clubb_zt.dat|DSET ^bomex_zt_wrf.dat|' $OUTPUT_FOLDER/bomex_zt_wrf.ctl
fi


if [[ "$FIRE" = true || "$ALL" = true ]] ; then

	# Make the forcing files we need
	ncl make_FIRE_forcing.ncl >> WRF_CLUBB.output

	# Copy namelist and input sounding for FIRE
	cp -f namelist.input.fire_clubb namelist.input
	cp -f input_sounding_fire input_sounding

	# Run WRF
	./ideal.exe >> WRF_CLUBB.output
	echo "Running FIRE"
	./wrf.exe >>WRF_CLUBB.output

	# Move output and change the name, modify ctl files to reflect the changed name

	mv clubb_sfc.ctl $OUTPUT_FOLDER/fire_sfc_wrf.ctl
	mv clubb_sfc.dat $OUTPUT_FOLDER/fire_sfc_wrf.dat
	mv clubb_zm.ctl	$OUTPUT_FOLDER/fire_zm_wrf.ctl
	mv clubb_zm.dat	$OUTPUT_FOLDER/fire_zm_wrf.dat
	mv clubb_zt.ctl	$OUTPUT_FOLDER/fire_zt_wrf.ctl
	mv clubb_zt.dat	$OUTPUT_FOLDER/fire_zt_wrf.dat

	sed -i 's|DSET ^clubb_sfc.dat|DSET ^fire_sfc_wrf.dat|' $OUTPUT_FOLDER/fire_sfc_wrf.ctl
	sed -i 's|DSET ^clubb_zm.dat|DSET ^fire_zm_wrf.dat|' $OUTPUT_FOLDER/fire_zm_wrf.ctl
	sed -i 's|DSET ^clubb_zt.dat|DSET ^fire_zt_wrf.dat|' $OUTPUT_FOLDER/fire_zt_wrf.ctl
fi

if [[ "$WANGARA" = true || "$ALL" = true ]] ; then

	# Make the forcing files we need
	ncl make_Wangara_forcing.ncl >> WRF_CLUBB.output

	# Copy namelist and input sounding for WANGARA
	cp -f namelist.input.wangara_clubb namelist.input
	cp -f input_sounding_wangara input_sounding

	# Run WRF
	./ideal.exe >> WRF_CLUBB.output
	echo "Running Wangara"
	./wrf.exe >>WRF_CLUBB.output

	# Move output and change the name, modify ctl files to reflect the changed name

	mv clubb_sfc.ctl $OUTPUT_FOLDER/wangara_sfc_wrf.ctl
	mv clubb_sfc.dat $OUTPUT_FOLDER/wangara_sfc_wrf.dat
	mv clubb_zm.ctl	$OUTPUT_FOLDER/wangara_zm_wrf.ctl
	mv clubb_zm.dat	$OUTPUT_FOLDER/wangara_zm_wrf.dat
	mv clubb_zt.ctl	$OUTPUT_FOLDER/wangara_zt_wrf.ctl
	mv clubb_zt.dat	$OUTPUT_FOLDER/wangara_zt_wrf.dat

	sed -i 's|DSET ^clubb_sfc.dat|DSET ^wangara_sfc_wrf.dat|' $OUTPUT_FOLDER/wangara_sfc_wrf.ctl
	sed -i 's|DSET ^clubb_zm.dat|DSET ^wangara_zm_wrf.dat|' $OUTPUT_FOLDER/wangara_zm_wrf.ctl
	sed -i 's|DSET ^clubb_zt.dat|DSET ^wangara_zt_wrf.dat|' $OUTPUT_FOLDER/wangara_zt_wrf.ctl
fi


# Move the results to the nightly_tests directory if -nightly was passed in
if [ "$NIGHTLY" = true ] ; then
        mv WRF_CLUBB_curr/* $NIGHTLY_OUT/WRF_CLUBB_curr/
fi

svn revert namelist.input input_sounding

echo "[`date +"%d %b %Y"` `date +%r`] *** Script ended successfully (run_WRF_CLUBB.bash)" 

