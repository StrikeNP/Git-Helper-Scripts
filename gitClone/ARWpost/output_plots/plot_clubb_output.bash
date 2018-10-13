#!/bin/bash
# $Id$
#===============================================================================
# Description:
# Plots CLUBB statistics.
#===============================================================================
function GrADS_profile {

# $1: Filename of GrADS *.ctl file.
# $2: Field name.
# $3: Initial time in time averaging.
# $4: Last time in time averaging.
# $5: Output file prefix name.
# $6: Label for the x-axis of the plot.
# $7: Label for the units for the x-axis of the plot.
# $8: Title of the plot.

FILENAME=$1
FIELD_NAME=$2
TIMEINIT=$3
TIMELAST=$4
OUTFILE_NAME=$5
XLABEL=$6
XLABELUN=$7
TITLE=$8

# Run the GrADS script plot_profile.gs in portrait mode in the background.
grads -p -b -c 'run plot_profile '"$FILENAME"' '"$FIELD_NAME"' '"$TIMEINIT"' '"$TIMELAST"' '"$OUTFILE_NAME"' '"$XLABEL"' '"$XLABELUN"' '"$TITLE"

# Produce a *.eps file from the GrADS file output.
gxeps -c -i "$OUTFILE_NAME".m -o "$OUTFILE_NAME".eps

# Remove the GrADS output file.
rm "$OUTFILE_NAME".m

# Produce a *.jpg file from the *.eps file.
convert "$OUTFILE_NAME".eps "$OUTFILE_NAME".jpg

}
#===============================================================================
function GrADS_surface {

# $1: Filename of GrADS *.ctl file.
# $2: Field name.
# $3: Output file prefix name.
# $4: Label for the y-axis of the plot.
# $5: Label for the units for the y-axis of the plot.
# $6: Title of the plot.

FILENAME=$1
FIELD_NAME=$2
OUTFILE_NAME=$3
YLABEL=$4
YLABELUN=$5
TITLE=$6

# Run the GrADS script plot_profile.gs in portrait mode in the background.
grads -p -b -c 'run plot_surface '"$FILENAME"' '"$FIELD_NAME"' '"$OUTFILE_NAME"' '"$YLABEL"' '"$YLABELUN"' '"$TITLE"

# Produce a *.eps file from the GrADS file output.
gxeps -c -i "$OUTFILE_NAME".m -o "$OUTFILE_NAME".eps

# Remove the GrADS output file.
rm "$OUTFILE_NAME".m

# Produce a *.jpg file from the *.eps file.
convert "$OUTFILE_NAME".eps "$OUTFILE_NAME".jpg

}
#===============================================================================

#===== MAIN =====

# Read in CLUBB data files.
# Default values.
GRADS_CLUBB_ZT=''
GRADS_CLUBB_ZM=''
GRADS_CLUBB_SFC=''
CLUBB_ZT_STATS=0
CLUBB_ZM_STATS=0
CLUBB_SFC_STATS=0

while true;
do
   case "$1" in
      --clubb-zt) # Path and filename of the CLUBB zt GrADS *.ctl stats file.
             if [ -f $2 ] && [ `echo $2 | rev | cut -d . -f 1 | rev` = 'ctl' ]; then
                GRADS_CLUBB_ZT=$2
                CLUBB_ZT_STATS=1
             else
                echo "Invalid filename entered; ignoring."
                echo "CLUBB zt GrADS stats file entered: " $2
             fi
             shift 2;;
      --clubb-zm) # Path and filename of the CLUBB zm GrADS *.ctl stats file.
             if [ -f $2 ] && [ `echo $2 | rev | cut -d . -f 1 | rev` = 'ctl' ]; then
                GRADS_CLUBB_ZM=$2
                CLUBB_ZM_STATS=1
             else
                
                echo "CLUBB zm GrADS stats file entered: " $2
             fi
             shift 2;;
      --clubb-sfc) # Path and filename of the CLUBB sfc GrADS *.ctl stats file.
             if [ -f $2 ] && [ `echo $2 | rev | cut -d . -f 1 | rev` = 'ctl' ]; then
                GRADS_CLUBB_SFC=$2
                CLUBB_SFC_STATS=1
             else
                echo "Invalid filename entered; ignoring."
                echo "CLUBB sfc GrADS stats file entered: " $2
             fi
             shift 2;;
      -h|--help) # Print help messge.
             echo -e "Usage: plot_wrf_output.bash [OPTIONS] <WRF GRADS output file>"
             exit 1;;
      --) shift; break;;
      *) break;;
   esac
done

#-------------------------------------------------------------------------------

if [ "$CLUBB_ZT_STATS" -eq 1 ]; then

    # Plot the time-averaged profile of thlm.
    GrADS_profile "$GRADS_CLUBB_ZT" thlm 0 1080 thlm "theta-l" "[K]" "thlm"

    # Plot the time-averaged profile of rtm.
    GrADS_profile "$GRADS_CLUBB_ZT" rtm 0 1080 rtm "rt" "[kg/kg]" "rtm"

    # Plot the time-averaged profile of um.
    GrADS_profile "$GRADS_CLUBB_ZT" um 0 1080 um "um" "[m/s]" "um"

    # Plot the time-averaged profile of vm.
    GrADS_profile "$GRADS_CLUBB_ZT" vm 0 1080 vm "vm" "[m/s]" "vm"

    # Plot the time-averaged profile of wp3.
    GrADS_profile "$GRADS_CLUBB_ZT" wp3 0 1080 wp3 "w'^3" "[m^3/s^3]" "wp3"

    # Plot the time-averaged profile of cloud fraction.
    GrADS_profile "$GRADS_CLUBB_ZT" cloud_frac 0 1080 cloud_frac "Cloud_Fraction" "[-]" "Cloud_Fraction"

    # Plot the time-averaged profile of rcm.
    GrADS_profile "$GRADS_CLUBB_ZT" rcm 0 1080 rcm "rcm" "[kg/kg]" "rcm"

    # Plot the time-averaged profile of Lscale.
    GrADS_profile "$GRADS_CLUBB_ZT" Lscale 0 1080 Lscale "L" "[m]" "Lscale"

    # Plot the time-averaged profile of wp2thvp.
    GrADS_profile "$GRADS_CLUBB_ZT" wp2thvp 0 1080 wp2thvp "w'^2thv'" "[(m^2/s^2)K]" "wp2thvp"

fi # $CLUBB_ZT_STATS = 1

if [ "$CLUBB_ZM_STATS" -eq 1 ]; then

    # Plot the time-averaged profile of wp2.
    GrADS_profile "$GRADS_CLUBB_ZM" wp2 0 1080 wp2 "w'^2" "[m^2/s^2]" "wp2"

    # Plot the time-averaged profile of wprtp.
    GrADS_profile "$GRADS_CLUBB_ZM" wprtp 0 1080 wprtp "w'rt'" "[(m/s)(kg/kg)]" "wprtp"

    # Plot the time-averaged profile of wpthlp.
    GrADS_profile "$GRADS_CLUBB_ZM" wpthlp 0 1080 wpthlp "w'thl'" "[(m/s)K]" "wpthlp"

    # Plot the time-averaged profile of rtp2.
    GrADS_profile "$GRADS_CLUBB_ZM" rtp2 0 1080 rtp2 "rt'^2" "[kg^2/kg^2]" "rtp2"

    # Plot the time-averaged profile of thlp2.
    GrADS_profile "$GRADS_CLUBB_ZM" thlp2 0 1080 thlp2 "thl'^2" "[K^2]" "thlp2"

    # Plot the time-averaged profile of rtpthlp.
    GrADS_profile "$GRADS_CLUBB_ZM" rtpthlp 0 1080 rtpthlp "rt'thl'" "[(kg/kg)K]" "rtpthlp"

    # Plot the time-averaged profile of up2.
    GrADS_profile "$GRADS_CLUBB_ZM" up2 0 1080 up2 "u'^2" "[m^2/s^2]" "up2"

    # Plot the time-averaged profile of vp2.
    GrADS_profile "$GRADS_CLUBB_ZM" vp2 0 1080 vp2 "v'^2" "[m^2/s^2]" "vp2"

    # Plot the time-averaged profile of wpthvp.
    GrADS_profile "$GRADS_CLUBB_ZM" wpthvp 0 1080 wpthvp "w'thv'" "[(m/s)K]" "wpthvp"

fi # $CLUBB_ZM_STATS = 1

if [ "$CLUBB_SFC_STATS" -eq 1 ]; then

    # Plot the time series of lwp.
    GrADS_surface "$GRADS_CLUBB_SFC" lwp lwp "lwp" "[kg/m^2]" "LWP"

fi # $CLUBB_SFC_STATS = 1

if [ -d output ]; then
   cd output
else
   mkdir output
   cd output
fi
if [ -d clubb_plots ]; then
   rm -rf clubb_plots
   mkdir clubb_plots
else
   mkdir clubb_plots
fi
cd clubb_plots
mv ../../*.eps .
mv ../../*.jpg .
cd ../..
