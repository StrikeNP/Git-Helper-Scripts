#!/bin/bash
# $Id$
#===============================================================================
# Description:
# Script that produces plots from WRF-CLUBB output.
# Brian Griffin; February 2017.
#===============================================================================
function GrADS_plot_location {

# $1: Field name.
# $2: Latitude of the location.
# $3: Longitude of the location.
# $4: Output file prefix name.
# $5: Label for the y-axis of the plot.
# $6: Label for the units for the y-axis of the plot.
# $7: Title of the plot.

FIELD_NAME=$1
LOCLAT=$2
LOCLON=$3
OUTFILE_NAME=$4
YLABEL=$5
YLABELUN=$6
TITLE=$7

# Run the GrADS script plot_location.gs in portrait mode in the background.
grads -p -b -c 'run plot_location '"$GRADS_FILE"' '"$FIELD_NAME"' '"$LOCLAT"' '"$LOCLON"' '"$OUTFILE_NAME"' '"$YLABEL"' '"$YLABELUN"' '"$TITLE"

# Produce a *.eps file from the GrADS file output.
gxeps -c -i "$OUTFILE_NAME".m -o "$OUTFILE_NAME".eps

# Remove the GrADS output file.
rm "$OUTFILE_NAME".m

# Produce a *.jpg file from the *.eps file.
convert "$OUTFILE_NAME".eps "$OUTFILE_NAME".jpg

}
#===============================================================================
function GrADS_plot_location_height {

# $1: Field name.
# $2: Latitude of the location.
# $3: Longitude of the location.
# $4: Output file prefix name.
# $5: Title of the plot.

FIELD_NAME=$1
LOCLAT=$2
LOCLON=$3
OUTFILE_NAME=$4
TITLE=$5

# Run the GrADS script plot_location_height.gs in portrait mode in the
# background.
grads -p -b -c 'run plot_location_height '"$GRADS_FILE"' '"$FIELD_NAME"' '"$LOCLAT"' '"$LOCLON"' '"$OUTFILE_NAME"' '"$TITLE"

# Produce a *.eps file from the GrADS file output.
gxeps -c -i "$OUTFILE_NAME".m -o "$OUTFILE_NAME".eps

# Remove the GrADS output file.
rm "$OUTFILE_NAME".m

# Produce a *.jpg file from the *.eps file.
convert "$OUTFILE_NAME".eps "$OUTFILE_NAME".jpg

}
#===============================================================================
function GrADS_plot_map {

# $1: Field name.
# $2: Vertical level index to be plotted.
# $3: First time index to plot.
# $4: Increment between time indices.
# $5: Output file prefix name.
# $6: Title of the plot.

# When $VERTLEV_IDX > 0, plot the field at the requested level.
# When $VERTLEV_IDX = 0, plot the average value of the field over all levels.
# When $VERTLEV_IDX = -1, plot the maximum value of the field over all levels.
# When $VERTLEV_IDX = -2, plot the minimum value of the field over all levels.
FIELD_NAME=$1
VERTLEV_IDX=$2
TIME_START_IDX=$3
TIME_INCR=$4
OUTFILE_NAME=$5
TITLE=$6

# Run the GrADS script plot_map.gs in portrait mode in the background.
grads -p -b -c 'run plot_map '"$GRADS_FILE"' '"$FIELD_NAME"' '"$VERTLEV_IDX"' '"$TIME_START_IDX"' '"$TIME_INCR"' '"$OUTFILE_NAME"' '"$TITLE"

cd output
mkdir "$OUTFILE_NAME"
cd "$OUTFILE_NAME"
mv ../../"$OUTFILE_NAME"-*.m .

for file in *.m
do

   # Get the time number extension.
   IDX=`echo "$file" | cut -d . -f 1 | cut -d - -f 2`

   # Produce a *.eps file from each of the GrADS output files.
   gxeps -c -i "$OUTFILE_NAME"-"$IDX".m -o "$OUTFILE_NAME"-"$IDX".eps

   # Remove the GrADS output files.
   rm "$OUTFILE_NAME"-"$IDX".m

   # Produce a *.jpg file from the *.eps files.
   convert "$OUTFILE_NAME"-"$IDX".eps "$OUTFILE_NAME"-"$IDX".jpg

done

cd ../..

}
#===============================================================================

# ===== MAIN =====

# Optional input for CLUBB stats column.
# Initialize
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
                echo "Invalid filename entered; ignoring."
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

# $1: Required path and filename of wrfout input file.
if [ -z $1 ]; then
   echo "Please provide the path and filename of the WRF GrADS output file."
   exit
else
   GRADS_FILE=$1
fi

# Find the BL PBL physics scheme used by WRF.
# Currently, the CLUBB scheme has a value of 25.
PBL_SCHEME=`grep 'comment BL_PBL_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`

# Find the latitude and longitude for the location plots.
# When CLUBB is used as the PBL scheme and CLUBB stats are output, extract the
# location from the CLUBB GrADS stats output files.  Otherwise, take it from the
# central location of the WRF GrADS stats output file.
if [ "$PBL_SCHEME" -eq 25 ]; then
   if [ "$CLUBB_ZT_STATS" -eq 1 ]; then
      LOCLAT=`grep YDEF "$GRADS_CLUBB_ZT" | cut -d R -f 2 | rev | cut -d 1 -f 2 | rev`
      LOCLON=`grep XDEF "$GRADS_CLUBB_ZT" | cut -d R -f 2 | rev | cut -d 1 -f 2 | rev`
      CLUBB_STATS_LOCLAT=$LOCLAT
      CLUBB_STATS_LOCLON=$LOCLON
   elif [ "$CLUBB_ZM_STATS" -eq 1 ]; then
      LOCLAT=`grep YDEF "$GRADS_CLUBB_ZM" | cut -d R -f 2 | rev | cut -d 1 -f 2 | rev`
      LOCLON=`grep XDEF "$GRADS_CLUBB_ZM" | cut -d R -f 2 | rev | cut -d 1 -f 2 | rev`
      CLUBB_STATS_LOCLAT=$LOCLAT
      CLUBB_STATS_LOCLON=$LOCLON
   elif [ "$CLUBB_SFC_STATS" -eq 1 ]; then
      LOCLAT=`grep YDEF "$GRADS_CLUBB_SFC" | cut -d R -f 2 | rev | cut -d 1 -f 2 | rev`
      LOCLON=`grep XDEF "$GRADS_CLUBB_SFC" | cut -d R -f 2 | rev | cut -d 1 -f 2 | rev`
      CLUBB_STATS_LOCLAT=$LOCLAT
      CLUBB_STATS_LOCLON=$LOCLON
   else
      # Find the longitude and latitude of the center of the domain from the WRF
      # GrADS output file.
      LOCLAT=`grep 'comment CEN_LAT' "$GRADS_FILE" | cut -d = -f 2`
      LOCLON=`grep 'comment CEN_LON' "$GRADS_FILE" | cut -d = -f 2`
   fi
else
   # Find the longitude and latitude of the center of the domain from the WRF
   # GrADS output file.
   LOCLAT=`grep 'comment CEN_LAT' "$GRADS_FILE" | cut -d = -f 2`
   LOCLON=`grep 'comment CEN_LON' "$GRADS_FILE" | cut -d = -f 2`
fi

# The GrADS WRF output file longitudes range from 0 to 360.
if [ $( echo "$LOCLON < 0.0" | bc ) -eq 1 ]; then
   LOCLON=$( echo "$LOCLON + 360" | bc)
fi

# Make the "output" subdirectory to store the plots.
if [ -d output ]; then
   rm -rf output
   mkdir output
else
   mkdir output
fi

#-------------------------------------------------------------------------------
# CLUBB stats plots.
if [ "$PBL_SCHEME" -eq 25 ]; then
   ./plot_clubb_output.bash --clubb-zt "$GRADS_CLUBB_ZT" --clubb-zm "$GRADS_CLUBB_ZM" --clubb-sfc "$GRADS_CLUBB_SFC"
fi

#-------------------------------------------------------------------------------
# Plots at a location

# Plot the sea-level pressure.
GrADS_plot_location slp $LOCLAT $LOCLON slp "Pressure" "[mb]" "SLP"

# Plot 2 m temperature.
GrADS_plot_location "1.8*(t2-273.15)+32" $LOCLAT $LOCLON temp_2m "Temperature" "[F]" "Temperature_at_2_m"

# Plot 10 m wind speed.
GrADS_plot_location "pow(pow(u10m,2)+pow(v10m,2),0.5)" $LOCLAT $LOCLON wspd_10m "Wind_Speed" "[m/s]" "Wind_Speed_at_10_m"

# Plot surface wind direction.
GrADS_plot_location wdir $LOCLAT $LOCLON wdir_sfc "Wind_Direction" "[degrees]" "Surface_Wind_Direction"

# Plot Accumulated surface rain.
GrADS_plot_location rainnc $LOCLAT $LOCLON rainnc "Accumulated_Rainfall" "[mm]" "Accumulated_Rainfall_(grid_scale)"

# Plot Accumulated surface snow (liquid).
GrADS_plot_location snownc $LOCLAT $LOCLON snownc "Accumulated_Snow" "[mm]" "Accumulated_Snow_Liquid_(grid_scale)"

# Plot Accumulated surface graupel (liquid).
GrADS_plot_location graupelnc $LOCLAT $LOCLON graupelnc "Accumulated_Graupel" "[mm]" "Accumulated_Graupel_Liquid_(grid_scale)"

if [ $PBL_SCHEME -eq 25 ]; then
   # Plot CLUBB's cloud fraction.
   GrADS_plot_location_height cf_CLUBB $LOCLAT $LOCLON cf_clubb "CLUBB's_Cloud_Fraction"
fi

if [ $PBL_SCHEME -eq 25 ]; then
   # Plot CLUBB's cloud water mixing ratio.
   GrADS_plot_location_height rcm_CLUBB $LOCLAT $LOCLON rcm_clubb "CLUBB's_Mean_rc"
fi

# Plot cloud water mixing ratio.
GrADS_plot_location_height qcloud $LOCLAT $LOCLON qcloud "Cloud_Water_Mixing_Ratio"

# Plot rain water mixing ratio.
GrADS_plot_location_height qrain $LOCLAT $LOCLON qrain "Rain_Water_Mixing_Ratio"

# Move all the plots at a location into the output/plots_loc directory.
cd output
mkdir plots_loc
cd plots_loc
mv ../../*.eps .
mv ../../*.jpg .
cd ../..

#-------------------------------------------------------------------------------
# Plot Maps

# Plot the map of sea-level pressure.
GrADS_plot_map slp 1 1 6 slp_map "SLP"

# Plot the map of 2 meter temperature.
GrADS_plot_map "1.8*(t2-273.15)+32" 1 1 6 temp_2m_map "Temperature_deg_F"

# Plot the map of surface wind speed.
GrADS_plot_map "pow(pow(u10m,2)+pow(v10m,2),0.5)" 1 1 6 wspd_map "Wind_Speed_at_10m"

# Plot the map of Accumulated Rainfall
GrADS_plot_map rainnc 1 1 6 rainnc_map "Accumulated_Rainfall"

# Plot the map of Accumulated Snowfall (liquid)
GrADS_plot_map snownc 1 1 6 snownc_map "Accumulated_Snowfall"

# Plot the map of Accumulated Graupel (liquid)
GrADS_plot_map graupelnc 1 1 6 graupelnc_map "Accumulated_Graupel"

if [ $PBL_SCHEME -eq 25 ]; then
   # Plot the map of maximum CLUBB cloud fraction.
   GrADS_plot_map cf_CLUBB -1 1 6 max_cf_clubb_map "Max_CLUBB_cld_frac"
fi

# Set up the html file to display the plots.
./setup_html.bash $GRADS_FILE $CLUBB_ZT_STATS $CLUBB_ZM_STATS $CLUBB_SFC_STATS $CLUBB_STATS_LOCLAT $CLUBB_STATS_LOCLON
