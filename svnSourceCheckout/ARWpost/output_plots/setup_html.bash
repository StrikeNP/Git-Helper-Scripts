#!/bin/bash
# $Id: setup_html.bash 862 2017-03-10 06:34:16Z bmg2@uwm.edu $
#===============================================================================
# Description:
# Sets up a webpage for the *.jpg files.
#===============================================================================
function list_all_files_in_dir {

# $1:  Directory where the files are found.
# $2:  List only files of this type (* for all files).

DIR=$1
FILE_TYPE=$2

# Find the current directory.
RUN_PATH=`pwd`

# Change to the directory where the files are located.
cd $DIR

# Find all files of type $FILE_TYPE in the directory.
for file in $FILE_TYPE
do
   echo "<img src=\"$DIR/$file\">" >> $RUN_PATH/index.html
done

# Change to the original directory.
cd $RUN_PATH
}
#===============================================================================

#===== MAIN =====

# $1: Required path and filename of wrfout input file.
# $2: Flag for having found the CLUBB zt statistical output file (1 for true).
# $3: Flag for having found the CLUBB zm statistical output file (1 for true).
# $4: Flag for having found the CLUBB sfc statistical output file (1 for true).
# $5: Latitude of the CLUBB statistical output column.
# $6: Longitude of the CLUBB statistical output column.
GRADS_FILE=$1
CLUBB_ZT_STATS=$2
CLUBB_ZM_STATS=$3
CLUBB_SFC_STATS=$4
CLUBB_STATS_LOCLAT=$5
CLUBB_STATS_LOCLON=$6

# Get information about the run.
START_DATE=`grep 'comment START_DATE' "$GRADS_FILE" | cut -d = -f 2`
CENTRAL_LAT=`grep 'comment CEN_LAT' "$GRADS_FILE" | cut -d = -f 2`
CENTRAL_LON=`grep 'comment CEN_LON' "$GRADS_FILE" | cut -d = -f 2`
TIMESTEP=`grep 'comment DT' "$GRADS_FILE" | cut -d = -f 2`
WEST_EAST_GRID_SPACING=`grep 'comment DX' "$GRADS_FILE" | cut -d = -f 2`
SOUTH_NORTH_GRID_SPACING=`grep 'comment DY' "$GRADS_FILE" | cut -d = -f 2`
NUM_LEVS=`grep 'comment BOTTOM-TOP_GRID_DIMENSION' "$GRADS_FILE" | cut -d = -f 2`
PBL_SCHEME=`grep 'comment BL_PBL_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`
MP_SCHEME=`grep 'comment MP_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`
RAD_LW_SCHEME=`grep 'comment RA_LW_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`
RAD_SW_SCHEME=`grep 'comment RA_SW_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`
RAD_TS=`grep 'comment RADT' "$GRADS_FILE" | cut -d = -f 2`
CU_SCHEME=`grep 'comment CU_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`
CU_TS=`grep 'comment CUDT' "$GRADS_FILE" | cut -d = -f 2`
SFCLAY_SCHEME=`grep 'comment SF_SFCLAY_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`
SFC_SCHEME=`grep 'comment SF_SURFACE_PHYSICS' "$GRADS_FILE" | cut -d = -f 2`

# Change directory to the "output" directory.
cd output

# Remove any current index.html file.
if [ -f index.html ]; then
   rm index.html
fi

# Start the index.html file to use for writing.
touch index.html

# Enter all html code.
echo "<html>" >> index.html
echo "" >> index.html

# Header of html file
echo "<head>" >> index.html
echo "" >> index.html

# Webpage title.
if [ $PBL_SCHEME -eq 25 ]; then
   echo "<title>WRF-CLUBB output plots</title>" >> index.html
else # $PBL_SCHEME != 25
   echo "<title>WRF output plots</title>" >> index.html
fi # $PBL_SCHEME -eq 25
echo "" >> index.html

# Close the header of the html file.
echo "</head>" >> index.html
echo "" >> index.html

# Body of html file.
echo "<body>" >> index.html
echo "" >> index.html

# Title of the page.
if [ $PBL_SCHEME -eq 25 ]; then
   echo "<h1><center>WRF-CLUBB output plots</center></h1>" >> index.html
else # $PBL_SCHEME != 25
   echo "<h1><center>WRF output plots</center></h1>" >> index.html
fi # $PBL_SCHEME -eq 25
echo "" >> index.html

# Print information about the run.
echo "Run Start Date (UTC): "$START_DATE"<br>" >> index.html
#echo "Run Duration (Hours):  <br>" >> index.html

echo "<br>" >> index.html

echo "Latitude of the Center of the Domain: "$CENTRAL_LAT"<br>" >> index.html
echo "Longitude of the Center of the Domain: "$CENTRAL_LON"<br>" >> index.html
if [ $PBL_SCHEME -eq 25 ]; then
   if [ $CLUBB_ZT_STATS -eq 1 ] || [ $CLUBB_ZM_STATS -eq 1 ] || [ $CLUBB_SFC_STATS -eq 1 ]; then
      echo "Latitude of CLUBB Stats Grid Column: "$CLUBB_STATS_LOCLAT"<br>" >> index.html
      echo "Longitude of CLUBB Stats Grid Column: "$CLUBB_STATS_LOCLON"<br>" >> index.html
   fi # $CLUBB_ZT_STATS -eq 1 ] || [ $CLUBB_ZM_STATS -eq 1 ] || [ $CLUBB_SFC_STATS -eq 1 ]
fi # $PBL_SCHEME -eq 25

echo "<br>" >> index.html

echo "Timestep Duration (Seconds): "$TIMESTEP"<br>" >> index.html
echo "West-East Grid Spacing (Meters): "$WEST_EAST_GRID_SPACING"<br>" >> index.html
echo "South-North Grid Spacing (Meters): "$SOUTH_NORTH_GRID_SPACING"<br>" >> index.html
echo "Number of Vertical Levels: "$NUM_LEVS"<br>" >> index.html

echo "<br>" >> index.html

echo "PBL Scheme: "$PBL_SCHEME"<br>" >> index.html
echo "(CLUBB is PBL Scheme 25)<br>" >> index.html
echo "Microphysics Scheme: "$MP_SCHEME"<br>" >> index.html
echo "LW Radiation Scheme: "$RAD_LW_SCHEME"<br>" >> index.html
echo "SW Radiation Scheme: "$RAD_SW_SCHEME"<br>" >> index.html
echo "Radiation Timestep (Minutes): "$RAD_TS"<br>" >> index.html
echo "Cumulus Scheme: "$CU_SCHEME"<br>" >> index.html
echo "Cumulus Timestep (Minutes): "$CU_TS"<br>" >> index.html
echo "Surface Layer Scheme: "$SFCLAY_SCHEME"<br>" >> index.html
echo "Surface Scheme: "$SFC_SCHEME"<br>" >> index.html

echo "<br>" >> index.html

#echo "SILHS on (Y or N):  <br>" >> index.html
#echo "Number of SILHS Sample Points:  <br>" >> index.html
#echo "" >> index.html

echo "<hr>" >> index.html
echo "" >> index.html

# Print time series or time and height plots at the selected location.
if [ $PBL_SCHEME -eq 25 ]; then
   if [ $CLUBB_ZT_STATS -eq 1 ] || [ $CLUBB_ZM_STATS -eq 1 ] || [ $CLUBB_SFC_STATS -eq 1 ]; then
      PLOT_LAT=$CLUBB_STATS_LOCLAT
      PLOT_LON=$CLUBB_STATS_LOCLON
   else # $CLUBB_ZT_STATS = $CLUBB_ZM_STATS = $CLUBB_SFC_STATS = 0
      PLOT_LAT=$CENTRAL_LAT
      PLOT_LON=$CENTRAL_LON
   fi # $CLUBB_ZT_STATS -eq 1 ] || [ $CLUBB_ZM_STATS -eq 1 ] || [ $CLUBB_SFC_STATS -eq 1 ]
else # $PBL_SCHEME != 25
   PLOT_LAT=$CENTRAL_LAT
   PLOT_LON=$CENTRAL_LON
fi # $PBL_SCHEME -eq 25

echo "<h2><center>Plots at Latitude: "$PLOT_LAT"; Longitude: "$PLOT_LON"</center></h2>" >> index.html
echo "" >> index.html
echo "<center>" >> index.html
echo "" >> index.html
list_all_files_in_dir plots_loc *.jpg
echo "" >> index.html
echo "</center>" >> index.html
echo "" >> index.html

echo "<hr>" >> index.html
echo "" >> index.html

# Print "map" plots over the whole domain.
# Temperature at 2 meters.
echo "<h2><center>Domain 2 meter Temperature</center></h2>" >> index.html
echo "" >> index.html
echo "<center>" >> index.html
echo "" >> index.html
list_all_files_in_dir temp_2m_map *.jpg
echo "" >> index.html
echo "</center>" >> index.html
echo "" >> index.html

# Sea Level Pressure.
echo "<h2><center>Domain Sea Level Pressure</center></h2>" >> index.html
echo "" >> index.html
echo "<center>" >> index.html
echo "" >> index.html
list_all_files_in_dir slp_map *.jpg
echo "" >> index.html
echo "</center>" >> index.html
echo "" >> index.html

# Wind Speed at 10 meters.
echo "<h2><center>Domain 10 meter Wind Speed</center></h2>" >> index.html
echo "" >> index.html
echo "<center>" >> index.html
echo "" >> index.html
list_all_files_in_dir wspd_map *.jpg
echo "" >> index.html
echo "</center>" >> index.html
echo "" >> index.html

# Accumulated Rainfall.
echo "<h2><center>Domain Accumulated Rainfall</center></h2>" >> index.html
echo "" >> index.html
echo "<center>" >> index.html
echo "" >> index.html
list_all_files_in_dir rainnc_map *.jpg
echo "" >> index.html
echo "</center>" >> index.html
echo "" >> index.html

# Maximum CLUBB Cloud Fraction in each Column.
if [ $PBL_SCHEME -eq 25 ]; then

   echo "<h2><center>Domain Maximum CLUBB Cloud Fraction</center></h2>" >> index.html
   echo "" >> index.html
   echo "<center>" >> index.html
   echo "" >> index.html
   list_all_files_in_dir max_cf_clubb_map *.jpg
   echo "" >> index.html
   echo "</center>" >> index.html
   echo "" >> index.html

fi # $PBL_SCHEME -eq 25

# Print time-averaged profiles from the CLUBB statistical output column (which
# is at one selected location) for CLUBB variables.
if [ $PBL_SCHEME -eq 25 ]; then
   if [ $CLUBB_ZT_STATS -eq 1 ] || [ $CLUBB_ZM_STATS -eq 1 ] || [ $CLUBB_SFC_STATS -eq 1 ]; then

      echo "<hr>" >> index.html
      echo "" >> index.html

      echo "<h2><center>CLUBB Time-Averaged Profiles at Latitude: "$CLUBB_STATS_LOCLAT"; Longitude: "$CLUBB_STATS_LOCLON"</center></h2>" >> index.html
      echo "" >> index.html
      echo "<center>" >> index.html
      echo "" >> index.html
      list_all_files_in_dir clubb_plots *.jpg
      echo "" >> index.html
      echo "</center>" >> index.html
      echo "" >> index.html

   fi # $CLUBB_ZT_STATS -eq 1 ] || [ $CLUBB_ZM_STATS -eq 1 ] || [ $CLUBB_SFC_STATS -eq 1 ]
fi # $PBL_SCHEME -eq 25

# Close the body section of the html file.
echo "</body>" >> index.html
echo "" >> index.html

# Close the html file.
echo "</html>" >> index.html

# Go back to the output_plots directory.
cd ..
