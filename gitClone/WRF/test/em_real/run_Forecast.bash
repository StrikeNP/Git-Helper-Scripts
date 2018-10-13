#!/bin/bash
# $Id$
#===============================================================================
# Description:
# Script to run a forecast using WRF-CLUBB.
# The options in this script for the large-scale forecast model that is used to
# provide initial conditions and boundary conditions to WPS are NAM North
# America 32 km or GFS global 0.25 degree.
# Brian Griffin; February 2017.
#===============================================================================
function get_days_in_month {
# Function to give the number of days in any month. 
# $1: month (01 through 12)
# $2: year
if [ $1 == "01" ]; then
   # January
   DAYS_IN_MONTH=31
elif [ $1 == "02" ]; then
   # February
   if [ $(( $2 % 400 )) == 0 ]; then
      # Year is evenly divisible by 400.
      DAYS_IN_MONTH=29
   elif [ $(( $2 % 100 )) == 0 ]; then
      # Year is evenly divisible by 100.
      DAYS_IN_MONTH=28
   elif [ $(( $2 % 4 )) == 0 ]; then
      # Year is evenly divisible by 4.
      DAYS_IN_MONTH=29
   else 
      DAYS_IN_MONTH=28
   fi
elif [ $1 == "03" ]; then
   # March
   DAYS_IN_MONTH=31
elif [ $1 == "04" ]; then
   # April
   DAYS_IN_MONTH=30
elif [ $1 == "05" ]; then
   # May
   DAYS_IN_MONTH=31
elif [ $1 == "06" ]; then
   # June
   DAYS_IN_MONTH=30
elif [ $1 == "07" ]; then
   # July
   DAYS_IN_MONTH=31
elif [ $1 == "08" ]; then
   # August
   DAYS_IN_MONTH=31
elif [ $1 == "09" ]; then
   # September
   DAYS_IN_MONTH=30
elif [ $1 == "10" ]; then
   # October
   DAYS_IN_MONTH=31
elif [ $1 == "11" ]; then
   # November
   DAYS_IN_MONTH=30
elif [ $1 == "12" ]; then
   # December
   DAYS_IN_MONTH=31
fi
}
#===============================================================================
function leading_zero {
# Function to tack a leading "0" onto single digit integers used for month, day
# of the month, or hour.
if [ $1 -eq 0 ]; then
   TWO_DIGIT="00"
elif [ $1 -ge 1 ] && [ $1 -le 9 ]; then
   TWO_DIGIT="0"$1
else
   TWO_DIGIT=$1
fi
}
#===============================================================================
function leading_zeros {
# Function to tack a leading "0" or "00" onto single digit or two digit integers
# used for the number of forecast hours.
if [ $1 -eq 0 ]; then
   THREE_DIGIT="000"
elif [ $1 -ge 1 ] && [ $1 -le 9 ]; then
   THREE_DIGIT="00"$1
elif [ $1 -ge 10 ] && [ $1 -le 99 ]; then
   THREE_DIGIT="0"$1
else
   THREE_DIGIT=$1
fi
}
#===============================================================================
function get_nam_na_32km_files {

# Function to download the correct NAM North America 32 km forecast files to use
# for initial conditions and boundary conditions.

START_DATE_STRING=$START_UTC_YEAR$START_UTC_MONTH$START_UTC_DAY

# Find the most recent available model forecast and download the first file.
STATUS=10
NUM_ITER=0
while [ $STATUS != 0 ]
do

   # Check to see if the file is available yet.
   # Check for the last file first (at forecast time $FCAST_FILES_HOURS),
   # because it will be the last one to become available.
   wget --spider http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/nam."$START_DATE_STRING"/nam.t"$START_UTC_HOUR"z.awip32"$FCAST_FILES_HOURS".tm00.grib2

   # If status = 0, the file is available.  Otherwise, status != 0.
   STATUS=$?

   if [ $STATUS != 0 ]; then

      # Check earlier time.

      NUM_ITER=$(($NUM_ITER+1))
      if [ $NUM_ITER == 10 ]; then
         break
      fi

      if [ $START_UTC_HOUR -ge 6 ]; then

         START_UTC_HOUR=$((10#$START_UTC_HOUR))
         START_UTC_HOUR=$(($START_UTC_HOUR-6))
         leading_zero $START_UTC_HOUR
         START_UTC_HOUR=$TWO_DIGIT

      else # $START_UTC_HOUR < 6

         START_UTC_HOUR=$((10#$START_UTC_HOUR))
         START_UTC_HOUR=$(($START_UTC_HOUR+18))

         if [ $START_UTC_DAY -gt 1 ]; then

            START_UTC_DAY=$((10#$START_UTC_DAY))
            START_UTC_DAY=$(($START_UTC_DAY-1))
            leading_zero $START_UTC_DAY
            START_UTC_DAY=$TWO_DIGIT

         else # $START_UTC_DAY = 1

            if [ $START_UTC_MONTH -gt 1 ]; then

               START_UTC_MONTH=$((10#$START_UTC_MONTH))
               START_UTC_MONTH=$(($START_UTC_MONTH-1))
               leading_zero $START_UTC_MONTH
               START_UTC_MONTH=$TWO_DIGIT
               get_days_in_month $START_UTC_MONTH $START_UTC_YEAR
               START_UTC_DAY=$DAYS_IN_MONTH

            else #$START_UTC_MONTH = 1

               START_UTC_YEAR=$(($START_UTC_YEAR-1))
               START_UTC_MONTH=12
               START_UTC_DAY=31

            fi # $START_UTC_MONTH > 1

         fi # $START_UTC_DAY > 1

         START_DATE_STRING=$START_UTC_YEAR$START_UTC_MONTH$START_UTC_DAY

      fi # $START_UTC_HOUR >= 6
      
   else # $STATUS = 0

      # Download file.
      wget http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/nam."$START_DATE_STRING"/nam.t"$START_UTC_HOUR"z.awip32"$FCAST_FILES_HOURS".tm00.grib2

      break

   fi # $STATUS != 0

done # while $STATUS != 0

# Get the remaining forecast files.
if [ $STATUS -eq 0 ]; then

   for FCAST_HOUR in `seq -w "$FCAST_FILES_HOURS" -"$FCAST_FILES_INCR" 0`;
   do

      if [ $FCAST_HOUR -eq $FCAST_FILES_HOURS ]; then
         # The forecast file at time $FCAST_FILES_HOURS has already been
         # downloaded.  Skip and go to the next file.
         continue
      fi

      # Check to see if the file is available.
      wget --spider http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/nam."$START_DATE_STRING"/nam.t"$START_UTC_HOUR"z.awip32"$FCAST_HOUR".tm00.grib2

      # If status = 0, the file is available.  Otherwise, status != 0.
      STATUS1=$?

      if [ $STATUS1 == 0 ]; then
         wget http://nomads.ncep.noaa.gov/pub/data/nccf/com/nam/prod/nam."$START_DATE_STRING"/nam.t"$START_UTC_HOUR"z.awip32"$FCAST_HOUR".tm00.grib2
      fi # $STATUS1 = 0

   done # for FCAST_HOUR in `seq -w "$FCAST_FILES_HOURS" -"$FCAST_FILES_INCR" 0`
   
fi # $STATUS = 0
}
#===============================================================================
function get_gfs_global_0p25deg_files {

# Function to download the correct GFS global 0.25 degree forecast files to use
# for initial conditions and boundary conditions.

START_DATE_STRING=$START_UTC_YEAR$START_UTC_MONTH$START_UTC_DAY

# Find the most recent available model forecast and download the first file.
STATUS=10
NUM_ITER=0
while [ $STATUS != 0 ]
do

   # Check to see if the file is available yet.
   # Check for the last file first (at forecast time $FCAST_FILES_HOURS),
   # because it will be the last one to become available.
   wget --spider http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs."$START_DATE_STRING""$START_UTC_HOUR"/gfs.t"$START_UTC_HOUR"z.pgrb2.0p25.f"$FCAST_FILES_HOURS"

   # If status = 0, the file is available.  Otherwise, status != 0.
   STATUS=$?

   if [ $STATUS != 0 ]; then

      # Check earlier time.

      NUM_ITER=$(($NUM_ITER+1))
      if [ $NUM_ITER == 10 ]; then
         break
      fi

      if [ $START_UTC_HOUR -ge 6 ]; then

         START_UTC_HOUR=$((10#$START_UTC_HOUR))
         START_UTC_HOUR=$(($START_UTC_HOUR-6))
         leading_zero $START_UTC_HOUR
         START_UTC_HOUR=$TWO_DIGIT

      else # $START_UTC_HOUR < 6

         START_UTC_HOUR=$((10#$START_UTC_HOUR))
         START_UTC_HOUR=$(($START_UTC_HOUR+18))

         if [ $START_UTC_DAY -gt 1 ]; then

            START_UTC_DAY=$((10#$START_UTC_DAY))
            START_UTC_DAY=$(($START_UTC_DAY-1))
            leading_zero $START_UTC_DAY
            START_UTC_DAY=$TWO_DIGIT

         else # $START_UTC_DAY = 1

            if [ $START_UTC_MONTH -gt 1 ]; then

               START_UTC_MONTH=$((10#$START_UTC_MONTH))
               START_UTC_MONTH=$(($START_UTC_MONTH-1))
               leading_zero $START_UTC_MONTH
               START_UTC_MONTH=$TWO_DIGIT
               get_days_in_month $START_UTC_MONTH $START_UTC_YEAR
               START_UTC_DAY=$DAYS_IN_MONTH

            else #$START_UTC_MONTH = 1

               START_UTC_YEAR=$(($START_UTC_YEAR-1))
               START_UTC_MONTH=12
               START_UTC_DAY=31

            fi # $START_UTC_MONTH > 1

         fi # $START_UTC_DAY > 1

         START_DATE_STRING=$START_UTC_YEAR$START_UTC_MONTH$START_UTC_DAY

      fi # $START_UTC_HOUR >= 6
      
   else # $STATUS = 0

      # Download file.
      wget http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs."$START_DATE_STRING""$START_UTC_HOUR"/gfs.t"$START_UTC_HOUR"z.pgrb2.0p25.f"$FCAST_FILES_HOURS"

      break

   fi # $STATUS != 0

done # while $STATUS != 0

# Get the remaining forecast files.
if [ $STATUS -eq 0 ]; then

   for FCAST_HOUR in `seq -w "$FCAST_FILES_HOURS" -"$FCAST_FILES_INCR" 0`;
   do

      if [ $FCAST_HOUR -eq $FCAST_FILES_HOURS ]; then
         # The forecast file at time $FCAST_FILES_HOURS has already been
         # downloaded.  Skip and go to the next file.
         continue
      fi

      # Check to see if the file is available.
      wget --spider http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs."$START_DATE_STRING""$START_UTC_HOUR"/gfs.t"$START_UTC_HOUR"z.pgrb2.0p25.f"$FCAST_HOUR"

      # If status = 0, the file is available.  Otherwise, status != 0.
      STATUS1=$?

      if [ $STATUS1 == 0 ]; then
         wget http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs."$START_DATE_STRING""$START_UTC_HOUR"/gfs.t"$START_UTC_HOUR"z.pgrb2.0p25.f"$FCAST_HOUR"
      fi # $STATUS1 = 0

   done # for FCAST_HOUR in `seq -w "$FCAST_FILES_HOURS" -"$FCAST_FILES_INCR" 0`
   
fi # $STATUS = 0
}
#===============================================================================
function get_sst_file {

# Function to download the sea surface temperature (SST) file.  This is a single
# file, and the SST will be constant over time during the forecast run.

SST_UTC_YEAR=$START_UTC_YEAR
SST_UTC_MONTH=$START_UTC_MONTH
SST_UTC_DAY=$START_UTC_DAY

SST_DATE_STRING=$SST_UTC_YEAR$SST_UTC_MONTH$SST_UTC_DAY

# Find the most recent available model forecast and download the first file.
STATUS=10
NUM_ITER=0
while [ $STATUS != 0 ]
do

   # Check to see if the file is available yet.
   wget --spider http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/sst."$SST_DATE_STRING"/rtgssthr_grb_0.5.grib2

   # If status = 0, the file is available.  Otherwise, status != 0.
   STATUS=$?

   if [ $STATUS != 0 ]; then

      # Check earlier time.

      NUM_ITER=$(($NUM_ITER+1))
      if [ $NUM_ITER == 10 ]; then
         break
      fi

      if [ $SST_UTC_DAY -gt 1 ]; then

         SST_UTC_DAY=$((10#$SST_UTC_DAY))
         SST_UTC_DAY=$(($SST_UTC_DAY-1))
         leading_zero $SST_UTC_DAY
         SST_UTC_DAY=$TWO_DIGIT

      else # $SST_UTC_DAY = 1

         if [ $SST_UTC_MONTH -gt 1 ]; then

            SST_UTC_MONTH=$((10#$SST_UTC_MONTH))
            SST_UTC_MONTH=$(($SST_UTC_MONTH-1))
            leading_zero $SST_UTC_MONTH
            SST_UTC_MONTH=$TWO_DIGIT
            get_days_in_month $SST_UTC_MONTH $SST_UTC_YEAR
            SST_UTC_DAY=$DAYS_IN_MONTH

         else #$SST_UTC_MONTH = 1

            SST_UTC_YEAR=$(($SST_UTC_YEAR-1))
            SST_UTC_MONTH=12
            SST_UTC_DAY=31

         fi # $SST_UTC_MONTH > 1

      fi # $SST_UTC_DAY > 1

      SST_DATE_STRING=$SST_UTC_YEAR$SST_UTC_MONTH$SST_UTC_DAY
      
   else # $STATUS = 0

      # Download file.
      wget http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/sst."$SST_DATE_STRING"/rtgssthr_grb_0.5.grib2

      break

   fi # $STATUS != 0

done # while $STATUS != 0
}
#===============================================================================
function set_forecast_times_get_data {

# Function that sets the forecast times and downloads the appropriate data
# files.

# Get the current time in UTC as a string in the form YYYYMMDDHH.
CURRENT_UTC_DATE_STRING=`date --utc +%Y%m%d%H`

# Current year.
CURRENT_UTC_YEAR=`echo $CURRENT_UTC_DATE_STRING | cut -c 1-4`

# Current month.
CURRENT_UTC_MONTH=`echo $CURRENT_UTC_DATE_STRING | cut -c 5-6`

# Current day of the month.
CURRENT_UTC_DAY=`echo $CURRENT_UTC_DATE_STRING | cut -c 7-8`

# Current hour (00-23).
CURRENT_UTC_HOUR=`echo $CURRENT_UTC_DATE_STRING | cut -c 9-10`

# Many large-scale forecast runs, including NAM and GFS, are run every six hours
# at 00Z, 06Z, 12Z, and 18Z.
# Calculate the most recent 6 hour interval using integer division.  Dividing
# the current hour by 6 using integer division produces an integer answer that
# is rounded to the smaller integer.  Multiplying that result by 6 produces the
# most recent 6 hour interval.
MOST_RECENT_6HOUR_INCR=$(( ( CURRENT_UTC_HOUR / 6 ) * 6 ))

# Find the most recently available large-scale forecast.
START_UTC_YEAR=$CURRENT_UTC_YEAR
START_UTC_MONTH=$CURRENT_UTC_MONTH
START_UTC_DAY=$CURRENT_UTC_DAY
START_UTC_HOUR=$MOST_RECENT_6HOUR_INCR
leading_zero $START_UTC_HOUR
START_UTC_HOUR=$TWO_DIGIT

# Enter the time duration of the forecast (in hours).
FCAST_FILES_HOURS=36
# FCAST_FILES_HOURS_ADD is set to FCAST_FILES_HOURS without the leading zeros.
FCAST_FILES_HOURS_ADD=$FCAST_FILES_HOURS
if [ $INPUT_MODEL == "NAM" ]; then
   # The value of $FCAST_FILES_HOURS needs to be two digits for NAM.
   leading_zero $FCAST_FILES_HOURS
   FCAST_FILES_HOURS=$TWO_DIGIT
elif [ $INPUT_MODEL == "GFS" ]; then
   # The value of $FCAST_FILES_HOURS needs to be three digits for GFS.
   leading_zeros $FCAST_FILES_HOURS
   FCAST_FILES_HOURS=$THREE_DIGIT
fi
# Enter the increment between forecast files (in hours).
FCAST_FILES_INCR=6

# Place the input model forecast files in the "Forecast_tmp" directory.
if [ -d Forecast_tmp ]; then
   # Directory already found.
   cd Forecast_tmp
else
   # Make the directory first.
   mkdir Forecast_tmp
   cd Forecast_tmp
fi

# Remove any files found in the directory.
rm -f *

if [ $INPUT_MODEL == "NAM" ]; then
   # Get the NAM North America 32 km forecast files.
   get_nam_na_32km_files
elif [ $INPUT_MODEL == "GFS" ]; then
   # Get the GFS global 0.25 degree forecast files.
   get_gfs_global_0p25deg_files
fi

cd ..

# Change the name of the "Forecast_tmp" directory to "Forecast_YYYYMMDD_HHZ".
if [ -d Forecast_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z ]; then
   # Directory already found.
   rm -rf Forecast_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z
fi
mv Forecast_tmp Forecast_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z

# Place the SST file in the "SST_YYYYMMDD_HHZ" directory.
if [ -d SST_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z ]; then
   # Directory already found.
   cd SST_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z
else
   # Make the directory first.
   mkdir SST_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z
   cd SST_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z
fi

# Remove any files found in the directory.
rm -f *

# Get the SST file.
get_sst_file

cd ..

# Calculate the end time of the run.
END_UTC_YEAR=$START_UTC_YEAR
END_UTC_MONTH=$START_UTC_MONTH
END_UTC_DAY=$START_UTC_DAY
END_UTC_HOUR=$((10#$START_UTC_HOUR+$FCAST_FILES_HOURS_ADD))
leading_zero $END_UTC_HOUR
END_UTC_HOUR=$TWO_DIGIT
if [ $END_UTC_HOUR -ge 24 ]; then

   # Use integer division (which will produce an integer answer that is rounded
   # down) to figure out $END_UTC_DAY.
   END_UTC_DAY=$(( 10#$END_UTC_DAY + ( 10#$END_UTC_HOUR / 24 ) ))
   leading_zero $END_UTC_DAY
   END_UTC_DAY=$TWO_DIGIT
   END_UTC_HOUR=$(( 10#$END_UTC_HOUR % 24 ))
   leading_zero $END_UTC_HOUR
   END_UTC_HOUR=$TWO_DIGIT

   # Get the number of days in a month.
   get_days_in_month $START_UTC_MONTH $START_UTC_YEAR

   if [ $END_UTC_DAY -gt $DAYS_IN_MONTH ]; then

      END_UTC_DAY=$(( 10#$END_UTC_DAY - $DAYS_IN_MONTH ))
      leading_zero $END_UTC_DAY
      END_UTC_DAY=$TWO_DIGIT
      END_UTC_MONTH=$(( 10#$END_UTC_MONTH + 1 ))
      leading_zero $END_UTC_MONTH
      END_UTC_MONTH=$TWO_DIGIT

      if [ $END_UTC_MONTH -gt 12 ]; then

         END_UTC_MONTH=1
         leading_zero $END_UTC_MONTH
         END_UTC_MONTH=$TWO_DIGIT
         END_UTC_YEAR=$(( $END_UTC_YEAR + 1 ))

      fi # $END_UTC_MONTH > 12

   fi # $END_UTC_DAY > $DAYS_IN_MONTH

fi # $END_UTC_HOURS >= 24
}
#===============================================================================

# ===== MAIN =====

# Set default values.
LAT=43.00
LON=-88.00
INPUT_MODEL=NAM
PLOT_OUTPUT=0

while true;
do
   case "$1" in
      --lat) # Set the latitude of the center of the domain.
             if [ $( echo "$2 <= 90.0" | bc ) -eq 1 ] && [ $( echo "$2 >= -90.0" | bc ) -eq 1 ] ; then
                LAT=$2
             else
                echo "Invalid latitude entered; using default."
                echo "Latitude entered: " $2
             fi
             shift 2;;
      --lon) # Set the longitude of the center of the domain.
             if [ $( echo "$2 <= 180.0" | bc ) -eq 1 ] && [ $( echo "$2 >= -180.0" | bc ) -eq 1 ] ; then
                LON=$2
             else
                echo "Invalid longitude entered; using default."
                echo "Longitude entered: " $2
             fi
             shift 2;;
      -i|--input_model) # Set the model used to provide input data for WPS.
             if [ $2 == "GFS" ] || [ $2 == "NAM" ] ; then
                INPUT_MODEL=$2
             else
                echo "Invalid input model entered; using default."
                echo "Valid options for this script are GFS and NAM."
                echo "Input model entered: " $2
             fi
             shift 2;;
      -p|--plot-output) # Option to plot the output.
             PLOT_OUTPUT=1
             shift;;
      -h|--help) # Print the help message.
             echo -e "Usage:  run_Forecast.bash [OPTION]"
             echo -e "\t--lat LAT\t\tThe latitude of the center of the domain (-90 to 90)."
             echo -e "\t--lon LON\t\tThe longitude of the center of the domain (-180 to 180)."
             echo -e "\t-i, --input_model MODEL\tThe model used by WPS to provide initial conditions and boundary conditions (GFS or NAM)."
             echo -e "\t-h, --help\t\tPrints this help message."
             exit 1;;
      --) shift; break;;
      *) break;;
   esac
done

# Figure out the directory where the script is located.
run_dir=`dirname $0`

# Change the directory to the one in which the script is located.
cd $run_dir

#---------------------------------------------------------------------------
# Compile WRF first.  It must be compiled before WPS can be compiled.

# Change the directory to WRF.
cd ../../

# Copy the custom configuration file for WRF-CLUBB.
cp custom_config_files/configure.wrf.intel64.serial.clubb configure.wrf

# Compile WRF.
./compile em_real

#---------------------------------------------------------------------------
# WPS

# Change the directory to WPS.
cd ../WPS

# Copy the custom configuration file to configure.wps.
cp custom_config_files/configure.wps.intel64.serial.grib2.WRF-CLUBB configure.wps

# Compile WPS.
./compile

# Compile additional utilities plotgrids.exe and plotfmt.exe.
./compile util

# Move the current namelist (namelist.wps) to namelist.wps.default.
mv namelist.wps namelist.wps.default

# Copy namelist.wps.Forecast to namelist.wps.
cp namelist.wps.Forecast namelist.wps

#-----------------------------
# WPS geogrid

# Remove any geo_em.*.nc files already found.
rm geo_em.*.nc

# Edit namelist.wps
sed -i "s/ref_lat.*/ref_lat = "$LAT",/g" namelist.wps
sed -i "s/ref_lon.*/ref_lon = "$LON",/g" namelist.wps
sed -i "s/stand_lon.*/stand_lon = "$LON",/g" namelist.wps

# Run geogrid.exe, which produces the file geo_em.d01.nc.
./geogrid.exe

#-----------------------------
# WPS ungrib

# Change the directory to the DATA directory.
cd ../DATA

# Set up the forecast times and obtain the necessary files for initial
# conditions, boundary conditions, and sea surface temperature.
set_forecast_times_get_data

# Change the directory back to the WPS directory.
cd ../WPS

if [ $INPUT_MODEL == "NAM" ]; then

   # Link the appropriate Vtable for the input data.
   ln -sf ungrib/Variable_Tables/Vtable.NAM Vtable

   # Link the GRIB data.
   ./link_grib.csh ../DATA/Forecast_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z/nam

elif [ $INPUT_MODEL == "GFS" ]; then

   # Link the appropriate Vtable for the input data.
   ln -sf ungrib/Variable_Tables/Vtable.GFS Vtable

   # Link the GRIB data.
   ./link_grib.csh ../DATA/Forecast_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z/gfs

fi

# Remove any FILE: files already found.
rm FILE\:*

# Edit namelist.wps based on the start time and end time of the data files that
# were obtained.
# Edit Start Date.
sed -i "s/start_date.*/start_date = '"$START_UTC_YEAR"-"$START_UTC_MONTH"-"$START_UTC_DAY"_"$START_UTC_HOUR":00:00','"$START_UTC_YEAR"-"$START_UTC_MONTH"-"$START_UTC_DAY"_"$START_UTC_HOUR":00:00',/g" namelist.wps
# Edit End Date.
sed -i "s/end_date.*/end_date   = '"$END_UTC_YEAR"-"$END_UTC_MONTH"-"$END_UTC_DAY"_"$END_UTC_HOUR":00:00','"$END_UTC_YEAR"-"$END_UTC_MONTH"-"$END_UTC_DAY"_"$END_UTC_HOUR":00:00',/g" namelist.wps

# Run ungrib.exe, which produces files that are used to help provide information
# on initial conditions and lateral boundary conditions to WRF.
./ungrib.exe

# Link the appropriate Vtable for the SST input data.
ln -sf ungrib/Variable_Tables/Vtable.SST Vtable

# Link the GRIB data for SST.
./link_grib.csh ../DATA/SST_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z/rtgssthr_grb_0.5.grib2

# Remove any SST: files already found.
rm SST\:*

# Edit namelist.wps based on the time for the SST data file that was obtained.
# Edit Start Date.
sed -i "s/start_date.*/start_date = '"$SST_UTC_YEAR"-"$SST_UTC_MONTH"-"$SST_UTC_DAY"_00:00:00','"$SST_UTC_YEAR"-"$SST_UTC_MONTH"-"$SST_UTC_DAY"_00:00:00',/g" namelist.wps
# Edit End Date.
sed -i "s/end_date.*/end_date   = '"$SST_UTC_YEAR"-"$SST_UTC_MONTH"-"$SST_UTC_DAY"_00:00:00','"$SST_UTC_YEAR"-"$SST_UTC_MONTH"-"$SST_UTC_DAY"_00:00:00',/g" namelist.wps

# Edit the prefix setting in the "ungrib" section of namelist.wps.
sed -i "s/prefix = 'FILE'/prefix = 'SST'/g" namelist.wps

# Run ungrib.exe, which produces files that are used to help provide information
# on sea surface temperature.
./ungrib.exe

#-----------------------------
# WPS metgrid

# Remove any met_em.* files already found.
rm met_em.*

# Edit namelist.wps based on the start time and end time of the data files that
# were obtained.
# Edit Start Date.
sed -i "s/start_date.*/start_date = '"$START_UTC_YEAR"-"$START_UTC_MONTH"-"$START_UTC_DAY"_"$START_UTC_HOUR":00:00','"$START_UTC_YEAR"-"$START_UTC_MONTH"-"$START_UTC_DAY"_"$START_UTC_HOUR":00:00',/g" namelist.wps
# Edit End Date.
sed -i "s/end_date.*/end_date   = '"$END_UTC_YEAR"-"$END_UTC_MONTH"-"$END_UTC_DAY"_"$END_UTC_HOUR":00:00','"$END_UTC_YEAR"-"$END_UTC_MONTH"-"$END_UTC_DAY"_"$END_UTC_HOUR":00:00',/g" namelist.wps
sed -i "s/constants_name.*/constants_name = '.\/SST:"$SST_UTC_YEAR"-"$SST_UTC_MONTH"-"$SST_UTC_DAY"_00'/g" namelist.wps

# Run metgrid.exe, which interpolates the input data onto the module domain.
./metgrid.exe

#-----------------------------

# Move namelist.wps to namelist.wps.Forecast.YYYYMMDD_HHZ.
mv namelist.wps namelist.wps.Forecast."$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z

# Move namelist.wps.default back to namelist.wps.
mv namelist.wps.default namelist.wps

#---------------------------------------------------------------------------
# WRF

# Change the directory to the one in which this script is located.
cd ../WRF/test/em_real

# Remove old met_em.d01.* links.
rm -f met_em.d01.*

# Link the met_em.* files generated by metgrid.exe.
ln -sf ../../../WPS/met_em.d01.* .

# Move the current namelist (namelist.input) to namelist.input.default.
mv namelist.input namelist.input.default

# Copy namelist.input.Forecast namelist.input.
cp namelist.input.Forecast namelist.input

# Edit namelist.input.
sed -i "s/run_days.*/run_days                            = 0,/g" namelist.input
sed -i "s/run_hours.*/run_hours                           = "$FCAST_FILES_HOURS_ADD",/g" namelist.input
sed -i "s/run_minutes.*/run_minutes                         = 0,/g" namelist.input
sed -i "s/run_seconds.*/run_seconds                         = 0,/g" namelist.input
sed -i "s/start_year.*/start_year                          = "$START_UTC_YEAR", "$START_UTC_YEAR", "$START_UTC_YEAR",/g" namelist.input
sed -i "s/start_month.*/start_month                         = "$START_UTC_MONTH",   "$START_UTC_MONTH",   "$START_UTC_MONTH",/g" namelist.input
sed -i "s/start_day.*/start_day                           = "$START_UTC_DAY",   "$START_UTC_DAY",   "$START_UTC_DAY",/g" namelist.input
sed -i "s/start_hour.*/start_hour                          = "$START_UTC_HOUR",   "$START_UTC_HOUR",   "$START_UTC_HOUR",/g" namelist.input
sed -i "s/start_minute.*/start_minute                        = 00,   00,   00,/g" namelist.input
sed -i "s/start_second.*/start_second                        = 00,   00,   00,/g" namelist.input
sed -i "s/end_year.*/end_year                            = "$END_UTC_YEAR", "$END_UTC_YEAR", "$END_UTC_YEAR",/g" namelist.input
sed -i "s/end_month.*/end_month                           = "$END_UTC_MONTH",   "$END_UTC_MONTH",   "$END_UTC_MONTH",/g" namelist.input
sed -i "s/end_day.*/end_day                             = "$END_UTC_DAY",   "$END_UTC_DAY",   "$END_UTC_DAY",/g" namelist.input
sed -i "s/end_hour.*/end_hour                            = "$END_UTC_HOUR",   "$END_UTC_HOUR",   "$END_UTC_HOUR",/g" namelist.input
sed -i "s/end_minute.*/end_minute                          = 00,   00,   00,/g" namelist.input
sed -i "s/end_second.*/end_second                          = 00,   00,   00,/g" namelist.input
sed -i "s/history_interval.*/history_interval                    = 60,   60,   60,/g" namelist.input
if [ $INPUT_MODEL == "NAM" ]; then
   sed -i "s/num_metgrid_levels.*/num_metgrid_levels                  = 43,/g" namelist.input
   sed -i "s/num_metgrid_soil_levels.*/num_metgrid_soil_levels             = 4,/g" namelist.input
   sed -i "s/sf_surface_physics.*/sf_surface_physics                  = 2,     2,     2,/g" namelist.input
elif [ $INPUT_MODEL == "GFS" ]; then
   sed -i "s/num_metgrid_levels.*/num_metgrid_levels                  = 32,/g" namelist.input
   sed -i "s/num_metgrid_soil_levels.*/num_metgrid_soil_levels             = 0,/g" namelist.input
   sed -i "s/sf_surface_physics.*/sf_surface_physics                  = 1,     1,     1,/g" namelist.input
fi

# Remove any wrfinput_d01 and wrfbdy_d01 files that are found.
rm wrfinput_d01
rm wrfbdy_d01

# Run real.exe to generate wrfinput_d01 and wrfbdy_d01 files.
./real.exe

# Run WRF.
./wrf.exe

# Move namelist.input to namelist.input.Forecast.YYYYMMDD_HHZ.
mv namelist.input namelist.input.Forecast."$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z

# Move namelist.input.default back to namelist.input.
mv namelist.input.default namelist.input

#---------------------------------------------------------------------------
# ARWpost

# Change the directory to ARWpost.
cd ../../../ARWpost

# Copy the custom configuration file to configure.arwp.
cp custom_config_files/configure.arwp.intel64.WRF-CLUBB configure.arwp

# Compile ARWpost.
./compile

# Move the current namelist (namelist.ARWpost) to namelist.ARWpost.default.
mv namelist.ARWpost namelist.ARWpost.default

# Copy namelist.ARWpost.Forecast to namelist.ARWpost.
cp namelist.ARWpost.Forecast namelist.ARWpost

# Edit namelist.ARWpost.
sed -i "s/start_date.*/start_date = '"$START_UTC_YEAR"-"$START_UTC_MONTH"-"$START_UTC_DAY"_"$START_UTC_HOUR":00:00',/g" namelist.ARWpost
sed -i "s/end_date.*/end_date   = '"$END_UTC_YEAR"-"$END_UTC_MONTH"-"$END_UTC_DAY"_"$END_UTC_HOUR":00:00',/g" namelist.ARWpost
sed -i "s/interval_seconds.*/interval_seconds = 3600,/g" namelist.ARWpost
sed -i "s/input_root_name.*/input_root_name = '..\/WRF\/test\/em_real\/wrfout_d01_"$START_UTC_YEAR"-"$START_UTC_MONTH"-"$START_UTC_DAY"_"$START_UTC_HOUR":00:00'/g" namelist.ARWpost
sed -i "s/output_root_name.*/output_root_name = '.\/Forecast_"$FCAST_FILES_HOURS_ADD"hr_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z'/g" namelist.ARWpost

# Run ARWpost.
./ARWpost.exe

# Move namelist.ARWpost to namelist.ARWpost.Forecast.YYYYMMDD_HHZ
mv namelist.ARWpost namelist.ARWpost.Forecast."$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z

# Move namelist.ARWpost.default back to namelist.ARWpost.
mv namelist.ARWpost.default namelist.ARWpost

# Plot the output.
if [ $PLOT_OUTPUT -eq 1 ]; then

   # Change directory to the output_plots directory.
   cd output_plots

   # Run the WRF(-CLUBB) plotting script.
   ./plot_wrf_output.bash --clubb-zt ../../WRF/test/em_real/clubb_zt.ctl --clubb-zm ../../WRF/test/em_real/clubb_zm.ctl --clubb-sfc ../../WRF/test/em_real/clubb_sfc.ctl ../Forecast_"$FCAST_FILES_HOURS_ADD"hr_"$START_UTC_YEAR$START_UTC_MONTH$START_UTC_DAY"_"$START_UTC_HOUR"Z.ctl

   # Zip the plots into the plot_Forecast_YYYYMMDD_HHZ.maff file.
   zip -r plot_Forecast_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z.maff output/

   # Send e-mail upon completion.
   echo -e "Your WRF(-CLUBB) forecast is ready!  Please view the attachment." | mail -s "WRF(-CLUBB) forecast" -a plot_Forecast_"$START_UTC_YEAR""$START_UTC_MONTH""$START_UTC_DAY"_"$START_UTC_HOUR"Z.maff bmg2@uwm.edu

fi # $ PLOT_OUTPUT -eq 1
