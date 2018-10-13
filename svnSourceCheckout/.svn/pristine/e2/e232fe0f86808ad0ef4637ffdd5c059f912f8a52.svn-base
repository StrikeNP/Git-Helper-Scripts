#!/bin/sh
#
# $Id$
#
#==============================================================================
# THIS SCRIPT CHECKS OUT WRF-CLUBB OF A GIVEN WRF-CLUBB REVISISON AND A GIVEN
# CLUBB REVISION.  
# 
# NOTE: 
# THIS SCRIPT WILL NOT WORK CORRECTLY IF THE GIVEN CLUBB REVISION IS OLDER THEN
# clubb:r6774 (SINCE THE SILHS DIRECTROY IN CLUBB WAS RENAMED IN THIS REVISION).
# IN THIS CASE YOU HAVE TO REPLACE ALL FILES IN /WRF/phys/silhs/ MANUALLY BY
# THE FILES OF THE DESIRED REVISION. THE BEST WAY TO DO THIS IS TO HAVE A
# SEPARATE CLUBB CHECKOUT, UPDATE THIS ONE TO THE DEISRED REVISION AND COPY THE
# FILES INTO /WRF/phys/silhs/!
# 
#==============================================================================
myhelp()
{
echo ""
echo "------------------------------------------------------------------------------------"
echo "This script can be used to checkout older revisons of WRF-CLUBB. It will"
echo "automatically update all externals pulled from CLUBB to a specific (matching)"
echo "revision."
echo ""
echo "The script can be used in two differnt ways:"
echo "  1. The script checks out (SVN) WRF-CLUBB of WRF-CLUBB revision <wrfr>. All"
echo "     externals pulled from CLUBB will be updated to CLUBB revision <clubbr>. In this"
echo "     case you manually have to specify matching revisions of WRF-CLUBB and CLUBB. This"
echo "     can be done by looking at the timeline on the trac page of CLUBB."
echo ""
echo "  2. You can specify a specific date (and time) and this script checks out WRF-CLUBB"
echo "     and the CLUBB externals at revisions which were up-to-date at this specific."
echo "     date. Dates can be specified for example by:"
echo "              -t 2014-02-17"
echo "              -t \"2014-02-17 15:30\""
echo "              -t 20140217T1530"
echo "              -t 2014-02-17T15:30-04:00"
echo "     For detailed information see http://svnbook.red-bean.com/en/1.7/svn.tour.revs.specifiers.html"
echo ""
echo "All files will be checked out to the directory <dir>." 
echo ""
echo "If neither -w <wrfr>, -c <clubbr> nor -t <date> are specified the latest revison is"
echo "checked out." 
echo ""
echo "It is the users reponsibility to specify existing and matching revision numbers!"
echo "It is the users reponsibility to specify valid strings for the date!"
echo ""
echo "Usage: ./checkout_wrf_clubb.sh -d <dir> [[-w <wrfr>] [-c <clubbr>] | -t <date> ]"
echo ""
echo "Options:"
echo "-d -- directory in which WRF-CLUBB is checked out"
echo "-w -- WRF-CLUBB revision"
echo "-c -- CLUBB revision"
echo "-h -- show this page"
echo "-t -- date at which WRF-CLUBB and CLUBB are checked out"
echo ""
echo "------------------------------------------------------------------------------------"
}


DIR=""
CLUBBREV=""
WRFREV=""
TIME=""

# FILTER OPTIONS
while getopts d:c:w:ht: opt
do
  case "$opt" in
    d) DIR="$OPTARG";;
    c) CLUBBREV="$OPTARG";;
    w) WRFREV="$OPTARG";;
    t) TIME="$OPTARG";;
    h) myhelp; exit;;
    \?) echo "Error: Unknown option."; myhelp; exit 1;;
  esac
done

# Switch to current directory
cd $PWD

# Check if there is 
if ([ "$CLUBBREV" = "" ] || [ "$WRFREV" = "" ]) && [ "$TIME" = "" ]; then
  echo ""
  echo "Error: You have to either specify revision numbers for WRF"
  echo "and CLUBB or a specific date!"
  echo ""
  myhelp
  exit 1
fi


# CHECK IF NOT REVISION NUMBER AND DATE GIVEN AT THE SAME TIME
if ([ "$CLUBBREV" != "" ] || [ "$WRFREV" != "" ]) && [ "$TIME" != "" ]; then
  echo ""
  echo "Error: You can either specify revision numbers for WRF"
  echo "and CLUBB or a specific date, but not both!"
  echo ""
  myhelp
  exit 1
fi



# DIRECTORY MUST BE SPECIFIED
if [ "$DIR" = "" ]; then
  echo ""
  echo "Error: You have to specify a directory name for the check out!"
  echo ""
  myhelp 
  exit 1
fi


# CHECKOUT SPECIFIED BY DATE
if [ "$TIME" != "" ]; then

  # CHECKOUT WRF-CLUBB (CLUBB EXTERNALS ARE AT THE LATEST REVISION)
  svn co http://carson.math.uwm.edu/repos/wrf/trunk/WRF $DIR '-r{'$TIME'}'

  # THE UPDATE IS NECESSARY IF THE CHECKOUT DIRECTORY ALREADY EXISTS
  svn update $DIR '-r{'$TIME'}'

  # UPDATE CLUBB EXTERNALS
  svn update $DIR/test/clubb_input/stats/ '-r{'$TIME'}'
  svn update $DIR/test/clubb_input/tunable_parameters/ '-r{'$TIME'}'
  svn update $DIR/phys/clubb/ '-r{'$TIME'}'
  svn update $DIR/phys/silhs/ '-r{'$TIME'}'
  svn update $DIR/phys/microutils/ '-r{'$TIME'}'

else # CHECKOUT SPECIFIED BY REVISION NUMBERS

  # TEST IF CLUBB REVISION IS AN INTEGER OR EMPTY
  re='^[0-9]+$'
  if ! [[ $CLUBBREV = "" ]] ; then
    if ! [[ $CLUBBREV =~ $re ]] ; then
      echo ""
      echo "Error: CLUBB's revision number must be an integer!"
      echo ""
      exit 1
    else
      CLUBBREV='-r'$CLUBBREV
    fi
  fi

  # TEST IF CLUBB REVISION IS AN INTEGER OR EMPTY
  if ! [[ $WRFREV = "" ]] ; then
    if ! [[ $WRFREV =~ $re ]] ; then
      echo ""
      echo "Error: WRF's revision number must be an integer!"
      echo ""
      exit 1
    else
      WRFREV='-r'$WRFREV
    fi
  fi

  # CHECKOUT WRF-CLUBB (CLUBB EXTERNALS ARE AT THE LATEST REVISION)
  svn co http://carson.math.uwm.edu/repos/wrf/trunk/WRF $DIR $WRFREV

  # THE UPDATE IS NECESSARY IF THE CHECKOUT DIRECTORY ALREADY EXISTS
  svn update $DIR $WRFREV

  # UPDATE CLUBB EXTERNALS
  svn update $DIR/test/clubb_input/stats/ $CLUBBREV
  svn update $DIR/test/clubb_input/tunable_parameters/ $CLUBBREV
  svn update $DIR/phys/clubb/ $CLUBBREV
  svn update $DIR/phys/silhs/ $CLUBBREV
  svn update $DIR/phys/microutils/ $CLUBBREV

fi
