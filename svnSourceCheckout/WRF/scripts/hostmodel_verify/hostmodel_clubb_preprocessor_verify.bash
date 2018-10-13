#!/bin/bash
#
# $Id: hostmodel_clubb_preprocessor_verify.bash 1336 2014-04-22 22:18:46Z charlass@uwm.edu $
#
###################################################################################################
#
#	hostmodel_clubb_preprocessor_verify.bash
#
#	Purpose: Verify that CLUBB preprocessor directives separate out changes made to the
#                source code.  If successful, no differences between files will be printed out.
#
#
#
#Files not being checked by this script:
#For WRF: phys/Makefile
###################################################################################################

set_args()
{
         # Loop through the list of arguments ($1, $2...). This loop ignores
         # anything not starting with '-'.
         while [ -n "$(echo $1 | grep "-")" ]; do
                 case $1 in
                         # '--nightly' sets the script to run the nightly version.
                         --vendor | -v ) shift
                                        VENDOR=$1;;
                         --clubb | -c ) shift
                       			CLUBB=$1;;
			 --host | -h ) shift
					HOST=$1;;
                              --help )  echo "                                            "
                                        echo "Usage: ./hostmodel_clubb_preprocessor_verify.bash (-v|-c|-h)"
                                        echo "                                            "
					echo "You must specify the host (eg. sam, wrf)"
					echo "						"
                                        echo " This script verifies that CLUBB preprocessor directives"
					echo " separate out any changes made to the baseline Host source."
                                        echo "                                            "
                                        echo " Directories can be specified with these command-line options:"
                                        echo "   -v|vendor    Specify baseline (original or "gold standard") VENDOR directory"
                                        echo "   -c|clubb     Specify working CLUBB directory, with our changes,"
					echo "                    that we hope will reduce to the baseline VENDOR version."
                                        echo "                                            "
                                        echo " To do a run in which both directories are specified:"
                                        echo "   ./hostmodel_clubb_preprocessor_verify.bash -v vendor/ -c clubb/ -h wrf"
                                        echo "                                            "
                                        exit;;
                 esac
                 # Shift moves the parameters up one. Ex: $2 -> $1 and so on.
                 # This is so we only have to check $1 on each iteration.
                 shift
         done
}
preprocess ()
{	
	for tryfile in "$@"; do
		if [ -d "$tryfile" ]; then
			thisfile=$tryfile
			if [ "$HOST" == "sam" ]; then
				FOLDER="/SRC"
				preprocessRecursiveSAM $(command ls $tryfile)
			elif [ "$HOST" == "wrf" ]; then
				preprocessRecursiveWRF $(command ls $tryfile)
			fi
		fi
	done
}

preprocessRecursiveSAM ()
{
	for file in "$@"; do
		thisfile=$thisfile/$file
		if [ -d "$thisfile" ]; then   # If this is a directory, search inside the directory.
			preprocessRecursiveSAM $(command ls $thisfile)
		else
			if echo "$file" | grep -q .F90; then             # Search for all .F90 files.
				g95 -E $thisfile > ${thisfile%.F90}.f90  # Preprocess and output to a .f90 file.

				if [ -e ${thisfile%.F90}.f90 -a ! -s ${thisfile%.F90}.f90 ]; then
					rm ${thisfile%.F90}.f90   #   Once preprocessed .f90 file has been created, 
					                          # remove .F90 original,
				fi

				rm $thisfile
			fi

		fi
		thisfile=${thisfile%/*}
	done
}


preprocessRecursiveWRF()
{
for file in "$@"; do
                thisfile=$thisfile/$file
                if [ -d "$thisfile" ]; then   # If this is a directory, search inside the directory.
                        preprocessRecursiveWRF $(command ls $thisfile)
                else
                    	if echo "$file" | grep -q -E '\.F|\.inc'; then             # Search for all .F files.
                                

				#This will remove all flags that are not commented
				fpp -free -macro=no -f_com=no -P $thisfile  > ${thisfile}.F 2>/dev/null # Preprocess and output to a .f file	
                                
				#Remove orignal and replace with the our new copy
				rm $thisfile
				mv ${thisfile}.F $thisfile

				sed -i -e '/.*$Id:.*/d' -e '/^!-*/d'  $thisfile
				sed -i '1d' $thisfile

			elif echo "$file" | grep -q .EM; then
#				sed -i -e 's|#|!#|' -e 's|ifdef|#ifdef|' -e 's|endif|#endif|' $thisfile
#				sed -i 's|#ifdef RUC_CLOUD|ifdef RUC_CLOUD|' $thisfile
				fpp -free -macro=no -f_com=no $thisfile  > ${thisfile}.F 2>/dev/null # Preprocess and output to a .f file
				rm $thisfile
				mv ${thisfile}.F $thisfile
				sed -i -e 's|!#|#|' -e 's|#/\*TESTCASES\*/||' -e 's|#/\*CLUBB\*/||' -e 's|#/\*ESRL\*/||' -e 's|#/\*VOCALS_RF06\*/||' \
					-e 's|^# [0-9]\+[^A-Za-z]\+||' \
					-e '1d' -e '/ifndef TESTCASES/d' -e '/ifndef ESRL/d' -e '/ifndef VOCALS_RF06/d' $thisfile

			elif [ "$file" == "SOURCEME_WINCSS_WRF" ]; then
				sed -i '1d' $thisfile
			fi
                fi
                thisfile=${thisfile%/*}
done
}



#This copies the source files into a temp directory
#The source files are different for WRF and SAM
copySrcFiles ()
{

	if [ "$HOST" == "wrf" ]; then

          # Delete files with known differences in WRF-CLUBB 
		cp -R $CLUBB/* /tmp/tempdir/clubb
		rm /tmp/tempdir/clubb/phys/Makefile
		rm /tmp/tempdir/clubb/frame/Makefile
		rm /tmp/tempdir/clubb/Registry/Registry.EM*
		rm /tmp/tempdir/clubb/Registry/registry.dimspec
		rm /tmp/tempdir/clubb/clean
		rm /tmp/tempdir/clubb/run/README.namelist
          rm /tmp/tempdir/clubb/dyn_em/depend.dyn_em
          rm /tmp/tempdir/clubb/main/depend.common

          # Delete files with known differences in vendor WRF
		cp -R $VENDOR/* /tmp/tempdir/vendor
		rm /tmp/tempdir/vendor/phys/Makefile
		rm /tmp/tempdir/vendor/frame/Makefile
		rm /tmp/tempdir/vendor/Registry/Registry.EM*
		rm /tmp/tempdir/vendor/Registry/registry.dimspec
		rm /tmp/tempdir/vendor/clean
		rm /tmp/tempdir/vendor/run/README.namelist
          rm /tmp/tempdir/vendor/dyn_em/depend.dyn_em
          rm /tmp/tempdir/vendor/main/depend.common
		mv /tmp/tempdir ./
	elif [ "$HOST" == "sam" ]; then		
		cp -R $CLUBB/SRC tempdir/clubb/		
		cp -R $VENDOR/SRC tempdir/vendor/
	fi
}


# Compare subdirectories recursively so we get all of the files
compareSubDir()
{

#Only do this the first time
if $first ; then
	cur=`pwd`
	cd tempdir/clubb/$FOLDER
	first=false
fi

for file in "`pwd`/$@"; do
	thisfile=$file	

	if [ -d  "$thisfile" ]; then
		cd $thisfile

		#Print the correct stuff, send incorrect stuff to /dev/null
		if [ "$thisfile" == "`pwd`/$thilsfile" ]; then
			echo "#####################################################################"
                	echo "Folder is: "$thisfile
               		echo "#####################################################################"
			compareSubDir $(command ls $thisfile)
		else
			compareSubDir $(command ls $thisfile) 2> /dev/null
		fi
		if [ "`pwd`" != "$cur/tempdir/clubb$FOLDER" ]; then
			cd ..
		fi
	else
		#Switch clubb to vendor
		vendorfile=`pwd | sed 's|tempdir/clubb|tempdir/vendor|'`

		#Revmoes endifs and elses from vendor code
		#if echo $thisfile | grep -q -E '.F|.F90|.EM|.inc' ; then
		#	sed -i -e '/!\?#\?[ ]*endif/d' -e '/^#[ ]*else/d' $vendorfile/$thisfile 
		#fi
		
		#Cleans up the mess made by the preprocessor in .*EM* files
		if echo $thisfile | grep -q -E '\.EM' ; then
			sed -i 's|&$|\\|' $thisfile
		fi

		diff -wBb --ignore-all-space --ignore-blank-lines `pwd`/$file $vendorfile/$file
	
		#Only display file name if a difference is found
	        if [ "$?" -eq 1 ]; then 
			echo $thisfile
			echo "---------------------------------------------------------------------"
        	fi
	fi
done
}

##########################################################################################
#	Begin Script
##########################################################################################

orig_path=`pwd`

# Process command-line arguments.
set_args $*

#Makes sure that the host was set
if [ "$HOST" != sam ] && [ "$HOST" != wrf ]; then
	echo "You must set the host, try --help for help"
	exit -1
fi


#Set the default path for VENDOR
if [ -z $VENDOR ]; then
	if [ "$HOST" == "wrf" ]; then
		VENDOR=~/wrf/vendor/current/WRF      # Baseline or original or "gold standard" version.
	elif [ "$HOST" == "sam" ]; then
		VENDOR=~/SAM6.6_PHILLIPS      # Baseline or original or "gold standard" version.
	fi
fi

#Set the default path for CLUBB
if [ -z $CLUBB ]; then
        if [ "$HOST" == "wrf" ]; then
                CLUBB=~/wrf/trunk/WRF                   # Working version with our changes.
        elif [ "$HOST" == "sam" ]; then
                CLUBB=~/SAM6.6_CLUBB                   # Working version with our changes.
        fi
fi

#Display the paths
echo "The vendor path is " $VENDOR
echo "The CLUBB path is " $CLUBB


# Remove the previous temporary directory
if [ -e tempdir ]; then
	rm -rf tempdir/
fi
# Remove the previous temporary directory
if [ -e /tmp/tempdir ]; then
	rm -rf /tmp/tempdir/
fi

# Process the CLUBB directory so it can be compared with the baseline.
mkdir /tmp/tempdir     # Create a temporary directory to serve as scratch space.
mkdir /tmp/tempdir/clubb
mkdir /tmp/tempdir/vendor
copySrcFiles
preprocess tempdir/clubb
preprocess tempdir/vendor

#Compare
first=true
compareSubDir /


cd $orig_path
#rm -rf tempdir/

##########################################################################################
#	End Script
##########################################################################################

