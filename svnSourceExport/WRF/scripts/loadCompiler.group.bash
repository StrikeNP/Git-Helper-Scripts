#!/bin/bash
#
# $Id$
#


# Intel MPICH 1:
INTELPATH=/opt/mpich/intel/bin

# Intel MPICH 2:
#INTELPATH=/usr/local/mpi/mpich2-1.4.1p1-intel/bin


# PGI MPICH 1
PGIPATH=/opt/pgi/linux86-64/2013/mpi/mpich/bin

# PGI MPICH 2
#PGIPATH=/usr/local/mpi/mpich2-1.4.1p1-pgi/bin

# GFortran MPICH 2:
GFORTRANPATH=/usr/local/mpi/mpich2-1.4.1p1-gfortran/bin


# Switch to current directory
cd $PWD


TMPFILE="compiler.source"

### parse options
while getopts hmnpc:t: opt
do
  case "$opt" in
    c) COMPILER="$OPTARG";;
    h) myhelp; exit;;
    \?) echo "Error: Unknown option."; myhelp; exit;;
  esac
done

if [ "$COMPILER" != "intel" ] && [ "$COMPILER" != "pgi" ] && [ "$COMPILER" != "gfortran" ]; then 
  echo ""
  echo "Error: The compiler option $COMPILER not valid!"
  echo ""
  echo "Choose one of the compiler options intel, pgi, gfortran."
  echo ""
  myhelp
  exit 1
fi

# Intel
if [ "$COMPILER" = "intel" ]; then
  echo "export PATH=$INTELPATH:$PATH" > $TMPFILE
# PGI
elif [ "$COMPILER" = "pgi" ]; then
  echo "export PATH=PGIPATH:$PATH" > $TMPFILE
# GFortran
elif [ "$COMPILER" = "gfortran" ]; then
  echo "export PATH=:GFORTRANPATH$PATH" > $TMPFILE
fi

echo ""
echo "Type the following to load the compiler:"
echo ""
echo "     source compiler.source"
echo ""


