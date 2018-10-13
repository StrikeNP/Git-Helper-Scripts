#!/bin/sh

echo "Configuring WRF..."
cd WRF/
cp custom_config_files/configure.wrf.intel64.dmpar.clubb configure.wrf
cp test/em_quarter_ss/namelist.input.restart_test_orig test/em_quarter_ss/namelist.input
export PATH=/usr/local/mpi/mpich2-1.4.1p1-intel/bin:$PATH

echo "Compiling WRF..."
./compile em_quarter_ss

cd test/em_quarter_ss

echo "Initial run..."
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./ideal.exe
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./wrf.exe

mkdir runOrig
mv wrfr* wrfo* clubb* namelist.output runOrig

echo "Configure restart..."
cp namelist.input.restart_test namelist.input
cp runOrig/wrfrst_d01_0001-01-01_00:30:00 .

echo "Restart run..."
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./ideal.exe
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./wrf.exe

mkdir runRestart
mv wrfr* wrfo* clubb* namelist.output runRestart

echo "ncdumping output..."
ncdump runOrig/wrfrst_d01_0001-01-01_01:00:00 > origDump
ncdump runRestart/wrfrst_d01_0001-01-01_01:00:00 > restartDump

echo "Diff-ing output..."
diff origDump restartDump > diffdump

LASTLINE=$(tail -1 diffdump)
if [[ $LASTLINE == *WRF_ALARM_SECS_TIL_NEXT_RING_55* ]] ; then
  echo "No differences have been found."
  exit 0
else
  echo "ERROR: The two output files are different.."
  exit 1
fi ;
