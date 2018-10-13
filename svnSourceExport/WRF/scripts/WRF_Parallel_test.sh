#!/bin/sh

echo "Configuring WRF..."
cd WRF/
cp custom_config_files/configure.wrf.intel64.dmpar.clubb configure.wrf
cp test/em_quarter_ss/namelist.input.parallel_test test/em_quarter_ss/namelist.input
export PATH=/usr/local/mpi/mpich2-1.4.1p1-intel/bin:$PATH

echo "Compiling WRF..."
./compile em_quarter_ss

cd test/em_quarter_ss

echo "Running with 2 processors..."
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 2 ./ideal.exe
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 2 ./wrf.exe
ncdump wrfout_d01_0001-01-01_00\:00\:00 > n2dump || exit 1

echo "Running with 4 processors..."
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./ideal.exe
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./wrf.exe
ncdump wrfout_d01_0001-01-01_00\:00\:00 > n4dump || exit 1

echo "Diff-ing output..."
diff n2dump n4dump > diffdump

if [[ -s diffdump ]] ; then
  echo "ERROR: A difference has been found."
  exit 1
else
  echo "No differences have been found."
  exit 0
fi ;
