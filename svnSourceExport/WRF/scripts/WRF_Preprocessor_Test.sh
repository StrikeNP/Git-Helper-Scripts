#!/bin/sh

svn co $SVNROOT/wrf/trunk wrf_clubb
svn co $SVNROOT/wrf/vendor/WRFV3.5.1 wrf_vendor

echo "Configure WRF..."
cd wrf_clubb/WRF/
cp custom_config_files/configure.wrf.intel64.dmpar.clubb configure.wrf
export PATH=/usr/local/mpi/mpich2-1.4.1p1-intel/bin:$PATH

sed -i 's/bl_pbl_physics.*/bl_pbl_physics                      = 1,     1,     1,/' test/em_quarter_ss/namelist.input
sed -i 's/mp_physics.*/mp_physics                          = 10,    10,    10,/' test/em_quarter_ss/namelist.input
sed -i 's/sf_sfclay_physics.*/sf_sfclay_physics                   = 1,     1,     1,/' test/em_quarter_ss/namelist.input

sed -i '/DCLUBBSTATS/d' configure.wrf

echo "Compile WRF with CLUBB preprocessors..."
./compile em_quarter_ss

cd test/em_quarter_ss

echo "Run with CLUBB preprocessors..."
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./ideal.exe
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./wrf.exe

echo "Configure run without preprocessors..."
cd ../../../../
cp wrf_clubb/WRF/custom_config_files/configure.wrf.intel64.dmpar.default wrf_vendor/WRF/configure.wrf
cd wrf_vendor/WRF/
sed -i 's/ icc/ gcc/g' configure.wrf
sed -i 's/CFLAGS_LOCAL    =.*/CFLAGS_LOCAL    =       -w -O3 -g #-xHost -fp-model fast=2 -no-prec-div -no-prec-sqrt -ftz -no-multibyte-chars/g' configure.wrf
sed -i 's/=       mpicc/=       mpicc -DMPI2_SUPPORT/g' configure.wrf
sed -i 's/=       $(DM_CC)/=       $(DM_CC) -DFSEEKO64_OK/g' configure.wrf
export PATH=/usr/local/mpi/mpich2-1.4.1p1-intel/bin:$PATH

sed -i 's/bl_pbl_physics.*/bl_pbl_physics                      = 1,     1,     1,/' test/em_quarter_ss/namelist.input
sed -i 's/mp_physics.*/mp_physics                          = 10,    10,    10,/' test/em_quarter_ss/namelist.input
sed -i 's/sf_sfclay_physics.*/sf_sfclay_physics                   = 1,     1,     1,/' test/em_quarter_ss/namelist.input


echo "Compile WRF without CLUBB preprocessors..."
./compile em_quarter_ss

cd test/em_quarter_ss

echo "Run without CLUBB preprocessors..."
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./ideal.exe
/usr/local/mpi/mpich2-1.4.1p1-intel/bin/mpiexec -n 4 ./wrf.exe

cd ../../../../
ncdump wrf_clubb/WRF/test/em_quarter_ss/wrfout_d01_0001-01-01_00\:00\:00 > clubbdump || exit 1
ncdump wrf_vendor/WRF/test/em_quarter_ss/wrfout_d01_0001-01-01_00\:00\:00 > vendordump || exit 1
diff clubbdump vendordump | grep [1-9] | egrep -v "(SGS|CLUBB|LH|SIGMA)"|grep -v '[0-9]*,[0-9]*d[0-9]*' > diffdump

if [[ -s diffdump ]] ; then
  echo "ERROR: One or more differences have been found."
  exit 1
else
  echo "No differences have been found."
  exit 0
fi ;

