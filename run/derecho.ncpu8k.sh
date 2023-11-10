#!/bin/bash
#PBS -N cm1
#PBS -l select=64:ncpus=128:mpiprocs=128:ompthreads=1:mem=235GB
#PBS -l walltime=04:00:00
#PBS -j oe
#PBS -q main
#PBS -A NTDD0004

module --force purge
module load ncarenv/23.06 intel cray-mpich/8.1.25 ncarcompilers/1.0.0
# mpiexec -n 8192 -ppn 128 ./cpu.exe --namelist namelist.scale1k.gpu >& derecho.intel.ncpu8k.scale.1024.log
mpiexec -n 8192 -ppn 128 ./cpu.intel.exe --namelist namelist.scale1k.cpu >& derecho.intel.ncpu8k.scale.1024.log


#module --force purge
#module load ncarenv/23.06 nvhpc/23.5 cray-mpich/8.1.25 ncarcompilers/1.0.0
#mpiexec -n 8192 -ppn 128 ./cpu.nvhpc.exe --namelist namelist.scale1k.cpu >& derecho.nvhpc.ncpu8k.scale.1024.log




