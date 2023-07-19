#!/bin/bash -l
#PBS -N cm1
#PBS -l select=1:ncpus=2:mpiprocs=2:ompthreads=1
#PBS -l walltime=00:40:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module --force purge
module load ncarenv intel cray-mpich ncarcompilers

mpiexec -n 2 -ppn 2 ./cpu.exe --namelist namelist_ASD.verify >& gust.mpi.ifort.namelist.ASD.log
