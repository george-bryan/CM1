#!/bin/bash -l
#PBS -N cm1
#PBS -l select=1:ncpus=4:mpiprocs=4:ompthreads=1
#PBS -l walltime=00:40:00
#PBS -j oe
#PBS -q main
#PBS -A UNDM0006

module --force purge
module load ncarenv intel cray-mpich ncarcompilers

mpiexec -n 4 -ppn 4 ./cpu.exe --namelist namelist_ASD.verify >& derecho.mpi.ifort.namelist.ASD.log
