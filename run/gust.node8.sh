#!/bin/bash -l
#PBS -N cm1
#PBS -l select=8:ncpus=128:mpiprocs=128:ompthreads=1
#PBS -l walltime=04:00:00
#PBS -j oe
#PBS -q main@gusched01
#PBS -A UNDM0006


module --force purge
module load ncarenv/23.03
module load nvhpc/23.1
module load ncarcompilers/0.8.0
module load cray-mpich/8.1.25

mpiexec -n 1024 ./cpu.exe --namelist namelist.figureE.cpu >& gust.node8.figureE.512x512c.log
#mpiexec ./cpu.exe --namelist namelist.figureD.cpu >& node=8.figureD.256.log
#mpiexec ./cm1.exe --namelist namelist.figureA.cpu >& gust.node8.figureA.256.log
