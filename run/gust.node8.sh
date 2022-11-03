#!/bin/bash -l
#PBS -N cm1
#PBS -l select=8:ncpus=128:mpiprocs=128:ompthreads=1
#PBS -l walltime=04:00:00
#PBS -j oe
#PBS -q main@gusched01
#PBS -A UNDM0006


export PALS_NRANKS=1024
export PALS_PPN=128
export PALS_DEPTH=1
export PALS_CPU_BIND=depth

#mpiexec ./cm1.exe --namelist namelist.figureE.cpu >& node=8.figureE.512x512.log
#mpiexec ./cpu.exe --namelist namelist.figureD.cpu >& node=8.figureD.256.log
mpiexec ./cpu.exe --namelist namelist.figureA.cpu >& gust.node8.figureA.256.log
