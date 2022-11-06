#!/bin/bash -l
#PBS -N cm1
#PBS -l select=1:ncpus=36:mpiprocs=36:ompthreads=1
#PBS -l walltime=04:00:00
#PBS -j oe
#PBS -q regular
#PBS -A NTDD0004


#mpiexec ./cm1.exe --namelist namelist.figureE.cpu >& node=8.figureE.512x512.log
#mpiexec ./cpu.exe --namelist namelist.figureD.cpu >& node=8.figureD.256.log
mpiexec_mpt ./cm1.exe --namelist namelist.figureA.cpu >& chey.orig.nvhpc22.5.node1.figureA.256.log
