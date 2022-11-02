#!/bin/bash
export CUDA_VISIBLE_DEVICES=$PMI_LOCAL_RANK
exec $*
