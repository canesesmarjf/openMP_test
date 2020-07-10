#export OMP_NUM_THREADS=256
make -f makefile.p2
time ./compute_pi
