#! /bin/sh

make distclean

make clean
make METHOD=1
sleep 15
./Bench -m run --compare= --name=rb

make clean
make METHOD=2
sleep 15
./Bench -m run --compare= --name=ll

./Bench -m graph --compare=rb,ll
