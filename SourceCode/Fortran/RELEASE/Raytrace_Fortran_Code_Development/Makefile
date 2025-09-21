#
# Makefile for raypath
# Builds:
# 3D Ray Tracing Computer Program for Radio Waves in the Ionosphere
# OT  Report 75-76
# R. Michael Jones
# Judith J/ Stephenson
# Office of Telecommunications
# October 1975
#
# Author: Sasan Ardalan
# Date: July 28, 2002
#
all: raypath

back_up.o: back_up.f
	gfortran -c -g back_up.f

chapx.o: chapx.f
	gfortran -c -g chapx.f

dipoly.o:  dipoly.f
	gfortran -c -g dipoly.f

nitial.o: nitial.f
	gfortran -c -g nitial.f

pol_car.o: pol_car.f
	gfortran -c -g pol_car.f


printr.o:  printr.f
	gfortran -c -g printr.f


ratplt.o: ratplt.f
	gfortran -c -g ratplt.f

reach.o: reach.f
	gfortran -c -g reach.f

read_w.o: read_w.f
	gfortran -c -g read_w.f
 
rkam.o: rkam.f
	gfortran -c -g rkam.f

hamltn.o: hamltn.f
	gfortran -c -g hamltn.f

ahwfwc.o: ahwfwc.f
	gfortran -c -g ahwfwc.f

expz2.o: expz2.f
	gfortran -c -g expz2.f


trace.o: trace.f
	gfortran -c -g trace.f

wave.o: wave.f
	gfortran -c -g wave.f

raypath: back_up.o chapx.o dipoly.o nitial.o pol_car.o printr.o ratplt.o reach.o read_w.o rkam.o hamltn.o  ahwfwc.o expz2.o trace.o wave.o
	gfortran -o raypath  -g back_up.o chapx.o dipoly.o nitial.o pol_car.o printr.o ratplt.o reach.o read_w.o rkam.o hamltn.o  ahwfwc.o expz2.o trace.o wave.o
