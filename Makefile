CC = gfortran

EWNCgrid: EWN_Cgrid.f90
	${CC} -o EWNCgrid EWN_Cgrid.f90
