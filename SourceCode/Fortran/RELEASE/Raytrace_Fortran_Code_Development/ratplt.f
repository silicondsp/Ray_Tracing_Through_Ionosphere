C   OT REPORT 75-76
C   A VERSATILE THREE·DIMENSIONAL RAY TRACING COMPUTER
C   PROGRAM FOR RADIO WAVES
C   IN THE IONOSPHERE
C   
C   R. MICHAEL JONES
C   JUDITH J. STEPHENSON
C   
C   U.S.  DEPARTMENT OF COMMERCE
C   October 1975
C
C    FORTRAN Code from Scanned Report and OCR in 2002 
C    by Sasan Ardalan
C
C    Compiles and Links using gfortran
C    
C    Rework in December 2018 by Sasan Ardalan
C
C    Detailed work to match source code with published code.
C    Major work to change from CDC Main Frame Fortran
C    to gfortran
C
C    Massive effort in debugging.
C    Also debug option for  printing of variables
C
C    This FORTRAN Code available at:
C        http://www.radiocalc.com
C
C    Visit the site for plots of raytracing
C    Also translation to Mathematica by Sasan Ardalan
C
C    Do Not Remove This Notice
C
C

	    SUBROUTINE RAYPLT	
C		RFPLACES SUBROUTINES RAYPLT,PLOT, AND LABPLT IF PLOTS ARE	
C		NOT WANTED OR IF A PLOTTER IS NOT AVAILABLE	
	    COMMON /WW/ ID(10),W0,W(400)	
	    EQUIVALENCE (PLT,W(81))	
	    PLT=0.		
	    ENTRY ENDPLT	
	    RETURN		
		END		
		

CP-84
