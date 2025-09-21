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
      SUBROUTINE DIPOLY	
C CHECKED	

       COMMON /DDEBUG/ debug        
      COMMON /CONST/ PI,PIT2,PID2,DUM(5)	

      COMMON /YY/ MODY,Y,PYPR,PYPTH,PYPPH,YR,PYRPR,
     2     PYRPT,PYRPP,YTH,PYTPR
     1  ,PYTPT,PYTPP,YPH,PYPPR,PYPPT,PYPPP	
	  COMMON R(6) /WW/ ID(10),W0,W(400)	
	  EQUIVALENCE (EARTHR,W(2)),(F,W(6)),(FH,W(201))	
CSHA	  DATA MODY/"DIPOLY"/	
	  ENTRY MAGY	
	  SINTH=SIN(R(2))	
	  COSTH=SIN(PID2-R(2))	
	  TERM9=SQRT(1.+3.*COSTH**2)	
	  T1=FH*(EARTHR/R(1))**3/F	
	  Y=T1*TERM9	
	  YR= 2.*T1*COSTH	
	  YTH= T1*SINTH	
	  PYRPR=-3.*YR/R(1)	
	  PYRPT=-2.*YTH	
	  PYTPR=-3.*YTH/R(1)	
	  PYTPT=.5*YR	
	  PYPR=-3.*Y/R(1)	
	  PYPTH=-3.*Y*SINTH*COSTH/TERM9**2	
	  RETURN	
	  END	

CP-143
