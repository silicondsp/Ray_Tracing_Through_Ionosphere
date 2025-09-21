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

	   SUBROUTINE EXPZ2	
C CHECKED
C     COLLISION FREQUENCY PROFILE FROM TWO EXPONENTIALS	
       COMMON /DDEBUG/ debug        
	   COMMON /CONST/ PI,PIT2,PID2,DUM(5)	
	   COMMON /ZZ/ MODZ,Z,PZPR,PZPTH,PZPPH	
	   COMMON R(6) /WW/ ID(10),W0,W(400)	
      EQUIVALENCE (EARTHR,W(2)),(F,W(6)),(NU1,W(251)),(H1,W(252)),	
     1 (A1,W(253)),(NU2,W(254)),(H2,W(255)),(A2,W(256))	
	   REAL NU1,NU2	
CSHA	   DATA (MODZ=6H EXPZ2)	
	   ENTRY COLFRZ	
	   H=R(1)-EARTHR	
	   EXP1= NU1* EXP(-A1*(H-H1))	
	   EXP2= NU2* EXP(-A2*(H-H2))	
	   Z=(EXP1+EXP2)/(PIT2*F*1.E6)	
	   PZPR=(-A1*EXP1-A2*EXP2)/(PIT2*F*1.E6)	
	   
	   if(debug.eq.1.) print 8887,H,R(1),NU1,NU2,A1,A2,
     1        H1,H2,EXP1,EXP2,Z,PZPR
 8887  format("EXPZ2:H,R(1),NU1,NU2,A1,A2,H1,H2,EXP1,EXP2,Z,PZPR",
     1     10(e14.6,2x))
 	   
	   
	   RETURN	
	   END	
	   

CP-157
