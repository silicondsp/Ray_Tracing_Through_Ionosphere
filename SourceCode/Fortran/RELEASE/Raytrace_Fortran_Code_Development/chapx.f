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

      SUBROUTINE ELECTX()
CSHA Was CHAP 	
C CHECKED

C	CHAPMAN LAYER  WITH TILTS, RIPPLES9 AND GRADIENTS 	

C	W(104)	= 0.5 FOR AN ALPHA-CHAPMAN LAYER	
C		= 1.0 FOR A BETA-CHAPMAN LAYER	
       COMMON /DDEBUG/ debug        
      COMMON /CONST/ PI,PIT2,PID2,DUM(5)	
      COMMON /XX/ MODX(2),X,PXPR,PXPTH,PXPPH,PXPT,HMAX	
      COMMON R(6) /WW/ ID(10),W0,W(400)	
      EQUIVALENCE (THETA,R(2))	
      EQUIVALENCE (EARTHR,W(2)),(F,W(6)),(FC,W(101)),(HM,W(102)),	
     1  (SH,W(103)),(ALPHA,W(104)),(A,W(105)),(B,W(106)),(C,W(107)), 	
     2  (E,W(108)),(PERT,W(150))	
CSHA      DATA MODX(1)/" CHAPX"/
          MODX(1)= 6H CHAPX

	
      	
C     iRINT 1000,R(1)
 1000 FORMAT("USING CHAPX",e14.6); 
      THETA2=THETA-PID2	
      HMAX=HM+EARTHR*E*THETA2	
      if(debug.eq.1.) print 8811,HMAX,HM,EARTHR,E,THETA2
 8811  format("chap2:HMAX,HM,EARTHR,E,THETA2",5(2x,e14.6))


      H=R(1)-EARTHR	
      Z=(H-HMAX)/SH	
      D=0.	
      IF (B.NE.0.) D=PIT2/B		
      TEMP=1.+A*SIN(D*THETA2)+C*THETA2	
      EXZ=1.-EXP(-Z)	
      X=(FC/F)**2*TEMP*EXP(ALPHA*(EXZ-Z))	
      PXPR= -ALPHA*X*EXZ/SH	
      PXPTH=X*(D*A*SIN(PID2-D*THETA2)+C)/TEMP-PXPR*EARTHR*E	
CSHA     PXPPH=0
      IF (PERT.NE.0.) CALL ELECT1
	   if(debug.eq.1.) print 8887,X,R(1),H,HM,HMAX,E,SH,Z
 8887  format("chap1:X,R(1),H,HM,HMAX,E,SH,Z",
     7      8(e14.6,2x,e14.6,2x))
      
      if(debug.eq.1.) print 8888,F,FC,D,B,Z,ALPHA,SHA,E,THETA,THETA2,
     5 EXZ,X,PXPR,PXPTH
 8888  format("chap2:F,FC,D,B,Z,ALPHA,SHA,E,THETA,THETA2,EXZ,X,PR,PH",
     8      7(e14.6,2x,e14.6,2x))
      
      if(debug.eq.1.) print 8889,TEMP
 8889  format("chap2:TEMP",e14.6)

     		
      RETURN	
      END	 

CP-116
