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


        SUBROUTINE HAMLTN	
C CHECKED
C#******** CALCULATES HAMILTONS EQUATIONS FOR RAY TRACING 
C        IMPLICIT NONE
        REAL PI,PIT2,PID2,DEGS,RAD,C
        REAL COLL,FIELD,SPACE	

       COMMON /DDEBUG/ debug        
	    COMMON /CONST/ PI,PIT2,PID2,DEGS,RAD,K,C,LOGTEN	
	    COMMON /RIN/ MODRIN(3),COLL,FIELD,SPACE,KAY2,KAY2I,	
     1  H,HI,PHPT,PHPTI,PHPR,PHPRI,PHPTH,PHPTHI,PHPPH,PHPPHI
     2, PHPOM,PHPOMI,PHPKR,PHPKRI,PHPKTH,PHPKTI,PHPKPH,PHPKPI	
     3 ,KPHPK,KPHPKI,POLAR,POLARI,LPOLAR,LPOLRI 	

       COMMON R(20),T,STP,DRDT(20) /WW/ ID(10),W0,W(400)	
       EQUIVALENCE (TH,R(2)),(PH,R(3)),(KR,R(4)),(KTH,R(5)),(KPH,R(6)),	
     1(DTHDT,DRDT(2)),(DPHDT,DRDT(3)),(DKRDT,DRDT(4)),(DKTHDT,DRDT(5)),
     2(DKPHDT,DRDT(6)),(F,W(6))	

       REAL KR,KTH,KPH,KPHPK,KPHPKI,LPOLAR,LPOLRI,LOGTEN,K,KAY2,KAY2I	
        if(debug.eq.1.) print 7723, R(1)
7723    format("HAMLTN ENTERED:R(1):",(e14.6,2x))

	   if (debug.eq.1.) print 8887,KR,KTH,KPH,K2
 8887  format("hamltn222: KR,KTH,KPH,K2",3(e14.6,2x),I4)

		
		
		OM=PIT2*1.E6*F		
		STH=SIN(TH)		
		CTH=SIN(PID2-TH)	
		RSTH=R(1)*STH		
		RCTH=R(1)*CTH	
        if(debug.eq.1.) print 7778, R(1)
7778    format("HAMLTN CALLING RINDEX2:R(1):",(e14.6,2x))
			
		CALL RINDEX		
        if(debug.eq.1.) print 7774, R(1)
7774    format("HAMLTN AFTER  CALLING RINDEX2:R(1):",(e14.6,2x))
		
CSHA CHECK THIS WAS DRDT WHICH IS ARRAY. Using DRDT1 instead.

         DRDT1=-PHPKR/ (PHPOM*C)
               	
         DTHDT=-PHPKTH/ (PHPOM*R(1) *C)	
	 DPHDT=-PHPKPH/ (PHPOM*RSTH*C)	
         DKRDT=PHPR/(PHPOM*C) +KTH*DTHDT+KPH*STH*DPHDT	
         DKTHDT= (PHPTH/ (PHPOM*C) -KTH*DRDT1+KPH*RCTH*DPHDT)/R(1)	
         DKPHDT=(PHPPH/(PHPOM*C)-KPH*STH*DRDT1-KPH*RCTH*DTHDT)/RSTH	
         DRDT(1) =DRDT1
		NR=6		
Cf***#**** PHASE PATH	
		IF (W(57).EQ.0.) GO TO 10	
		NR=NR+1		
		DRDT(NR)=-	KPHPK/PHPOM/OM	
C********* ABSORPTION	
 10     IF (W (58).EQ.0.) GO TO 15	
		NR=NR+1		
       DRDT(NR)= 10./LOGTEN*KPHPK*KAY2I/(KR*KR+KTH*KTH+KPH*KPH)/PHPOM/C	
C********* DOPPLER SHIFT	
 15     IF (W(59).EQ.0.) GO TO 20	
		NR=NR+1		
		DRDT(NR)=-PHPT/PHPOM/C/PIT2	
C****#**** GEOMETRICAL PATH LENGTH	
 20     IF (W(60).EQ.0.) GO TO 25	
		NR=NR+1		

		DRDT(NR)=-SQRT(PHPKR**2+PHPKTH**2+PHPKPH**2)/PHPOM/C	
C********* OTHER CALCULATIONS		HAML042
 25     CONTINUE	
        if(debug.eq.1.) print 7733, R(1)
7733    format("HAMLTN RETURNING:R(1):",(e14.6,2x))
 	
		RETURN		
		END		

CP-90
