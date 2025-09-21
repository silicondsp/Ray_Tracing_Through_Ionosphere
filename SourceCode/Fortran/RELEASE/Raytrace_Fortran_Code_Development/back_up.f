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
	   SUBROUTINE BACK UP(HS,IGRAZE)
C CHECKED		

       COMMON /DDEBUG/ debug        
	   COMMON /RK/ N,STEP,MODE,E1MAX,E1MIN,E2MAX,E2MIN,FACT,RSTART	
	   COMMON /TRAC/ GROUND,PERIGE,THERE,MINDIS,NEWRAY,SMT	
	   COMMON R(20),T,STP,DRDT(20) /WW/ ID(10),W0,W(400)
	   EQUIVALENCE (EARTHR,W(2)),(INTYP,W(41)),(STEP1,W(44))	
	   REAL INTYP	
	   LOGICAL GROUND,PERIGE,THERE,MINDIS,NEWRAY,HOME
	   
C       SUBROUTINE RKAM(DELY,BET,XV,FV,YU,...)
C Apparently on the CDC it keeps values between subroutine calls!!!!
C So we keep the variables in the calling program
       DIMENSION DELY(4,20),BET(4),XV(5),FV(4,20),YU(5,20)
       DOUBLE PRECISION YU
       REAL RR
C end RKAM variables
	   
	   
	   IF(IGRAZE.EQ.1) GO TO 55	   	
C********* DIAGNOSTIC PRINTOUT	
C		CALL PRINTR (8HBACK__UP,0.)	
C********* GOING AWAY FROM THE HEIGHT HS	
	   HOME=DRDT(1)*(R(1)-EARTHR-HS).GE.0.	

       IF(HS.GT.0..AND..NOT.HOME.OR.HS.EQ.0..AND.DRDT(1).GT.0.)
     6     GO TO 30
       
CP-74


C********* FIND NEAREST INTERSECTION OF RAY WITH THE HEIGHT HS	
	    DO 10 I=1,10	
	    STEP=-(R(1)-EARTHR-HS)/DRDT(1)	
	    STEP=SIGN(AMIN1(ABS(STP),ABS(STEP)),STEP)	
	    IF (ABS(R(1)-EARTHR-HS).LT..5E-4.AND.ABS(STEP).LT.1.) 
     4           GO TO 60	
C********* DIAGNOSTIC PRINTOUT	BACK018

C			CALL PRINTR(8HBACK__UP,0)	
		MODE=1	
		RSTART=1.	
		CALL RKAM(DELY,BET,XV,FV,YU,LL,MM,ALPHA,EPM,RR)	
  10    RSTART=1.0	
C				

C********* FIND NEAREST CLOSEST APPROACH OF RAY TO THE HEIGHT HS	
CSHA	    ENTRY GRAZE	
 55     CONTINUE
            
            Write (6,5555) 
 5555       format("GRAZE ")
	    THERE=.FALSE.	
C********* DIAGNOSTIC PRINTOUT	

C		CALL PRINTR (SHGRAZE 0 90o)	
	    IF (SMT.GT.ABS(R(1)-EARTHR-HS)) GO TO 30		
	    DO 20 I=1,10		
	    STEP=-R(4)/DRDT(4)	
	    STEP=SIGN(AMIN1(ABS(STP),ABS(STEP)),STEP)	
	    IF (ABS(R(4)).LE.1.E-6.AND.ABS(STEP).LT.1.) GO TO 60		

C********* DIAGNOSTIC PRINTOUT	

C			CALL PRINTR (8HGRAZE 1 90-)	
		MODE=1	
		RSTART=1.	 
		CALL RKAM(DELY,BET,XV,FV,YU,LL,MM,ALPHA,EPM,RR)	
		RSTART=1.	
		IF (DRDT(4)*(R(1)-EARTHR-HS).LT.0.) GO TO 30		
		IF(R(5).EQ.0..AND.R(6).EQ.0.) GO TO 60	
   20   CONTINUE	

C********* IF A CLOSEST APPROACH COULD NOT BE FOUND IN 10 STEPS9 IT	
C********* PROBABLY MEANS THAT THE RAY INTERSECTS THE HEIGHT HS	
   30   CONTINUE		
C********* DIAGNOSTIC PRINTOUT	
c			CALL PRINTR (8HBACK UP290*)	
		MODE=1	

C********* ESTIMATE DISTANCE T-0 NEAREST INTERSECTION OF XAY WITH HEIGHT 

C********* HS BEHIND THE PRESENT RAY POINT	BACK051
C SHA Corrections from Jones
        H=R(1)-EARTHR
      STEP=(-R(4)-SQRT(R(4)**2-2.*(H-HS)*R(4)/DRDT(1)*DRDT(4)))/DRDT(4)
CSHA OLD	    STEP=(-R(4)-SQRT(R(4)**2-2.*(R(1)-EARTHR-HS)*DRDT(4)))/DRDT(4)	BACK52
  
	    RSTART=1.	
	    CALL RKAM(DELY,BET,XV,FV,YU,LL,MM,ALPHA,EPM,RR)	
	    RSTART=1.	
C********* FIND NEAREST INTERSECTION OF RAY WITH HEIGHT HS	
	    DO 40 I=1,10	
	    STEP=-(R(1)-EARTHR-HS)/DRDT(1)	
	    STEP=SIGN(AMIN1(ABS(STP),ABS(STEP)),STEP)
CSHA Original belo correction IF (ABS(R(1)-EARTHR-HS).LT..5E-4.AND.ABS(STEP).LT.1.) GO TO 60	
CSHA    IF (ABS(R(1)-EARTHR-HS).LT..5E-4.AND.ABS(STEP).LT.1.) GO TO 50
      IF (ABS(R(1)-EARTHR-HS).LT..5E-4.AND.ABS(STEP).LT.1.) GO TO 60
C********* DIAGNOSTIC PRINTOUT	BACK061

C			CALL PRINTR (8HBACK UP390-)	
		MODE=1	
		RSTART=1.	
		CALL RKAM(DELY,BET,XV,FV,YU,LL,MM,ALPHA,EPM,RR)	
  40    RSTART=1.	
  50    THERE=.TRUE.	

C********* RESET STANDARD MODE AND INTEGRATION TYPE	
  60    MODE=INTYP	

	    STEP=STEP1	
	    RETURN	
		END	

CP-75
