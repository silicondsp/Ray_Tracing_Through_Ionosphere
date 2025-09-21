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


	   SUBROUTINE READ W	
C		READS W ARRAY	READ002

C	A 1 IN THE FOLLOWING COLUMNS WILL MAKE THE DESCRIBED CONVERSIONS

COL 18	DEGREES TO RADIANS	
COL 19	GREAT CIRCLE DISTANCE IN KM TO RADIANS	
COL 20	NAUTICAL MILES TO KM	
COL 21	FEET TO KM	
c				
       COMMON /DDEBUG/ debug        
		COMMON /CONST/ PI,PIT2,PID2,DEGS,RAD,DUM(3)	
		COMMON /WW/ ID(10),W0,W(400)	
		EQUIVALENCE (EARTHR,W(2))	
		INTEGER DEG,FEET	
		READ 1000, ID	
 1000   FORMAT (10A8)	
CSHA		IF (EOF,60) 3,4		

CSHA 3      CALL EXIT	

 4      READ 1100, NW,W(NW),DEG,KM,NM,FEET	

 1100   FORMAT (I3,E14.7,5I1)	
	    IF (NW.EQ.0) GO TO 10	

        IF (NW.GT.0.AND.NW.LE.400) GO TO 5	
      PRINT 4000, NW
 4000 FORMAT(15H1THE SUBSCRIPT ,I3,75HON THE W-ARRAY INPUT IS OUT OF BO
     1UNDS.	ALLOWABLE VALUES ARE 1 THROUGH 400.	    )
	    CALL EXIT	
 5      IF (DEG.NE.0.) W(NW)=W(NW)*RAD	
	    IF (KM.NE.0) W(NW)=W(NW)/EARTHR	
	    IF (NM.NE.0) W(NW)=W(NW)*1.852	

	    IF (FEET.NE.0) W(NW)=W(NW)*3.048006096E-4		
	    GO TO 4		
 10     RETURN			
	    END		
	
	
