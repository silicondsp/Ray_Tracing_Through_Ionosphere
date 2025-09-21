
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
C SUBROUTINE RKAM(DELY,BET,XV,FV,YU,LL,MM,ALPHA,EPM,RR)  
        SUBROUTINE RKAM()
       DIMENSION DELY(4,20),BET(4),XV(5),FV(4,20),YU(5,20)
        REAL RR
        DOUBLE PRECISION YU
        
C CHECKED
C NUMERICAL INTEGRATION OF DIFFERENTIAL EQUATIONS
       COMMON /DDEBUG/ debug        
        COMMON /RK/ NN,SPACE,MODE,E1MAX,E1MIN,E2MAX,E2MIN,FACT,RSTART
        COMMON R(20),T,STEP,DRDT(20)

CSHA added
         COMMON  /RK/ DELY ,BET ,XV ,FV ,YU ,RR,LL,MM,ALPHA,EPM


C        DIMENSION DELY(4,20),BET(4),XV(5),FV(4,20),YU(5,20)
C        DOUBLE PRECISION YU


C        COMMON R(20),T,STP,DRDT(20) /WW/  ID(10),W0,W(400)	
        
        if(debug.eq.1.) print 7777,RSTART,NN,R(1)
 7777   format("RKAM11: RESTART:",E14.6, "NN=",I5, 2x,E14.6);
        do 33 I=1,20
	    if(debug.eq.1.) print 7576,NN,DRDT(I),R(I)
 7576   format("RKAM12   NN DRDT ",I5, 2x,"DRDT=",E14.6,2x,
     3     "R(I)=",2x,E14.6);
  33    continue
        
        IF(RSTART.EQ.0.) GO TO 1000
        LL=1
        MM=1
        IF (MODE.EQ.1) MM=4
        ALPHA=T
        EPM=0.0
        BET(2)=0.5
        BET(1)=BET(2)
        BET(3)=1.0
        BET(4)=0.0
        STEP=SPACE
        RR=19.0/270.0
        XV(MM)=T
        IF(E1MIN.LE.0.) E1MIN=E1MAX/55.
        IF(FACT.LE.0.) FACT=0.5
	    if(debug.eq.1.) print 7776,NN,R(1),STEP
 7776   format("RKAM2 CALLING HMLTN NN=",I5, 2x,E14.6," STEP=",E14.6);
        
        CALL HAMLTN
        DO 320 I=1,NN
        FV(MM,I)=DRDT(I)
 320    YU(MM,I)=R(I)
        RSTART=0.
        GO TO 1001
 1000   IF(MODE.NE.1) GO TO 2000
C
C       RUNGE-KUTTA
 1001   DO 1034 K=1,4
        DO 1350 I=1,NN
        DELY(K,I)=STEP*FV(MM,I)
        Z=YU(MM,I)
 1350   R(I)=Z+BET(K)*DELY(K,I)
        T=BET(K)*STEP+XV(MM)
 	    if(debug.eq.1.) print 7775,NN,R(1)
 7775   format("RKAM3 CALLING HMLTN NN=",I6, 2x,E14.6);
        
        CALL HAMLTN
        DO 1034 I=1,NN
 1034   FV(MM,I)=DRDT(I)
        DO 1039 I=1,NN
        DEL=(DELY(1,I)+2.0*DELY(2,I)+2.0*DELY(3,I)+DELY(4,I))/6.0
 1039   YU(MM+1,I)=YU(MM,I)+DEL
        MM=MM+1
        XV(MM)=XV(MM-1)+STEP
        DO 1400 I=1,NN
 1400   R(I)=YU(MM,I)
        T=XV(MM)
 	    if(debug.eq.1.) print 7771,NN,R(1)
 7771   format("RKAM4 CALLING HMLTN NN=",I5, 2x,E14.6);
       
        CALL HAMLTN
        IF(MODE.EQ.1) GO TO 42
        DO 150 I=1,NN
        FV(MM,I)=DRDT(I)
	    if(debug.eq.1.) print 7731, DRDT(I),FV(MM,I),MM,I
 7731   format("RKAM9999  DRDT(I),FV(MM,I),MM,I",
     4            2(2x,E14.6),I2,2x,I2)
  

 150    CONTINUE
        IF(MM.LE.3) GO TO 1001
C
C       ADAMS-MOULTON
 2000   CONTINUE
        DO 2048 I=1,NN
        DEL=STEP*(55.*FV(4,I)-59.0*FV(3,I)+37.*FV(2,I)-9.*FV(1,I))/24.
        R(I)=YU(4,I)+DEL
        DELY(1,I)=R(I)
	    if(debug.eq.1.) print 7371,FV(4,I),FV(3,I),FV(2,I),FV(1,I),
     5      YU(4,I),DEL,DRDT(I),I
 7371  format("RKAM44 FV(4,I),FV(3,I),FV(2,I),FV(1,I),YU(4,I),DEL,I",
     4    7(2x,E14.6),2x,I2);
 



 2048       CONTINUE
        T=XV(4)+STEP
	    if(debug.eq.1.) print 7735,NN,R(1)
 7735   format("RKAM5 CALLING HMLTN NN=",I5, 2x,E14.6);
         
        CALL HAMLTN
        XV(5)=T
        DO 2051 I=1,NN
        DEL=STEP*(9.*DRDT(I)+19.*FV(4,I)-5.*FV(3,I)+FV(2,I))/24.
        YU(5,I)=YU(4,I)+DEL
        R(I)=YU(5,I)


	    if(debug.eq.1.) print 7373,FV(4,I),FV(3,I),FV(2,I),FV(1,I),
     5      YU(4,I),DEL,DRDT(I),I
7373  format("RKAM1122 FV(4,I),FV(3,I),FV(2,I),FV(1,I),YU(4,I),DEL,I",
     4    7(2x,E14.6),2x,I2);
 


 7711   format("RKAM66 CALLING HMLTN: NN=",I5, 8(2x,E14.6));

        if(debug.eq.1.) print 7733,I,YU(5,I),YU(4,I),DRDT(I),FV(4,I),
     5 FV(3,I),FV(2,I),R(I),DEL
 7733   format("RKAM66 CALLING HMLTN: NN=",I5, 8(2x,E14.6));

 2051   CONTINUE
 	    if(debug.eq.1.) print 7755,NN,R(1)
 7755   format("RKAM6 CALLING HMLTN: NN=",I5, 2x,E14.6);
 
        CALL HAMLTN
        IF (MODE.LE.2) GO TO 42
        
        
CP-88
 
c			
c		ERROR ANALYSIS	
		SSE=0.0	
		DO 3033 I=1,NN	
		EPSIL=RR*ABS(R(I) -DELY (1,I))
		IF (MODE.EQ.3.AND.R(I).NE.0.) EPSIL=EPSIL/ABS(R(I))	
		IF (SSE.LT.EPSIL) SSE=EPSIL	
 3033   CONTINUE	
		IF (E1MAX.GT.SSE) GO TO 3035	
		IF (ABS (STEP). LE.E2MIN) GO TO 42	
		LL=1
		MM=1	
		STEP=STEP*FACT	
		GO TO 1001	
 3035   IF (LL.LE.1.OR.SSE.GE.E1MIN.OR.E2MAX.LE.ABS(STEP)) GO TO 42	
		LL=2		
		MM=3		
		XV(2)=XV(3)	
		XV(3)=XV(5)	
		DO 5363 I=1,NN	
		FV(2,I)=FV(3,I)	
		FV (3,I) =DRDT(I)	
		YU(2,I)=YU(3,I)	
 5363             YU(3,I)=YU(5,I)	

	    STEP=2.0*STEP	
	    GO TO 1001	
c			
c		EXIT ROUTINE	
 42     LL=2	
	    MM=4	
	    DO 12 K=1,3	
	    XV(K)=XV(K+1)	
	    DO 12 I=1,NN	
	    FV(K,I)=FV(K+1,I)	
 12	    YU (K,I) =YU (K+1,I)	
	    XV(4)=XV(5)	
	    DO 52 I=1,NN		
	    FV(4,I)=DRDT(I)	
 52	    YU(4,I)=YU(5,I)	
	    IF (MODE.LE.2) RETURN		
	    E=ABS(XV(4)-ALPHA)	

	    IF (E.LE.EPM) GO TO 2000		
	    EPM=E	
	    RETURN	
		END	

CP-89
        
