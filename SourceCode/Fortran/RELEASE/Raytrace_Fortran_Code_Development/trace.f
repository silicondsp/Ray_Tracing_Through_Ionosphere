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
	
			SUBROUTINE TRACE
C CHECKED	
C						CALCULATES THE RAY PATH	
			DIMENSION ROLD(20), DROLD (20)	

CSHA Added
       DIMENSION DELY(4,20),BET(4),XV(5),FV(4,20),YU(5,20)
       DOUBLE PRECISION YU

      COMMON  /DDEBUG/ debug
      COMMON /RK/ N,STEP,MODE,E1MAX,EIMIN,E2MAX,E2MIN,FACT,RSTART	
      COMMON /FLG/ NTYP,NEWWR,NEWWP,PENET,LINES,IHOP,HPUNCH	
      COMMON /TRAC/ GROUND, PERIGE, THERE, MINDIS, NEWRAY,SMT	
      COMMON /RIN/ MODRIN(3),COLL,FIELD, SPACE,N2,PNP(10),POLAR,LPOLAR	
      COMMON /XX/ MODX(2),X,PXPR,PXPTH,PXPPH,PXPT,HMAX	
      COMMON R(20),T,STP,DRDT(20) /WW/ ID(10),W0,W(400)	

CSHA Added

       COMMON  /RK/ DELY ,BET ,XV ,FV ,YU ,RR,LL,MM,ALPHA,EPM
      LOGICAL SPACE, HOME, WASNT, UNDRGD ,
     6 GROUND ,PERIGE,THERE, MINDIS,NEWWR,		
     1NEWWP,PENET,NEWRAY,WAS	
      REAL MAXSTP		
      COMPLEX N2,PNP,POLAR,LPOLAR
C SHA Added since REACH previously assumed that subroutines maintained their value			
      LOGICAL CROSS , CROSSG, CROSSR, RSPACE
				
C       SUBROUTINE RKAM(DELY,BET,XV,FV,YU,LL,MM,ALPHA,EPM)
C Apparently on the CDC it keeps values between subroutine calls!!!!
C So we keep the variables in the calling program
C       DIMENSION DELY(4,20),BET(4),XV(5),FV(4,20),YU(5,20)
C       DOUBLE PRECISION YU
C end RKAM variables
				

      EQUIVALENCE (EARTHR,W(2)),(RCVRH,W(20)),(HOP, W(22)),
     1(MAXSTP,W(23)),(SKIP,W(71)),(RAYSET,W(72)),(PLT,W(81))
      
     
       CROSS=.FALSE.
         CROSSG=.FALSE.
         CROSSR=.FALSE.
         RSPACE=.FALSE.
         
          LL=1
        MM=1
        ALPHA=0
        EPM=0
        RR=0
        
        
        
         

	     NHOP=HOP	
	     MAX=MAXSTP	
	     NSKIP=SKIP	
	     RSTART=1.	
         if(debug.eq.1.) print 8888, R(1),R(2),R(3),R(4),R(5),R(6)
 8888    format("TRACE1: ",6(E14.6,2x))
	     
	     
	     CALL HAMLTN	
	     HOME=DRDT(1)*(R(1)-EARTHR-RCVRH).GE.0.	

C********* IHOP=O TELLS PRINTR TO PRINT HEADING AND PUNCH A TRANSMITTER 

C******#** RAYSET AND TELLS RAYPLT TO ST-ART A NEW RAY	
	     IHOP=0		
       CALL PRINTR (8HXMTR____,0.)	
	     IF (PLT.NE.0.) CALL RAYPLT	
	     HTMAX=0.		
	     NEWRAY=.TRUE.		

CSHA 2018 Original:
CSHA	     THERE=(R(1)-EARTHR).EQ.RCVRH	
             THERE=R(1)-EARTHR.EQ.RCVRH
C           THERE=(R(1)-EARTHR).EQ.RCVRH
	     if(debug.eq.1.) print 8889, THERE,R(1),EARTHR,RCVRH
8889    format("TRACE1: THERE R(1),EARTHR,RCVRH ",1L,2X,3(E14.6,2x))

         if(debug.eq.1.) print 8888, R(1),R(2),R(3),R(4),R(5),R(6)
	     

CP-72

C*****#*** LOOP ON HUMBER OF HOPS	
 10      IHOP=IHOP+1	

	    IF (IHOP.GT.NHOP) RETURN	
	   PENET=.FALSE.	
	   APHT=RCVRH	
C********* LOOP ON MAXIMUM NUMBER OF STEPS PER HOP	
	   DO 79 J=1,MAX	
	   H=R(1)-EARTHR	
	   IF (ABS(H-RCVRH).GT.ABS(APHT-RCVRH)) APHT=H	
	   HTMAX=AMAX1(H, HTMAX)	
	   IF (.NOT.SPACE) GO TO 12	
	   CALL REACH(CROSS , CROSSG, CROSSR, RSPACE)	
	   RSTART=1.	
	   H=R(1)-EARTHR	
	   IF (ABS(H-RCVRH).GT.ABS(APHT-RCVRH)) APHT=H	
	   HTMAX=AMAX1(H,HTMAX)	
	   IF (.NOT.SPACE) GO TO 12	
	   IF (PERIGE) CALL PRINTR (8HPERIGEE_,0.)	
	   IF (THERE) GO TO 51	
	   IF (MINDIS) GO TO 40	
	   IF (GROUND) GO TO 60	
	   IF (PLT.NE.0.) CALL RAYPLT	
	   IF (PERIGE) GO TO 79	

 12    DO 13 L=1,N	
	   ROLD (L) =R(L)	

 13    DROLD(L)=DRDT(L)	
	   TOLD=T	
	   WAS=THERE	
	   if(debug.eq.1.) print 7777,RSTART
 7777  format("TRACE CALLING RKAM: RESTART:",E14.6);
 
 
 
 
C	   CALL RKAM(DELY,BET,XV,FV,YU,LL,MM,ALPHA,EPM,RR)	
 	   CALL RKAM()
 	   
 	   
	   H=R(1)-EARTHR	
	   THERE=.FALSE.	
	   WASNT=.NOT.HOME	
	   HOME=DRDT(1)*(H-RCVRH).GE.0.	
	   TMP=(DRDT(1)-DROLD(1))*(T-TOLD)	
	   SMT=0.	
	   IF (TMP.NE.0.) SMT=0.5*(R(1)-ROLD(1)+0.5*TMP)**2/ABS(TMP)	

       IF (((H-RCVRH)*(ROLD(1)-EARTHR-RCVRH).LT.0..AND..NOT.WAS).OR.	
     1  (WAS.AND.DRDT(1)*DROLD(1).LT.0..AND.HOME)) GO TO 50	
       IF(HOME.AND.WASNT) GO TO 30	
	   IF (H.LT.0..OR.DRDT(1).GT.0..AND.DROLD(1).LT.0..AND.SMT.GT.H)	
     1  GO TO 20	

       IF (DROLD(1).LT.0..AND.DRDT(1).GT.0.) CALL PRINTR(8HPERIGEE_,0.) 
       IF (DROLD(1).GT.0..AND.DRDT(1).LT.0.) CALL PRINTR(8HAPOGEE__,0.) 

       IF (DROLD(2)*DRDT(2).LT.0.) CALL PRINTR(8HMAX_LAT_,0.)	
       IF (DROLD(3)*DRDT(3).LT.0.) CALL PRINTR(8HMAX_LONG,0.)	
       DO 14 I=4,6	

	   IF (ROLD(I)*R(I).LT.0.) CALL PRINTR(8HWAVE_REV,0.)	
 14    CONTINUE	
	   GO TO 75	
C****#**** RAY WENT UNDERGROUNJ	
 20    CALL BACK UP(0.,0)	
	   GO TO 60	

C********* RAY 4AY 4AVE MAOE A CLOSEST APPROACH	
CSHA 30    CALL GRAZE(RCVRH) Original
 
 30    CALL BACK UP(RCVRH,1)	
	   IF (THERE) GO TO 51	
 40	   DRDT(1)=0.	
		HPUNCH=R(1)-EARTHR	
		CALL PRINTR(8HMIN_DIST,RAYSET)	

	  IF (PLT.NE.0.) CALL RAYPLT	

      IF (IHOP.GE.NHOP) RETURN	
      IHOP=IHOP+1	
      CALL PRINTR (8HMIN_DIST,RAYSET)	
      GO TO 89	

CP-73

C********* RAY CROSSEQ RECEIVER HEIGMT	
 50	    CALL BACK UP(RCVRH,0)		
        THERE=.TRUE.		
 51     R(1) =EARTHR+RCVRH		
		HTMAX=AMAX1(RCVRH,HTMAX)	
		HPUNCH=APHT		
		CALL PRINTR(8HRCVR____,RAYSET)	
		IF (PLT.NE.0.) CALL RAYPLT	
		IF (RCVRH.NE.0.) GO TO 89	
		IF (IHOP.GE.NHOP) RETURN	
		IHOP=IHOP+1		
		APHT=RCVRH		
C********* GROUND REFLECT		
 60     R(1)=EARTHR		
		IF (ABS(RCVRH).GT.ABS(APHT-RCVRH)) APHT=0.	
		R(4)=ABS (R(4))		
		DRDT(1)=ABS (DRDT(1))		
		RSTART=1.		
		HPUNCH=HTMAX		
		CALL PRINTR(8HGRND_REF,RAYSET)	
		HTMAX=0. 		
		IF (RCVRH.NE.0.) GO TO 65	
		THERE=.TRUE.		
		HPUNCH=APHT		

		CALL PRINTR (8HRCVR____,RAYSET)	
		GO TO 89		
 65     H=0.		
		THERE=.FALSE.		
C*********		

 75     IF (PLT.NE.0.) CALL RAYPLT		
		IF (H.GT.HMAX.AND.H.GT.RCVRH.AND.DRDT(1).GT.0.) GO TO 90		
		IF (MOD(J,NSKIP).EQ.0) CALL PRINTR(8H________,0.)
 79     CONTINUE	
C********* EXCEEDED MAXIMUM NUMBER OF STEPS	
		HPUNCH=H	
		CALL PRINTR(8HSTEP_MAX,RAYSET)	
		RETURN	
C*********	TRACM,
 89     HOME=.TRUE.	
		GO TO 10		
C*****#*** RAY PENETRATED	
 90     PENET=.TRUE.	
		HPUNCH=H	
		CALL PRINTR(8HPENETRAT,RAYSET)	
		RETURN	
		END	
CP-74		
		

