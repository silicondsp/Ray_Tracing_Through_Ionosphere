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


	    SUBROUTINE REACH(CROSS , CROSSG, CROSSR, RSPACE)
C CHECKED	
C		CALCULATES THE STRAIGHT LINE. RAY PATH BETWEEN THE EARTH 	

C		AND THE IONOSPHERE OR BETWEEN IONOSPHERIC LAYERS	
	   LOGICAL CROSS , CROSSG, CROSSR, RSPACE	
        COMMON  /DDEBUG/ debug
        COMMON /RK/ N,STEP,MODE,E1MAX,E1MIN,E2MAX,E2MIN,FACT,RSTART	
        COMMON /TRAC/ GROUND ,PERIGE, THERE , MINDIS, NEWRAY, SMT	
        COMMON /COORD/ S	
        COMMON /RIN/ MODRIN(3),COLL,FIELD,SPACE,N2,N2I,PNP(10),POLAR,		
     1  LPOLAR	
        COMMON /XX/ MODX(2) ,X,PXPR,PXPTH,PXPPH,PXPT,HMAX	
        COMMON R(20 ) ,T,STP,DRDT(20) /WW/ ID(10) ,W0, W(400)	
CSHA        DIMENSION XO(6),XXX(6),RO(4)
C      COMMON /POLX/ XO(1),XO(2),XO(3),XO(4),XO(5),XO(6)
C       COMMON /POLXX/ XXX(1),XXX(2),XXX(3),XXX(4),XXX(5),XXX(6)
C       COMMON /POLR/ RO(1),RO(2),RO(3),RO(4),RO(5),RO(6),VERT


       COMMON /POLX/ XO(3) 
       COMMON /POLXX/ XXX(6) 
       COMMON /POLR/ RO(6),VERT 

        EQUIVALENCE (EARTHR,W(2)) ,(XMTRH,W(3) ) , (RCVRH,W(20))
	   LOGICAL SPACE ,GROUND, PERIGE ,THERE ,MINDIS,	
     1			NEWRAY	
	   REAL N2,N2I			
	   COMPLEX PNP,POLAR,LPOLAR

	
	   
 
	   REAL VERT		
    
CSHA End	   
	   
	   	
	   DATA NSTEP/500/

       VERT=0.0	
       if(debug.eq.1.) print 8872, R(1),EARTHR,VERT
8872   format("REACH111  CALLED HAMLTN :R(1),EARTHR,VERT:",3(e14.6,2x))

      if(debug.eq.1.) print 8372, R(1),R(2),R(3),EARTHR
8372   format("REACH110 R(1), R(2), R(3) EARTHR",4(e14.6,2x));

 	   

	   CALL HAMLTN			
	   H=R(1)  -EARTHR		

	   IF (.NOT.NEWRAY.AND..NOT.RSPACE) CALL PRINTR(8HEXIT_ION,0.)

	   NEWRAY=.FALSE.	
	   V=SQRT(R(4)**2+R(5)**2+R(6)**2)	
C********* NORMALIZE THE WAVE NORMAL DIRECTION TO ONE	
	   R(4) =R(4)/V	
	   R(5)=R(5)/V
	   R(6)=R(6)/V	

C******** NEGATIVE OF DISTANCE ALONG RAY TO CLOSEST APPROACH TO CENTER 

C********* OF EARTH	
	  UP=R(1)*R(4)	
	  RADG=EARTHR**2-R(1)**2*(R(5)**2+R(6)**2)	
	  DISTG=SQRT (AMAX1(0.,RADG))	
C********* DISTANCE ALONG RAY TO FIRST INTERSECTION WITH OR CLOSESThhhhhhhhhhhhhhhhhhhhhhhhhhhhhh	
C********* APPROACH TO THE EARTH	R-EAC	32
	  SG=-UP-DISTG	
	  
C

C********* CROSSG IS TRUE IF THE RAY WILL INTERSECT OR TOUCH THE EARTH	
	   CROSSG=UP.LT.0..AND.RADG.GE.0.	
	   RADR=(EARTHR+RCVRH)**2-R(1)**2*(R(5)**2+R(6)**2)	
	   DISTR=SQRT (AMAX1(0.,RADR))

C********* DISTANCE ALONG RAY TO THE FIRST INTERSECTION WITH 00 CLOSEST 

C********* APPROACH TO THE kECEIVER, HEIGHT	REAC 39

       SR=DISTR-UP	
      IF(UP.LT.0..AND.DISTR.LT.-UP.AND.R(1).NE.EARTHR+RCVRH)SR=-DISTR
     1  -UP	


C********* CROSSR IS TRUE IF THE RAY WILL INTERSECT WITH OR MAKE A	REAC 43
C*********.CLOSEST APPROACH TO THE RECEIVER HEIGHT	REAC 44

	   CROSSR=R(4).LT.0..OR.R(1).LT.(EARTHR+RCVRH)	

           if(debug.eq.1.) print 8672, R(1),R(4), EARTHR,RCVRH,CROSSR
8672   format("REACH122  R(1),R(4), EARTHR,RCVRH  CROSSR :",
     3    4(e14.6,2x),2x,L)



	   CROSS=CROSSG.OR.CROSSR	
C********* M-AXIMUM DISTANCE IN WHICH TO LOOK FOR THE IONOSPHERE	
	  S1=AMIN1(SR,SG)	
	  IF(.NOT.CROSSG) S1=SR	
	  IF (UP.GE.0.) GO TO 15	
	  CROSS=.TRUE.	

C********* IF RAY IS GOING DOWN, S1 IS AT MOST THE DISTANCE TO A PERIGEE

		S1=AMIN1(S1,-UP)	
C********* CONVERT THE POSITION AND DIRECTION 3F THE RAY TO CARTESIAN	
C********* COORDINATES	

   15   CALL POL CAR( 0)
       if(debug.eq.1.) print 8882, R(1),EARTHR,VERT
 8882   format("REACH000 CALLED POL CAR :R(1),EARTHR,VERT:",3(e14.6,2x));
   
   	
		SSTEP=100.	
		S=SSTEP	
		DO 20 I=1,NSTEP
		
        if(debug.eq.1.)print 8884, I,R(1),EARTHR,S,SSTEP,S1,CROSS,SPACE
 8884   format("REACH111:I,R(1),EARTHR,S,SSTEP,S1,CROSS,SPACE:",I4,2x,
     6    5(e14.6,2x),L,2x,L);
		
			
		IF ((S-SSTEP).GT.S1.AND.CROSS) GO TO 25	

C********* CONVERT POSITION AND DIRECTION TO SPHERICAL POLAR COORDINATES

C*********	AT A DISTANCE S ALONG THE RAY
CSHA Was CAR POL using Entry
       CALL POL CAR( 1)	
  

       if(debug.eq.1.) print 8883, R(1),EARTHR,VERT
 8883   format("REACH00:R(1),EARTHR,VERT:",3(e14.6,2x));
	
	
	    CALL	ELECTX	
C*********	FREE SPACE	

CP-76

       

		IF (X.EQ.0. ) GO TO 20	
        if(debug.eq.1.) print 7777, R(1)
 7777   format("REACH CALLING RINDEX1:R(1):",(e14.6,2x))
		
		CALL RINDEX	
C********* EFFECTIVELY FREE SPACE	
		IF (SPACE) GO TO 20	
CSHA ORIG		IF (SSTEP.LT.0.5E-4) GO TO 25
CSHA         IF (SSTEP.LT.0.5E-1) GO TO 25
CSHA        IF (SSTEP.LT.0.5E-2) GO TO 25	
          if(debug.eq.1.) print 7788, SSTEP
 7788      format("REACH SSTEP=",E14.6)
        
C       IF (SSTEP.LT.0.5E-4) GO TO 25
       IF (SSTEP.LT.0.5E-3) GO TO 25	
C********* RAY IN THE IONOSPHERE. STEP BACK OUT	
		S=S-SSTEP	
C********* DECREASE STEP SIZE	
		SSTEP=SSTEP/10.	
  20    S=S+SSTEP
		PRINT 2000, NSTEP	

 2000   FORMAT (9H EXCEEDED,I5,26H STEPS IN SUBROUTINE REACH)	
	    CALL EXIT	

 25    IF(CROSS) S=AMIN1 (S,S1)	

C********* CONVERT POSITION AND DIRECTION TO SPHERICAL POLAR, COORDINATES

C********* AT A DISTANCE S ALONG THE RAY	
CSHA		CALL CAR POL
CSHA   CAR POL was an Entry. Rewrote this so last parameter tells POL CAR to goto Entry
        CALL	POL CAR( 1)		
C********* AVOID THE RAY BEING SLIGHTLY UNDERGROUND
         	
		R(1)=AMAX1(R(1) ,EARTHR)	
        if(debug.eq.1.) print 8887, R(1),EARTHR,N,T,S
 8887   format("REACH2:R(1),EARTHR,N,T,S:",2(e14.6,2x),I3,2x,
     1     2(e14.6,2x));
		
C********* ONE STEP INTEGRATION	
		IF (N.LT.7) GO TO 31
		DO 30 NN=7,N	
  30    R(NN)=R(NN)+S*DRDT(NN)	
  31    T=T+S	
         if(debug.eq.1.) print 8880, R(1),EARTHR,N,T,S
 8880   format("REACH3:R(1),EARTHR,N,T,S:",2(e14.6,2x),I3,2x,
     1     2(e14.6,2x));
     
     
        if(debug.eq.1.) print 7778, R(1)
7778    format("REACH CALLING RINDEX2:R(1):",(e14.6,2x))

        CALL RINDEX
         if(debug.eq.1.) print 8881
 8881   format("REACH3:After RINDEX")
        
        
C********* AT A PERIGEE	
        PERIGE= S.EQ.(-UP)	
C********* CORRECT MINOR, ERRORS	
        IF (PERIGE) R(4)=0.	
C********* KEEP CONSISTENCY AFTER CORRECTING MINOR ERRORS
		DRDT(1)=R(4)	
C********* ON THE GROUND	
		GROUND=S.EQ.SG.AND.CROSSG	
C********* AT THE RECEIVER HEIGHT	

	    THERE=S.EQ.SR.AND.CROSSR.AND..NOT.PERIGE	
C********* AT A CLOSEST APPROACH TO THE RECEIVER HEIGHT

	     MINDIS= PERIGE.AND.S.EQ.SR.AND.CROSSR		
	    RSPACE=SPACE	
	    V=SQRT(N2/(R(4)**2+R(5)**2+ R(6)** 2))		
C********* RENORMALIZE THE WAVE NORMAL DIRECTION TO	SQPT(REAL(N**2))	
	    R(4)=R(4)*V		
	    R(5)=R(5)*V		
	    R(6)=R(6)*V		
	    RSTART=1.		
	    IF (.NOT.SPACE) CALL PRINTR (8HENTR_ION,0.)	

            if(debug.eq.1.) print 8781,  CROSS , CROSSG, CROSSR, RSPACE
 8781        format("REACH4:CROSS=",1L,"  CROSSG=",1L, " CROSSR= ",1L,
     1           " RSPACE=",1L)

            
	
	    RETURN		
	    END		


