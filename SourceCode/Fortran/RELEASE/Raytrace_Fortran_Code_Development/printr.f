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
		SUBROUTINE PRINTR (NWHY, CARD)	
		CHARACTER*8 NWHY
C CHECKED
C				PRINTS OUTPUT AND PUNCHES RAYSETS WHEN REQUESTED	

      DIMENSION G(3,3),G1(3,3),TYPE(3),RPRINT(20),NPR(20)
      CHARACTER*8 HEADR1(20),HEADR2(20),UNITS(20), 
     1		HEAD1(20),HEAD2(20),UNIT(20)	
      COMMON /DDEBUG/ debug        
      COMMON /CONST/ PI,PIT2,PID2,DEGS,RAD,DUM(3)	
      COMMON /FLG/ NTYP,NEWWR,NEWWP,PENET,LINES,IHOP,HPUNCH		
      COMMON /RIN/ MODRIN(3),COLL,FIELD,SPACE,N2,N2I,PNP(10),POLAR(2),		
     1 			LPOLAR(2),SGN	
      COMMON R(20),T /WW/ ID(10),W0,W(400)	
      EQUIVALENCE (THETA ,R(2)), (PHI,R(3) )	

      EQUIVALENCE (EARTHR,W(2)),(XMTRH,W(3)),(TLAT,W(4)),(TLON,W(5)), 	
     1	(F,W(6)),(AZ1,W(10)),(BETA,W(14)),(RCVRH,W(20)),(HOP,W(22)),	
     2 (PLAT,W(24)),(PLON,W(25)),(RAYSET,W(72))	
	    LOGICAL SPACE,NEWWR,NEWWP,PENET	
	    REAL N2,N2I,LPOLAR	
	    COMPLEX PNP	
	DATA TYPE/1HX,1HN,1HO/
     2 ,HEADR1(7)/"  PHAS"/,HEADR2(7)/"E PATH"/,UNITS(7)/"   KM "/,	
     3  HEADR1(8)/" ABSO"/,HEADR2(8)/"RPTION"/,UNITS(8)/"   DB "/,	
     4  HEADR1(9)/" DOP  "/,HEADR2(9)/"PLER  "/,UNITS(9)/"  C/S "/,	
     5  HEADR1(10)/" PATH"/,HEADR2(10)/"LENGTH"/,UNITS(10)/"   KM "/

	    CALL RINDEX	
C	    IF (.NOT.NEWWP) GO TO 10
	    
	    
	    	
CP-78 (PARTIAL)
       if(debug.eq.1.) print 9003, THETA, PHI
 9003   format("PRINTR THETA=",E14.6, " PHI=",E14.6, 2x)

          if(debug.eq.1.) print 9002, PLON,TLON,PLAT,TLAT,AZ1,BETA
9002   format("PRINTR PLON=",E14.6," TLON=" E14.6 ," PLAT=",
     4     E14.6," TLAT=",E14.6," AZ1=",E14.6," BETA=",E14.6)

C********* NEW W ARRAY -- REINTTIALIZE	
	    NEWWP=.FALSE.	
	    SPL=SIN (PLON-TLON)	
	    CPL=SIN (PID2-(PLON-TLON))	
	    SP=SIN (PLAT)	
	    CP=SIN (PID2-PLAT)	
	    SL=SIN (TLAT)	
	    CL=SIN (PID2-TLAT)	
 

           if(debug.eq.1.) Print 9001, SPL,CPL,SP,CP,SL,CL
9001   format("PRINTR SPL=",E14.6," CPL=" E14.6 ," SP=",
     4     E14.6," CP=",E14.6," SL=",E14.6," CL=",E14.6)


C********* MATRIX T3 ROTATE COORDINATES	
	    G(1,1)=CPL*SP*CL-CP*SL	
	    G(1,2)=SPL*SP	
	    G (1,3) =-SL*SP* CPL-CL*CP	
	    G(2,1)=-SPL*CL	
	    G(2,2)=CPL	
	    G(2,3)=SL*SPL	
	    G (3,1) =CL*CP*CPL+SP*SL	
	    G(3,2)=CP*SPL	
	    G(3,3) =-SL*CP*CPL+SP*CL	

      DENM=G(1,1)*G(2,2)*G(3,3)+G (1,2)*G(3,1)*G (2,3)+G(2,1)*G(3, 2)* 
     1G(1,3)-G(2,2) *G (3,1) *G(1,3)-G(1,2) *G (2,1) *G (3,3)-
     2G(1,1)*G(3,2)*G(2,3)	
C********* THE MATRIX G1 IS THE INVERSE OF THE MATRIX G	
      G1 (1,1) = (G (2,2) *G (3,3) -G (3, 2) *G (2, 3) ) /DENM	
      G1 (1,2) = (G (3,2) *G (1, 3) -G(1,2) *G(3,3 ))/DENM	
      G1 (1,3) =(G (1,2) *G (2,3) -G(2,2) *G(1,3) ) /DENM	
      G1 (2,1) =(G (3,1) *G (2 ,3) -G(2,1 )*G (3,3) )/DENM	
      G1 (2,2) =(G (1,1) *G (3 ,3) -G(3,1) *G (1,3) ) /DENM	
      G1 (2,3) =(G (2,1) *G( 1,3) -G(1,1)*G (2,3) ) /DENM	
      G1 (3,1) = (G (2,1) *G (3,2) -G (3, 1)*G (2,2) ) /DENM	
      G1 (3,2) = (G (3, 1) *G ( 1,2) -G(1, 1) *G (3,2) ) /DENM	
      G1 (3,3) = (G (1, 1) *G (2,2) -G(2,1) *G (1, 2) ) /DENM	
      R0=EARTHR+XMTRH	
C********* CARTESIAN COORDINATES OF TRANSMITTER	
      XR=R0*G(1,1)	
      YR=R0*G(2,1)	
      ZR=R0*G(3,1)	
      CTHR=G(3,1)	
      STHR=SIN (ACOS (CTHR))	
      PHIR=ATAN2(YR,XR)	
      ALPH=ATAN2 (G (3 , 2) , G (3, 3))	

C+++++++++++++++
        do 90 ii=1,3
        do 90 jj=1,3
 90       if(debug.eq.1.) print 8889, ii,jj,G(ii,jj) , G1(ii,jj)
 8889   format("PRINTR G()", I2,2x,I2,2x,E14.6, 10x , E14.6)
 
        if(debug.eq.1.) print 8888, R0, XR,YR,ZR
 8888   format("PRINTR R0=",E14.6, "XR,YR,ZR=",3(E14.6, 2x))

C++++++++++++++
C*********	PRIND65
		NR=6	
		NP=0	
		DO 7 NN=7,20	

        IF (W(NN+50).EQ.0.) GO TO 7	

C********* DEPENDENT VARIABLE NJM13ER NN IS 13EING INTEGRATED	
C********* NR IS THE NUMBER OF DEPENDENT VARIABLES BEING INTEGRATED	
	    NR=NR+1	
	    IF (W(NN+50).NE.2.) GO TO 7	

C**#***#** DEPENDENT VARIABL.E NJMBER NN IS BEING INTEGRATED ANO PRINTED.
C********* NP IS THE NUMBER OF DEPENDENT VARIABLES BEING INTEGRATED AND 
C********* PRINTE)	
		NP=NP+1	
C********* SAVE THE INDEX OF THE DEPENDENT VARIABLE TO PRINT	
		NPR(NP)=NR	
		HEAD1 (NP) =HEADR1 (NN)	
		HEAD2(NP)=HEADR2(NN)	
		UNIT(NP)=UNITS(NN)	
  7     CONTINUE	
		NP1=MIN0 (NP, 3)	
		PDEV=0
		ABSORB=0
		DOPP=0.	

CP-79
C********* PRINT COLUMN HEADINGS AT THE 3EGINNING OF EACH RAY 	

 10     IF (IHOP.NE.0) GO TO 12	
      PRINT 1101
      PRINT 1100, (HEAD1(NN),HEAD2(NN),NN=1,NP1)	
 1101 FORMAT (44X,7HAZIMUTH/43X,9HDEVIATION,8X,9HELEVATION)
 1100 FORMAT(20X,22HHEIGHT       RANGE    ,1X,2(5X,12HXMTR  LOCAL ),5X,
     326HPOLARIZATION	GROUP PATH      ,5A6,A5)
      PRINT 1150, (UNIT(NN),NN=1,NP1)	
	    
 1150 FORMAT (13X,2(8X,2HKM),2X,2(6X,3HDEG,5X,3HDEG),6X,12HREAL	IMAG   ,
     17X ,2HKM, 4X, 3 ( 4X , A6,2X))	
			IF (RAYSET.EQ.0.) GO TO 12	
C********* PUNIC4 A TRANSMITTER RAYSET	
			TLOND=TLON*DEGS	
			IF (TLOND.LT.0.) TLOND=TLOND+360.	
			TLATD=TLAT*DEGS	

            IF (TLATD.LT.0.) TLATD=TLATD+360.	

            AZ=AZ1*DEGS	
            EL=BETA*DEGS	
            NHOP=HOP	
CSHA was punch
            
      PRINT 1200,ID(1),TYPE(NTYP),XMTRH,TLATD,TLOND,RCVRH,F,AZ,EL,POLAR
     1      ,NHOP
CSHA     1      ,NHOP,1HT
 1200 FORMAT("PUNCH ",A3,A1,4PF9.0,3P2F6.0,4P2F9.0,5P2F10.0,5X,
     32P2F5.0,I1,"T")	
C*********	
 12      V=0.	

	    IF (N2.NE.0.) V=(R(4)**2+R(5)**2+R(6)**2)/N2-1.	
	    if(debug.eq.1.) print 8884, V,R(4),R(5),R(6),N2,POLAR,T
 8884   format("PRINTR:V,R(4),R(5),R(6),N2,POLAR,T",4(E14.6,2x),"N2=",
     1   E14.6,2x,2(E14.6,2x))
	    
	    
	    H=R(1)-EARTHR	
	    STH=SIN (THETA)	
	    CTH=SIN (PID2-THETA)	
C**#*#**** CARTESIAN COORDINATES OF RAY POINT, ORIGIN AT TRANSMITTER	
	    XP=R(1)*STH*SIN (PID2-PHI)-XR	
	    YP=R(1)*STH*SIN (PHI)-YR	
	    ZP=R(1) *CTH-ZR	

C********* CARTESIAN COORDINATES OF RAY POINT, ORIGIN AT TRANSMITTER AN

C******** ROTATEO	
		EPS=XP*G1(1,1)+YP*G1(1,2)+ZP*G1(1,3)


		
      if(debug.eq.1.) print 8855, EPS,XP,G1(1,1),YP,G1(1,2),ZP,G1(1,3)
 8855   format("PRINTR33:EPS,XP,G1(1,1),YP,G1(1,2),ZP,G1(1,3)",
     1      8(E14.6,2x))
		
			
		ETA=XP*G1(2,1)+YP*G1(2,2)+ZP*G1(2,3)	
		ZETA=XP*G1(3,1)+YP*G1(3,2)+ZP*G1(3,3)	
		RCE2=ETA**2+ZETA**2	
		RCE=SQRT (RCE2)	
C********* GROUND RANGE	PRINi24 Corrected based on Jones
		RANGE=EARTHR*ATAN2 (RCE,EARTHR+EPS+XMTRH)	
	    if(debug.eq.1.) print 8885, XP,YP,ZP,G1(3,1),G(3,2),G1(3,3)
 8885   format("PRINTR:XP,YP,ZP,G1(3,1),G(3,2),G1(3,3)",6(E14.6,2x))
		
	    if(debug.eq.1.) print 8887, EARTHR,RCE,EPS,RANGE,
     6          ZETA,ETA,RCE2
 8887   format("PRINTR:EARTHR,RCE,EPS,RANGE,ZETA,ETA,RCE2",7(E14.6,2x))
 
C****##*** ANGLE OF WAVE NORMAL WITH LOCAL HORIZONTAL	
		ELL=ATAN2(R(4),SQRT (R(5)**2+R(6)**2))*DEGS	
C********* STRAIGHT LINE OISTANC.IE FROM TRANSMITTER TO RAY POINT	PRINi26
		SR=SQRT (RCE2+EPS**2)	
		IF (NP.LT.1) GO TO 16	
		DO 15 I=1,NP	
		NN=NPR(I)	
  15    RPRINT(I)=R(NN)	

  16    IF (SR.GE.1.E-6) GO TO 20	

C********* TOO CLOSE TO TRANSMITTER TO CALCULATE DIRECTION FROM	
C********* TRANSMITTER	
      PRINT 1500, V,NWHY,H,RANGE,ELL,POLAR,T,(RPRINT(NN),NN=1,NP1)	
 1500 FORMAT("XX",1X,E6.0,1X,A8,F10.4,F11.4,26X,F8.3,F9.3,F8.3,4F12.4)		
			GO TO 40	
C********* tLEVATION ANGLE OF RAY POINT FROM TRANSMITTER	
  20    EL=ATAN2(EPS,RCE)*DEGS	
  
  
  	    if(debug.eq.1.) print 8881, EL,EPS,RCE,DEGS
 8881   format("PRINTR44: EL,EPS,RCE,DEGS",4(E14.6,2x))
	
		IF (RCE.GE.1.E-6) GO TO 30	

C********* NFARLY DIRECTLY A13OVE OR BELOW TRANSMITTER. CAN NOT CALCULATE

C********* AZIMJTI OIRECTION FROM TRANSMITTER ACCURATELY	
      PRINT 2500, V,NWHV,H,RANGE,EL,ELL,POLAR,T,(RPRINT(NN), NN=1,NP1)	
 2500 FORMAT ("yy",1X,E6.0,1X,A8,F10.4,F11.4,17X,F9.3,F8.3,F9.3,F8.3,
     14F12.4)	
      GO TO 40	

CP-80
C******** AZIMUTH ANGLE OF RAY POINT FROM TRANSMITTER	
  30    ANGA=ATAN2 (ETA, ZETA) 	

        AZDEV=180.-AMOD(540.-(AZ1-ANGA)*DEGS,360.) 	

        IF (R(5).NE.0..OR.R(6).NE.0.) GO TO 34 	

C********* WAVE NORMAL IS VERTICAL, SO AZIMUTH DIRECTION CANNOT BE	
C********* CALCULATEO	
       PRINT 3000,V,NWHY,H,RANGE,AZDEV,EL,ELL,POLAR,T,
     1 (RPRINT(NN),NN=1,NP1) 


 3000  FORMAT("zz",1X,E14.6,1X,A8,F10.4,F11.4,F9.3,8X,F9.3,F9.3,F9.3,
     1  F8.3,4F12.4)	



	   GO TO 40	
 34	   ANA=ANGA-ALPH	
	   SANA=SIN (ANA)	
	   SPHI=SANA*STHR/STH	

       CPHI=-SIN (PID2-ANA)*SIN (PID2-(PHI-PHIR))+SANA*SIN (PHI-PHIR)
     1 *CTHR	


      AZA=180.-AMOD(540.-(ATAN2(SPHI,CPHI)-ATAN2(R(6),R(5)))*DEGS,360.)

      PRINT 3500,V,NWHY,H,RANGE,AZDEV*AZA,EL,ELL,POLAR,T,
     1(RPRINT(NN),NN=1,NP1)

	
 3500  FORMAT ("uu",1X,E12.6,1X,A8,F10.4,2x, F11.4,
     4 2(2x F9.3,F8.3),2x,F9.3,2x,F8.3, 4(F12.4))
        	

		
C*********	
  40   LINES=LINES+1		
			IF (NP.LE.3) GO TO 45	

C********* ADDITION4L LINE TO PRINT REMAINING DEPENDENT INTEGRATION 	PRINi73
C********* VARIABLES	PRIN174
			PRINT 4000, (RPRINT(NN),NN=4,NP)	
 4000   FORMAT ("vv",99X,3F12.4)		
			LINES=LINES+1	
 45     IF (CARD.EQ.0.) RETURN		
c			
C********* PUNCH A RAYSET	
       IF (AZDEV.LT.-90.) AZDEV=AZDEV+360.	

	   IF (AZA.LT.-90.) AZA=AZA+360.	
	   TDEV=T-SR	
	   NR=6	
	   IF (W(57).EQ.0.) GO TO 47	
C********* PHASE PATH	
	   NR=NR+1	
	   PDEV=R(NR) -SR	

 47    IF (W(58).EQ.0.) GO TO 48	
Cf******** ABSORPTION	

	   NR=NR+1	
	   ABSORB=R(NR)	

C****#**** DOPPLER SHIFT	
 48   IF (W (59) . NE. 0 .) DOPP=R(NR+1)	
CSHA used to be puch
       PRINT  4500,HPUNCH,RANGE,AZDEV,AZA,ELL,SR,TDEV,PDEV,ABSORB,DOPP, 
     1   POLAR,IHOP,NWHY	
 4500  FORMAT ("tt",4P2F9.0,3P3F6.0,3PF8.0,3P4F6.0,2P2F5.0,I1,A1)	

	   RETURN	
	   END	

CP-81
