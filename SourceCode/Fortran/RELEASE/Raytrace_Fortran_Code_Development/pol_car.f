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
C		SUBROUTINE POL CAR(VERT,XO,X,RO,MM,NN,KK,IENTR)
 		SUBROUTINE POL CAR( IENTR)
C CHECKED
		INTEGER MM,NN,KK,IENTR	
		REAL VERT
C		DIMENSION XO(MM),X(NN),RO(KK)	
            COMMON /POLX/ XO(3)
            COMMON /POLXX/ XXX(6)
            COMMON /POLR/ RO(6),VERT

C       COMMON /POLX/ XO 
C       COMMON /POLXX/ XXX 
C       COMMON /POLR/ RO,VERT 		
		
       COMMON /DDEBUG/ debug        
		COMMON R(6) /COORD/ S		
		COMMON /CONST/ PI,PIT2,PID2,DUM(5)		
						
C				CONVERTS SPHERICAL COORDINATES TO CARTESIAN		
        if(IENTR.EQ.1) GO TO 1000
       if(debug.eq.1.) print 8882, R(1),R(2),R(3),PID2
 8882   format("POL CAR0:R(1),R(2),R(3),PID2:",4(e14.6,2x));
        if(debug.eq.1.) print 8812, R(4),R(5),R(6),PID2
 8812   format("POL CAR2:R(4),R(5),R(6),PID2:",4(e14.6,2x));
	    IF (R(5).EQ.0..AND.R(6).EQ.0.) GO TO 1	
                                                                     
        VERT=0.	
        SINA=SIN(R(2))	
        COSA=SIN(PID2-R(2))	
        SINP=SIN(R(3))	
        COSP=SIN(PID2-R(3))	
        XO(1)=R(1)*SINA*COSP	
        XO(2)=R(1)*SINA*SINP	
        XO(3)=R(1)*COSA	

CP-77


		XXX(4)=R(4)*SINA*COSP+R(5)*COSA*COSP-R(6)*SINP	
		XXX(5)=R(4)*SINA*SINP+R(5)*COSA*SINP+R(6)*COSP	
		XXX(6)=R(4)*COSA-R(5)*SINA	
		RETURN		
C			VERTICAL		INCIDENCE
						
   1    VERT=1.		
		RO(1)=R(1)		
		RO(2)=R(2)		
		RO(3)=R(3)		
		RO(4)=SIGN (1.,R(4))
       if(debug.eq.1.) print 8883, R(1),RO(1),RO(2),RO(3),RO(4),VERT
 8883   format("POL CAR1 VERTICAL :R(1),RO(1),RO(2),RO(3),RO(4):",
     3      5(e14.6,2x),L)
	
		RETURN		
c						
c				STEPS THE RAY A DISTANCE So AND THEN	
C				CONVERTS CARTESIAN COORDINATES TO SPHERICAL.COORDINATES		
CSHA		ENTRY CAR POL	
 	
 1000           CONTINUE
                if(debug.eq.1.) print 8987, VERT
 8987      format("CAR POL00000000 VERT=",e14.6)		

                IF (VERT.NE.0.) GO TO 2	
	
 		XXX(1)=XO(1)+S*XXX(4)
                
		XXX(2)=XO(2)+S*XXX(5)	
		XXX(3)=XO(3)+S*XXX(6)	
		TEMP=SQRT(XXX(1)**2+XXX(2)**2)	
		R(1)=SQRT(XXX(1)**2+XXX(2)**2+XXX(3)**2)	
        
        if(debug.eq.1.) print 8888, S,R(1),XXX(1),XXX(2),XXX(3),
     4              XO(1),XO(2),XO(3)
 8888   format("S=",e14.6,2x,"CAR POL2:R(1),X(1),X(2),X(3),XO(N):"
     3    ,7(e14.6,2x))
        
		R(2)=ATAN2(TEMP,XXX(3))	
		R(3)=ATAN2(XXX(2),XXX(1))	
		R(4)=(XXX(1)*XXX(4)+XXX(2)*XXX(5)+XXX(3)*XXX(6))/R(1)	
        R(5)=(XXX(3)*(XXX(1)*XXX(4)+XXX(2)*XXX(5))-(XXX(1)**2
     2  +XXX(2)**2)*XXX(6))/	
     1 (R(1)*TEMP)		
       R(6)=(XXX(1)*XXX(5)-XXX(2)*XXX(4))/TEMP	
         if(debug.eq.1.) print 8887, R(1),RO(1),RO(4),S
         if(debug.eq.1.) print 8087, XXX(1),XXX(2),XXX(3),
     5  R(1),R(2),R(3),S,XO(1),XO(2),XO(3)
 8087   format("YYYYY   CAR POL5:X(1),X(2),X(3),S ",10(e14.6,":",2x))
        if(debug.eq.1.) print 8037, XXX(4),XXX(5),XXX(6),
     5   R(4),R(5),R(6),S
 8037   format("QQQQ   CAR POL5:X(4),X(5),X(6),R(4),R(5),R(6) S  ",
     5 7(e14.6,":",2x))

CSHAxxx        do 666 ii=1,3
CSHAxxx  666        XO(ii)=XXX(ii) 
		RETURN		
C				VERTICAL INCIDENCE		
   2    CONTINUE
           if(debug.eq.1.) print 8387, VERT
 8387      format("CAR POL11111 VERT=",e14.6)		


            R(1)=RO(1)+RO(4)*S 
		R(2)=RO(2) 
		R(3)=RO(3) 
		R(4)=RO(4) 
		R(5) =0. 
		R(6)=0. 
         if(debug.eq.1.) print 8887, R(1),RO(1),RO(4),S,VERT
 8887   format("CAR POL3001 VERT:R(1),RO(1),RO(4),S:",4(e14.6,2x),
     7   "VERT=",e14.6) 
		
        if(debug.eq.1.) print 8187, R(1),R(2),R(3),S,
     6    RO(1),RO(2),RO(3),RO(4),VERT
 8187 format("XXXXX VERT CAR POL31:R(1),R(2),R(3),S ",4(e14.6,":"),
     4  "RO ",4(e14.6,":",2x,"VERT=",e14.6))
       if(debug.eq.1.) print 8147, R(1),RO(1),RO(4),S 
      
 8147 format("WWWW VERT CAR POL32: R(1),RO(1),RO(4),S ",4(e14.6,":",2x))
    	
		RETURN		
		END		
		

CP-78
