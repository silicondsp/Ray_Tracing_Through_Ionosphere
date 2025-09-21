
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

CSHA	    SUBROUTINE AHWFWC
CPage 105
	     SUBROUTINE RINDEX	    
	    	
C CHECKED
C		CALCULATES THE REFRACTIVE INDEX AND ITS GRADIENT USING THE 	
C		APPLET04-HARTREE FDRMULA WITH FIEL09 WITH COLLISIONS	

       COMMON /DDEBUG/ debug        
		COMMON /CONST/ PI,PIT2,PID2,DEGS,RADIAN,K,C,LOGTEN	
		COMMON /RIN/ MODRIN(3),COLL,FIELD,SPACE,KAY2,H,PHPT,PHPR,PHPTH,	
     1	   PHPPH,PHPOM,PHPKR,PHPKTH,PHPKPH,KPHPK,POLAR,LPOLAR
     2     ,SGN 
	  COMMON /XX/ MODX(2),X,PXPR,PXPTH,PXPPH,PXPT,HMAX	
      COMMON 	/YY/ MODY,Y,PYPR,PYPTH,PYPPA,YR,PYRPR,PYRPT,PYRPP,YTH,PYTPR
     1   ,PYTPT,PYTPP,YPH,PYPPR,PYPPT,PYPPP	

      COMMON /ZZ/ MODZ,Z,PZPR,PZPTH,PZPPH	
      COMMON R,TH,PH,KR,KTH,KPH	  /WW/ ID(10),W0,W(400)	
      COMMON /RK/ N,STEP,MODE,E1MAX,E1MIN,E2MAX,E2MIN,FACT,RSTART	
      
CSHA      EQUIVALENCE (RAY,W(1)),(F, W(6))      
      EQUIVALENCE (RAY,W(1)),(F, W(7))	
      LOGICAL SPACE
      LOGICAL TT1,TT2,TT3			
      REAL KR,KTH,KPH,K2			
      COMPLEX N2,PNPR,PNPTH,PNPPH,PNPVR,PNPVTH,PNPVPH,NNP,PNPT,		
     1  POLAR, LPOLAR,I,U,RAD,D,PNPPS,PNPX,PNPY,PNPZ,UX,UX2,D2,	
     2	KAY2,H,PHPT,PHPR,PHPTH,PHPPH,PHPOM,PHPKR,PHPKTH,PHPKPH,	
     3			KPHPK		
C       DATA MODRIN/8HAPPLETON, 8H-HARTREE,8H FORMULA/,COLL/1./,
       DATA COLL/1./,				
     1		FIELD /1./,		
     2		X /0./,PXPR /0./,PXPTH /0./,PXPPH /0./,PXPT /0./,	
     3		Y /0./,PYPR/0./,PYPTH/0./,PYPPH /0./,YR/0./,PYRPR/0./,	
     4  	PYRPT /0./,PYRPP/0./,YTH/0./,PYTPR /0./,PYTPT /0./,	
     5		PYTPP /0./,YPH /0./,PYPPR /0./,PYPPT /0./,PYPPP /0./	
     6	    ,Z /0./,PZPR /0./,PZPTH /0./,PZPPH /0./,	
     7		I /(0.,1.)/,ABSLIM /1.E-5/	
CSHA	   ENTRY RINDEX	
	   
C	   MODRIN(1)="APPLETON"
C	   MODRIN(2)="-HARTREE"
C	   MODRIN(3)=" FORMULA"
	   
	   
	   OM=PIT2*1.E6*F	
	   C2=C*C	
	   K2=KR*KR+KTH*KTH+KPH*KPH	
	   OM2=OM*OM	
	   VR =C/OM*KR	
	   VTH=C/OM*KTH	
	   VPH=C/OM*KPH
	   if (debug.eq.1.) print 8887,C,Z,VR,VTH,VPH,KR,KTH,KPH,K2
 8887  format("ahwfwc1:C,Z,VR,VTH,VPH,KR,KTH,KPH,K2",2(e14.6,2x),
     6    6(e14.6,2x,e14.6,2x))
	   
	   CALL ELECTX()
	   CALL MAGY
	   if (debug.eq.1.) print 8886,C,Z,VR,VTH,VPH
 8886  format("ahwfwc2:C,Z,VR,VTH,VPH",2(e14.6,2x),
     6    3(e14.6,2x,e14.6,2x))
	   
	   			
	   V2=VR**2+VTH**2+VPH**2	
	   VDOTY=VR*YR+VTH*YTH+VPH*YPH	
	   YLV=VDOTY/V2	
	   YL2=VDOTY**2/V2	
	   YT2=Y**2-YL2	
	   YT4=YT2*YT2	
	   
	   if (debug.eq.1.) print 8885,Y,V2,VDOTY,YL2,YT2
 8885  format("ahwfwc3:Y,V2,VDOTY,YL2,YT2",2(e14.6,2x),
     6    6(e14.6,2x,e14.6,2x))
 	   
	   CALL COLFRZ			
	   U=CMPLX(1.,-Z)	
	   UX=U-X	
	   UX2=UX*UX	
	   RAD=RAY*CSQRT(YT4+4.*YL2*UX2)	
	   D=2.*U*UX-YT2+RAD	
	   D2=D*D	
	   N2=1.-2.*X*UX/D
	   if (debug.eq.1.) print 8888,R,Z,X,N2,KAY2,UX,D,U,YT2
 8888  format("ahwfwc4:R(1),Z,X,N2,KAY2,UX,D",e14.6,2x,2(e14.6,2x),
     6    6(e14.6,2x,e14.6,2x))
 	
	   PNPPS=2.*X*UX*(-1.+(YT2-2.*UX2)/RAD)/D2	
	   PPSPR =YL2/Y*PYPR -(VR*PYRPR+VTH*PYTPR+VPH*PYPPR)*YLV		
	   PPSPTH=YL2/Y*PYPTH-(VR*PYRPT+VTH*PYTPT+VPH*PYPPT)*YLV	
	   PPSPPH=YL2/Y*PYPPH-(VR*PYRPP+VTH*PYTPP+VPH*PYPPP)*YLV	
           PNPX=-(2.*U*UX2-YT2*(U-2.*X)+(YT4*(U-2.*X)+
     5           4.*YL2*UX*UX2)/RAD)/D2
	   PNPY=2.*X*UX*(-YT2+(YT4+2.*YL2*UX2)/RAD)/(D2*Y)	
	   PNPZ=I*X*(-2.*UX2-YT2+YT4/RAD)/D2	
	   PNPR =PNPX*PXPR +PNPY*PYPR +PNPZ*PZPR +PNPPS*PPSPR			
	   PNPTH=PNPX*PXPTH+PNPY*PYPTH+PNPZ*PZPTH+PNPPS*PPSPTH	
	   PNPPH=PNPX*PXPPH+PNPY*PYPPH+PNPZ*PZPPH+PNPPS*PPSPPH	
	   PNPVR =PNPPS*(VR *YL2/V2-YLV*YR )	
	   PNPVTH=PNPPS*(VTH*YL2/V2-YLV*YTH)	
	   PNPVPH=PNPPS*(VPH*YL2/V2-YLV*YPH)	
	   NNP=N2-(2.*X*PNPX+Y*PNPY+Z*PNPZ)	

CP-93

       PNPT=PNPX*PXPT	

C	   SPACE=REAL(N2).EQ.1..AND.(ABS(AIMAG(N2)).LT.ABSLIM)
C       TT3=REAL(N2).EQ.1..AND.(ABS(AIMAG(N2)).LT.ABSLIM)
C111       ZZZZ=REAL(N2)
C       TT1=(ABS(ZZZZ-1.0).LT.0.000000001)
C111        TT1=(ABS(ZZZZ-1.0).EQ.0.00000000)


       TT1=REAL(N2).EQ.1.
       TT2=(ABS(AIMAG(N2)).LT.ABSLIM)
       SPACE=REAL(N2).EQ.1..AND.ABS(AIMAG(N2)).LT.ABSLIM
	   if (debug.eq.1.) print 8882,N2,R,ABSLIM,TT1,TT2,TT3
 8882  format("ahwfwc55:SPACE,N2,R(1),ABSLIM",2x,
     1 e20.10,2x,e14.6,2x,e14.6,2x,e14.6," COMPN2=",L,
     2 " COMPIN2=",L," TT3=",L);
	   
	   	
       POLAR= -I*SQRT(V2)*(-YT2+RAD)/(2.*VDOTY*UX)	
	   GAM= (-YT2+RAD)/(2.*UX)	
	   LPOLAR=I*X*SQRT(YT2)/(UX*(U+GAM))	
	   KAY2=OM2/C2*N2	
	   IF(RSTART.EQ.0.) GO TO 1	
	   SCALE=SQRT (REAL (KAY2)/K2)	
	   KR =SCALE*KR	
	   KTH=SCALE*KTH	
	   KPH=SCALE*KPH	
 1     CONTINUE	

C********* CALCULATES A HAMILTONIAN H	
	   H=.5*(C2*K2/OM2-N2)	

C********* AND ITS PARTTAL DERIVATIVES WITH RESPECT TO	WFWC080

Cf******** TIME, R, THETA, PHI, OMEGA, KR, KTHETA, AND KPHI.	
	   PHPT =-PNPT	
	   PHPR =-PNPR	
	   PHPTH=-PNPTH	
	   PHPPH=-PNPPH	
	   PHPOM=-NNP/OM	

       PHPKR =C2/OM2*KR -C/OM*PNPVR	

	   PHPKTH=C2/OM2*KTH-C/OM*PNPVTH	
	   PHPKPH=C2/OM2*KPH-C/OM*PNPVPH	
	   KPHPK=N2	
	   if (debug.eq.1.) print 7776,R
 7776  format("ahwfwc66:R",E14.6);
	   
	   RETURN	
	   END	


