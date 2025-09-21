    A VERSATILE THREE·DIMENSIONAL RAY TRACING COMPUTER
    PROGRAM FOR RADIO WAVES
    IN THE IONOSPHERE
    
    R. MICHAEL JONES
    JUDITH J. STEPHENSON
    
    U.S.  DEPARTMENT OF COMMERCE
    October 1975
 
     FORTRAN Code from Scanned Report and OCR in 2002 
     by Sasan Ardalan
 
     Compiles and Links using gfortran
     
     Rework in December 2018 by Sasan Ardalan
 
     Detailed work to match source code with published code.
     Major work to change from CDC Main Frame Fortran
     to gfortran:
     
     https://gcc.gnu.org/fortran/
 
     Massive effort in debugging.
     Also debug option for  printing of variables. See nitial.f
 
     This FORTRAN Code available at:
         http://www.radiocalc.com
 
     Visit the site for plots of raytracing.
     Also translation to Mathematica by Sasan Ardalan
     
     We have also included the handy PERL script:
          process_raytracing.pl
     This is in the Results folder.
     
     Note that we added the text "uu" to each relavant line in
     the FORTRAN results so that the line can be grabbed  with PERL 
     and the Height and Range extracted (and Swapped) to plot the ray trace.

     Issues:
     1-Had to change  SSTEP error in reach.f to 0.5E-3 so that elevation angles 
     below 30 degrees work ( 15 and 0).
     2- Results follow the OT Report 75-76, however do not match which could be for a
        number of reasons. Need to verify that models are exactly the same.
     3- At close to and including elevation of 90 degrees results are off 
         when wave reversus. The range starts to decrease. 
     4- If we continue to debug  it would never end and you would not be reading this.
     5- GFORTRAN needs to be checked against a commercial or another FORTRAN compiler.
     6- Cases where gfortran  doing something or not doing something for no good reason.
     7- Will try and document and submit to GFORTRAN group.
     8- Check nitial for some hardwired values ( W values ignored).
     9- Added COMMON  /RK/ DELY ,BET ,XV ,FV ,YU ,RR,LL,MM,ALPHA,EPM to rkam.f and trace.f 
        this was a major change since original CDC FORTRAN must have assumed variable persistance
        between subroutine calls.
     10- In CDC FORTRAN DRDT(1) was set using only DRDT. Fixed this in hamltn.f 



     Do Not Remove This Notice
 
 

