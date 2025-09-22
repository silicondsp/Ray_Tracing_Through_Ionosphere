# Ray_Tracing_Through_Ionosphere
Ray Tracing Through the IONOSPHERE using  Mathematica® with Notebook Code.  (c) 2018 Sasan Ardalan, The Mathematica® Code is based on the FORTRAN Code by R. Michael Jones, Judith J. Stephenson, A Versatile Three-Dimensional Ray Tracing Compute Program for Radio Waves in the Ionosphere, OT Report 75-75, US Department of Commerce, October 1975



 
<h1 align="center">Ray Tracing Through the IONOSPHERE</h1>
    <h1 align="center" >Open Source Mathematica &reg; Code </h1>
    <p>
    Visit <a href="">Radio Calc</a></p>
</p>
 <strong>Ray Tracing Through the IONOSPHERE    FORTRAN  Code. Click <a href="Ray_Tracing_Ionosphere_Project_FORTRAN_Code.html">Here</a>.</strong>
 <img src="raytracing_fortran_results_cap7_grouped-1024x410.png">
 <img src="raytracing_fortran_results_cyan_black-1024x524.png">
    <p><strong>For Description</strong> Click <a href="#description">Here</a></p>
    <table width="1428" border="1">
      <tbody>
        <tr>
          <th width="40" scope="col"><div align="center">Case</div></th>
          <th width="1022" scope="col">Description</th>
          <th width="104" scope="col">Code</th>
          <th width="53" scope="col">WEB View</th>
          <th width="175" scope="col">Result Plots</th>
        </tr>
        <tr>
          <td><div align="center">1</div></td>
          <td>Mathematica&reg; Notebook for Ray Tracing Radio Waves Through the IONOSPHERE. Based on the original FORTRAN Code in [Jones, Stephenson, 1975]. GPL  version 3 .</td>
          <td><a href="SourceCode/Mathematica/RayTrace_IONOSPHERE_NoteBook.nb">Notebook</a></td>
          <td><a href="SourceCode/Mathematica/RayTrace_IONOSPHERE_NoteBook_HTML/RayTrace_IONOSPHERE_NoteBook.htm"> HTML</a></td>
          <td><p><a href="../MultiConductorCAD/Microstrip/Near_End_Cross_Talk_Mathematica_Code_Based_On_Ardalan_et_al_1988.png"> </a>1- Ray Path Through IONOSPHERE <a href="Documentation/RayTraceIONOSPHERE_Earth_3D_45_45/RayTrace_Ionosphere_Earth_2nd_HOP_post_process_45_45.png">View 1</a></p>
          <p>2- Ray Path Through IONOSPHERE <a href="Documentation/RayTraceIONOSPHERE_Earth_3D_45_45/RayTrace_Ionosphere_Earth_post_process_45_45.png">View 2</a></p>
          <p>3-<a href="Documentation/RayTraceIONOSPHERE_Earth_3D_45_45/arrows_k_vector_reversing.png">Propagation Vector </a></p></td>
        </tr>
        <tr>
          <td><div align="center">2</div></td>
          <td>FORTRAN Code for Ray Tracing Radio Waves Through the IONOSPHERE. Cleaned up and debugged from Scan and OCR of [Jones, Stephenson, 1975] Original CDC Mainframe FORTRAN. Compiles with <em>gfortran</em>. Note: Use a Professional Compiler until some issues with <em>gfortran</em> need to be resolved. </td>
          <td><a href="Ray_Tracing_Ionosphere_Project_FORTRAN_Code.html">FORTRAN</a></td>
          <td>--</td>
          <td><a href="../MultiConductorCAD/Microstrip/Near_End_Cross_Talk_Mathematica_Code_Based_On_Theory_Hill_1994.png"> </a></td>
        </tr>
        <tr>
          <td><div align="center">3</div></td>
          <td>Simulation Results. Matrix of all Ray Vectors ( 800 iterations) one ray trace case. Elevation 45 degrees. See ray.dat. </td>
          <td><p><a href="Documentation/RayMatrix.wdx">RayMatrix.wdx</a></p>
          <p><a href="Documentation/W.wdx">W.wdx</a></p></td>
          <td>--</td>
          <td><a href="../MultiConductorCAD/Microstrip/Microstrip_Ardalan_Riddle_Theory_Far_End_ Hill_1994_Experiment.png"> </a>--</td>
        </tr>
        <tr>
          <td>&nbsp;</td>
          <td>Parameter text file. Place in same directory as Mathematica&reg; Notebook.</td>
          <td><p>(1)<a href="Documentation/ray.dat"> ray.dat</a></p>
          <p>(2) <a href="Documentation/ray.csv">ray.csv</a></p></td>
          <td>--</td>
          <td>--</td>
        </tr>
        <tr>
          <td><div align="center">5</div></td>
          <td>Latex File for Hamiltonian, Derivatives and Symbol Table</td>
          <td><p><a href="Documentation/formulas-ray_tracing.tex">Latex File</a></p></td>
          <td>--</td>
          <td><div align="center"><a href="Documentation/formulas-ray_tracing.pdf"><img src="../pdf.gif" width="22" height="21" alt=""/></a><a href="../MultiConductorCAD/Microstrip/Far_End_Cross_Talk_S41_Measurements_Hill_1994.png"></a></div></td>
        </tr>
      </tbody>
    </table>


    
   <h2 class="style1"><div id=tools> Tools</div></h2>
    <table width="1129" border="1">
      <tbody>
        <tr>
          <th width="47" scope="col"><div align="center"><div id="tools1">Item</div></th>
          <th width="484" scope="col">Description</th>
          <th width="87" scope="col">Code</th>
          <th width="81" scope="col">WEB View</th>
          <th width="81" scope="col">Graphic</th>
        </tr>
        <tr>
          <td><div align="center">1</div></td>
          <td>Tools for Postprocessing Ray Matrix from Ray Trace Mathematica&reg; Notebook. Propagation Vector. </td>
          <td><a href="SourceCode/Mathematica/RayPathPostProcessingPropagationVector.nb">Notebook</a></td>
          <td><a href="SourceCode/Mathematica/RayPathPostProcessingPropagationVector_HTML/RayPathPostProcessingPropagationVector.htm">HTML</a></td>
          <td><a href="Documentation/RayTraceIONOSPHERE_Earth_3D_45_45/arrows_k_vector_reversing.png">Propagation Vector</a></td>
        </tr>
        <tr>
          <td><div align="center">2</div></td>
          <td>Tools for Postprocessing Ray Matrix from Ray Trace Mathematica&reg; Notebook. Ray Trace Curve.</td>
          <td><a href="SourceCode/Mathematica/RayTracingPostProcessingRayPosition.nb">Notebook</a></td>
          <td><a href="SourceCode/Mathematica/RayTracingPostProcessingRayPosition_HTML/RayTracingPostProcessingRayPosition.htm">HTML</a></td>
          <td><a href="Documentation/RayTraceIONOSPHERE_Earth_3D_45_45/RayTrace_Ionosphere_Earth_2nd_HOP_post_process_45_45.png">Two Hop</a></td>
        </tr>
        <tr>
          <td><div align="center">3</div></td>
          <td>Mathematica&reg; Code for FORTRAN PRINTR Routine. Work in Progress. Incomplete.</td>
          <td><a href="SourceCode/Mathematica/RayPathPrintrWorkInProgress.nb">Notebook</a></td>
          <td><a href="SourceCode/Mathematica/RayPathPrintrWorkInProgress/RayPathPrintrWorkInProgress.htm">HTML</a></td>
          <td><a href="Documentation/Check_PRINTR.jpg">Preliminary</a></td>
        </tr>
      </tbody>
    </table>
    <h2 class="style1">References:</h2>
    <p style="margin-left:1em;">1- R. Michael Jones, Judith J. Stephenson, <strong>A Versatile Three-Dimensional  Ray Tracing <br>
Compute Program for Radio Waves in the Ionosphere</strong>, OT Report 75-75, US Department of Commerce,  October 1975 <span style="margin-left:1em;"><a href="https://www.its.bldrdoc.gov/publications/download/75-76.pdf

"><img src="pdf.gif" width="22" height="21" alt=""/>PDF Report</a></span>.</p>
    <h1 align="center" class="style1" style="margin-left:1em;"><div id="description">Description</div></h1>
    <p style="margin-left:1em;"> Ray Trace Through the IONOSPHERE  Mathematica&reg; Notebook <br>
Copyright (C)2018 Sasan Ardalan <br>
This program is free software:you can redistribute it and/or modify it under the terms of the GNU General Public License as published by <br>
the Free Software Foundation,either version 3 of the License,or at your option) any later version.This program is distributed <br>
in the hope that it will be useful, but WITHOUT ANY WARRANTY <br>
without even the implied warranty of <br>
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.</p>
    <p style="margin-left:1em;">See the <br>
      GNU General Public License for more details.You should have received a copy of the GNU General Public License along with this program.</p>
    <p style="margin-left:1em;">If not, see&lt;http://www.gnu.org/licenses/&gt;. <br>
      Author:Sasan Ardalan <br>
  Date:December 22,2018 <br>
  <br>
      <a href="http://www.radiocalc.com">http://www.radiocalc.com </a></p>
    <p style="margin-left:1em;">The Mathematica&reg; Code is based on the FORTRAN Code by [Jones, Stepehnson, 1975]. See Reference above. </p>
    <p style="margin-left:1em;"> The Code was scanned and OCR'd (back in 2002). The corrections took a major effort. Debugged more in December 2018. Obviously a very error prone procedure. The code in the OT Report 75-76 had some issues related to porting from a CDC Mainfram to <em>gfortran</em>. <br>
  Also the CDC FORTRAN allowed for some non standard variable persistance between subroutine calls. Also some assignments were fixed especially for DRDT(1). <br>
      A debug option with extensive reporting on computations  was added. <br>
    The routine POL CAR was modified. <br>
    </p>
    <p style="margin-left:1em;">The  Mathematica&reg;  code eliminates all the FORTRAN GO TO statements. Also one 
      gets a better view of global variables.Mathematica&reg;  uses color coding on variables (local global).  
 <p>
      The  Mathematica&reg;  Modules hopefully clear up what variables are global. Will be nice to elliminate most global variables (except the W Array) from the code. <br>
 </p>  
  <p>
      The Mathematica Code is a great basis for porting to Java  and other Programming languages ( we have eliminated the Go TO's as stated before plus extensive work for structured programming). <br>
 </p>   
  <p> 
      The effort is to capture the setup used by Jones and Stephenson.
  </p>     
  With Mathematica&reg;   vast 3D Graphing capabilties are available as well as 2-D Graphing. Also easy to access Matrices and Arrays for analysis. At any rate getting the excellent work by Jones and Stephenson into Mathematica&reg; opens up a huge opportunity for Research and Development in the area of Ray Tracing through the IONOSPHERE. </p>
    <p style="margin-left:1em;">Note in FORTRAN Code Elevation at 90 Degrees is Vertical.</p>
    <p> <p style="margin-left:1em;">The GPL Copyright applies to the Mathematica&reg; Code. </p>
    <p align="center" style="margin-left:1em;"><img src="Documentation/Spherical_Coordinates.png" width="486" height="435" alt=""/></p>
    <p align="center" style="margin-left:1em;"><strong>Spherical Coordinates</strong></p>
    <p style="margin-left:1em;">&nbsp;</p>
    <p style="margin-left:1em;"><img src="Documentation/List_of_Symbols.png" width="818" height="473" alt=""/></p>
    <p align="left" style="margin-left:1em;"><strong>Hamiltonian and Derivatives</strong></p>
    <p style="margin-left:1em;"><img src="Documentation/Hamiltoinian_and_Derivatives_Ray_Tracing.png" width="690" height="543" alt=""/></p>
    <p align="left" style="margin-left:1em;"><strong>Ray Vector</strong></p>
    <p style="margin-left:1em;"><img src="Documentation/Ray_Vector.png" width="445" height="260" alt=""/></p>
    <p align="left" style="margin-left:1em;"><strong>Ray Vector Options</strong></p>
    <p style="margin-left:1em;"><img src="Documentation/Ray_Vector_Options.png" width="714" height="303" alt=""/></p>
    <h2 align="center"><span class="style1">Usage Guidance </span><br>
    </h2>
      <br>
       <p style="margin-left:1em;">The code is written so  
      one ray launch  is analysed. 
      Set the Elevation and Azimuth and the Transmitter Longitude and Lattitude. All   key parameters including above  read through the test file: <br>
      <em>ray.dat</em> ( Mathematica&reg; Reads the CVS file <em>ray.csv </em>that we have prepared based on the original <em>ray.dat </em>file).<br>
      This is the same file used by Jones and Stephenson. <br>
  <br>
      Earth Radius set in the code: <br>
  <br>
      EARTHR=6370. ;  W[[2]]=EARTHR; <br>
  <br>
      The Mathematica&reg; code uses the W array as in the FORTRAN code as a database of parameters just as in Jones and Stephenson          
    <p style="margin-left:1em;">Set the maximum iterations in the TraceRT routine loop: <br>
      <br>
      MAXITERATIONS <br>
  <br>
      Multi Hop is different than Jones and Stephenson since we only use the Iterations loop. <br>
  <br>
      The results are store in the matrix: <em>RayMatrix</em> which is stored in a file using Mathematica&reg;: <br>
  <br>
      <em>Export[&quot;RayMatrix.wdx&quot;,RayMatrix]; </em><br>
  <br>
      This way the ray information (state vector) is available for Post Processing. </p>
    <p style="margin-left:1em;">We have also translated the Subroutine PRINTR to Mathematica&reg; code. <br>
      <br>
      There are some issues with Fortran ATAN2 and Mathematica ArcTan so this is a work in progress. <br>
      <br>
      The Mathematica&reg; Notebook implements one  case out of many possible models as explained in Jones and Stephenson. <br>
      <br>
      Hopefully, by following the Mathematica code, one can substitute various models as outlined in Jones and Stephenson. </p>
    <p style="margin-left:1em;">&nbsp;</p>
    <p align="center">&nbsp;</p>
    <p align="center">&nbsp;</p>
    <p align="center">&nbsp;</p>
    <p align="center">&nbsp;</p>
    <p align="center">&nbsp;</p>
    <p align="center">&nbsp;</p>
    <div id=notes111></div>
<div id="notes"><strong>Notes</strong></div>
     <p style="margin-left:1em;">&nbsp; </p>
  
  
 
