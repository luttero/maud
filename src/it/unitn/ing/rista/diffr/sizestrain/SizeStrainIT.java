/*
 * @(#)SizeStrainIT.java created 19/6/1999 Riva del Garda
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.diffr.sizestrain;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.interfaces.Peak;

/**
 *  The SizeStrainIT is a class
 *
 *
 * @version $Revision: 1.6 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class SizeStrainIT extends SizeStrainModel {

  public SizeStrainIT(XRDcat aobj, String alabel) {
    super(aobj, alabel);
    initXRD();
    identifier = "IT";
    IDlabel = "IT";
    description = "select this to apply the IT model";
  }

  public SizeStrainIT(XRDcat aobj) {
    this(aobj, "Line Broadening IT model");
  }

  public SizeStrainIT() {
    identifier = "Disabled IT";
    IDlabel = "IT";
    description = "select this to apply the IT model";
  }

  public double getBetaChauchy(double dspace, double cryst, double mstrain) {
    if (cryst == 0.0)
      return 0.0;
    return dspace * dspace / Math.abs(cryst);
  }

  public double getBetaGauss(double dspace, double cryst, double mstrain) {
    return 2.0 * Math.abs(mstrain) * Constants.mstraintoetilde * dspace;
  }

}

/*

--------------ITN (Fourier?)-------------------

'******************************************************************************
20000 'SUBROUTINE per il calcolo della funzione da minimizzare Y
'******************************************************************************
20010 '
      INDG%=NK+NK*BGR+ML1%+2
      D0PL=PI/SIN(PARM(INDG%-1)*PI/360)  '*LAMB
      FOR K%=0 TO MAXN
       G1(K%)=0
       FOR J%=1 TO ML1%
	GW=SIN((PARM(INDG%-1)+PARM(INDG%+2+J%))/360*PI)*D0PL
      	GW1=SIN(GW*K%*STEPn)
 	GW2=SIN(GW)
 	G1=GW1*GW1/(GW2*GW2)
 	G1(K%)=G1(K%)+PARM(J%+BGR*NK+NK)*G1
       NEXT J%
      NEXT K%
      L=1E+5
      FOR I%=0 TO MAXN
       GW=GW+EXP(-G1(I%))/L
      NEXT I%
      RHOM=0
      FOR I%=0 TO MAXN
       RHO(I%)=EXP(-G1(I%))/GW*L
       IF RHOM<RHO(I%) THEN RHOM=RHO(I%)
      NEXT I%
      dxint=STEPn/3
      for i%=0 to MAXN
       rhoint=RHO(MAXN)*(MAXN-i%)
       for j%=i%+1 to MAXN-1
        if (j%-i%)/2<>int((j%-i%)/2) then
      	 rhoint=rhoint+4*(j%-i%)*RHO(j%)
        else
         rhoint=rhoint+2*(j%-i%)*RHO(j%)
        end if
       next j%
       asn(i%)=rhoint*STEPn/3
       asn(i%)=asn(i%)/asn(0)
       GAM(i%)=PARM(INDG)*(i%*STEPn*D0PL*LAMB/2/PI)^(PARM(INDG+1)
      next i%


      Y=0:ERROVER=FALSE
      K%=NK+NK*BGR+ML1%+4
      DTH=360/PI*PARM(K%)*COS(PARM(K%-2)/360*PI)/RAGG
      FOR I=IND0%-NDST TO IND1%+NDST
      X(I)=XFI(I)-DTH
      NEXT I
      IF NK=1 GOTO 20020
      AQ=2*SIN(PARM(K%-2)*PI/360)
      XS=ATN(AQ/(1-AQ^2)^.5)
      DTH=360/PI*PARM(K%)*COS(XS)/RAGG
      FOR I=IND2%-NDST TO IND%+NDST
      X(I)=XFI(I)-DTH
      NEXT I

      NMIN=1:NMAX=MAXN:NORMF=0
      BAND=TRUE:IF RHODIS THEN RETURN
      FOR J=IND0%-NDST TO IND1%+NDST
	      F1(J)=0
	      FOR K=NMIN TO NMAX
	      IF NPT>6 THEN NPM=-3*GAM(K):NPD=3*GAM(K):STNP=6*GAM(K)/(NPT-1):GOTO 20040
	      IF NPT>4 THEN NPM=-2*GAM(K):NPD=2*GAM(K):STNP=GAM(K):GOTO 20040
	      IF NPT>2 THEN
	      NPM=-GAM(K):NPD=GAM(K):STNP=GAM(K)
	      ELSE
	      NPM=0:NPD=0:STNP=SQR(LO2/PI)
	      END IF
20040                 G1=0:G2=0
		      FOR RX=NPM TO NPD STEP STNP
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB
			      GW1=SIN(GW*K*STEPn):GW2=SIN(GW)
			      GWS=STNP*EXP(-LO2*(RX/GAM(K))^2)
			      G1=G1+GWS*GW1*GW1/GW2/GW2
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB2
			      GW1=SIN(GW*K*STEPn):GW2=SIN(GW)
			      G2=G2+GWS*GW1*GW1/GW2/GW2
		      NEXT RX
	      IF NPT>2 THEN G1=(G1+G2/2)/(NPD-NPM)
	      F1(J)=F1(J)+RHO(K)*G1
	      NEXT K
	      NORMF=NORMF+F1(J)
      NEXT J
      FOR J=IND0%-NDST TO IND1%+NDST:F1(J)=F1(J)/NORMF*PARM(1):NEXT J
      IF NK=1 THEN 20055
      NORMF=0
      D0PL=D0PL/2
      FOR J=IND2%-NDST TO IND%+NDST
	      F1(J)=0
	      FOR K=NMIN TO NMAX
	      IF NPT>6 THEN NPM=-3*GAM(K):NPD=3*GAM(K):STNP=6*GAM(K)/(NPT-1):GOTO 20050
	      IF NPT>4 THEN NPM=-2*GAM(K):NPD=2*GAM(K):STNP=GAM(K):GOTO 20050
	      IF NPT>2 THEN
	      NPM=-GAM(K):NPD=GAM(K):STNP=GAM(K)
	      ELSE
	      NPM=0:NPD=0:STNP=SQR(LO2/PI)
	      END IF
20050                 G1=0:G2=0
		      FOR RX=NPM TO NPD STEP STNP
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB
			      GW1=SIN(2*GW*K*STEPn):GW2=SIN(GW)
			      GWS=STNP*EXP(-LO2*(RX/GAM(K))^2)
			      G1=G1+GWS*GW1*GW1/GW2/GW2
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB2
			      GW1=SIN(2*GW*K*STEPn):GW2=SIN(GW)
			      G2=G2+GWS*GW1*GW1/GW2/GW2
		      NEXT RX
	      IF NPT>2 THEN G1=(G1+G2/2)/(NPD-NPM)
	      F1(J)=F1(J)+RHO(K)*G1
	      NEXT K
	      NORMF=NORMF+F1(J)
      NEXT J
      FOR J=IND2%-NDST TO IND%+NDST:F1(J)=F1(J)/NORMF*PARM(2):NEXT J
      D0PL=D0PL*2

20055 FOR J=IND0% TO IND1%
      F(J)=0
      FOR JI=-NDST TO NDST
      F(J)=F(J)+F1(J-JI)*DTS(0,JI+NDST)/NORM(0)
      NEXT JI
	      FOR K=1 TO BGR
	      F(J)=F(J)+PARM(NK+K)*X(J)^(K-1)
	      NEXT K
      NEXT J
      IF NK=1 THEN 20100
      FOR J=IND2% TO IND%
      F(J)=0
      FOR JI=-NDST TO NDST
      F(J)=F(J)+F1(J-JI)*DTS(1,JI+NDST)/NORM(1)
      NEXT JI
	      FOR K=1 TO BGR
	      F(J)=F(J)+PARM(NK+BGR+K)*X(J)^(K-1)
	      NEXT K
      NEXT J
      FOR J=IND1%+1 TO IND2%-1:F(J)=0:NEXT
20100 RETURN



--------------IT (Normal?)-------------------

'******************************************************************************
20000 'SUBROUTINE per il calcolo della funzione da minimizzare Y
'******************************************************************************
20010 '
      Y=0:ERROVER=FALSE
      K%=NK+NK*BGR+ML1%+4
      DTH=360/PI*PARM(K%)*COS(PARM(K%-2)/360*PI)/RAGG
      FOR I=IND0%-NDST TO IND1%+NDST
      X(I)=XFI(I)-DTH
      NEXT I
      IF NK=1 GOTO 20020
      AQ=2*SIN(PARM(K%-2)*PI/360)
      XS=ATN(AQ/(1-AQ^2)^.5)
      DTH=360/PI*PARM(K%)*COS(XS)/RAGG
      FOR I=IND2%-NDST TO IND%+NDST
      X(I)=XFI(I)-DTH
      NEXT I
20020 GASUM=0
      INDG=NK+NK*BGR+ML1%+2
      D0PL=LAMB*PI/SIN(PARM(INDG-1)*PI/360)
      FOR K=1 TO MAXN
	      GAM(K)=PARM(INDG)*(K*STEPn*D0PL/2/PI)^(PARM(INDG+1))
	      IF NPT>6 THEN NPM=-3*GAM(K):NPD=3*GAM(K):STNP=6*GAM(K)/(NPT-1):GOTO 20030
	      IF NPT>4 THEN NPM=-2*GAM(K):NPD=2*GAM(K):STNP=GAM(K):GOTO 20030
	      IF NPT>2 THEN
	      NPM=-GAM(K):NPD=GAM(K):STNP=GAM(K)
	      ELSE
	      NPM=0:NPD=0:STNP=SQR(LO2/PI)
	      END IF
20030         G1(K)=0
	      FOR J=1 TO ML1%
		      G1=0
		      MOLT=4
		      FOR RX=NPM TO NPD STEP STNP
			GW=SIN((PARM(INDG-1)+PARM(INDG+2+J))/360*PI)*D0PL/(1-RX)/LAMB
			GW1=SIN(GW*K*STEPn):GW2=SIN(GW)
			GWS=STNP*EXP(-LO2*(RX/GAM(K))^2)
			IF RX=NPM OR RX=NPD OR CONV=0 THEN
			  G1=G1+GWS*GW1*GW1/(GW2*GW2)
			ELSE
			  G1=G1+MOLT*GWS*GW1*GW1/(GW2*GW2)
			  IF MOLT=4 THEN MOLT=2 ELSE MOLT=4
			END IF
		      NEXT RX
		      G1(K)=G1(K)+PARM(J+BGR*NK+NK)*G1/(CONV*2+1)
	      NEXT J
              IF NPT>2 THEN G1(K)=G1(K)/(NPD-NPM)
      NEXT K
      EROV=TRUE:L=.1
20035 GW=0:L=L*10:IF L>10^5 THEN EROV=FALSE
      FOR I%=1 TO MAXN
	      GW=GW+EXP(-G1(I%))/L
      NEXT I%
      RHOM=0
      FOR I%=1 TO MAXN
	      RHO(I%)=EXP(-G1(I%))/GW*L
	      IF RHOM<RHO(I%) THEN RHOM=RHO(I%)
      NEXT I%
      EROV=FALSE
      GOTO 20038
20037 ERROVER=TRUE:PRINT "OVERFLOW; REPEAT":RETURN
20038 NMIN=1:NMAX=MAXN:NORMF=0
      BAND=TRUE:IF RHODIS THEN RETURN
      FOR J=IND0%-NDST TO IND1%+NDST
	      F1(J)=0
	      FOR K=NMIN TO NMAX
	      IF NPT>6 THEN NPM=-3*GAM(K):NPD=3*GAM(K):STNP=6*GAM(K)/(NPT-1):GOTO 20040
	      IF NPT>4 THEN NPM=-2*GAM(K):NPD=2*GAM(K):STNP=GAM(K):GOTO 20040
	      IF NPT>2 THEN
	      NPM=-GAM(K):NPD=GAM(K):STNP=GAM(K)
	      ELSE
	      NPM=0:NPD=0:STNP=SQR(LO2/PI)
	      END IF
20040                 G1=0:G2=0:MOLT=4
		      FOR RX=NPM TO NPD STEP STNP
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB
			      GW1=SIN(GW*K*STEPn):GW2=SIN(GW)
			      GWS=STNP*EXP(-LO2*(RX/GAM(K))^2)
			IF RX=NPM OR RX=NPD OR CONV=0 THEN
			  G1=G1+GWS*GW1*GW1/(GW2*GW2)
			ELSE
			  G1=G1+MOLT*GWS*GW1*GW1/(GW2*GW2)
			END IF
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB2
			      GW1=SIN(GW*K*STEPn):GW2=SIN(GW)
			IF RX=NPM OR RX=NPD OR CONV=0 THEN
			  G2=G2+GWS*GW1*GW1/(GW2*GW2)
			ELSE
			  G2=G2+MOLT*GWS*GW1*GW1/(GW2*GW2)
			  IF MOLT=4 THEN MOLT=2 ELSE MOLT=4
			END IF
		      NEXT RX
	      IF NPT>2 THEN G1=(G1+G2/2)/((NPD-NPM)*(CONV*2+1))
	      F1(J)=F1(J)+RHO(K)*G1
	      NEXT K
	      NORMF=NORMF+F1(J)
      NEXT J
      FOR J=IND0%-NDST TO IND1%+NDST:F1(J)=F1(J)/NORMF:NEXT J
      IF NK=1 THEN 20055
      NORMF=0
      D0PL=D0PL/2
      FOR J=IND2%-NDST TO IND%+NDST
	      F1(J)=0
	      FOR K=NMIN TO NMAX
	      IF NPT>6 THEN NPM=-3*GAM(K):NPD=3*GAM(K):STNP=6*GAM(K)/(NPT-1):GOTO 20050
	      IF NPT>4 THEN NPM=-2*GAM(K):NPD=2*GAM(K):STNP=GAM(K):GOTO 20050
	      IF NPT>2 THEN
	      NPM=-GAM(K):NPD=GAM(K):STNP=GAM(K)
	      ELSE
	      NPM=0:NPD=0:STNP=SQR(LO2/PI)
	      END IF
20050                 G1=0:G2=0:MOLT=4
		      FOR RX=NPM TO NPD STEP STNP
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB
			      GW1=SIN(2*GW*K*STEPn):GW2=SIN(GW)
			      GWS=STNP*EXP(-LO2*(RX/GAM(K))^2)
			IF RX=NPM OR RX=NPD OR CONV=0 THEN
			  G1=G1+GWS*GW1*GW1/(GW2*GW2)
			ELSE
			  G1=G1+MOLT*GWS*GW1*GW1/(GW2*GW2)
			END IF
			      GW=SIN(X(J)/360*PI)*D0PL/(1-RX)/LAMB2
			      GW1=SIN(2*GW*K*STEPn):GW2=SIN(GW)
			IF RX=NPM OR RX=NPD OR CONV=0 THEN
			  G2=G2+GWS*GW1*GW1/(GW2*GW2)
			ELSE
			  G2=G2+MOLT*GWS*GW1*GW1/(GW2*GW2)
			  IF MOLT=4 THEN MOLT=2 ELSE MOLT=4
			END IF
		      NEXT RX
	      IF NPT>2 THEN G1=(G1+G2/2)/((NPD-NPM)*(CONV*2+1))
	      F1(J)=F1(J)+RHO(K)*G1
	      NEXT K
	      NORMF=NORMF+F1(J)
      NEXT J
      FOR J=IND2%-NDST TO IND%+NDST:F1(J)=F1(J)/NORMF:NEXT J
      D0PL=D0PL*2

20055   NORM!=0
	FOR J=IND0% TO IND1%
      F(J)=0
      FOR JI=-NDST TO NDST
      F(J)=F(J)+F1(J-JI)*DTS(0,JI+NDST)/NORM(0)
      NEXT JI
      NORM!=NORM!+F(J)
      NEXT J
      FOR J=IND0% TO IND1%
      	F(J)=F(J)/NORM!*PARM(1)
	      FOR K=1 TO BGR
	      F(J)=F(J)+PARM(NK+K)*X(J)^(K-1)
	      NEXT K
      NEXT J
      IF NK=1 THEN 20100
      NORM!=0
      FOR J=IND2% TO IND%
      F(J)=0:MOLT=4
      FOR JI=-NDST TO NDST
       IF JI=-NDST OR JI=NDST THEN
        F(J)=F(J)+F1(J-JI)*DTS(1,JI+NDST)/NORM(1)
       ELSE
        F(J)=F(J)+MOLT*F1(J-JI)*DTS(1,JI+NDST)/NORM(1)
        IF MOLT=4 THEN MOLT=2 ELSE MOLT=4
       END IF
      NEXT JI
      F(J)=F(J)/(CONV*2+1)
      NORM!=NORM!+F(J)
      NEXT J
      FOR J=IND2% TO IND%
      	F(J)=F(J)/NORM!*PARM(2)
	      FOR K=1 TO BGR
	      F(J)=F(J)+PARM(NK+BGR+K)*X(J)^(K-1)
	      NEXT K
      NEXT J
      FOR J=IND1%+1 TO IND2%-1:F(J)=0:NEXT
20100 RETURN

*/

