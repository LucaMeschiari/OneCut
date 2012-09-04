/* MTU2.f -- translated by f2c (version 20060506).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Table of constant values */

static real c_b26 = 0.f;
static integer c__1 = 1;

/* Subroutine */ int mtu2_(integer *n, integer *p, integer *w, integer *c__, 
	integer *z__, integer *x, integer *jdim, integer *jfo, integer *jck, 
	integer *jub, integer *po, integer *wo, real *xo, real *rr, integer *
	pp)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i__, j, k, ib, kc, if__, kf, jj, il, kk, ip;
    static real rk;
    static integer if1, ip1, ip2, ip3, is1, is2, iw1, iw2, iw3, icr, jpk, jwk,
	     jpx, ixo1;
    extern /* Subroutine */ int mtu1_(integer *, integer *, integer *, 
	    integer *, real *, integer *, real *, integer *, integer *, 
	    integer *, real *);
    static integer lim12;
    extern /* Subroutine */ int redu_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static integer icrr, icws;
    extern /* Subroutine */ int sortr_(integer *, real *, integer *, integer *
	    ), chmtu2_(integer *, integer *, integer *, integer *, integer *, 
	    integer *), ksmall_(integer *, real *, integer *, integer *, real 
	    *);
    static integer itrunc;


/* THIS SUBROUTINE SOLVES THE UNBOUNDED SINGLE KNAPSACK PROBLEM */

/* MAXIMIZE  Z = P(1)*X(1) + ... + P(N)*X(N) */

/* SUBJECT TO:   W(1)*X(1) + ... + W(N)*X(N) .LE. C , */
/*               X(J) .GE. 0 AND INTEGER  FOR J=1,...,N. */

/* THE PROGRAM IS INCLUDED IN THE VOLUME */
/*   S. MARTELLO, P. TOTH, "KNAPSACK PROBLEMS: ALGORITHMS */
/*   AND COMPUTER IMPLEMENTATIONS", JOHN WILEY, 1990 */
/* AND IMPLEMENTS THE ENUMERATIVE ALGORITHM DESCRIBED IN */
/* SECTION  3.6.3 . */

/* THE INPUT PROBLEM MUST SATISFY THE CONDITIONS */

/*   1) 2 .LE. N .LE. JDIM - 1 ; */
/*   2) P(J), W(J), C  POSITIVE INTEGERS; */
/*   3) MAX (W(J)) .LE. C . */

/* MTU2   CALLS  5  PROCEDURES: CHMTU2, KSMALL, MTU1, REDU AND SORTR. */
/* KSMALL CALLS  8  PROCEDURES: BLD, BLDF, BLDS1, DETNS1, DETNS2, */
/*                              FORWD, MPSORT AND SORT7. */

/* THE PROGRAM IS COMPLETELY SELF-CONTAINED AND COMMUNICATION TO IT IS */
/* ACHIEVED SOLELY THROUGH THE PARAMETER LIST OF MTU2. */
/* NO MACHINE-DEPENDENT CONSTANT IS USED. */
/* THE PROGRAM IS WRITTEN IN 1967 AMERICAN NATIONAL STANDARD FORTRAN */
/* AND IS ACCEPTED BY THE PFORT VERIFIER (PFORT IS THE PORTABLE */
/* SUBSET OF ANSI DEFINED BY THE ASSOCIATION FOR COMPUTING MACHINERY). */
/* THE PROGRAM HAS BEEN TESTED ON A DIGITAL VAX 11/780 AND AN H.P. */
/* 9000/840. */

/* MTU2 NEEDS  8  ARRAYS ( P ,  W ,  X ,  PO ,  WO ,  XO ,  RR  AND */
/*                        PP ) OF LENGTH AT LEAST  JDIM . */

/* MEANING OF THE INPUT PARAMETERS: */
/* N    = NUMBER OF ITEM TYPES; */
/* P(J) = PROFIT OF EACH ITEM OF TYPE  J  (J=1,...,N); */
/* W(J) = WEIGHT OF EACH ITEM OF TYPE  J  (J=1,...,N); */
/* C    = CAPACITY OF THE KNAPSACK; */
/* JDIM = DIMENSION OF THE 8 ARRAYS; */
/* JFO  = 1 IF OPTIMAL SOLUTION IS REQUIRED, */
/*      = 0 IF APPROXIMATE SOLUTION IS REQUIRED; */
/* JCK  = 1 IF CHECK ON THE INPUT DATA IS DESIRED, */
/*      = 0 OTHERWISE. */

/* MEANING OF THE OUTPUT PARAMETERS: */
/* Z    = VALUE OF THE SOLUTION FOUND IF  Z .GT. 0 , */
/*      = ERROR IN THE INPUT DATA (WHEN JCK=1) IF Z .LT. 0 : CONDI- */
/*        TION  - Z  IS VIOLATED; */
/* X(J) = NUMBER OF ITEMS OF TYPE  J  IN THE SOLUTION FOUND; */
/* JUB  = UPPER BOUND ON THE OPTIMAL SOLUTION VALUE (TO EVALUATE Z */
/*        WHEN JFO=0). */

/* ARRAYS PO, WO, XO, RR AND PP ARE DUMMY. */

/* ALL THE PARAMETERS BUT XO AND RR ARE INTEGER. ON RETURN OF MTU2 */
/* ALL THE INPUT PARAMETERS ARE UNCHANGED. */

    /* Parameter adjustments */
    --pp;
    --rr;
    --xo;
    --wo;
    --po;
    --x;
    --w;
    --p;

    /* Function Body */
    *z__ = 0;
    if (*jck == 1) {
	chmtu2_(n, &p[1], &w[1], c__, z__, jdim);
    }
    if (*z__ < 0) {
	return 0;
    }

/* HEURISTIC SOLUTION THROUGH THE CORE PROBLEM. */

    kc = *n;
    if (*n <= 200) {
	goto L180;
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	rr[j] = (real) p[j] / (real) w[j];
/* L10: */
    }
    kc = *n / 100;
    if (kc < 100) {
	kc = 100;
    }
    if (*jfo == 0) {
	kc = 100;
    }
    kk = *n - kc + 1;
    i__1 = (*n + 5) / 6;
    ksmall_(n, &rr[1], &kk, &i__1, &xo[1]);
    rk = rr[kk];
    if__ = 0;
    il = *n + 1;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	rr[j] = (real) p[j] / (real) w[j];
	if (rr[j] < rk) {
	    goto L30;
	}
	if (rr[j] == rk) {
	    goto L20;
	}
	++if__;
	pp[if__] = j;
	goto L30;
L20:
	--il;
	pp[il] = j;
L30:
	;
    }
    if (if__ == 0) {
	goto L50;
    }
    sortr_(&if__, &rr[1], &pp[1], jdim);
    i__1 = if__;
    for (j = 1; j <= i__1; ++j) {
	i__ = pp[j];
	po[j] = p[i__];
	wo[j] = w[i__];
	w[i__] = -w[i__];
/* L40: */
    }
L50:
    if1 = if__ + 1;
    i__1 = kc;
    for (j = if1; j <= i__1; ++j) {
	i__ = pp[il];
	po[j] = p[i__];
	wo[j] = w[i__];
	w[i__] = -w[i__];
	pp[j] = i__;
	++il;
/* L60: */
    }
    jpk = po[kc];
    jwk = wo[kc];
/* REDUCTION OF THE CORE PROBLEM. */
    redu_(&kc, &po[1], &wo[1], jdim, &jpx, &x[1]);
    k = 0;
    j = jpx;
L70:
    ++k;
    po[k] = po[j];
    wo[k] = wo[j];
    pp[k] = pp[j];
    j = x[j];
    if (j > 0) {
	goto L70;
    }
    if (k > 1) {
	goto L80;
    }
    xo[1] = (real) (*c__ / wo[1]);
    ixo1 = xo[1];
    *z__ = po[1] * ixo1;
    *jub = *z__ + (*c__ - wo[1] * ixo1) * jpk / jwk;
    po[2] = jpk;
    wo[2] = jwk;
    goto L90;
/* SOLUTION OF THE REDUCED CORE PROBLEM. */
L80:
    mtu1_(&k, &po[1], &wo[1], c__, &rk, z__, &xo[1], jdim, jub, &x[1], &rr[1])
	    ;
L90:
    if (*jfo == 0 || *z__ == *jub) {
	goto L140;
    }
    ip1 = po[1];
    ip2 = po[2];
    iw1 = wo[1];
    iw2 = wo[2];
    ip3 = po[3];
    iw3 = wo[3];
    if (k > 2) {
	goto L100;
    }
    ip3 = ip2;
    iw3 = iw2;
L100:
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	x[j] = 0;
	if (w[j] > 0) {
	    goto L110;
	}
	w[j] = -w[j];
	goto L130;
L110:
	icr = *c__ - w[j];
	is1 = icr / iw1;
	ib = p[j] + is1 * ip1 + (icr - is1 * iw1) * ip2 / iw2;
	if (ib <= *z__) {
	    goto L130;
	}
	icrr = icr - is1 * iw1;
	is2 = icrr / iw2;
	ip = p[j] + is1 * ip1 + is2 * ip2;
	icws = icrr - is2 * iw2;
	ib = ip + icws * ip3 / iw3;
	itrunc = (iw2 - icws + iw1 - 1) / iw1;
	lim12 = ip + (icws + itrunc * iw1) * ip2 / iw2 - itrunc * ip1;
	if (lim12 > ib) {
	    ib = lim12;
	}
	if (ib <= *z__) {
	    goto L130;
	}
	i__2 = *n;
	for (jj = j; jj <= i__2; ++jj) {
	    w[jj] = (i__3 = w[jj], abs(i__3));
/* L120: */
	}
	goto L180;
L130:
	;
    }
    goto L160;
L140:
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	x[j] = 0;
	w[j] = (i__2 = w[j], abs(i__2));
/* L150: */
    }
L160:
    i__1 = k;
    for (j = 1; j <= i__1; ++j) {
	i__ = pp[j];
	x[i__] = xo[j];
/* L170: */
    }
    *jub = *z__;
    return 0;

/* SOLUTION THROUGH COMPLETE SORTING. */

L180:
    i__1 = kc;
    for (j = 1; j <= i__1; ++j) {
	rr[j] = (real) p[j] / (real) w[j];
/* L190: */
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	pp[j] = j;
/* L200: */
    }
    sortr_(n, &rr[1], &pp[1], jdim);
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__ = pp[j];
	po[j] = p[i__];
	wo[j] = w[i__];
/* L210: */
    }
/* REDUCTION OF THE PROBLEM. */
    redu_(n, &po[1], &wo[1], jdim, &jpx, &x[1]);
    kf = 0;
    j = jpx;
L220:
    ++kf;
    po[kf] = po[j];
    wo[kf] = wo[j];
    pp[kf] = pp[j];
    j = x[j];
    if (j > 0) {
	goto L220;
    }
    if (kf > 1) {
	goto L230;
    }
    xo[1] = (real) (*c__ / wo[1]);
    ixo1 = xo[1];
    *z__ = po[1] * ixo1;
    *jub = *z__;
    goto L240;
L230:
    mtu1_(&kf, &po[1], &wo[1], c__, &c_b26, z__, &xo[1], jdim, jub, &x[1], &
	    rr[1]);
L240:
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	x[j] = 0;
/* L250: */
    }
    i__1 = kf;
    for (j = 1; j <= i__1; ++j) {
	i__ = pp[j];
	x[i__] = xo[j];
/* L260: */
    }
    return 0;
} /* mtu2_ */

/* Subroutine */ int chmtu2_(integer *n, integer *p, integer *w, integer *c__,
	 integer *z__, integer *jdim)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer j;


/* CHECK THE INPUT DATA. */

    /* Parameter adjustments */
    --w;
    --p;

    /* Function Body */
    if (*n > 1 && *n <= *jdim - 1) {
	goto L10;
    }
    *z__ = -1;
    return 0;
L10:
    if (*c__ > 0) {
	goto L30;
    }
L20:
    *z__ = -2;
    return 0;
L30:
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (p[j] <= 0) {
	    goto L20;
	}
	if (w[j] <= 0) {
	    goto L20;
	}
	if (w[j] > *c__) {
	    goto L50;
	}
/* L40: */
    }
    return 0;
L50:
    *z__ = -3;
    return 0;
} /* chmtu2_ */

/* Subroutine */ int ksmall_(integer *n, real *s, integer *k, integer *n6, 
	real *ss)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, l[6], r__[6], t[6];
    static real v;
    static integer ii, il, ir, nn, it, ns1, nn7, ns2;
    extern /* Subroutine */ int bld_(integer *, real *, integer *, integer *, 
	    integer *, integer *, integer *);
    static integer ns12, lev, nlr, nxt;
    extern /* Subroutine */ int bldf_(integer *, real *, integer *, real *, 
	    integer *, integer *, integer *), blds1_(integer *, real *, 
	    integer *, integer *, integer *, integer *);
    static integer jflag;
    extern /* Subroutine */ int forwd_(integer *, real *, integer *, real *, 
	    integer *, integer *, integer *, integer *, integer *, real *, 
	    integer *), detns1_(integer *, real *, integer *, real *, integer 
	    *, integer *, integer *, integer *, real *, integer *, integer *),
	     detns2_(integer *, real *, integer *, real *, integer *, integer 
	    *, integer *, integer *, real *, integer *, integer *);

/* SUBROUTINE TO FIND THE  K-TH  SMALLEST OF  N  ELEMENTS */
/* IN  O(N)  TIME. */
/* ENTRANCE TO KSMALL IS ACHIEVED BY USING THE STATEMENT */
/*        CALL KSMALL(N,S,K,(N+5)/6,SS) */

/* THE VALUES OF THE FIRST THREE PARAMETERS MUST BE DEFINED */
/* BY THE USER PRIOR TO CALLING KSMALL. KSMALL NEEDS ONE */
/* ARRAY  ( S )  OF LENGTH  N  AND ONE ARRAY ( SS )  OF */
/* LENGTH  (N+5)/6  .THESE ARRAYS MUST BE DIMENSIONED BY THE */
/* USER IN THE CALLING PROGRAM. */

/* KSMALL CALLS EIGHT SUBROUTINES: BLD, BLDF, BLDS1, DETNS1, */
/* DETNS2, FORWD, MPSORT AND SORT7. */
/* THESE SUBROUTINES ARE COMPLETELY LOCAL, I.E. THE INFORMA- */
/* TION THEY NEED IS PASSED THROUGH THE PARAMETER LIST. */
/* THE WHOLE PROGRAM IS COMPLETELY SELF CONTAINED AND COMMU- */
/* NICATION WITH IT IS ACHIEVED SOLELY THROUGH THE PARAMETER */
/* LIST OF KSMALL. NO MACHINE DEPENDENT COSTANTS ARE USED. */
/* THE PROGRAM IS WRITTEN IN AMERICAN NATIONAL STANDARD */
/* FORTRAN AND IS ACCEPTED BY THE PFORT VERIFIER. */
/* THE PROGRAM HAS BEEN TESTED ON A  CDC CYBER 76 , ON A  CDC */
/* CYBER 730  AND ON A  DIGITAL VAX 11/780 . */

/* MEANING OF THE INPUT PARAMETERS: */
/* N = NUMBER OF ELEMENTS. */
/* S = ARRAY CONTAINING THE ELEMENTS. */
/* K = INTEGER VALUE INDICATING THAT THE  K-TH  SMALLEST */
/*     ELEMENT OF  S  MUST BE FOUND ( 1 .LE. K .LE. N ) . */

/* N ,  K  AND  N6  ARE INTEGER,  S  AND  SS  ARE REAL. */
/* ON RETURN, THE ELEMENTS OF  S  ARE REARRANGED SO THAT */
/* S(K)  CONTAINS THE  K-TH  SMALLEST ELEMENT OF  S , WHILE */
/* S(I) .LE. S(K)  IF  I .LT. K ,  S(I) .GE. S(K)  IF */
/* I .GT. K . */
/* THE CURRENT DIMENSIONS OF WORK ARRAYS  L ,  R  AND  T */
/* ALLOW USE OF THE CODE FOR PRACTICALLY ANY VALUE OF  N */
/* ( N .LT. 98*7**5 ). */

/* IN THE FOLLOWING, THE COMMENT SECTIONS REFER TO PROCEDURE */
/* KSMALL DESCRIBED IN  "A HYBRID ALGORITHM FOR FINDING */
/* THE  K-TH  SMALLEST OF  N  ELEMENTS IN  O(N)  TIME" , BY */
/* M. FISCHETTI AND S. MARTELLO, ANNALS OF OPERATIONAL */
/* RESEARCH 13, 1988. */

    /* Parameter adjustments */
    --s;
    --ss;

    /* Function Body */
    l[0] = 1;
    r__[0] = *n;
    t[0] = *k;
    lev = 1;

/* STATEMENTS  1 - 10 . */

L10:
    if (lev > 1) {
	goto L20;
    }
    forwd_(n, &s[1], n6, &ss[1], &lev, l, r__, t, &c__1, &v, &jflag);
    goto L30;
L20:
    i__1 = r__[lev - 1] + 1;
    forwd_(n6, &ss[1], n6, &ss[1], &lev, l, r__, t, &i__1, &v, &jflag);
L30:
    if (jflag == 0) {
	goto L20;
    }
L40:
    --lev;
    if (lev == 0) {
	return 0;
    }
    il = l[lev - 1];
    ir = r__[lev - 1];
    it = t[lev - 1];
    nn = ir - il + 1;
    nn7 = nn / 7;
    ii = il + (nn7 - 1) * 7;
    nxt = 1;
    if (lev > 1) {
	nxt = ir + 1;
    }

/* STATEMENTS  11 - 13 . */

/* COMPUTE  NS1 = CARDINALITY OF SET  A1 . */
    if (lev > 1) {
	goto L50;
    }
    detns1_(n, &s[1], n6, &ss[1], &il, &ir, &ii, &nxt, &v, &ns1, &nlr);
    goto L60;
L50:
    detns1_(n6, &ss[1], n6, &ss[1], &il, &ir, &ii, &nxt, &v, &ns1, &nlr);
L60:
    if (ns1 < it) {
	goto L90;
    }
/* EXPLICITLY DETERMINE SET  A1 . */
    if (lev > 1) {
	goto L70;
    }
    bldf_(n, &s[1], n6, &ss[1], &il, &ir, &nlr);
    goto L80;
L70:
    blds1_(n6, &ss[1], &il, &ii, &nxt, &nlr);
L80:
    r__[lev - 1] = il + ns1 - 1;
    goto L10;
L90:
    if (ns1 < nn * 11 / 70) {
	goto L110;
    }
/* EXPLICITLY DETERMINE SET  A - A1 . */
    t[lev - 1] = it - ns1;
    if (lev > 1) {
	goto L100;
    }
    bldf_(n, &s[1], n6, &ss[1], &il, &ir, &nlr);
    l[lev - 1] = il + ns1;
    goto L10;
L100:
    bld_(n6, &ss[1], &il, &ir, &ii, &nxt, &nlr);
    r__[lev - 1] = il + (nn - ns1) - 1;
    goto L10;

/* STATEMENTS  14 - 16 . */

/* COMPUTE  NS2 = CARDINALITY OF SET  A2 . */
L110:
    if (lev > 1) {
	goto L120;
    }
    detns2_(n, &s[1], n6, &ss[1], &il, &ir, &ii, &nxt, &v, &ns2, &nlr);
    goto L130;
L120:
    detns2_(n6, &ss[1], n6, &ss[1], &il, &ir, &ii, &nxt, &v, &ns2, &nlr);
L130:
    ns12 = ns1 + ns2;
    if (ns12 < it) {
	goto L160;
    }
    if (lev > 1) {
	goto L40;
    }
    bldf_(n, &s[1], n6, &ss[1], &il, &ir, &nlr);
    i__1 = ns12;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s[i__] == v) {
	    goto L150;
	}
/* L140: */
    }
L150:
    s[i__] = s[*k];
    s[*k] = v;
    return 0;
/* EXPLICITLY DETERMINE SET  A - A1 - A2 . */
L160:
    t[lev - 1] = it - ns12;
    if (lev > 1) {
	goto L170;
    }
    bldf_(n, &s[1], n6, &ss[1], &il, &ir, &nlr);
    l[lev - 1] = il + ns12;
    goto L10;
L170:
    bld_(n6, &ss[1], &il, &ir, &ii, &nxt, &nlr);
    r__[lev - 1] = il + (nn - ns12) - 1;
    goto L10;
} /* ksmall_ */

/* Subroutine */ int bld_(integer *n6, real *ss, integer *il, integer *ir, 
	integer *ii, integer *nxt, integer *nlr)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, i6, ij, ik, irr, next;

/* SUBROUTINE TO EXPLICITLY DETERMINE SET  A - A1  OR SET */
/* A - A1 - A2  WHEN LEV .GT. 1 . */
    /* Parameter adjustments */
    --ss;

    /* Function Body */
    j = *il - 1;
    next = *nxt;
    i__1 = *ii;
    for (i__ = *il; i__ <= i__1; i__ += 7) {
	ik = ss[next] + 1.f;
	i6 = i__ + 6;
	if (ik > i6) {
	    goto L20;
	}
	i__2 = i6;
	for (ij = ik; ij <= i__2; ++ij) {
	    ++j;
	    ss[j] = ss[ij];
/* L10: */
	}
L20:
	++next;
/* L30: */
    }
    irr = *ii + 7 + *nlr;
    if (irr > *ir) {
	return 0;
    }
    i__1 = *ir;
    for (ij = irr; ij <= i__1; ++ij) {
	++j;
	ss[j] = ss[ij];
/* L40: */
    }
    return 0;
} /* bld_ */

/* Subroutine */ int bldf_(integer *n, real *s, integer *n6, real *ss, 
	integer *il, integer *ir, integer *nlr)
{
    static real ap;
    static integer nn7, icd, ipd, ics, iud, ips, ius;

/* SUBROUTINE TO EXPLICITLY DETERMINE SET  A1  OR SET  A - A1 */
/* OR SET  A - A1 - A2  WHEN  LEV .EQ. 1 . */
    /* Parameter adjustments */
    --s;
    --ss;

    /* Function Body */
    nn7 = (*ir - *il + 1) / 7;
    ics = 1;
    icd = nn7 + 1;
    ius = *il + 6;
    ips = ss[1] + 1.f;
    iud = *il + nn7 * 7;
    ipd = iud + *nlr - 1;
L10:
    if (ips <= ius) {
	goto L20;
    }
    ++ics;
    if (ics == icd) {
	return 0;
    }
    ius += 7;
    ips = ss[ics] + 1.f;
    goto L10;
L20:
    if (ipd >= iud) {
	goto L30;
    }
    --icd;
    if (icd == ics) {
	return 0;
    }
    iud += -7;
    ipd = ss[icd];
    goto L20;
L30:
    ap = s[ips];
    s[ips] = s[ipd];
    s[ipd] = ap;
    ++ips;
    --ipd;
    goto L10;
} /* bldf_ */

/* Subroutine */ int blds1_(integer *n6, real *ss, integer *il, integer *ii, 
	integer *nxt, integer *nlr)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, ij, ik, irr, next;

/* SUBROUTINE TO EXPLICITLY DETERMINE SET  A1  WHEN */
/* LEV .GT. 1 . */
    /* Parameter adjustments */
    --ss;

    /* Function Body */
    j = *il - 1;
    next = *nxt;
    i__1 = *ii;
    for (i__ = *il; i__ <= i__1; i__ += 7) {
	ik = ss[next];
	if (ik < i__) {
	    goto L20;
	}
	i__2 = ik;
	for (ij = i__; ij <= i__2; ++ij) {
	    ++j;
	    ss[j] = ss[ij];
/* L10: */
	}
L20:
	++next;
/* L30: */
    }
    ik = *ii + 7;
    irr = ik + *nlr - 1;
    if (ik > irr) {
	return 0;
    }
    i__1 = irr;
    for (ij = ik; ij <= i__1; ++ij) {
	++j;
	ss[j] = ss[ij];
/* L40: */
    }
    return 0;
} /* blds1_ */

/* Subroutine */ int detns1_(integer *na, real *a, integer *n6, real *ss, 
	integer *il, integer *ir, integer *ii, integer *nxt, real *v, integer 
	*ns1, integer *nlr)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, irr, next;

/* SUBROUTINE TO COMPUTE THE CARDINALITY OF SET  A1 . */
    /* Parameter adjustments */
    --a;
    --ss;

    /* Function Body */
    *ns1 = 0;
    next = *nxt;
    i__1 = *ii;
    for (i__ = *il; i__ <= i__1; i__ += 7) {
	if (a[i__ + 3] < *v) {
	    goto L40;
	}
	if (a[i__ + 1] < *v) {
	    goto L20;
	}
	if (a[i__] < *v) {
	    goto L10;
	}
	ss[next] = (real) (i__ - 1);
	goto L80;
L10:
	++(*ns1);
	ss[next] = (real) i__;
	goto L80;
L20:
	if (a[i__ + 2] < *v) {
	    goto L30;
	}
	*ns1 += 2;
	ss[next] = (real) (i__ + 1);
	goto L80;
L30:
	*ns1 += 3;
	ss[next] = (real) (i__ + 2);
	goto L80;
L40:
	if (a[i__ + 5] < *v) {
	    goto L60;
	}
	if (a[i__ + 4] < *v) {
	    goto L50;
	}
	*ns1 += 4;
	ss[next] = (real) (i__ + 3);
	goto L80;
L50:
	*ns1 += 5;
	ss[next] = (real) (i__ + 4);
	goto L80;
L60:
	if (a[i__ + 6] < *v) {
	    goto L70;
	}
	*ns1 += 6;
	ss[next] = (real) (i__ + 5);
	goto L80;
L70:
	*ns1 += 7;
	ss[next] = (real) (i__ + 6);
L80:
	++next;
/* L90: */
    }
    *nlr = 0;
    irr = *ii + 7;
    if (irr > *ir) {
	return 0;
    }
    i__1 = *ir;
    for (i__ = irr; i__ <= i__1; ++i__) {
	if (a[i__] >= *v) {
	    goto L110;
	}
	++(*nlr);
/* L100: */
    }
L110:
    *ns1 += *nlr;
    return 0;
} /* detns1_ */

/* Subroutine */ int detns2_(integer *na, real *a, integer *n6, real *ss, 
	integer *il, integer *ir, integer *ii, integer *nxt, real *v, integer 
	*ns2, integer *nlr)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, j, i6, is, ner, irr, next;

/* SUBROUTINE TO COMPUTE THE CARDINALITY OF SET  A2 . */
    /* Parameter adjustments */
    --a;
    --ss;

    /* Function Body */
    *ns2 = 0;
    next = *nxt;
    i__1 = *ii;
    for (i__ = *il; i__ <= i__1; i__ += 7) {
	is = ss[next] + 1.f;
	i6 = i__ + 6;
	if (is <= i6) {
	    goto L10;
	}
	ss[next] = (real) i6;
	goto L40;
L10:
	i__2 = i6;
	for (j = is; j <= i__2; ++j) {
	    if (a[j] > *v) {
		goto L30;
	    }
/* L20: */
	}
	*ns2 = *ns2 + i6 - is + 1;
	ss[next] = (real) i6;
	goto L40;
L30:
	*ns2 = *ns2 + j - is;
	ss[next] = (real) (j - 1);
L40:
	++next;
/* L50: */
    }
    irr = *ii + 7 + *nlr;
    if (irr > *ir) {
	return 0;
    }
    ner = 0;
    i__1 = *ir;
    for (i__ = irr; i__ <= i__1; ++i__) {
	if (a[i__] > *v) {
	    goto L70;
	}
	++ner;
/* L60: */
    }
L70:
    *ns2 += ner;
    *nlr += ner;
    return 0;
} /* detns2_ */

/* Subroutine */ int forwd_(integer *na, real *a, integer *n6, real *ss, 
	integer *lev, integer *l, integer *r__, integer *t, integer *nxt, 
	real *v, integer *jflag)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    static real p;
    static integer i1, i2, n1, n2;
    static real ap;
    static integer ii, il, ir, nn, it, it1, nn7, ilc, irc;
    static real aux;
    static integer itt, imed, next;
    extern /* Subroutine */ int sort7_(integer *, real *, integer *);
    static integer itarg;
    extern /* Subroutine */ int mpsort_(integer *, real *, integer *, integer 
	    *, integer *, real *);

/* SUBROUTINE TO PERFORM STATEMENTS  1 - 9 . */
    /* Parameter adjustments */
    --a;
    --ss;
    --l;
    --r__;
    --t;

    /* Function Body */
    il = l[*lev];
    ir = r__[*lev];
    it = t[*lev];
    nn = ir - il + 1;
    itarg = nn;
/* STATEMENT  1 . */
    if (nn > 97) {
	goto L20;
    }
L10:
    mpsort_(na, &a[1], &il, &ir, &it, v);
    *jflag = 1;
    return 0;
/* STATEMENT  2 . */
L20:
    itarg = itarg * 59 / 70;
    ilc = il;
    imed = (il + ir) / 2;
    p = a[imed];
    if (a[il] <= p) {
	goto L30;
    }
    a[imed] = a[il];
    a[il] = p;
    p = a[imed];
L30:
    irc = ir;
    if (a[ir] >= p) {
	goto L50;
    }
    a[imed] = a[ir];
    a[ir] = p;
    p = a[imed];
    if (a[il] <= p) {
	goto L50;
    }
    a[imed] = a[il];
    a[il] = p;
    p = a[imed];
    goto L50;
L40:
    a[irc] = a[ilc];
    a[ilc] = aux;
L50:
    --irc;
    if (a[irc] > p) {
	goto L50;
    }
    aux = a[irc];
L60:
    ++ilc;
    if (a[ilc] < p) {
	goto L60;
    }
    if (ilc <= irc) {
	goto L40;
    }
/* STATEMENT  3 . */
    if (it > ilc - il) {
	goto L70;
    }
    ir = ilc - 1;
    goto L80;
/* STATEMENT  4 . */
L70:
    it -= ilc - il;
    il = ilc;
L80:
    nn = ir - il + 1;
    if (nn <= 97) {
	goto L10;
    }
/* STATEMENT  5 . */
    if (nn <= itarg) {
	goto L20;
    }
/* STATEMENT  6 . */
    *jflag = 0;
    l[*lev] = il;
    r__[*lev] = ir;
    t[*lev] = it;
    ++(*lev);
    nn7 = nn / 7;
    ii = il + (nn7 - 1) * 7;
    next = *nxt;
    l[*lev] = *nxt;
    i__1 = ii;
    for (i__ = il; i__ <= i__1; i__ += 7) {
	sort7_(na, &a[1], &i__);
	ss[next] = a[i__ + 3];
	++next;
/* L90: */
    }
    i1 = ii + 7;
    i2 = ir - 1;
L100:
    if (i1 > i2) {
	goto L120;
    }
    i__1 = i2;
    for (i__ = i1; i__ <= i__1; ++i__) {
	if (a[i__] <= a[i__ + 1]) {
	    goto L110;
	}
	ap = a[i__];
	a[i__] = a[i__ + 1];
	a[i__ + 1] = ap;
L110:
	;
    }
    --i2;
    goto L100;
L120:
    r__[*lev] = next - 1;
/* STATEMENT  7 . */
    it1 = it;
    n1 = (nn * 11 + 279) / 280;
    it /= 7;
    if (n1 > it) {
	it = n1;
    }
    n2 = nn7 - n1 + 1;
    if (n2 < it) {
	it = n2;
    }
/* STATEMENT  8 . */
    if (it <= (it1 + 3) / 4) {
	goto L130;
    }
    t[*lev] = (it1 + 3) / 4;
    return 0;
/* STATEMENT  9 . */
L130:
    itt = nn7 - (nn - it1 + 4) / 4 + 1;
    if (it < itt) {
	it = itt;
    }
    t[*lev] = it;
    return 0;
} /* forwd_ */

/* Subroutine */ int mpsort_(integer *na, real *a, integer *i1, integer *i2, 
	integer *it, real *v)
{
    static integer i__, j;
    static real ap;
    static integer ij, ita, ill, irr;
    static real aux;

/* SUBROUTINE TO REARRANGE THE ARRAY SEGMENT  A(I1:I2) SO */
/* THAT  A(IT+I1-1)  CONTAINS THE  IT-TH  SMALLEST ELEMENT. */
    /* Parameter adjustments */
    --a;

    /* Function Body */
    i__ = *i1;
    j = *i2;
    ita = *it + *i1 - 1;
L10:
    if (i__ < j) {
	goto L20;
    }
    *v = a[ita];
    return 0;
L20:
    irr = i__;
    ij = (i__ + j) / 2;
    ap = a[ij];
    if (a[i__] <= ap) {
	goto L30;
    }
    a[ij] = a[i__];
    a[i__] = ap;
    ap = a[ij];
L30:
    ill = j;
    if (a[j] >= ap) {
	goto L50;
    }
    a[ij] = a[j];
    a[j] = ap;
    ap = a[ij];
    if (a[i__] <= ap) {
	goto L50;
    }
    a[ij] = a[i__];
    a[i__] = ap;
    ap = a[ij];
    goto L50;
L40:
    a[ill] = a[irr];
    a[irr] = aux;
L50:
    --ill;
    if (a[ill] > ap) {
	goto L50;
    }
    aux = a[ill];
L60:
    ++irr;
    if (a[irr] < ap) {
	goto L60;
    }
    if (irr <= ill) {
	goto L40;
    }
    if (ita < irr) {
	goto L70;
    }
    i__ = irr;
    goto L80;
L70:
    j = ill;
L80:
    if (j - i__ > 10) {
	goto L20;
    }
    if (i__ == *i1) {
	goto L10;
    }
    --i__;
L90:
    ++i__;
    if (i__ != j) {
	goto L100;
    }
    *v = a[ita];
    return 0;
L100:
    ap = a[i__ + 1];
    if (a[i__] <= ap) {
	goto L90;
    }
    irr = i__;
L110:
    a[irr + 1] = a[irr];
    --irr;
    if (ap < a[irr]) {
	goto L110;
    }
    a[irr + 1] = ap;
    goto L90;
} /* mpsort_ */

/* Subroutine */ int sort7_(integer *na, real *a, integer *i__)
{
    static real a1, a2, a3, a4, a5, a6;
    static integer i1, i2, i3, i4, i5, i6;
    static real aux;

/* SUBROUTINE TO SORT IN INCREASING ORDER THE ELEMENTS FROM */
/* A(I)  TO  A(I+6)  OF  A  BY PERFORMING AT MOST  13  TESTS. */
    /* Parameter adjustments */
    --a;

    /* Function Body */
    i1 = *i__ + 1;
    i2 = *i__ + 2;
    i3 = *i__ + 3;
    i4 = *i__ + 4;
    i5 = *i__ + 5;
    i6 = *i__ + 6;
    if (a[*i__] > a[i1]) {
	goto L10;
    }
    a1 = a[*i__];
    a2 = a[i1];
    goto L20;
L10:
    a1 = a[i1];
    a2 = a[*i__];
L20:
    if (a[i2] > a[i3]) {
	goto L30;
    }
    a3 = a[i2];
    a4 = a[i3];
    goto L40;
L30:
    a3 = a[i3];
    a4 = a[i2];
L40:
    if (a1 > a3) {
	goto L50;
    }
    a5 = a2;
    a2 = a3;
    a3 = a4;
    goto L60;
L50:
    a5 = a4;
    aux = a3;
    a3 = a2;
    a2 = a1;
    a1 = aux;
L60:
    a4 = a[i4];
    if (a4 >= a2) {
	goto L80;
    }
    if (a4 >= a1) {
	goto L70;
    }
    a4 = a3;
    a3 = a2;
    a2 = a1;
    a1 = a[i4];
    goto L90;
L70:
    a4 = a3;
    a3 = a2;
    a2 = a[i4];
    goto L90;
L80:
    if (a4 >= a3) {
	goto L90;
    }
    a4 = a3;
    a3 = a[i4];
L90:
    a[*i__] = a1;
    if (a5 > a3) {
	goto L110;
    }
    a[i3] = a3;
    a[i4] = a4;
    if (a5 > a2) {
	goto L100;
    }
    a[i1] = a5;
    a[i2] = a2;
    goto L130;
L100:
    a[i1] = a2;
    a[i2] = a5;
    goto L130;
L110:
    a[i1] = a2;
    a[i2] = a3;
    if (a5 > a4) {
	goto L120;
    }
    a[i3] = a5;
    a[i4] = a4;
    goto L130;
L120:
    a[i3] = a4;
    a[i4] = a5;
L130:
    a5 = a[i5];
    if (a5 < a[i2]) {
	goto L150;
    }
    if (a5 < a[i3]) {
	goto L140;
    }
    if (a5 >= a[i4]) {
	goto L180;
    }
    a[i5] = a[i4];
    a[i4] = a5;
    goto L180;
L140:
    a[i5] = a[i4];
    a[i4] = a[i3];
    a[i3] = a5;
    goto L180;
L150:
    a[i5] = a[i4];
    a[i4] = a[i3];
    a[i3] = a[i2];
    if (a5 < a[i1]) {
	goto L160;
    }
    a[i2] = a5;
    goto L180;
L160:
    a[i2] = a[i1];
    if (a5 < a[*i__]) {
	goto L170;
    }
    a[i1] = a5;
    goto L180;
L170:
    a[i1] = a[*i__];
    a[*i__] = a5;
L180:
    a6 = a[i6];
    if (a6 < a[i3]) {
	goto L200;
    }
    if (a6 < a[i4]) {
	goto L190;
    }
    if (a6 >= a[i5]) {
	return 0;
    }
    a[i6] = a[i5];
    a[i5] = a6;
    return 0;
L190:
    a[i6] = a[i5];
    a[i5] = a[i4];
    a[i4] = a6;
    return 0;
L200:
    a[i6] = a[i5];
    a[i5] = a[i4];
    a[i4] = a[i3];
    if (a6 < a[i1]) {
	goto L220;
    }
    if (a6 < a[i2]) {
	goto L210;
    }
    a[i3] = a6;
    return 0;
L210:
    a[i3] = a[i2];
    a[i2] = a6;
    return 0;
L220:
    a[i3] = a[i2];
    a[i2] = a[i1];
    if (a6 < a[*i__]) {
	goto L230;
    }
    a[i1] = a6;
    return 0;
L230:
    a[i1] = a[*i__];
    a[*i__] = a6;
    return 0;
} /* sort7_ */

/* Subroutine */ int mtu1_(integer *n, integer *p, integer *w, integer *c__, 
	real *rn, integer *z__, real *x, integer *jdim, integer *jub, integer 
	*xx, real *min__)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer j, k, r__, s, t, s1, s2, ib, ii, kk, ip, nn, ps, ii1, kk1, 
	    cwf, lim, cws, diff, lim12, mink, profit, itrunc;


/* THIS SUBROUTINE SOLVES THE UNBOUNDED SINGLE KNAPSACK PROBLEM */

/* MAXIMIZE  Z = P(1)*X(1) + ... + P(N)*X(N) */

/* SUBJECT TO:   W(1)*X(1) + ... + W(N)*X(N) .LE. C , */
/*               X(J) .GE. 0 AND INTEGER  FOR J=1,...,N. */

/* THE PROGRAM IS BASED ON THE BRANCH-AND-BOUND ALGORITHM PRESENTED IN */
/*  S. MARTELLO, P. TOTH, "BRANCH AND BOUND ALGORITHMS FOR THE SOLUTION */
/*  OF THE GENERAL UNIDIMENSIONAL KNAPSACK PROBLEM", IN M. ROUBENS, ED., */
/*  "ADVANCES IN OPERATIONS RESEARCH", NORTH HOLLAND, 1977. */


/* STEP 1. */

    /* Parameter adjustments */
    --min__;
    --xx;
    --x;
    --w;
    --p;

    /* Function Body */
    cwf = *c__;
    s1 = *c__ / w[1];
    s2 = (*c__ - s1 * w[1]) / w[2];
    ip = s1 * p[1] + s2 * p[2];
    cws = *c__ - s1 * w[1] - s2 * w[2];
    if (cws != 0) {
	goto L20;
    }
    *z__ = ip;
    *jub = *z__;
    x[1] = (real) s1;
    x[2] = (real) s2;
    if (*n == 2) {
	return 0;
    }
    i__1 = *n;
    for (j = 3; j <= i__1; ++j) {
	x[j] = 0.f;
/* L10: */
    }
    return 0;
L20:
    mink = *c__ + 1;
    min__[*n] = (real) mink;
    i__1 = *n;
    for (j = 2; j <= i__1; ++j) {
	k = *n + 2 - j;
	if (w[k] < mink) {
	    mink = w[k];
	}
	min__[k - 1] = (real) mink;
/* L30: */
    }
    w[*n + 1] = *c__ + 1;
    p[*n + 1] = 0;
    lim = ip + cws * p[3] / w[3];
    if (*n == 2) {
	lim = (real) ip + (real) cws * *rn;
    }
    itrunc = (w[2] - cws + w[1] - 1) / w[1];
    lim12 = ip + (cws + itrunc * w[1]) * p[2] / w[2] - itrunc * p[1];
    if (lim12 > lim) {
	lim = lim12;
    }
    *jub = lim;
    *z__ = 0;
    xx[1] = s1;
    xx[2] = s2;
    if (*n == 2) {
	goto L50;
    }
    i__1 = *n;
    for (j = 3; j <= i__1; ++j) {
	xx[j] = 0;
/* L40: */
    }
L50:
    profit = ip;
    *c__ = cws;
    ii = 2;
    goto L110;

/* STEP 2. */

L60:
    s = *c__ / w[ii];
    if (s > 0) {
	goto L70;
    }
    if (*z__ >= profit + *c__ * p[ii + 1] / w[ii + 1]) {
	goto L120;
    }
    ++ii;
    goto L60;

/* STEP 3. */

L70:
    ps = profit + s * p[ii];
    cws = *c__ - s * w[ii];
    if (cws == 0 || ii == *n) {
	goto L80;
    }
    if (*z__ >= ps + cws * p[ii + 1] / w[ii + 1]) {
	goto L150;
    }
    *c__ = cws;
    profit = ps;
    xx[ii] = s;
    goto L110;
L80:
    if (*z__ >= ps) {
	goto L150;
    }
    *z__ = ps;
    ii1 = ii - 1;
    i__1 = ii1;
    for (j = 1; j <= i__1; ++j) {
	x[j] = (real) xx[j];
/* L90: */
    }
    x[ii] = (real) s;
    ii1 = ii + 1;
    i__1 = *n;
    for (j = ii1; j <= i__1; ++j) {
	x[j] = 0.f;
/* L100: */
    }
    if (*z__ != lim) {
	goto L150;
    }
    *c__ = cwf;
    return 0;

/* STEP 4. */

L110:
    ++ii;
    if (*c__ >= (integer) min__[ii - 1]) {
	goto L60;
    }

/* STEP 5. */

L120:
    if (*z__ >= profit) {
	goto L140;
    }
    *z__ = profit;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	x[j] = (real) xx[j];
/* L130: */
    }
    if (*z__ != lim) {
	goto L140;
    }
    *c__ = cwf;
    return 0;
L140:
    if (xx[*n] == 0) {
	goto L150;
    }
    *c__ += xx[*n] * w[*n];
    profit -= xx[*n] * p[*n];
    xx[*n] = 0;

/* STEP 6. */

L150:
    ib = ii - 1;
    i__1 = ib;
    for (j = 1; j <= i__1; ++j) {
	kk = ii - j;
	if (xx[kk] > 0) {
	    goto L170;
	}
/* L160: */
    }
    *c__ = cwf;
    return 0;
L170:
    r__ = *c__;
    *c__ += w[kk];
    profit -= p[kk];
    --xx[kk];
    if (*z__ < profit + *c__ * p[kk + 1] / w[kk + 1]) {
	goto L180;
    }
    *c__ += xx[kk] * w[kk];
    profit -= xx[kk] * p[kk];
    xx[kk] = 0;
    ii = kk + 1;
    goto L150;
L180:
    if (r__ < (integer) min__[kk]) {
	goto L190;
    }
    ii = kk + 1;
    goto L60;
L190:
    nn = kk + 1;
    ii = kk + 1;

/* STEP 7. */

L200:
    diff = w[nn] - w[kk];
    if (diff < 0) {
	goto L210;
    } else if (diff == 0) {
	goto L260;
    } else {
	goto L220;
    }
L210:
    t = r__ - diff;
    if (t < (integer) min__[nn - 1]) {
	goto L260;
    }
    s = *c__ / w[nn];
    ii = nn;
    goto L70;
L220:
    if (diff > r__) {
	goto L260;
    }
    if (*z__ >= profit + p[nn]) {
	goto L260;
    }
    *z__ = profit + p[nn];
    i__1 = kk;
    for (j = 1; j <= i__1; ++j) {
	x[j] = (real) xx[j];
/* L230: */
    }
    kk1 = kk + 1;
    i__1 = *n;
    for (j = kk1; j <= i__1; ++j) {
	x[j] = 0.f;
/* L240: */
    }
    x[nn] = 1.f;
    if (*z__ != lim) {
	goto L250;
    }
    *c__ = cwf;
    return 0;
L250:
    r__ -= diff;
    kk = nn;

/* STEP 8. */

L260:
    if (*z__ >= profit + *c__ * p[nn + 1] / w[nn + 1]) {
	goto L150;
    }
    ++nn;
    goto L200;
} /* mtu1_ */

/* Subroutine */ int redu_(integer *n, integer *po, integer *wo, integer *
	jdim, integer *jpx, integer *x)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer j, k;
    static real rj;
    static integer feq, prj, prk;
    static real crat;
    static integer ipoj, iwoj, prfeq;


/* REDUCE AN UNBOUNDED KNAPSACK PROBLEM (PO,WO) THROUGH DOMINANCE */
/* RELATIONS. */
/* ON OUTPUT, JPX IS THE FIRST UNDOMINATED ITEM, X(JPX) THE SECOND, AND */
/* SO ON. IF Y IS THE LAST ONE, THEN X(Y) = 0. */

/* INITIALIZATION. */
    /* Parameter adjustments */
    --x;
    --wo;
    --po;

    /* Function Body */
    *jpx = 1;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	x[j] = j + 1;
/* L10: */
    }
    x[*n] = 0;
    crat = (real) po[1] / (real) wo[1] + 1.f;
    prfeq = 0;
    prj = 0;
/* MAIN ITERATION. */
    j = *jpx;
L20:
    iwoj = wo[j];
    ipoj = po[j];
    rj = (real) ipoj / (real) iwoj;
    if (rj == crat) {
	goto L30;
    }
    crat = rj;
    prfeq = prj;
    feq = j;
    goto L80;
/* ITEMS K PRECEDING J WITH SAME RATIO. */
L30:
    k = feq;
    prk = prfeq;
L40:
    if (wo[k] / iwoj * ipoj < po[k]) {
	goto L60;
    }
/* ITEM J DOMINATES ITEM K. */
    if (prk == 0) {
	goto L50;
    }
    x[prk] = x[k];
    goto L70;
L50:
    *jpx = x[k];
    goto L70;
L60:
    prk = k;
L70:
    k = x[k];
    if (k < j) {
	goto L40;
    }
/* ITEMS K FOLLOWING J. */
L80:
    k = x[j];
    prk = j;
L90:
    if (k == 0) {
	goto L120;
    }
    if (wo[k] / iwoj * ipoj < po[k]) {
	goto L100;
    }
/* ITEM J DOMINATES ITEM K. */
    x[prk] = x[k];
    goto L110;
L100:
    prk = k;
L110:
    k = x[k];
    goto L90;
L120:
    prj = j;
    j = x[j];
    if (j > 0) {
	goto L20;
    }
    return 0;
} /* redu_ */

/* Subroutine */ int sortr_(integer *n, real *a, integer *v, integer *jda)
{
    static integer i__, j, k, l, m;
    static real t;
    static integer ii, ij, jj, il[20], ki, iu[20], iv, ivt;


/* SORT THE REAL ARRAY A BY DECREASING VALUES (DERIVED FROM SUBROUTINE */
/* SORTZV OF THE C.E.R.N. LIBRARY). */

/* JDA           = LENGTH OF ARRAY A; */
/* N             = NUMBER OF ELEMENTS OF A TO BE SORTED; */
/* V(I) (INPUT)  = POINTER TO THE I-TH ELEMENT TO BE SORTED; */
/* V(I) (OUTPUT) = POINTER TO THE I-TH ELEMENT OF THE SORTED ARRAY. */

/* ON RETURN, ARRAY A IS UNCHANGED. */

    /* Parameter adjustments */
    --v;
    --a;

    /* Function Body */
    ii = 1;
    jj = *n;
    if (*n <= 1) {
	return 0;
    }
    m = 1;
    i__ = ii;
    j = jj;
L10:
    if (i__ >= j) {
	goto L80;
    }
L20:
    k = i__;
    ij = (j + i__) / 2;
    iv = v[ij];
    t = a[iv];
    ki = v[i__];
    if (a[ki] >= t) {
	goto L30;
    }
    v[ij] = ki;
    v[i__] = iv;
    iv = v[ij];
    t = a[iv];
L30:
    l = j;
    ki = v[j];
    if (a[ki] <= t) {
	goto L50;
    }
    v[ij] = ki;
    v[j] = iv;
    iv = v[ij];
    t = a[iv];
    ki = v[i__];
    if (a[ki] >= t) {
	goto L50;
    }
    v[ij] = ki;
    v[i__] = iv;
    iv = v[ij];
    t = a[iv];
    goto L50;
L40:
    v[l] = v[k];
    v[k] = ivt;
L50:
    --l;
    ki = v[l];
    if (a[ki] < t) {
	goto L50;
    }
    ivt = ki;
L60:
    ++k;
    ki = v[k];
    if (a[ki] > t) {
	goto L60;
    }
    if (k <= l) {
	goto L40;
    }
    if (l - i__ <= j - k) {
	goto L70;
    }
    il[m - 1] = i__;
    iu[m - 1] = l;
    i__ = k;
    ++m;
    goto L90;
L70:
    il[m - 1] = k;
    iu[m - 1] = j;
    j = l;
    ++m;
    goto L90;
L80:
    --m;
    if (m == 0) {
	return 0;
    }
    i__ = il[m - 1];
    j = iu[m - 1];
L90:
    if (j - i__ >= ii) {
	goto L20;
    }
    if (i__ == ii) {
	goto L10;
    }
    --i__;
L100:
    ++i__;
    if (i__ == j) {
	goto L80;
    }
    iv = v[i__ + 1];
    t = a[iv];
    ki = v[i__];
    if (a[ki] >= t) {
	goto L100;
    }
    k = i__;
L110:
    v[k + 1] = v[k];
    --k;
    ki = v[k];
    if (t > a[ki]) {
	goto L110;
    }
    v[k + 1] = iv;
    goto L100;
} /* sortr_ */

