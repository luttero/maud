package it.unitn.ing.rista.diffr.rsa.epsc;

import java.util.Arrays;

/**
 * Contains all memory allocation and resizing routines from the Fortran code.
 *
 * These methods directly access and modify the public static array fields
 * in the CommonGlobals and CommonPlastic classes.
 */
public class AllocationRoutines {

    /**
     * Allocates arrays based on NMOD.
     * Note: Java initializes new numeric arrays to 0/0.0 by default.
     */
    public static void allocateForNMOD() {
        CommonGlobals.nsm = new int[CommonGlobals.NMOD + 1];
        CommonGlobals.itw = new int[CommonGlobals.NMOD + 1];
        CommonGlobals.stw = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.vfrac_mod_acum = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.vfrac_mod = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.TwinFrac = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.TwinCRSS = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau0_mode_c = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.BURG = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.ACTENER = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.aK1 = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.DRAG = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.rho_ini_for = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.rho_ini_deb = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau0_mode_a = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau0_mode_b = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.a_deb_a = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.a_deb_b = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.a_deb_c = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.TLATENT = new double[CommonGlobals.NMOD + 1][CommonGlobals.NMOD + 1];
        CommonGlobals.HPK0 = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.HPK1 = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.HPK2 = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.edot_zero = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau_prop_a = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau_crit_a = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau_crit_b = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau_crit_c = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau_prop_b = new double[CommonGlobals.NMOD + 1];
        
        // Special case for source=1.0d0
        CommonGlobals.tau_prop_c = new double[CommonGlobals.NMOD + 1];
        Arrays.fill(CommonGlobals.tau_prop_c, 1.0);
        
        CommonGlobals.shearmod = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.burg_tw = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.tau0_mode = new double[CommonGlobals.NMOD + 1];
        CommonGlobals.a_deb = new double[CommonGlobals.NMOD + 1];
    }

    /**
     * Resizes arrays based on NMOD.
     * Must assign the returned array back to the static field.
     */
    public static void resizeForNMOD() {
        CommonGlobals.nsm = Resize.resize_int1(CommonGlobals.nsm, CommonGlobals.NMOD + 1);
        CommonGlobals.itw = Resize.resize_int1(CommonGlobals.itw, CommonGlobals.NMOD + 1);
        CommonGlobals.stw = Resize.resize_double1(CommonGlobals.stw, CommonGlobals.NMOD + 1);
        CommonGlobals.vfrac_mod_acum = Resize.resize_double1(CommonGlobals.vfrac_mod_acum, CommonGlobals.NMOD + 1);
        CommonGlobals.vfrac_mod = Resize.resize_double1(CommonGlobals.vfrac_mod, CommonGlobals.NMOD + 1);
        CommonGlobals.TwinFrac = Resize.resize_double1(CommonGlobals.TwinFrac, CommonGlobals.NMOD + 1);
        CommonGlobals.TwinCRSS = Resize.resize_double1(CommonGlobals.TwinCRSS, CommonGlobals.NMOD + 1);
        CommonGlobals.tau0_mode_c = Resize.resize_double1(CommonGlobals.tau0_mode_c, CommonGlobals.NMOD + 1);
        CommonGlobals.BURG = Resize.resize_double1(CommonGlobals.BURG, CommonGlobals.NMOD + 1);
        CommonGlobals.ACTENER = Resize.resize_double1(CommonGlobals.ACTENER, CommonGlobals.NMOD + 1);
        CommonGlobals.aK1 = Resize.resize_double1(CommonGlobals.aK1, CommonGlobals.NMOD + 1);
        CommonGlobals.DRAG = Resize.resize_double1(CommonGlobals.DRAG, CommonGlobals.NMOD + 1);
        CommonGlobals.rho_ini_for = Resize.resize_double1(CommonGlobals.rho_ini_for, CommonGlobals.NMOD + 1);
        CommonGlobals.rho_ini_deb = Resize.resize_double1(CommonGlobals.rho_ini_deb, CommonGlobals.NMOD + 1);
        CommonGlobals.tau0_mode_a = Resize.resize_double1(CommonGlobals.tau0_mode_a, CommonGlobals.NMOD + 1);
        CommonGlobals.tau0_mode_b = Resize.resize_double1(CommonGlobals.tau0_mode_b, CommonGlobals.NMOD + 1);
        CommonGlobals.a_deb_a = Resize.resize_double1(CommonGlobals.a_deb_a, CommonGlobals.NMOD + 1);
        CommonGlobals.a_deb_b = Resize.resize_double1(CommonGlobals.a_deb_b, CommonGlobals.NMOD + 1);
        CommonGlobals.a_deb_c = Resize.resize_double1(CommonGlobals.a_deb_c, CommonGlobals.NMOD + 1);
        CommonGlobals.TLATENT = Resize.resize_double2(CommonGlobals.TLATENT, CommonGlobals.NMOD + 1, CommonGlobals.NMOD + 1);
        CommonGlobals.HPK0 = Resize.resize_double1(CommonGlobals.HPK0, CommonGlobals.NMOD + 1);
        CommonGlobals.HPK1 = Resize.resize_double1(CommonGlobals.HPK1, CommonGlobals.NMOD + 1);
        CommonGlobals.HPK2 = Resize.resize_double1(CommonGlobals.HPK2, CommonGlobals.NMOD + 1);
        CommonGlobals.edot_zero = Resize.resize_double1(CommonGlobals.edot_zero, CommonGlobals.NMOD + 1);
        CommonGlobals.tau_prop_a = Resize.resize_double1(CommonGlobals.tau_prop_a, CommonGlobals.NMOD + 1);
        CommonGlobals.tau_crit_a = Resize.resize_double1(CommonGlobals.tau_crit_a, CommonGlobals.NMOD + 1);
        CommonGlobals.tau_crit_b = Resize.resize_double1(CommonGlobals.tau_crit_b, CommonGlobals.NMOD + 1);
        CommonGlobals.tau_crit_c = Resize.resize_double1(CommonGlobals.tau_crit_c, CommonGlobals.NMOD + 1);
        CommonGlobals.tau_prop_b = Resize.resize_double1(CommonGlobals.tau_prop_b, CommonGlobals.NMOD + 1);
        CommonGlobals.tau_prop_c = Resize.resize_double1(CommonGlobals.tau_prop_c, CommonGlobals.NMOD + 1);
        CommonGlobals.shearmod = Resize.resize_double1(CommonGlobals.shearmod, CommonGlobals.NMOD + 1);
        CommonGlobals.burg_tw = Resize.resize_double1(CommonGlobals.burg_tw, CommonGlobals.NMOD + 1);
        CommonGlobals.tau0_mode = Resize.resize_double1(CommonGlobals.tau0_mode, CommonGlobals.NMOD + 1);
        CommonGlobals.a_deb = Resize.resize_double1(CommonGlobals.a_deb, CommonGlobals.NMOD + 1);
    }

    /**
     * Allocates arrays based on NSLS.
     */
    public static void allocateForNSLS() {
        CommonGlobals.iTwinSys = new int[CommonGlobals.NSLS + 1];
        CommonGlobals.iSysMode = new int[CommonGlobals.NSLS + 1];
        CommonGlobals.tau0 = new double[CommonGlobals.NSLS + 1];
        CommonGlobals.tau1 = new double[CommonGlobals.NSLS + 1];
        CommonGlobals.thet0 = new double[CommonGlobals.NSLS + 1];
        CommonGlobals.ncc = new double[3 + 1][CommonGlobals.NSLS + 1];
        CommonGlobals.thet1 = new double[CommonGlobals.NSLS + 1];
        CommonGlobals.qc2 = new double[3 + 1][3 + 1][CommonGlobals.NSLS + 1];
        CommonGlobals.mc2 = new double[3 + 1][3 + 1][CommonGlobals.NSLS + 1];
        CommonGlobals.h = new double[CommonGlobals.NSLS + 1][CommonGlobals.NSLS + 1];
        CommonGlobals.bcc = new double[3 + 1][CommonGlobals.NSLS + 1];
        CommonGlobals.hd = new double[CommonGlobals.NSLS + 1][CommonGlobals.NSLS + 1];
        CommonGlobals.dnsa = new double[3 + 1][CommonGlobals.NSLS + 1];
        CommonGlobals.dbsa = new double[3 + 1][CommonGlobals.NSLS + 1];

        allocateForNMOD_NSLS();
    }

    /**
     * Resizes arrays based on NSLS.
     */
    public static void resizeForNSLS() {
        CommonGlobals.iTwinSys = Resize.resize_int1(CommonGlobals.iTwinSys, CommonGlobals.NSLS + 1);
        CommonGlobals.iSysMode = Resize.resize_int1(CommonGlobals.iSysMode, CommonGlobals.NSLS + 1);
        CommonGlobals.tau0 = Resize.resize_double1(CommonGlobals.tau0, CommonGlobals.NSLS + 1);
        CommonGlobals.tau1 = Resize.resize_double1(CommonGlobals.tau1, CommonGlobals.NSLS + 1);
        CommonGlobals.thet0 = Resize.resize_double1(CommonGlobals.thet0, CommonGlobals.NSLS + 1);
        CommonGlobals.ncc = Resize.resize_double2(CommonGlobals.ncc, 3 + 1, CommonGlobals.NSLS + 1);
        CommonGlobals.thet1 = Resize.resize_double1(CommonGlobals.thet1, CommonGlobals.NSLS + 1);
        CommonGlobals.qc2 = Resize.resize_double3(CommonGlobals.qc2, 3 + 1, 3 + 1, CommonGlobals.NSLS + 1);
        CommonGlobals.mc2 = Resize.resize_double3(CommonGlobals.mc2, 3 + 1, 3 + 1, CommonGlobals.NSLS + 1);
        CommonGlobals.h = Resize.resize_double2(CommonGlobals.h, CommonGlobals.NSLS + 1, CommonGlobals.NSLS + 1);
        CommonGlobals.bcc = Resize.resize_double2(CommonGlobals.bcc, 3 + 1, CommonGlobals.NSLS + 1);
        CommonGlobals.hd = Resize.resize_double2(CommonGlobals.hd, CommonGlobals.NSLS + 1, CommonGlobals.NSLS + 1);
        CommonGlobals.dnsa = Resize.resize_double2(CommonGlobals.dnsa, 3 + 1, CommonGlobals.NSLS + 1);
        CommonGlobals.dbsa = Resize.resize_double2(CommonGlobals.dbsa, 3 + 1, CommonGlobals.NSLS + 1);

        resizeForNMOD_NSLS();
    }

    /**
     * Allocates arrays based on NGR.
     */
    public static void allocateForNGR() {
        CommonGlobals.nact = new int[CommonGlobals.NGR + 1];
        CommonGlobals.iTwinLevel = new int[CommonGlobals.NGR + 1];
        CommonGlobals.ng_update = new int[CommonGlobals.NGR + 1];
        CommonGlobals.nactSlip = new int[CommonGlobals.NGR + 1];
        CommonGlobals.iParentGrain = new int[CommonGlobals.NGR + 1];
        CommonGlobals.iParentSystem = new int[CommonGlobals.NGR + 1];
        CommonGlobals.iParentMode = new int[CommonGlobals.NGR + 1];

        CommonGlobals.ccs2 = new double[6 + 1][6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.scs2 = new double[6 + 1][6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.alfacs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.etelcs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.etelhycs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.gamtot = new double[CommonGlobals.NGR + 1];
        CommonGlobals.acs2 = new double[6 + 1][6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.wgtd = new double[CommonGlobals.NGR + 1];
        CommonGlobals.fijgr = new double[3 + 1][3 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.meffc = new double[CommonGlobals.NGR + 1];
        CommonGlobals.wgtx = new double[CommonGlobals.NGR + 1];
        CommonGlobals.link = new double[CommonGlobals.NGR + 1];
        CommonGlobals.phi = new double[CommonGlobals.NGR + 1];
        CommonGlobals.the = new double[CommonGlobals.NGR + 1];
        CommonGlobals.ome = new double[CommonGlobals.NGR + 1];
        CommonGlobals.wgt = new double[CommonGlobals.NGR + 1];
        CommonGlobals.r = new double[3 + 1][3 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.etthcs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.omegag = new double[3 + 1][3 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.EINVSAGR = new double[3 + 1][3 + 1][3 + 1][3 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.ESCR4GR = new double[3 + 1][3 + 1][3 + 1][3 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.aefgr = new double[6 + 1][6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.aloca = new double[6 + 1][6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.as = new double[3 + 1][3 + 1][3 + 1][3 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.axisgr = new double[3 + 1][3 + 1][CommonGlobals.NGR + 1]; // Fortran (0:3 + 1, 3 + 1, NGR)
        CommonGlobals.stcs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.strcs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.stcsref = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.etcs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.etrcs = new double[6 + 1][CommonGlobals.NGR + 1];
        CommonGlobals.rho_deb = new double[CommonGlobals.NGR + 1];

        allocateForNSLS_NGR();
    }

    /**
     * Resizes arrays based on NGR.
     */
    public static void resizeForNGR() {
        CommonGlobals.nact = Resize.resize_int1(CommonGlobals.nact, CommonGlobals.NGR + 1);
        CommonGlobals.iTwinLevel = Resize.resize_int1(CommonGlobals.iTwinLevel, CommonGlobals.NGR + 1);
        CommonGlobals.ng_update = Resize.resize_int1(CommonGlobals.ng_update, CommonGlobals.NGR + 1);
        CommonGlobals.nactSlip = Resize.resize_int1(CommonGlobals.nactSlip, CommonGlobals.NGR + 1);
        CommonGlobals.iParentGrain = Resize.resize_int1(CommonGlobals.iParentGrain, CommonGlobals.NGR + 1);
        CommonGlobals.iParentSystem = Resize.resize_int1(CommonGlobals.iParentSystem, CommonGlobals.NGR + 1);
        CommonGlobals.iParentMode = Resize.resize_int1(CommonGlobals.iParentMode, CommonGlobals.NGR + 1);

        CommonGlobals.ccs2 = Resize.resize_double3(CommonGlobals.ccs2, 6 + 1, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.scs2 = Resize.resize_double3(CommonGlobals.scs2, 6 + 1, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.alfacs = Resize.resize_double2(CommonGlobals.alfacs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.etelcs = Resize.resize_double2(CommonGlobals.etelcs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.etelhycs = Resize.resize_double2(CommonGlobals.etelhycs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.gamtot = Resize.resize_double1(CommonGlobals.gamtot, CommonGlobals.NGR + 1);
        CommonGlobals.acs2 = Resize.resize_double3(CommonGlobals.acs2, 6 + 1, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.wgtd = Resize.resize_double1(CommonGlobals.wgtd, CommonGlobals.NGR + 1);
        CommonGlobals.fijgr = Resize.resize_double3(CommonGlobals.fijgr, 3 + 1, 3 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.meffc = Resize.resize_double1(CommonGlobals.meffc, CommonGlobals.NGR + 1);
        CommonGlobals.wgtx = Resize.resize_double1(CommonGlobals.wgtx, CommonGlobals.NGR + 1);
        CommonGlobals.link = Resize.resize_double1(CommonGlobals.link, CommonGlobals.NGR + 1);
        CommonGlobals.phi = Resize.resize_double1(CommonGlobals.phi, CommonGlobals.NGR + 1);
        CommonGlobals.the = Resize.resize_double1(CommonGlobals.the, CommonGlobals.NGR + 1);
        CommonGlobals.ome = Resize.resize_double1(CommonGlobals.ome, CommonGlobals.NGR + 1);
        CommonGlobals.wgt = Resize.resize_double1(CommonGlobals.wgt, CommonGlobals.NGR + 1);
        CommonGlobals.r = Resize.resize_double3(CommonGlobals.r, 3 + 1, 3 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.etthcs = Resize.resize_double2(CommonGlobals.etthcs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.omegag = Resize.resize_double3(CommonGlobals.omegag, 3 + 1, 3 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.EINVSAGR = Resize.resize_double5(CommonGlobals.EINVSAGR, 3 + 1, 3 + 1, 3 + 1, 3 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.ESCR4GR = Resize.resize_double5(CommonGlobals.ESCR4GR, 3 + 1, 3 + 1, 3 + 1, 3 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.aefgr = Resize.resize_double3(CommonGlobals.aefgr, 6 + 1, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.aloca = Resize.resize_double3(CommonGlobals.aloca, 6 + 1, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.as = Resize.resize_double5(CommonGlobals.as, 3 + 1, 3 + 1, 3 + 1, 3 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.axisgr = Resize.resize_double0(CommonGlobals.axisgr, 0, 3 + 1, 1, 3 + 1, 1, CommonGlobals.NGR + 1);
        CommonGlobals.stcs = Resize.resize_double2(CommonGlobals.stcs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.strcs = Resize.resize_double2(CommonGlobals.strcs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.stcsref = Resize.resize_double2(CommonGlobals.stcsref, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.etcs = Resize.resize_double2(CommonGlobals.etcs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.etrcs = Resize.resize_double2(CommonGlobals.etrcs, 6 + 1, CommonGlobals.NGR + 1);
        CommonGlobals.rho_deb = Resize.resize_double1(CommonGlobals.rho_deb, CommonGlobals.NGR + 1);

        resizeForNSLS_NGR();
    }

    /**
     * Allocates arrays based on NDIFFX.
     */
    public static void allocateForNDIFFX() {
        CommonGlobals.ngrset = new int[CommonGlobals.NDIFFX + 1];
        CommonGlobals.RAND_WGT = new double[CommonGlobals.NDIFFX + 1];
        CommonGlobals.wgtset = new double[CommonGlobals.NDIFFX + 1];
        CommonGlobals.wgtsetini = new double[CommonGlobals.NDIFFX + 1];
        CommonGlobals.chiPoleFig = new double[CommonGlobals.NDIFFX + 1];
        CommonGlobals.etaPoleFig = new double[CommonGlobals.NDIFFX + 1];

        allocateForNDIFFX_NGR();
    }

    /**
     * Resizes arrays based on NDIFFX.
     */
    public static void resizeForNDIFFX() {
        CommonGlobals.ngrset = Resize.resize_int1(CommonGlobals.ngrset, CommonGlobals.NDIFFX + 1);
        CommonGlobals.RAND_WGT = Resize.resize_double1(CommonGlobals.RAND_WGT, CommonGlobals.NDIFFX + 1);
        CommonGlobals.wgtset = Resize.resize_double1(CommonGlobals.wgtset, CommonGlobals.NDIFFX + 1);
        CommonGlobals.wgtsetini = Resize.resize_double1(CommonGlobals.wgtsetini, CommonGlobals.NDIFFX + 1);
        CommonGlobals.chiPoleFig = Resize.resize_double1(CommonGlobals.chiPoleFig, CommonGlobals.NDIFFX + 1);
        CommonGlobals.etaPoleFig = Resize.resize_double1(CommonGlobals.etaPoleFig, CommonGlobals.NDIFFX + 1);

        resizeForNDIFFX_NGR();
    }

    /**
     * Allocates arrays based on NMOD and NSLS.
     */
    public static void allocateForNMOD_NSLS() {
        CommonGlobals.mode_slip = new int[CommonGlobals.NMOD + 1][CommonGlobals.NSLS + 1];
    }

    /**
     * Resizes arrays based on NMOD and NSLS.
     */
    public static void resizeForNMOD_NSLS() {
        CommonGlobals.mode_slip = Resize.resize_int2(CommonGlobals.mode_slip, CommonGlobals.NMOD + 1, CommonGlobals.NSLS + 1);
    }

    /**
     * Allocates arrays based on NSLS and NGR.
     */
    public static void allocateForNSLS_NGR() {
        CommonGlobals.iactSlip = new int[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.iact = new int[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.iChildGrain = new int[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.mcs = new double[6 + 1][CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.qcs = new double[6 + 1][CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.bcs = new double[3 + 1][CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.f = new double[6 + 1][CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.taud = new double[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.tau = new double[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.tau_update = new double[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.rho_for = new double[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.ktwtag = new double[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.ncs = new double[3 + 1][CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
        CommonGlobals.gamd = new double[CommonGlobals.NSLS + 1][CommonGlobals.NGR + 1];
    }

    /**
     * Resizes arrays based on NSLS and NGR.
     */
    public static void resizeForNSLS_NGR() {
        CommonGlobals.iactSlip = Resize.resize_int2(CommonGlobals.iactSlip, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.iact = Resize.resize_int2(CommonGlobals.iact, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.iChildGrain = Resize.resize_int2(CommonGlobals.iChildGrain, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.mcs = Resize.resize_double3(CommonGlobals.mcs, 6 + 1, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.qcs = Resize.resize_double3(CommonGlobals.qcs, 6 + 1, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.bcs = Resize.resize_double3(CommonGlobals.bcs, 3 + 1, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.f = Resize.resize_double3(CommonGlobals.f, 6 + 1, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.taud = Resize.resize_double2(CommonGlobals.taud, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.tau = Resize.resize_double2(CommonGlobals.tau, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.tau_update = Resize.resize_double2(CommonGlobals.tau_update, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.rho_for = Resize.resize_double2(CommonGlobals.rho_for, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.ktwtag = Resize.resize_double2(CommonGlobals.ktwtag, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.ncs = Resize.resize_double3(CommonGlobals.ncs, 3 + 1, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
        CommonGlobals.gamd = Resize.resize_double2(CommonGlobals.gamd, CommonGlobals.NSLS + 1, CommonGlobals.NGR + 1);
    }

    /**
     * Allocates arrays based on NDIFFX and NGR.
     */
    public static void allocateForNDIFFX_NGR() {
        CommonGlobals.igrset = new int[CommonGlobals.NDIFFX + 1][CommonGlobals.NGR + 1];
        CommonGlobals.wgtgrset = new double[CommonGlobals.NDIFFX + 1][CommonGlobals.NGR + 1];
    }

    /**
     * Resizes arrays based on NDIFFX and NGR.
     */
    public static void resizeForNDIFFX_NGR() {
        CommonGlobals.igrset = Resize.resize_int2(CommonGlobals.igrset, CommonGlobals.NDIFFX + 1, CommonGlobals.NGR + 1);
        CommonGlobals.wgtgrset = Resize.resize_double2(CommonGlobals.wgtgrset, CommonGlobals.NDIFFX + 1, CommonGlobals.NGR + 1);
    }

    /**
     * Allocates arrays for the CommonPlastic module.
     */
    public static void allocateForPlasticity() {
        CommonPlastic.shear_mod_acum_ch = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.shear_mod_acum_pa = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.shear_mod_ch = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.shear_dif_acum_ch = new double[CommonGlobals.NDIFFX + 1];
        CommonPlastic.shear_mod_pa = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.shear_dif_acum_pa = new double[CommonGlobals.NDIFFX + 1];
        
        // Fortran multi-allocation line:
        CommonPlastic.shear_mod_acum = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.shear_mod = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.shear_dif_acum = new double[CommonGlobals.NDIFFX + 1];
        
        CommonPlastic.WP = new double[CommonGlobals.NGR + 1];
        CommonPlastic.WC = new double[CommonGlobals.NGR + 1];
        CommonPlastic.aux = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.aux_pa = new double[CommonGlobals.NMOD + 1];
        CommonPlastic.aux_ch = new double[CommonGlobals.NMOD + 1];
    }
}