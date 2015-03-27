# endf6-loading
The SUBROUTINE and DERIVED TYPE definition enables the programmer to load cross section data from the ENDF-6 format database.  If you do not know what is ENDF-6, then this code apparently has no use to you.

The programmer should take care of OPENing and CLOSEing the database in the program, and the SUBROUTINE load_endf6_xs(u_xs, req_mt, xsc, amass) will load the section of data, marked by MF=26, MT=req_mt, OPENed in the program using UNIT=u_xs, to the DERIVED TYPE variable xsc.  This SUBROUTINE also optionally reads the atomic mass of this isotope.
