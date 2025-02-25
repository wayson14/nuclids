#!/usr/bin/python


# Python script to read data of HIVAP output file format
# (hivaperg.dat), extract cross-section data etc. and write
# the data to stdout in a "compact format".
# Note: This version of the script should be used for output files
# produced by the version of HIVAP from 1994-06-07 with German text.
# Note: the HIVAP run must be performed with DISC=3 in the HIVAP
# input file.
#
# Johan Nyberg, 2015
#
# ToDo list:
# ----------
# - Write only specific residual nuclei, option -n "102sn 103sn 102In".
# - Extract and print spin-distributions of final nuclei, if available.
# - Is there info in the input file regarding particle multiplicities
#   for each exit channel? If so, one can get info from that regarding
#    the emission of alpha versus 2p2n.

# For use of the python 3.x print function in python >=2.6 :
from __future__ import print_function

# Script version number:
rev = "%(prog)s 2.1"

import argparse

# import io
# import os
import sys
import string
import numpy as np
import matplotlib.pyplot as plt


def check_python_version():
    # Check that we have python version >= 2.7, else exit
    # print(sys.version_info[0] + sys.version_info[1]);
    if sys.version_info[0] > 2:
        return
    if sys.version_info[0] >= 2 and sys.version_info[1] >= 7:
        return
    print("Your python version is too old!")
    print("This script requires version >= 2.7")
    print("Your version is: ", sys.version)
    sys.exit(1)


def get_args(argv=None):
    # This function decodes the command line arguments and returns the
    # arguments
    #
    # Create an ArgumentParser object caller parser:
    parser = argparse.ArgumentParser(
        # formatter_class=argparse.RawTextHelpFormatter,
        # formatter_class=argparse.RawDescriptionHelpFormatter,
        description="Read HIVAP output file of type hivaperg.dat, \
    extract cross section data etc. from the file and write it \
    to stdout or to an output file. \n\n\
    Note: this script works only with hivaperg.dat files \
    produced by the HIVAP version from 1994-06-09 (with german \
    text) when it has been run with option DISC=3.",
        epilog="Johan Nyberg, Feb-Sep 2015",
    )

    # USe the add_argument method to take the strings on the command line
    # and turn them into objects:
    if sys.version_info > (3, 4):
        # Python version >= 3.4 code in this block.
        # The hivaperg.dat files produced by with the HIVAP version from
        # 1994-06-07 (contains german text) contains a character on the
        # (a german 'double s' character) third line that is not compatible
        # with utf-8 encoding -> python3 gives the error:
        #   UnicodeDecodeError: 'utf-8' codec can't decode byte 0xdf in
        #   position 115: invalid continuation byte
        # This can be fixed by using
        # ... type=argparse.FileType('r', encoding='ISO-8859-1') ...
        # or
        # ... type=argparse.FileType('r', errors='replace') ...
        # in parser.add_argument(). However, the keyword arguments encoding and
        # errors are not supported by python versions <3.4. Hence this test of
        # python version.
        parser.add_argument(
            "input",  # action="store_true",
            type=argparse.FileType(mode="r", encoding="ISO-8859-1"),
            # type=argparse.FileType(mode='r', errors='replace'),
            # type=argparse.FileType(mode='r'),
            help="input file, use '-' for stdin",
        )
    else:
        parser.add_argument(
            "input",  # action="store_true",
            type=argparse.FileType(mode="r"),
            help="input file, use '-' for stdin",
        )

    # Output file is opened with mode='a' (append; mode 'x' does not exist!)
    parser.add_argument(
        "output",  # action="store_true",
        type=argparse.FileType(mode="a"),
        help="output file (appended to if it exists), \
                      use '-' for stdout",
    )
    parser.add_argument(
        "-r",
        "--reverse",
        action="store_true",
        help="print table in reverse energy order",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="verbose output (for debugging)"
    )
    parser.add_argument(
        "--version",
        action="version",
        version=rev,
        help="print script and python version",
    )

    # Require at least one argument:
    if len(sys.argv) < 2:
        parser.print_usage()
        sys.exit(1)

    # Return the decode command line arguments:
    return parser.parse_args(argv)


def main():

    # Check that we have good enough python version:
    check_python_version()

    # Get the command line arguments
    argvals = None  # init argv in case not testing
    # argvals = '6 2 -v'.split() #example of passing test params to parser
    args = get_args(argvals)

    # Verbose:
    if args.verbose:
        print("Verbosity turned on")
        print("Python version: " + sys.version)

    # Read input data:
    if args.verbose:
        print("Start reading data.")
    # if args.verbose: print("args.input = ", args.input);
    data = args.input.read()
    if args.verbose:
        print("End reading data.")

    # Test if output file exists. If so exit with an error
    # if os.path.isfile(args.output.name) == True:
    #  print("Output file", args.output.name, "exists. Exiting script ...");
    #  exit(1)

    # Create list my_list, which contains the lines in the input data,
    # breaking at line boundaries:
    my_list = data.splitlines()

    # Return length of the list:
    nrl = len(my_list)
    if args.verbose:
        print("{}{}".format("Nr of lines in input data = ", nrl))

    # Create and initialise arrays:

    # Nuclide id, e.g. 100Sn, 101Sn etc:
    Nuc = ["" for i in range(256)]

    # Excitation energy of CN:
    Estar = [0.0 for i in range(128)]

    # Cross section as 2D array with E* vs nuc id.
    sigres = [[[0.0 for z in range(16)] for n in range(16)] for e in range(16)]

    # if args.verbose: print ("sigres = ", sigres);

    # Beam energy lab
    Elab = [0.0 for i in range(128)]

    # Fusion cross section etc.
    sigfus = [0.0 for i in range(128)]
    sigsum = [0.0 for i in range(128)]
    sigrat = [0.0 for i in range(128)]
    sigvr = [0.0 for i in range(128)]
    sigfis = [0.0 for i in range(128)]
    lcrit = [0.0 for i in range(128)]
    bfis = [0.0 for i in range(128)]

    iw = 0
    nuc = 0
    enr = 0
    znr = -1
    nnr = 0
    sifus = 0
    xsects = 0
    reaction = ""
    l = 0
    nrn = 0
    nri = 0

    hivap1990 = "THIS IS HIVAP VERSION HIVA 20 JUN 90"
    hivap1994 = (
        'Programm HIVAP,  Autor  W.Reisdorf   Version fuer "rzri6f" vom  7. 6. 1994'
    )

    # if args.verbose:
    #  print (hivap1990);
    #  print (hivap1994);
    #  print (my_list[0]);

    if hivap1990 in my_list[0]:
        ver = 1990
        fusion_table = "ELAB   MEV/U   EXCIT      SIGFUS"
        # "ELAB   MEV/U   EXCIT      SIGFUS      SIGEVA      SIGFIS   LCRIT   PERCNT FIS"
        shell_effect_table = "EXP SHELLS"
        barfac = "BARFAC"
        cross_section_table = "   CROSS SECTIONS (MB)"
        z_distribution_table = "Z-DISTRIBUTION"
        estar_cross_section_table = "EXCIT"
    elif hivap1994 in my_list[0]:
        ver = 1994
        shell_effect_table = "Exp. Schaleneffekte"
        barfac = "BARFAC"
        fusion_table = "SI_fus"
        cross_section_table = "     Querschnitte / mbarn"
        z_distribution_table = "Z-Verteilung"
        estar_cross_section_table = "E*/MeV"
    else:
        print(
            "Unknown version of HIVAP input file. First line should \
    contain version info. See details in script. Exiting script ..."
        )
        exit(1)

    if args.verbose:
        print("Input file for HIVAP version from ", ver)

    # Extract the nuclide names of all nuclides produced. The names
    # are extracted from the table with the shell effects, which
    # starts with the line containing the valye in string shell_effect_table
    # and ends with a line that contains the value in string barfac.
    # The table itself contains two lines for each element. The first
    # line contains the isotopes of the element, e.g.
    #
    # Sn 105  Sn 106  Sn 107  Sn 108  Sn 109  Sn 110  Sn 111  Sn 112
    #
    # The second line contains the shell effect values in MeV for each
    # isotope, e.g.
    # -8.00   -7.00   -6.20   -5.40   -4.81   -4.21   -3.79   -3.36

    nrn = 0
    # total nr of final nuclides produced.
    for l in range(0, nrl):  # Loop through all lines..
        if len(my_list[l]) == 0:  # Skip all empty lines
            continue
        # if args.verbose: print(l, my_list[l]);
        if shell_effect_table in my_list[l]:  # Found shell effect table
            # if "Exp. Schaleneffekte" in my_list[l]:
            for l2 in range(l + 1, nrl, 2):
                # if args.verbose: print(l2, my_list[l2]);
                if my_list[l2] == "" or barfac in my_list[l2]:
                    # if args.verbose: print("Found empty line or BARFAC");
                    break
                else:
                    words = my_list[l2].split()
                    nw = len(words)
                    # nri = nr of isotopes per element. Same for all elements.
                    # Used later when extracting cross section data/
                    nri = nw / 2
                    # if args.verbose: print("nw = ", nw, "nri = ", nri);
                    for w in range(0, nw, 2):
                        s1 = words[w + 1] + words[w]
                        Nuc[nrn] = "".join(s1.split())
                        # if args.verbose: print(w, words[w], words[w+1],
                        #                       nrn, Nuc[nrn]);
                        nrn = nrn + 1

    if nrn == 0:
        print(
            "Something wrong with the input file: could not find table ",
            "with shell effects (Exp. Schaleneffekte). ",
            "Exiting script...",
        )
        exit(1)

    # if args.verbose:
    #    print("Nr of isotopes per element =", nri);
    #    print("Total nr of final nuclides =", nrn, "and the list:");
    #    print(Nuc[0:nrn]);

    # Note: the only thing used below from the above extraction of
    # nuclide names etc from the table with shell effects is nri
    # (nr of isotopes per element)! How to find a simpler and more
    # clever way to extract this nr? This number is given in the
    # hivapein.dat input file as "NEUTRONS= nri".

    # Loop through list and extract the stuff.
    #
    # The beam energy, fusion cross section, l_crit etc are assumed
    # to be in a table containing the value of string fusion_table
    # in the header. Variable sifus is set = 1 when this table is
    # processed.
    #
    # The cross section data for the residual nuclei is assumed to
    # be between the lines containing the value of the strings
    # cross_section_table and z_distribution_table. xsects = 1
    # when data inside these tables are processed.

    # l = 0;
    for str in my_list:
        if len(str) == 0:  # Skip all empty lines
            continue
        # if args.verbose: print (str);
        if "ZT" in str:  # Line containing ZT, AT, ZP, AP
            # if args.verbose: print (str);
            reaction = str
            continue
        if fusion_table in str:  # Start of fusion table
            # if "SI_fus" in str:
            # if "ELAB   MEV/U   EXCIT      SIGFUS      SIGEVA      SIGFIS   LCRIT   PERCNT FIS" in str:
            # if args.verbose: print (str);
            sifus = 1
            e0 = 0
            continue
        if str == cross_section_table:  # End of SIGFUS, start of xsect tables
            # if "Querschnitte / mbarn" in str:
            # if "   CROSS SECTIONS (MB)" in str:
            # if args.verbose: print (str);
            xsects = 1
            sifus = 0
            continue
        if z_distribution_table in str:  # End of xsect tables
            # if "Z-Verteilung" in str:
            # if "Z-DISTRIBUTION" in str:
            # if args.verbose: print (str);
            xsects = 0
            sifus = 0
            break

        if sifus == 1:  # We are inside the sigfus tables
            # if args.verbose: print (str);
            words = str.split()
            Elab[e0] = float(words[0])
            sigfus[e0] = float(words[3])
            sigvr[e0] = float(words[4])
            sigfis[e0] = float(words[5])
            lcrit[e0] = float(words[6])
            bfis[e0] = float(words[7])
            # casout.f: bfis = sigfis/sigfus*100
            e0 = e0 + 1
            continue
        if xsects == 1:  # We are inside the xsect tables
            if estar_cross_section_table in str:  # header line of xsect table
                # if "E*/MeV" in str: # header line of xsect table
                # if "EXCIT" in str: # header line of xsect table
                enr = 0
                # reset the E* counter
                znr = znr + 1  # increment the Z (element) nr counter
                # if args.verbose: print ("znr =", znr);
                # if args.verbose: print (str);
                words = str.split()
                nrw = len(words)
                # if args.verbose: print ("len(words) = ", nrw);
                if nrw - 1 == nri:  # no space between mass number and element nr
                    for iw in range(0, nrw):  # Extract the nuclide names
                        # for w in words: # Extract the nuclide names
                        # if args.verbose:
                        #  print ("iw = ", iw);
                        #  print ("words[", iw, "]", words[iw]);
                        if iw == 0:
                            estar = words[iw]
                            # This word should be "E*/MeV"
                        else:
                            s1 = words[iw]
                            Nuc[nuc] = "".join(s1.split())
                            # if args.verbose:
                            #  print("Nuc[", nuc, "] = ", Nuc[nuc]);
                            nuc = nuc + 1
                            # if args.verbose: print("nuc = ", nuc)
                else:
                    for iw in range(0, nrw, 2):  # Extract the nuclide names
                        # for w in words: # Extract the nuclide names
                        # if args.verbose:
                        #  print ("iw = ", iw);
                        #  print ("words[", iw, "]", words[iw]);
                        if iw == 0:
                            estar = words[iw]
                            # This word should be "E*/MeV"
                        else:
                            s1 = words[iw - 1] + words[iw]
                            Nuc[nuc] = "".join(s1.split())
                            # if args.verbose:
                            #  print("Nuc[", nuc, "] = ", Nuc[nuc]);
                            nuc = nuc + 1
                            # if args.verbose: print("nuc = ", nuc)
            else:  # Here are the lines with E* (first column) and xsects
                words = str.split()
                nrw = len(words)
                for iw in range(0, nrw):  # Extract E* and cross sections
                    # if args.verbose: print ("iw=", iw);
                    if iw == 0:  # This is the first column with the E* value
                        Estar[enr] = float(words[0])
                        # if args.verbose: print ("Estar[", enr, "] = ",
                        #                        Estar[enr]);
                    else:  # cross section values in the remaining nri columns
                        nnr = iw - 1
                        # neutron nr counter
                        # if args.verbose: print ("nnr=", nnr);
                        sigres[znr][nnr][enr] = float(words[iw])
                        # if args.verbose: print ("sigres[", \
                        #                        znr, "][", \
                        #                        nnr, "][", \
                        #                        enr, "] = ", \
                        #                    sigres[znr][nnr][enr]);
                enr = enr + 1
                # increment the E* counter
                # if args.verbose: print ("enr=", enr);

    nrz = znr + 1
    nrn = nnr + 1
    nre = enr

    if args.verbose:
        print("nrz =", nrz, "nrn =", nrn, "nre =", nre)

    # Calculate SIGSUM and SIGSUM/SIGFUS:

    for e in range(0, nre):
        sigsum[e] = 0.0
        for z in range(0, nrz):
            for n in range(0, nrn):
                sigsum[e] = sigsum[e] + sigres[z][n][e]
                if sigfus[e] > 0:
                    sigrat[e] = sigsum[e] / sigfus[e]
                else:
                    sigrat[e] = "undef"
        # if args.verbose: print (sigsum[e], sigrat[e]);

    # Print results:

    print("Reaction: ", reaction, file=args.output)
    print(
        "Energies in MeV, cross sections in mb, ",
        "angular momenta in hbar",
        file=args.output,
    )
    print(
        "SIGSUM = sum of residual nucleus cross sections. ",
        "SIGRAT = SIGSUM/SIGFUS",
        file=args.output,
    )
    print("", file=args.output)

    print("{:>6}".format("ELAB"), end="", file=args.output)
    # print("{:>6}".format("MeV/u"), end='', file = args.output);
    print("{:>6}".format("E*_CN"), end="", file=args.output)
    print("{:>10}".format(" SIGFUS"), end="", file=args.output)
    print("{:>10}".format(" SIGSUM"), end="", file=args.output)
    print("{:>10}".format(" SIGRAT"), end="", file=args.output)
    print("{:>10}".format("SIGEVAP"), end="", file=args.output)
    print("{:>10}".format(" SIGFIS"), end="", file=args.output)
    print("{:>7}".format(" LCRIT"), end="", file=args.output)
    for n in range(0, nuc):
        s1 = "sig_" + Nuc[n]
        nucstr = "".join(s1.split())
        print("{:>10}".format(nucstr), end="", file=args.output)
        # print ("{0:>5}{1:>5}".format("sig_",Nuc[n]),
        #       end='', file = args.output);
    print("", file=args.output)

    sta = 0
    sto = nre
    stp = 1
    if args.reverse:
        sta = nre - 1
        sto = -1
        stp = -1

    # Produce and print table in reverse order compared to the input data:
    # for e in range(nre-1, -1, -1):
    # Produce and print table:
    # for e in range(0, nre, 1):
    cross = [[0] * (nrz * nrn) for i in range(sta, sto, stp)]
    energyIndex = 0
    energyList = [0 for i in range(sta, sto, stp)]
    for e in range(sta, sto, stp):
        energyList[e] = Elab[e]
        print("{:6.1f}".format(Elab[e]), end="", file=args.output)
        print("{:6.1f}".format(Estar[e]), end="", file=args.output)
        print("{:10.3e}".format(sigfus[e]), end="", file=args.output)
        print("{:10.3e}".format(sigsum[e]), end="", file=args.output)
        print("{:10.3e}".format(sigrat[e]), end="", file=args.output)
        print("{:10.3e}".format(sigvr[e]), end="", file=args.output)
        print("{:10.3e}".format(sigfis[e]), end="", file=args.output)
        print("{:7.1f}".format(lcrit[e]), end="", file=args.output)
        # print ("{:7.2f}".format(bfis[e]), end='', file = args.output);
        nuclIndex = 0
        for z in range(0, nrz):
            for n in range(0, nrn):
                print("{0:10.2e}".format(sigres[z][n][e]), end="", file=args.output)
                # print("{0:10.2e}".format(sigres[z][n][e]), end='');
                # print ("ind", energyIndex, nuclIndex, sigres[z][n][e], z, n);
                cross[energyIndex][nuclIndex] = sigres[z][n][e]

                nuclIndex += 1
        energyIndex += 1
        print("", file=args.output)
    arr = np.array(cross)

    effFact = [
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0.00009,
        0.00009,
        0.0000018,
        0.0000000063,
        0,
        0,
        0.000018,
        0.000018,
        0.00000036,
        0.0000000063,
        0,
        0,
        0.0000936,
        0.0000036,
        0.000000072,
        0.000000072,
        0,
        0,
        0.000000072,
        0,
        0.000000036,
        0.00000036,
        0,
        0,
        0,
    ]

    # plt.subplot(2, 1, 1);
    # for i in range(nuclIndex):
    #  if(sum(arr[:, i])>= 100 or Nuc[i] == "34Ar"):
    #      plt.plot(energyList, arr[:, i], label=Nuc[i]);
    #      print(Nuc[i])

    # plt.legend()
    # plt.yscale('log')
    # plt.subplot(2, 1, 2)

    # I HAVE COMMENTED FOLLOWING SCOPE
    # for i in range(nuclIndex):
    #     if sum(arr[:, i]) * effFact[i] >= 1e-4 or Nuc[i] == "45V":
    #         plt.plot(energyList, arr[:, i] * effFact[i], label=Nuc[i])
    # plt.legend()
    # plt.yscale("log")
    # # plt.title('Masked and NaN data')
    # plt.show()


# Execute function main:
main()
