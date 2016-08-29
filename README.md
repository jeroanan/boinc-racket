# Boinc-Racket

Boinc-Racket is primarily a set of bindings to the BOINC XML-RPC API. It aims to
provide bindings to query and control the connected BOINC client.

BOINC is a framework that allows computers to use idle time to participate in
various distributed computing projects. More information about it can be found
at the [BOINC website](https://boinc.berkeley.edu/ "BOINC homepage").

BOINC-Racket is written in Racket and licensed under the GPL V3. As of V1.0 it
is still very much a work in-progress. Most of the RPC commands themselves have
been written but some of them, particularly those that issue commands to BOINC,
are still untested. For details of how the project is progressing from release
to release, check out the changelog in boinc.rkt.

In order to test the commands that are being written, Boinc-Rakcet is being
written with a GUI. The GUI is not the main focus of the project, so it is
liable to being wrong, bad and unmaintained.

A quick breakdown of the principle files in the project:

1. boinc.rkt -- Contains the changelog. Requires other key files to provide an
   entry point to running the software.

2. boinc-xml.rkt -- Handles sending XML over a TCP connection to the BOINC
   client. It can also handle commands that require autorization. Functions here
   return the raw XML that the BOINC client returns.

3. boinc-structs.rkt -- Contains structs that can be used to hold the values
   returned in the raw XML from the BOINC client.

4. parse.rkt -- Parses the XML output from boinc-xml.rkt into structs from
boinc-structs.rkt.


