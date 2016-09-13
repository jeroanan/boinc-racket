;; Copyright (c) 2016 David Wilson (Jeroanan)

;; This file is part of boinc-racket.

;; boinc-racket is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; boinc-racket is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with boinc-racket.  If not, see <http://www.gnu.org/licenses/>.


;; Changelog:
;; 1.0    26 Aug 2016   First version
;;
;;                      Can make RPC calls, both with and without authorization.
;;
;;                      All commands have basic RPC calls implemented for them,
;;                       albeit most of the commands that have anything to do
;;                       with runnning a command rather than merely querying
;;                       BOINC are untested at this point.
;;
;;                      Most of the "Query" commands can have their returned XML
;;                       parsed into convenient structs. Some of these structs
;;                       are surely unfinished at the moment; commands such as
;;                       get_project_status can contain elements e.g.
;;                       dont_request_more_work that are absent if the client
;;                       has not been told to request no more work for a given
;;                       project.
;;
;;                      There is a rudimentary GUI.
;;                       While I mean for the focus of this project to be to
;;                       provide BOINC bindings for Racket, having a GUI seeemed
;;                       to be the best way to test my work while at the same
;;                       time learning more about Racket/GUI programming. It
;;                       currently just displays project and task status, and it
;;                       doesn't even do that perfectly yet.
;;
;; 1.1    12 Sep 2016   Add README.md
;;
;;                      Added project list dialog.
;;                       It lists all publicly-available BOINC projects and
;;                       allows the user to click through into the attach
;;                       project dialog.
;;
;;                      Added attach project dialog.
;;
;;                      Added widget tools to ease the creation of commonly-used
;;                      GUI widgets. So far this includes:
;;                       + button-tools
;;                       + caution-box
;;                       I (or you) should be able to reuse them in future
;;                       projects that use a GUI.
;;
;;                      Can use attach project dialog to attach to an existing
;;                      account on a project.
;;
;;                      Some big refactoring of the main window
;;
;;                      Can detach from a project
;;
;; 1.2    XX Sep 2016   Some more refactoring of the GUI

#lang racket

;; This file is basically something that holds the change log and references to
;; other things that can be run.

(require "boinc-xml.rkt") ;; Run RPC calls and return raw XML.
(require "boinc-commands.rkt") ;; RUN RPC calls and return structs
(require "boinc-structs.rkt") ;; Access to struct members
(require "gui/main-window/main-window.rkt") ;; The GUI. Run (launch-gui) to start it.
