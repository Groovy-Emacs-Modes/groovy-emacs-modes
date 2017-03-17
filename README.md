# Emacs Modes for Groovy and Grails

[![Licence](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/groovy-mode-badge.svg)](https://melpa.org/#/groovy-mode)
[![MELPA](https://stable.melpa.org/packages/groovy-mode-badge.svg)](https://stable.melpa.org/#/groovy-mode)


*Nota Bene A new branch standalone\_mode has been started to shift this Groovy mode from being a sub-mode of
Java mode in CC Mode to being a standalone mode. Being a CC Mode has led to some problems that many feel are
blocking to progress of this mode. Becoming a standalone mode required more code in the source to replace all
the bits and pieces the came "for free" from CC Mode. On the other hand it means that it is possible to make
Groovy Mode whatever people want. So we are looking for people to clone this repository and use the mode in
standalone\_mode branch and give feedback, raise issues, send in pull requests, etc. The intention is to
merge standalone\_mode into master so that it becomes the official Groovy Mode once people are happy.*


This is a collection of (X)Emacs modes for use with Groovy-related technology -- Groovy, Grails, etc.

These modes are currently known to work with Emacs 24 and believed to work with Emacs 23.

The Groovy major mode is a derived mode of Java mode which is itself a derived mode in CC Mode. Inspiration
came from Dylan R.E. Moonfire's C# mode.

The best way of installing these modes, at least for Emacs 24, is to use the packaging system. Add MELPA or
MELPA Stable to the list of repositories to access this mode. For those who want only formal, tagged
releases use MELPA Stable:

    (require 'package)
    (add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
    (package-initialize)

For those who want rolling releases as they happen use MELPA:

    (require 'package)
    (add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)

and then use M-x package-list-package to get to the package listing and install from there. MELPA tracks
this Git repository and updates relatively soon after each commit or formal release. For more detail on
setting up see [MELPA Getting Started](https://melpa.org/#/getting-started).

The master of all the material is the Git repository at
https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes .

This software is licenced using GNU General Public Licence version 3.
