# Emacs Modes for Groovy and Grails

This is a collection of (X)Emacs modes for use with Groovy-related technology -- Groovy, Grails, etc.

These modes are currently known to work with Emacs 24 and believed to work with Emacs 23.

The Groovy major mode is a derived mode of Java mode which is itself a derived mode in CC Mode. Inspiration
came from Dylan R.E. Moonfire's C# mode.

The best way of installing these modes, at least for Emacs 24, is to use the packaging system. Add MELPA
to the list of repositories:

    (require 'package)
    (add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)

and then use M-x package-list-package to get to the package listing and install from there. MELPA tracks
this Git repository and updates relatively soon after each commit, so there are unlikely to be any formal
releases of this major mode in the future.

The master of all the material is the Git repository at
https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes .

This software is licenced using GNU General Public Licence version 2.
