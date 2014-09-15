This is a collection of (X)Emacs modes for use with Groovy-related technology -- Groovy, Grails, etc.

The Groovy major mode is a derived mode of Java mode which is itself a derived mode in CC Mode. Inspiration
came from Dylan R.E. Moonfire's C# mode.

This software is licenced using GNU General Public Licence version 2.

The master of all the material is the Git repository at git://github.com/russel/Emacs-Groovy-Mode.git. See
[https://github.com/russel/Emacs-Groovy-Mode](https://github.com/russel/Emacs-Groovy-Mode)

The best way of installing this major mode, at least for Emacs 24, is to use the packaging system. Add MELPA
to the list of repositories:

    (require 'package)
    (add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)

and then use M-x package-list-package to get to the package listing and install from there. MELPA tracks
this Git repository and updates relatively soon after each commit, so there are no formal releases of this
major mode per se any longer.
