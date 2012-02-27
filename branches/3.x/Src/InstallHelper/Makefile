# ------------------------------------------------------------------------------
# Makefile
#
# Makefile for the CodeSnip install helper.
#
# $Rev$
# $Date$
#
# ***** BEGIN LICENSE BLOCK *****
#
# Version: MPL 1.1
#
# The contents of this file are subject to the Mozilla Public License Version
# 1.1 (the "License"); you may not use this file except in compliance with the
# License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
#
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
# the specific language governing rights and limitations under the License.
#
# The Original Code is Makefile
#
# The Initial Developer of the Original Code is Peter Johnson
# (http://www.delphidabbler.com/).
#
# Portions created by the Initial Developer are Copyright (C) 2009 Peter
# Johnson. All Rights Reserved.
#
# Contributors:
#   NONE
#
# ***** END LICENSE BLOCK *****
# ------------------------------------------------------------------------------


# BIN macro required by Common.mak
BIN = ..\..\Bin\InstallHelper

!include ..\Common.mak

# Default is to build the exe file
exe: resources pascal

# Synonyms
pas: pascal
res: resources

# Compile the resources and delete intermediate file created by VIED
resources: CSSetupHelperRes.res VCSSetupHelper.res
  -@del VCSSetupHelper.rc

# Compile Pascal source and link in resources
pascal: CSSetupHelper.exe

# Configure source folder
config:
  @echo Configuring InstallHelper
  # Create .cfg file from template
  @copy /Y CSSetupHelper.cfg.tplt CSSetupHelper.cfg
