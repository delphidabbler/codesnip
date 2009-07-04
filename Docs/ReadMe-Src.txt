
¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
DELPHIDABBLER CODESNIP SOURCE CODE README
________________________________________________________________________________

Source code for the current version of DelphiDabbler CodeSnip is always
available from http://www.delphidabbler.com/download?id=codesnip&type=src.

CodeSnip must be compiled with Delphi 2006 or later. These instructions assume
Delphi 2006. Versions before v1.9.1 were compiled with Delphi 7.

CodeSnip's source code is provided in a zip file, dd-codesnip-src.zip. This file
includes all CodeSnip's original code along with some 3rd party sources. Files
should be extracted from the zip file and the directory structure should be
preserved.

The directory structure is:

  Bin             : Binary resource and type library files (*1)
    InstallHelper : Binary resource files for install helper (*1)
  Src             : Pascal, other source code & build script (*2)
    3rdParty      : 3rd party source code 
    AutoGen       : Automatically generated source code
    Help          : Source files for the CodeSnip.chm HTML Help file
      CSS         : Cascading style sheet used by then Help file
      HTML        : HTML source documents for HTML help topics
      Images      : GIF and PNG files used by the HTML help file
    Install       : Scripts used to compile the setup program
    InstallHelper : Source code & build script for the install helper prog
      Res         : Assets used by the install helper program
    Res           : Contains subfolders of assets compiled into resources
      HTML        : HTML, JavaScript and CSS files
      Img         : Image files
      Misc        : Other file types

  Notes:
    (*1) - See below for details of how to recompile these files
    (*2) - Build script file described below

In order to compile the program you also need to create a "Exe" directory at the
same level as the "Bin" and "Src" directories.

Additional libraries / components are required to compile CodeSnip successfully.
They are:

  From DelphiDabbler.com:
    Version Information Component (v3.1.1 or later)
    System Information Unit (v3.0 or later)
    Window State Components (v5.3 or later, v5 or later preferred)
    Stream Extension Classes (v2.0.1 or later)
  The Indy internet components v9 (supplied with Delphi 2006). Note you need to
    configure Delphi 2006 to use Indy v9 rather han Indy v10
  The Delphi 2006 VCL.

All the libraries must be available on Delphi's library path.

NOTE: None of the the additional components need to be installed in Delphi's
component palette since none are required at design time - they are all created
at run time as required.

The program also requires six binary files in order to compile (provided in the 
Bin folder (1..4) and Bin\InstallHelper folder (5..6)):

1) VCodeSnip.res - the program's version information.
2) HTML.res - RT_HTML resources containing the HTML, JavaScript, CSS and images
   displayed in various web browser controls.
3) Resources.res - all other resources.
4) CodeSnip.tlb - type library.
5) CSSetupHelperRes.res - general resources for install helper
6) VCSSetupHelper.res - version information for the install helper


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Build Tools
________________________________________________________________________________

A batch file - Build.bat - is provided in the "Src" directory. It can be used to
automate full or partial builds of CodeSnip. It must be called with a command
line switch. Switches are:

  all   - Builds everything.
  res   - Builds binary resource files only. Requires VIEd, HTMLRes and BRCC32.
          Creates VCodeSnip.res, Resources.res and HTML.res from VCodeSnip.vi,
          Resources.rc and HTML.hrc respectively.
  tlb   - Builds the ExternalObj.tlb type library and creates the
          IntfExternalObj.pas pascal source for its interfaces from
          ExternalObj.idl. Requires TLibImp and MIDL.
  pas   - Builds the Delphi Pascal project. Requires DCC32.
  help  - Builds the HTML help file. Requires HHC.
  setup - Builds the install program. Requires ISCC and ISPP. Creates installer
          from CodeSnip.iss.

The programs required by the build process are:

+ VIEd    - DelphiDabbler Version Information Editor, available from
            www.delphidabbler.com. Requires v2.11 or later.
+ HTMLRes - DelphiDabbler HTML Resource Compiler, available from
            www.delphidabbler.com. Requires v1.1 or later.
+ BRCC32  - Borland Resource Compiler, supplied with Delphi 2006.
+ TLibImp - Borland Type Library Importer, supplied with Delphi 2006.
+ MIDL    - Microsoft IDL Compiler, supplied with the MS Platform SDK. MIDL also
            requires the use of Microsoft's CL.exe C Pre-processor which in turn
            requires mspdb**.dll, where ** is a number that depends on the
            version of Visual Studio used. CodeSnip was compiled with MIDL v7
            and mspdb80.dll from the Windows 2008 (v6.1) platform SDK.
+ DCC32   - Delphi Command Line Compiler, supplied with Delphi 2006.
+ HHC     - Microsoft HTML Help Compiler, supplied with Microsoft HTML Help
            Workshop.
+ ISCC    - Inno Setup command line compiler, supplied with Inno Setup v5.2.3 or
            later. Also requires the ISPP pre-processor v5.2.3 or later.
        
Note that this script does not build CSSetupHelper.exe. A separate script is
provided for that, again named Build.bat but this time located in
"Src\InstallHelper". This script is called with no parameters and compiles the
resources and pascal source. The script requires DCC32, BRCC32 and VIEd. The
script was kept separate from the main script because it is unlikely the install
helper will need to be rebuilt very often. CSSetupHelper is also built with
Delphi 2006. *** WARNING: Make sure you have a build of CSSetupHelper.exe before
creating the setup program, because the setup includes CSSetupHelper.exe.

Both build scripts expect certain environment variables to be present. They are:

+ DELPHI2006
    Set to the install directory of Delphi 2006.
+ DELPHIDABLIBD2006
    Set to the install directory of the required DelphiDabbler components on
    Delphi 2006.
+ INDY9
    Set to the folder containing the Delphi 2006 Indy 9 components.
+ MSSDK
    Set to Microsoft Platform SDK install directory. MIDL.exe will be in the Bin
    sub-directory and include files in Include sub-directory.
+ MSCPP
    Set to the path to the MS cl.exe C pre-processor. This will probably be in a
    sub folder of a MS Visual Studio installation folder.
+ MSCOMMONIDE
    Set to the Common Visual studio binary directory where mspdb**.dll is
    located (required by cl.exe).
+ INNOSETUP
    Set to Inno Setup install directory.

VIEd is expected to be on the system path.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Licensing
________________________________________________________________________________

Please see SourceCodeLicenses.txt in the Docs directory for information about
source code licenses.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Earlier Versions
________________________________________________________________________________

If you would like the source code of earlier, publicly released, versions of
CodeSnip, please contact me at delphidabbler [At] yahoo [DOT] co [DOT] uk,
specifying the version you would like source code for. See the change log at
http://www.delphidabbler.com/software/codesnip/changelog for details of
available versions. 

Please note: source code for versions more than one year old or of versions that
had no public release may not be available. 


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
Contribute to the Database
________________________________________________________________________________

Please do contribute code to the on-line Code Snippets database. Please send any
complete functions, procedures, constants definitions and type definitions to
the author by sending an email to delphidabbler [At] yahoo [DOT] co [DOT] uk.

See the CodeSnip help file for information about the types of code that can be
included in the database.


¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯