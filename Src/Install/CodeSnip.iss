;===============================================================================
; CodeSnip.iss
;                    
; Install file generation script for use with Inno Setup.
;
; v1.0 of 04 Jun 2006  - Original version for release 1.0.
; v1.1 of 14 Oct 2006  - Updated to reference release 1.0.1.
; v1.2 of 25 Oct 2006  - Updated to reference release 1.0.2.
; v1.3 of 26 Oct 2006  - Updated to reference release 1.0.3.
; v1.4 of 29 Oct 2006  - Updated to reference release 1.1.
; v1.5 of 29 Oct 2006  - Updated to reference release 1.1.1.
; v1.6 of 07 Nov 2006  - Updated to reference release 1.1.2.
; v1.7 of 08 Nov 2006  - Updated to reference release 1.2.
; v1.8 of 11 Nov 2006  - Updated to reference release 1.2.1.
; v1.9 of 12 Nov 2006  - Updated to reference release 1.2.2.
; v1.10 of 12 Nov 2006 - Updated to reference release 1.2.3.
; v1.11 of 14 Nov 2006 - Updated to reference release 1.2.4.
; v1.12 of 16 Nov 2006 - Updated to reference release 1.2.5.
; v1.13 of 18 Nov 2006 - Updated to reference release 1.3.
; v1.14 of 20 Nov 2006 - Updated to reference release 1.3.1.
; v1.15 of 24 Nov 2006 - Updated to reference release 1.3.2.
; v1.16 of 25 Nov 2006 - Updated to reference release 1.3.3.
; v1.17 of 26 Nov 2006 - Updated to reference release 1.3.4.
; v1.18 of 01 Dec 2006 - Updated to reference release 1.3.5.
; v1.19 of 03 Dec 2006 - Updated to reference release 1.4.0.
; v1.20 of 03 Dec 2006 - Updated to reference release 1.4.1.
; v1.21 of 03 Dec 2006 - Updated to reference release 1.4.2.
; v1.22 of 03 Dec 2006 - Updated to reference release 1.4.3.
; v1.23 of 04 Dec 2006 - Updated to reference release 1.4.4.
; v1.24 of 04 Dec 2006 - Updated to reference release 1.4.5.
; v1.25 of 17 Dec 2006 - Updated to reference release 1.4.6.
; v1.26 of 03 Feb 2007 - Updated to reference release 1.5.
;                      - Updated program copyright date range to include 2007.
; v1.27 of 04 Feb 2007 - Updated to reference release 1.5.1.
; v1.28 of 04 Feb 2007 - Updated to reference release 1.5.2.
; v1.29 of 08 Feb 2007 - Updated to reference release 1.5.3.
; v1.30 of 09 Feb 2007 - Updated to reference release 1.5.4.
; v1.31 of 11 Feb 2007 - Updated to reference release 1.5.5.
; v1.32 of 11 Feb 2007 - Updated to reference release 1.5.6.
; v1.33 of 12 Feb 2007 - Updated to reference release 1.5.7.
; v1.34 of 16 Feb 2007 - Updated to reference release 1.5.8.
; v1.35 of 16 Feb 2007 - Updated to reference release 1.5.9.
; v1.36 of 17 Feb 2007 - Updated to reference release 1.5.10.
; v1.37 of 25 Feb 2007 - Updated to reference release 1.5.11.
; v1.38 of 01 Mar 2007 - Updated to reference release 1.5.12.
; v1.39 of 04 Mar 2007 - Updated to reference release 1.5.13.
; v1.40 of 08 May 2007 - Updated to reference release 1.6.
; v1.41 of 09 May 2007 - Updated to reference release 1.6.1.
; v1.42 of 12 May 2007 - Updated to reference release 1.6.2.
; v1.43 of 13 May 2007 - Updated to reference release 1.6.3.
; v1.44 of 02 Jul 2007 - Updated to reference release 1.6.4.
; v1.45 of 08 Sep 2007 - Updated to reference release 1.7.0.
;                      - Added [Code] section that includes new UpdateIni.pas.
; v1.46 of 22 Sep 2007 - Updated to reference release 1.7.1.
; v1.47 of 24 Sep 2007 - Updated to reference release 1.7.2.
;                      - Updated web links to CodeSnip page to use new software
;                        URL format.
; v1.48 of 27 Sep 2007 - Updated to reference release 1.7.3.
; v1.49 of 14 Oct 2007 - Updated to reference release 1.7.4.
; v1.50 of 17 Oct 2007 - Updated to reference release 1.7.5.
; v1.51 of 18 Oct 2007 - Updated to reference release 1.7.6.
; v1.52 of 29 Oct 2007 - Updated to reference release 1.7.7.
; v1.53 of 03 Nov 2007 - Updated to reference release 1.8.0.
; v1.54 of 04 Nov 2007 - Updated to reference release 1.8.1.
; v1.55 of 04 Nov 2007 - Updated to reference release 1.8.2.
; v1.56 of 05 Nov 2007 - Updated to reference release 1.8.3.
; v2.0 of 21 Apr 2008  - Re-written to work with ISPP pre-processor.
;                      - Now automatically gets version number from CodeSnip.exe
;                        removing need to update this file for each release.
;                      - Most paths, files and other app information now
;                        represented by pre-processor macros.
; v2.1 of 30 May 2008  - Removed option to create desktop icon.
;                      - Removed isreadme flag from files section and added
;                        display of readme file to run section.
; v2.2 of 14 Aug 2008  - Included new DataLocations.ps, EventHandler.ps and
;                        UpdateDBase.ps scripts to enable database and config
;                        to be moved to new locations if necessary.
;                      - Included install helper program in files. This file is
;                        only used during installation.
;                      - Data folders are now created in and removed from in
;                        common application data folder rather than in user's
;                        application data folder.
; v2.3 of 10 May 2009  - Changed to prevent installation on Windows 9x.
;
;
;===============================================================================
;
; ***** BEGIN LICENSE BLOCK *****
;
; Version: MPL 1.1
;
; The contents of this file are subject to the Mozilla Public License Version
; 1.1 (the "License"); you may not use this file except in compliance with the
; License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
;
; Software distributed under the License is distributed on an "AS IS" basis,
; WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
; the specific language governing rights and limitations under the License.
;         
; The Original Code is CodeSnip.iss
;
; The Initial Developer of the Original Code is Peter Johnson
; (http://www.delphidabbler.com/).
;
; Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
; Johnson. All Rights Reserved.
;
; ***** END LICENSE BLOCK *****
;
;===============================================================================


; Deletes "Release " from beginning of S
#define DeleteToVerStart(str S) \
  /* assumes S begins with "Release " followed by version as x.x.x */ \
  Local[0] = Copy(S, Len("Release ") + 1, 99), \
  Local[0]

; The following defines use these macros that are predefined by ISPP:
;   SourcePath - path where this script is located
;   GetStringFileInfo - gets requested version info string from an executable
;   GetFileProductVersion - gets product version info string from an executable

#define AppPublisher "DelphiDabbler"
#define AppName "CodeSnip"
#define ExeFile AppName + ".exe"
#define HelpFile AppName + ".chm"
#define ReadMeFile "ReadMe.txt"
#define LicenseFile "License.rtf"
#define LicenseTextFile "License.txt"
#define PrivacyFile "Privacy.txt"
#define OutDir SourcePath + "..\..\Exe"
#define SrcDocsPath SourcePath + "..\..\Docs\"
#define SrcExePath SourcePath + "..\..\Exe\"
#define ExeProg SrcExePath + ExeFile
#define AppVersion DeleteToVerStart(GetFileProductVersion(ExeProg))
#define Copyright GetStringFileInfo(ExeProg, LEGAL_COPYRIGHT)
#define Company "DelphiDabbler.com"
#define WebAddress "www.delphidabbler.com"
#define WebURL "http://" + WebAddress + "/"
#define AppURL WebURL + "software/codesnip"
#define InstUninstDir "Uninst"
#define SetupHelper "CSSetupHelper.exe"


[Setup]
AppID={{B4BF7490-25F0-4CB1-A2D5-9E006D9FF05F}
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppPublisher} {#AppName} {#AppVersion}
AppPublisher={#AppPublisher}
AppPublisherURL={#WebURL}
AppSupportURL={#AppURL}
AppUpdatesURL={#AppURL}
AppReadMeFile={app}\{#ReadMeFile}
AppCopyright={#Copyright} ({#WebAddress})
AppComments=
AppContact=
DefaultDirName={pf}\{#AppPublisher}\{#AppName}
DefaultGroupName={#AppPublisher} {#AppName}
AllowNoIcons=true
LicenseFile={#SrcDocsPath}{#LicenseFile}
Compression=lzma/ultra
SolidCompression=true
InternalCompressLevel=ultra
OutputDir={#OutDir}
OutputBaseFilename={#AppName}-Setup-{#AppVersion}
VersionInfoVersion={#AppVersion}
VersionInfoCompany={#Company}
VersionInfoDescription=Installer for {#AppName}
VersionInfoTextVersion=Release {#AppVersion}
VersionInfoCopyright={#Copyright}
MinVersion=0,4.0.1381sp6
TimeStampsInUTC=true
ShowLanguageDialog=yes
UninstallFilesDir={app}\{#InstUninstDir}
UninstallDisplayIcon={app}\{#ExeFile}
PrivilegesRequired=admin

[Dirs]
Name: {commonappdata}\{#AppPublisher}\{#AppName}; permissions: everyone-modify;
Name: {commonappdata}\{#AppPublisher}\{#AppName}\Data; permissions: everyone-modify;

[Files]
Source: {#SrcExePath}{#SetupHelper}; Flags: dontcopy
Source: {#SrcExePath}{#ExeFile}; DestDir: {app}
Source: {#SrcExePath}{#HelpFile}; DestDir: {app}; Flags: ignoreversion
Source: {#SrcDocsPath}{#LicenseTextFile}; DestDir: {app}; Flags: ignoreversion
Source: {#SrcDocsPath}{#ReadMeFile}; DestDir: {app}; Flags: ignoreversion
Source: {#SrcDocsPath}{#PrivacyFile}; DestDir: {app}; Flags: ignoreversion

[Icons]
Name: {group}\{#AppPublisher} {#AppName}; Filename: {app}\{#ExeFile}
Name: {group}\{cm:UninstallProgram,{#AppPublisher} {#AppName}}; Filename: {uninstallexe}

[Run]
Filename: {app}\{#ReadMeFile}; Description: "View the README file"; Flags: nowait postinstall skipifsilent shellexec
Filename: {app}\{#ExeFile}; Description: {cm:LaunchProgram,{#AppPublisher} {#AppName}}; Flags: nowait postinstall skipifsilent

[Messages]
BeveledLabel={#Company}

[UninstallDelete]
; Deletes CodeSnip app config and data files (per-user data file is left in place)
Type: filesandordirs; Name: "{commonappdata}\{#AppPublisher}\{#AppName}"

[Code]
// DataLocations.pas must be declared first
#include "DataLocations.ps"
#include "UpdateIni.ps"
#include "UpdateDBase.ps"
#include "EventHandlers.ps"

