; CodeSnip.iss
;                    
; Install file generation script for use with Inno Setup.
;
; $Rev$
; $Date$
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
; Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
; Johnson. All Rights Reserved.
;
; Contributors:
;    NONE
;
; ***** END LICENSE BLOCK *****

; The following defines use these macros that are predefined by ISPP:
;   SourcePath - path where this script is located
;   GetStringFileInfo - gets requested version info string from an executable
;   GetFileProductVersion - gets product version info string from an executable

; Deletes "Release " from beginning of S
#define DeleteToVerStart(str S) \
  /* assumes S begins with "Release " followed by version as x.x.x */ \
  Local[0] = Copy(S, Len("Release ") + 1, 99), \
  Local[0]
  
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

; Creates name of setup file from app name, version and any special build string
#define CreateSetupName(str fn) \
  Local[0] = GetStringFileInfo(fn, "SpecialBuild"), \
  Local[1] = (Local[0] == "") ? "" : "-" + Local[0], \
  AppName + "-Setup-" + AppVersion + Local[1]

#define SetupName CreateSetupName(ExeProg)

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
OutputBaseFilename={#SetupName}
VersionInfoVersion={#AppVersion}
VersionInfoCompany={#Company}
VersionInfoDescription=Installer for {#AppName}
VersionInfoTextVersion=Release {#AppVersion}
VersionInfoCopyright={#Copyright}
MinVersion=0,5.0.2195
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

