; This Source Code Form is subject to the terms of the Mozilla Public License,
; v. 2.0. If a copy of the MPL was not distributed with this file, You can
; obtain one at http://mozilla.org/MPL/2.0/
;
; Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
;
; Install file generation script for use with Inno Setup.


; The following defines use these macros that are predefined by ISPP:
;   SourcePath - path where this script is located
;   GetStringFileInfo - gets requested version info string from an executable
;   GetFileProductVersion - gets product version info string from an executable

; Deletes "Release " from beginning of string S
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
#define LicenseTextFile "License.html"
#define PrivacyFile "Privacy.txt"
#define OutDir SourcePath + "..\..\Exe"
#define SrcDocsPath SourcePath + "..\..\Docs\"
#define SrcAssetsPath SourcePath + '\Assets\"
#define SrcExePath SourcePath + "..\..\Exe\"
#define ProgDataSubDir AppName + ".4"
#define ExeProg SrcExePath + ExeFile
#define AppVersion DeleteToVerStart(GetFileProductVersion(ExeProg))
#define Copyright GetStringFileInfo(ExeProg, LEGAL_COPYRIGHT)
#define Company "DelphiDabbler.com"
#define WebAddress "www.delphidabbler.com"
#define WebURL "http://" + WebAddress + "/"
#define AppURL WebURL + "software/codesnip"
#define InstUninstDir "Uninst"

; Creates name of setup file from app name, version and any special build string
#define CreateSetupName(str fn) \
  Local[0] = GetStringFileInfo(fn, "SpecialBuild"), \
  Local[1] = (Local[0] == "") ? "" : "-" + Local[0], \
  AppName + "-Setup-" + AppVersion + Local[1]

#define SetupName CreateSetupName(ExeProg)

[Setup]
AppID={{578D0FFE-5873-4457-95F2-AAB38F52B622}
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
DefaultDirName={pf}\{#AppPublisher}\{#AppName}-4
DefaultGroupName={#AppPublisher} {#AppName} v4
AllowNoIcons=true
LicenseFile={#SrcAssetsPath}{#LicenseFile}
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
Name: {commonappdata}\{#AppPublisher}\{#ProgDataSubDir}; permissions: everyone-modify;
Name: {commonappdata}\{#AppPublisher}\{#ProgDataSubDir}\Database; permissions: everyone-modify;

[Files]
Source: {#SrcExePath}{#ExeFile}; DestDir: {app}
Source: {#SrcExePath}{#HelpFile}; DestDir: {app}; Flags: ignoreversion
Source: {#SrcDocsPath}{#LicenseTextFile}; DestDir: {app}; Flags: ignoreversion
Source: {#SrcDocsPath}{#ReadMeFile}; DestDir: {app}; Flags: ignoreversion
Source: {#SrcDocsPath}{#PrivacyFile}; DestDir: {app}; Flags: ignoreversion
Source: {#SrcAssetsPath}UpdatingPreview.rtf; Flags: dontcopy
Source: {#SrcAssetsPath}AutoUpdateChecks.rtf; Flags: dontcopy

[Icons]
Name: {group}\{#AppPublisher} {#AppName}; Filename: {app}\{#ExeFile}
Name: {group}\{cm:UninstallProgram,{#AppPublisher} {#AppName}}; Filename: {uninstallexe}

[Run]
Filename: {app}\{#ReadMeFile}; Description: "View the README file"; Flags: nowait postinstall skipifsilent shellexec
Filename: {app}\{#ExeFile}; Description: {cm:LaunchProgram,{#AppPublisher} {#AppName}}; Flags: nowait postinstall skipifsilent

[Messages]
BeveledLabel={#Company}

[UninstallDelete]
; Deletes CodeSnip common app config and data files (per-user data file is left in place)
Type: filesandordirs; Name: "{commonappdata}\{#AppPublisher}\{#ProgDataSubDir}"

[Code]
// DataLocations.ps must be declared first
#include "DataLocations.ps"
#include "VersionInfo.ps" 
#include "Unicode.ps"
#include "UpdateIni.ps"
#include "UpdateDBase.ps"
#include "EventHandlers.ps"

