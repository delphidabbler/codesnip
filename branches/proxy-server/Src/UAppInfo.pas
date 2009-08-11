{
 * UAppInfo.pas
 *
 * Class that provides information about the application.
 *
 * v0.1 of 30 Jan 2005  - Original version that accessed config file directly.
 * v0.2 of 18 Feb 2005  - Deleted unused TAppLocations.GeneratorDir method.
 *                      - Deleted unused TAppInfo constructor, destructor and
 *                        fFindHistory field.
 * v0.3 of 21 Apr 2005  - Total rewrite of TAppInfo object that accesses
 *                        sections via identifiers rather than names, implements
 *                        subsection names and provides direct access to data
 *                        items within sections and to whole sections via new
 *                        object that encapsulates them and their data. All
 *                        section and data specific methods were deleted.
 *                        IniFileName method renamed as StorageFileName.
 *                      - Added new IAppSectionInfo interface to the new section
 *                        encapsulation object.
 * v0.4 of 25 Apr 2005  - Added new Source Output storage section.
 * v0.5 of 08 Jan 2006  - Added new ItemExists method to IAppSectionInfo and its
 *                        implememtaion.
 * v0.6 of 04 Apr 2006  - Major revision:
 *                        - Removed AppInfo object and associated code to new
 *                          USettings unit and renamed and revised there.
 *                        - Renamed TAppLocations to TAppInfo. This class will
 *                          be extended at a later stage to provide more than
 *                          application locations.
 * v0.7 of 07 Apr 2006  - Several additions were made to TAppInfo:
 *                        - Added support for setting and querying application
 *                          registration.
 *                        - Added support for generating and storing unique key
 *                          for application.
 *                        - Added support for returning various pieces of
 *                          version information about the application.
 * v1.0 of 28 May 2006  - Improved and corrected comments.
 *                      - Changed methods that created instance of TVersionInfo
 *                        to call new static functions instead.
 *                      - Made TAppInfo.RegistrationCode private.
 *                      - Added new TAppInfo.AppExeDir and TAppInfo.AppExeFile
 *                        methods.
 * v1.1 of 26 Oct 2006  - Added new TAppInfo.LicenseFileName method.
 * v1.2 of 11 Nov 2006  - Added new TAppInfo.HelpFileName method to return path
 *                        to help file.
 * v1.3 of 14 Nov 2006  - Removed now unused TAppInfo.LicenseFileName method.
 * v1.4 of 11 Feb 2007  - Added new TAppInfo.ContribFileName method to return
 *                        path to contributors file.
 * v1.5 of 22 Sep 2007  - Added TAppInfo.TestersFileName method to return path
 *                        to testers file.
 * v1.6 of 13 Aug 2008  - Renamed TAppDir.AppDir as TAppDir.UserAppDir.
 *                      - Added new TAppDir.CommonAppDir class method.
 * v1.7 of 24 Aug 2008  - Changed TAppInfo.GenerateKey to use SystemIDStr to
 *                        build ID string rather using previous method that used
 *                        MAC Address since MAC address code doesn't work on
 *                        Windows Vista.
 *                      - Removed obsolete TAppInfo methods: ContribFileName,
 *                        TestersFileName and MasterFileName.
 * v1.8 of 26 Aug 2008  - Added new TAppInfo.UserDataDir method that returns
 *                        directory where user database is stored.
 * v1.9 of 04 Oct 2008  - Changed TAppInfo to derive from TNoConstructObject and
 *                        hence prevented it from being constructed.
 *                      - Made private section strict.
 * v1.10 of 11 Jan 2009 - Replaced direct call to MD5 routines with call to
 *                        TCheckSum static class.
 * v1.11 of 13 May 2009 - Added new class consts to TAppInfo: CompanyName,
 *                        ProgramName, FullProgramName and ProgamID. These
 *                        value were previosly provided by the UGlobals unit.
 * v1.12 of 23 May 2009 - Changed user database sub-directory to UserData.3 from
 *                        UserData to prevent making data in UserData unreadable
 *                        by CodeSnip versions before release 3. 
 *
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UAppInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UAppInfo;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TAppInfo:
    Static class that provides information about the application.
  }
  TAppInfo = class(TNoConstructObject)
  strict private
    class function GenerateKey: string;
      {Generates unique program key for application in deterministic way.
        @return Required key.
      }
    class function RegistrationCode: string;
      {Reads program's registration code from persistent storage.
        @return Registration code or '' if not registered.
      }
  public
    const CompanyName = 'DelphiDabbler';
      {Name of "company" that owns this program}
    const ProgramName = 'CodeSnip';
      {Name of program}
    const FullProgramName = CompanyName + ' ' + ProgramName;
      {Full name of program, including company name}
    const ProgramID = 'codesnip';
      {Machine readable identifier of program}
    class function UserAppDir: string;
      {Gets the CodeSnip data directory stored within the user's application
      data directory.
        @return Full path to per-user application data directory.
      }
    class function CommonAppDir: string;
      {Gets the CodeSnip data directory stored within the common application
      data directory.
        @return Full path to common application data directory.
      }
    class function AppDataDir: string;
      {Returns the directory where CodeSnip stores the "database" files.
        @return Full path to database sub directory.
      }
    class function UserDataDir: string;
      {Returns the directory where CodeSnip stores the user's "database" files.
        @return Full path to database sub directory.
      }
    class function AppExeDir: string;
      {Returns the directory of CodeSnip's executable files.
        @return Full path to executable files directory.
      }
    class function AppExeFile: string;
      {Returns name of CodeSnip's executable file. Adapts if file has been
      renamed since compilation.
        @return Name of executable file.
      }
    class function HelpFileName: string;
      {Returns name of CodeSnip's help file.
        @return Name of help file.
      }
    class function ProgramReleaseInfo: string;
      {Gets information about the current program release.
        @return Release information.
      }
    class function ProgramReleaseVersion: string;
      {Gets current version number of current release of program.
        @return Version number as dotted quad.
      }
    class function ProgramFileVersion: string;
      {Gets version number of program's executable file.
        @return Version number as dotted quad.
      }
    class function ProgramCopyright: string;
      {Gets program's copyright information.
        @return Copyright details.
      }
    class function ProgramKey: string;
      {Gets program's unique identifying key. This key should be different on
      each installation. If key does not exist it is created.
        @return 32 digit key.
      }
    class procedure RegisterProgram(const Code, Name: string);
      {Registers program by storing registration code and user name in
      persistent storage.
        @param Code [in] Registration code.
        @param Name [in] Registered user name.
      }
    class function IsRegistered: Boolean;
      {Checks if program is registered.
        @return True if program registered.
      }
    class function RegisteredUser: string;
      {Reads name of registered user from persistent storage.
        @return User name or '' if not registered.
      }
  end;


implementation


uses
  // Delphi
  SHFolder, SysUtils,
  // Project
  UCheckSum, USettings, USystemID, UVersionInfo, UUtils;


{ TAppInfo }

class function TAppInfo.AppDataDir: string;
  {Returns the directory where CodeSnip stores the "database" files.
    @return Full path to database sub directory.
  }
begin
  Result := CommonAppDir + '\Data';                        
end;

class function TAppInfo.AppExeDir: string;
  {Returns the directory of CodeSnip's executable files.
    @return Full path to executable files directory.
  }
begin
  Result := ExtractFileDir(ParamStr(0));
end;

class function TAppInfo.AppExeFile: string;
  {Returns name of CodeSnip's executable file. Adapts if file has been renamed
  since compilation.
    @return Name of executable file.
  }
begin
  Result := ExtractFileName(ParamStr(0));
end;

class function TAppInfo.CommonAppDir: string;
  {Gets the CodeSnip data directory stored within the common application data
  directory.
    @return Full path to common application data directory.
  }
begin
  Result := SpecialFolderPath(CSIDL_COMMON_APPDATA)
    + '\DelphiDabbler\CodeSnip';
end;

class function TAppInfo.GenerateKey: string;
  {Generates unique program key for application in deterministic way.
    @return Required key.
  }
begin
  Result := UpperCase(TCheckSum.Calculate(USystemID.SystemIDStr));
end;

class function TAppInfo.HelpFileName: string;
  {Returns name of CodeSnip's help file.
    @return Name of help file.
  }
begin
  Result := AppExeDir + '\CodeSnip.chm';                  
end;

class function TAppInfo.IsRegistered: Boolean;
  {Checks if program is registered.
    @return True if program registered.
  }
begin
  Result := RegistrationCode <> '';
end;

class function TAppInfo.ProgramCopyright: string;
  {Gets program's copyright information.
    @return Copyright details.
  }
begin
  Result := TVersionInfo.LegalCopyrightStr;
end;

class function TAppInfo.ProgramFileVersion: string;
  {Gets version number of program's executable file.
    @return Version number as dotted quad.
  }
begin
  Result := TVersionInfo.FileVersionNumberStr;
end;

class function TAppInfo.ProgramKey: string;
  {Gets program's unique identifying key. This key should be different on each
  installation. If key does not exist it is created.
    @return 32 digit key.
  }
var
  Section: ISettingsSection;  // persistent storage where key is recorded
begin
  // Try to get key from storage
  Section := Settings.ReadSection(ssApplication);
  Result := Section.ItemValues['Key'];
  if Result = '' then
  begin
    // Key not present: create and store it
    Result := GenerateKey;
    Section.ItemValues['Key'] := Result;                  
    Section.Save;
  end;
end;

class function TAppInfo.ProgramReleaseInfo: string;
  {Gets information about the current program release.
    @return Release information.
  }
begin
  Result := TVersionInfo.ProductVersionStr;
end;

class function TAppInfo.ProgramReleaseVersion: string;
  {Gets current version number of current release of program.
    @return Version number as dotted quad.
  }
begin
  Result := TVersionInfo.ProductVersionNumberStr;
end;

class function TAppInfo.RegisteredUser: string;
  {Reads name of registered user from persistent storage.
    @return User name or '' if not registered.
  }
var
  Section: ISettingsSection;  // persistent storage where name is recorded
begin
  Section := Settings.ReadSection(ssApplication);
  Result := Section.ItemValues['RegName'];
end;

class procedure TAppInfo.RegisterProgram(const Code, Name: string);
  {Registers program by storing registration code and user name in persistent
  storage.
    @param Code [in] Registration code.
    @param Name [in] Registered user name.
  }
var
  Section: ISettingsSection;  // persistent storage where code is to be recorded
begin
  Section := Settings.ReadSection(ssApplication);
  Section.ItemValues['RegCode'] := Code;
  Section.ItemValues['RegName'] := Name;                   
  Section.Save;
end;

class function TAppInfo.RegistrationCode: string;
  {Reads program's registration code from persistent storage.
    @return Registration code or '' if not registered.
  }
var
  Section: ISettingsSection;  // persistent storage where code is recorded
begin
  Section := Settings.ReadSection(ssApplication);
  Result := Section.ItemValues['RegCode'];
end;

class function TAppInfo.UserAppDir: string;
  {Gets the CodeSnip data directory stored within the user's application data
  directory.
    @return Full path to per-user application data directory.
  }
begin
  Result := SpecialFolderPath(CSIDL_APPDATA)
    + '\DelphiDabbler\CodeSnip';
end;

class function TAppInfo.UserDataDir: string;
  {Returns the directory where CodeSnip stores the user's "database" files.
    @return Full path to database sub directory.
  }
begin
  Result := UserAppDir + '\UserData.3';
end;

end.

