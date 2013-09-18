{
 * UAppInfo.pas
 *
 * Class that provides information about the application.
 *
 * $Rev$
 * $Date$
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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
    class function AppExeFilePath: string;
      {Returns fully specified name of program's executable file.
        @return Name of file.
      }
    class function AppExeDir: string;
      {Returns the directory of CodeSnip's executable files.
        @return Full path to executable files directory.
      }
    class function HelpFileName: string;
      {Returns fully specified name of CodeSnip's help file.
        @return Name of help file.
      }
    class function ProgramReleaseInfo: string;
      {Gets information about the current program release. Includes any special
      build information if present in version information.
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
  SysUtils,
  // DelphiDabbler library
  PJMD5,
  // Project
  USettings, USystemID, USystemInfo, UVersionInfo;


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
  Result := ExtractFileDir(AppExeFilePath);
end;

class function TAppInfo.AppExeFilePath: string;
  {Returns fully specified name of program's executable file.
    @return Name of file.
  }
begin
  Result := ParamStr(0);
end;

class function TAppInfo.CommonAppDir: string;
  {Gets the CodeSnip data directory stored within the common application data
  directory.
    @return Full path to common application data directory.
  }
begin
  Result := TSystemFolders.CommonAppData + '\DelphiDabbler\CodeSnip';
end;

class function TAppInfo.GenerateKey: string;
  {Generates unique program key for application in deterministic way.
    @return Required key.
  }
begin
  Result := UpperCase(
    TPJMD5.Calculate(
      USystemID.SystemIDStr, TEncoding.ASCII
    )
  );
end;

class function TAppInfo.HelpFileName: string;
  {Returns fully specified name of CodeSnip's help file.
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
  {Gets information about the current program release. Includes any special
  build information if present in version information.
    @return Release information.
  }
begin
  Result := Trim(TVersionInfo.ProductVersionStr);
  if Trim(TVersionInfo.SpecialBuildStr) <> '' then
    Result := Result + '-' + Trim(TVersionInfo.SpecialBuildStr);   
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
  Result := TSystemFolders.PerUserAppData + '\DelphiDabbler\CodeSnip';
end;

class function TAppInfo.UserDataDir: string;
  {Returns the directory where CodeSnip stores the user's "database" files.
    @return Full path to database sub directory.
  }
begin
  Result := UserAppDir + '\UserData.3';
end;

end.

