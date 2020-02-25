{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that provides information about the application.
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
  public
    const CompanyName = 'DelphiDabbler';
      {Name of "company" that owns this program}
    {$IFNDEF PORTABLE}
    const ProgramName = 'CodeSnip';
    {$ELSE}
    const ProgramName = 'CodeSnip-p';
    {$ENDIF}
      {Name of program}
    {$IFNDEF PORTABLE}
    const ProgramCaption = 'CodeSnip 4';
    {$ELSE}
    const ProgramCaption = 'CodeSnip 4 (Portable Edition)';
    {$ENDIF}
      {Name of program displayed in main window and task bar caption}
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
    class function DefaultUserDataDir: string;
      {Returns the default directory where CodeSnip stores the uer's "database"
      files.
        @return Full path to required directory.
      }
    class procedure ChangeUserDataDir(const NewDir: string);
      {Changes directory where CodeSnip stores the user's "database" files.
        @param NewDir [in] New directory.
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
    class function AppConfigFileName: string;
      {Returns fully specified name of application config file.
      }
    class function UserConfigFileName: string;
      {Returns fully specified name of per-user config file.
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
    class function ProgramKey: string;
      {Gets program's unique identifying key. This key should be different on
      each installation. If key does not exist it is created.
        @return 32 digit key.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // DelphiDabbler library
  PJMD5,
  // Project
  USettings, UStrUtils, USystemID, USystemInfo, UVersionInfo;


{ TAppInfo }

class function TAppInfo.AppConfigFileName: string;
begin
  Result := CommonAppDir + '\Common.config';
end;

class function TAppInfo.AppDataDir: string;
  {Returns the directory where CodeSnip stores the "database" files.
    @return Full path to database sub directory.
  }
begin
  {$IFNDEF PORTABLE}
  Result := CommonAppDir + '\Database';
  {$ELSE}
  Result := CommonAppDir + '\CSDB';
  {$ENDIF}
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

class procedure TAppInfo.ChangeUserDataDir(const NewDir: string);
  {Changes directory where CodeSnip stores the user's "database" files.
    @param NewDir [in] New directory.
  }
{$IFNDEF PORTABLE}
var
  Section: ISettingsSection;
{$ENDIF}
begin
  {$IFNDEF PORTABLE}
  Section := Settings.ReadSection(ssDatabase);
  if StrSameText(ExcludeTrailingPathDelimiter(NewDir), DefaultUserDataDir) then
    Section.DeleteItem('UserDataDir')
  else
    Section.SetString('UserDataDir', NewDir);
  Section.Save;
  {$ENDIF}
end;

class function TAppInfo.CommonAppDir: string;
  {Gets the CodeSnip data directory stored within the common application data
  directory.
    @return Full path to common application data directory.
  }
begin
  {$IFNDEF PORTABLE}
  Result := TSystemFolders.CommonAppData + '\DelphiDabbler\CodeSnip.4';
  {$ELSE}
  Result := AppExeDir + '\AppData';
  {$ENDIF}
end;

class function TAppInfo.DefaultUserDataDir: string;
  {Returns the default directory where CodeSnip stores the uer's "database"
  files.
    @return Full path to required directory.
  }
begin
  {$IFNDEF PORTABLE}
  Result := UserAppDir + '\UserDatabase';
  {$ELSE}
  Result := UserAppDir + '\UserDB';
  {$ENDIF}
end;

class function TAppInfo.GenerateKey: string;
  {Generates unique program key for application in deterministic way.
    @return Required key.
  }
begin
  Result := StrToUpper(
    TPJMD5.Calculate(
      USystemID.SystemIDStr, TEncoding.ASCII
    )
  );
  {$IFDEF PORTABLE}
  Result := 'P:' + StrSliceRight(Result, Length(Result) - 2);
  {$ENDIF}
end;

class function TAppInfo.HelpFileName: string;
  {Returns fully specified name of CodeSnip's help file.
    @return Name of help file.
  }
begin
  Result := AppExeDir + '\CodeSnip.chm';
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
  Result := Section.GetString('Key');
  if Result = '' then
  begin
    // Key not present: create and store it
    Result := GenerateKey;
    Section.SetString('Key', Result);
    Section.Save;
  end;
end;

class function TAppInfo.ProgramReleaseInfo: string;
  {Gets information about the current program release. Includes any special
  build information if present in version information.
    @return Release information.
  }
begin
  Result := StrTrim(TVersionInfo.ProductVersionStr);
  if StrTrim(TVersionInfo.SpecialBuildStr) <> '' then
    Result := Result + '-' + StrTrim(TVersionInfo.SpecialBuildStr);
end;

class function TAppInfo.ProgramReleaseVersion: string;
  {Gets current version number of current release of program.
    @return Version number as dotted quad.
  }
begin
  Result := TVersionInfo.ProductVersionNumberStr;
end;

class function TAppInfo.UserAppDir: string;
  {Gets the CodeSnip data directory stored within the user's application data
  directory.
    @return Full path to per-user application data directory.
  }
begin
  {$IFNDEF PORTABLE}
  Result := TSystemFolders.PerUserAppData + '\DelphiDabbler\CodeSnip.4';
  {$ELSE}
  Result := CommonAppDir;
  {$ENDIF}
end;

class function TAppInfo.UserConfigFileName: string;
begin
  Result := UserAppDir + '\User.config';
end;

class function TAppInfo.UserDataDir: string;
  {Returns the directory where CodeSnip stores the user's "database" files.
    @return Full path to database sub directory.
  }
{$IFNDEF PORTABLE}
var
  Section: ISettingsSection;  // persistent storage where code is recorded
{$ENDIF}
begin
  {$IFNDEF PORTABLE}
  Section := Settings.ReadSection(ssDatabase);
  Result := Section.GetString('UserDataDir', DefaultUserDataDir);
  {$ELSE}
  Result := DefaultUserDataDir;
  {$ENDIF}
end;

end.

