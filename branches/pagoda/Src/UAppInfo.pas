{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2015, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
    ///  <summary>Returns the fully specified path to the given file in
    ///  CodeSnip's per-user data directory.</summary>
    class function UserFilePath(const FileName: string): string; inline;
  public
    const CompanyName = 'DelphiDabbler';
      {Name of "company" that owns this program}
    const ProgramName = 'CodeSnip';
      {Name of program}
    const FullProgramName = CompanyName + ' ' + ProgramName;
      {Full name of program, including company name}
  public
    class function ProgramCaption: string;
      {Returns caption to use in main window and task bar}
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
    ///  <summary>Returns fully specified path to CodeSnip's common
    ///  configuration file.</summary>
    class function CommonConfigFileName: string;
    ///  <summary>Returns fully specified path to CodeSnip's per-user
    ///  configuration file.</summary>
    class function UserConfigFileName: string;
    ///  <summary>Returns fully specified path to current user's favourites
    ///  file.</summary>
    class function FavouritesFileName: string;
    ///  <summary>Returns fully specified path to current user's highlighter
    ///  themes file.</summary>
    class function HiliteThemesFileName: string;
    ///  <summary>Returns fully specified path to current user's source code
    ///  languages file.</summary>
    class function SourceCodeLanguagesFileName: string;
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
  CS.Init.CommandLineOpts,
  USettings,
  UStrUtils,
  USystemID,
  USystemInfo,
  UVersionInfo,
  Web.UInfo;


{ TAppInfo }

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
var
  Section: ISettingsSection;
begin
  if TCommandLineOpts.IsPortable then
    Exit;
  Section := Settings.ReadSection(ssDatabase);
  if StrSameText(ExcludeTrailingPathDelimiter(NewDir), DefaultUserDataDir) then
    Section.DeleteItem('Path')
  else
    Section.SetString('Path', NewDir);
  Section.Save;
end;

class function TAppInfo.CommonAppDir: string;
  {Gets the CodeSnip data directory stored within the common application data
  directory.
    @return Full path to common application data directory.
  }
begin
  Result := StrIf(
    TCommandLineOpts.IsPortable,
    AppExeDir + '\AppData.5',
    TSystemFolders.CommonAppData + '\DelphiDabbler\CodeSnip.5'
  );
end;

class function TAppInfo.CommonConfigFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(CommonAppDir) + 'Common.config';
end;

class function TAppInfo.DefaultUserDataDir: string;
  {Returns the default directory where CodeSnip stores the uer's "database"
  files.
    @return Full path to required directory.
  }
begin
  Result := UserAppDir + '\Database';
end;

class function TAppInfo.FavouritesFileName: string;
begin
  Result := UserFilePath('Favourites');
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
  if TCommandLineOpts.IsPortable then
    Result := 'P:' + StrSliceRight(Result, Length(Result) - 2);
end;

class function TAppInfo.HelpFileName: string;
  {Returns fully specified name of CodeSnip's help file.
    @return Name of help file.
  }
begin
  Result := AppExeDir + '\CodeSnip.chm';
end;

class function TAppInfo.HiliteThemesFileName: string;
begin
  Result := UserFilePath('HighlighterThemes');
end;

class function TAppInfo.ProgramCaption: string;
  {Returns caption to use in main window and task bar}
const
  { TODO -cPRERELEASE: Remove text in chevrons from ProgramCaption before
                       release }
  CaptionBase = 'CodeSnip 5 <<<Pagoda - dev>>>';
resourcestring
  sPortableMode = '(Portable mode)';
begin
  Result := CaptionBase;
  if TCommandLineOpts.IsPortable then
    Result := Result + ' ' + sPortableMode;
  if TCommandLineOpts.UseLocalHost then
    Result := Result + Format(' [%s]', [TWebInfo.LocalHost]);
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
  if not StrIsBlank(TVersionInfo.SpecialBuildStr) then
    Result := Result + '-' + StrTrim(TVersionInfo.SpecialBuildStr);
end;

class function TAppInfo.ProgramReleaseVersion: string;
  {Gets current version number of current release of program.
    @return Version number as dotted quad.
  }
begin
  Result := TVersionInfo.ProductVersionNumberStr;
end;

class function TAppInfo.SourceCodeLanguagesFileName: string;
begin
  Result := UserFilePath('SourceCodeLanguages');
end;

class function TAppInfo.UserAppDir: string;
  {Gets the CodeSnip data directory stored within the user's application data
  directory.
    @return Full path to per-user application data directory.
  }
begin
  Result := StrIf(
    TCommandLineOpts.IsPortable,
    CommonAppDir,
    TSystemFolders.PerUserAppData + '\DelphiDabbler\CodeSnip.5'
  );
end;

class function TAppInfo.UserConfigFileName: string;
begin
  Result := UserFilePath('User.config');
end;

class function TAppInfo.UserDataDir: string;
  {Returns the directory where CodeSnip stores the user's "database" files.
    @return Full path to database sub directory.
  }
var
  Section: ISettingsSection;  // persistent storage where code is recorded
begin
  if TCommandLineOpts.IsPortable then
    Result := DefaultUserDataDir
  else
  begin
    Section := Settings.ReadSection(ssDatabase);
    Result := Section.GetString('Path', DefaultUserDataDir);
  end;
end;

class function TAppInfo.UserFilePath(const FileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(UserAppDir) + FileName;
end;

end.

