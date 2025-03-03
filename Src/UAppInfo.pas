{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2024, Peter Johnson (gravatar.com/delphidabbler).
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
  public
    const CompanyName = 'DelphiDabbler';
      {Name of "company" that owns this program}
    {$IFNDEF PORTABLE}
    const ProgramName = 'CodeSnip';
    {$ELSE}
    const ProgramName = 'CodeSnip-p';
    {$ENDIF}
      {Name of program}
    const FullProgramName = CompanyName + ' CodeSnip.Vault';
      {Full name of program, including company name}
    const ProgramID = 'codesnip';
      {TODO -cVault: Remove unused ProgramID const}
      {Machine readable identifier of program}
  public
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
      {TODO -cCollections: Remove AppDataDir method: used for "main" database}
      {Returns the directory where CodeSnip stores the "database" files.
        @return Full path to database sub directory.
      }

    ///  <summary>Returns the path where collections are recommended to be
    ///  stored for the current user.</summary>
    ///  <remarks>
    ///  <para>Collections each have their own sub-directory of this directory.
    ///  </para>
    ///  <para>The user may ignore this recommendation and install collections
    ///  anywhere they choose.</para>
    ///  </remarks>
    class function UserCollectionsDir: string;

    ///  <summary>Returns the path where the Default collection is recommended
    ///  to be stored for the current user.</summary>
    ///  <remarks>
    ///  <para>If the Default collection is not present then CodeSnip will
    ///  automatically create it in this directory.</para>
    ///  <para>The user may move the Default collection anywhere they choose.
    ///  </para>
    ///  </remarks>
    class function UserDefaultCollectionDir: string;

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

    ///  <summary>Returns fully specified name of the current user's global
    ///  categories file.</summary>
    class function UserCategoriesFileName: string;

    ///  <summary>Returns fully specified name of the current user's favourites
    ///  file.</summary>
    class function UserFavouritesFileName: string;

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
    class function ProgramCaption: string;
      {Gets the program caption to be displayed in main window,
        @return Required caption,
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.Vaults,
  USettings,
  UStrUtils,
  USystemInfo,
  UVersionInfo;


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

class function TAppInfo.CommonAppDir: string;
  {Gets the CodeSnip data directory stored within the common application data
  directory.
    @return Full path to common application data directory.
  }
begin
  {$IFNDEF PORTABLE}
  Result := TSystemFolders.CommonAppData + '\DelphiDabbler\CodeSnip.Vault';
  {$ELSE}
  Result := AppExeDir + '\AppData.Vault';
  {$ENDIF}
end;

class function TAppInfo.HelpFileName: string;
  {Returns fully specified name of CodeSnip's help file.
    @return Name of help file.
  }
begin
  Result := AppExeDir + '\CodeSnip.chm';
end;

class function TAppInfo.ProgramCaption: string;
var
  ProductVer: TVersionNumber;
begin
  ProductVer := TVersionInfo.ProductVerNum;
  Result := Format(
    'CodeSnip.Vault v%d.%d.%d', [ProductVer.V1, ProductVer.V2, ProductVer.V3]
  );
  {$IFDEF PORTABLE}
  Result := Result + ' (Portable Edition)'
  {$ENDIF}
end;

class function TAppInfo.ProgramFileVersion: string;
  {Gets version number of program's executable file.
    @return Version number as dotted quad.
  }
begin
  Result := TVersionInfo.FileVersionNumberStr;
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
  Result := TSystemFolders.PerUserAppData + '\DelphiDabbler\CodeSnip.Vault';
  {$ELSE}
  Result := CommonAppDir;
  {$ENDIF}
end;

class function TAppInfo.UserCategoriesFileName: string;
begin
  Result := UserAppDir + '\Categories';
end;

class function TAppInfo.UserCollectionsDir: string;
begin
  Result := UserAppDir + '\Collections';
end;

class function TAppInfo.UserConfigFileName: string;
begin
  Result := UserAppDir + '\User.config';
end;

class function TAppInfo.UserDefaultCollectionDir: string;
begin
  Result := UserCollectionsDir + '\Default';
end;

class function TAppInfo.UserFavouritesFileName: string;
begin
  Result := UserAppDir + '\Favourites';
end;

end.

