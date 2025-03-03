{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines an advanced record that can persist a list of favourites to and from
 * a file on a per user basis.
}


unit Favourites.UPersist;


interface


uses
  // Delphi
  SysUtils,
  // Project
  Favourites.UFavourites,
  UExceptions;


type
  ///  <summary>Type of exception raised by TFavouritesPersist.</summary>
  EFavouritesPersist = class(ECodeSnip);

type
  ///  <summary>Persists a list of favourites to and from a file in the current
  ///  user's application data directory.</summary>
  TFavouritesPersist = record
  strict private
    const
      ///  <summary>Watermark that is present one the first line of a valid
      ///  favourites file.</summary>
      Watermark = #$25BA + ' CodeSnip Favourites v2 ' + #$25C4;
    ///  <summary>Returns date format settings to be used when writing and
    ///  reading the Favourites file.</summary>
    class function DateFormatSettings: TFormatSettings; static;
  public
    ///  <summary>Saves all favourites from given favourites list to file.
    ///  </summary>
    class procedure Save(Favourites: TFavourites); static;
    ///  <summary>Loads all favourites from file into given favourites list.
    ///  </summary>
    class procedure Load(Favourites: TFavourites); static;
  end;


implementation


uses
  // Delphi
  IOUtils,
  Classes,
  /// Project
  DB.UMain,
  DB.Vaults,
  UAppInfo,
  UConsts,
  UIOUtils,
  UIStringList,
  USnippetIDs,
  UStrUtils,
  UTabSeparatedFileIO;


{ TFavouritesPersist }

class function TFavouritesPersist.DateFormatSettings: TFormatSettings;
begin
  // We use YYYY-MM-DD HH:MM:SS date format in Favourites file
  Result := TFormatSettings.Create;
  Result.DateSeparator := '-';
  Result.TimeSeparator := ':';
  Result.ShortDateFormat := 'yyyy/mm/dd';
  Result.ShortTimeFormat := 'hh:nn:ss';
end;

class procedure TFavouritesPersist.Load(Favourites: TFavourites);
var
  TSVReader: TTabSeparatedReader;
resourcestring
  sBadFormat = 'Invalid favourites file format (v2)';
begin
  if not TFile.Exists(TAppInfo.UserFavouritesFileName, False) then
    Exit;

  try
    TSVReader := TTabSeparatedReader.Create(
      TAppInfo.UserFavouritesFileName, Watermark
    );
    try
      TSVReader.Read(
        procedure (AFields: TArray<string>)
        var
          Key: string;
          CollectionID: TVaultID;
          LastAccess: TDateTime;
        begin
          if Length(AFields) <> 3 then
            raise EFavouritesPersist.Create(sBadFormat);
          Key := StrTrim(AFields[0]);
          CollectionID := TVaultID.CreateFromHexString(
            StrTrim(AFields[1])
          );
          LastAccess := StrToDateTime(StrTrim(AFields[2]), DateFormatSettings);
          if Database.Snippets.Find(Key, CollectionID) <> nil then
            Favourites.Add(TSnippetID.Create(Key, CollectionID), LastAccess);
        end
      );
    finally
      TSVReader.Free;
    end;

  except
    on E: EConvertError do
      raise EFavouritesPersist.Create(E);
    on E: ETabSeparatedReader do
      raise EFavouritesPersist.Create(E);
    else
      raise;
  end;
end;

class procedure TFavouritesPersist.Save(Favourites: TFavourites);
var
  Fav: TFavourite;
  TSVWriter: TTabSeparatedFileWriter;
begin
  TDirectory.CreateDirectory(
    TPath.GetDirectoryName(TAppInfo.UserFavouritesFileName)
  );
  try
    TSVWriter := TTabSeparatedFileWriter.Create(
      TAppInfo.UserFavouritesFileName, Watermark
    );
    try
      for Fav in Favourites do
      begin
        TSVWriter.WriteLine(
          [
            Fav.SnippetID.Key,
            Fav.SnippetID.CollectionID.ToHexString,
            DateTimeToStr(Fav.LastAccessed, DateFormatSettings)
          ]
        );
      end;
    finally
      TSVWriter.Free;
    end;
  except
    on E: EStreamError do
      raise EFavouritesPersist.Create(E);
    else
      raise;
  end;
end;

end.

