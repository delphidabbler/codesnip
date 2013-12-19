{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines an advanced record that can persist a list of favourites to and from
 * a file on a per user basis.
}


unit Favourites.UPersist;


interface


uses
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
      ///  <summary>Watermark that is present one the first line of a valid v1
      ///  of the favourites file.</summary>
      WatermarkV1 = #$25BA + ' CodeSnip Favourites v1 ' + #$25C4;
      ///  <summary>Watermark that is present one the first line of a valid v2
      ///  of the favourites file.</summary>
      WatermarkV2 = #$25BA + ' CodeSnip Favourites v2 ' + #$25C4;
  strict private
    ///  <summary>Returns fully specified name of the favourites file.</summary>
    class function FavouritesFileName: string; static;
  public
    ///  <summary>Saves all favourites from given favourites list to file.
    ///  </summary>
    ///  <remarks>Always saves the file in version 2 format.</remarks>
    class procedure Save(Favourites: TFavourites); static;
    ///  <summary>Loads all favourites from file into given favourites list.
    ///  </summary>
    ///  <remarks>Can read from files in either v1 or v2 format. The "user
    ///  defined" field of v1 files is ignored.</remarks>
    class procedure Load(Favourites: TFavourites); static;
  end;


implementation


uses
  // Delphi
  SysUtils,
  IOUtils,
  Classes,
  /// Project
  CS.Database.Types,
  UAppInfo,
  UConsts,
  UIOUtils,
  UIStringList,
  UStrUtils;


{ TFavouritesPersist }

class function TFavouritesPersist.FavouritesFileName: string;
begin
  Result := TAppInfo.FavouritesFileName;
end;

class procedure TFavouritesPersist.Load(Favourites: TFavourites);
resourcestring
  sBadFormat = 'Invalid favourites file format';
type
  TVersionNumber = 1..2;
var
  Lines: IStringList;
  Line: string;
  SnippetID: TSnippetID;
  LastAccess: TDateTime;
  Version: TVersionNumber;

  procedure ReadFieldsFromLine;
  const
    FieldCount: array[TVersionNumber] of Byte = (3, 2);
    IDFileIdx = 0;
    DateFieldIdx: array[TVersionNumber] of Byte = (2, 1);
  var
    Fields: IStringList;
  begin
    Fields := TIStringList.Create(Line, TAB, False, True);
    if Fields.Count <> FieldCount[Version] then
      raise EFavouritesPersist.Create(sBadFormat);
    SnippetID := TSnippetID.Create(Fields[IDFileIdx]);
    if not TryStrToDateTime(Fields[DateFieldIdx[Version]], LastAccess) then
      raise EFavouritesPersist.Create(sBadFormat);
  end;

begin
  if not TFile.Exists(FavouritesFileName, False) then
    Exit;
  try
    Lines := TIStringList.Create(
      TFileIO.ReadAllLines(FavouritesFileName, TEncoding.UTF8, True)
    );
  except
    on E: EStreamError do
      raise EFavouritesPersist.Create(E);
    on E: EIOUtils do
      raise EFavouritesPersist.Create(E);
    else
      raise;
  end;
  Line := Lines[0];
  if Line = WatermarkV2 then
    Version := 2
  else if Line = WatermarkV1 then
    Version := 1
  else
    raise EFavouritesPersist.Create(sBadFormat);
  Lines.Delete(0);
  for Line in Lines do
  begin
    if StrIsBlank(Line) then
      Continue;
    ReadFieldsFromLine;
    Favourites.Add(SnippetID, LastAccess);
  end;
end;

class procedure TFavouritesPersist.Save(Favourites: TFavourites);
var
  SB: TStringBuilder;
  Fav: TFavourite;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine(WatermarkV2); // always save favourites as v2
    for Fav in Favourites do
    begin
      SB.Append(Fav.SnippetID.ToString);
      SB.Append(TAB);
      SB.Append(DateTimeToStr(Fav.LastAccessed));
      SB.AppendLine;
    end;
    TDirectory.CreateDirectory(TPath.GetDirectoryName(FavouritesFileName));
    try
      TFileIO.WriteAllText(
        FavouritesFileName, SB.ToString, TEncoding.UTF8, True
      );
    except
      on E: EStreamError do
        raise EFavouritesPersist.Create(E);
      else
        raise;
    end;
  finally
    SB.Free;
  end;
end;

end.

