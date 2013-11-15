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
  Favourites.UFavourites, UExceptions;


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
      Watermark = #$25BA + ' CodeSnip Favourites v1 ' + #$25C4;
  strict private
    ///  <summary>Returns fully specified name of the favourites file.</summary>
    class function FavouritesFileName: string; static;
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
  SysUtils, IOUtils, Classes,
  /// Project
  DB.UMain, UAppInfo, UConsts, UIOUtils, UIStringList, USnippetIDs, UStrUtils;


{ TFavouritesPersist }

class function TFavouritesPersist.FavouritesFileName: string;
begin
  Result := TAppInfo.FavouritesFileName;
end;

class procedure TFavouritesPersist.Load(Favourites: TFavourites);
var
  Lines: IStringList;
  Line: string;
  Fields: IStringList;
  SnippetName: string;
  LastAccess: TDateTime;
resourcestring
  sBadFormat = 'Invalid favourites file format';
begin
  if not TFile.Exists(FavouritesFileName) then
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
  if Line <> Watermark then
    raise EFavouritesPersist.Create(sBadFormat);
  Lines.Delete(0);
  for Line in Lines do
  begin
    if StrIsBlank(Line) then
      Continue;
    Fields := TIStringList.Create(Line, TAB, False, True);
    if Fields.Count <> 3 then
      raise EFavouritesPersist.Create(sBadFormat);
    SnippetName := Fields[0];
    if not TryStrToDateTime(Fields[2], LastAccess) then
      raise EFavouritesPersist.Create(sBadFormat);
    // only add to favourites if snippet in database
    if Database.Snippets.Find(SnippetName) <> nil then
      Favourites.Add(TSnippetID.Create(SnippetName), LastAccess);
  end;
end;

class procedure TFavouritesPersist.Save(Favourites: TFavourites);
var
  SB: TStringBuilder;
  Fav: TFavourite;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine(Watermark);
    for Fav in Favourites do
    begin
      SB.Append(Fav.SnippetID.ToString);
      SB.Append(TAB);
      SB.Append(BoolToStr(True, True)); // redundant field: always True in v5
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

