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
  SysUtils,
  IOUtils,
  Classes,
  /// Project
  DB.UCollections,
  DB.UMain,
  UAppInfo,
  UConsts,
  UIOUtils,
  UIStringList,
  USnippetIDs,
  UStrUtils;


{ TFavouritesPersist }

class function TFavouritesPersist.FavouritesFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(TAppInfo.UserAppDir)
    + 'Favourites';
end;

class procedure TFavouritesPersist.Load(Favourites: TFavourites);
var
  Lines: IStringList;
  Line: string;
  Fields: IStringList;
  SnippetName: string;
  UserDef: Boolean;
  LastAccess: TDateTime;
resourcestring
  sBadFormat = 'Invalid favourites file format';
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
  if Line <> Watermark then
    raise EFavouritesPersist.Create(sBadFormat);
  Lines.Delete(0);
  for Line in Lines do
  begin
    if StrTrim(Line) = '' then
      Continue;
    Fields := TIStringList.Create(Line, TAB, False, True);
    if Fields.Count <> 3 then
      raise EFavouritesPersist.Create(sBadFormat);
    SnippetName := Fields[0];
    UserDef := True; // accept any text as true excpet "false"
    if StrSameText(Fields[1], 'false') then
      UserDef := False;
    if not TryStrToDateTime(Fields[2], LastAccess) then
      raise EFavouritesPersist.Create(sBadFormat);
    // only add to favourites if snippet in database
    if Database.Snippets.Find(SnippetName, UserDef) <> nil then
//      Favourites.Add(TSnippetID.Create(SnippetName, UserDef), LastAccess);
      Favourites.Add(TSnippetID.Create(SnippetName, TCollectionID.__TMP__DBCollectionID(UserDef)), LastAccess);
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
      SB.Append(Fav.SnippetID.Name);
      SB.Append(TAB);
//      SB.Append(BoolToStr(Fav.SnippetID.UserDefined, True));
      SB.Append(BoolToStr(Fav.SnippetID.CollectionID <> TCollectionID.__TMP__MainDBCollectionID, True));
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

