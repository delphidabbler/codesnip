{
 * UCodeImportMgr.pas
 *
 * Implements a static class that handles import of a codesnip export file into
 * the user-defined database.
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
 * The Original Code is UCodeImportMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCodeImportMgr;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UCodeImportExport, UExceptions, UIStringList;


type

  TImportInfo = record
  strict private
    fOrigName: string;
    fImportAsName: string;
    fSkip: Boolean;
  public
    constructor Create(const AOrigName, AImportAsName: string;
      const ASkip: Boolean = False);
    property OrigName: string read fOrigName;
    property ImportAsName: string read fImportAsName write fImportAsName;
    property Skip: Boolean read fSkip write fSkip;
  end;

  TImportInfoList = class(TList<TImportInfo>)
  public
    constructor Create;
    function FindByName(const Name: string; out ImportInfo: TImportInfo):
      Boolean;
    function IndexOfName(const Name: string): Integer;
  end;

  {
  TCodeImportMgr:
    Sealed class that manages import of a codesnip export file into the
    user-defined database. Designed for ease of interaction with a suitable UI.
  }
  TCodeImportMgr = class sealed(TObject)
  strict private
    var
      fSnippetInfoList: TSnippetInfoList; // todo: change to proper list
      fImportInfoList: TImportInfoList;
      fUserInfo: TUserInfo;
    procedure Initialise;
    procedure UpdateImportInfo;
    function DisallowedNames(const ExcludedName: string): IStringList;
    function GetUniqueSnippetName(const SnippetName: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Import(const FileName: string);
    procedure UpdateDatabase;
    property UserInfo: TUserInfo read fUserInfo;
    property ImportInfo: TImportInfoList read fImportInfoList;
  end;

  {
  ECodeImportMgr:
    Class of exception raised when import manager encounteres an expected error.
  }
  ECodeImportMgr = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils, Classes, Generics.Defaults,
  // Project
  DB.UMain, DB.USnippet, UActiveText, UIOUtils, USnippetIDs;


{ TCodeImportMgr }

constructor TCodeImportMgr.Create;
begin
  inherited Create;
  SetLength(fSnippetInfoList, 0);
  fImportInfoList := TImportInfoList.Create;
end;

destructor TCodeImportMgr.Destroy;
begin
  fImportInfoList.Free;
  SetLength(fSnippetInfoList, 0);
  inherited;
end;

function TCodeImportMgr.DisallowedNames(const ExcludedName: string):
  IStringList;
var
  Snippet: TSnippet;
  SnippetInfo: TSnippetInfo;
begin
  Result := TIStringList.Create;
  Result.CaseSensitive := False;
  for Snippet in Database.Snippets do
    if Snippet.UserDefined then
      Result.Add(Snippet.Name);
  for SnippetInfo in fSnippetInfoList do
    if not AnsiSameText(SnippetInfo.Name, ExcludedName) then
      Result.Add(SnippetInfo.Name);
end;

function TCodeImportMgr.GetUniqueSnippetName(
  const SnippetName: string): string;
var
  UsedNames: IStringList;
  Postfix: Cardinal;
begin
  UsedNames := DisallowedNames(SnippetName);
  if not UsedNames.Contains(SnippetName) then
    Exit(SnippetName);
  Postfix := 1;
  repeat
    Inc(PostFix);
    Result := SnippetName + IntToStr(PostFix);
  until not UsedNames.Contains(Result);
end;

procedure TCodeImportMgr.Import(const FileName: string);
var
  Data: TBytes;
begin
  Initialise;
  fImportInfoList.Clear;
  try
    Data := TFileIO.ReadAllBytes(FileName);
    TCodeImporter.ImportData(fUserInfo, fSnippetInfoList, Data);
  except
    on E: EStreamError do
      raise ECodeImportMgr.Create(E);
    on E: ECodeImporter do
      raise ECodeImportMgr.Create(E);
  end;
  UpdateImportInfo;
end;

procedure TCodeImportMgr.Initialise;
begin
  fUserInfo := TUserInfo.CreateNul;
end;

procedure TCodeImportMgr.UpdateDatabase;

  // ---------------------------------------------------------------------------
  procedure AdjustDependsList(const Depends: ISnippetIDList);
    {Adjusts a snippet's dependency list so that main database is searched for
    a required snippet if it is not in the user database.
      @param Depends [in] Dependency list to be adjusted.
    }
  var
    Idx: Integer;           // loops through dependencies
    SnippetID: TSnippetID;  // each snippet ID in dependency list
  begin
    // NOTE: The data file format does not record which database a required
    // snippet belongs to, so we first look in the user database and if it's
    // not there, we assume the main database
    for Idx := 0 to Pred(Depends.Count) do
    begin
      SnippetID := Depends[Idx];
      SnippetID.UserDefined :=
        Database.Snippets.Find(SnippetID.Name, True) <> nil;
      Depends[Idx] := SnippetID;
    end;
  end;

  function UserInfoActiveText: IActiveText;
    {Builds an active text representation of the contributing user's name and
    email address.
      @return Required active text representation.
    }
  resourcestring
    // user information text template
    sContributorText = 'Contributed by: %0:s <%1:s>';
  begin
    Assert(not UserInfo.IsNul, ClassName +
      '.UpdateUserDatabase:UserInfoActiveText: UserInfo is nul');
    Result := TActiveTextFactory.CreateActiveText;
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsOpen));
    Result.AddElem(
      TActiveTextFactory.CreateTextElem(
        Format(
          sContributorText, [UserInfo.Details.Name, UserInfo.Details.Email]
        )
      )
    );
    Result.AddElem(TActiveTextFactory.CreateActionElem(ekPara, fsClose));
  end;
  // ---------------------------------------------------------------------------

var
//  Idx: Integer;           // loops through all snippets
  Editor: IDatabaseEdit;  // object used to update user database
  Snippet: TSnippet;      // reference to existing snippets
  SnippetInfo: TSnippetInfo;
  ImportInfo: TImportInfo;
resourcestring
  sBadNameError = 'Can''t find snippet "%s" in import data';
begin
  Editor := Database as IDatabaseEdit;
  for SnippetInfo in fSnippetInfoList do
  begin
    if not fImportInfoList.FindByName(SnippetInfo.Name, ImportInfo) then
      raise EBug.CreateFmt(sBadNameError, [SnippetInfo.Name]);
    if ImportInfo.Skip then
      Continue;

    AdjustDependsList(SnippetInfo.Data.Refs.Depends);
    if not UserInfo.IsNul then
      SnippetInfo.Data.Props.Extra.Append(UserInfoActiveText);

    Snippet := Database.Snippets.Find(ImportInfo.ImportAsName, True);
    if Assigned(Snippet) then
      // snippet already exists: overwrite it
      Editor.UpdateSnippet(Snippet, SnippetInfo.Data)
    else
      // snippet is new: add to database
      Editor.AddSnippet(ImportInfo.ImportAsName, SnippetInfo.Data);
  end;
end;

procedure TCodeImportMgr.UpdateImportInfo;
var
  SnippetInfo: TSnippetInfo;
begin
  fImportInfoList.Clear;
  for SnippetInfo in fSnippetInfoList do
  begin
    fImportInfoList.Add(
      TImportInfo.Create(
        SnippetInfo.Name, GetUniqueSnippetName(SnippetInfo.Name)
      )
    );
  end;
end;

{ TImportInfo }

constructor TImportInfo.Create(const AOrigName, AImportAsName: string;
  const ASkip: Boolean);
begin
  fOrigName := AOrigName;
  fImportAsName := AImportAsName;
  fSkip := ASkip;
end;

{ TImportInfoList }

constructor TImportInfoList.Create;
begin
  { TODO: create a CompareNames class static function on TSnippetID to simply
          compare names with having to create a TSnippetID record }
  inherited Create(
    TDelegatedComparer<TImportInfo>.Create(
      function(const Left, Right: TImportInfo): Integer
      begin
        Result := TSnippetID.Create(Left.OrigName, True).Compare(
          TSnippetID.Create(Right.OrigName, True)
        );
      end
    )
  );
end;

function TImportInfoList.FindByName(const Name: string;
  out ImportInfo: TImportInfo): Boolean;
var
  Idx: Integer;
begin
  Idx := IndexOf(TImportInfo.Create(Name, ''));
  if Idx = -1 then
    Exit(False);
  ImportInfo := Items[Idx];
  Result := True;
end;

function TImportInfoList.IndexOfName(const Name: string): Integer;
begin
  Result := IndexOf(TImportInfo.Create(Name, ''));
end;

end.

