unit CS.Database.Main;

interface

uses
  Generics.Collections,
  CS.Database.Snippets,
  CS.Database.Types,
  CS.Database.Core.Lookups,
  CS.Database.Core.SnippetsTable,
  CS.Database.IO.Native,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  UBaseObjects,
  UExceptions;

type
  TDatabase = class(TNoConstructObject)
  strict private
    class var
      fSnippetsTable: TDBSnippetsTable;
      fTagLookup: TDBLookup<TDBTag>;
      fLanguageLookup: TDBLookup<TSourceCodeLanguageID>;
      fLastModified: TUTCDateTime;
      fDirty: Boolean;
    class procedure FlagUpdate;
    class procedure Clear;
    class function DatabasePath: string;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Load;
    class procedure Save;
    class function NewSnippet: ISnippet;
    class procedure InsertSnippet(ASnippet: ISnippet);
    class procedure UpdateSnippet(ASnippet: ISnippet);
    class procedure DeleteSnippet(const ASnippetID: TDBSnippetID);
    class function SnippetExists(const ASnippetID: TDBSnippetID): Boolean;
    class function GetReadOnlySnippet(const ASnippetID: TDBSnippetID;
      const RequiredProps: TDBSnippetProps = []): IReadOnlySnippet;
    class function GetSnippet(const ASnippetID: TDBSnippetID): ISnippet;
    class function GetSnippetIDs(Filter: IDBFilter): IDBSnippetIDList;
    class function IsDirty: Boolean;
  end;


implementation

uses
  SysUtils,
  CS.Database.Exceptions;

{ TDatabase }

class procedure TDatabase.Clear;
begin
  fSnippetsTable.Clear;
end;

class constructor TDatabase.Create;
begin
  fSnippetsTable := TDBSnippetsTable.Create;
  fTagLookup := TDBLookup<TDBTag>.Create(
    TDBTag.TEqualityComparer.Create
  );
  fLanguageLookup := TDBLookup<TSourceCodeLanguageID>.Create(
    TSourceCodeLanguageID.TEqualityComparer.Create
  );
  fLastModified := TUTCDateTime.CreateNull;
  fDirty := False;
end;

class function TDatabase.DatabasePath: string;
begin
  // TODO -cPRERELEASE: Replace this database path one from TAppInfo
  Result := ExtractFilePath(ParamStr(0));
end;

class procedure TDatabase.DeleteSnippet(const ASnippetID: TDBSnippetID);
var
  OldSnippet: TDBSnippet;
  Tag: TDBTag;
begin
  OldSnippet := fSnippetsTable.Get(ASnippetID);
  for Tag in OldSnippet.GetTags do
    fTagLookup.Delete(Tag, ASnippetID);
  fLanguageLookup.Delete(OldSnippet.GetLanguageID, ASnippetID);
  fSnippetsTable.Delete(ASnippetID);
  FlagUpdate;
end;

class destructor TDatabase.Destroy;
begin
  fLanguageLookup.Free;
  fTagLookup.Free;
  fSnippetsTable.Free;
end;

class procedure TDatabase.FlagUpdate;
begin
  fLastModified := TUTCDateTime.Now.RoundToNearestSecond;
  fDirty := True;
end;

class function TDatabase.GetReadOnlySnippet(const ASnippetID: TDBSnippetID;
  const RequiredProps: TDBSnippetProps): IReadOnlySnippet;
var
  Row: TDBSnippet;
begin
  Row := fSnippetsTable.Get(ASnippetID);
  Result := Row.CopyPartial(RequiredProps);
end;

class function TDatabase.GetSnippet(const ASnippetID: TDBSnippetID): ISnippet;
var
  Row: TDBSnippet;
begin
  Row := fSnippetsTable.Get(ASnippetID);
  Result := Row.Copy;
end;

class function TDatabase.GetSnippetIDs(Filter: IDBFilter): IDBSnippetIDList;
var
  Snippet: TDBSnippet;
begin
  Result := TDBSnippetIDList.Create;
  for Snippet in fSnippetsTable do
  begin
    if Filter.Match(
      TPartialSnippet.Create(Snippet, Filter.RequiredProperties)
    ) then
      Result.Add(Snippet.GetID);
  end;
end;

class procedure TDatabase.InsertSnippet(ASnippet: ISnippet);
var
  DBSnippet: TDBSnippet;
  Tag: TDBTag;
begin
  DBSnippet := TDBSnippet.CreateFrom(ASnippet);
  fSnippetsTable.Add(DBSnippet);
  FlagUpdate;
  DBSnippet.SetModified(fLastModified);
  for Tag in ASnippet.Tags do
    fTagLookup.Add(Tag, ASnippet.ID);
  fLanguageLookup.Add(ASnippet.LanguageID, ASnippet.ID);
end;

class function TDatabase.IsDirty: Boolean;
begin
  Result := fDirty;
end;

class procedure TDatabase.Load;
begin
  Clear;
  // TODO: implement load process
  // update last modification date to that from file
  // clear and load snippets table
  // clear and rebuild lookups
  fDirty := False;
end;

class function TDatabase.NewSnippet: ISnippet;
begin
  Result := TSnippet.CreateNew;
end;

class procedure TDatabase.Save;
var
  Writer: TDBNativeWriter;
begin
  if not fDirty then
    Exit;
  // TODO: backup database here
  try
    Writer := TDBNativeWriter.Create(DatabasePath);
    try
      Writer.Save(fSnippetsTable, fLastModified);
    finally
      Writer.Free;
    end;
  except
    // TODO: restore database on exception here
    raise;
  end;
  fDirty := False;
end;

class function TDatabase.SnippetExists(const ASnippetID: TDBSnippetID): Boolean;
begin
  Result := fSnippetsTable.Contains(ASnippetID);
end;

class procedure TDatabase.UpdateSnippet(ASnippet: ISnippet);
var
  OldSnippet: TDBSnippet;
  UpdatedSnippet: TDBSnippet;
  Tag: TDBTag;
begin
  OldSnippet := fSnippetsTable.Get(ASnippet.ID);
  UpdatedSnippet := TDBSnippet.CreateFrom(ASnippet);
  // Update snippets table
  fSnippetsTable.Update(UpdatedSnippet);
  FlagUpdate;
  UpdatedSnippet.SetModified(fLastModified);
  // Update tags lookup
  for Tag in OldSnippet.GetTags do
    fTagLookup.Delete(Tag, ASnippet.ID);
  for Tag in NewSnippet.GetTags do
    fTagLookup.Add(Tag, ASnippet.ID);
  // Update languages lookup
  fLanguageLookup.Update(
    OldSnippet.GetLanguageID, UpdatedSnippet.GetLanguageID, ASnippet.ID
  );
end;

end.

