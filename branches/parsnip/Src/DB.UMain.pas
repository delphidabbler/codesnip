{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a singleton object and subsidiary classes that encapsulate the
 * snippets and tags in the snippets database.
}


unit DB.UMain;


interface


uses
  // Project
  CS.Database.SnippetsTable,
  CS.Database.Types,
  CS.Utils.Dates,
  DB.USnippet,
  UMultiCastEvents,
  USingleton;


type

  {
  TDatabaseChangeEventKind:
    Enumeration that specifies the different kind of change events triggered by
    the database.
  }
  TDatabaseChangeEventKind = (
    evChangeBegin,          // a change to the database is about to take place
    evChangeEnd,            // a change to the database has completed
    evSnippetAdded,         // a snippet has been added
    evBeforeSnippetDelete,  // a snippet is about to be deleted
    evSnippetDeleted,       // a snippet has been deleted
    evBeforeSnippetChange,  // a snippet is about to be changed
    evSnippetChanged        // a snippet's properties / references have changed
    // TODO: add suitable event kinds for changes to tags as required
  );

  {
  IDatabaseChangeEventInfo:
    Interface supported by objects passed to Database object's change event
    handler that provides information about a change event. Some properites
    are not defined for certain event types. Kind property always defined.
  }
  IDatabaseChangeEventInfo = interface(IInterface)
    ['{80DEE62F-DC23-4EE7-A0B1-5DE46F483CE1}']
    function GetKind: TDatabaseChangeEventKind;
      {Gets kind (type) of event.
        @return Event kind.
      }
    function GetInfo: TObject;
      {Gets additional information about event.
        @return Object that provides required information.
      }
    property Kind: TDatabaseChangeEventKind read GetKind;
      {Identifies kind (type) of an event. Always defined}
    property Info: TObject read GetInfo;
      {Provides additional information about the event. Actual type of object
      depends on Kind. May be nil}
  end;

  TDBFilterFn = reference to function (ASnippet: ISnippet): Boolean;

  TDatabase = class(TSingleton)
  strict private
    type
      {
      TEventInfo:
        Class that provides information about a change event.
      }
      TEventInfo = class(TInterfacedObject, IDatabaseChangeEventInfo)
      strict private
        fKind: TDatabaseChangeEventKind;  // Kind of event
        fInfo: TObject;                   // Extra info about event
      public
        constructor Create(const Kind: TDatabaseChangeEventKind;
          const Info: TObject = nil);
          {Constructor. Creates an event information object.
            @param Kind [in] Kind of event.
            @param Info [in] Reference to further information about the event.
               May be nil if event doesn't have additional information.
          }
        { IDatabaseChangeEventInfo methods }
        function GetKind: TDatabaseChangeEventKind;
          {Gets kind (type) of event.
            @return Event kind.
          }
        function GetInfo: TObject;
          {Gets additional information about event.
            @return Object that provides required information.
          }
      end;
  strict private
    class var
      fInstance: TDatabase;
    class function GetInstance: TDatabase; static;
  strict private
    var
      fSnippetsTable: TDBSnippetsTable;
      fAllTags: ITagSet;
      fLastModified: TUTCDateTime;
      fDirty: Boolean;
      fChangeEvents: TMulticastEvents;  // List of change event handlers
    procedure FlagUpdate;
    procedure TriggerSnippetChangeEvent(const Kind: TDatabaseChangeEventKind;
      const SnippetID: TSnippetID);
    procedure TriggerNullDataEvent(const Kind: TDatabaseChangeEventKind);
    procedure TriggerChangeEvent(const Kind: TDatabaseChangeEventKind;
      const Info: TObject = nil);
      {Triggers a change event. Notifies all registered listeners.
        @param Kind [in] Kind of event.
        @param Info [in] Reference to any further information for event. May be
          nil.
      }
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  protected
    { TODO: Remove these properties when _TDatabase no longer needs access to
            them }
    // NOTE: These properties have been made protected so that _TDatabase can
    //       directly access them using unit scope.
    //       DON'T make this section strict.
    property __SnippetsTable: TDBSnippetsTable read fSnippetsTable;
    property __AllTags: ITagSet read fAllTags;
    procedure __SetDirty(Dirty: Boolean);
    property __Updated: Boolean read fDirty write __SetDirty;
    procedure __TriggerEvent(const Kind: TDatabaseChangeEventKind;
      const Info: TObject = nil);
  public
    class property Instance: TDatabase read GetInstance;
  public
    procedure Load;
    procedure Save;
    function IsDirty: Boolean;
    function LookupEditableSnippet(const ASnippetID: TSnippetID):
      IEditableSnippet;
    function TryLookupEditableSnippet(const ASnippetID: TSnippetID;
      out ASnippet: IEditableSnippet): Boolean;
    function LookupSnippet(const ASnippetID: TSnippetID): ISnippet;
    function TryLookupSnippet(const ASnippetID: TSnippetID;
      out ASnippet: ISnippet): Boolean;
    function SelectSnippets(FilterFn: TDBFilterFn): ISnippetIDList;
    function GetAllSnippets: ISnippetIDList;
    function GetAllTags: ITagSet;
    function SnippetExists(const ASnippetID: TSnippetID): Boolean;
    function SnippetCount: Integer;
    function IsEmpty: Boolean;
    // Returns a list of IDs of all snippets that depend on the snippet with
    // the given ID.
    function GetDependentsOf(const ASnippetID: TSnippetID): ISnippetIDList;
    // Returns a list of IDs of all snippet that refer to (i.e. cross-reference)
    // the snippet with the given ID.
    function GetReferrersTo(const ASnippetID: TSnippetID): ISnippetIDList;

    function NewSnippet: IEditableSnippet;
    procedure AddSnippet(ASnippet: IEditableSnippet);
    procedure UpdateSnippet(ASnippet: IEditableSnippet);
    procedure DeleteSnippet(ASnippetID: TSnippetID);

    procedure AddChangeEventHandler(const Handler: TNotifyEventInfo);
      {Adds a change event handler to list of listeners.
        @param Handler [in] Event handler to be added.
      }
    procedure RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
      {Removes a change event handler from list of listeners.
        @param Handler [in] Handler to remove from list.
      }
  end;


function Database: TDatabase;


implementation


uses
  // Project
  CS.Database.IO.Factory,
  CS.Database.IO.Types,
  CS.Database.Snippets,
  CS.Database.Tags,
  UBox,
  UExceptions,
  UQuery,
  UStrUtils,
  UUniqueID;


function Database: TDatabase;
begin
  Result := TDatabase.Instance;
end;

{ TDatabase }

procedure TDatabase.AddChangeEventHandler(const Handler: TNotifyEventInfo);
begin
  fChangeEvents.AddHandler(Handler);
end;

procedure TDatabase.AddSnippet(ASnippet: IEditableSnippet);
var
  DBSnippet: TDBSnippet;
resourcestring
  // Error message
  sNameExists = 'Snippet "%s" already exists in database';
begin
  if fSnippetsTable.Contains(ASnippet.ID) then
      raise ECodeSnip.CreateFmt(sNameExists, [ASnippet.ID.ToString]);
  if not fAllTags.ContainsSubSet(ASnippet.Tags) then
  begin
    { TODO: Add TriggerNullDataEvent(evChangeBegin); }
    fAllTags.Include(ASnippet.Tags);
    { TODO: Add change event for all tags added. Something like:
       for Tag in ASnippet.Tags do
        TriggerTagChangeEvent(evTagAdded, Tag);  }
    { TODO: Add TriggerNullDataEvent(evChangeEnd); }
  end;
  TriggerNullDataEvent(evChangeBegin);
  try
    // TODO: Do we need to check snippet references and xrefs here?
    DBSnippet := TDBSnippet.CreateFrom(ASnippet);
    FlagUpdate; // sets fLastModifed
    DBSnippet.SetCreated(fLastModified);
    DBSnippet.SetModified(fLastModified);
    fSnippetsTable.Add(DBSnippet);
    Query.Update;
    TriggerSnippetChangeEvent(evSnippetAdded, ASnippet.ID);
  finally
    TriggerNullDataEvent(evChangeEnd);
  end;
end;

procedure TDatabase.DeleteSnippet(ASnippetID: TSnippetID);
var
  Dependent: TSnippetID;      // each snippet that depends on Snippet
  Dependents: ISnippetIDList; // list of dependent snippets
  Referrer: TSnippetID;       // each snippet that cross references Snippet
  Referrers: ISnippetIDList;  // list of referencing snippets
begin
  Assert(Database.SnippetExists(ASnippetID),
    ClassName + '.DeleteSnippet: Snippet is not in the database');
  TriggerNullDataEvent(evChangeBegin);
  TriggerSnippetChangeEvent(evBeforeSnippetDelete, ASnippetID);
  // Get list of referencing and dependent snippets
  Dependents := nil;
  Referrers := nil;
  try
    Dependents := Database.GetDependentsOf(ASnippetID);
    Referrers := Database.GetReferrersTo(ASnippetID);
    // TODO: scan all snippets and remove references that match snippet ID
    // Delete snippet for XRef or Depends list of referencing snippets
    for Referrer in Referrers do
      Database.__SnippetsTable.Get(Referrer).XRefs.Remove(ASnippetID);
    for Dependent in Dependents do
      Database.__SnippetsTable.Get(Dependent).RequiredSnippets.Remove(
        ASnippetID
      );
    // Delete snippet itself
    fSnippetsTable.Delete(ASnippetID);
    Query.Update;
  finally
    FlagUpdate;
    TriggerSnippetChangeEvent(evSnippetDeleted, ASnippetID);
    TriggerNullDataEvent(evChangeEnd);
  end;
end;

procedure TDatabase.Finalize;
begin
  fChangeEvents.Free;
  fSnippetsTable.Free;
  inherited;
end;

procedure TDatabase.FlagUpdate;
begin
  fDirty := True;
  fLastModified := TUTCDateTime.Now;
end;

function TDatabase.GetAllSnippets: ISnippetIDList;
var
  Snippet: TDBSnippet;
begin
  Result := TSnippetIDList.Create(fSnippetsTable.Size);
  for Snippet in fSnippetsTable do
    Result.Add(Snippet.ID);
end;

function TDatabase.GetAllTags: ITagSet;
begin
  Result := TTagSet.Create(fAllTags);
end;

function TDatabase.GetDependentsOf(const ASnippetID: TSnippetID):
  ISnippetIDList;
var
  Snippet: TDBSnippet;
begin
  Result := TSnippetIDList.Create;
  for Snippet in fSnippetsTable do
    if (Snippet.GetID <> ASnippetID)
      and Snippet.GetRequiredSnippets.Contains(ASnippetID) then
      Result.Add(Snippet.GetID);
end;

class function TDatabase.GetInstance: TDatabase;
begin
  if not Assigned(fInstance) then
    fInstance := TDatabase.Create;
  Result := fInstance;
end;

function TDatabase.GetReferrersTo(const ASnippetID: TSnippetID): ISnippetIDList;
var
  Snippet: TDBSnippet;  // references each snippet in database
begin
  Result := TSnippetIDList.Create;
  for Snippet in fSnippetsTable do
    if (Snippet.GetID <> ASnippetID)
      and Snippet.GetXRefs.Contains(ASnippetID) then
      Result.Add(Snippet.GetID);
end;

procedure TDatabase.Initialize;
begin
  inherited;
  fSnippetsTable := TDBSnippetsTable.Create;
  fAllTags := TTagSet.Create;
  fChangeEvents := TMultiCastEvents.Create(Self);
end;

function TDatabase.IsDirty: Boolean;
begin
  Result := fDirty;
end;

function TDatabase.IsEmpty: Boolean;
begin
  Result := fSnippetsTable.Size = 0;
end;

procedure TDatabase.Load;
var
  Loader: IDatabaseLoader;
begin
  fSnippetsTable.Clear; // TODO: decide if loaders should clear this table
  Loader := TDatabaseIOFactory.CreateLoader;
  Loader.Load(fSnippetsTable, fAllTags, fLastModified);
  fDirty := False;
end;

function TDatabase.LookupEditableSnippet(const ASnippetID: TSnippetID):
  IEditableSnippet;
var
  Row: TDBSnippet;
begin
  Row := fSnippetsTable.Get(ASnippetID);
  Result := Row.CloneAsEditable;
end;

function TDatabase.LookupSnippet(const ASnippetID: TSnippetID): ISnippet;
var
  Row: TDBSnippet;
begin
  Row := fSnippetsTable.Get(ASnippetID);
  Result := Row.CloneAsReadOnly;
end;

function TDatabase.NewSnippet: IEditableSnippet;
begin
  // Snippet IDs created in TEditableSnippet.CreateNew *should* be unique, but
  // we make sure the ID is unique, just in case!
  repeat
    Result := TEditableSnippet.CreateNew;
  until not fSnippetsTable.Contains(Result.ID);
end;

procedure TDatabase.RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
begin
  fChangeEvents.RemoveHandler(Handler);
end;

procedure TDatabase.Save;
var
  Writer: IDatabaseWriter;
begin
  Writer := TDatabaseIOFactory.CreateWriter;
  Writer.Save(fSnippetsTable, fAllTags, fLastModified);
  fDirty := False;
end;

function TDatabase.SelectSnippets(FilterFn: TDBFilterFn): ISnippetIDList;
var
  Snippet: TDBSnippet;
begin
  Result := TSnippetIDList.Create;
  for Snippet in fSnippetsTable do
    if FilterFn(Snippet.CloneAsReadOnly) then
      Result.Add(Snippet.ID)
end;

function TDatabase.SnippetCount: Integer;
begin
  Result := fSnippetsTable.Size;
end;

function TDatabase.SnippetExists(const ASnippetID: TSnippetID): Boolean;
begin
  Result := fSnippetsTable.Contains(ASnippetID);
end;

procedure TDatabase.TriggerChangeEvent(const Kind: TDatabaseChangeEventKind;
  const Info: TObject);
var
  EvtInfo: IDatabaseChangeEventInfo;  // event information object
begin
  EvtInfo := TEventInfo.Create(Kind, Info);
  fChangeEvents.TriggerEvents(EvtInfo);
end;

procedure TDatabase.TriggerNullDataEvent(const Kind: TDatabaseChangeEventKind);
begin
  TriggerChangeEvent(Kind, nil);
end;

procedure TDatabase.TriggerSnippetChangeEvent(
  const Kind: TDatabaseChangeEventKind; const SnippetID: TSnippetID);
var
  Obj: TBox<TSnippetID>;
begin
  Obj := TBox<TSnippetID>.Create(SnippetID);
  try
    TriggerChangeEvent(Kind, Obj);
  finally
    Obj.Free;
  end;
end;

function TDatabase.TryLookupEditableSnippet(const ASnippetID: TSnippetID;
  out ASnippet: IEditableSnippet): Boolean;
begin
  Result := fSnippetsTable.Contains(ASnippetID);
  if Result then
    ASnippet := fSnippetsTable.Get(ASnippetID).CloneAsEditable
  else
    ASnippet := nil;
end;

function TDatabase.TryLookupSnippet(const ASnippetID: TSnippetID;
  out ASnippet: ISnippet): Boolean;
begin
  Result := fSnippetsTable.Contains(ASnippetID);
  if Result then
    ASnippet := fSnippetsTable.Get(ASnippetID).CloneAsReadOnly
  else
    ASnippet := nil;
end;

procedure TDatabase.UpdateSnippet(ASnippet: IEditableSnippet);
var
  DBSnippet: TDBSnippet;
begin
  // TODO: refactor out this common in common with AddSnippet
  if not fAllTags.ContainsSubSet(ASnippet.Tags) then
  begin
    { TODO: Add TriggerNullDataEvent(evChangeBegin); }
    fAllTags.Include(ASnippet.Tags);
    { TODO: Add change event for all tags added. Something like:
       for Tag in ASnippet.Tags do
        TriggerTagChangeEvent(evTagAdded, Tag);  }
    { TODO: Add TriggerNullDataEvent(evChangeEnd); }
  end;
  TriggerNullDataEvent(evChangeBegin);
  TriggerSnippetChangeEvent(evBeforeSnippetChange, ASnippet.ID);
  DBSnippet := TDBSnippet.CreateFrom(ASnippet);
  try
    // TODO: Do we need to check snippet references and xrefs here?
    FlagUpdate;
    DBSnippet.SetModified(fLastModified);
    fSnippetsTable.Update(DBSnippet);
    Query.Update;
  finally
    TriggerSnippetChangeEvent(evSnippetChanged, ASnippet.ID);
    TriggerNullDataEvent(evChangeEnd);
    DBSnippet.Free;
  end;
end;

procedure TDatabase.__SetDirty(Dirty: Boolean);
begin
  if Dirty then
    FlagUpdate
  else
    fDirty := False;
end;

procedure TDatabase.__TriggerEvent(const Kind: TDatabaseChangeEventKind;
  const Info: TObject);
begin
  if not Assigned(Info) then
    TriggerNullDataEvent(Kind)
  else if (Info is TSnippet) then
    TriggerSnippetChangeEvent(Kind, (Info as TSnippet).ID)
  else
    raise EBug.CreateFmt('Unexpected Info object type "%s"', [Info.ClassName]);
end;

{ TDatabase.TEventInfo }

constructor TDatabase.TEventInfo.Create(const Kind: TDatabaseChangeEventKind;
  const Info: TObject);
begin
  inherited Create;
  fKind := Kind;
  fInfo := Info;
end;

function TDatabase.TEventInfo.GetInfo: TObject;
begin
  Result := fInfo;
end;

function TDatabase.TEventInfo.GetKind: TDatabaseChangeEventKind;
begin
  Result := fKind;
end;

end.

