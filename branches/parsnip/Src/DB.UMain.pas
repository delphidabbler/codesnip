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
    procedure TriggerTagChangeEvent(const Kind: TDatabaseChangeEventKind;
      const Tag: TTag);
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
    procedure AddChangeEventHandler(const Handler: TNotifyEventInfo);
      {Adds a change event handler to list of listeners.
        @param Handler [in] Event handler to be added.
      }
    procedure RemoveChangeEventHandler(const Handler: TNotifyEventInfo);
      {Removes a change event handler from list of listeners.
        @param Handler [in] Handler to remove from list.
      }
  end;

  _IDatabase = interface(IInterface)
    ['{A280DEEF-0336-4264-8BD0-7CDFBB207D2E}']
    function Lookup(const SnippetID: TSnippetID): TSnippet;
    function TryLookup(const SnippetID: TSnippetID; out Snippet: TSnippet):
      Boolean;
  end;

  {
  IDatabaseEdit:
    Interface to object that can be used to edit the database.
  }
  IDatabaseEdit = interface(IInterface)
    ['{CBF6FBB0-4C18-481F-A378-84BB09E5ECF4}']
    function GetEditableSnippetInfo(const Snippet: TSnippet = nil):
      TSnippetEditData;
      {Provides details of all a snippet's data (properties and references) that
      may be edited.
        @param Snippet [in] Snippet for which data is required. May be nil in
          which case a blank record is returned.
        @return Required data.
      }
    procedure UpdateSnippet(const Snippet: TSnippet; Data: TSnippetEditData);
      {Updates a snippet's properties and references using provided data.
        @param Snippet [in] Snippet to be updated.
        @param Data [in] Record containing revised data.
      }
    function AddSnippet(Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the database.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    ///  <summary>Creates new snippet that duplicates the given snippet, except
    ///  that its is a given a new unique name and the given title.</summary>
    ///  <param name="Snippet">TSnippet [in] Snippet to be duplicated.</param>
    ///  <param name="Title">string [in] Title of duplicated snippet.</param>
    ///  <return>TSnippet. Duplicated snippet.</return>
    function DuplicateSnippet(const Snippet: TSnippet; const Title: string):
      TSnippet;
    function CreateTempSnippet(const Data: TSnippetEditData;
      const Name: string = ''): TSnippet; overload;
      {Creates a new temporary snippet without adding it to the Snippets
      object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Data [in] Record storing new snippet's properties and references.
        @param Name [in] Required snippet name. If ommitted then the snippet is
          created with a new, unique, name.
        @return Reference to new snippet.
      }
    function CreateTempSnippet(const Snippet: TSnippet): TSnippet; overload;
      {Creates a new temporary copy of a snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Snippet [in] Snippet to be copied.
        @return Reference to new copied snippet.
      }
    procedure DeleteSnippet(const Snippet: TSnippet);
      {Deletes a snippet from the database.
        @param Snippet [in] Snippet to be deleted.
      }
  end;


function _Database: _IDatabase;
  {Returns singleton instance of object that encapsulates the snippets database.
    @return Singleton object.
  }

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


var
  // Private global snippets singleton object
  _PvtDatabase: _IDatabase = nil;


type

  _TDatabase = class(TInterfacedObject,
    _IDatabase,
    IDatabaseEdit
  )
  strict private
    function InternalAddSnippet(const SnippetID: TSnippetID;
      const Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the database. Assumes snippet not already in
      database.
        @param SnippetID [in] ID of new snippet.
        @param Data [in] Properties and references of new snippet.
        @return Reference to new snippet object.
      }
    procedure InternalDeleteSnippet(const Snippet: TSnippet);
      {Deletes a snippet from database.
        @param Snippet [in] Snippet to delete from database.
      }
    function UniqueSnippetName: string;
      {Generates a snippet "name" that is unique in the database.
        @return Required unique snippet "name".
      }
    procedure CleanUpRefs(var Refs: TSnippetReferences);
      {Cleans any references non-existant snippets from snippet references.
        @param [in/out] Snippet references to be cleaned: .RequiredSnippets and
          .XRefs fields are modified.
      }
    function MakeValidRefs(const Refs: TSnippetReferences): TSnippetReferences;
  public
    { IDatabase methods }
    function Lookup(const SnippetID: TSnippetID): TSnippet;
    function TryLookup(const SnippetID: TSnippetID; out Snippet: TSnippet):
      Boolean;
    { IDatabaseEdit methods }
    function GetEditableSnippetInfo(const Snippet: TSnippet = nil):
      TSnippetEditData;
      {Provides details of all a snippet's data (properties and references) that
      may be edited.
        @param Snippet [in] Snippet for which data is required. May be nil in
          which case a blank record is returned.
        @return Required data.
      }
    procedure UpdateSnippet(const Snippet: TSnippet; Data: TSnippetEditData);
      {Updates a snippet's properties and references using provided data.
        @param Snippet [in] Snippet to be updated.
        @param Data [in] Record containing revised data.
      }
    function AddSnippet(Data: TSnippetEditData): TSnippet;
      {Adds a new snippet to the database.
        @param Data [in] Record storing new snippet's properties and references.
        @return Reference to new snippet.
      }
    ///  <summary>Creates new snippet that duplicates the given snippet, except
    ///  that its is a given a new unique name and the given title.</summary>
    ///  <param name="Snippet">TSnippet [in] Snippet to be duplicated.</param>
    ///  <param name="Title">string [in] Title of duplicated snippet.</param>
    ///  <return>TSnippet. Duplicated snippet.</return>
    ///  <remarks>Method of IDatabaseEdit.</remarks>
    function DuplicateSnippet(const Snippet: TSnippet; const Title: string):
      TSnippet;
    function CreateTempSnippet(const Data: TSnippetEditData;
      const Name: string = ''): TSnippet; overload;
      {Creates a new temporary snippet without adding it to the Snippets
      object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Data [in] Record storing new snippet's properties and references.
        @param Name [in] Required snippet name. If ommitted then the snippet is
          created with a new, unique, name.
        @return Reference to new snippet.
      }
    function CreateTempSnippet(const Snippet: TSnippet): TSnippet; overload;
      {Creates a new temporary copy of a snippet without adding it to the
      Snippets object's snippets list. The new instance may not be added to the
      Snippets object.
        @param Snippet [in] Snippet to be copied.
        @return Reference to new snippet.
      }
    procedure DeleteSnippet(const Snippet: TSnippet);
      {Deletes a snippet from the database.
        @param Snippet [in] Snippet to be deleted.
      }
  end;

function Database: TDatabase;
begin
  Result := TDatabase.Instance;
end;

function _Database: _IDatabase;
  {Returns singleton instance of object that encapsulates the snippets database.
    @return Singleton object.
  }
begin
  if not Assigned(_PvtDatabase) then
    _PvtDatabase := _TDatabase.Create;
  Result := _PvtDatabase;
end;

{ _TDatabase }

function _TDatabase.AddSnippet(Data: TSnippetEditData): TSnippet;
  {Adds a new snippet to the database.
    @param Data [in] Record storing new snippet's properties and references.
    @return Reference to new snippet.
  }
resourcestring
  // Error message
  sNameExists = 'Snippet "%s" already exists in database';
var
  NewSnippetID: TSnippetID;
begin
  Result := nil;  // keeps compiler happy
  Database.__TriggerEvent(evChangeBegin);
  try
    NewSnippetID := TSnippetID.Create(UniqueSnippetName);
    // Check if snippet with same name exists in database: error if so
    if Database.__SnippetsTable.Contains(NewSnippetID) then
      raise ECodeSnip.CreateFmt(sNameExists, [NewSnippetID.ToString]);
    CleanUpRefs(Data.Refs);
    Result := InternalAddSnippet(NewSnippetID, Data);
    Query.Update;
    Database.__TriggerEvent(evSnippetAdded, Result);
  finally
    Database.__Updated := True;
    Database.__TriggerEvent(evChangeEnd);
  end;
end;

procedure _TDatabase.CleanUpRefs(var Refs: TSnippetReferences);

  function CleanSnippetList(const SnipList: ISnippetIDList): ISnippetIDList;
  var
    SnippetID: TSnippetID;
  begin
    Result := TSnippetIDList.Create;
    for SnippetID in SnipList do
      if Database.__SnippetsTable.Contains(SnippetID) then
        Result.Add(SnippetID);
  end;

begin
  Refs.RequiredSnippets := CleanSnippetList(Refs.RequiredSnippets);
  Refs.XRefs := CleanSnippetList(Refs.XRefs);
end;

function _TDatabase.CreateTempSnippet(const Snippet: TSnippet): TSnippet;
  {Creates a new temporary copy of a snippet without adding it to the
  Snippets object's snippets list. The new instance may not be added to the
  Snippets object.
    @param Snippet [in] Snippet to be copied.
    @return Reference to new snippet.
  }
begin
  Assert(Assigned(Snippet), ClassName + '.CreateTempSnippet: Snippet is nil');
  Result := TSnippet.Create(Snippet.ID);
  Result.SetProps(Snippet.GetProps);
  Result.SetCreated(TUTCDateTime.Now);
  Result.SetModified(Result.GetCreated);
  Result.UpdateRefs(MakeValidRefs(Snippet.GetReferences));
end;

function _TDatabase.CreateTempSnippet(const Data: TSnippetEditData;
  const Name: string = ''): TSnippet;
  {Creates a new temporary snippet without adding it to the Snippets object's
  snippets list. The new instance may not be added to the Snippets object.
    @param Data [in] Record storing new snippet's properties and references.
    @param Name [in] Required snippet name. If ommitted then the snippet is
      created with a new, unique, name.
    @return Reference to new snippet.
  }
begin
  Result := TSnippet.Create(
    TSnippetID.Create(StrIf(Name <> '', Name, UniqueSnippetName))
  );
  Result.SetProps(Data.Props);
  Result.SetCreated(TUTCDateTime.Now);
  Result.SetModified(Result.GetCreated);
  Result.UpdateRefs(MakeValidRefs(Data.Refs));
end;

procedure _TDatabase.DeleteSnippet(const Snippet: TSnippet);
  {Deletes a snippet from the database.
    @param Snippet [in] Snippet to be deleted.
  }
var
  Dependent: TSnippetID;      // each snippet that depends on Snippet
  Dependents: ISnippetIDList; // list of dependent snippets
  Referrer: TSnippetID;       // each snippet that cross references Snippet
  Referrers: ISnippetIDList;  // list of referencing snippets
begin
  Assert(Database.__SnippetsTable.Contains(Snippet.ID),
    ClassName + '.DeleteSnippet: Snippet is not in the database');
  Database.__TriggerEvent(evChangeBegin);
  Database.__TriggerEvent(evBeforeSnippetDelete, Snippet);
  // Get list of referencing and dependent snippets
  Dependents := nil;
  Referrers := nil;
  try
    Dependents := Database.GetDependentsOf(Snippet.ID);
    Referrers := Database.GetReferrersTo(Snippet.ID);
    // TODO: scan all snippets and remove references that match snippet ID
    // Delete snippet for XRef or Depends list of referencing snippets
    for Referrer in Referrers do
      Database.__SnippetsTable.Get(Referrer).XRefs.Remove(Snippet.ID);
    for Dependent in Dependents do
      Database.__SnippetsTable.Get(Dependent).RequiredSnippets.Remove(
        Snippet.ID
      );
    // Delete snippet itself
    InternalDeleteSnippet(Snippet);
    Query.Update;
  finally
    Database.__Updated := True;
    Database.__TriggerEvent(evSnippetDeleted);
    Database.__TriggerEvent(evChangeEnd);
  end;
end;

function _TDatabase.DuplicateSnippet(const Snippet: TSnippet;
  const Title: string): TSnippet;
var
  Data: TSnippetEditData;
begin
  Data := Snippet.GetEditData;
  Data.Props.Title := Title;
  Result := AddSnippet(Data);
end;

function _TDatabase.GetEditableSnippetInfo(
  const Snippet: TSnippet): TSnippetEditData;
  {Provides details of all a snippet's data (properties and references) that may
  be edited.
    @param Snippet [in] Snippet for which data is required. May be nil in which
      case a blank record is returned.
    @return Required data.
  }
begin
  if Assigned(Snippet) then
    Result := Snippet.GetEditData
  else
    Result.Init;
end;

function _TDatabase.InternalAddSnippet(const SnippetID: TSnippetID;
  const Data: TSnippetEditData): TSnippet;
  {Adds a new snippet to the database. Assumes snippet not already in the
  database.
    @param SnippetID [in] ID of new snippet.
    @param Data [in] Properties and references of new snippet.
    @return Reference to new snippet object.
  }
begin
  Result := TSnippet.Create(SnippetID);
  Result.SetProps(Data.Props);
  Result.SetCreated(TUTCDateTime.Now);
  Result.SetModified(Result.GetCreated);
  Result.UpdateRefs(MakeValidRefs(Data.Refs));
  // ensure any unknown tags are added to set of all known tags
  Database.__AllTags.Include(Result.Tags);
  Database.__SnippetsTable.Add(Result);
end;

procedure _TDatabase.InternalDeleteSnippet(const Snippet: TSnippet);
  {Deletes a snippet from the database.
    @param Snippet [in] Snippet to delete from database.
  }
begin
  // Don't remove snippets tags from tag list - leave them there
  // Delete from "master" list: this frees Snippet
  Database.__SnippetsTable.Delete(Snippet.ID);
end;

function _TDatabase.Lookup(const SnippetID: TSnippetID): TSnippet;
begin
  if not TryLookup(SnippetID, Result) then
    raise EBug.Create(ClassName + '.Lookup: SnippetID not found in database');
end;

function _TDatabase.MakeValidRefs(const Refs: TSnippetReferences):
  TSnippetReferences;
begin
  Result.Assign(Refs);
  CleanUpRefs(Result);
end;

function _TDatabase.TryLookup(const SnippetID: TSnippetID;
  out Snippet: TSnippet): Boolean;
begin
  Result := Database.__SnippetsTable.Contains(SnippetID);
  if Result then
    Snippet := Database.__SnippetsTable.Get(SnippetID);
end;

function _TDatabase.UniqueSnippetName: string;
begin
  repeat
    Result := 'Snippet' + TUniqueID.Generate;
  until not Database.__SnippetsTable.Contains(TSnippetID.Create(Result));
end;

procedure _TDatabase.UpdateSnippet(const Snippet: TSnippet;
  Data: TSnippetEditData);
  {Updates a defined snippet's properties and references using provided data.
    @param Snippet [in] Snippet to be updated.
    @param Data [in] Record containing revised data.
  }
begin
  Database.__TriggerEvent(evChangeBegin);
  Database.__TriggerEvent(evBeforeSnippetChange, Snippet);
  try
    CleanUpRefs(Data.Refs);
    Snippet.Update(Data);
    Snippet.SetModified(TUTCDateTime.Now);
    // ensure any new, unknown, tags are added to set of all tags
    Database.__AllTags.Include(Snippet.Tags);
    Query.Update;
    Database.__TriggerEvent(evSnippetChanged, Snippet);
  finally
    Database.__Updated := True;
    Database.__TriggerEvent(evChangeEnd);
  end;
end;

{ TDatabase }

procedure TDatabase.AddChangeEventHandler(const Handler: TNotifyEventInfo);
begin
  fChangeEvents.AddHandler(Handler);
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

procedure TDatabase.TriggerTagChangeEvent(const Kind: TDatabaseChangeEventKind;
  const Tag: TTag);
var
  Obj: TBox<TTag>;
begin
  Obj := TBox<TTag>.Create(Tag);
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

initialization

finalization

// Free the singleton
_PvtDatabase := nil;

end.

