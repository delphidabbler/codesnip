unit CS.Database.SnippetLinks;

interface

uses
  CS.Database.Types;

type
  // TODO: May need to be in a separate Synch Space unit, or in Types
  TSnippetSynchSpaceIDs = record
  public
    const LegacyDB: TGUID = '{04AE1B53-6D60-48FA-B81F-E8CF15222F6D}';
  end;

  TSnippetLinkInfo = class(TInterfacedObject, ISnippetLinkInfo)
  strict private
    var
      fSynchSpaceID: TGUID;
      fLinkedSnippetID: TSnippetID;
  public
    constructor Create(const ASynchSpaceID: TGUID;
      const ALinkedSnippetID: TSnippetID); overload;
    constructor Create(Src: ISnippetLinkInfo); overload;
    function IsLinked: Boolean;
    function GetSynchSpaceID: TGUID;
    function GetLinkedSnippetID: TSnippetID;
  end;

  TNullSnippetLinkInfo = class(TInterfacedObject, ISnippetLinkInfo)
  public
    function IsLinked: Boolean;
    function GetSynchSpaceID: TGUID;
    function GetLinkedSnippetID: TSnippetID;
  end;

implementation

uses
  SysUtils;

{ TSnippetLinkInfo }

constructor TSnippetLinkInfo.Create(const ASynchSpaceID: TGUID;
  const ALinkedSnippetID: TSnippetID);
begin
  inherited Create;
  fSynchSpaceID := ASynchSpaceID;
  fLinkedSnippetID := ALinkedSnippetID;
end;

constructor TSnippetLinkInfo.Create(Src: ISnippetLinkInfo);
begin
  Create(Src.SynchSpaceID, Src.LinkedSnippetID);
end;

function TSnippetLinkInfo.GetLinkedSnippetID: TSnippetID;
begin
  Result := fLinkedSnippetID;
end;

function TSnippetLinkInfo.GetSynchSpaceID: TGUID;
begin
  Result := fSynchSpaceID;
end;

function TSnippetLinkInfo.IsLinked: Boolean;
begin
  Result := True;
end;

{ TNullSnippetLinkInfo }

function TNullSnippetLinkInfo.GetLinkedSnippetID: TSnippetID;
begin
  raise ENotSupportedException.Create(
    'GetLinkedSnippetID is not implemented in ' + ClassName
  );
end;

function TNullSnippetLinkInfo.GetSynchSpaceID: TGUID;
begin
  raise ENotImplemented.Create(
    'GetSynchSpaceID is not implemented in ' + ClassName
  );
end;

function TNullSnippetLinkInfo.IsLinked: Boolean;
begin
  Result := False;
end;

end.

