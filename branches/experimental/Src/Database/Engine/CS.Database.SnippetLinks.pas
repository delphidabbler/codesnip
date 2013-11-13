unit CS.Database.SnippetLinks;

interface

uses
  CS.Database.Types;

type
  TSnippetLinkInfo = class(TInterfacedObject, ISnippetLinkInfo)
  strict private
    var
      fSynchSpaceID: TGUID;
      fLinkedSnippetID: TDBSnippetID;
  public
    constructor Create(const ASynchSpaceID: TGUID;
      const ALinkedSnippetID: TDBSnippetID); overload;
    constructor Create(Src: ISnippetLinkInfo); overload;
    function IsLinked: Boolean;
    function GetSynchSpaceID: TGUID;
    function GetLinkedSnippetID: TDBSnippetID;
  end;

  TNullSnippetLinkInfo = class(TInterfacedObject, ISnippetLinkInfo)
  public
    function IsLinked: Boolean;
    function GetSynchSpaceID: TGUID;
    function GetLinkedSnippetID: TDBSnippetID;
  end;

implementation

uses
  SysUtils;

{ TSnippetLinkInfo }

constructor TSnippetLinkInfo.Create(const ASynchSpaceID: TGUID;
  const ALinkedSnippetID: TDBSnippetID);
begin
  inherited Create;
  fSynchSpaceID := ASynchSpaceID;
  fLinkedSnippetID := ALinkedSnippetID;
end;

constructor TSnippetLinkInfo.Create(Src: ISnippetLinkInfo);
begin
  Create(Src.SynchSpaceID, Src.LinkedSnippetID);
end;

function TSnippetLinkInfo.GetLinkedSnippetID: TDBSnippetID;
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

function TNullSnippetLinkInfo.GetLinkedSnippetID: TDBSnippetID;
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
