unit NsDatabase.UDataItem;

interface

uses
  UBaseObjects, NsDatabase.UCookies;

type

  TDBDataItem = class(TOwnedConditionalFreeObject)
  private
    fCookie: TDBCookie;
  public
    // todo: make constructor create own cookie??
    constructor Create(const Cookie: TDBCookie);
    property Cookie: TDBCookie read fCookie;
  end;

implementation

{ TDBDataItem }

constructor TDBDataItem.Create(const Cookie: TDBCookie);
begin
  // create with no owner
  inherited Create;
  fCookie := Cookie;
end;

end.
