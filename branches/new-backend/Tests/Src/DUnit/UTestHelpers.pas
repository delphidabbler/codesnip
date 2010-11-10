{
  Helper unit that provides common code for use in test suites
  ------------------------------------------------------------

  $Rev$
  $Date$
}

unit UTestHelpers;

interface

uses
  UBaseObjects;

type

  TAlwaysFreeController = class(TInterfacedObject, IConditionalFreeController)
  public
    function PermitDestruction(const Obj: TObject): Boolean;
  end;

implementation

{ TAlwaysFreeController }

function TAlwaysFreeController.PermitDestruction(const Obj: TObject): Boolean;
begin
  Result := True;
end;

end.
