{
 * Database.UObjectDestructionMgr.pas
 *
 * Implements a helper class that manages destruction of controlled conditional-
 * free objects.
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
 * The Original Code is Database.UObjectDestructionMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit Database.UObjectDestructionMgr;

interface

uses
  SysUtils,
  UBaseObjects, Database.UCookies, Database.UDataItem;

type
  TObjectDestructionMgr = class(TObject)
  strict private
    type
      TPermissionFn = TFunc<TDBDataItem,Boolean>;
  public  // todo: try to make private after testing
    type
      TController = class(TInterfacedObject, IConditionalFreeController)
      strict private
        fCanDestroy: TPermissionFn;
      public
        function PermitDestruction(const Obj: TObject): Boolean;
          {Checks if an object can be destroyed.
            @param Obj [in] Object to be checked.
            @return True if object can be destroyed, False if not.
          }
        constructor Create(const CanDestroy: TPermissionFn);
      end;
  strict private
    type
      TPermission = (dpFreeAll, dpFreePerCookie, dpFreeNone);
    var
      fState: TPermission;
      fCookie: TDBCookie;
      fController: IConditionalFreeController;
  public
    constructor Create;
    procedure AllowDestroyNone;
    procedure AllowDestroyAll;
    procedure AllowDestroyCookie(const Cookie: TDBCookie);
    // todo: replace property with method that hooks controller to an object
    // todo: name such method HookController or *Register* or some such
    property Controller: IConditionalFreeController read fController;
  end;

implementation

{ TObjectDestructionMgr }

procedure TObjectDestructionMgr.AllowDestroyAll;
begin
  fState := dpFreeAll;
end;

procedure TObjectDestructionMgr.AllowDestroyCookie(const Cookie: TDBCookie);
begin
  fState := dpFreePerCookie;
  fCookie := Cookie;
end;

procedure TObjectDestructionMgr.AllowDestroyNone;
begin
  fState := dpFreeNone;
end;

constructor TObjectDestructionMgr.Create;
var
  PermissionFn: TPermissionFn;
begin
  inherited Create;
  AllowDestroyNone;
  PermissionFn :=
    function(Obj: TDBDataItem): Boolean
    begin
      Result := False;  // keeps compiler quiet
      case fState of
        dpFreeAll:
          Result := True;
        dpFreePerCookie:
          Result := Obj.Cookie = fCookie;
        dpFreeNone:
          Result := False;
      end;
    end;
    fController := TController.Create(PermissionFn);
end;

{ TObjectDestructionMgr.TController }

constructor TObjectDestructionMgr.TController.Create(
  const CanDestroy: TPermissionFn);
begin
  Assert(Assigned(CanDestroy), ClassName + '.Create: CanDestroy is nil');
  inherited Create;
  fCanDestroy := CanDestroy;
end;

function TObjectDestructionMgr.TController.PermitDestruction(
  const Obj: TObject): Boolean;
resourcestring
  sBadObject = '%0:s.TController.PermitDestruction: Obj parameter must descend '
    + 'from %1:s';
begin
  if not (Obj is TDBDataItem) then
    raise EArgumentException.CreateFmt(
      sBadObject, [ClassName, TDBDataItem.ClassName]
    );
  Result := fCanDestroy(Obj as TDBDataItem);
end;

end.
