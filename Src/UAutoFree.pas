{
 * UAutoFree.pas
 *
 * Interfaced class that can wrap an object and automatically free it when the
 * wrapper class goes out of scope.
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
 * The Original Code is UAutoFree.pas (formerly UProtocolHandler.pas).
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UAutoFree;


interface


type
  {
  TAutoObjFree:
    Interfaced class that wraps another object and automatically frees it when
    it goes out of scope.
  }
  TAutoObjFree = class(TInterfacedObject, IInterface)
  strict private
    fObject: TObject; // wrapped object
  public
    constructor Create(Obj: TObject); overload;
      {Class constructor. Sets up object to auto-free another object.
        @param Obj [in] Wrapped object.
      }
    destructor Destroy; override;
      {Class destructor. Frees wrapped object.
      }
  end;


implementation


{ TAutoObjFree }

constructor TAutoObjFree.Create(Obj: TObject);
  {Class constructor. Sets up object to auto-free another object.
    @param Obj [in] Wrapped object.
  }
begin
  inherited Create;
  fObject := Obj;
end;

destructor TAutoObjFree.Destroy;
  {Class destructor. Frees wrapped object.
  }
begin
  fObject.Free;
  inherited;
end;

end.
