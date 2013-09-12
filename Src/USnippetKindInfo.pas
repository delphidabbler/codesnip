{
 * USnippetKindInfo.pas
 *
 * Defines a class that provides information about the different snippet types
 * enumerated by TSnippetKind. The class can only be instantiated as a singleton
 * and can't be freed until the unit is finalised. The class maintains a list of
 * objects representing each snippet kind. Similarly, these object cannot be
 * freed until the owning object is freed.
 *
 * The only reason to go through these convolutions instead of simply providing
 * some structured constant was to retrofit the snippet kind view item to
 * TViewItem. TViewItem expects view items that maintain some value or
 * description (in this case a description of the snippet kind) to be
 * represented as an object to which TViewItem maintains a reference. This means
 * that the lifetime of the object must be greater than any TViewItem. The best
 * way to do this seemed to maintain a list of objects, one per snippet kind,
 * that can't be destroyed until the program closes and have a lifetime the same
 * as the program.
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
 * The Original Code is USnippetKindInfo.pas
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


unit USnippetKindInfo;


interface


uses
  // Project
  UBaseObjects, USnippets;


type

  {
  TSnippetKindInfo:
    Abstract class that encapsulates a snippet kind. Descendant must make
    concrete.
  }
  TSnippetKindInfo = class(TNoPublicConstructObject)
  strict protected
    function GetDescription: string; virtual; abstract;
       {Read accessor for Description property. To be defined by descendant
       classes.
        @return Required description.
      }
   function GetKind: TSnippetKind; virtual; abstract;
       {Read accessor for Kind property. To be defined by descendant classes.
        @return Required kind.
      }
  public
    property Kind: TSnippetKind read GetKind;
      {Snippet kind}
    property Description: string read GetDescription;
      {Description of snippet kind}
  end;

  {
  TSnippetKindInfoList:
    Class that maintains a list of concreate TSnippetKindInfo descendant
    objects, one for each snippet kind.
  }
  TSnippetKindInfoList = class(TNoPublicConstructObject)
  strict private
    var fItems: array[TSnippetKind] of TSnippetKindInfo;
      {Storage for Items[] property}
    class var fInstance: TSnippetKindInfoList;
      {Stores singleton instance}
    class function GetInstance: TSnippetKindInfoList; static;
      {Gets an instance of the object. When first first called the object is
      created. Subsequent calls return the same instance.
        @return Singletom object instance.
      }
    function GetItem(Kind: TSnippetKind): TSnippetKindInfo;
      {Read accessor for Items] property.
        @param Kind [in] Specifies required snippet kind.
        @return Required snippet kinf information object.
      }
  strict protected
    constructor InternalCreate;
      {Class constructor Called by GetInstance. Sets up object and creates all
      required TSnippetKindInfo objects.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object. Frees all snippet kind instances by
      setting their CanDestroy flag before freeing.
      }
    class property Instance: TSnippetKindInfoList read GetInstance;
      {Singleton instance of this object}
    property Items[Kind: TSnippetKind]: TSnippetKindInfo read GetItem; default;
      {Array of snippets info objects}
  end;


implementation


resourcestring
  // Snippet kind descriptions
  sFreeForm         = 'Freeform';
  sRoutine          = 'Routine';
  sConstant         = 'Constant';
  sTypeDef          = 'Type Definition';
const
  // Map of snippet kinds onto descriptions
  cDescriptions: array[TSnippetKind] of string = (
    sFreeForm, sRoutine, sConstant, sTypeDef
  );

type

  {
  TSnippetKindInfoImpl:
    Concrete implementation of TSnippetKindInfo that can't be freed except by
    owning object.
  }
  TSnippetKindInfoImpl = class(TSnippetKindInfo)
  strict private
    var fKind: TSnippetKind;  // value of Kind property
  strict protected
    function GetDescription: string; override;
       {Read accessor for Description property.
        @return Required description.
      }
   function GetKind: TSnippetKind; override;
       {Read accessor for Kind property.
      }
  public
    var CanDestroy: Boolean;  // flag indicating if object can be freed
    constructor Create(const Kind: TSnippetKind);
      {Class constructor. Creates a non-destructable object.
        @param Kind [in] Snippet kind to be represented.
      }
    procedure FreeInstance; override;
      {Override of method that releases object's memory on destruction that
      does not release memory unless CanDestroy is true. This effectively means
      the instance cannot be freed until CanDestroy is set true.
      }
  end;

  {
  TSnippetKindInfoListImpl:
    Extension of TSnippetKindInfoList that can't be destroyed except in
    finalization section of this unit.
  }
  TSnippetKindInfoListImpl = class(TSnippetKindInfoList)
  public
    class var CanDestroy: Boolean;
      {Flag that must be true to permit object to be freed}
    destructor Destroy; override;
      {Class destructor. Only calls inherited destructor to destroy snippet kind
      information objects if CanDestroy is True.
      }
    procedure FreeInstance; override;
      {Override of method that releases object's memory on destruction that
      does not release memory unless CanDestroy is true. This effectively means
      the instance cannot be freed until CanDestroy is set true.
      }
  end;

{ TSnippetKindInfoList }

destructor TSnippetKindInfoList.Destroy;
  {Class destructor. Tears down object. Frees all snippet kind instances by
  setting their CanDestroy flag before freeing.
  }
var
  Kind: TSnippetKind; // loops through all snippet kinds
begin
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
  begin
    (fItems[Kind] as TSnippetKindInfoImpl).CanDestroy := True;
    fItems[Kind].Free;
  end;
  fInstance := nil;
  inherited;
end;

class function TSnippetKindInfoList.GetInstance: TSnippetKindInfoList;
  {Gets an instance of the object. When first first called the object is
  created. Subsequent calls return the same instance.
    @return Singletom object instance.
  }
begin
  if not Assigned(fInstance) then
    // Object created as TSnippetKindInfoListImpl to get the hidden "no-free"
    // functionality of that class without exposing CanDestroy flag to wider
    // world
    fInstance := TSnippetKindInfoListImpl.InternalCreate;
  Result := fInstance;
end;

function TSnippetKindInfoList.GetItem(Kind: TSnippetKind): TSnippetKindInfo;
  {Read accessor for Items] property.
    @param Kind [in] Specifies required snippet kind.
    @return Required snippet kinf information object.
  }
begin
  Result := fItems[Kind];
end;

constructor TSnippetKindInfoList.InternalCreate;
  {Class constructor Called by GetInstance. Sets up object and creates all
  required TSnippetKindInfo objects.
  }
begin
  inherited;
  // Actually creates TSnippetKindInfoImpl objects
  fItems[skFreeform] := TSnippetKindInfoImpl.Create(skFreeForm);
  fItems[skRoutine] := TSnippetKindInfoImpl.Create(skRoutine);
  fItems[skConstant] := TSnippetKindInfoImpl.Create(skConstant);
  fItems[skTypeDef] := TSnippetKindInfoImpl.Create(skTypeDef);
end;

{ TSnippetKindInfoImpl }

constructor TSnippetKindInfoImpl.Create(const Kind: TSnippetKind);
  {Class constructor. Creates a non-destructable object.
    @param Kind [in] Snippet kind to be represented.
  }
begin
  inherited InternalCreate;
  fKind := Kind;
  CanDestroy := False;
end;

procedure TSnippetKindInfoImpl.FreeInstance;
  {Override of method that releases object's memory on destruction that does not
  release memory unless CanDestroy is true. This effectively means the instance
  cannot be freed until CanDestroy is set true.
  }
begin
  if CanDestroy then
    inherited FreeInstance;
end;

function TSnippetKindInfoImpl.GetDescription: string;
   {Read accessor for Description property.
    @return Required description.
  }
begin
  Result := cDescriptions[Kind];
end;

function TSnippetKindInfoImpl.GetKind: TSnippetKind;
   {Read accessor for Kind property.
  }
begin
  Result := fKind;
end;

{ TSnippetKindInfoListImpl }

destructor TSnippetKindInfoListImpl.Destroy;
  {Class destructor. Only calls inherited destructor to destroy snippet kind
  information objects if CanDestroy is True.
  }
begin
  if CanDestroy then
    inherited;
end;

procedure TSnippetKindInfoListImpl.FreeInstance;
  {Override of method that releases object's memory on destruction that
  does not release memory unless CanDestroy is true. This effectively means
  the instance cannot be freed until CanDestroy is set true.
  }
begin
  if CanDestroy then
    inherited;
end;


initialization


finalization

// Destroy the TSnippetKindInfoList singleton
TSnippetKindInfoListImpl.CanDestroy := True;
TSnippetKindInfoListImpl.Instance.Free;

end.

