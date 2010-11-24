{
 * UGC.pas
 *
 * Provides a garbage collector to assist in maintaining lifetimes of various
 * resources.
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
 * The Original Code is UGC.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UGC;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TGC:
    Static garbage collector class. Used to manage lifetimes of various
    resources.
  }
  TGC = class(TNoConstructObject)
  strict private
    class procedure GetGC(var GC: IInterface);
      {Ensures that a local garbage collector is instantiated.
        @param GC [in/out] Garbage collector. If nil a new garbage collector
          will be created and stored in GC. If GC is not nil it is checked for
          validity and left unchanged.
      }
  public
    class procedure GCLocalObj(var LocalGC: IInterface; const Obj: TObject);
      {Adds an object to a local garbage collector instance so that the object
      is freed when the garbage collector goes out of scope.
        @param LocalGC [in/out] Valid garbage collector to use, or nil. If nil a
          new garbage collector is created and stored in LocalGC.
        @param Obj [in] Object to place under garbage collection. Obj is added
          to LocalGC and freed when LocalGC goes out of scope.
      }
  end;


implementation


uses
  // Project
  SysUtils, Generics.Collections;


type
  {
  IGC:
    Interface supported by garbage collector objects.
  }
  IGC = interface(IInterface)
    ['{61D7BA32-3999-4AD7-A901-4E83A29508F2}']
    function AddObject(const Obj: TObject): TObject;
      {Adds an object to a garbage collector.
        @param Obj [in] Object to be garbage collected.
        @return Obj unchanged.
      }
    // Add futher AddXXX methods for each required resource types
  end;

  {
  TGarbageCollector:
    Garbage collector object. Manages a list of resources to be garbage
    collected. All the resource are released / freed when the garbage collector
    goes out of scope and is destroyed.
  }
  TGarbageCollector = class(TInterfacedObject, IInterface, IGC)
  strict private
    var fList: TList<IInterface>; // List of managed resources
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Releases all resources and tears down object.
      }
    { IGC methods }
    function AddObject(const Obj: TObject): TObject;
      {Adds an object to the list of managed resources. Object will be freed
      when garbage collector goes out of scope.
        @param Obj [in] Object to be garbage collected.
        @return Obj unchanged.
      }
  end;

  {
  TGCObjectWrapper:
    Interfaced class that wraps another object and automatically frees it when
    this object goes out of scope.
  }
  TGCObjectWrapper = class(TInterfacedObject, IInterface)
  strict private
    fObject: TObject; // Wrapped object
  public
    constructor Create(Obj: TObject); overload;
      {Object constructor. Sets up object wrapper to auto-free another object.
        @param Obj [in] Wrapped object.
      }
    destructor Destroy; override;
      {Object destructor. Frees wrapped object.
      }
  end;


{ TGCObjectWrapper }

constructor TGCObjectWrapper.Create(Obj: TObject);
  {Object constructor. Sets up object wrapper to auto-free another object.
    @param Obj [in] Wrapped object.
  }
begin
  Assert(Assigned(Obj), ClassName + '.Create: Obj is nil');
  inherited Create;
  fObject := Obj;
end;

destructor TGCObjectWrapper.Destroy;
  {Object destructor. Frees wrapped object.
  }
begin
  fObject.Free;
  inherited;
end;

{ TGarbageCollector }

function TGarbageCollector.AddObject(const Obj: TObject): TObject;
  {Adds an object to the list of managed resources. Object will be freed when
  garbage collector goes out of scope.
    @param Obj [in] Object to be garbage collected.
    @return Obj unchanged.
  }
begin
  Assert(Assigned(Obj), ClassName + '.AddObject: Obj is nil');
  fList.Add(TGCObjectWrapper.Create(Obj));
  Result := Obj;
end;

constructor TGarbageCollector.Create;
  {Object constructor. Sets up object.
  }
begin
  inherited Create;
  fList := TList<IInterface>.Create;
end;

destructor TGarbageCollector.Destroy;
  {Object destructor. Releases all resources and tears down object.
  }
begin
  fList.Free; // releases contained interfaced objects
  inherited;
end;

{ TGC }

class procedure TGC.GCLocalObj(var LocalGC: IInterface; const Obj: TObject);
  {Adds an object to a local garbage collector instance so that the object
  is freed when the garbage collector goes out of scope.
    @param LocalGC [in/out] Valid garbage collector to use, or nil. If nil a
      new garbage collector is created and stored in LocalGC.
    @param Obj [in] Object to place under garbage collection. Obj is added
      to LocalGC and freed when LocalGC goes out of scope.
  }
begin
  GetGC(LocalGC);
  (LocalGC as IGC).AddObject(Obj);
end;

class procedure TGC.GetGC(var GC: IInterface);
  {Ensures that a local garbage collector is instantiated.
    @param GC [in/out] Garbage collector. If nil a new garbage collector will be
      created and stored in GC. If GC is not nil it is checked for validity and
      left unchanged.
  }
begin
  Assert(not Assigned(GC) or Supports(GC, IGC),
    ClassName + '.GetGC: GC is not valid - must support IGC');
  if not Assigned(GC) then
    GC := TGarbageCollector.Create;
end;

end.

