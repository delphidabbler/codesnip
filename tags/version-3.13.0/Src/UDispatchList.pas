{
 * UDispatchList.pas
 *
 * Interface to list of IDispatch objects along with an implementation of the
 * list and an enumerator.
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
 * The Original Code is UDispatchList.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UDispatchList;


interface


uses
  // Delphi
  Generics.Collections;


type

  {
  IDispatchList:
    Interface of object that implements a list IDispatch objects.
  }
  IDispatchList = interface(IInterface)
    ['{AA577EFC-B826-4DA8-8A16-45AB9EB84F03}']
    function Add(const Obj: IDispatch): Integer;
      {Adds an object to the list.
        @param Obj [in] Object to be added to the list.
        @return Index element where object was added.
      }
    function Count: Integer;
      {Gets number of items in the list.
        @return Number of items.
      }
    function GetItem(const Idx: Integer): IDispatch;
      {Gets IDispatch interface of object at specified element in list.
        @param Idx [in] Index of required element.
        @return IDispatch of element at Idx.
      }
    function GetEnumerator: TEnumerator<IDispatch>;
      {Creates an enumerator for the dispatch list.
        @return Enumerator instance.
      }
    property Items[const Idx: Integer]: IDispatch
      read GetItem; default;
      {List of IDispatch objects}
  end;

  {
  TDispatchList:
    Implements a list of objects that support the IDispatch interface.
  }
  TDispatchList = class(TInterfacedObject,
    IDispatchList
  )
  strict private
    var fList: TList<IDispatch>;  // Dispatch object list
  protected
    { IDispatchList methods }
    function Add(const Obj: IDispatch): Integer;
      {Adds an object to the list.
        @param Obj [in] Object to be added to the list.
        @return Index element where object was added.
      }
    function Count: Integer;
      {Gets number of items in the list.
        @return Number of items.
      }
    function GetItem(const Idx: Integer): IDispatch;
      {Gets IDispatch interface of object at specified element in list.
        @param Idx [in] Index of required element.
        @return IDispatch of element at Idx.
      }
    function GetEnumerator: TEnumerator<IDispatch>;
      {Creates an enumerator for the dispatch list.
        @return Enumerator instance.
      }
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
  end;


implementation


{ TDispatchList }

function TDispatchList.Add(const Obj: IDispatch): Integer;
  {Adds an object to the list.
    @param Obj [in] Object to be added to the list.
    @return Index element where object was added.
  }
begin
  Result := fList.Add(Obj);
end;

function TDispatchList.Count: Integer;
  {Gets number of items in the list.
    @return Number of items.
  }
begin
  Result := fList.Count;
end;

constructor TDispatchList.Create;
  {Object constructor. Sets up object.
  }
begin
  inherited;
  fList := TList<IDispatch>.Create;
end;

destructor TDispatchList.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fList.Free;
  inherited;
end;

function TDispatchList.GetEnumerator: TEnumerator<IDispatch>;
  {Creates an enumerator for the dispatch list.
    @return Enumerator instance.
  }
begin
  Result := fList.GetEnumerator;
end;

function TDispatchList.GetItem(const Idx: Integer): IDispatch;
  {Gets IDispatch interface of object at specified element in list.
    @param Idx [in] Index of required element.
    @return IDispatch of element at Idx.
  }
begin
  Result := fList[Idx];
end;

end.

