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
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
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
  Classes;


type

  {
  IDispatchListEnum:
    Enumerator for IDispatchList objects.
  }
  IDispatchListEnum = interface(IInterface)
    ['{EA5F537F-3FEE-46ED-9569-97178446AC07}']
    function GetCurrent: IDispatch;
      {Gets current string in enumeration.
        @return Current string.
      }
    function MoveNext: Boolean;
      {Moves to next item in enumeration.
        @return True if there is a next item, False if enumeration completed.
      }
    property Current: IDispatch read GetCurrent;
      {Current item in enumeration}
  end;

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
    function GetEnumerator: IDispatchListEnum;
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
    fList: TInterfaceList;
      {Object list}
    type
      {
      TEnumerator:
        Implements enumerator for IDispatchList.
      }
      TEnumerator = class(TInterfacedObject, IDispatchListEnum)
      strict private
        fList: IDispatchList;
          {Reference to object being enumerated}
        fIndex: Integer;
          {Index of current item in enumeration}
      protected
        function GetCurrent: IDispatch;
          {Gets current string in enumeration.
            @return Current string.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, False if enumeration
              completed.
          }
      public
        constructor Create(const List: IDispatchList);
          {Class constructor. Sets up and initialises enumeration.
            @param List [in] Reference to object to be enumerated.
          }
      end;
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
    function GetEnumerator: IDispatchListEnum;
      {Creates an enumerator for the dispatch list.
        @return Enumerator instance.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
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
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fList := TInterfaceList.Create;
end;

function TDispatchList.GetEnumerator: IDispatchListEnum;
  {Creates an enumerator for the dispatch list.
    @return Enumerator instance.
  }
begin
  Result := TEnumerator.Create(Self);
end;

function TDispatchList.GetItem(const Idx: Integer): IDispatch;
  {Gets IDispatch interface of object at specified element in list.
    @param Idx [in] Index of required element.
    @return IDispatch of element at Idx.
  }
begin
  Result := fList[Idx] as IDispatch;
end;

{ TDispatchList.TEnumerator }

constructor TDispatchList.TEnumerator.Create(const List: IDispatchList);
  {Class constructor. Sets up and initialises enumeration.
    @param List [in] Reference to object to be enumerated.
  }
begin
  inherited Create;
  fIndex := -1;
  fList := List;
end;

function TDispatchList.TEnumerator.GetCurrent: IDispatch;
  {Gets current string in enumeration.
    @return Current string.
  }
begin
  Result := fList[fIndex];
end;

function TDispatchList.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, False if enumeration completed.
  }
begin
  Result := fIndex < Pred(fList.Count);
  if Result then
    Inc(fIndex);
end;

end.

