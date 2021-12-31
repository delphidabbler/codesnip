{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Interface to list of IDispatch objects along with an implementation of the
 * list and an enumerator.
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
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
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

