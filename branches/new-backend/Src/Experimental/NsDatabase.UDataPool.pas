{
 * NsDatabase.UDataPool.pas
 *
 * Implements a generic data pool object used to store various database objects.
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
 * The Original Code is NsDatabase.UDataPool.pas
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


unit NsDatabase.UDataPool;

interface

uses
  // Delphi
  SysUtils, Generics.Collections,
  // Project
  NsDatabase.UCookies, NsDatabase.UDataItem,
  UBaseObjects, UExceptions;

type

  EDBDataPoolError = class(EBug);


  {
  TDBDataPool:
    Generic object that implements a pool of data item objects. Designed for use
    in storing database objects that are identified by a cookie and cannot be
    freed by users of the objects. Objects placed in the pool can only be freed
    when the pool permits it.
  }
  TDBDataPool<T: TDBDataItem> = class(TOwnedConditionalFreeObject)
  strict private
    type
      // Object that controls whether a data item can be freed
      TFreeController = class(TNonRefCountedObject, IConditionalFreeController)
      strict private
        fObjectToFree: TObject;
      public
        function PermitDestruction(const Obj: TObject): Boolean;
        property ObjectToFree: TObject read fObjectToFree write fObjectToFree;
      end;
  strict private
    fFreeController: TFreeController; // Controls freeing of data items
    fMap: TDictionary<TDBCookie,T>;   // Maps cookies to data item object
    function GetItem(const Cookie: TDBCookie): T;
      {Gets reference to object associated with a cookie in the pool.
        @param Cookie [in] Cookie to search for.
        @return Required object reference.
        @except Raises EDBDataPoolError if cookie is not in pool.
      }
    function GetCookies: TEnumerable<TDBCookie>;
      {Gets reference to cookies collection from pool. Collection will be freed
      when pool object is freed.
        @return Required collection.
      }
    procedure FreeObject(Obj: T);
      {Frees a data item.
        @param Obj [in] Data item object to be freed.
      }
  public
    constructor Create(const Capacity: Integer = 0);
      {Constructor. Optionally sets pool's capacity.
        @param Capacity [in] Optional pool capacity.
      }
    destructor Destroy; override;
      {Destructor. Clears pools and frees contained objects.
      }
    function IsInPool(const Cookie: TDBCookie): Boolean;
      {Checks if a cookie exists in pool.
        @param Cookie [in] Cookie to check.
        @return True if cookie exists, False otherwise.
      }
    procedure Add(const Obj: T);
      {Adds object to pool.
        @param Obj [in] Object to add to pool.
        @except Raises EDBDataPoolError if object's cookie is already in pool.
      }
    procedure Remove(const Cookie: TDBCookie);
      {Removes object with given cookie from pool and frees it.
        @param Cookie of object to be removed.
        @except Raises EDBDataPoolError if Cookie not found.
      }
    procedure Clear;
      {Clears pool and frees contained objects.
      }
    function Count: Integer;
      {Gets number of objects in pool.
        @return Required number of objects.
      }
    function GetEnumerator: TEnumerator<T>;
      {Gets enumerator for the pool. Enumerates values. Cookies are not
      enumerated but can be retrieved from value's Cookie property.
        @return Required enumerator.
      }
    property Items[const Cookie: TDBCookie]: T read GetItem; default;
      {Gets object associated with a cookie from pool}
    property Cookies: TEnumerable<TDBCookie> read GetCookies;
      {Gets an enumerable collection of cookies from pool}
  end;

resourcestring  // must be declared in interface: used in parameterised type
  sObjectPoolCookieExists = 'Object with same cookie already exists';
  sObjectPoolCookieMissing = 'No object with given cookie found';
  sObjectPoolCookieIsNul = 'Attempt to add data item with nul cookie to pool';


implementation

uses
  Generics.Defaults;


{ TDBDataPool<T> }

procedure TDBDataPool<T>.Add(const Obj: T);
  {Adds object to pool.
    @param Obj [in] Object to add to pool.
    @except Raises EDBDataPoolError if object's cookie is already in pool.
  }
begin
  if Obj.Cookie.IsNul then
    raise EDBDataPoolError.Create(sObjectPoolCookieIsNul);
  if IsInPool(Obj.Cookie) then
    raise EDBDataPoolError.Create(sObjectPoolCookieExists);
  fMap.Add(Obj.Cookie, Obj);
  Obj.FreeController := fFreeController;
end;

procedure TDBDataPool<T>.Clear;
var
  Value: T;
begin
  for Value in fMap.Values do
    FreeObject(Value);
  fMap.Clear;
end;

function TDBDataPool<T>.Count: Integer;
begin
  Result := fMap.Count;
end;

constructor TDBDataPool<T>.Create(const Capacity: Integer);
begin
  inherited Create;
  fMap := TDictionary<TDBCookie, T>.Create(
    Capacity,
    TEqualityComparer<TDBCookie>.Construct(
      // equality test
      function(const Left, Right: TDBCookie): Boolean
      begin
        Result := Left = Right;
      end,
      // hash
      function(const Value: TDBCookie): Integer
      begin
        Result := Value.Hash;
      end
    )
  );
  fFreeController := TFreeController.Create;
end;

destructor TDBDataPool<T>.Destroy;
begin
  Clear;
  fMap.Free;
  fFreeController.Free;
  inherited;
end;

procedure TDBDataPool<T>.FreeObject(Obj: T);
begin
  fFreeController.ObjectToFree := Obj;
  try
    Obj.Free;
  finally
    fFreeController.ObjectToFree := nil;
  end;
end;

function TDBDataPool<T>.GetCookies: TEnumerable<TDBCookie>;
begin
  Result := fMap.Keys;
end;

function TDBDataPool<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := TDictionary<TDBCookie,T>.TValueEnumerator.Create(fMap);
end;

function TDBDataPool<T>.GetItem(const Cookie: TDBCookie): T;
begin
  if not fMap.TryGetValue(Cookie, Result) then
    raise EDBDataPoolError.Create(sObjectPoolCookieMissing);
end;

function TDBDataPool<T>.IsInPool(const Cookie: TDBCookie): Boolean;
begin
  Result := fMap.ContainsKey(Cookie);
end;

procedure TDBDataPool<T>.Remove(const Cookie: TDBCookie);
var
  Obj: T;
begin
  Obj := GetItem(Cookie);
  fMap.Remove(Cookie);
  FreeObject(Obj);
end;

{ TDBDataPool<T>.TFreeController }

function TDBDataPool<T>.TFreeController.PermitDestruction(
  const Obj: TObject): Boolean;
begin
  Result := Assigned(fObjectToFree) and (Obj = fObjectToFree);
end;

end.

