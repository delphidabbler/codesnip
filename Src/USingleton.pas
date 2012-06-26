{
 * USingleton.pas
 *
 * Provides a base class for singleton objects along with a manager object that
 * records instances of each type of singleton.
 *
 * Based on by code by Yoav Abrahami see <URL:http://bit.ly/cAH0HO>, updated to
 * take advantage of modern Delphi features: generics, class vars, class
 * constructor and destructor etc. Further updated to use class types instead of
 * class names as dictionary keys following suggestions made in comments on my
 * blog post at <URL:http://bit.ly/d8n9Hq>.
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
 * The Original Code is USingleton.pas
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


unit USingleton;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TSingleton:
    Base class for singleton objects. Only one singleton object of each
    descendant class can be created. Once a subclassed TSingleton object is
    created further attempts to create an instance of that type return the
    already created object. All attempts to destroy singletons will fail,
    although instances are freed correctly when the program closes.
    *** NOTE: TSingleton instances should not be created directly. The class
    should be subclassed.
  }
  TSingleton = class(TConditionalFreeObject)
  strict private
    procedure Dispose;
      {Frees the singleton instance.
      }
  strict protected
    function CanDestroy: Boolean; override;
      {Determines if the object can be destroyed. This is the case only when the
      singleton manager is destroying.
        @return True if object can be destroyed.
      }
    procedure Initialize; virtual;
      {Initialises object. Descendants should override this method instead of
      constructor. Does nothing in this base class.
      }
  public
    class function NewInstance: TObject; override;
      {Creates a new instance of singleton if it doesn't exist. If singleton
      already exists returns existing instance.
        @return Singleton instance.
      }
  end;


{$IFDEF TESTING}
procedure FreeAllSingletons;
  {Frees all singleton instances. Used when testing only.
  }
{$ENDIF}


implementation


uses
  // Delphi
  SysUtils, Generics.Collections;


type

  {
  TSingletonManager:
    Class that records instantiated TSingleton descendant objects. Maintains a
    map of class names to instances that TSingleton uses to decide whether to
    create a new singleton instance. Ensures all singletons are freed when the
    program closes.
  }
  TSingletonManager = class(TNoConstructObject)
  strict private
    class var fDestroying: Boolean;
      {Flag that indicates if manager is destroying singletons}
    class var fMap: TDictionary<TClass,TSingleton>;
      {Map of class names to singleton instances}
  {$IFNDEF TESTING}strict{$ENDIF}
  protected
    class procedure FreeAll;
      {Frees all registered singletons.
      }
    class procedure CreateMap;
      {Create Map object if doesn't exist.
      }
  public
    class constructor Create;
      {Class constructor. Sets up required class vars.
      }
    class destructor Destroy;
      {Class destructor. Frees all singletons.
      }
    class procedure RegisterSingleton(const S: TSingleton);
      {Registers a new singleton object providing it is not already registered.
        @param S [in] Singleton to register.
      }
    class function SingletonExists(const Cls: TClass): Boolean;
      {Checks if a singleton of a certain class already exists.
        @param Name of singleton class.
        @return True if an instance of this class already exists, False if not.
      }
    class function Lookup(const Cls: TClass): TSingleton;
      {Looks up a singleton class name in the map.
        @param ClsName [in] Name of requested singleton class.
        @return Required singleton instance.
        @except EListError raised if there is no singleton instance with the
          requested class name.
      }
    class property Destroying: Boolean read fDestroying write fDestroying;
      {Indicates if the this class is destroying singletons. Singleton instances
      use this property to allow themselves to be destroyed}
  end;

{$IFDEF TESTING}
procedure FreeAllSingletons;
  {Frees all singleton instances. Used when testing only.
  }
begin
  // Can't call class constructor directly so we use following methods.
  // These methods are normally *strict* protected, but relaxed for testing.
  TSingletonManager.FreeAll;
  TSingletonManager.CreateMap;
end;
{$ENDIF}

{ TSingleton }

function TSingleton.CanDestroy: Boolean;
  {Determines if the object can be destroyed. This is the case only when the
  singleton manager is destroying.
    @return True if object can be destroyed.
  }
begin
  Result := TSingletonManager.Destroying;
end;

procedure TSingleton.Dispose;
  {Frees the singleton instance.
  }
begin
  inherited FreeInstance;
end;

procedure TSingleton.Initialize;
  {Initialises object. Descendants should override this method instead of
  constructor. Does nothing in this base class.
  }
begin
  // Override to initialise code that would normally be placed in constructor
end;

class function TSingleton.NewInstance: TObject;
  {Creates a new instance of singleton if it doesn't exist. If singleton already
  exists returns existing instance.
    @return Singleton instance.
  }
var
  S: TSingleton;  // reference to a new singleton
begin
  if not TSingletonManager.SingletonExists(Self) then
  begin
    S := TSingleton(inherited NewInstance);
    try
      S.Initialize;
      TSingletonManager.RegisterSingleton(S);
    except
      S.Dispose;
      raise;
    end;
  end;
  Result := TSingletonManager.Lookup(Self);
end;

{ TSingletonManager }

class constructor TSingletonManager.Create;
  {Class constructor. Sets up required class vars.
  }
begin
  CreateMap;
end;

class procedure TSingletonManager.CreateMap;
  {Create Map object if doesn't exist.
  }
begin
  if not Assigned(fMap) then
    fMap := TDictionary<TClass,TSingleton>.Create;
end;

class destructor TSingletonManager.Destroy;
  {Class destructor. Frees all singletons.
  }
begin
  FreeAll;
end;

class procedure TSingletonManager.FreeAll;
  {Frees all registered singletons.
  }
var
  Singleton: TSingleton;  // each singleton in map
begin
  // indicate to singletons they can destroy
  Destroying := True;
  // free the singletons in the map, then the map itself
  for Singleton in fMap.Values do
    Singleton.Free;
  FreeAndNil(fMap);
  Destroying := False;
  // setting fMap nil and Destroying False make it safe to re-create map when
  // testing
end;

class function TSingletonManager.Lookup(const Cls: TClass): TSingleton;
  {Looks up a singleton class name in the map.
    @param ClsName [in] Name of requested singleton class.
    @return Required singleton instance.
    @except EListError raised if there is no singleton instance with the
      requested class name.
  }
begin
  Result := fMap[Cls];
end;

class procedure TSingletonManager.RegisterSingleton(const S: TSingleton);
  {Registers a new singleton object providing it is not already registered.
    @param S [in] Singleton to register.
  }
begin
  if not SingletonExists(S.ClassType) then
    fMap.Add(S.ClassType, S);
end;

class function TSingletonManager.SingletonExists(
  const Cls: TClass): Boolean;
  {Checks if a singleton of a certain class already exists.
    @param Name of singleton class.
    @return True if an instance of this class already exists, False if not.
  }
begin
  Result := fMap.ContainsKey(Cls);
end;

end.

