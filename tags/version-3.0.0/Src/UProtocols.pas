{
 * UProtocols.pas
 *
 * Set of classes that can register and provide instances of appropriate
 * protocol handler classes according to a URL's protocol. Also provides base
 * class for all protocol implementations.
 *
 * v1.0 of 25 Oct 2006  - Original version.
 * v2.0 of 14 Nov 2006  - Complete rewrite.
 *                      - Renamed unit from UProtocolHandler to UProtocols.
 *                      - Renamed TProtocolHandler as TProtocol.
 *                      - Removed IProtocolHandler and removed from protocol
 *                        handler classes.
 *                      - Moved code from THTTPProtocolHandler and
 *                        TShellExecProtocolHandler to new UHTTProtocol unit
 *                        where both classes were merged as THTTPProtocol.
 *                      - Removed now unused TExecuteProtocolHandler class.
 *                      - Renamed TProtocolHandlerFactory as TProtocolFactory.
 *                      - Added new TProtocolRegistrar and TProtocolRegisterItem
 *                        private classes to manage registered protocol classes
 *                        along with private singleton instance of
 *                        TProtocolRegistrar.
 *                      - Changed TProtocolHandlerFactory to work with
 *                        registered protocol classes rather than hard wired
 *                        protocol classes. This lets new protocols be added
 *                        without modifying this unit.
 * v2.1 of 04 Nov 2007  - Removed redundant uses clause reference to removed
 *                        UHelpTopicAction unit along with other unused units.
 * v2.2 of 04 Oct 2008  - Changed TProtocolFactory to derive from
 *                        TNoConstructObject and hence prevented it from being
 *                        constructed.
 *                      - Made private and protected sections of various classes
 *                        strict.
 *                      - Now use ClassName method in assert statement.
 *
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
 * The Original Code is UProtocols.pas (formerly UProtocolHandler.pas).
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UProtocols;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TProtocolClass:
    Class reference for TProtocol descendant classes.
  }
  TProtocolClass = class of TProtocol;


  {
  TProtocol:
    Abstract base class for all URL protocol handler classes.
  }
  TProtocol = class(TObject)
  strict private
    fURL: string;
      {Value of URL property}
  strict protected
    property URL: string read fURL;
      {URL specifying a protocol and a resource}
  public
    constructor Create(const URL: string);
      {Class constructor. Creates protocol handler for a specified resource.
        @param URL [in] URL specifying protocol and resource.
      }
    function Execute: Boolean; virtual; abstract;
      {Attempts to execute a resource. Descendants must implement.
        @return True if resource could be handled (executed) or false if not.
      }
  end;

  {
  TProtocolFactory:
    Factory class that creates appropriate TProtocol descendant objects that can
    handle various supported URL protocols.
  }
  TProtocolFactory = class(TNoConstructObject)
  public
    class function CreateHandler(const URL: string): TProtocol;
      {Creates an appropriate TProtocol class instance for a protocol defined
      in a URL.
        @param URL [in] URL of a resource, prefixed by appropriate protocol.
        @return Protocol handler for URL's protocol. If no handler is available
          for the resource a nul handler instance is returned.
      }
    class procedure RegisterProtocol(const Name: string;
      const ClassRef: TProtocolClass);
      {Registers a protocol with the factory class.
        @param Name [in] Name of protocol.
        @param ClassRef [in] Reference to class implementing protocol.
      }
  end;


implementation


{
  NOTES
  -----

  In this program we don't want the browser control to perform its default
  activity when certain URLs are clicked, that is we don't want the browser
  control to display the referenced URL in its window. We want to intercept
  the URL and perform special processing depending on the URL's protocol.

  In addition to performing custom processing on some standard URL protocols,
  the program also defines its own "fake" protocols that have special meaning
  within the program.

  The technique used is to define a series of classes that descend from the
  TProtocol abstract class. There is a separate class for each supported
  protocol and each class knows how to handle its own protocol.

  The TProtocol.Execute method must return true if it handles a protocol and
  false if not. Code using the protocol classes must prevent the browser from
  displaying the URL if Execute returns true but must allow the browser control
  to process the URL normally if Execute returns false.

  A static factory class is provided that analyses URLs and creates the
  appropriate TProtocol object, based on the URL's protocol.

  Sub-classes of TProtocol must register themselves with the factory class by
  calling its RegisterProtocol method and passing the name of the supported
  protocol and a class reference. In order to do this implementations expose a
  constructor that accepts a URL as its only parameter. If an implementation
  supports more than one protocol it must register itself separately for each
  supported protocol.

  Here is some boilerplate code for a protocol implementation:

    TMyProtocol = class(TProtocol)
    public
      function Execute: Boolean; override;
    end;
    ...
    function TMyProtocol.Execute: Boolean;
    begin
      Result := True;
      // Handle protocol here
    end;
    ...
    initialization

    TProtocolFactory.RegisterProtocol('protocolname', TMyProtocol);

    end.
}


uses
  // Delphi
  SysUtils, StrUtils, Contnrs;


type

  {
  TNulProtocolHandler:
    Nul protocol used when there is no suitable handler for a protocol. This
    object is used to indicate that default handling of protocol by the browser
    control is to be permitted. To this end the Execute method does nothing and
    returns false to indicate the protocol was not handled.
  }
  TNulProtocolHandler = class(TProtocol)
  public
    function Execute: Boolean; override;
      {Does nothing.
        @return False.
      }
  end;

  {
  TProtocolRegisterItem:
    Class that represents a protocol registered in protocol registrar.
    Effectively maps a protocol name to its implementing class.
  }
  TProtocolRegisterItem = class(TObject)
  strict private
    fName: string;
      {Value of Protocol property}
    fClassRef: TProtocolClass;
      {Value of ProtocolClass property}
  public
    constructor Create(const Name: string;
      const ClassRef: TProtocolClass);
      {Class constructor. Sets object's properties.
        @param Name [in] Name of protocol.
        @param ClassRef [in] Reference to class that supports the protocol.
      }
    property Name: string read fName;
      {Name of URL protocol}
    property ClassRef: TProtocolClass read fClassRef;
      {Type of class that implements support for protocol}
  end;

  {
  TProtocolRegistrar:
    Class that implements register of URL protocol handlers that can be
    instantiated by protocol factory class.
  }
  TProtocolRegistrar = class(TObject)
  strict private
    fRegister: TObjectList;
      {List of items stored in registry}
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of registered protocols.
      }
    function GetProtocol(const Idx: Integer): TProtocolRegisterItem;
      {Read accessor for Protocols property.
        @param Idx [in] Index of protocol in registry.
        @return Instance of indexed protocol register item.
      }
  public
    constructor Create;
      {Class constructor. Sets up empty registry.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure RegisterProtocol(const Name: string;
      const ClassRef: TProtocolClass);
      {Registers a protocol.
        @param Name [in] Name of protocol.
        @param ClassRef [in] Class that implements support for the protocol.
      }
    property Protocols[const Idx: Integer]: TProtocolRegisterItem
      read GetProtocol; default;
      {List of regsitered protocols}
    property Count: Integer read GetCount;
      {Number of registered protocols}
  end;

var
  // Private protocol registrar singletion
  PvtRegistrar: TProtocolRegistrar = nil;

{ TProtocolFactory }

class function TProtocolFactory.CreateHandler(
  const URL: string): TProtocol;
  {Creates an appropriate TProtocol class instance for a protocol defined in a
  URL.
    @param URL [in] URL of a resource, prefixed by appropriate protocol.
    @return Protocol handler for URL's protocol. If no handler is available for
      the resource a nul handler instance is returned.
  }
var
  Idx: Integer; // loops thru registered protocols
begin
  // Check for supported protocols
  Result := nil;
  for Idx := 0 to Pred(PvtRegistrar.Count) do
  begin
    if AnsiStartsStr(PvtRegistrar[Idx].Name, URL) then
    begin
      Result := PvtRegistrar[Idx].ClassRef.Create(URL);
      Break;
    end;
  end;
  if not Assigned(Result) then
    // Protocol not supported: use nul protocol to indicate normal handling.
    Result := TNulProtocolHandler.Create('');
end;

class procedure TProtocolFactory.RegisterProtocol(const Name: string;
  const ClassRef: TProtocolClass);
  {Registers a protocol with the factory class.
    @param Name [in] Name of protocol.
    @param ClassRef [in] Reference to class implementing protocol.
  }
begin
  PvtRegistrar.RegisterProtocol(Name, ClassRef);
end;

{ TProtocol }

constructor TProtocol.Create(const URL: string);
  {Class constructor. Creates protocol handler for a specified resource.
    @param URL [in] URL specifying protocol and resource.
  }
begin
  inherited Create;
  fURL := URL;
end;

{ TNulProtocolHandler }

function TNulProtocolHandler.Execute: Boolean;
  {Does nothing.
    @return False.
  }
begin
  Result := False;
end;

{ TProtocolRegistrar }

constructor TProtocolRegistrar.Create;
  {Class constructor. Sets up empty registry.
  }
begin
  inherited Create;
  fRegister := TObjectList.Create(True);
end;

destructor TProtocolRegistrar.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fRegister);  // frees owned objects
  inherited;
end;

function TProtocolRegistrar.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of registered protocols.
  }
begin
  Result := fRegister.Count;
end;

function TProtocolRegistrar.GetProtocol(
  const Idx: Integer): TProtocolRegisterItem;
  {Read accessor for Protocols property.
    @param Idx [in] Index of protocol in registry.
    @return Instance of indexed protocol register item.
  }
begin
  Result := fRegister[Idx] as TProtocolRegisterItem;
end;

procedure TProtocolRegistrar.RegisterProtocol(const Name: string;
  const ClassRef: TProtocolClass);
  {Registers a protocol.
    @param Name [in] Name of protocol.
    @param ClassRef [in] Class that implements support for the protocol.
  }
begin
  fRegister.Add(TProtocolRegisterItem.Create(Name, ClassRef));
end;

{ TProtocolRegisterItem }

constructor TProtocolRegisterItem.Create(const Name: string;
  const ClassRef: TProtocolClass);
  {Class constructor. Sets object's properties.
    @param Name [in] Name of protocol.
    @param ClassRef [in] Reference to class that supports the protocol.
  }
begin
  inherited Create;
  Assert(Assigned(ClassRef),                               // ** do not localise
    ClassName + '.Create: ClassRef is nil');
  fName := Name;
  fClassRef := ClassRef;
end;

initialization

// Instantiate protocol registrar singleton
PvtRegistrar := TProtocolRegistrar.Create;

finalization

// Free registrar singleton
FreeAndNil(PvtRegistrar);

end.

