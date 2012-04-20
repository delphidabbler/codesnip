{
 * UProtocols.pas
 *
 * Set of classes that can register and provide instances of appropriate
 * protocol handler classes according to a URL's protocol. Also provides base
 * class for all protocol implementations.
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
 * The Original Code is UProtocols.pas (formerly UProtocolHandler.pas).
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UProtocols;


interface


uses
  // Delphi
  SysUtils, Contnrs,
  // Project
  UBaseObjects, UExceptions;


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
  TProtocol = class abstract(TObject)
  strict private
    fURL: string;   // Value of URL property
  strict protected
    property URL: string read fURL;
      {URL specifying a protocol and a resource}
  public
    constructor Create(const URL: string);
      {Class constructor. Creates protocol handler for a specified resource.
        @param URL [in] URL specifying protocol and resource.
      }
    class function SupportsProtocol(const URL: string): Boolean;
      virtual; abstract;
      {Checks if this protocol handler handles a URL's protocol.
        @param URL [in] URL whose protocol is to be checked.
        @return True if URL's protocol is supported, False if not.
      }
    function Execute: Boolean; virtual; abstract;
      {Attempts to execute a resource. Descendants must implement.
        @return True if resource could be handled (executed) or false if not.
      }
  end;

  {
  TProtocolFactory:
    Factory class that creates and registers various TProtocol descendant
    objects that can handle various supported URL protocols.
  }
  TProtocolFactory = class(TNoConstructObject)
  strict private
    type
      {
      TProtocolRegistrar:
        Class that implements register of URL protocol handlers that can be
        instantiated by protocol factory class.
      }
      TProtocolRegistrar = class(TObject)
      strict private
        fRegister: TClassList;  // List of registered TProtocolClass references
        function GetCount: Integer;
          {Read accessor for Count property.
            @return Number of registered protocols.
          }
        function GetProtocol(const Idx: Integer): TProtocolClass;
          {Read accessor for Protocols property.
            @param Idx [in] Index of protocol class reference in registry.
            @return Instance of indexed protocol class reference.
          }
        type
          {
          TEnumerator:
            Enumerator for TProtocolRegistrar.
          }
          TEnumerator = class(TObject)
          strict private
            fReg: TProtocolRegistrar; // Reference to register being enumerated
            fIndex: Integer;          // Index of current item in enumeration
          public
            constructor Create(const Reg: TProtocolRegistrar);
              {Class constructor. Sets up enumeration.
                @param Reg [in] Protocol registrar instance being enumerated.
              }
            function GetCurrent: TProtocolClass;
              {Gets current protocol class in enumeration.
                @return Current class.
              }
            function MoveNext: Boolean;
              {Moves to next item in enumeration.
                @return True if there is a next item, false if enumeration
                  completed.
              }
            property Current: TProtocolClass read GetCurrent;
              {Current item in enumeration}
          end;
      public
        constructor Create;
          {Class constructor. Sets up empty registry.
          }
        destructor Destroy; override;
          {Class destructor. Tears down object.
          }
        function GetEnumerator: TEnumerator;
          {Gets an instance of the registrar's enumerator.
            @return Required enumerator.
          }
        procedure RegisterProtocol(const ClassRef: TProtocolClass);
          {Registers a protocol.
            @param ClassRef [in] Class that implements support for the protocol.
          }
        property Protocols[const Idx: Integer]: TProtocolClass
          read GetProtocol; default;
          {List of regsitered protocol classes}
        property Count: Integer read GetCount;
          {Number of registered protocols}
      end;
    class var fGC: IInterface;                // Garbage collector for class var
    class var fRegistrar: TProtocolRegistrar; // Singleton protocol registrar
    class function Registrar: TProtocolRegistrar;
      {Gets singleton instance of protocol registar.
        @return Singleton regsitrar object.
      }
  public
    class function CreateHandler(const URL: string): TProtocol;
      {Creates an appropriate TProtocol class instance for a protocol defined
      in a URL.
        @param URL [in] URL of a resource, prefixed by appropriate protocol.
        @return Protocol handler for URL's protocol. If no handler is available
          for the resource a nul handler instance is returned.
      }
    class procedure RegisterProtocol(const ClassRef: TProtocolClass);
      {Registers a protocol with the factory class.
        @param ClassRef [in] Reference to class implementing protocol.
      }
  end;


  {
  EProtocol:
    Exception class for use in TProtocol descendant classes.
  }
  EProtocol = class(ECodeSnip);


implementation


{
  NOTES
  -----

  In this program we don't want the browser control to perform its default
  activity when certain URLs are clicked, that is we don't want the browser
  control to display the referenced URL in its window. We may want to intercept
  the URL and perform special processing depending on the URL's protocol.

  In addition to performing custom processing on some standard URL protocols,
  the program also defines its own "fake" protocols that have special meaning
  within the program.

  The technique used is to define a series of classes that descend from the
  TProtocol abstract class. There is a separate class for each supported
  protocol and each class knows how to handle its own protocol. Each class
  registers itself with TProtocolFactory.

  TProtocol.SupportsProtocol must return true if the class handles a protocol
  or false if not. The first of the registered TProtocol sub classes to return
  true from it SupportsProtocol method is used to handle the protocol and its
  TProtcol.Execute method is called.

  The TProtocol.Execute method must return true if it handles a protocol and
  wishes to inhibit further processing. Code using the protocol classes must
  prevent the browser from displaying the URL if Execute returns true but must
  allow the browser control to process the URL normally if Execute returns
  false.

  A static factory class is provided that analyses URLs and creates the
  appropriate TProtocol object, based on the URL's protocol.
}


uses
  // Delphi
  Classes {for inlining},
  // Project
  UGC;


type

  {
  TNulProtocolHandler:
    Nul protocol used when there is no suitable handler for a protocol. This
    object is used to indicate that default handling of protocol by the browser
    control is to be permitted. To this end the Execute method does nothing and
    returns false to indicate the protocol was not handled and SupportsProtocol
    returns false because this handler recognises no protocols.
  }
  TNulProtocolHandler = class(TProtocol)
  public
    class function SupportsProtocol(const URL: string): Boolean; override;
      {Checks if this protocol handler handles a URL's protocol. It never does
      so it returns False.
        @param URL [in] URL whose protocol is to be checked. Ignored.
        @return False.
      }
    function Execute: Boolean; override;
      {Does nothing.
        @return False.
      }
  end;

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
  RegisteredCls: TProtocolClass;  // enumerates registered protocol classes
begin
  // Check for supported protocols
  Result := nil;
  for RegisteredCls in Registrar do
  begin
    if RegisteredCls.SupportsProtocol(URL) then
    begin
      Result := RegisteredCls.Create(URL);
      Break;
    end;
  end;
  if not Assigned(Result) then
    // Protocol not supported: use nul protocol to indicate normal handling.
    Result := TNulProtocolHandler.Create('');
end;

class procedure TProtocolFactory.RegisterProtocol(
  const ClassRef: TProtocolClass);
  {Registers a protocol with the factory class.
    @param ClassRef [in] Reference to class implementing protocol.
  }
begin
  Registrar.RegisterProtocol(ClassRef);
end;

class function TProtocolFactory.Registrar: TProtocolRegistrar;
  {Gets singleton instance of protocol registar.
    @return Singleton regsitrar object.
  }
begin
  if not Assigned(fRegistrar) then
  begin
    fRegistrar := TProtocolRegistrar.Create;
    TGC.GCLocalObj(fGC, fRegistrar);
  end;
  Result := fRegistrar;
end;

{ TProtocolFactory.TProtocolRegistrar }

constructor TProtocolFactory.TProtocolRegistrar.Create;
  {Class constructor. Sets up empty registry.
  }
begin
  inherited Create;
  fRegister := TClassList.Create;
end;

destructor TProtocolFactory.TProtocolRegistrar.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fRegister);
  inherited;
end;

function TProtocolFactory.TProtocolRegistrar.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of registered protocols.
  }
begin
  Result := fRegister.Count;
end;

function TProtocolFactory.TProtocolRegistrar.GetEnumerator: TEnumerator;
  {Gets an instance of the registrar's enumerator.
    @return Required enumerator.
  }
begin
  Result := TEnumerator.Create(Self);
end;

function TProtocolFactory.TProtocolRegistrar.GetProtocol(
  const Idx: Integer): TProtocolClass;
  {Read accessor for Protocols property.
    @param Idx [in] Index of protocol class reference in registry.
    @return Instance of indexed protocol class reference.
  }
begin
  Result := TProtocolClass(fRegister[Idx]);
end;

procedure TProtocolFactory.TProtocolRegistrar.RegisterProtocol(
  const ClassRef: TProtocolClass);
  {Registers a protocol.
    @param ClassRef [in] Class that implements support for the protocol.
  }
begin
  fRegister.Add(ClassRef);
end;

{ TProtocolFactory.TProtocolRegistrar.TEnumerator }

constructor TProtocolFactory.TProtocolRegistrar.TEnumerator.Create(
  const Reg: TProtocolRegistrar);
  {Class constructor. Sets up enumeration.
    @param Reg [in] Protocol registrar instance being enumerated.
  }
begin
  inherited Create;
  fReg := Reg;
  fIndex := -1;
end;

function TProtocolFactory.TProtocolRegistrar.TEnumerator.GetCurrent:
  TProtocolClass;
  {Gets current protocol class in enumeration.
    @return Current class.
  }
begin
  Result := fReg[fIndex];
end;

function TProtocolFactory.TProtocolRegistrar.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, false if enumeration completed.
  }
begin
  Result := fIndex < Pred(fReg.Count);
  if Result then
    Inc(fIndex);
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

class function TNulProtocolHandler.SupportsProtocol(const URL: string): Boolean;
  {Checks if this protocol handler handles a URL's protocol. It never does so it
  returns False.
    @param URL [in] URL whose protocol is to be checked. Ignored.
    @return False.
  }
begin
  Result := False;
end;

end.

