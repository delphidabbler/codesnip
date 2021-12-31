{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Set of classes that can register and provide instances of appropriate
 * protocol handler classes according to a URL's protocol. Also provides base
 * class for all protocol implementations.
}


unit UProtocols;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UBaseObjects, UExceptions;


type
  ///  <summary>
  ///  Abstract base class for all URL protocol handler classes.
  ///  </summary>
  TProtocol = class abstract(TObject)
  strict private
    var
      ///  <summary>Value of URL property.</summary>
      fURL: string;
  strict protected
    ///  <summary>URL specifying a protocol and a resource.</summary>
    property URL: string read fURL;
  public
    ///  <summary>Object constructor. Creates protocol handler for given URL's
    ///  protocol.</summary>
    constructor Create(const URL: string);
    ///  <summary>Checks if this protocol handler handles a given URL's
    ///  protocol.</summary>
    class function SupportsProtocol(const URL: string): Boolean;
      virtual; abstract;
    ///  <summary>Attempts to execute a resource according to its protocol.
    ///  Descendants must implement.</summary>
    function Execute: Boolean; virtual; abstract;
  end;

type
  ///  <summary>
  ///  Factory class that creates and registers various TProtocol descendant
  ///  objects that can handle various supported URL protocols.
  ///  </summary>
  TProtocolFactory = class(TNoConstructObject)
  strict private
    type
      ///  <summary>Class reference for TProtocol descendant classes.</summary>
      TProtocolClass = class of TProtocol;
    type
      ///  <summary>Implements a register of URL protocol handlers that can be
      ///  instantiated by protocol factory class.</summary>
      TProtocolRegistrar = class(TObject)
      strict private
        var
          ///  <summary>List of registered TProtocolClass references.</summary>
          fRegister: TList<TProtocolClass>;
        ///  <summary>Getter for Count property.</summary>
        function GetCount: Integer;
        ///  <summary>Getter for indexed Protocols property.</summary>
        function GetProtocol(const Idx: Integer): TProtocolClass;
      public
        ///  <summary>Object constructor. Sets up empty register.</summary>
        constructor Create;
        ///  <summary>Object destructor. Tears down object.</summary>
        destructor Destroy; override;
        ///  <summary>Gets instance of the registrar's enumerator.</summary>
        function GetEnumerator: TEnumerator<TProtocolClass>;
        ///  <summary>Registers the given protocol class.</summary>
        procedure RegisterProtocol(const ClassRef: TProtocolClass);
        ///  <summary>Indexed list of registered protocol classes.</summary>
        property Protocols[const Idx: Integer]: TProtocolClass
          read GetProtocol; default;
        ///  <summary>Number of registered protocols.</summary>
        property Count: Integer read GetCount;
      end;
    ///  <summary>Gets singleton instance of protocol registrar.</summary>
    class function Registrar: TProtocolRegistrar;
    class var
      ///  <summary>Protocol registrar singleton.</summary>
      fRegistrar: TProtocolRegistrar;
  public
    ///  <summary>Class constructor. Creates registrar singleton.</summary>
    class constructor Create;
    ///  <summary>Class destructor. Frees registrar singleton.</summary>
    class destructor Destroy;
    ///  <summary>Creates an appropriate concrete TProtocol class instance for
    ///  given URL's protocol.</summary>
    ///  <remarks>If no handler has been registered for the URL's protocol a nul
    ///  handler instance is returned.</remarks>
    class function CreateHandler(const URL: string): TProtocol;
    ///  <summary>Registers a protocol class with the factory.</summary>
    class procedure RegisterProtocol(const ClassRef: TProtocolClass);
  end;

type
  ///  <summary>
  ///  Exception class for use in TProtocol descendant classes.
  ///  </summary>
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
  the program also defines its own "fake" protocols that have special meaning.

  The technique used is to define a series of classes that descend from the
  TProtocol abstract class. There is a separate class for each supported
  protocol and each class knows how to handle its own protocol. Each class
  registers itself with TProtocolFactory.

  TProtocol.SupportsProtocol must return True if the class handles a protocol
  or False if not. The first of the registered TProtocol sub classes to return
  True from its SupportsProtocol method is used to handle the protocol and its
  TProtcol.Execute method is called.

  The TProtocol.Execute method must return True if it handles a protocol and
  wishes to inhibit further processing. Code using the protocol classes must
  prevent the browser from displaying the URL if Execute returns True but must
  allow the browser control to process the URL normally if Execute returns
  False.

  A static factory class is provided that analyses URLs and creates the
  appropriate TProtocol object, based on the URL's protocol and the registered
  handler classes.
}


type
  ///  <summary>
  ///  Nul protocol used when there is no suitable handler for a protocol.
  ///  </summary>
  ///  <remarks>
  ///  This object is used to indicate that default handling of protocol by the
  ///  browser control is to be permitted. To this end the Execute method does
  ///  nothing and returns false to indicate the protocol was not handled and
  ///  SupportsProtocol returns false because this handler recognises no
  ///  protocols.
  ///  </remarks>
  TNulProtocolHandler = class(TProtocol)
  public
    ///  <summary>Checks if this protocol handler handles a URL's protocol.
    ///  </summary>
    ///  <remarks>This class handles no protocols so always returned False.
    ///  </remarks>
    class function SupportsProtocol(const URL: string): Boolean; override;
    ///  <summary>Does nothing because no protocol is handled.</summary>
    ///  <returns>Boolean. Always returns False.</returns>
    function Execute: Boolean; override;
  end;


{ TProtocolFactory }

class constructor TProtocolFactory.Create;
begin
  fRegistrar := TProtocolRegistrar.Create;
end;

class function TProtocolFactory.CreateHandler(
  const URL: string): TProtocol;
var
  RegisteredCls: TProtocolClass;  // enumerates registered protocol classes
begin
  for RegisteredCls in Registrar do
    if RegisteredCls.SupportsProtocol(URL) then
      Exit(RegisteredCls.Create(URL));
  Result := TNulProtocolHandler.Create('');
end;

class destructor TProtocolFactory.Destroy;
begin
  fRegistrar.Free;
end;

class procedure TProtocolFactory.RegisterProtocol(
  const ClassRef: TProtocolClass);
begin
  Registrar.RegisterProtocol(ClassRef);
end;

class function TProtocolFactory.Registrar: TProtocolRegistrar;
begin
  Result := fRegistrar;
end;

{ TProtocolFactory.TProtocolRegistrar }

constructor TProtocolFactory.TProtocolRegistrar.Create;
begin
  inherited Create;
  fRegister := TList<TProtocolClass>.Create;
end;

destructor TProtocolFactory.TProtocolRegistrar.Destroy;
begin
  fRegister.Free;
  inherited;
end;

function TProtocolFactory.TProtocolRegistrar.GetCount: Integer;
begin
  Result := fRegister.Count;
end;

function TProtocolFactory.TProtocolRegistrar.GetEnumerator:
  TEnumerator<TProtocolClass>;
begin
  Result := fRegister.GetEnumerator;
end;

function TProtocolFactory.TProtocolRegistrar.GetProtocol(
  const Idx: Integer): TProtocolClass;
begin
  Result := fRegister[Idx];
end;

procedure TProtocolFactory.TProtocolRegistrar.RegisterProtocol(
  const ClassRef: TProtocolClass);
begin
  fRegister.Add(ClassRef);
end;

{ TProtocol }

constructor TProtocol.Create(const URL: string);
begin
  inherited Create;
  fURL := URL;
end;

{ TNulProtocolHandler }

function TNulProtocolHandler.Execute: Boolean;
begin
  Result := False;
end;

class function TNulProtocolHandler.SupportsProtocol(const URL: string): Boolean;
begin
  Result := False;
end;

end.

