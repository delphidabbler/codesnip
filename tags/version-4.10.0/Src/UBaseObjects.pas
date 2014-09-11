{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Contains various common classes for use as base classes to other classes.
 * Classes are:
 *
 *   1) TNoConstructObject:
 *      Object that fails if its constructor is called. Provided as a base class
 *      for classes that contains only class methods and should never be
 *      constructed.
 *
 *   2) TNoPublicConstructObject:
 *      Object that provides a protected constructor but fails if the public
 *      constructor is called. For use as a base class for objects that are
 *      constructed via class methods but should not be constructed directly.
 *
 *   3) TNoPublicConstructIntfObject:
 *      Same as 2) except class descends from TInterfacedObject instead of
 *      TObject.
 *
 *   4) TNonRefCountedObject implements a non reference counted implementation
 *      of IInterface.
 *
 *   5) TAggregatedOrLoneObject is a base class for objects that can either
 *      exist as aggregated objects or as stand-alone reference counted objects.
 *      This implementation is based on code suggested by Hallvard VossBotn, as
 *      presented in Eric Harmon's book "Delphi COM programming".
 *
 *   6) TConditionalFreeObject:
 *      An abstract base class for objects that cannot be destroyed unless some
 *      condition is met.
 *
 * Also provides an interface - INoPublicConstruct - that can be supported by
 * objects that don't allow public construction but cannot inherited from
 * TNoPublicConstructObject, for example forms.
}


unit UBaseObjects;


interface


type

  {
  TNoConstructObject:
    Class that raises an exception if its constructor is called. Provided as a
    base class for classes that contain only class methods and should never be
    constructed.
  }
  TNoConstructObject = class(TObject)
  public
    constructor Create;
      {Prevents construction of an object instance by raising an
      ENoConstructException if called.
      }
  end;

  {
  TNoPublicConstructObject:
    Class that provides a protected constructor but fails if the public
    constructor is called. For use as a base class for objects that are
    constructed via class methods but should not be constructed directly.
  }
  TNoPublicConstructObject = class(TObject)
  strict protected
    constructor InternalCreate;
      {Protected class constructor. Does nothing but call inherited constructor.
      Should be called by class methods of derived classes instead of inherited
      Create.
      }
  public
    constructor Create;
      {Prevents construction of an object instance by raising an
      ENoConstructException if called.
      }
  end;

  {
  TNoPublicConstructIntfObject:
    Class that provides a protected constructor but fails if the public
    constructor is called. For use as a base class for reference counted
    interfaced objects that are constructed via class methods but should not be
    constructed directly.
  }
  TNoPublicConstructIntfObject = class(TInterfacedObject)
  strict protected
    constructor InternalCreate;
      {Protected class constructor. Does nothing but call inherited constructor.
      Should be called by class methods of derived classes instead of inherited
      Create.
      }
  public
    constructor Create;
      {Prevents construction of an object instance by raising an
      ENoConstructException if called.
      }
  end;

  {
  INoPublicConstruct:
    Interface that can be supported by objects that do not permit public
    construction but that cannot descend from TNoPublicConstructObject. Such
    objects must handle their own detection of attempts to call public
    constructor. An example of use is in forms that may not be publically
    constructed. It is also useful to flag some members of a class heirachy as
    no public construct while leaving others able to be constructed.
    The interface provides no additional methods - it exists purely for
    detection purposes.
    To flag an object as no-public-construct, add INoPublicConstruct to its type
    definition then test for it in the public constructor using
    Supports(Self, INoPublicConstruct).
  }
  INoPublicConstruct = interface(IInterface)
    ['{1170DC2C-5B79-453A-94AB-2C35B583F4BD}']
  end;

  {
  TNonRefCountedObject:
    Implements a non reference counted implementation of IInterface. It derives
    directly from TObject rather than TInterfacedObject since most of the
    code of TInterfacedObject is redundant when reference counting not used.
  }
  TNonRefCountedObject = class(TObject, IInterface)
  public
    { IInterface methods }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      {Checks the specified interface is supported by this object. If so
      reference to interface is passed out.
        @param IID [in] Specifies interface being queried.
        @param Obj [out] Reference to interface implementation or nil if not
          supported.
        @result S_OK if interface supported or E_NOINTERFACE if not supported.
      }
    function _AddRef: Integer; stdcall;
      {Called by Delphi when interface is referenced. Reference count is not
      updated.
        @return -1.
      }
    function _Release: Integer; stdcall;
      {Called by Delphi when interface reference goes out of scope. Reference
      count is not updated and instance is never freed.
        @return -1.
      }
  end;

  {
  TAggregatedOrLoneObject:
    Base class for objects that can either exist as aggregated objects
    (as specified by the "implements" directive) or as stand-alone reference
    counted objects. If the IInterface reference of the containing (controller)
    object is passed to the constructor this object behaves as an aggregated
    object and defers to the container object for reference counting and
    interface queries. When nil is passed to the constructor or the
    parameterless constructor is called the object bahaves as a stand alone
    implementation and handles its own reference counting.
  }
  TAggregatedOrLoneObject = class(TInterfacedObject, IInterface)
  strict private
    fController: Pointer;
      {Weak reference to controlling object if aggregated or nil if stand-alone}
    function GetController: IInterface;
      {Returns IInterface of controlling object.
        @return Required IInterface reference. This is the container object if
          aggregated or Self if stand-alone.
      }
  public
    { IInterface redefinitions }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      {Checks whether the specified interface is supported. If so reference to
      interface is passed out. If aggregated then the controller object is
      queried otherwise this object determines if interface is supported.
        @param IID [in] Specifies interface being queried.
        @param Obj [out] Reference to interface implementation or nil if not
          supported.
        @result S_OK if interface supported or E_NOINTERFACE if not supported.
      }
    function _AddRef: Integer; stdcall;
      {Called by Delphi when interface is referenced. If aggregated call is
      passed off to controller, which may or may not perform reference counting.
      Otherwise reference count is incremented.
        @return Updated reference count.
      }
    function _Release: Integer; stdcall;
      {Called by Delphi when interface reference goes out of scope. If
      aggregated call is passed off to controller, which may or may not perform
      reference counting. Otherwise reference count is decremented and object is
      freed if count reaches zero.
        @return Updated reference count.
      }
  public
    constructor Create(const Controller: IInterface); overload; virtual;
      {Class constructor. Creates either an aggregated or stand-alone object.
        @param Controller [in] IInterface reference to containing object if
          aggregated or nil if not aggregated.
      }
    constructor Create; overload; virtual;
      {Class constructor. Creates a stand-alone object. Equivalent to calling
      Create(nil).
      }
    property Controller: IInterface read GetController;
      {Reference to controlling object's IInterface. Non-nil if aggregated or
      nil if stand-alone}
  end;

  {
  TConditionalFreeObject:
    Abstract base class for objects that cannot be destroyed unless some
    condition is met. Descendants must override CanDestroy which must return
    True if the object can be freed. Attempts to free the object when CanDestroy
    returns False fail and the object remains in existance.
  }
  TConditionalFreeObject = class abstract(TObject)
  strict protected
    procedure Finalize; virtual;
      {Tidies up object. Descendants should override this method instead of
      destructor.
      }
    function CanDestroy: Boolean; virtual; abstract;
      {Determines if the object can be destroyed.
        @return True if object can be destroyed.
      }
  public
    destructor Destroy; override;
      {Class destructor tidies up and tears down object only if object can be
      freed.
      }
    procedure FreeInstance; override;
      {Frees instance data only if object can be destroyed.
      }
  end;


implementation

uses
  // Delphi
  SysUtils;


{ TNoConstructObject }

constructor TNoConstructObject.Create;
  {Prevents construction of an object instance by raising an
  ENoConstructException if called.
  }
begin
  raise ENoConstructException.Create(
    ClassName + '.Create: Constructor can''t be called'
  );
end;

{ TNoPublicConstructObject }

constructor TNoPublicConstructObject.Create;
  {Prevents construction of an object instance by raising an
  ENoConstructException if called.
  }
begin
  raise ENoConstructException.Create(
    ClassName + '.Create: Public constructor can''t be called'
  );
end;

constructor TNoPublicConstructObject.InternalCreate;
  {Protected class constructor. Does nothing but call inherited constructor.
  Should be called by class methods of derived classes instead of inherited
  Create.
  }
begin
  inherited Create;
end;

{ TNoPublicConstructIntfObject }

constructor TNoPublicConstructIntfObject.Create;
  {Prevents construction of an object instance by raising an
  ENoConstructException if called.
  }
begin
  raise ENoConstructException.Create(
    ClassName + '.Create: Public constructor can''t be called'
  );
end;

constructor TNoPublicConstructIntfObject.InternalCreate;
  {Protected class constructor. Does nothing but call inherited constructor.
  Should be called by class methods of derived classes instead of inherited
  Create.
  }
begin
  inherited Create;
end;

{ TNonRefCountedObject }

function TNonRefCountedObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
  {Checks the specified interface is supported by this object. If so reference
  to interface is passed out.
    @param IID [in] Specifies interface being queried.
    @param Obj [out] Reference to interface implementation or nil if not
      supported.
    @result S_OK if interface supported or E_NOINTERFACE if not supported.
  }
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TNonRefCountedObject._AddRef: Integer;
  {Called by Delphi when interface is referenced. Reference count is not
  updated.
    @return -1.
  }
begin
  Result := -1;
end;

function TNonRefCountedObject._Release: Integer;
  {Called by Delphi when interface reference goes out of scope. Reference count
  is not updated and instance is never freed.
    @return -1.
  }
begin
  Result := -1;
end;

{ TAggregatedOrLoneObject }

constructor TAggregatedOrLoneObject.Create(const Controller: IInterface);
  {Class constructor. Creates either an aggregated or stand-alone object.
    @param Controller [in] IInterface reference to containing object if
      aggregated or nil if not aggregated.
  }
begin
  inherited Create;
  fController := Pointer(Controller);
end;

constructor TAggregatedOrLoneObject.Create;
  {Class constructor. Creates a stand-alone object. Equivalent to calling
  Create(nil).
  }
begin
  Create(nil);
end;

function TAggregatedOrLoneObject.GetController: IInterface;
  {Returns IInterface of controlling object.
    @return Required IInterface reference. This is the container object if
      aggregated or Self if stand-alone.
  }
begin
  if Assigned(fController) then
    Result := IInterface(fController)
  else
    Result := Self;
end;

function TAggregatedOrLoneObject.QueryInterface(const IID: TGUID;
  out Obj): HResult;
  {Checks whether the specified interface is supported. If so reference to
  interface is passed out. If aggregated then the controller object is queried
  otherwise this object determines if interface is supported.
    @param IID [in] Specifies interface being queried.
    @param Obj [out] Reference to interface implementation or nil if not
      supported.
    @result S_OK if interface supported or E_NOINTERFACE if not supported.
  }
begin
  if Assigned(fController) then
    Result := IInterface(fController).QueryInterface(IID, Obj)
  else
    Result := inherited QueryInterface(IID, Obj);
end;

function TAggregatedOrLoneObject._AddRef: Integer;
  {Called by Delphi when interface is referenced. If aggregated call is passed
  off to controller, which may or may not perform reference counting. Otherwise
  reference count is incremented.
    @return Updated reference count.
  }
begin
  if Assigned(fController) then
    Result := IInterface(fController)._AddRef
  else
    Result := inherited _AddRef;
end;

function TAggregatedOrLoneObject._Release: Integer;
  {Called by Delphi when interface reference goes out of scope. If aggregated
  call is passed off to controller, which may or may not perform reference
  counting. Otherwise reference count is decremented and object is freed if
  count reaches zero.
    @return Updated reference count.
  }
begin
  if Assigned(fController) then
    Result := IInterface(fController)._Release
  else
    Result := inherited _Release;
end;

{ TConditionalFreeObject }

destructor TConditionalFreeObject.Destroy;
  {Class destructor tidies up and tears down object only if object can be freed.
  }
begin
  // Do not override to tidy up unless you first check CanDestroy and only tidy
  // up if it returns true. Override Finalize instead.
  if CanDestroy then
  begin
    Finalize;
    inherited;
  end;
end;

procedure TConditionalFreeObject.Finalize;
  {Tidies up object. Descendants should override this method instead of
  destructor.
  }
begin
  // Override this to tidy up the object instead of overriding the destructor
end;

procedure TConditionalFreeObject.FreeInstance;
  {Frees instance data only if object can be destroyed.
  }
begin
  // Check if object can be destroyed
  if CanDestroy then
    inherited;
end;

end.

