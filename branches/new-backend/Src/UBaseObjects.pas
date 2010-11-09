{
 * UBaseObjects.pas
 *
 * Contains various common classes for use as base classes to other classes.
 * Classes are:
 *
 *   1) TNoConstructObject:
 *      Object that fails if its constructor is called. Provided as a base class
 *      for classes that contains only class methods and should never be
 *      constructed.
 *   2) TNoPublicConstructObject:
 *      Object that provides a protected constructor but fails if the public
 *      constructor is called. For use as a base class for objects that are
 *      constructed via class methods but should not be constructed directly.
 *   3) TNoPublicConstructIntfObject:
 *      Same as 2) except class descends from TInterfacedObject instead of
 *      TObject.
 *   4) TNonRefCountedObject:
 *      Implements a non reference counted implementation of IInterface.
 *   5) TAggregatedOrLoneObject:
 *      Base class for objects that can either exist as aggregated objects or as
 *      stand-alone reference counted objects. This implementation is based on
 *      code suggested by Hallvard VossBotn, as presented in Eric Harmon's book
 *      "Delphi COM programming".
 *   6) TConditionalFreeObject:
 *      An abstract base class for objects that cannot be destroyed unless some
 *      condition is met.
 *   7) TOwnedConditionalFreeObject:
 *      Class that cannot be destroyed for as long as it has an owner object.
 *      Attempts to free succeed only if Owner property is nil.
 *   8) TDelegatedConditionalFreeObject:
 *      Concrete descendant of TConditionalFreeObject that delegates the
 *      decision about whether the object can be freed to a callback method
 *      passed to the constructor.
 *   9) TControlledConditionalFreeObject:
 *      Concrete descendant of TConditionalFreeObject that defers decision about
 *      whether the object can be freed to an associated controller object.
 *
 * Interfaces:
 *
 *   1) INoPublicConstruct:
 *      A "do nothing" interface that can be supported by objects that don't
 *      allow public construction but cannot inherit from
 *      TNoPublicConstructObject, for example forms.
 *   2) IConditionalFreeController:
 *      Interface supported by objects that control whether an object can be
 *      freed. Used with TControlledConditionalFreeObject.
 *
 * Exceptions:
 *
 *   ELocked:
 *      Class of exception raised when an attempt is made to write to a locked
 *      resource.
 *
 * Unit originally named UIntfObjects.pas. Changed to UBaseObjects.pas at v2.0,
 * 5th October 2008.
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
 * The Original Code is UBaseObjects.pas
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


unit UBaseObjects;


interface


uses
  // Delphi
  SysUtils;


type

  {
  TNoConstructObject:
    Class that fails if its constructor is called. Provided as a base class for
    classes that contain only class methods and should never be constructed.
  }
  TNoConstructObject = class(TObject)
  public
    constructor Create;
      {Object constructor. Fails if called. The object is never constructed.
        @except ENoConstructException raised if constructor is called.
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
      {Protected object constructor. Does nothing but call inherited
      constructor. Should be called by class methods of derived classes instead
      of inherited Create.
      }
  public
    constructor Create;
      {Object constructor. Fails if called. The object is never constructed.
        @except ENoConstructException raised if constructor is called.
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
      {Object constructor. Fails if called. The object is never constructed.
        @except ENoConstructException raised if constructor is called.
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
  protected
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
  private
    fController: Pointer;
      {Weak reference to controlling object if aggregated or nil if stand-alone}
    function GetController: IInterface;
      {Returns IInterface of controlling object.
        @return Required IInterface reference. This is the container object if
          aggregated or Self if stand-alone.
      }
  protected
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

  {
  TOwnedConditionalFreeObject:
    Class that can only be freed when it is not owned, i.e. Owner property is
    nil.
  }
  TOwnedConditionalFreeObject = class(TConditionalFreeObject)
  strict private
    fOwner: TObject;  // Value of Owner property
  strict protected
    function CanDestroy: Boolean; override;
      {Determines if this instance can be destroyed.
        @return True if instance can be destroyed, False if not.
      }
  public
    constructor Create(const AOwner: TObject = nil);
      {Constructor that create object with optional owner object.
        @param AOwner [in] Optional owner object.
      }
    property Owner: TObject read fOwner write fOwner;
      {Reference to owning object}
  end;

  {
  TDelegatedConditionalFreeObject:
    Class that delegates the decision as to whether it can be destroyed to a
    callback function passed to the constructor. The object is only destroyed if
    the callback function returns True.
  }
  TDelegatedConditionalFreeObject = class(TConditionalFreeObject)
  strict private
    fCanDestroy: TFunc<TObject,Boolean>;
      {Reference callback function that determines if the object can be
      destroyed}
  strict protected
    function CanDestroy: Boolean; override;
      {Determines if this instance can be destroyed. Calls function stored in
      fCanDestroy.
        @return True if instance can be destroyed, False if not.
      }
  public
    constructor Create(const CanFreeFn: TFunc<TObject,Boolean>);
      {Class constructor.
        @param CanFreeFn [in] Reference to function to call to determine if
          the instance can be destroyed.
      }
  end;

  {
  IConditionalFreeController:
    Interface supported by objects that control whether an object can be freed
    (destroyed). Used with TControlledConditionalFreeObject.
  }
  IConditionalFreeController = interface(IInterface)
    ['{8B929FBE-F305-432F-A7CD-FE69ED8A685B}']
    function PermitDestruction(const Obj: TObject): Boolean;
      {Checks if an object can be destroyed.
        @param Obj [in] Object to be checked.
        @return True if object canbe destroyed, False if not.
      }
  end;

  {
  ELocked:
    Class of exception raised when an attempt is made to write to a locked
    resource.
  }
  ELocked = class(Exception);

  {
  TControlledConditionalFreeObject:
    Class that can only be freed if an associated controller class permits it.
    If no controller object is associated then objects of this class can not be
    freed.
  }
  TControlledConditionalFreeObject = class(TConditionalFreeObject)
  strict private
    fFreeController: IConditionalFreeController;
      {Value of FreeControlle property}
    fLocked: Boolean;
      {Value of Locked property}
    procedure SetFreeController(const Value: IConditionalFreeController);
      {Write accessor for FreeController property.
        @param Value [in] New property value.
        @except ELocked raised when an attempt is made to set the property value
          after it has been locked by the Lock method, unless the property is
          nil.
      }
  strict protected
    function CanDestroy: Boolean; override;
      {Determines if the object can be destroyed. Calls method of any assigned
      controller object to get decision.
        @return True if object can be destroyed.
      }
  public
    constructor Create(AFreeController: IConditionalFreeController = nil);
      {Object constructor.
        @param AFreeController [in] Reference to an object that controls whether
          this object can be destroyed. This value can be changed via the
          FreeController property.
      }
    procedure Lock;
      {Locks the object so that the FreeController property cannot be updated if
      it has already been set. If FreeController is nil when Lock is called it
      can still be set to a non-nil value. Lock sets the Locked property to
      true. Once locked the object cannot be unlocked.
      }
    property FreeController: IConditionalFreeController
      read fFreeController write SetFreeController;
      {Reference to object that controls whether this object can be freed. This
      property enables the controller to be changed over the lifetime of
      instances of this class. If the property is nil the object cannot be
      freed. An ELocked exception is raised if an attempt is made to set this
      property when the Locked property is True unless FreeController is nil}
    property Locked: Boolean read fLocked default False;
      {Flag indicating if the object is locked against setting a new value for
      a non-nil FreeController property}
  end;


implementation


{ TNoConstructObject }

constructor TNoConstructObject.Create;
  {Object constructor. Fails if called. The object is never constructed.
    @except ENoConstructException raised if constructor is called.
  }
begin
  raise ENoConstructException.Create(
    ClassName + '.Create: Constructor can''t be called'
  );
end;

{ TNoPublicConstructObject }

constructor TNoPublicConstructObject.Create;
  {Object constructor. Fails if called. The object is never constructed.
    @except ENoConstructException raised if constructor is called.
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
  {Object constructor. Fails if called. The object is never constructed.
    @except ENoConstructException raised if constructor is called.
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

{ TOwnedConditionalFreeObject }

function TOwnedConditionalFreeObject.CanDestroy: Boolean;
  {Determines if this instance can be destroyed.
    @return True if instance can be destroyed, False if not.
  }
begin
  // Instance can only be destroyed if it has no owner
  Result := not Assigned(fOwner);
end;

constructor TOwnedConditionalFreeObject.Create(const AOwner: TObject);
  {Constructor that create objects with optional owner object.
    @param AOwner [in] Optional owner object.
  }
begin
  inherited Create;
  fOwner := AOwner;
end;

{ TDelegatedConditionalFreeObject }

function TDelegatedConditionalFreeObject.CanDestroy: Boolean;
  {Determines if this instance can be destroyed. Calls function stored in
  fCanDestroy.
    @return True if instance can be destroyed, False if not.
  }
begin
  if not Assigned(fCanDestroy) then
    Exit(False);
  Result := fCanDestroy(Self);
end;

constructor TDelegatedConditionalFreeObject.Create(
  const CanFreeFn: TFunc<TObject,Boolean>);
  {Class constructor.
    @param CanFreeFn [in] Reference to function to call to determine if the
      instance can be destroyed.
  }
begin
  inherited Create;
  fCanDestroy := CanFreeFn;
end;

{ TControlledConditionalFreeObject }

function TControlledConditionalFreeObject.CanDestroy: Boolean;
  {Determines if the object can be destroyed. Calls method of any assigned
  controller object to get decision.
    @return True if object can be destroyed.
  }
begin
  if not Assigned(fFreeController) then
    Exit(False);
  Result := fFreeController.PermitDestruction(Self);
end;

constructor TControlledConditionalFreeObject.Create(
  AFreeController: IConditionalFreeController);
  {Object constructor.
    @param AFreeController [in] Reference to an object that controls whether
      this object can be destroyed. This value can be changed via the
      FreeController property.
  }
begin
  inherited Create;
  fFreeController := AFreeController;
end;

procedure TControlledConditionalFreeObject.Lock;
  {Locks the object so that the FreeController property cannot be updated if it
  has already been set. If FreeController is nil when Lock is called it can
  still be set to a non-nil value. Lock sets the Locked property to true. Once
  locked the object cannot be unlocked.
  }
begin
  fLocked := True;
end;

procedure TControlledConditionalFreeObject.SetFreeController(
  const Value: IConditionalFreeController);
  {Write accessor for FreeController property.
    @param Value [in] New property value.
    @except ELocked raised when an attempt is made to set the property value
    after it has been locked by the Lock method, unless the property is nil.
  }
resourcestring
  // Exception error message
  sLockError = 'Can''t set FreeController: object is locked';
begin
  if Locked and Assigned(fFreeController) then
    raise ELocked.Create(sLockError);
  fFreeController := Value;
end;

end.

