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
 *   3) TNonRefCountedObject implements a non reference counted implementation
 *      of IInterface.
 *   4) TAggregatedOrLoneObject is a base class for objects that can either
 *      exist as aggregated objects or as stand-alone reference counted objects.
 *      This implementation is based on code suggested by Hallvard VossBotn, as
 *      presented in Eric Harmon's book "Delphi COM programming".
 *
 * Unit originally named UIntfObjects.pas. Changed to UBaseObjects.pas at v2.0.
 *
 * v0.1 of 30 Jan 2005  - Original version, named UIntfObjects.pas.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Relocated and rationalised $WARN directives.
 * v1.1 of 31 Oct 2007  - Made TAggregatedOrLoneObject's constructors virtual.
 * v2.0 of 05 Oct 2008  - Renamed to UBaseObjects.pas
 *                      - Added new TNoConstructObject and
 *                        TNoPublicConstructObject classes that prevent use of
 *                        public constructor. Latter provides a protected
 *                        constructor for use by base classes.
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
 * The Original Code is UBaseObjects.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UBaseObjects;

{$WARN UNSAFE_TYPE OFF}

interface


type

  {
  TNoConstructObject:
    Class that fails if its constructor is called. Provided as a base class for
    classes that contain only class methods and should never be constructed.
  }
  TNoConstructObject = class(TObject)
  public
    constructor Create;
      {Class constructor. Causes an assertion failure if called. The object is
      never constructed.
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
      Should be called by sub class constructors instead of inherited Create.
      }
  public
    constructor Create;
      {Class constructor. Causes an assertion failure if called. The object is
      never constructed.
      }
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


implementation


{ TNoConstructObject }

constructor TNoConstructObject.Create;
  {Class constructor. Causes an assertion failure if called. The object is never
  constructed.
  }
begin
  Assert(False,                                            // ** do not localise
    ClassName + '.Create: Constructor can''t be called');
end;

{ TNoPublicConstructObject }

constructor TNoPublicConstructObject.Create;
  {Class constructor. Causes an assertion failure if called. The object is
  never constructed.
  }
begin
  Assert(False,                                            // ** do not localise
    ClassName + '.Create: Public constructor can''t be called');
end;

constructor TNoPublicConstructObject.InternalCreate;
  {Protected class constructor. Does nothing but call inherited constructor.
  Should be called by sub class constructors instead of inherited Create.
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

end.

