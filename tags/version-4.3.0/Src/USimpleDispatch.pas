{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides an abstract base class that provides a simple, non reference
 * counted, implementation of IDispatch.
}


unit USimpleDispatch;


interface


uses
  // Delphi
  ActiveX,
  // Project
  UBaseObjects;

type

  {
  TInvokeInfo:
    Structure providing information about a method or property invocation on an
    object. Implementors of TSimpleDispatch.DoInvoke must use and update this
    structure. Note that locales and exception information are not supported.
  }
  TInvokeInfo = record
    DispatchID: Integer;    // provides dispatch id of method or property
    Flags: Word;            // flags describing context of invocation
    Params: TDispParams;    // provides parameters passed to method
    FnResult: OleVariant;   // set to return value from method
    ArgErr: Cardinal;       // set to index of error parameter
    SCode: HResult;         // set to HResult describing success of invocation
  end;

  {
  TSimpleDispatch:
    Abstract base class that implements IDispatch that can be subclassed by
    classes that need to provide a simple, non reference counted, implementation
    of IDispatch. Descendants override the abstract DoInvoke method.
  }
  TSimpleDispatch = class(TNonRefCountedObject,
    IDispatch
  )
  strict protected
    procedure DoInvoke(var InvokeInfo: TInvokeInfo); virtual; abstract;
      {Abstract method to be implemented by sub-classes to invoke properties
      and methods.
        @param InvokeInfo [in/out] Information about method / property
          invocation. Structure is to be updated with information about any
          error and function results etc.
      }
    function GetValidParam(var InvokeInfo: TInvokeInfo; const Idx: Integer;
      const VType: Byte; out Param: OleVariant): Boolean;
      overload;
      {Validates and retrieves value of a parameter of required type. Updates
      InvokeInfo if parameter invalid.
        @param InvokeInfo [in/out] Information about method invocation.
          Structure is updated with information about any error.
        @param Idx [in] Index of parameter (parameters stored in reverse order).
        @param VType [in] Required parameter type.
        @param Param [out] Receives value of parameter. Value is undefined if
          parameter is invalid.
        @return True if parameter is valid, False if not.
      }
    function GetValidParam(var InvokeInfo: TInvokeInfo; const Idx: Integer;
      const VType: Byte; const IID: TGUID; out Param): Boolean;
      overload;
      {Validates and retrieves required interface to a parameter. Updates
      InvokeInfo if parameter invalid.
        @param InvokeInfo [in/out] Information about method invocation.
          Structure is updated with information about any error.
        @param Idx [in] Index of parameter (parameters stored in reverse order).
        @param VType [in] Required parameter type. Must be either varUnknown or
          varDispatch.
        @param IID [in] Specifies required interface supported by parameter.
        @param Param [out] Reference to required interface implementation.
          Undefined if parameter wrong type or interface not supported.
        @return True if parameter is valid and supports the required interface,
          False if VType not varUnknowm or varDispatch, parameter is not
          required VType or parameter does not support the required interface.
      }
  public
    { IDispatch methods }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
      {Maps a single member and an optional set of argument names to a
      corresponding set of integer DISPIDs, which may be used on subsequent
      calls to IDispatch::Invoke.
        @param IID [in] Reserved. Must be 0.
        @param Names [in] Array of names to be mapped.
        @param NameCount [in] Count of names in Names.
        @param LocaleID [in] The locale context in which to interpret the names.
        @param DispIDs [in] Caller-allocated array, each element of which
          contains an ID corresponding to one of the names passed in the Names
          array. The first element represents the member name. Subsequent
          elements represent each of the member's parameters.
        @return Success or failure code. We return E_NOTIMPL.
      }
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
      stdcall;
      {Retrieves a type information object, which can be used to get the type
      information for an interface.
        @param Index [in] Index of type information to return. Pass 0 to
          retrieve type information for the IDispatch implementation.
        @param LocaleID [in] The locale ID for the type information.
        @param TypeInfo [out] Set to ITypeInfo interface of type information
          requested. We set to nil.
        @return Success or failure code. We return E_NOTIMPL.
      }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
      {Retrieves the number of type information interfaces that an object
      provides (either 0 or 1).
        @param Count [out] Receives the number of type information interfaces
          that the object provides. If the object provides type information,
          this number is 1; otherwise the number is 0. We set to 0.
        @return Success of failure code. We return E_NOTIMPL.
      }
    function Invoke(ADispID: Integer; const AIID: TGUID; ALocaleID: Integer;
      AFlags: Word; var AParams; AVarResult, AExcepInfo,
      AArgErr: Pointer): HResult; stdcall;
      {Provides access to properties and methods exposed by an object.
        @param ADispId [in] Identifies the member of the object.
        @param AIID [in] Reserved. Must be IID_NULL.
        @param ALocaleID [in] The locale context in which to interpret
          arguments. Not used.
        @param AFlags [in] Flags describing the context of the Invoke call.
        @param AParams [in/out] Pointer to structure containing an array of
          arguments, array of argument dispatch IDs for named arguments, and
          counts for number of elements in the arrays.
        @param AVarResult [in/out] Pointer to where the result is to be stored,
          or NULL if the caller expects no result.
        @param AExcepInfo [in/out] Pointer to a structure containing any
          exception information. Not used.
        @param AArgErr [in/out] Index within AParams.rgvarg[] array of the first
          argument that has an error.
        @return Success or failure code.
      }
  end;


implementation


uses
  // Delphi
  Variants, Windows;


{ TSimpleDispatch }

function TSimpleDispatch.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
  {Maps a single member and an optional set of argument names to a corresponding
  set of integer DISPIDs, which may be used on subsequent calls to
  IDispatch.Invoke.
    @param IID [in] Reserved. Must be 0.
    @param Names [in] Array of names to be mapped.
    @param NameCount [in] Count of names in Names.
    @param LocaleID [in] The locale context in which to interpret the names.
    @param DispIDs [in] Caller-allocated array, each element of which contains
      an ID corresponding to one of the names passed in the Names array. The
      first element represents the member name. Subsequent elements represent
      each of the member's parameters.
    @return Success or failure code. We return E_NOTIMPL.
  }
begin
  Result := E_NOTIMPL;
end;

function TSimpleDispatch.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
  {Retrieves a type information object, which can be used to get the type
  information for an interface.
    @param Index [in] Index of type information to return. Pass 0 to retrieve
      type information for the IDispatch implementation.
    @param LocaleID [in] The locale ID for the type information.
    @param TypeInfo [out] Set to ITypeInfo interface of type information
      requested. We set to nil.
    @return Success or failure code. We return E_NOTIMPL.
  }
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TSimpleDispatch.GetTypeInfoCount(out Count: Integer): HResult;
  {Retrieves the number of type information interfaces that an object provides
  (either 0 or 1).
    @param Count [out] Receives the number of type information interfaces that
      the object provides. If the object provides type information, this number
      is 1; otherwise the number is 0. We set to 0.
    @return Success of failure code. We return E_NOTIMPL.
  }
begin
  Count := 0;
  Result := E_NOTIMPL;
end;

function TSimpleDispatch.GetValidParam(var InvokeInfo: TInvokeInfo;
  const Idx: Integer; const VType: Byte; out Param: OleVariant): Boolean;
  {Validates and retrieves value of a parameter of required type. Updates
  InvokeInfo if parameter invalid.
    @param InvokeInfo [in/out] Information about method invocation. Structure is
      updated with information about any error.
    @param Idx [in] Index of parameter (parameters stored in reverse order).
    @param VType [in] Required parameter type.
    @param Param [out] Receives value of parameter. Value is undefined if
      parameter is invalid.
    @return True if parameter is valid, False if not.
  }
begin
  if (Idx < 0) or (Idx >= InvokeInfo.Params.cArgs) then
  begin
    // Index is out of range: no such parameter
    InvokeInfo.SCode := DISP_E_PARAMNOTFOUND;
    InvokeInfo.ArgErr := Idx;
    Result := False;
    Exit;
  end;
  // Retrieve parameter as variant and check if it is required type
  Param := OleVariant(InvokeInfo.Params.rgvarg^[Idx]);
  Result := VarType(Param) = VType;
  if not Result then
  begin
    // Parameter type is incorrect
    InvokeInfo.SCode := DISP_E_TYPEMISMATCH;
    InvokeInfo.ArgErr := Idx;
  end;
end;

function TSimpleDispatch.GetValidParam(var InvokeInfo: TInvokeInfo;
  const Idx: Integer; const VType: Byte; const IID: TGUID;
  out Param): Boolean;
  {Validates and retrieves required interface to a parameter. Updates InvokeInfo
  if parameter invalid.
    @param InvokeInfo [in/out] Information about method invocation. Structure is
      updated with information about any error.
    @param Idx [in] Index of parameter (parameters stored in reverse order).
    @param VType [in] Required parameter type. Must be either varUnknown or
      varDispatch.
    @param IID [in] Specifies required interface supported by parameter.
    @param Param [out] Reference to required interface implementation.
      Undefined if parameter wrong type or interface not supported.
    @return True if parameter is valid and supports the required interface,
      False if VType not varUnknowm or varDispatch, parameter is not
      required VType or parameter does not support the required interface.
  }
var
  Value: OleVariant;  // parameter as variant
begin
  // Get value of parameter, providing of required VType
  Result := False;
  if (VType = varUnknown) or (VType = varDispatch) then
  begin
    // VType is valid: check parameter supports required VType and get value
    if GetValidParam(InvokeInfo, Idx, VType, Value) then
      // Try to get required interface from parameter
      Result := VarSupports(Value, IID, Param);
  end;
  if not Result then
  begin
    // Parameter type is incorrect
    InvokeInfo.SCode := DISP_E_TYPEMISMATCH;
    InvokeInfo.ArgErr := Idx;
  end;
end;

function TSimpleDispatch.Invoke(ADispID: Integer; const AIID: TGUID;
  ALocaleID: Integer; AFlags: Word; var AParams; AVarResult, AExcepInfo,
  AArgErr: Pointer): HResult;
  {Provides access to properties and methods exposed by an object.
    @param ADispId [in] Identifies the member of the object.
    @param AIID [in] Reserved. Must be IID_NULL.
    @param ALocaleID [in] The locale context in which to interpret arguments.
      Not used.
    @param AFlags [in] Flags describing the context of the Invoke call.
    @param AParams [in/out] Pointer to structure containing an array of
      arguments, array of argument dispatch IDs for named arguments, and counts
      for number of elements in the arrays.
    @param AVarResult [in/out] Pointer to where the result is to be stored, or
      NULL if the caller expects no result.
    @param AExcepInfo [in/out] Pointer to a structure containing any exception
      information. Not used.
    @param AArgErr [in/out] Index within AParams.rgvarg[] array of the first
      argument that has an error.
    @return Success or failure code.
  }
var
  InvokeInfo: TInvokeInfo;  // information about method invocation
begin
  // Hand work to DoInvoke
  // record invocation info
  InvokeInfo.DispatchID := ADispID;
  InvokeInfo.Flags := AFlags;
  InvokeInfo.Params := TDispParams(AParams);
  InvokeInfo.FnResult := Unassigned;    // assume no function result
  InvokeInfo.ArgErr := 0;               // init ArgErr: ignored if no error
  InvokeInfo.SCode := S_OK;             // assume success
  // do invocation
  DoInvoke(InvokeInfo);                 // this may update InvokeInfo
  // unpack results of invocation
  Result := InvokeInfo.SCode;
  if Succeeded(Result) then
  begin
    if Assigned(AVarResult) and not VarIsEmpty(InvokeInfo.FnResult) then
      // record any function result
      OleVariant(AVarResult^) := InvokeInfo.FnResult;
  end
  else
    // set error code
    Cardinal(AArgErr^) := InvokeInfo.ArgErr;
end;

end.

