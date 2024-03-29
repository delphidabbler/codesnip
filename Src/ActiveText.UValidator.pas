{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a container record that provides methods to validate active text
 * object.
}


unit ActiveText.UValidator;


interface


uses
  // Project
  ActiveText.UMain;


type
  ///  <summary>
  ///  Container for active text validation methods.
  ///  </summary>
  TActiveTextValidator = record
  public
    type
      ///  <summary>Enumeration of all possible error codes.</summary>
      TErrorCode = (errBadStructure, errBadLinkProtocol, errBadLinkURL);
    type
      ///  <summary>Record storing information about an error.</summary>
      TErrorInfo = record
      public
        ///  <summary>Error code.</summary>
        Code: TErrorCode;
        ///  <summary>Description of error.</summary>
        Description: string;
        ///  <summary>Constructs a record. Sets fields from parameter values.
        ///  </summary>
        constructor Create(const ACode: TErrorCode; const ADescription: string);
      end;
  strict private
    ///  <summary>Validates given link element.</summary>
    ///  <param name="LinkElem">IActiveTextActionElem [in] Reference to link
    ///  element to be validated.</param>
    ///  <param name="ErrInfo">TErrorInfo [out] Contains error information if
    ///  validation fails. Undefined if validation succeeds.</param>
    ///  <returns>Boolean. True on success or False on failure.</returns>
    class function ValidateLink(LinkElem: IActiveTextActionElem;
      out ErrInfo: TErrorInfo): Boolean; static;
    ///  <summary>Validates document structure.</summary>
    ///  <param name="ActiveText">IActiveText [in] Active text to be validated.
    ///  </param>
    ///  <param name="ErrInfo">TErrorInfo [out] Contains error information if
    ///  validation fails. Undefined if validation succeeds.</param>
    ///  <returns>Boolean. True on success or False on failure.</returns>
    class function ValidateDocumentStructure(ActiveText: IActiveText;
      out ErrInfo: TErrorInfo): Boolean; static;
  public
    ///  <summary>Validates given active text.</summary>
    ///  <param name="ActiveText">IActiveText [in] Active text to be validated.
    ///  </param>
    ///  <param name="ErrInfo">TErrorInfo [out] Contains error information if
    ///  validation fails. Undefined if validation succeeds.</param>
    ///  <returns>Boolean. True on success or False on failure.</returns>
    class function Validate(ActiveText: IActiveText; out ErrInfo: TErrorInfo):
      Boolean; overload; static;
    ///  <summary>Validates given active text.</summary>
    ///  <param name="ActiveText">IActiveText [in] Active text to be validated.
    ///  </param>
    ///  <returns>Boolean. True on success or False on failure.</returns>
    class function Validate(ActiveText: IActiveText): Boolean; overload; static;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UURIEncode, UStrUtils;

{ TActiveTextValidator }

class function TActiveTextValidator.Validate(ActiveText: IActiveText;
  out ErrInfo: TErrorInfo): Boolean;
var
  Elem: IActiveTextElem;              // each element in active text
  ActionElem: IActiveTextActionElem;  // references action element
begin
  if ActiveText.IsEmpty then
    Exit(True);
  // Validate document structure
  if not ValidateDocumentStructure(ActiveText, ErrInfo) then
    Exit(False);
  // Validate elements
  for Elem in ActiveText do
  begin
    if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      // Validate action elements
      case ActionElem.Kind of
        ekLink:
          if not ValidateLink(ActionElem, ErrInfo) then
            Exit(False);
      end;
    end;
  end;
  Result := True;
end;

class function TActiveTextValidator.Validate(ActiveText: IActiveText): Boolean;
var
  Dummy: TErrorInfo; // dummy error info record
begin
  Result := Validate(ActiveText, Dummy);
end;

class function TActiveTextValidator.ValidateDocumentStructure(
  ActiveText: IActiveText; out ErrInfo: TErrorInfo): Boolean;
resourcestring
  sNoDocTags = 'Document must start and end with document tags';
begin
  Result := ActiveText.IsValidActiveTextDocument;
  if not Result then
    ErrInfo := TErrorInfo.Create(errBadStructure, sNoDocTags);
end;

class function TActiveTextValidator.ValidateLink(
  LinkElem: IActiveTextActionElem; out ErrInfo: TErrorInfo): Boolean;
resourcestring
  // validation error messages
  sURLProtocolErr = 'Link URL "%s" must use one of the '
    + '"http://", "https://" or "file://" protocols';
  sURLLengthErr = 'Link URL "%s" is badly formed';
type
  // Record of information about each supported URL protocol.
  TProtocolInfo = record
    Protocol: string;       // name of protocol
    MinURLLength: Integer;  // minimum length of URL after protocol
  end;
  TProtocolInfos = array[1..3] of TProtocolInfo;
const
  // Array of info about supported protocols
  ProtocolInfos: TProtocolInfos = (
    (Protocol: 'http://'; MinURLLength: 6),
    (Protocol: 'https://'; MinURLLength: 6),
    (Protocol: 'file://'; MinURLLength: 4)
  );
var
  URL: string;        // link URL
  PI: TProtocolInfo;  // references each supported protocol
begin
  URL := URIDecode(LinkElem.Attrs[TActiveTextAttrNames.Link_URL]);
  // Search for a supported protocol: check URL length if found
  for PI in ProtocolInfos do
  begin
    if StrStartsText(PI.Protocol, URL) then
    begin
      if Length(URL)
        < Length(PI.Protocol) + PI.MinURLLength then
      begin
        ErrInfo := TErrorInfo.Create(
          errBadLinkURL, Format(sURLLengthErr, [URL])
        );
        Exit(False);
      end;
      Exit(True);
    end;
  end;
  // No supported protocol
  Result := False;
  ErrInfo := TErrorInfo.Create(
    errBadLinkProtocol, Format(sURLProtocolErr, [URL])
  );
end;

{ TActiveTextValidator.TErrorInfo }

constructor TActiveTextValidator.TErrorInfo.Create(const ACode: TErrorCode;
  const ADescription: string);
begin
  Code := ACode;
  Description := ADescription;
end;

end.
