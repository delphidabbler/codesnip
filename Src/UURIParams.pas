{
 * UURIParams.pas
 *
 * Encapsulates URI query string parameters and generates the query string from
 * the parameters.
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
 * The Original Code is UURIParams.pas
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


unit UURIParams;


interface


uses
  // Delphi
  SysUtils, Classes, Generics.Collections;


type

  {
  TURIParam:
    Records that encapsulates a query string parameter. Records the parameter's
    name and value.
  }
  TURIParam = record
    Name: string;   // Parameter name
    Value: string;  // Parameter value
    constructor Create(const AName, AValue: string);
      {Initialises record with parameter name and value.
        @param AName [in] Parameter name.
        @param AValue [in] Parameter value.
      }
  end;

  {
  TURIParams:
    Class that encapsulates URI query string parameters and generates the query
    string from the parameters.
  }
  TURIParams = class(TObject)
  strict private
    fDict: TDictionary<string,string>;
      {Dictionary mapping param names to values}
    function BuildQueryString(const Encode: TFunc<string,string>): string;
      {Creates a custom encoded query string containing details of all recorded
      parameters.
        @param Encode [in] Anonymous method called to encode the parameters.
        @return Required query string.
      }
  public
    constructor Create; overload;
      {Constructs object that contains no parameters.
      }
    constructor Create(const Name, Value: string); overload;
      {Constructs object containing a single parameter.
        @param Name [in] Name of parameter to be recorded.
        @param Value [in] Value of parameter to be recorded.
      }
    constructor Create(const Param: TURIParam); overload;
      {Constructs object containing a single parameter.
        @param Param [in] Parameter to be recorded.
      }
    constructor Create(const Params: array of TURIParam); overload;
      {Constructs object containing zero or more parameters.
        @param Params [in] Array of parameters to be recorded.
      }
    constructor Create(const Params: TStrings); overload;
      {Constructs object containing zero or more parameters.
        @param Params [in] String list where each element specifies a parameter
          in name=value format.
      }
    constructor Create(const Params: array of string); overload;
      {Constructs object containing zero or more parameters.
        @param Params [in] Array of strings where each element specifies a
          parameter in name=value format.
      }
    destructor Destroy; override;
      {Tears down object.
      }
    procedure Add(const Name, Value: string); overload;
      {Adds a single new parameter to object.
        @param Name [in] Name of parameter to be added.
        @param Value [in] Value of parameter to be added.
      }
    procedure Add(const Param: TURIParam); overload;
      {Adds a single new parameter to object.
        @param Param [in] Parameter to be added.
      }
    procedure Add(const Params: array of TURIParam); overload;
      {Adds zero or more new parameters to object.
        @param Params [in] Array of parameters to be added.
      }
    procedure Add(const Params: TStrings); overload;
      {Adds zero or more new parameters to object.
        @param Params [in] String list containing parameters to be added. Each
          element is a string in name=value format.
      }
    procedure Add(const Params: array of string); overload;
      {Adds zero or more new parameters to object.
        @param Params [in] Array of strings containing parameters to be added.
          Each element is a string in name=value format.
      }
    function Exists(const Name: string): Boolean; overload;
      {Checks if a named parameter is recorded in object.
        @param Name [in] Name of required parameter.
        @return True if parameter is found, False if not.
      }
    function Exists(const Param: TURIParam): Boolean; overload;
      {Checks if a specified parameter is recorded in object. Only the
      parameter's name is checked for, the value is ignored.
        @param Param [in] Required parameter.
        @return True if parameter with same name is found, False if not.
      }
    procedure Update(const Name, Value: string); overload;
      {Updates the value of a named parameter.
        @param Name [in] Name of parameter to be updated.
        @param Value [in] New parameter value.
      }
    procedure Update(const Param: TURIParam); overload;
      {Updates the value of a specified parameter.
        @param Param [in] Parameter to be updated with new value.
      }
    function QueryString: string;
      {Creates an un-encoded query string containing details of all recorded
      parameters.
        @return Required query string.
      }
    function EncodedQueryString: string;
      {Creates a URI encoded query string containing details of all recorded
      parameters.
        @return Required query string.
      }
    function IsEmpty: Boolean;
      {Checks if the object is empty, i.e. has no recorded parameters.
        @return True if there are no recorded parameters, False otherwise.
      }
  end;


implementation


uses
  // Project
  UComparers, UURIEncode;


{ TURIParams }

procedure TURIParams.Add(const Param: TURIParam);
  {Adds a single new parameter to object.
    @param Param [in] Parameter to be added.
  }
begin
  Add(Param.Name, Param.Value);
end;

procedure TURIParams.Add(const Name, Value: string);
  {Adds a single new parameter to object.
    @param Name [in] Name of parameter to be added.
    @param Value [in] Value of parameter to be added.
  }
begin
  fDict.Add(Name, Value);
end;

procedure TURIParams.Add(const Params: array of TURIParam);
  {Adds zero or more new parameters to object.
    @param Params [in] Array of parameters to be added.
  }
var
  Param: TURIParam; // references each parameter in array
begin
  for Param in Params do
    Add(Param);
end;

procedure TURIParams.Add(const Params: TStrings);
  {Adds zero or more new parameters to object.
    @param Params [in] String list containing parameters to be added. Each
      element is a string in name=value format.
  }
var
  Idx: Integer; // loops through all elements of Params
begin
  Assert(Assigned(Params), ClassName + '.Add(TStrings): Params is nil');
  for Idx := 0 to Pred(Params.Count) do
    Add(Params.Names[Idx], Params.ValueFromIndex[Idx]);
end;

procedure TURIParams.Add(const Params: array of string);
  {Adds zero or more new parameters to object.
    @param Params [in] Array of strings containing parameters to be added. Each
      element is a string in name=value format.
  }
var
  Param: string;        // references each parameter string of Params
  ParamList: TStrings;  // string list that receives elements of Params
begin
  ParamList := TStringList.Create;
  try
    for Param in Params do
      ParamList.Add(Param);
    Add(ParamList);
  finally
    ParamList.Free;
  end;
end;

function TURIParams.BuildQueryString(
  const Encode: TFunc<string, string>): string;
  {Creates a custom encoded query string containing details of all recorded
  parameters.
    @param Encode [in] Anonymous method called to encode the parameters.
    @return Required query string.
  }
var
  AParam: TPair<string,string>; // references each recorded parameter
  SB: TStringBuilder;           // used to build query string
begin
  SB := TStringBuilder.Create;
  try
    for AParam in fDict do
    begin
      if SB.Length > 0 then
        SB.Append('&');
      SB.Append(Encode(AParam.Key));
      if AParam.Value <> '' then
        SB.AppendFormat('=%s', [Encode(AParam.Value)]);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

constructor TURIParams.Create;
  {Constructs object that contains no parameters.
  }
begin
  inherited Create;
  fDict := TDictionary<string,string>.Create(
    24, TSameTextEqualityComparer.Create
  );
end;

constructor TURIParams.Create(const Name, Value: string);
  {Constructs object containing a single parameter.
    @param Name [in] Name of parameter to be recorded.
    @param Value [in] Value of parameter to be recorded.
  }
begin
  Create;
  Add(Name, Value);
end;

constructor TURIParams.Create(const Param: TURIParam);
  {Constructs object containing a single parameter.
    @param Param [in] Parameter to be recorded.
  }
begin
  Create;
  Add(Param);
end;

constructor TURIParams.Create(const Params: array of string);
  {Constructs object containing zero or more parameters.
    @param Params [in] Array of strings where each element specifies a parameter
      in name=value format.
  }
begin
  Create;
  Add(Params);
end;

constructor TURIParams.Create(const Params: array of TURIParam);
  {Constructs object containing zero or more parameters.
    @param Params [in] Array of parameters to be recorded.
  }
begin
  Create;
  Add(Params);
end;

constructor TURIParams.Create(const Params: TStrings);
  {Constructs object containing zero or more parameters.
    @param Params [in] String list where each element specifies a parameter in
      name=value format.
  }
begin
  Create;
  Add(Params);
end;

destructor TURIParams.Destroy;
  {Tears down object.
  }
begin
  fDict.Free;
  inherited;
end;

function TURIParams.EncodedQueryString: string;
  {Creates a URI encoded query string containing details of all recorded
  parameters.
    @return Required query string.
  }
begin
  Result := BuildQueryString(
    function(S: string): string
    begin
      Result := URIEncodeQueryString(S);
    end
  );
end;

function TURIParams.Exists(const Name: string): Boolean;
  {Checks if a named parameter is recorded in object.
    @param Name [in] Name of required parameter.
    @return True if parameter is found, False if not.
  }
begin
  Result := fDict.ContainsKey(Name);
end;

function TURIParams.Exists(const Param: TURIParam): Boolean;
  {Checks if a specified parameter is recorded in object. Only the parameter's
  name is checked for, the value is ignored.
    @param Param [in] Required parameter.
    @return True if parameter with same name is found, False if not.
  }
begin
  Result := Exists(Param.Name);
end;

function TURIParams.IsEmpty: Boolean;
  {Checks if the object is empty, i.e. has no recorded parameters.
    @return True if there are no recorded parameters, False otherwise.
  }
begin
  Result := fDict.Count = 0;
end;

function TURIParams.QueryString: string;
  {Creates an un-encoded query string containing details of all recorded
  parameters.
    @return Required query string.
  }
begin
  Result := BuildQueryString(
    function(S: string): string
    begin
      Result := S;
    end
  );
end;

procedure TURIParams.Update(const Name, Value: string);
  {Updates the value of a named parameter.
    @param Name [in] Name of parameter to be updated.
    @param Value [in] New parameter value.
  }
begin
  fDict.Items[Name] := Value;
end;

procedure TURIParams.Update(const Param: TURIParam);
  {Updates the value of a specified parameter.
    @param Param [in] Parameter to be updated with new value.
  }
begin
  Update(Param.Name, Param.Value);
end;

{ TURIParam }

constructor TURIParam.Create(const AName, AValue: string);
  {Initialises record with parameter name and value.
    @param AName [in] Parameter name.
    @param AValue [in] Parameter value.
  }
begin
  Name := AName;
  Value := AValue;
end;

end.

