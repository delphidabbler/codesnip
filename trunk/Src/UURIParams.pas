{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Encapsulates URI query string parameters and generates a query string from
 * them.
}


unit UURIParams;


interface


uses
  // Delphi
  Classes, Generics.Collections;


type
  {
  TURIParams:
    Class that encapsulates URI query string parameters and generates a query
    string from them.
  }
  TURIParams = class(TObject)
  strict private
    fDict: TDictionary<string,string>;
      {Dictionary mapping param names to values}
    procedure Add(const Params: TStrings); overload;
      {Adds zero or more new parameters to object.
        @param Params [in] String list containing parameters to be added. Each
          element is a string in name=value format.
      }
  public
    constructor Create; overload;
      {Constructs object that contains no parameters.
      }
    constructor Create(const Params: TStrings); overload;
      {Constructs object containing zero or more parameters.
        @param Params [in] String list where each element specifies a parameter
          in name=value format.
      }
    destructor Destroy; override;
      {Tears down object.
      }
    procedure Add(const Name, Value: string); overload;
      {Adds a single new parameter to object.
        @param Name [in] Name of parameter to be added.
        @param Value [in] Value of parameter to be added.
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
  // Delphi
  SysUtils,
  // Project
  UComparers, UURIEncode;


{ TURIParams }

procedure TURIParams.Add(const Name, Value: string);
  {Adds a single new parameter to object.
    @param Name [in] Name of parameter to be added.
    @param Value [in] Value of parameter to be added.
  }
begin
  fDict.Add(Name, Value);
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

constructor TURIParams.Create;
  {Constructs object that contains no parameters.
  }
begin
  inherited Create;
  fDict := TDictionary<string,string>.Create(
    24, TTextEqualityComparer.Create
  );
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
      SB.Append(URIEncodeQueryString(AParam.Key));
      if AParam.Value <> '' then
        SB.AppendFormat('=%s', [URIEncodeQueryString(AParam.Value)]);
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TURIParams.IsEmpty: Boolean;
  {Checks if the object is empty, i.e. has no recorded parameters.
    @return True if there are no recorded parameters, False otherwise.
  }
begin
  Result := fDict.Count = 0;
end;

end.

