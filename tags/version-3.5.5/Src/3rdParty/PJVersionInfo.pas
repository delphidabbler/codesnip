{
 * PJVersionInfo.pas
 *
 * Version Information Component (32 bit). The component reads version
 * information from files.
 *
 * $Rev$
 * $Date$
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
 * The Original Code is PJVersionInfo.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 1998-2009 Peter
 * Johnson. All Rights Reserved.
 * 
 * Contributor(s):
 *   Roland Beduerftig (C++ builder compatibility)
 * 
 * ***** END LICENSE BLOCK *****
}


unit PJVersionInfo;

// Determine if certain required features are supported
{$DEFINE Supports_Assert}
{$DEFINE Supports_ResourceString}
{$IFDEF VER90} // Delphi 2 doesn't support resourcestring or Assert
  {$UNDEF Supports_Assert}
  {$UNDEF Supports_ResourceString}
{$ENDIF}
// Switch off unsafe code warnings if switch supported
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0}  // >= Delphi 7
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
{$ENDIF}

interface


uses
  // Delphi
  Windows, Classes;


type

  {
  TPJVersionNumber:
    Record holding version numbers.
  }
  TPJVersionNumber = record
    V1, V2, V3, V4: WORD;
  end;

  {
  TPJVersionInfo:
    Component that exposes the version information embedded in an executable
    file and exposed the detail as properties.
  }
  TPJVersionInfo = class(TComponent)
  private // properties
    fFileName: string;
    fHaveInfo: Boolean;
    fNumTranslations: Integer;
    fCurrentTranslation: Integer;
    fFixedFileInfo: TVSFixedFileInfo;
    procedure SetFileName(AName: string);
    function GetProductVersionNumber: TPJVersionNumber;
    function GetFileVersionNumber: TPJVersionNumber;
    function GetLanguage: string;
    function GetCharSet: string;
    function GetCharSetCode: WORD;
    function GetLanguageCode: WORD;
    function GetCurrentTranslation: Integer;
    procedure SetCurrentTranslation(const Value: Integer);
    function GetStringFileInfo(const Name: string): string;
    function GetStringFileInfoByIdx(Index: Integer): string;
    function GetFixedFileInfoItemByIdx(Index: Integer): DWORD;
  private
    fPInfoBuffer: PChar;      // Pointer to info buffer
    fPTransBuffer: Pointer;   // Pointer to translation buffer
    procedure GetInfoBuffer(Len: DWORD);
      {Creates an info buffer of required size}
    procedure GetTransBuffer(Len: UINT);
      {Creates a translation table buffer of required size}
    function GetTransStr: string;
      {Translation info encoded in string: takes into account current
      translation}
  protected
    procedure ClearProperties; virtual;
      {Forces properties to return cleared values}
    procedure ReadVersionInfo; virtual;
      {Reads version info from file}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: sets default values}
    destructor Destroy; override;
      {Class destructor: frees allocated memory}
    {Properties}
    property HaveInfo: Boolean
      read fHaveInfo;
      {Property true if file version info for the file per FileName property has
      been successfully read}
    property FixedFileInfo: TVSFixedFileInfo
      read fFixedFileInfo;
      {Exposes the whole fixed file info record: following properties expose
      the various fields of it}
    property FileVersionNumber: TPJVersionNumber
      read GetFileVersionNumber;
      {Version number of file, in numeric format, from fixed file info}
    property ProductVersionNumber: TPJVersionNumber
      read GetProductVersionNumber;
      {Version number of product, in numeric format, from fixed file info}
    property FileOS: DWORD  index 0
      read GetFixedFileInfoItemByIdx;
      {Code describing operating system to be used by file}
    property FileType: DWORD index 1
      read GetFixedFileInfoItemByIdx;
      {Code descibing type of file}
    property FileSubType: DWORD index 2
      read GetFixedFileInfoItemByIdx;
      {Code describing sub-type of file - only used for certain values of
      FileType property}
    property FileFlagsMask: DWORD index 3
      read GetFixedFileInfoItemByIdx;
      {Code describing which FileFlags are valid}
    property FileFlags: DWORD index 4
      read GetFixedFileInfoItemByIdx;
      {Flags describing file state}
    property Comments: string index 0
      read GetStringFileInfoByIdx;
      {String file info property giving user defined comments for current
      translation}
    property CompanyName: string index 1
      read GetStringFileInfoByIdx;
      {String file info property giving name of company for current translation}
    property FileDescription: string index 2
      read GetStringFileInfoByIdx;
      {String file info property giving description of file for current
      translation}
    property FileVersion: string index 3
      read GetStringFileInfoByIdx;
      {String file info property giving version number of file in string format
      for current translation}
    property InternalName: string index 4
      read GetStringFileInfoByIdx;
      {String file info property giving internal name of file for current
      translation}
    property LegalCopyright: string index 5
      read GetStringFileInfoByIdx;
      {String file info property giving copyright message for current
      translation}
    property LegalTrademarks: string index 6
      read GetStringFileInfoByIdx;
      {String file info property giving trademark info for current translation}
    property OriginalFileName: string index 7
      read GetStringFileInfoByIdx;
      {String file info property giving original name of file for current
      translation}
    property PrivateBuild: string index 8
      read GetStringFileInfoByIdx;
      {String file info property giving information about a private build of
      file for current translation}
    property ProductName: string index 9
      read GetStringFileInfoByIdx;
      {String file info property giving name of product for current translation}
    property ProductVersion: string index 10
      read GetStringFileInfoByIdx;
      {String file info property giving version number of product in string
      format for current translation}
    property SpecialBuild: string index 11
      read GetStringFileInfoByIdx;
      {String file info property giving information about a special build of
      file for current translation}
    property StringFileInfo[const Name: string]: string
      read GetStringFileInfo;
      {Returns the value for string file info with given name for current
      translation. This property can return both standard and custom string
      info}
    property Language: string
      read GetLanguage;
      {Name of language in use in current translation}
    property CharSet: string
      read GetCharSet;
      {Name of character set in use in current translation}
    property LanguageCode: WORD
      read GetLanguageCode;
      {Code of laguage in use in current translation}
    property CharSetCode: WORD
      read GetCharSetCode;
      {Character set code in use in current translation}
    property NumTranslations: Integer
      read fNumTranslations;
      {The number of difference translations (ie languages and char sets) in
      the version information}
    property CurrentTranslation: Integer
      read GetCurrentTranslation write SetCurrentTranslation;
      {Zero-based index of the current translation: this is 0 when a file is
      first accessed. Set to a value in range 0..NumTranslations-1 to access
      other translations. All string info, language and char set properties
      return information for the current translation}
  published
    property FileName: string read fFileName write SetFileName;
      {Name of file to which version information relates}
  end;


procedure Register;
  {Register this component}


implementation


uses
  // Delphi
  SysUtils;


procedure Register;
  {Register this component}
begin
  RegisterComponents('DelphiDabbler', [TPJVersionInfo]);
end;

type
  // ANSI version of CPINFOEX: provides information about a code page
  _cpinfoexA = packed record
    MaxCharSize: UINT;
      {max length in bytes of a character in the code page}
    DefaultChar: array[0..MAX_DEFAULTCHAR-1] of Byte;
      {default character used to translate strings to the specific code page}
    LeadByte: array[0..MAX_LEADBYTES-1] of Byte;
      {fixed-length array of lead byte ranges: all elements null if none}
    UnicodeDefaultChar: WideChar;
      {unicode default char used in translations from the specific code page}
    CodePage: UINT;
      {code page value}
    CodePageName: array[0..MAX_PATH-1] of AnsiChar;
      {full localised name of the code page}
  end;
  CPINFOEXA = _cpinfoexA;
  PCPInfoExA = ^CPINFOEXA;
  TCPInfoExA = CPINFOEXA;

  // Unicode version of CPINFOEX: provides information about a code page
  _cpinfoexW = packed record
    MaxCharSize: UINT;
      {max length in bytes of a character in the code page}
    DefaultChar: array[0..MAX_DEFAULTCHAR-1] of Byte;
      {default character used to translate strings to the specific code page}
    LeadByte: array[0..MAX_LEADBYTES-1] of Byte;
      {fixed-length array of lead byte ranges: all elements null if none}
    UnicodeDefaultChar: WideChar;
      {unicode default char used in translations from the specific code page}
    CodePage: UINT;
      {code page value}
    CodePageName: array[0..MAX_PATH-1] of WideChar;
      {full localised name of the code page}
  end;
  CPINFOEXW = _cpinfoexW;
  PCPInfoExW = ^CPINFOEXW;
  TCPInfoExW = CPINFOEXW;

  // Set TCPInfoEx etc to required ANSI or Unicode version of structure
  {$IFDEF UNICODE}
  TCPInfoEx = TCPInfoExW;
  PCPInfoEx = PCPInfoExW;
  {$ELSE}
  TCPInfoEx = TCPInfoExA;
  PCPInfoEx = PCPInfoExA;
  {$ENDIF}
  CPINFOEX = TCPInfoEx;

var
  // Pointer to Windows API GetCPInfoEx function if it exists or to GetCPInfoAlt
  // otherwise
  GetCPInfoExFn: function (CodePage: UINT; dwFlags: DWORD;
    var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;

const
  // Import name of GetCPInfoEx. Unicode and ANSI versions.
  {$IFDEF UNICODE}
  cGetCPInfoEx = 'GetCPInfoExW';
  {$ELSE}
  cGetCPInfoEx = 'GetCPInfoExA';
  {$ENDIF}

function GetCPInfoAlt(CodePage: UINT; dwFlags: DWORD;
  var lpCPInfoEx: TCPInfoEx): BOOL; stdcall;
  {Local implementation of GetCPInfoEx, for use on OSs that don't support
  GetCPInfoEx. Calls older GetCPInfo API function and calculates members of
  TCPInfoEx not provided by GetCPInfo.
  }
  // ---------------------------------------------------------------------------
  procedure CopyByteArray(const Src: array of Byte; var Dest: array of Byte);
    {Makes a copy of a byte array.
      @param Src [in] Byte array to be copied.
      @param Dest [in/out] In: Array to receive copy: must be same size as Src.
        Out: Receives copy of Src.
    }
  var
    Idx: Integer; // loops thru array
  begin
    {$IFDEF Supports_Assert}
    Assert((Low(Src) = Low(Dest)) and (High(Src) = High(Dest)));
    {$ENDIF}
    for Idx := Low(Src) to High(Src) do
      Dest[Idx] := Src[Idx];
  end;
  // ---------------------------------------------------------------------------
{$IFDEF Supports_ResourceString}
resourcestring
{$ELSE}
const
{$ENDIF}
  sCodePage = 'Code Page %d'; // description of code page if OS doesn't provide
var
  OldInfo: TCPInfo; // old style code page info structure for Win95/NT4
begin
  // We haven't got GetCPInfoEx: use old GetCPInfo to get some info
  Result := GetCPInfo(CodePage, OldInfo);
  if Result then
  begin
    // We update TCPInfoEx structure from old style structure and calculate
    // additional info
    // copy over from old style TCPInfo structure
    lpCPInfoEx.MaxCharSize := OldInfo.MaxCharSize;
    CopyByteArray(OldInfo.DefaultChar, lpCPInfoEx.DefaultChar);
    CopyByteArray(OldInfo.LeadByte, lpCPInfoEx.LeadByte);
    // no new default char
    lpCPInfoEx.UnicodeDefaultChar := #0;
    // store reference to code page
    lpCPInfoEx.CodePage := CodePage;
    // description is simply "Code Page NNN"
    StrPLCopy(
      lpCPInfoEx.CodePageName,
      Format(sCodePage, [CodePage]),
      SizeOf(lpCPInfoEx.CodePageName)
    );
  end;
end;

{ TPJVersionInfo }

type
  {
  TTransRec:
    Record of language code and char set codes that are returned from version
    information.
  }
  TTransRec = packed record      // record to hold translation information
    Lang, CharSet: Word;
  end;
  {
  TTransRecs:
    Type used to type cast translation data into an array of translation
    records.
  }
  TTransRecs = array[0..1000] of TTransRec;
  {
  PTransRecs:
    Pointer to an array of translation records.
  }
  PTransRecs = ^TTransRecs;


procedure TPJVersionInfo.ClearProperties;
  {Forces properties to return cleared values}
begin
  // Record that we haven't read ver info: this effectively clears properties
  // since each property read access method checks this flag before returning
  // result
  fHaveInfo := False;
end;

constructor TPJVersionInfo.Create(AOwner: TComponent);
  {Class constructor: sets default values}
begin
  inherited Create(AOwner);
  // Default is no file name - refers to executable file for application
  FileName := '';
end;

destructor TPJVersionInfo.Destroy;
  {Class destructor: frees allocated memory}
begin
  // Ensure that info buffer is freed if allocated
  if fPInfoBuffer <> nil then
    StrDispose(fPInfoBuffer);
  // Ensure that translation buffer is free if allocated
  if fPTransBuffer <> nil then
    FreeMem(fPTransBuffer);
  inherited Destroy;
end;

function TPJVersionInfo.GetCharSet: string;
  {Read access method for CharSet property: return string describing character
  set if we have some version info and empty string if we haven't}
var
  Info: TCPInfoEx;  // receives code page info
  CP: Word;         // code page
{$IFDEF Supports_ResourceString}
resourcestring
{$ELSE}
const
{$ENDIF}
  // Special code page messages
  sUnknownCP = '%d (Unknown Code Page)';  // unknown
  // Messages for pages API can't return (managed apps only)
  sUTF16LE  = '%d (Unicode UTF-16, little endian byte order)';
  sUTF16BE  = '%d (Unicode UTF-16, big endian byte order)';
  sUTF32LE  = '%d (Unicode UTF-32, little endian byte order)';
  sUTF32BE  = '%d (Unicode UTF-32, big endian byte order)';
begin
  Result := '';
  if fHaveInfo then
  begin
    CP := GetCharSetCode;
    case CP of
      // Check for char codes only available in managed apps (API call won't
      // find them)
      1200:   Result := Format(sUTF16LE, [CP]);
      1201:   Result := Format(sUTF16BE, [CP]);
      12000:  Result := Format(sUTF32LE, [CP]);
      12001:  Result := Format(sUTF32BE, [CP]);
      else
      begin
        // Not a known problem code page: get it from OS
        if GetCPInfoExFn(CP, 0, Info) then
          Result := Info.CodePageName
        else
          // Give up: can't find it
          Result := Format(sUnknownCP, [CP]);
      end;
    end;
  end;
end;

function TPJVersionInfo.GetCharSetCode: WORD;
  {Read access for CharSetCode property: returns char set code for current
  translation or 0 if there is no translation or we have no version info}
begin
  if fHaveInfo and (GetCurrentTranslation >= 0) then
    Result := PTransRecs(fPTransBuffer)^[GetCurrentTranslation].CharSet
  else
    Result := 0;
end;

function TPJVersionInfo.GetCurrentTranslation: Integer;
  {Read access method for CurrentTranslation property: returns index to current
  translation if we have version info or -1 if no version info}
begin
  if fHaveInfo then
    Result := fCurrentTranslation
  else
    Result := -1;
end;

function TPJVersionInfo.GetFileVersionNumber: TPJVersionNumber;
  {Read access method for FileVersionNumber property: fill version info
  structure and return it - if there's no version info all values will be zero}
begin
  Result.V1 := HiWord(fFixedFileInfo.dwFileVersionMS);
  Result.V2 := LoWord(fFixedFileInfo.dwFileVersionMS);
  Result.V3 := HiWord(fFixedFileInfo.dwFileVersionLS);
  Result.V4 := LoWord(fFixedFileInfo.dwFileVersionLS);
end;

function TPJVersionInfo.GetFixedFileInfoItemByIdx(Index: Integer): DWORD;
  {Read access method for various DWORD fields of the fixed file information
  record accessed by index.
  NOTE: This is a fix for C++ Builder. Delphi is able to access the fields of
  the TVSFixedFileInfo record directly in the read clause of the property
  declaration but this is not possible in C++ Builder}
begin
  case Index of
    0:  Result := fFixedFileInfo.dwFileOS;
    1:  Result := fFixedFileInfo.dwFileType;
    2:  Result := fFixedFileInfo.dwFileSubType;
    3:  Result := fFixedFileInfo.dwFileFlagsMask;
    4:  Result := fFixedFileInfo.dwFileFlags;
    else Result := 0;
  end;
end;

procedure TPJVersionInfo.GetInfoBuffer(Len: DWORD);
  {Creates an info buffer of required size}
begin
  // Clear any existing buffer
  if fPInfoBuffer <> nil then
    StrDispose(fPInfoBuffer);
  // Create the new one
  fPInfoBuffer := StrAlloc(Len);
end;

function TPJVersionInfo.GetLanguage: string;
  {Read access method for Language property: return string describing language
  if we have some version info and empty string if we haven't}
const
  cBufSize = 256;   // size of buffer
var
  Buf: array[0..Pred(cBufSize)] of Char;   // stores langauge string from API call
begin
  // Assume failure
  Result := '';
  // Try to get language name from Win API if we have ver info
  if fHaveInfo and
    (VerLanguageName(GetLanguageCode, Buf, Pred(cBufSize)) > 0) then
    Result := Buf;
end;

function TPJVersionInfo.GetLanguageCode: WORD;
  {Read access for LanguageCode property: returns language code for current
  translation or 0 if there is no translation or we have no version info}
begin
  if fHaveInfo and (GetCurrentTranslation >= 0) then
    Result := PTransRecs(fPTransBuffer)^[GetCurrentTranslation].Lang
  else
    Result := 0;
end;

function TPJVersionInfo.GetProductVersionNumber: TPJVersionNumber;
  {Read access method for ProductVersionNumber property: fill version info
  structure and return it - if there's no version info all values will be zero}
begin
  Result.V1 := HiWord(fFixedFileInfo.dwProductVersionMS);
  Result.V2 := LoWord(fFixedFileInfo.dwProductVersionMS);
  Result.V3 := HiWord(fFixedFileInfo.dwProductVersionLS);
  Result.V4 := LoWord(fFixedFileInfo.dwProductVersionLS);
end;

function TPJVersionInfo.GetStringFileInfo(const Name: string): string;
  {Read access method for StringFileInfo array property: returns string
  associated with given name or empty string if we have no version info}
var
  CommandBuf: array[0..255] of char;  // buffer to build API call command str
  Ptr: Pointer;                       // pointer to result of API call
  Len: UINT;                          // length of structure returned from API
begin
  // Set default failure result to empty string
  Result := '';
  // Check if we have valid information recorded in info buffer - exit if not
  if fHaveInfo then
  begin
    // Build API call command string for reading string file info:
    //   this uses info string + language and character set
    StrPCopy(CommandBuf, '\StringFileInfo\' + GetTransStr + '\' + Name);
    // Call API to get required string and return it if successful
    if VerQueryValue(fPInfoBuffer, CommandBuf, Ptr, Len) then
      Result := PChar(Ptr);
  end;
end;

function TPJVersionInfo.GetStringFileInfoByIdx(Index: Integer): string;
  {Read access method for all string file info properties: returns appropriate
  string for the given property or empty string if we have no version info}
const
  cNames: array[0..11] of string =
    ('Comments', 'CompanyName', 'FileDescription', 'FileVersion',
    'InternalName', 'LegalCopyright', 'LegalTrademarks', 'OriginalFileName',
    'PrivateBuild', 'ProductName', 'ProductVersion', 'SpecialBuild');
    {names of predefined string file info strings}
begin
  Result := GetStringFileInfo(cNames[Index]);
end;

procedure TPJVersionInfo.GetTransBuffer(Len: UINT);
  {Creates a translation table buffer of required size}
begin
  // Clear any existing buffer
  if fPTransBuffer <> nil then
    FreeMem(fPTransBuffer);
  // Create the new one
  GetMem(fPTransBuffer, Len);
end;

function TPJVersionInfo.GetTransStr: string;
  {Translation info encoded in string: takes into account current translation}
var
  TransRec: TTransRec;  // translation record in array of translations
begin
  if GetCurrentTranslation >= 0 then
  begin
    // There is a valid current translation: return hex string related to it
    TransRec := PTransRecs(fPTransBuffer)^[GetCurrentTranslation];
    Result := Format('%4.4x%4.4x', [TransRec.Lang, TransRec.CharSet]);
  end
  else
    // No valid translation string: return empty string
    Result := '';
end;

procedure TPJVersionInfo.ReadVersionInfo;
  {Reads version info from file}
var
  Len: UINT;        // length of structs returned from API calls
  Ptr: Pointer;     // points to version info structures
  InfoSize: DWORD;  // size of info buffer
  Dummy: DWORD;     // stores 0 in call to GetFileVersionInfoSize
begin
  // Record default value of HaveInfo property - no info read
  fHaveInfo := False;
  // Store zeros in fixed file info structure: this is used when no info
  FillChar(fFixedFileInfo, SizeOf(fFixedFileInfo), 0);
  // Set NumTranslations property to 0: this is value if no info
  fNumTranslations := 0;
  // Record required size of version info buffer
  InfoSize := GetFileVersionInfoSize(PChar(fFileName), Dummy);
  // Check that there was no error
  if InfoSize > 0 then
  begin
    // Found info size OK
    // Ensure we have a sufficiently large buffer allocated
    GetInfoBuffer(InfoSize);
    // Read file version info into storage and check success
    if GetFileVersionInfo(PChar(fFileName), Dummy, InfoSize, fPInfoBuffer) then
    begin
      // Success: we've read file version info to storage OK
      fHaveInfo := True;
      // Get fixed file info & copy to own storage
      VerQueryValue(fPInfoBuffer, '\', Ptr, Len);
      fFixedFileInfo := PVSFixedFileInfo(Ptr)^;
      // Get first translation table info from API
      VerQueryValue(fPInfoBuffer, '\VarFileInfo\Translation', Ptr, Len);
      // Ptr is to block of translation records each of size Len:
      // work out number of translations
      fNumTranslations := Len div SizeOf(TTransRec);
      // store translation array in a buffer
      GetTransBuffer(Len);
      Move(Ptr^, fPTransBuffer^, Len);
      // make first translation in block current one (-1 if no translations)
      SetCurrentTranslation(0);   // adjusts value to -1 if no translations
    end;
  end;
end;

procedure TPJVersionInfo.SetCurrentTranslation(const Value: Integer);
  {Write accees method for CurrentTranslation property: if value is out of
  translation range then it is set to -1 to indicate no translation}
begin
  if (Value >= 0) and (Value < NumTranslations) then
    fCurrentTranslation := Value
  else
    fCurrentTranslation := -1
end;

procedure TPJVersionInfo.SetFileName(AName: string);
  {Write access method for FileName property: action at design time is different
  to run time. At design time we simply record value while at run time we store
  value - using actual name of program for '' string - and read the version
  information}
begin
  if csDesigning in ComponentState then
    // We are designing, simply record the required name
    fFileName := AName
  else
  begin
    // It's run-time
    // use Application exec file name if name is ''
    if AName = '' then
      fFileName := ParamStr(0)
    else
      fFileName := AName;
    // clear all properties and read file version info for new file
    ClearProperties;
    ReadVersionInfo;
  end;
end;

initialization

// Get reference to GetCPInfoEx function
GetCPInfoExFn := GetProcAddress(GetModuleHandle('Kernel32.dll'), cGetCPInfoEx);
if not Assigned(GetCPInfoExFn) then
  GetCPInfoExFn := GetCPInfoAlt;

end.
