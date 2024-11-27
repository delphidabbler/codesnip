unit DB.DataFormats;

interface

type

  ///  <summary>Enumeration of the kinds of supported snippet collection data
  ///  formats.</summary>
  ///  <remarks>
  ///  <para><c>Error</c> -- Invalid format. Used to indicate an unknown format
  ///  or other error.</para>
  ///  <para><c>DCSC_v2</c> -- Format used by the DelphiDabbler Code Snippets
  ///  Collection v2.</para>
  ///  <para><c>Native_v4</c> -- Native format used by CodeSnip v4 to store user
  ///  snippets.</para>
  ///  </remarks>
  TDataFormatKind = (
    // NEVER specify a literal ordinal value in this enumeration.
    // NEVER delete or re-order the enumeration items: the ordinal values may
    //       be written to a config file and changing the ordinal value here can
    //       cause hard to trace bugs. If an item goes out of use then leave it
    //       in place & possibly rename the item to indicate its redundancy.
    // NEVER associate error with a format loader or saver class.
    Error,
    DCSC_v2,
    Native_v4
  );

  ///  <summary>Record containing details of the data format and location in
  ///  which a collection is stored.</summary>
  TDataStorageDetails = record
  strict private
    var
      ///  <summary>Value of the <c>Directory</c> property.</summary>
      fDirectory: string;
      ///  <summary>Value of the <c>Format</c> property.</summary>
      fFormat: TDataFormatKind;
    ///  <summary>Write access method for <c>Directory</c> property.</summary>
    procedure SetDirectory(const AValue: string);
  public
    ///  <summary>Constructs a new record instance with the given property
    ///  values.</summary>
    constructor Create(const AFormat: TDataFormatKind;
      const ADirectory: string);
    ///  <summary>The format in which the data is stored.</summary>
    property Format: TDataFormatKind read fFormat;
    ///  <summary>The directory in which the data is stored.</summary>
    property Directory: string read fDirectory write SetDirectory;
  end;

  TDataFormatInfo = record
  strict private
    type
      TMapRecord = record
        ///  <summary>Data format kind.</summary>
        Kind: TDataFormatKind;
        ///  <summary>Data format name.</summary>
        Name: string;
      end;
    const
      // There are so few entries in this table it's not worth the overhead
      // of using a dicitionary for the lookup.
      LookupTable: array[0..1] of TMapRecord = (
        (Kind: TDataFormatKind.Native_v4;
          Name: 'CodeSnip Native Snippet Collection v4'),
        (Kind: TDataFormatKind.DCSC_v2;
          Name: 'DelphiDabbler Code Snippets Collection v2')
      );
    class function IndexOf(const AKind: TDataFormatKind): Integer; static;
  public
    const
      ///  <summary>Specifies the data format used for the default collection.
      ///  </summary>
      DefaultFormat = TDataFormatKind.Native_v4;
  public
    ///  <summary>Gets the name of the data format specified by
    ///  <c>AKind</c>. Returns an empty string if no name is associated with
    ///  <c>AKind</c>.</summary>
    class function GetName(const AKind: TDataFormatKind): string; static;
    ///  <summary>Returns an array of all supported data formats.</summary>
    ///  <returns><c>TArray&lt;TDataFormatKind&gt;</c>. Array of values
    ///  that identify the supported data formats.</returns>
    class function GetSupportedFormats: TArray<TDataFormatKind>; static;
  end;

implementation

{ TDataFormatInfo }

class function TDataFormatInfo.GetName(const AKind: TDataFormatKind): string;
var
  Idx: Integer;
begin
  Idx := IndexOf(AKind);
  if Idx < 0 then
    Exit('');
  Result := LookupTable[Idx].Name;
end;

class function TDataFormatInfo.GetSupportedFormats: TArray<TDataFormatKind>;
var
  Idx: Integer;
  Item: TMapRecord;
begin
  SetLength(Result, Length(LookupTable));
  Idx := 0;
  for Item in LookupTable do
  begin
    Result[Idx] := Item.Kind;
    Inc(Idx);
  end;
end;

class function TDataFormatInfo.IndexOf(const AKind: TDataFormatKind): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := Low(LookupTable) to High(LookupTable) do
    if LookupTable[Idx].Kind = AKind then
      Exit(Idx);
end;

{ TDataStorageDetails }

constructor TDataStorageDetails.Create(const AFormat: TDataFormatKind;
  const ADirectory: string);
begin
  fFormat := AFormat;
  fDirectory := ADirectory;
end;

procedure TDataStorageDetails.SetDirectory(const AValue: string);
begin
  fDirectory := AValue;
end;

end.
