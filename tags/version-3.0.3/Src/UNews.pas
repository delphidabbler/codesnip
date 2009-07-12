{
 * UNews.pas
 *
 * Maintains a list of news items for display by program and reads list from
 * formatted data stream.
 *
 * v0.1 of 01 May 2006  - Original version.
 * v1.0 of 25 May 2006  - Improved and corrected comments.
 *                      - Changed exception used to detect bugs from Exception
 *                        type to EBug.
 * v1.1 of 12 May 2007  - Removed all code that tested for program version
 *                        numbers at which news items were targetted. This
 *                        testing now performed by web service rather than
 *                        program.
 *                      - Adapted to use revised news data format: versions
 *                        field no included in data stream.
 * v1.2 of 12 Jul 2009  - Replaced reference to UDataStreamReader unit with
 *                        UDataStreamIO.
 *                      - Made TNews and TNewsItems private sections strict.
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
 * The Original Code is UNews.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UNews;


interface


uses
  // Delphi
  Classes, Contnrs,
  // Project
  UDataStreamIO;


type

  {
  TNewsItem:
    Encapsulates a news item. Provides properties to describe it and a means
    of creating object from a formatted data stream.
  }
  TNewsItem = class(TObject)
  strict private
    fId: SmallInt;    // Value of Id property
    fDate: TDateTime; // Value of Date property
    fHTML: string;    // Value of HTML property
  public
    constructor Create(const Reader: TDataStreamReader);
      {Class constructor. Creates news item object from a data stream.
        @param Reader [in] Object used to read/parse data stream.
      }
    property Id: SmallInt read fId;
      {Unique identifier of news item}
    property Date: TDateTime read fDate;
      {Date news item published}
    property HTML: string read fHTML;
      {News item content as HTML}
  end;

  {
  TNews:
    Stores news items for display in application. Parses news items from a
    data stream supplied to constructor.
  }
  TNews = class(TObject)
  strict private
    fNewsItems: TObjectList;  // Stores list of news items
    procedure Parse(const NewsData: TStream);
      {Creates list of news items from a data stream.
        @param NewsData [in] Data stream containing news items.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of news items in list.
      }
    function GetItem(Idx: Integer): TNewsItem;
      {Read accessor for Items[] property.
        @param Idx [in] Index of required item.
        @return Requested news item.
      }
  public
    constructor Create(const NewsData: TStream);
      {Class constructor. Creates list of news items described by a data stream.
        @param NewsData [in] Stream containing news items.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    property Count: Integer read GetCount;
      {Number of news items in list}
    property Items[Idx: Integer]: TNewsItem read GetItem; default;
      {Array of news items}
  end;


implementation


uses
  // Delphi
  SysUtils;


{
  News data stream format
  -----------------------

  Stream compriises text characters. Numbers are encoded in hex format.

  File format is:

    Item-count: SmallInt    - number of news items in stream

  followed by Item-count news item records:

    Id: SmallInt            - unique item identifier
    Date: SizedString       - publication date as MySQL date (YYYY-MM-DD)
    Content: SizedString    - news item content as HTML

  Data types are:

    SmallInt      - 16 bit integer encoded as 4 hex digits
    SizedString   - SmallInt specifying string length followed by specified
                    number of characters
}


{ TNews }

constructor TNews.Create(const NewsData: TStream);
  {Class constructor. Creates list of news items described by a data stream.
    @param NewsData [in] Stream containing news items.
  }
begin
  inherited Create;
  fNewsItems := TObjectList.Create(True);
  Parse(NewsData);
end;

destructor TNews.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fNewsItems);   // frees news items in list
  inherited;
end;

function TNews.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of news items in list.
  }
begin
  Result := fNewsItems.Count;
end;

function TNews.GetItem(Idx: Integer): TNewsItem;
  {Read accessor for Items[] property.
    @param Idx [in] Index of required item.
    @return Requested news item.
  }
begin
  Result := fNewsItems[Idx] as TNewsItem;
end;

procedure TNews.Parse(const NewsData: TStream);
  {Creates list of news items from a data stream.
    @param NewsData [in] Data stream containing news items.
  }
var
  Reader: TDataStreamReader;  // object used to interpret data stream
  NumItems: SmallInt;         // number of news items to create
  Idx: Integer;               // loops thru news items
begin
  Reader := TDataStreamReader.Create(NewsData);
  try
    // Get number of items in data stream
    NumItems := Reader.ReadSmallInt;
    // Create required number of news items from stream
    for Idx := 1 to NumItems do
      fNewsItems.Add(TNewsItem.Create(Reader))
  finally
    FreeAndNil(Reader);
  end;
end;

{ TNewsItem }

function MySQLDateToDateTime(const MySQLDate: string): TDateTime;
  {Converts a date in MySQL format into a TDateTime.
    @param MySQLDate [in] Date string in format YYYY-MM-DD.
    @return Binary date value.
  }
begin
  Result := EncodeDate(
    StrToInt(Copy(MySQLDate, 1, 4)),
    StrToInt(Copy(MySQLDate, 6, 2)),
    StrToInt(Copy(MySQLDate, 9, 2))
  )
end;

constructor TNewsItem.Create(const Reader: TDataStreamReader);
  {Class constructor. Creates news item object from a data stream.
    @param Reader [in] Object used to read/parse data stream.
  }
begin
  inherited Create;
  // Get property values from stream. Order is important
  fId := Reader.ReadSmallInt;
  fDate := MySQLDateToDateTime(Reader.ReadSizedString);
  fHTML := Reader.ReadSizedString;
end;

end.

