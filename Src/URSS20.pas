{
 * URSS20.pas
 *
 * Implements classes that encapsulate the information contained in an RSS 2.0
 * XML document.
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
 * The Original Code is URSS20.pas
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


unit URSS20;


interface


uses
  // Delphi
  Generics.Defaults, Generics.Collections, XMLIntf,
  // Project
  UExceptions, UXMLDocumentEx;


type

  {
  TRSS20Item:
    Encapsulates an RSS 2.0 item. Instances cannot be created and must not be
    freed directly. Lifetimes are managed by TRSS20.
  }
  TRSS20Item = class abstract(TObject)
  strict protected
    function GetDescription: string; virtual; abstract;
      {Read accessor for Description property.
        @return Value of property.
      }
    function GetGUID: string; virtual; abstract;
      {Read accessor for GUID property.
        @return Value of property.
      }
    function GetGUIDIsPermalink: Boolean; virtual; abstract;
      {Read accessor for GUIDIsPermalink property.
        @return Value of property.
      }
    function GetLink: string; virtual; abstract;
      {Read accessor for Link property.
        @return Value of property.
      }
    function GetPubDateAsText: string; virtual; abstract;
      {Read accessor for PubDate property.
        @return Value of property.
      }
    function GetTitle: string; virtual; abstract;
      {Read accessor for Title property.
        @return Value of property.
      }
    function GetPermalink: string; virtual;
      {Read accessor for Permalink property.
        @return Value of property.
      }
    function GetPubDate: TDateTime; virtual;
      {Read accessor for PubDate property.
        @return Value of property.
      }
  public
    property Title: string read GetTitle;
      {The title of the item. May be '', but must be non-empty if Description is
      ''}
    property Link: string read GetLink;
      {The URL of complete story etc associated with the item. May be ''}
    property Description: string read GetDescription;
      {The item synopsis. May be '' but must be non-empty if Title is ''}
    property GUID: string read GetGUID;
      {A string that uniquely identifies the item. May by a URL (permalink). May
      be ''}
    property GUIDIsPermalink: Boolean read GetGUIDIsPermalink;
      {True if GUID is a permalink, False if not}
    property Permalink: string read GetPermalink;
      {A unique link to the item. This is the same as GUID if GUIDIsPermalink is
      True or '' if GUIDIsPermalink is False}
    property PubDate: TDateTime read GetPubDate;
      {Publication date of item in GMT (UTC). Set to midnight 01 Jan 1900 if no
      date specified or if date string is invalid}
    property PubDateAsText: string read GetPubDateAsText;
      {Publication date of the item in RFC822 date format. May be ''}
  end;

  {
  TRSS20:
    Encapsulates and RSS 2.0 channel. The channel is loaded from an XML object
    DOM. If there is more than one channel in the feed then only the first
    channel is used.
  }
  TRSS20 = class(TObject)
  strict private
    fItems: TObjectList<TRSS20Item>;  // Maintains list if items
    fChannel: IXMLNode;               // Reference to [first] channel element
    function GetCopyright: string;
      {Read accessor for Copyright property. Gets value from XML document.
        @return Value of Copyright property.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of items in XML document.
      }
    function GetDescription: string;
      {Read accessor for Description property. Gets value from XML document.
        @return Value of Description property.
      }
    function GetDocs: string;
      {Read accessor for Docs property. Gets value from XML document.
        @return Value of Docs property.
      }
    function GetItem(Idx: Integer): TRSS20Item;
      {Read accessor for Items[] property.
        @param Idx [in] Index of required item.
        @return Reference to required item object.
      }
    function GetLanguage: string;
      {Read accessor for Language property. Gets value from XML document.
        @return Value of Language property.
      }
    function GetLink: string;
      {Read accessor for Link property. Gets value from XML document.
        @return Value of Link property.
      }
    function GetPubDate: TDateTime;
      {Read accessor for PubDate property. Gets value from PubDateAsText
      property.
        @return Value of PubDate property.
      }
    function GetPubDateAsText: string;
      {Read accessor for PubDateAsText property. Gets value from XML document.
        @return Value of PubDateAsText property.
      }
    function GetTitle: string;
      {Read accessor for Title property. Gets value from XML document.
        @return Value of Title property.
      }
  strict protected
    function FindChannelChildNode(const NodeName: string): IXMLNode;
      {Finds a named child node of the "channel" node in the XML document.
        @param NodeName [in] Name of required node.
        @return Reference to required node or nil if not found.
      }
    function GetChannelChildNodeText(const NodeName: string): string;
      {Gets the text of a named text element child node of the "channel" node in
      the XML document.
        @param NodeName [in] Name of required node.
        @return The child node's text or '' if node not found or not a text
          element.
      }
  public
    constructor Create;
      {Object constructor. Sets up an empty RSS object.
      }
    destructor Destroy; override;
      {Object destructor. Tidies up object.
      }
    procedure Load(XMLDoc: IXMLDocumentEx);
      {Loads RSS information from an XML Document DOM.
        @param XMLDoc [in] DOM from which to read information.
      }
    function GetEnumerator: TEnumerator<TRSS20Item>;
      {Create an enumerator for the object that enumerates all the news items.
        @return Enumerator instance.
      }
    property Title: string read GetTitle;
      {Name of the channel. Must be non-empty}
    property Link: string read GetLink;
      {URL of the HTML website corresponding to the channel. Must be non-empty}
    property Description: string read GetDescription;
      {Short description of the channel. Must be non-empty}
    property Language: string read GetLanguage;
      {Language the channel is written in. Uses language codes defined by
      Netscape or W3C. May be empty}
    property Copyright: string read GetCopyright;
      {Copyright notice for the channel. May be empty}
    property PubDate: TDateTime read GetPubDate;
      {Publication date for the channel's in GMT (UTC). Set to midnight 01 Jan
      1900 if no date specified or if date string is invalid}
    property PubDateAsText: string read GetPubDateAsText;
      {Publication date for the channel's content in RFC822 date format. May be
      empty}
    property Docs: string read GetDocs;
      {A URL that points to the documentation for the format used in the RSS
      file. May be empty}
    property Items[Idx: Integer]: TRSS20Item read GetItem; default;
      {Array of new items in the channel}
    property Count: Integer read GetCount;
      {Number of news items in the channel}
  end;

  {
  ERSS20:
    Class of exception raised by classes in this unit.
  }
  ERSS20 = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  // Project
  URFC2822Date;


type
  {
  TRSS20ItemImpl:
    Concrete implementation of TRSS20Item that reads its properties from an
    "item" node from the RSS feed's XML DOM.
  }
  TRSS20ItemImpl = class sealed(TRSS20Item)
  strict private
    fItemNode: IXMLNode;  // Reference to XML item node
    function FindChildNode(const NodeName: string): IXMLNode;
      {Finds a named child node of the item node in the XML document.
        @param NodeName [in] Name of required node.
        @return Reference to required node or nil if not found.
      }
    function GetTextOfChildNode(const NodeName: string): string;
      {Gets the text of a named text element child node of the "item" node in
      the XML document.
        @param NodeName [in] Name of required node.
        @return The child node's text or '' if node not found or not a text
          element.
      }
  strict protected
    function GetDescription: string; override;
      {Read accessor for Description property. Gets value from XML item node.
        @return Value of Description property.
      }
    function GetGUID: string; override;
      {Read accessor for GUID property. Gets value from XML item node.
        @return Value of GUID property.
      }
    function GetGUIDIsPermalink: Boolean; override;
      {Read accessor for GUIDIsPermalink property. Gets value from "guid" child
      node.
        @return Value of GUIDIsPermalink property.
      }
    function GetLink: string; override;
      {Read accessor for Link property. Gets value from XML item node.
        @return Value of Link property.
      }
    function GetPubDateAsText: string; override;
      {Read accessor for PubDateAsText property. Gets value from XML item node.
        @return Value of PubDateAsText property.
      }
    function GetTitle: string; override;
      {Read accessor for Title property. Gets value from XML item node.
        @return Value of Title property.
      }
  public
    constructor Create(ItemNode: IXMLNode);
      {Object constructor. Creates item instance that has properties of a
       item node from XML document.
        @param ItemNode [in] XML "item" node that contains required information.
      }
    destructor Destroy; override;
      {Object desctructor. Tears down object.
      }
  end;

function DecodeDate(const DateStr: string): TDateTime;
  {Decodes RFC2822 date string into TDateTime format in GMT. If date string is
  empty or invalid a default date is used.
    @param DateStr [in] RFC2822 date format string to be decoded.
    @return GMT TDateTime equivalent value or default date value if date string
      empty or invalid.
  }
  // ---------------------------------------------------------------------------
  function DefaultDate: TDateTime;
    {Provides a default date.
      @return Default date of midnight, 01 Jan 1900.
    }
  begin
    Result := EncodeDate(1900, 1, 1);
  end;
  // ---------------------------------------------------------------------------
begin
  if DateStr <> '' then
  begin
    try
      Result := RFC2822DateToGMTDateTime(DateStr);
    except
      Result := DefaultDate;
    end;
  end
  else
    Result := DefaultDate;
end;

{ TRSS20 }

constructor TRSS20.Create;
  {Object constructor. Sets up an empty RSS object.
  }
begin
  inherited Create;
  fItems := TObjectList<TRSS20Item>.Create(True);
end;

destructor TRSS20.Destroy;
  {Object destructor. Tidies up object.
  }
begin
  fItems.Free;  // frees owned item objects
  inherited;
end;

function TRSS20.FindChannelChildNode(const NodeName: string): IXMLNode;
  {Finds a named child node of the "channel" node in the XML document.
    @param NodeName [in] Name of required node.
    @return Reference to required node or nil if not found.
  }
begin
  Result := fChannel.ChildNodes.FindNode(NodeName);
end;

function TRSS20.GetChannelChildNodeText(const NodeName: string): string;
  {Gets the text of a named text element child node of the "channel" node in the
  XML document.
    @param NodeName [in] Name of required node.
    @return The child node's text or '' if node not found or not a text element.
  }
var
  Node: IXMLNode; // named child node
begin
  Node := FindChannelChildNode(NodeName);
  if Assigned(Node) and (Node.IsTextElement) then
    Result := Node.Text
  else
    Result := '';
end;

function TRSS20.GetCopyright: string;
  {Read accessor for Copyright property. Gets value from XML document.
    @return Value of Copyright property.
  }
begin
  Result := GetChannelChildNodeText('copyright');
end;

function TRSS20.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of items in XML document.
  }
begin
  Result := fItems.Count;
end;

function TRSS20.GetDescription: string;
  {Read accessor for Description property. Gets value from XML document.
    @return Value of Description property.
  }
begin
  Result := GetChannelChildNodeText('description');
end;

function TRSS20.GetDocs: string;
  {Read accessor for Docs property. Gets value from XML document.
    @return Value of Docs property.
  }
begin
  Result := GetChannelChildNodeText('docs');
end;

function TRSS20.GetEnumerator: TEnumerator<TRSS20Item>;
  {Create an enumerator for the object that enumerates all the news items.
    @return Enumerator instance.
  }
begin
  Result := fItems.GetEnumerator;
end;

function TRSS20.GetItem(Idx: Integer): TRSS20Item;
  {Read accessor for Items[] property.
    @param Idx [in] Index of required item.
    @return Reference to required item object.
  }
begin
  Result := fItems[Idx];
end;

function TRSS20.GetLanguage: string;
  {Read accessor for Language property. Gets value from XML document.
    @return Value of Language property.
  }
begin
  Result := GetChannelChildNodeText('language');
end;

function TRSS20.GetLink: string;
  {Read accessor for Link property. Gets value from XML document.
    @return Value of Link property.
  }
begin
  Result := GetChannelChildNodeText('link');
end;

function TRSS20.GetPubDate: TDateTime;
  {Read accessor for PubDate property. Gets value from PubDateAsText property.
    @return Value of PubDate property.
  }
begin
  Result := DecodeDate(PubDateAsText);
end;

function TRSS20.GetPubDateAsText: string;
  {Read accessor for PubDateAsText property. Gets value from XML document.
    @return Value of PubDateAsText property.
  }
begin
  Result := GetChannelChildNodeText('pubDate');
end;

function TRSS20.GetTitle: string;
  {Read accessor for Title property. Gets value from XML document.
    @return Value of Title property.
  }
begin
  Result := GetChannelChildNodeText('title');
end;

procedure TRSS20.Load(XMLDoc: IXMLDocumentEx);
  {Loads RSS information from an XML Document DOM.
    @param XMLDoc [in] DOM from which to read information.
  }
resourcestring
  // Error message
  sInvalidDoc = 'Invalid RSS document';
var
  ItemNodes: IXMLSimpleNodeList;  // list of items nodes of channel
  ItemNode: IXMLNode;             // each item in ItemNodes
begin
  fItems.Clear;
  fChannel := XMLDoc.FindNode('rss\channel');
  if not Assigned(fChannel) then
    raise ERSS20.Create(sInvalidDoc);
  // create items
  ItemNodes := XMLDoc.FindChildNodes(fChannel, 'item');
  for ItemNode in ItemNodes do
    fItems.Add(TRSS20ItemImpl.Create(ItemNode));
end;

{ TRSS20Item }

function TRSS20Item.GetPermalink: string;
  {Read accessor for Permalink property.
    @return Value of property.
  }
begin
  // Calculates value from other properties
  if GUIDIsPermalink then
    Result := GUID
  else
    Result := '';
end;

function TRSS20Item.GetPubDate: TDateTime;
  {Read accessor for PubDate property.
    @return Value of property.
  }
begin
  // Calculates value from PubDateAsText property
  Result := DecodeDate(PubDateAsText);
end;

{ TRSS20ItemImpl }

constructor TRSS20ItemImpl.Create(ItemNode: IXMLNode);
  {Object constructor. Creates item instance that has properties of a specified
  item node from XML document.
    @param ItemNode [in] XML "item" node that contains required information.
  }
begin
  inherited Create;
  fItemNode := ItemNode;
end;

destructor TRSS20ItemImpl.Destroy;
  {Object desctructor. Tears down object.
  }
begin
  fItemNode := nil;
  inherited;
end;

function TRSS20ItemImpl.FindChildNode(const NodeName: string): IXMLNode;
  {Finds a named child node of the item node in the XML document.
    @param NodeName [in] Name of required node.
    @return Reference to required node or nil if not found.
  }
begin
  Result := fItemNode.ChildNodes.FindNode(NodeName);
end;

function TRSS20ItemImpl.GetDescription: string;
  {Read accessor for Description property. Gets value from XML item node.
    @return Value of Description property.
  }
begin
  Result := GetTextOfChildNode('description');
end;

function TRSS20ItemImpl.GetGUID: string;
  {Read accessor for GUID property. Gets value from XML item node.
    @return Value of GUID property.
  }
begin
  Result := GetTextOfChildNode('guid');
end;

function TRSS20ItemImpl.GetGUIDIsPermalink: Boolean;
  {Read accessor for GUIDIsPermalink property. Gets value from "guid" child
  node.
    @return Value of GUIDIsPermalink property.
  }
var
  Node: IXMLNode; // reference to "guid" node
begin
  Node := FindChildNode('guid');
  if not Assigned(Node) then
    Exit(False);
  if not Node.HasAttribute('isPermaLink') then
    Exit(True); // isPermaLink is optional and defaults to true
  Result := Node.Attributes['isPermaLink'] = 'true';
end;

function TRSS20ItemImpl.GetLink: string;
  {Read accessor for Link property. Gets value from XML item node.
    @return Value of Link property.
  }
begin
  Result := GetTextOfChildNode('link');
end;


function TRSS20ItemImpl.GetPubDateAsText: string;
  {Read accessor for PubDateAsText property. Gets value from XML item node.
    @return Value of PubDateAsText property.
  }
begin
  Result := GetTextOfChildNode('pubDate');
end;

function TRSS20ItemImpl.GetTextOfChildNode(const NodeName: string): string;
  {Gets the text of a named text element child node of the "item" node in the
  XML document.
    @param NodeName [in] Name of required node.
    @return The child node's text or '' if node not found or not a text element.
  }
var
  Node: IXMLNode;
begin
  Node := FindChildNode(NodeName);
  if Assigned(Node) and (Node.IsTextElement) then
    Result := Node.Text
  else
    Result := '';
end;

function TRSS20ItemImpl.GetTitle: string;
  {Read accessor for Title property. Gets value from XML item node.
    @return Value of Title property.
  }
begin
  Result := GetTextOfChildNode('title');
end;

end.

