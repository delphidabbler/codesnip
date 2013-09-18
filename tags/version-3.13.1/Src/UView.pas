{
 * UView.pas
 *
 * Classes that encapsulate and provide information about "view items" that are
 * displayed in the user interface, e.g.. routines, categories and the welcome
 * page.
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
 * The Original Code is UView.pas
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


unit UView;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UAlphabet, USnippetKindInfo, USnippets;


type

  {
  TViewKind:
    Various kinds of item that can be displayed in GUI.
  }
  TViewKind = (
    vkNone,         // view kind not defined
    vkWelcome,      // welcome page
    vkRoutine,      // information about a routine
    vkCategory,     // information about a category
    vkSnipKind,     // information about a kind of snippet
    vkAlphabet      // information about snippets beginning with a letter
  );

  {
  TViewItem:
    Encapsulates and provides information about "view items" that are displayed
    in the overview pane of the GUI.
  }
  TViewItem = class(TObject)
  strict private
    var
      fData: TObject;   // More information about view item, depending on kind
      fKind: TViewKind; // Kind of view item
    function GetDescription: string;
      {Read accessor for Description property.
        @return Description.
      }
    function GetRoutine: TRoutine;
      {Read access for Routine property. Kind must be vkRoutine.
        @return Data object describing routine.
      }
    function GetCategory: TCategory;
      {Read access for Category property. Kind must be vkCategory.
        @return Data object describing category.
      }
    function GetAlphChar: TLetter;
      {Read access for AlphaChar property. Kind must be vkSnipKind.
        @return Data object describing alphabetic section.
      }
    function GetSnippetKind: TSnippetKindInfo;
      {Read access for SnippetKind property. Kind must be vkSnipKind.
        @return Data object describing snippet kind section.
      }
    constructor InternalCreate(const Kind: TViewKind; const Data: TObject);
      {Private object constructor. Creates a view item of a specified kind with
      extra information.
        @param Kind [in] Kind of view item to create.
        @param Data [in] Object providing extra information. Object type depends
          on Kind and must be as follows:
          + vkRoutine => TRoutine
          + vkCategory => TCategory
          + vkSnipKind => TSnippetKindInfo
          + vkAlphabet => TLetter
          + vkNone or vkWelcome => nil
      }
  public
    constructor Create; overload;
      {Object constructor. Creates a view item of kind vkNone.
      }
    constructor Create(const Kind: TViewKind); overload;
      {Object constructor. Creates a view item of a kind that has no associated
      object to store as extra information.
        @param Kind [in] Kind of view item to create. Must be vkNone or
          vkWelcome
      }
    constructor Create(const ViewItem: TViewItem); overload;
      {Object constructor. Creates a view item that is a clone of another view
      item.
        @param ViewItem [in] View item to be cloned.
      }
    constructor Create(const Routine: TRoutine); overload;
      {Object constructor. Creates a view item representing a routine.
        @param Routine [in] Routine to be represented.
      }
    constructor Create(const Category: TCategory); overload;
      {Object constructor. Creates a view item representing a category.
        @param Category [in] Category to be represented.
      }
    constructor Create(const SnippetKind: TSnippetKindInfo); overload;
      {Object constructor. Creates a view item representing a snippet kind.
        @param SnippetKind [in] Snippet kind to be represented.
      }
    constructor Create(const Letter: TLetter); overload;
      {Object constructor. Creates a view item representing a alphabetic letter.
        @param Letter [in] Letter to be represented.
      }
    procedure Assign(const ViewItem: TViewItem);
      {Sets this view item to have same properties as another view item.
        @param ViewItem [in] Item we are copying. A nil view item implies a Kind
          of vkNone.
      }
    function IsEqual(const ViewItem: TViewItem): Boolean;
      {Checks if this view item is the same as another view item.
        @param ViewItem [in] View item being testing for equality.
        @return True if view items are same, false otherwise.
      }
    property Kind: TViewKind read fKind;
      {Kind of view item}
    property Description: string read GetDescription;
      {Description of view item}
    property Category: TCategory read GetCategory;
      {Information about category represented by view item. Only valid when
      Kind = vkCategory}
    property Routine: TRoutine read GetRoutine;
      {Information about routine represented by view item. Only valid when
      Kind = vkRoutine}
    property AlphaChar: TLetter read GetAlphChar;
      {Information about an alphabetic character represented by view item. Only
      valid when Kind = vkAlphabet}
    property SnippetKind: TSnippetKindInfo read GetSnippetKind;
      {Information about a snippet kind represented by a view item. Only valid
      when Kind = vkSnippetKind}
  end;

  {
  TViewItemList:
    Implements a list of TViewItem objects.
  }
  TViewItemList = class(TObjectList<TViewItem>)
  public
    constructor Create;
      {Object constructor. Sets up list.
      }
  end;


implementation


{ TViewItem }

procedure TViewItem.Assign(const ViewItem: TViewItem);
  {Sets this view item to have same properties as another view item.
    @param ViewItem [in] Item we are copying. A nil view item implies a Kind of
      vkNone.
  }
begin
  if Assigned(ViewItem) then
  begin
    // New view item not nil: copy it
    Self.fKind := ViewItem.fKind;
    Self.fData := ViewItem.fData;
  end
  else
  begin
    // New view item nil: make a vkNone item
    Self.fKind := vkNone;
    Self.fData := nil;
  end;
end;

constructor TViewItem.Create;
  {Object constructor. Creates a view item of kind vkNone.
  }
begin
  inherited Create;
  fKind := vkNone;
  fData := nil;
end;

constructor TViewItem.Create(const ViewItem: TViewItem);
  {Object constructor. Creates a view item that is a clone of another view item.
    @param ViewItem [in] View item to be cloned.
  }
begin
  inherited Create;
  Assign(ViewItem);
end;

constructor TViewItem.Create(const SnippetKind: TSnippetKindInfo);
  {Object constructor. Creates a view item representing a snippet kind.
    @param SnippetKind [in] Snippet kind to be represented.
  }
begin
  InternalCreate(vkSnipKind, SnippetKind);
end;

constructor TViewItem.Create(const Routine: TRoutine);
  {Object constructor. Creates a view item representing a routine.
    @param Routine [in] Routine to be represented.
  }
begin
  InternalCreate(vkRoutine, Routine);
end;

constructor TViewItem.Create(const Letter: TLetter);
  {Object constructor. Creates a view item representing a alphabetic letter.
    @param Letter [in] Letter to be represented.
  }
begin
  InternalCreate(vkAlphabet, Letter);
end;

constructor TViewItem.Create(const Kind: TViewKind);
  {Object constructor. Creates a view item of a kind that has no associated
  object to store as extra information.
    @param Kind [in] Kind of view item to create. Must be vkNone or vkWelcome
  }
begin
  Assert(Kind in [vkNone, vkWelcome],
    ClassName + '.Create: Kind must be vkNone or vkWelcome');
  InternalCreate(Kind, nil);
end;

constructor TViewItem.Create(const Category: TCategory);
  {Object constructor. Creates a view item representing a category.
    @param Category [in] Category to be represented.
  }
begin
  InternalCreate(vkCategory, Category);
end;

function TViewItem.GetAlphChar: TLetter;
  {Read access for AlphaChar property. Kind must be vkSnipKind.
    @return Data object describing alphabetic section.
  }
begin
  Assert(fKind = vkAlphabet,
    ClassName + '.GetAlphaChar: Kind is not vkAlphabet');
  Result := fData as TLetter;
end;

function TViewItem.GetCategory: TCategory;
  {Read access for Category property. Kind must be vkCategory.
    @return Data object describing category.
  }
begin
  Assert(fKind = vkCategory,
    ClassName + '.GetCategory: Kind is not vkCategory');
  Result := fData as TCategory;
end;

function TViewItem.GetDescription: string;
  {Read accessor for Description property.
    @return Description.
  }
resourcestring
  // view item descriptions
  sNoneDesc         = 'Empty item';
  sWelcomeDesc      = 'Welcome page';
  sUnknownDesc      = 'Unknown view item type';
begin
  // Description depends on kind of view item
  case fKind of
    vkNone:         Result := sNoneDesc;
    vkWelcome:      Result := sWelcomeDesc;
    vkRoutine:      Result := Routine.Name;
    vkCategory:     Result := Category.Description;
    vkAlphabet:     Result := AlphaChar.Letter;
    vkSnipKind:     Result := SnippetKind.Description;
    else            Result := sUnknownDesc;
  end;
end;

function TViewItem.GetRoutine: TRoutine;
  {Read access for Routine property. Kind must be vkRoutine.
    @return Data object describing routine.
  }
begin
  Assert(fKind = vkRoutine, ClassName + '.GetRoutine: Kind is not vkRoutine');
  Result := fData as TRoutine;
end;

function TViewItem.GetSnippetKind: TSnippetKindInfo;
  {Read access for SnippetKind property. Kind must be vkSnipKind.
    @return Data object describing snippet kind section.
  }
begin
  Assert(fKind = vkSnipKind,
    ClassName + '.GetSnippetKind: Kind is not vkSnipKind');
  Result := fData as TSnippetKindInfo;
end;

constructor TViewItem.InternalCreate(const Kind: TViewKind;
  const Data: TObject);
  {Private object constructor. Creates a view item of a specified kind with
  extra information.
    @param Kind [in] Kind of view item to create.
    @param Data [in] Object providing extra information. Object type depends on
      Kind and must be as follows:
      + vkRoutine => TRoutine
      + vkCategory => TCategory
      + vkSnipKind => TSnippetKindInfo
      + vkAlphabet => TLetter
      + vkNone or vkWelcome => nil
  }
begin
  Assert(
    ( (Kind in [vkNone, vkWelcome]) and not Assigned(Data) )
    or ( (Kind = vkRoutine) and Assigned(Data) and (Data is TRoutine) )
    or ( (Kind = vkCategory) and Assigned(Data) and (Data is TCategory) )
    or ( (Kind = vkSnipKind) and Assigned(Data) and (Data is TSnippetKindInfo) )
    or ( (Kind = vkAlphabet) and Assigned(Data) and (Data is TLetter) ),
    ClassName + '.Create: Invalid parameters'
  );
  inherited Create;
  // Record kind and data object
  fKind := Kind;
  fData := Data;
end;

function TViewItem.IsEqual(const ViewItem: TViewItem): Boolean;
  {Checks if this view item is the same as another view item.
    @param ViewItem [in] View item being testing for equality.
    @return True if view items are same, false otherwise.
  }
begin
  // Kinds of objects must be same for equality
  Result := (Self.fKind = ViewItem.fKind);
  // If kind has a data object, they must also be "same"
  if Result then
    case fKind of
      vkRoutine:  Result := Routine.IsEqual(ViewItem.Routine);
      vkCategory: Result := Category.IsEqual(ViewItem.Category);
      vkAlphabet: Result := AlphaChar.Letter = ViewItem.AlphaChar.Letter;
      vkSnipKind: Result := SnippetKind.Kind = ViewItem.SnippetKind.Kind;
    end;
end;

{ TViewItemList }

constructor TViewItemList.Create;
  {Object constructor. Sets up list.
  }
begin
  // List does not own the objects stored in it
  inherited Create(False);
end;

end.

