{
 * FmFindTextDlg.pas
 *
 * Dialog box that is used to select criteria for text searches. Also defines a
 * class that is used to persist the last chosen search criteria.
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
 * The Original Code is FmFindTextDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmFindTextDlg;


interface


uses
  // Delphi
  StdCtrls, ExtCtrls, Controls, Classes,
  // Project
  FmGenericOKDlg, UBaseObjects, USearch;


type

  TTextSearchParams = class;

  {
  TFindTextDlg:
    Defines a dialog box that is used to select criteria for searches for
    routines containing specified text.
  }
  TFindTextDlg = class(TGenericOKDlg, INoPublicConstruct)
    cbCaseSensitive: TCheckBox;
    cbFindText: TComboBox;
    cbWholeWords: TCheckBox;
    gbOptions: TGroupBox;
    lblDesc: TLabel;
    lblFindText: TLabel;
    rgLogic: TRadioGroup;
    procedure btnOKClick(Sender: TObject);
    procedure cbFindTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    fSearchParams: TTextSearchParams; // Object that persists search options
    fSearch: ISearch;                 // Search corresponding to criteria
  strict protected
    procedure ArrangeForm; override;
      {Sizes and arrange controls in dialog box and determine size of dialog
      box.
      }
    procedure InitForm; override;
      {Populates and initialises controls.
      }
  public
    class function Execute(const AOwner: TComponent;
      out ASearch: ISearch): Boolean;
      {Displays dialog and returns search object based on entered criteria.
        @param AOwner [in] Component that owns this dialog.
        @param ASearch [out] Set to value of Search property: nil if user
          cancels.
        @return True if user OKs and search object created or false if user
          cancels and search object is nil.
      }
  end;

  {
  TTextSearchParams:
    Object used to store persistent parameters for text search.
    Format in ini file is
    + Count of history entries in 'HistoryCount'
    + History entries for each of history items with name 'History99' where 99
      is a whole number.
    + 'Logic' entry with value Ord(slAnd) or Ord(slOr)
    + 'MatchCase' and 'WholeWord' entries with value 1 if set and 0 if not set
  }
  TTextSearchParams = class(TObject)
  strict private
    const
      cMaxTextSearchHistory = 20;   // Max text search items remembered
    var
      fUpdated: Boolean;            // Tells if object's properties updaed
      fHistoryList: TStrings;       // Value of HistoryList property
      fOptions: TTextSearchOptions; // Value of Options property
      fLogic: TSearchLogic;         // Value of Logic property
    procedure SetHistoryList(const Value: TStrings);
      {Write access method for HistoryList property.
        @param Value [in] String list to be assigned to property.
      }
    procedure SetLogic(const Value: TSearchLogic);
      {Write access method for Logic property. Records value and flags updated.
        @param Value [in] New property value.
      }
    procedure SetOptions(const Value: TTextSearchOptions);
      {Write access method for Options property. Records value and flags
      updated.
        @param Value [in] New property value.
      }
    procedure ReadValues;
      {Reads search parameters and history list from persistent storage.
      }
    procedure WriteValues;
      {Writes search parameters and history list to persistent storage.
      }
    procedure OnHistoryChange(Sender: TObject);
      {Called when HistoryList string list changes. Flags that search params
      have been updated.
        @param Sender [in] Not used.
      }
  public
    constructor Create;
      {Class constructor. Sets up object from persistent storage.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object and saves current values to
      persistent storage if they have changed.
      }
    procedure AddHistoryItem(const Item: string);
      {Adds an item to top of history list. If item is in list it is moved to
      top. If item not in list it is added. If there are too many items in list
      last item is deleted. Empty strings are not added to list.
        @param Item [in] Item to be added to list.
      }
    function FirstHistoryItem: string;
      {Gets first item in history list.
        @return First item in history list if non empty and '' if list empty.
      }
    property HistoryList: TStrings
      read fHistoryList write SetHistoryList;
      {Stores list of recent entries in search text combo box}
    property Options: TTextSearchOptions
      read fOptions write SetOptions;
      {Text search options. Specifies type of compilation result we are
      looking for. See TTextSearchOption definition for possible values}
    property Logic: TSearchLogic
      read fLogic write SetLogic;
      {Search logic. Either AND or OR}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UCtrlArranger, UIStringList, USettings, FmGenericDlg;


{$R *.dfm}


{ TFindTextDlg }

procedure TFindTextDlg.cbFindTextChange(Sender: TObject);
  {OnChange event handler. Enables or disables OK button depending on if text
  box has text.
    @param Sender [in] Not used.
  }
begin
  inherited;
  btnOK.Enabled := cbFindText.Text <> '';
end;

procedure TFindTextDlg.ArrangeForm;
  {Sizes and arrange controls in dialog box and determine size of dialog box.
  }
begin
  TCtrlArranger.SetLabelHeight(lblDesc);
  lblDesc.Top := TCtrlArranger.BottomOf(cbFindText, 4);
  gbOptions.Top := TCtrlArranger.BottomOf(lblDesc, 8);
  rgLogic.Top := gbOptions.Top;
  pnlBody.ClientHeight := rgLogic.Top + rgLogic.Height + 4;
  inherited;
end;

procedure TFindTextDlg.btnOKClick(Sender: TObject);
  {OK button click event handler. Creates and records search criteria.
    @param Sender [in] Not used.
  }

  // ---------------------------------------------------------------------------
  function ParseOptionsControls: TTextSearchOptions;
    {Returns set of search options selected by user using options check boxes.
      @return Selected search options.
    }
  begin
    Result := [];
    if cbWholeWords.Checked then
      Include(Result, soWholeWord);
    if cbCaseSensitive.Checked then
      Include(Result, soMatchCase);
  end;

  function ParseLogicControls: TSearchLogic;
    {Returns search logic selected by user using search logic radio buttons.
      @return Required search logic.
    }
  begin
    if rgLogic.ItemIndex = 0 then
      Result := slAnd
    else
      Result := slOr;
  end;
  // ---------------------------------------------------------------------------

var
  SearchCriteria: ITextSearchCriteria;  // user's search criteria
begin
  inherited;
  // Create search criteria object containing info from dialog controls
  SearchCriteria := TSearchCriteriaFactory.CreateTextSearchCriteria(
    cbFindText.Text,
    ParseLogicControls,
    ParseOptionsControls
  );
  // Record search params in persistent store
  fSearchParams.AddHistoryItem(cbFindText.Text);
  fSearchParams.Logic := SearchCriteria.Logic;
  fSearchParams.Options := SearchCriteria.Options;
  // Create search object from the entered criteria
  fSearch := TSearchFactory.CreateTextSearch(SearchCriteria);
end;

procedure TFindTextDlg.FormCreate(Sender: TObject);
  {Called when form created. Creates owned objects.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Create search params object with default values
  fSearchParams := TTextSearchParams.Create;
end;

procedure TFindTextDlg.FormDestroy(Sender: TObject);
  {Frees owned object when form destroyed.
    @param Sender [in] Not used.
  }
begin
  inherited;
  FreeAndNil(fSearchParams);
end;

class function TFindTextDlg.Execute(const AOwner: TComponent;
  out ASearch: ISearch): Boolean;
  {Displays dialog and returns search object based on entered criteria.
    @param AOwner [in] Component that owns this dialog.
    @param ASearch [out] Set to value of Search property: nil if user cancels.
    @return True if user OKs and search object created or false if user cancels
      and search object is nil.
  }
begin
  with InternalCreate(AOwner) do
    try
      Result := (ShowModal = mrOK);
      ASearch := fSearch;
    finally
      Free;
    end;
end;

procedure TFindTextDlg.InitForm;
  {Populates and initialises controls.
  }
begin
  inherited;
  // Populate history combo and select most recent
  cbFindText.Items := fSearchParams.HistoryList;
  cbFindText.Text := fSearchParams.FirstHistoryItem;
  btnOK.Enabled := cbFindText.Text <> '';
  // Check required options
  cbWholeWords.Checked := soWholeWord in fSearchParams.Options;
  cbCaseSensitive.Checked := soMatchCase in fSearchParams.Options;
  // Set require logic radio button
  rgLogic.ItemIndex := Ord(fSearchParams.Logic);
end;

{ TTextSearchParams }

procedure TTextSearchParams.AddHistoryItem(const Item: string);
  {Adds an item to top of history list. If item is in list it is moved to top.
  If item not in list it is added. If there are too many items in list last item
  id deleted. Empty strings are not added to list.
    @param Item [in] Item to be added to list.
  }
var
  Idx: Integer; // index of item in history list
begin
  // Do nothing if item is ''
  if Item = '' then
    Exit;
  Idx := fHistoryList.IndexOf(Item);
  // Remove old items
  if Idx = -1 then
  begin
    if fHistoryList.Count = cMaxTextSearchHistory then
      fHistoryList.Delete(Pred(cMaxTextSearchHistory));
  end
  else
    fHistoryList.Delete(Idx);
  // Add new item
  fHistoryList.Insert(0, Item);
end;

constructor TTextSearchParams.Create;
  {Class constructor. Sets up object from persistent storage.
  }
begin
  inherited Create;
  // Initialise properties
  fOptions := [];
  fLogic := slOr;
  // Create object to hold history list
  fHistoryList := TStringList.Create;
  (fHistoryList as TStringList).OnChange := OnHistoryChange;
  // Initialise from persistent storage and record not changed
  ReadValues;
  fUpdated := False;
end;

destructor TTextSearchParams.Destroy;
  {Class destructor. Tears down object and saves current values to persistent
  storage if they have changed.
  }
begin
  if fUpdated then
    WriteValues;
  fHistoryList.Free;
  inherited;
end;

function TTextSearchParams.FirstHistoryItem: string;
  {Gets first item in history list.
    @return First item in history list if non empty and '' if list empty.
  }
begin
  if fHistoryList.Count = 0 then
    Result := ''
  else
    Result := fHistoryList[0];
end;

procedure TTextSearchParams.OnHistoryChange(Sender: TObject);
  {Called when HistoryList string list changes. Flags that search params have
  been updated.
    @param Sender [in] Not used.
  }
begin
  fUpdated := True;
end;

procedure TTextSearchParams.ReadValues;
  {Reads search parameters and history list from persistent storage.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage
begin
  // Read data from persistent storage
  Storage := Settings.ReadSection(ssFindText);
  // Update data
  Storage.GetStrings('HistoryCount', 'History%d').CopyTo(fHistoryList, True);
  fOptions := [];
  if Storage.ItemValues['MatchCase'] = '1' then
    Include(fOptions, soMatchCase);
  if Storage.ItemValues['WholeWord'] = '1' then
    Include(fOptions, soWholeWord);
  fLogic := TSearchLogic(StrToIntDef(Storage.ItemValues['Logic'], Ord(slOr)));
end;

procedure TTextSearchParams.SetHistoryList(const Value: TStrings);
  {Write access method for HistoryList property.
    @param Value [in] String list to be assigned to property.
  }
begin
  fHistoryList.Assign(Value);
end;

procedure TTextSearchParams.SetLogic(const Value: TSearchLogic);
  {Write access method for Logic property. Records value and flags updated.
    @param Value [in] New property value.
  }
begin
  fLogic := Value;
  fUpdated := True;
end;

procedure TTextSearchParams.SetOptions(const Value: TTextSearchOptions);
  {Write access method for Options property. Records value and flags updated.
    @param Value [in] New property value.
  }
begin
  fOptions := Value;
  fUpdated := True;
end;

procedure TTextSearchParams.WriteValues;
  {Writes search parameters and history list to persistent storage.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage
begin
  // Create new empty data object
  Storage := Settings.EmptySection(ssFindText);
  // Update data object
  Storage.SetStrings(
    'HistoryCount', 'History%d', TIStringList.Create(fHistoryList)
  );
  Storage.ItemValues['MatchCase'] := IntToStr(Ord(soMatchCase in fOptions));
  Storage.ItemValues['WholeWord'] := IntToStr(Ord(soWholeWord in fOptions));
  Storage.ItemValues['Logic'] := IntToStr(Ord(fLogic));
  // Write name=value pairs to appropriate ini file section
  Storage.Save;
end;

end.

