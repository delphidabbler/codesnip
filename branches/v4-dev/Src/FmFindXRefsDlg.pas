{
 * FmFindXRefsDlg.pas
 *
 * Dialog box that is used to select criteria for searches for cross referenced
 * snippets. Also defines a class that is used to persist the last chosen search
 * criteria.
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
 * The Original Code is FmFindXRefsDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmFindXRefsDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, UBaseObjects, USearch, USnippets;


type

  TXRefSearchParams = class;

  {
  TFindXRefsDlg:
    Defines a dialog box that is used to select criteria for searches for
    cross referenced snippets.
  }
  TFindXRefsDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblDesc: TLabel;
    lblSnippetName: TLabel;
    chkRequired: TCheckBox;
    chkSeeAlso: TCheckBox;
    chkIncludeSnippet: TCheckBox;
    chkRequiredRecurse: TCheckBox;
    chkSeeAlsoRecurse: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchCheckClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    fSearchParams: TXRefSearchParams; // Persists XRef search options
    fSearch: ISearch;                 // Search for user's criteria
    fSnippet: TSnippet;               // Snippet whose x-refs to be found
    procedure UpdateControls;
      {Updates state of controls.
      }
  strict protected
    procedure ConfigForm; override;
      {Style labels and record name of snippet in labels and checkboxes.
      }
    procedure ArrangeForm; override;
      {Arranges components on form and rezize form as required.
      }
    procedure InitForm; override;
      {Populates and initialises controls.
      }
  public
    class function Execute(const AOwner: TComponent; const Snippet: TSnippet;
      out ASearch: ISearch): Boolean;
      {Displays dialog and returns search object based on entered criteria.
        @param AOwner [in] Component that owns this dialog.
        @param Snippet [in] Snippet whose cross references are to be found.
        @param ASearch [out] Search to be performed if user OKs. Has criteria
          that causes specified cross references be returned by search. Set to
          nil if user cancels.
        @return True if user OKs and search object created or false if user
          cancels and search object is nil.
      }
  end;

  {
  TXRefSearchParams:
    Object used to store persistent parameters for cross-reference search.
    Format in ini file is
    + Whether to include base snippet in search in 'IncludeRoutine'
    + Whether to include required snippets in search in 'Required'
    + Whether to recusively include required snippets in 'RequiredRecurse'
    + Whether to include "see also" snippets in search in 'SeeAlso'
    + Whether to recusively include "see also" snippets in 'SeeAlsoRecurse'
  }
  TXRefSearchParams = class(TObject)
  strict private
    fUpdated: Boolean;            // Flag true if object's properties change
    fOptions: TXRefSearchOptions; // Options controlling X-ref search
    procedure SetOptions(const Value: TXRefSearchOptions);
      {Write accessor for Options property.
        @param Value [in] Option controlling X-ref search.
      }
    procedure Updated;
      {Sets updated flag.
      }
    procedure ReadValues;
      {Reads search parameters from persistent storage.
      }
    procedure WriteValues;
      {Writes search parameters to persistent storage.
      }
  public
    constructor Create;
      {Class constructor. Sets up object with parameters from persistent
      storage.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object and saves current values to
      persistent storage if they have changed.
      }
    property Options: TXRefSearchOptions
      read fOptions write SetOptions;
      {X-ref search options}
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UColours, UCtrlArranger, USettings;


{$R *.dfm}


{ TFindXRefsDlg }

procedure TFindXRefsDlg.ArrangeForm;
  {Arranges components on form and rezize form as required.
  }
begin
  // Place snippet name after end of description label
  TCtrlArranger.MoveToRightOf(lblDesc, lblSnippetName);
  // Check if snippet name is clipped at right of dialog box and increase
  // available body panel space if so
  if lblSnippetName.Left + lblSnippetName.Width > pnlBody.ClientWidth then
    pnlBody.ClientWidth := lblSnippetName.Left + lblSnippetName.Width;
  // Inherited arrangement: will set dialog width based on body panel width
  inherited;
end;

procedure TFindXRefsDlg.btnOKClick(Sender: TObject);
  {OK button click event handler. Creates and records search criteria.
    @param Sender [in] Not used.
  }

  // ---------------------------------------------------------------------------
  function GetOptionsFromUI: TXRefSearchOptions;
    {Builds set of options selected by user in user interface.
      @return Required option set.
    }
  begin
    Result := [];
    if chkRequired.Checked then
      Include(Result, soRequired);
    if chkRequiredRecurse.Checked then
      Include(Result, soRequiredRecurse);
    if chkSeeAlso.Checked then
      Include(Result, soSeeAlso);
    if chkSeeAlsoRecurse.Checked then
      Include(Result, soSeeAlsoRecurse);
    if chkIncludeSnippet.Checked then
      Include(Result, soIncludeRoutine);
  end;
  // ---------------------------------------------------------------------------

var
  SearchCriteria: IXRefSearchCriteria;  // user's search criteria
begin
  // Create search criteria from entries made in dialog box
  SearchCriteria := TSearchCriteriaFactory.CreateXRefSearchCriteria(
    fSnippet, GetOptionsFromUI
  );
  // Persist the search criteria
  fSearchParams.Options := SearchCriteria.Options;
  // Create search object from the entered criteria
  fSearch := TSearchFactory.CreateXRefSearch(SearchCriteria);
end;

procedure TFindXRefsDlg.ConfigForm;
  {Style labels and record name of snippet in labels and checkboxes.
  }
begin
  inherited;
  // Set label font styles and colours
  lblSnippetName.Font.Style := [fsBold];
  if fSnippet.UserDefined then
    lblSnippetName.Font.Color := clUserRoutine;
  // Display selected snippet name in appropriate controls
  lblSnippetName.Caption := fSnippet.Name;
  chkIncludeSnippet.Caption := Format(
    chkIncludeSnippet.Caption, [fSnippet.Name]
  );
end;

class function TFindXRefsDlg.Execute(const AOwner: TComponent;
  const Snippet: TSnippet; out ASearch: ISearch): Boolean;
  {Displays dialog and returns search object based on entered criteria.
    @param AOwner [in] Component that owns this dialog.
    @param Snippet [in] Snippet whose cross references are to be found.
    @param ASearch [out] Search to be performed if user OKs. Has criteria that
      causes specified cross references be returned by search. Set to nil if
      user cancels.
    @return True if user OKs and search object created or false if user cancels
      and search object is nil.
  }
begin
  Assert(Assigned(Snippet), ClassName + '.Execute: Snippet is nil');
  with InternalCreate(AOwner) do
    try
      fSnippet := Snippet;
      Result := (ShowModal = mrOK);
      ASearch := fSearch;
    finally
      Free;
    end;
end;

procedure TFindXRefsDlg.FormCreate(Sender: TObject);
  {Creates owned objects when form is created.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Create search params object with default values
  fSearchParams := TXRefSearchParams.Create;
end;

procedure TFindXRefsDlg.FormDestroy(Sender: TObject);
  {Frees owned object when form destroyed.
    @param Sender [in] Not used.
  }
begin
  inherited;
  FreeAndNil(fSearchParams);
end;

procedure TFindXRefsDlg.InitForm;
  {Populates and initialises controls.
  }
begin
  inherited;
  // Check appropriate check boxes
  chkRequired.Checked := soRequired in fSearchParams.Options;
  chkRequiredRecurse.Checked := soRequiredRecurse in fSearchParams.Options;
  chkSeeAlso.Checked := soSeeAlso in fSearchParams.Options;
  chkSeeAlsoRecurse.Checked := soSeeAlsoRecurse in fSearchParams.Options;
  chkIncludeSnippet.Checked := soIncludeRoutine in fSearchParams.Options;
  // Update other controls per state of check boxes
  UpdateControls;
end;

procedure TFindXRefsDlg.SearchCheckClick(Sender: TObject);
  {Click event handler for "required snippets" and "see also" check boxes.
  Updates state of OK button.
    @param Sender [in] Not used.
  }
begin
  UpdateControls;
end;

procedure TFindXRefsDlg.UpdateControls;
  {Updates state of controls.
  }
begin
  btnOK.Enabled := chkRequired.Checked or chkSeeAlso.Checked;
  chkRequiredRecurse.Enabled := chkRequired.Checked;
  chkSeeAlsoRecurse.Enabled := chkSeeAlso.Checked;
end;


{ TXRefSearchParams }

constructor TXRefSearchParams.Create;
  {Class constructor. Sets up object with parameters from persistent storage.
  }
begin
  inherited Create;
  ReadValues;
  fUpdated := False;
end;

destructor TXRefSearchParams.Destroy;
  {Class destructor. Tears down object and saves current values to persistent
  storage if they have changed.
  }
begin
  if fUpdated then
    WriteValues;
  inherited;
end;

procedure TXRefSearchParams.ReadValues;
  {Reads search parameters from persistent storage.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage
begin
  Storage := Settings.ReadSection(ssFindXRefs);
  fOptions := [];
  if Boolean(StrToIntDef(Storage.ItemValues['IncludeRoutine'], 1)) then
    Include(fOptions, soIncludeRoutine);
  if Boolean(StrToIntDef(Storage.ItemValues['Required'], 1)) then
    Include(fOptions, soRequired);
  if Boolean(StrToIntDef(Storage.ItemValues['RequiredRecurse'], 0)) then
    Include(fOptions, soRequiredRecurse);
  if Boolean(StrToIntDef(Storage.ItemValues['SeeAlso'], 0)) then
    Include(fOptions, soSeeAlso);
  if Boolean(StrToIntDef(Storage.ItemValues['SeeAlsoRecurse'], 0)) then
    Include(fOptions, soSeeAlsoRecurse);
end;

procedure TXRefSearchParams.SetOptions(const Value: TXRefSearchOptions);
  {Write accessor for Options property.
    @param Value [in] Option controlling X-ref search.
  }
begin
  fOptions := Value;
  Updated;
end;

procedure TXRefSearchParams.Updated;
  {Sets updated flag.
  }
begin
  fUpdated := True;
end;

procedure TXRefSearchParams.WriteValues;
  {Writes search parameters to persistent storage.
  }
var
  Storage: ISettingsSection;  // object used to access persistent storage

  // ---------------------------------------------------------------------------
  procedure StoreOption(const Name: string; const Option: TXRefSearchOption);
    {Stores a X-ref search option in persistent storage object.
      @param Name [in] Name of option in storage.
      @param Option [in] Option to store.
    }
  begin
    // Store boolean flag as string for each option
    Storage.ItemValues[Name] := IntToStr(Ord(Option in fOptions));
  end;
  // ---------------------------------------------------------------------------

begin
  // Create blank persistent storage object
  Storage := Settings.EmptySection(ssFindXRefs);
  // Record parameters
  StoreOption('IncludeRoutine', soIncludeRoutine);
  StoreOption('Required', soRequired);
  StoreOption('RequiredRecurse', soRequiredRecurse);
  StoreOption('SeeAlso', soSeeAlso);
  StoreOption('SeeAlsoRecurse', soSeeAlsoRecurse);
  // Store data
  Storage.Save;
end;

end.

