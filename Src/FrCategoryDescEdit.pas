{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that accepts and validates a description for a snippet
 * category.
}


unit FrCategoryDescEdit;


interface


uses
  // Delphi
  Forms, Controls, StdCtrls, Classes;


type
  {
  TCategoryDescEditFrame:
    Frame that accepts and validates a description for a snippet category.
  }
  TCategoryDescEditFrame = class(TFrame)
    edDescription: TEdit;
    lblDescription: TLabel;
    lblError: TLabel;
    procedure edDescriptionChange(Sender: TObject);
    procedure FrameEnter(Sender: TObject);
  public
    type
      {
      TDescriptionCheck:
        Type of OnDescriptionCheck event handler.
          @param Sender [in] Object that triggered event.
          @param Desc [in] Description being checked.
          @param Valid [in/out] When called Valid indicates whether the caller
            has the decided the description is valid or not. Handler can change
            the value to override the decision.
      }
      TDescriptionCheck = procedure(Sender: TObject; const Desc: string;
        var Valid: Boolean) of object;
  strict private
    fOnChange: TNotifyEvent;                // OnChange event handler
    fOnCheckDescription: TDescriptionCheck; // OnCheckDescription event handler
    function CategoryDescExists(const Desc: string): Boolean;
      {Checks if a category with a specified description exists. Case is
      ignored.
        @param Desc [in] Required description.
        @return True if a category exists with this description, False if not.
      }
    procedure UpdateControls;
      {Updates state of controls depending on current entries in frame.
      }
    function IsValidDescription: Boolean;
      {Checks if current content of description edit box is valid. Invalid
      content causes an error message to be displayed. Triggers the
      OnCheckDescription event to permit the owner to override the decision
      about validity.
        @return True if description is valid, False if not.
      }
    procedure DoChange;
      {Triggers OnChange event.
      }
    function GetDescription: string;
      {Read accessor for Description property.
        @return Current description from edit control.
      }
    procedure SetDescription(const Value: string);
      {Write accessor for Description property.
        @param Value [in] New description to be displayed in edit control.
      }
    function GetPrompt: string;
      {Read accessor for Prompt property.
        @return Current prompt text.
      }
    procedure SetPrompt(const Value: string);
      {Write accessor for Prompt property.
        @param Value [in] New prompt text to display.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Component that owns this frame.
      }
    procedure ArrangeFrame;
      {Arranges controls in frame and sizes it to fit the controls.
      }
    function IsValidEntry: Boolean;
      {Checks if data entered in frame is valid.
        @return True if entry is valid, False if not.
      }
    property Prompt: string read GetPrompt write SetPrompt;
      {Text that is displayed above description edit control}
    property Description: string read GetDescription write SetDescription;
      {Description currently displayed in description edit control, stripped of
      leading and trailing spaces}
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
      {Event triggered when content of description edit control changes}
    property OnCheckDescription: TDescriptionCheck
      read fOnCheckDescription write fOnCheckDescription;
      {Event triggered when validity of description is being checked. Provides
      information about the description being checked and whether it is
      considered valid. Handler can override validity}
  end;


implementation


uses
  // Delphi
  Windows {for inlining},
  // Project
  DB.UCategory, DB.UMain, UColours, UCtrlArranger, UFontHelper, UStrUtils;

{$R *.dfm}

{ TCategoryDescEditFrame }

procedure TCategoryDescEditFrame.ArrangeFrame;
  {Arranges controls in frame and sizes it to fit the controls.
  }
begin
  TCtrlArranger.SetLabelHeights(Self);
  edDescription.Top := TCtrlArranger.BottomOf(lblDescription, 4);
  lblError.Top := TCtrlArranger.BottomOf(edDescription, 4);
  Self.ClientHeight := TCtrlArranger.TotalControlHeight(Self);
  Self.ClientWidth := TCtrlArranger.TotalControlWidth(Self);
end;

function TCategoryDescEditFrame.CategoryDescExists(const Desc: string): Boolean;
  {Checks if a category with a specified description exists. Case is ignored.
    @param Desc [in] Required description.
    @return True if a category exists with this description, False if not.
  }
var
  Cat: TCategory; // each category in database
begin
  Result := False;
  for Cat in Database.Categories do
    if StrSameText(Desc, Cat.Description) then
    begin
      Result := True;
      Break;
    end;
end;

constructor TCategoryDescEditFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Component that owns this frame.
  }
begin
  inherited;
  lblError.Font.Color := clWarningText;
  TFontHelper.SetDefaultBaseFont(lblError.Font);
  UpdateControls;
end;

procedure TCategoryDescEditFrame.DoChange;
  {Triggers OnChange event.
  }
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCategoryDescEditFrame.edDescriptionChange(Sender: TObject);
  {Handles OnChange events in description edit control. Updates control state
  and triggers frame's OnChange event.
    @param Sender [in] Not used.
  }
begin
  UpdateControls;
  DoChange;
end;

procedure TCategoryDescEditFrame.FrameEnter(Sender: TObject);
  {Handles frame's OnEnter event. Sets focus on description edit control.
    @param Sender [in] Not used.
  }
begin
  edDescription.SetFocus;
end;

function TCategoryDescEditFrame.GetDescription: string;
  {Read accessor for Description property.
    @return Current description from edit control.
  }
begin
  Result := StrTrim(edDescription.Text);
end;

function TCategoryDescEditFrame.GetPrompt: string;
  {Read accessor for Prompt property.
    @return Current prompt text.
  }
begin
  Result := lblDescription.Caption;
end;

function TCategoryDescEditFrame.IsValidDescription: Boolean;
  {Checks if current content of description edit box is valid. Invalid content
  causes an error message to be displayed. Triggers the OnCheckDescription event
  to permit the owner to override the decision about validity.
    @return True if description is valid, False if not.
  }
begin
  // Valid content is either the empty (or all white space) string or a
  // description that is not used by any category other.
  Result := (Description = '') or not CategoryDescExists(Description);
  if Assigned(fOnCheckDescription) then
    fOnCheckDescription(Self, Description, Result);
end;

function TCategoryDescEditFrame.IsValidEntry: Boolean;
  {Checks if data entered in frame is valid.
    @return True if entry is valid, False if not.
  }
begin
  // To be valid, Description must be non-empty and not the description of
  // another category in the database.
  Result := (Description <> '') and IsValidDescription;
end;

procedure TCategoryDescEditFrame.SetDescription(const Value: string);
  {Write accessor for Description property.
    @param Value [in] New description to be displayed in edit control.
  }
begin
  edDescription.Text := Value;
  UpdateControls;
  DoChange;
end;

procedure TCategoryDescEditFrame.SetPrompt(const Value: string);
  {Write accessor for Prompt property.
    @param Value [in] New prompt text to display.
  }
begin
  lblDescription.Caption := Value;
end;

procedure TCategoryDescEditFrame.UpdateControls;
  {Updates state of controls depending on current entries in frame.
  }
begin
  lblError.Visible := not IsValidDescription;
end;

end.

