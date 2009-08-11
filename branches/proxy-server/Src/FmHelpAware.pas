{ ##
  @FILE                     FmHelpAware.pas
  @COMMENTS                 Descends from TBaseForm and adds help-awareness and
                            F1 key functionality to descendant forms.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              0.1
      @DATE                 30/01/2005
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              0.2
      @DATE                 20/04/2005
      @COMMENTS             Added DisableHelp property used to inhibit help
                            being activated.
    )
    @REVISION(
      @VERSION              0.3
      @DATE                 30/11/2005
      @COMMENTS             Replaced calls to Application.Help*** methods with
                            calls to HelpMgr object.
    )
    @REVISION(
      @VERSION              1.0
      @DATE                 24/05/2006
      @COMMENTS             Improved and corrected comments.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 26/10/2006
      @COMMENTS             Fixed bug in one of overloaded DisplayHelp methods
                            that was not calling help when enabled.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 04/02/2007
      @COMMENTS             + Deleted unused THelpActivator enumeration and
                              revised method signatures accordingly.
    )
  )
}


{
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
 * The Original Code is FmHelpAware.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit FmHelpAware;


interface


uses
  // Delphi
  Classes,
  // Projct
  FmBase;


type

  {
  THelpAwareForm:
    Form class that provides help functionality and F1 key awareness to
    descendant forms.
  }
  THelpAwareForm = class(TBaseForm)
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fDisableHelp: Boolean;
      {Flag true if help is disabled}
  protected
    procedure DisplayHelp; overload; virtual;
      {Displays help according to various help related form properties. Uses
      keyword provided by form properties, or if they are not set, by the
      CustomHelpKeyword method.
      }
    procedure DisplayHelp(const AKeyword: string); overload; virtual;
      {Displays help using an A-link keyword.
        @param AKeyword [in] A-link keyword used to select help topic.
      }
    procedure DisplayHelp(const AHelpContext: THelpContext); overload; virtual;
      {Displays help using help context.
        @param AHelpContext [in] Help context number used to select help topic.
      }
    function CustomHelpKeyword: string; virtual;
      {Gets the help A-link keyword to be used if help context or keyword is not
      supplied via form properties.
        @return Name of form as A-link keyword.
      }
    property DisableHelp: Boolean
      read fDisableHelp write fDisableHelp default False;
      {Determines whether help is enabled or disabled. When true form is not
      "help-enabled"}
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UHelpMgr;


{$R *.dfm}


function THelpAwareForm.CustomHelpKeyword: string;
  {Gets the help A-link keyword to be used if help context or keyword is not
  supplied via form properties.
    @return Name of form as A-link keyword.
  }
begin
  Result := Self.Name;
end;

procedure THelpAwareForm.DisplayHelp;
  {Displays help according to various help related form properties. Uses keyword
  provided by form properties, or if they are not set, by the CustomHelpKeyword
  method.
  }
begin
  // Only call help if DisableHelp property is false
  if not fDisableHelp then
  begin
    if HelpContext = 0 then
    begin
      // No help context number specified: we'll use an A-link keyword
      if (HelpType = htKeyword) and (HelpKeyword <> '') then
        // key word property specified: use it
        DisplayHelp(HelpKeyword)
      else
        // no key word specified: use name of form
        DisplayHelp(CustomHelpKeyword)
    end
    else
      // Help context number specified: use it
      DisplayHelp(HelpContext);
  end;
end;

procedure THelpAwareForm.DisplayHelp(const AKeyword: string);
  {Displays help using an A-link keyword.
    @param AKeyword [in] A-link keyword used to select help topic.
  }
begin
  if not fDisableHelp then
    HelpMgr.ShowHelp(AKeyword);
end;

procedure THelpAwareForm.DisplayHelp(const AHelpContext: THelpContext);
  {Displays help using help context.
    @param AHelpContext [in] Help context number used to select help topic.
  }
begin
  if not fDisableHelp then
    HelpMgr.ShowHelp(AHelpContext);
end;

procedure THelpAwareForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Traps form key-down event. If FI key pressed with no modifiers we display
  help.
    @param Sender [in] Not used.
    @param Key [in/out] Key pressed by user (setting to 0 inhibits further
      processing).
    @param Shift [in] Modifier keys pressed.
  }
begin
  inherited;
  if (Key = VK_F1) and (Shift = []) then
  begin
    // F1 pressed with no modifier: display help noting called from keypress
    DisplayHelp;
    Key := 0;
  end;
end;

end.

