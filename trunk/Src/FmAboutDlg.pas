{
 * FmAboutDlg.pas
 *
 * Implements about dialog box.
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
 * The Original Code is FmAboutDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmAboutDlg;


interface


uses
  // Delphi
  Forms, ComCtrls, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmHTMLViewDlg, FrBrowserBase, FrHTMLDlg, FrHTMLTpltDlg, UCSSBuilder,
  UHTMLEvents;


type

  {
  TAboutDlg:
    Implements an about dialog box that uses web browser controls to display
    information about the program and the database. HTML templates containing
    the dialog box content are loaded from resources. Also provides access to
    the program's easter egg.
  }
  TAboutDlg = class(THTMLViewDlg)
    btnRegister: TButton;
    bvlSeparator: TBevel;
    frmDatabase: THTMLTpltDlgFrame;
    frmProgram: THTMLTpltDlgFrame;
    pcDetail: TPageControl;
    tsDatabase: TTabSheet;
    tsProgram: TTabSheet;
    pnlTitle: TPanel;
    frmTitle: THTMLTpltDlgFrame;
    procedure btnRegisterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict private
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
      {Handles title frame's OnHTMLEvent event. Checks for easter-egg related
      mouse events on icon image and acts accordingly.
        @param Sender [in] Not used.
        @param EventInfo [in] Object providing information about the event.
      }
  strict protected
    procedure InitForm; override;
      {Initialises form's controls.
      }
    procedure InitHTMLFrame; override;
      {Initialises HTML frame to use required template document with
      placeholders replaced by required values.
      }
    function GetBodyPanelHeight: Integer; override;
      {Calculates required height of dialog's body panel from height of various
      HTML frames.
        @return Required height.
      }
    procedure ArrangeForm; override;
      {Adjusts position of registration button on bottom button line. Called
      from ancestor class.
      }
    procedure UpdateTitleCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
      {Updates CSS used for HTML displayed in title frame.
        @param Sender [in] Not used.
        @param CSSBuilder [in] Object used to update CSS.
      }
    procedure UpdateDetailCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
      {Updates CSS used for HTML displayed in detail (i.e. program and database)
      frames.
        @param Sender [in] Not used.
        @param CSSBuilder [in] Object used to update CSS.
      }
  public
    class procedure Execute(AOwner: TComponent);
      {Displays dialog box.
        @param AOwner [in] Component that owns this dialog box.
      }
  end;


implementation


uses   
  // Delphi
  SysUtils, Graphics, Math, Windows {for inlining},
  // Project
  FmEasterEgg, FmRegistrationDlg, UAppInfo, UColours, UConsts, UContributors,
  UCSSUtils, UFontHelper, UHTMLUtils, UThemesEx;


{
  NOTE:

  The about box uses three HTML templates. These are stored in RT_HTML resources
  as:
    + "dlg-about-head-tplt.html"
    + "dlg-about-program-tplt.html"
    + "dlg-about-database-tplt.html".

  The following placeholders are used in one or more of the templates. The
  placeholders are replaced by their values within this unit:

  <%Release%>         program release number
  <%Copyright%>       copyright info
  <%ResURL%>          url of programs HTML resources
  <%Registered%>      info about whether program is registered
}


{$R *.dfm}


{ TAboutDlg }

procedure TAboutDlg.ArrangeForm;
  {Adjusts position of registration button on bottom button line. Called from
  ancestor class.
  }
begin
  inherited;
  btnRegister.Left := pnlBody.Left;
  btnRegister.Top := btnHelp.Top;
end;

procedure TAboutDlg.btnRegisterClick(Sender: TObject);
  {Displays registration wizard when "Register CodeSnip" button is clicked.
    @param Sender [in] Not used.
  }
begin
  if TRegistrationDlg.Execute(Self) then
    btnRegister.Hide; // hide registration button now that program registered OK
end;

class procedure TAboutDlg.Execute(AOwner: TComponent);
  {Displays dialog box.
    @param AOwner [in] Component that owns this dialog box.
  }
begin
  with Create(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TAboutDlg.FormCreate(Sender: TObject);
  {Form initialisation event handler. Sets handler that updates frame's CSS.
    @param Sender [in] Not used.
  }
begin
  inherited;
  frmTitle.OnBuildCSS := UpdateTitleCSS;
  frmProgram.OnBuildCSS := UpdateDetailCSS;
  frmDatabase.OnBuildCSS := UpdateDetailCSS;
end;

function TAboutDlg.GetBodyPanelHeight: Integer;
  {Calculates required height of dialog's body panel from height of various HTML
  frames.
    @return Required height.
  }
begin
  // Set height of title frame and page control
  pnlTitle.Height := frmTitle.DocHeight;
  pcDetail.ClientHeight := pcDetail.Height - tsProgram.ClientHeight
    + Max(frmProgram.DocHeight, frmDatabase.DocHeight) + 8;
  // Calculate body panel height
  Result := pnlTitle.Height + bvlSeparator.Height + pcDetail.Height;
end;

procedure TAboutDlg.HTMLEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
  {Handles title frame's OnHTMLEvent event. Checks for easter-egg related mouse
  events on icon image and acts accordingly.
    @param Sender [in] Not used.
    @param EventInfo [in] Object providing information about the event.
  }
const
  cIconImgId = 'icon';  // id of icon image
begin
  // Check for onclick event on icon tag: display easter egg if ctrl key
  // pressed. Such an event is cancelled.
  if (EventInfo.DispatchId = cDocEventOnClick) and
    EventInfo.Args.ctrlKey and
    AnsiSameText(EventInfo.Args.srcElement.id, cIconImgId) then
  begin
    EventInfo.Cancelled := True;
    TEasterEggForm.Execute(Self);
  end;
  // Check for mouse move over icon tag: change cursor to hand if ctrl key
  // pressed to indicate clickable. Event permitted to bubble up.
  if (EventInfo.DispatchId = cDocEventOnMouseMove) and
    AnsiSameText(EventInfo.Args.srcElement.id, cIconImgId) then
  begin
    if EventInfo.Args.ctrlKey then
      EventInfo.Args.srcElement.style.cursor := 'hand'
    else
      EventInfo.Args.srcElement.style.cursor := 'auto';
  end;
end;

procedure TAboutDlg.InitForm;
  {Initialises form's controls.
  }
begin
  inherited;
  // Decide whether to display register button
  btnRegister.Visible := not TAppInfo.IsRegistered;
end;

procedure TAboutDlg.InitHTMLFrame;
  {Initialises HTML frame to use required template document with placeholders
  replaced by required values.
  }
resourcestring
  // Registration messages
  sRegisteredMessage    = 'Registered to %0:s.';
  sUnregisteredMessage  = 'Unregistered copy:';
  sRegistrationPrompt   = 'Please click the button below to register CodeSnip.';
  // Error string used when contributor file not available
  sNoContributors       = 'List not available, please update database.';

  // ---------------------------------------------------------------------------
  function BuildContribList(const ContribClass: TContributorsClass): string;
    {Builds HTML used to display list of contributors or creates an error
    message if contributor list is not available.
      @param ContribClass [in] Type of contributor class to use. This determines
        names that are displayed.
      @return Required HTML.
    }
  var
    Contributors: TContributors;  // contributors to database
    Contributor: string;          // name of a contributor
    DivAttrs: IHTMLAttributes;    // attributes of div tag
  begin
    Result := '';
    // Get list of contributors
    Contributors := ContribClass.Create;
    try
      if not Contributors.IsError then
      begin
        for Contributor in Contributors do
          Result := Result
            + MakeCompoundTag('div', MakeSafeHTMLText(Contributor))
            + EOL;
      end
      else
      begin
        // List couldn't be found: display warning message
        DivAttrs := THTMLAttributes.Create;
        DivAttrs.Add('class', 'warning');
        Result := MakeCompoundTag(
          'div', DivAttrs, MakeSafeHTMLText(sNoContributors)
        );
      end;
    finally
      FreeAndNil(Contributors);
    end;
  end;
  // ---------------------------------------------------------------------------

var
  Values: TStringList;        // map of HTML placeholders to actual values
  SpanAttrs: IHTMLAttributes; // attributes of span tag
begin
  // Build map of placeholders to actual values
  // the placeholders include all values for all three templates
  Values := TStringList.Create;
  try
    Values.Values['Release'] := TAppInfo.ProgramReleaseInfo;
    Values.Values['Copyright'] := TAppInfo.ProgramCopyright;
    Values.Values['ResURL'] := MakeResourceURL('');  // URL part before res name
    if TAppInfo.IsRegistered then
      Values.Values['Registered'] :=
        Format(sRegisteredMessage, [TAppInfo.RegisteredUser])
    else
    begin
      SpanAttrs := THTMLAttributes.Create;
      SpanAttrs.Add('class', 'warning');
      Values.Values['Registered'] :=
        MakeCompoundTag(
          'span', SpanAttrs, MakeSafeHTMLText(sUnregisteredMessage)
        ) +
        MakeSafeHTMLText(' ' + sRegistrationPrompt);
    end;
    Values.Values['ContribList'] := BuildContribList(TCodeContributors);
    Values.Values['TesterList'] := BuildContribList(TTesters);

    // Initialise the dialog content from HTML templates and replacement values
    frmTitle.Initialise('dlg-about-head-tplt.html', Values);
    pcDetail.ActivePage := tsDatabase;  // display page to let browser load OK
    frmDatabase.Initialise('dlg-about-database-tplt.html', Values);
    pcDetail.ActivePage := tsProgram;   // display page to let browser load OK
    frmProgram.Initialise('dlg-about-program-tplt.html', Values);

    // Handle HTML events on title frame
    frmTitle.OnHTMLEvent := HTMLEventHandler;
  finally
    FreeAndNil(Values);
  end;
end;

procedure TAboutDlg.UpdateDetailCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
  {Updates CSS used for HTML displayed in detail (i.e. program and database)
  frames.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to update CSS.
  }
var
  ContentFont: TFont; // font used for content
begin
  // Modify body's margin and, for themed windows, background colour
  with CSSBuilder.Selectors['body'] do
  begin
    ContentFont := TFont.Create;
    try
      TFontHelper.SetContentFont(ContentFont, True);
      AddProperty(CSSFontProps(ContentFont));
      if ThemeServicesEx.ThemesEnabled then
        AddProperty(CSSBackgroundColorProp(ThemeServicesEx.GetTabBodyColour));
      AddProperty(UCSSUtils.CSSMarginProp(0, 2, 6, 2));
    finally
      FreeAndNil(ContentFont);
    end;
  end;
  // Put border round scroll box
  with CSSBuilder.AddSelector('.scrollbox') do
    AddProperty(UCSSUtils.CSSBorderProp(cssAll, 1, cbsSolid, clBorder));
  // Set colours and font style of contributors and testers headings
  with CSSBuilder.AddSelector('.contrib-head, .tester-head') do
  begin
    AddProperty(CSSBackgroundColorProp(clBtnFace));
    AddProperty(CSSColorProp(clBtnText));
    AddProperty(CSSFontWeightProp(cfwBold));
  end;
end;

procedure TAboutDlg.UpdateTitleCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
  {Updates CSS used for HTML displayed in title frame.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to update CSS.
  }
begin
  // Set body colour, and put border round it
  with CSSBuilder.Selectors['body'] do
  begin
    AddProperty(CSSBackgroundColorProp(clWindow));
    AddProperty(CSSPaddingProp(4));
  end;
end;

end.

