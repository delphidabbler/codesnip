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
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
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
  Forms, ComCtrls, StdCtrls, Controls, ExtCtrls, Classes, Messages,
  // Project
  FmHTMLViewDlg, FrBrowserBase, FrHTMLDlg, FrHTMLTpltDlg, UCSSBuilder,
  UHTMLEvents;


type

  {
  TPathInfoBox:
    Component that displays a path in a group box with an associated button that
    displays the path in Windows Explorer.
  }
  TPathInfoBox = class(TCustomGroupBox)
  strict private
    fPathLbl: TLabel;   // Label that displays path
    fViewBtn: TButton;  // Button that displays path in explorer
    function GetPath: string;
      {Read accessor for Path property. Gets value from label.
        @return Property value.
      }
    procedure SetPath(const Value: string);
      {Write accessor for Path property. Stores value in label.
        @param Value [in] New property value.
      }
    procedure BtnClick(Sender: TObject);
      {Button click event handler. Displays folder stored in Path property in
      Windows Explorer.
        @param Sender [in] Not used.
      }
    procedure FontChange(var Msg: TMessage); message CM_FONTCHANGED;
      {Handles font changes by resizing control to allow for new font size.
        @param Msg [in/out] Not used.
      }
    procedure ReArrange;
      {Resizes and re-arranges control and its sub-components.
      }
  strict protected
    procedure Resize; override;
      {Handles control resizing. Re-arranges control's sub-components.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Component constructor. Creates sub-components and arranges them.
        @param AOwner [in] Owning component.
      }
    property Path: string read GetPath write SetPath;
      {Path displayed in group box and displayed by view button}
  end;

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
    tsPaths: TTabSheet;
    procedure btnRegisterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    fMainDBPathGp: TPathInfoBox;  // control that displays main database folder
    fUserDBPathGp: TPathInfoBox;  // control that displays user database folder
    fInstallPathGp: TPathInfoBox; // control that displays program install path
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo);
      {Handles title frame's OnHTMLEvent event. Checks for easter-egg related
      mouse events on icon image and acts accordingly.
        @param Sender [in] Not used.
        @param EventInfo [in] Object providing information about the event.
      }
  strict protected
    procedure ConfigForm; override;
      {Configures form by creating custom controls.
      }
    procedure InitForm; override;
      {Initialises form's controls.
      }
    procedure InitHTMLFrame; override;
      {Initialises HTML frame to use required template document with
      placeholders replaced by required values.
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
  SysUtils, Graphics, Math, Windows {for inlining}, ShellAPI,
  // Project
  FmEasterEgg, FmRegistrationDlg, UAppInfo, UColours, UConsts, UContributors,
  UCSSUtils, UCtrlArranger, UFontHelper, UHTMLUtils, UResourceUtils, UThemesEx,
  UUtils;


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

function ExploreFolder(const Folder: string): Boolean;
  {Displays Windows Explorer showing a specified folder.
    @param Folder [in] Folder to explore.
    @return True if explorer displayed, False if not.
  }
begin
  if IsDirectory(Folder) then
    Result := ShellExecute(
      0, 'explore', PChar(Folder), nil, nil, SW_SHOWNORMAL
    ) > 32
  else
    Result := False;
end;

{ TAboutDlg }

procedure TAboutDlg.ArrangeForm;
  {Adjusts position of registration button on bottom button line. Called from
  ancestor class.
  }
var
  PathTabHeight: Integer;
begin
  fMainDBPathGp.Top := TCtrlArranger.BottomOf(fInstallPathGp, 8);
  fUserDBPathGp.Top := TCtrlArranger.BottomOf(fMainDBPathGp, 8);
  PathTabHeight := TCtrlArranger.BottomOf(fUserDBPathGp);
  // Set height of title frame and page control
  pnlTitle.Height := frmTitle.DocHeight;
  pcDetail.ClientHeight :=
    pcDetail.Height - tsProgram.ClientHeight +
    Max(
      PathTabHeight,
      Max(frmProgram.DocHeight, frmDatabase.DocHeight)
    ) + 8;
  pnlBody.ClientHeight := pnlTitle.Height + bvlSeparator.Height +
    pcDetail.Height;
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

procedure TAboutDlg.ConfigForm;
  {Configures form by creating custom controls.
  }

  function CreatePathInfoBox(const Caption, Path: string): TPathInfoBox;
    {Creates and initialises a custom path information control.
      @param Caption [in] Group box caption.
      @param Path [in] Path to be displayed.
      @return New control.
    }
  begin
    Result := TPathInfoBox.CreateParented(tsPaths.Handle);
    Result.Parent := tsPaths;
    Result.SetBounds(8, 8, tsPaths.ClientWidth - 16, 0);
    Result.Caption := Caption;
    Result.Path := Path;
  end;

resourcestring
  // Captions for custom controls
  sInstallPathGpCaption = 'Install Directory';
  sMainDBPathGpCaption = 'Main Database Directory';
  sUserDBPathGpCaption = 'User Database Directory';
begin
  inherited;
  // Creates required custom controls
  fInstallPathGp := CreatePathInfoBox(
    sInstallPathGpCaption, TAppInfo.AppExeDir
  );
  fMainDBPathGp := CreatePathInfoBox(
    sMainDBPathGpCaption, TAppInfo.AppDataDir
  );
  fUserDBPathGp := CreatePathInfoBox(
    sUserDBPathGpCaption, TAppInfo.UserDataDir
  );
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

procedure TAboutDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees non-owned controls.
    @param Sender [in] Not used.
  }
begin
  inherited;
  fInstallPathGp.Free;
  fMainDBPathGp.Free;
  fUserDBPathGp.Free;
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

procedure TAboutDlg.pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Handles event triggered when user clicks on one of page control tabs. Ensures
  page control has focus. This does always happen automatically.
    @param Sender [in] Not used.
    @param Button [in] Not used.
    @param Shift [in] Not used.
    @param X [in] X co-ordinate of mouse in client co-ordinates.
    @param Y [in] Y co-ordinate of mouse in client co-ordinates.
  }
begin
  if htOnItem in pcDetail.GetHitTestInfoAt(X, Y) then
    pcDetail.SetFocus;
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

{ TPathInfoBox }

procedure TPathInfoBox.BtnClick(Sender: TObject);
  {Button click event handler. Displays folder stored in Path property in
  Windows Explorer.
    @param Sender [in] Not used.
  }
begin
  if Assigned(fPathLbl) and (fPathLbl.Caption <> '') then
    ExploreFolder(fPathLbl.Caption);
end;

constructor TPathInfoBox.Create(AOwner: TComponent);
  {Component constructor. Creates sub-components and arranges them.
    @param AOwner [in] Owning component.
  }
resourcestring
  // Hint attached to view button
  sViewBtnHint = 'Explore...|Display the path in Windows Explorer';
begin
  inherited;
  // Create and setup path label
  fPathLbl := TLabel.Create(Self);
  fPathLbl.Parent := Self;
  fPathLbl.Left := 8;
  fPathLbl.Top := 8;
  fPathLbl.Width := 120;
  fPathLbl.AutoSize := False;
  fPathLbl.EllipsisPosition := epPathEllipsis;
  fPathLbl.Width := Self.Width - 16;
  fPathLbl.Caption := ' ';
  fPathLbl.Transparent := True;
  // Create and setup view button
  fViewBtn := TButton.Create(Self);
  fViewBtn.Parent := Self;
  fViewBtn.OnClick := BtnClick;
  fViewBtn.Height := 19;
  fViewBtn.Width := 26;
  fViewBtn.Caption := '...';
  fViewBtn.Hint := sViewBtnHint;
  fViewBtn.ShowHint := True;
  // Ensure correct default font is used
  TFontHelper.SetDefaultBaseFont(Font, True);
  // Size and arrange controls
  ReArrange;
end;

procedure TPathInfoBox.FontChange(var Msg: TMessage);
  {Handles font changes by resizing control to allow for new font size.
    @param Msg [in/out] Not used.
  }
begin
  inherited;
  ReArrange;
end;

function TPathInfoBox.GetPath: string;
  {Read accessor for Path property. Gets value from label.
    @return Property value.
  }
begin
  Result := fPathLbl.Caption;
end;

procedure TPathInfoBox.ReArrange;
  {Resizes and re-arranges control and its sub-components.
  }
begin
  TCtrlArranger.SetLabelHeight(fPathLbl);
  Height := Max(fPathLbl.Height, fViewBtn.Height) + 24;
  TCtrlArranger.AlignVCentres(
    (ClientHeight - Max(fPathLbl.Height, fViewBtn.Height)) div 3 * 2,
    [fPathLbl, fViewBtn]
  );
  fViewBtn.Left := ClientWidth - fViewBtn.Width - 8;
  fPathLbl.Left := 8;
  fPathLbl.Width := fViewBtn.Left - fPathLbl.Left - 8;
end;

procedure TPathInfoBox.Resize;
  {Handles control resizing. Re-arranges control's sub-components.
  }
begin
  inherited;
  ReArrange;
end;

procedure TPathInfoBox.SetPath(const Value: string);
  {Write accessor for Path property. Stores value in label.
    @param Value [in] New property value.
  }
begin
  fPathLbl.Caption := Value;
end;

end.

