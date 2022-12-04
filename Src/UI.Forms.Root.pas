unit UI.Forms.Root;

interface

uses

  Winapi.Messages,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,

  IntfAligner,
  UControlStateMgr;

type
  TRootForm = class(TForm)

    ///  <summary>Handles form's OnCreate event to perform initialisations
    ///  required for every form.</summary>
    procedure FormCreate(Sender: TObject);

    ///  <summary>OnShow event handler for the form. Calls several virtual
    ///  protected methods which should be overridden by sub classes.</summary>
    ///  <remarks>Subclasses should not override this event handler but should
    ///  instead override the virtual methods it calls.</remarks>
    procedure FormShow(Sender: TObject);

    ///  <summary>Handles form's OnDestroy event. Frees owned objects.</summary>
    procedure FormDestroy(Sender: TObject);

    ///  <summary>Handles form's OnKeyDown event. Intercepts Alt+F10 key press
    ///  and displays any available context menu.</summary>
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  strict private

    var
      ///  <summary>Object used to enable / disable all form's controls when.
      ///  form's enabled state changes.</summary>
      fCtrlStateMgr: TControlStateMgr;
      ///  <summary>Value of DisableHelp property.</summary>
      fDisableHelp: Boolean;
    const
      ///  <summary>Custom message used to call AfterShow method after form
      ///  appears on screen.</summary>
      WM_AFTERSHOW = WM_USER + 1; // Custom message used to call AfterShow

    ///  <summary>Message handler that responds to custom message sent from
    ///  OnShow event handler that arrives after form has been displayed. Calls
    ///  virtual AfterShow method.</summary>
    procedure WMAfterShow(var Msg: TMessage); message WM_AFTERSHOW;

    ///  <summary>Aligns form on screen using an IAligner instance.</summary>
    ///  <remarks>Called from OnShow event after form is customised and arranged
    ///  but before it is initialised.</remarks>
    procedure AlignForm;

    ///  <summary>Activates any context menu associated with the active control
    ///  or any of its parent controls.</summary>
    procedure ActivateContextMenu;

  strict protected

    ///  <summary>Protected constructor. Does nothing but call the inherited
    ///  constructor.</summary>
    ///  <remarks>
    ///  <para>This constructor is provided for use in derived form classes that
    ///  implement the INoPublicConstruct interface where the public Create
    ///  constructor can't be called.</para>
    ///  <para>Such classes must instantiate the form from a class method that
    ///  must call InternalCreate instead of Create.</para>
    ///  </remarks>
    constructor InternalCreate(AOwner: TComponent); virtual;

    ///  <summary>Overrides window creation parameters to set window class name
    ///  to that provided by virtual WindowClassName method.</summary>
    procedure CreateParams(var Params: TCreateParams); override;

    ///  <summary>Returns a window class name comprised of company, program and
    ///  form class names.</summary>
    function WindowClassName: string; virtual;

    ///  <summary>Customises form's controls as necessary.</summary>
    ///  <remarks>
    ///  <para>This method is called from the form's OnShow event, immediately
    ///  before ArrangeControls.</para>
    ///  <para>This implementation does nothing. Subclasses should override to
    ///  create any custom controls and perform any other customsation that may
    ///  change the size of a control.</para>
    ///  </remarks>
    procedure CustomiseControls; virtual;

    ///  <summary>Arranges controls on form as necessary.</summary>
    ///  <remarks>
    ///  <para>This method is called from the form's OnShow event after controls
    ///  have been customised and and before form is aligned.</para>
    ///  <para>This implementation does nothing. Subclasses should override to
    ///  arrange all the forms controls, instead of handling the OnShow event.
    ///  </para>
    ///  </remarks>
    procedure ArrangeControls; virtual;

    ///  <summary>Initialises content of form's controls.</summary>
    ///  <remarks>
    ///  <para>This method is called during the form's OnShow event after the
    ///  form is aligned.</para>
    ///  <para>This implementation does nothing. Subclasses should override to
    ///  initialise the controls to their default values, instead of handling
    ///  the OnShow event.</para>
    ///  <para>The form size should not be changed in this method since it will
    ///  interfere with the aligment.</para>
    ///  </remarks>
    procedure InitialiseControls; virtual;

    ///  <summary>Performs any actions needed after the form is visible on
    ///  screen.</summary>
    ///  <remarks>This implementation does nothing. Subclasses that need this
    ///  functionality should override this method.</remarks>
    procedure AfterShowForm; virtual;

    ///  <summary>Returns new instance of aligner object used to align form to
    ///  owner.</summary>
    ///  <remarks>This implementation returns a null aligner. Subclasses that
    ///  require alignment should return a suitable IAligner instance.</remarks>
    function Aligner: IFormAligner; virtual;

    ///  <summary>Displays help specified by a keyword. Form's HelpKeyword is
    ///  used if set otherwise the keyword returned by CustomHelpKeyword method
    ///  is used. Help context numbers are ignored. Help is not called if
    ///  DisableHelp is true or if no keyword can be found.</summary>
    procedure DisplayHelp; overload; virtual;

    ///  <summary>Displays help using a keyword. Does nothing if DisableHelp
    ///  property is true.</summary>
    ///  <param name="AKeyword">Keyword to be used. [in]</param>
    procedure DisplayHelp(const AKeyword: string); overload; virtual;

    ///  <summary>Gets a help keyword to be used if form's HelpKeyword property
    ///  is not set. Default is to use name of form. Subclasses can override
    ///  this behaviour.</summary>
    ///  <returns>string. Name of form.</returns>
    function CustomHelpKeyword: string; virtual;

    ///  <summary>Determines whether help is enabled or disabled.</summary>
    property DisableHelp: Boolean
      read fDisableHelp write fDisableHelp default False;

  public

    ///  <summary>Public constructor. Does nothing but call the inherited
    ///  constructor.</summary>
    ///  <remarks>
    ///  <para>This constructor can be called directly or from class methods in
    ///  a descendant class, providing that class does not support the
    ///  INoPublicConstruct interface.</para>
    ///  <para>In cases where INoPublicConstruct is supported the protected
    ///  InternalCreate constructor must be called instead.</para>
    ///  </remarks>
    constructor Create(AOwner: TComponent); override;

  end;

implementation

uses
  System.SysUtils,
  System.Types,
  Winapi.Windows,

  UAppInfo,
  UBaseObjects,
  UClassHelpers,
  UHelpMgr,
  UKeysHelper,
  UMenus,
  UNulFormAligner,    // TODO: rename this unit & class to spell Null correctly!
  UStrUtils;

{$R *.dfm}

{ TForm1 }

procedure TRootForm.ActivateContextMenu;
var
  MenuIntf: IPopupMenu; // interface reference to controls supporting IPopupMenu
begin
  // search active control parents to try to find if pop-up menu supported
  var Ctrl: TControl := ActiveControl;
  if not Assigned(Ctrl) then
    Ctrl := Self;
  while Assigned(Ctrl)
    and (Ctrl <> Self)
    and not Ctrl.HasPopupMenu
    and not (Supports(Ctrl, IPopupMenu, MenuIntf) and MenuIntf.HasPopup) do
    Ctrl := Ctrl.Parent;
  if not Assigned(Ctrl) then
    Exit;
  // we use an arbitrary pop-up position: may be able to improve on this
  var PopupPos := Ctrl.ClientToScreen(
    Point(40, 40)
  );
  // show pop-up menu, either via PopupMenu property or via IPopupMenu interface
  if Ctrl.HasPopupMenu then
    Ctrl.GetPopupMenu.Popup(PopupPos.X, PopupPos.Y)
  else if Supports(Ctrl, IPopupMenu, MenuIntf) and MenuIntf.HasPopup then
    MenuIntf.Popup(PopupPos);
end;

procedure TRootForm.AfterShowForm;
begin
  // Do nothing: descendants override
end;

function TRootForm.Aligner: IFormAligner;
begin
  Result := TNulAligner.Create;
end;

procedure TRootForm.AlignForm;
begin
  Aligner.AlignForm(Self);
end;

procedure TRootForm.ArrangeControls;
begin
  // Do nothing: descendants override
end;

constructor TRootForm.Create(AOwner: TComponent);
begin
  Assert(not Supports(Self, INoPublicConstruct),
    ClassName + '.Create: Form''s public constructor can''t be called');
  inherited;
end;

procedure TRootForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  var ClassName := WindowClassName;
  if ClassName <> '' then
    StrLCopy(
      Params.WinClassName,
      PChar(ClassName),
      SizeOf(Params.WinClassName) div SizeOf(Char) - 1
    );
end;

function TRootForm.CustomHelpKeyword: string;
begin
  Result := Name;
end;

procedure TRootForm.CustomiseControls;
begin
  // Do nothing: descendants override
end;

procedure TRootForm.DisplayHelp;
begin
  if DisableHelp then
    Exit;
  var AKeyword := StrIf(HelpKeyword = '', CustomHelpKeyword, HelpKeyword);
  if AKeyword <> '' then
    DisplayHelp(AKeyword);
end;

procedure TRootForm.DisplayHelp(const AKeyword: string);
begin
  if not fDisableHelp then
    HelpMgr.ShowHelp(AKeyword);
end;

procedure TRootForm.FormCreate(Sender: TObject);
begin
  inherited;
  fCtrlStateMgr := TControlStateMgr.Create(Self);
end;

procedure TRootForm.FormDestroy(Sender: TObject);
begin
  fCtrlStateMgr.Free;
end;

procedure TRootForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F10) and (ExtractShiftKeys(Shift) = [ssAlt]) then
    ActivateContextMenu
  else if (Key = VK_F1) and (Shift = []) then
  begin
    DisplayHelp;
    Key := 0;
  end;
end;

procedure TRootForm.FormShow(Sender: TObject);
begin
  CustomiseControls;
  ArrangeControls;
  AlignForm;
  InitialiseControls;
  // Post message that causes AfterShowForm to be called after form has appeared
  // on screen
  PostMessage(Handle, WM_AFTERSHOW, 0, 0);
end;

procedure TRootForm.InitialiseControls;
begin
  // Do nothing: descendants override
end;

constructor TRootForm.InternalCreate(AOwner: TComponent);
begin
  Assert(Supports(Self, INoPublicConstruct), ClassName + '.InternalCreate: '
    + 'Form''s protected constructor can''t be called');
  inherited Create(AOwner);
end;

function TRootForm.WindowClassName: string;
var
  PostfixName: string;  // Postfix to name, based on form's class name
begin
  // Calculate window class name postfix. This is form class name, stripped of
  // any preceeding 'T'
  if StrStartsStr('T', ClassName) then
    PostfixName := StrSliceRight(ClassName, Length(ClassName) - 1)
  else
    PostfixName := ClassName;
  // Build window class name
  Result := TAppInfo.CompanyName
    + '.' + TAppInfo.ProgramName
    + '.' + TAppInfo.CodeName
    + '.' + PostfixName;
end;

procedure TRootForm.WMAfterShow(var Msg: TMessage);
begin
  AfterShowForm;
end;

end.

