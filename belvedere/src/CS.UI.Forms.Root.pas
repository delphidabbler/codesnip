unit CS.UI.Forms.Root;

interface

uses
//  Winapi.Windows,
//  Winapi.Messages,
//  System.SysUtils,
//  System.Variants,
  System.Classes,
//  Vcl.Graphics,
//  Vcl.Controls,
  Vcl.Forms;

type
  TRootForm = class(TForm)
    ///  <summary>OnShow event handler for the form. Calls several virtual
    ///  protected methods which should be overridden by sub classes.</summary>
    ///  <remarks>Subclasses should not override this event handler but should
    ///  instead override the virtual methods it calls.</remarks>
    procedure FormShow(Sender: TObject);
  strict private
    ///  <summary>Aligns form on screen using an IAligner instance.</summary>
    ///  <remarks>Called from OnShow event after form is customised and arranged
    ///  but before it is initialised.</remarks>
    procedure AlignForm;
  strict protected
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

    { TODO: function Aligner: IFormAligner; virtual;
        virtual function that returns instance of required form aligner
    }
    { TODO: function HelpManager: IHelpManager;
        function that returns instance of system wide help manager
    }
    { TODO: property EnableHelp: Boolean default True;
        property that sub-classes can set False to disable help
    }
    { TODO: various methods from old FmHelpAware form to support help keywords
        etc.
    }
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation


{$R *.dfm}

procedure TRootForm.AlignForm;
begin
  // TODO: Get instance of required form aligner and call its Align method
end;

procedure TRootForm.ArrangeControls;
begin
  // Do nothing
end;

constructor TRootForm.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: Set default aligner
  // TODO: Set default help manager
end;

procedure TRootForm.CustomiseControls;
begin
  // Do nothing
end;

procedure TRootForm.FormShow(Sender: TObject);
begin
  CustomiseControls;
  ArrangeControls;
  AlignForm;
  InitialiseControls;
end;

procedure TRootForm.InitialiseControls;
begin
  // Do nothing
end;

end.
