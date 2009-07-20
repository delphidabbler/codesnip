inherited FindXRefsDlg: TFindXRefsDlg
  Left = 426
  Top = 222
  Caption = 'Find Cross References'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 305
    Height = 159
    ExplicitWidth = 305
    ExplicitHeight = 159
    object lblDesc: TLabel
      Left = 0
      Top = 0
      Width = 119
      Height = 13
      Caption = 'Find cross references for '
    end
    object lblRoutineName: TLabel
      Left = 119
      Top = 0
      Width = 75
      Height = 13
      Caption = 'lblRoutineName'
    end
    object chkRequired: TCheckBox
      Left = 0
      Top = 28
      Width = 305
      Height = 17
      Caption = 'Search for &required snippets'
      TabOrder = 0
      OnClick = SearchCheckClick
    end
    object chkSeeAlso: TCheckBox
      Left = 0
      Top = 76
      Width = 305
      Height = 17
      Caption = 'Search for "&see also" cross references'
      TabOrder = 2
      OnClick = SearchCheckClick
    end
    object chkIncludeRoutine: TCheckBox
      Left = 0
      Top = 126
      Width = 305
      Height = 17
      Caption = '&Include "%s" in search'
      TabOrder = 4
    end
    object chkRequiredRecurse: TCheckBox
      Left = 24
      Top = 50
      Width = 281
      Height = 17
      Caption = 'Search re&cursively'
      TabOrder = 1
    end
    object chkSeeAlsoRecurse: TCheckBox
      Left = 24
      Top = 98
      Width = 281
      Height = 17
      Caption = 'Search rec&ursively'
      TabOrder = 3
    end
  end
  inherited btnCancel: TButton
    Left = 231
    ExplicitLeft = 231
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end
