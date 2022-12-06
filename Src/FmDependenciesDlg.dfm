inherited DependenciesDlg: TDependenciesDlg
  Caption = 'DependenciesDlg'
  ClientHeight = 1844
  ClientWidth = 2561
  Font.Height = -200
  ExplicitWidth = 2585
  ExplicitHeight = 1908
  PixelsPerInch = 168
  TextHeight = 265
  inherited bvlBottom: TBevel
    Left = 77
    Top = 2777
    Width = 3537
    Height = 23
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 2777
    ExplicitWidth = 3537
    ExplicitHeight = 23
  end
  inherited pnlBody: TPanel
    Left = 77
    Top = 77
    Width = 3537
    Height = 2637
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 77
    ExplicitWidth = 3537
    ExplicitHeight = 2637
    object pcBody: TPageControl
      Left = 0
      Top = 0
      Width = 3537
      Height = 2637
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = tsDependsUpon
      Align = alClient
      TabOrder = 0
      OnMouseDown = pcBodyMouseDown
      object tsDependsUpon: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Depends Upon'
        object lblCircularRef: TLabel
          Left = 0
          Top = 420
          Width = 2450
          Height = 265
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Caption = 'Circular Reference Detected'
          Visible = False
        end
        object lblNoDependencies: TLabel
          Left = 0
          Top = 0
          Width = 55
          Height = 265
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
        end
        object tvDependencies: TTreeView
          Left = 0
          Top = 0
          Width = 3529
          Height = 415
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          Indent = 33
          ReadOnly = True
          ShowButtons = False
          ShowLines = False
          ShowRoot = False
          TabOrder = 0
          OnCollapsing = tvDependenciesCollapsing
        end
      end
      object tsRequiredBy: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Required By'
        ImageIndex = 1
        object lblNoDependents: TLabel
          Left = 14
          Top = 14
          Width = 55
          Height = 265
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
        end
        object lbDependents: TListBox
          Left = 0
          Top = 0
          Width = 646
          Height = 443
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Style = lbOwnerDrawFixed
          Align = alClient
          ItemHeight = 28
          TabOrder = 0
          OnDrawItem = lbDependentsDrawItem
        end
      end
    end
  end
  inherited btnHelp: TButton
    Left = 2928
    Top = 2851
    Width = 702
    Height = 236
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    TabOrder = 3
    ExplicitLeft = 2928
    ExplicitTop = 2851
    ExplicitWidth = 702
    ExplicitHeight = 236
  end
  inherited btnClose: TButton
    Left = 711
    Top = 931
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    TabOrder = 2
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
  end
  object btnSelectAndClose: TButton
    Left = 196
    Top = 532
    Width = 175
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Action = actSelectAndClose
    ModalResult = 1
    TabOrder = 1
  end
  object alSelectAndClose: TActionList
    Left = 224
    Top = 176
    object actSelectAndClose: TAction
      Caption = '&Select && Close'
      OnExecute = actSelectAndCloseExecute
      OnUpdate = actSelectAndCloseUpdate
    end
  end
end
