inherited DependenciesDlg: TDependenciesDlg
  Caption = 'DependenciesDlg'
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object pcBody: TPageControl
      Left = 0
      Top = 0
      Width = 957
      Height = 631
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
          Width = 258
          Height = 30
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
          Width = 6
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
        end
        object tvDependencies: TTreeView
          Left = 0
          Top = 0
          Width = 949
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
          Width = 6
          Height = 30
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
        end
        object lbDependents: TListBox
          Left = 0
          Top = 0
          Width = 949
          Height = 586
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
    TabOrder = 3
  end
  inherited btnClose: TButton
    TabOrder = 2
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
