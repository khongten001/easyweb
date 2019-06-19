object Form1: TForm1
  Left = 0
  Top = 0
  Width = 821
  Height = 603
  Color = clWhite
  PixelsPerInch = 96
  TextHeight = 13
  object EWTable1: TEWTable
    Left = 32
    Top = 176
    Width = 468
    Height = 281
    Columns = <
      item
        Text = '#'
      end
      item
        Text = 'First'
      end
      item
        Text = 'Last'
      end
      item
        Text = 'Handle'
      end>
    Hover = True
    OnClickCell = EWTable1ClickCell
  end
  object EWButton1: TEWButton
    Left = 32
    Top = 64
    Width = 121
    Height = 41
    OnClick = EWButton1Click
    Text = 'Clear Table'
  end
  object EWLabel1: TEWLabel
    Left = 164
    Top = 111
    Width = 181
    Height = 41
    Font.Variant = fvNormal
    Font.Size = 0
    Font.Style = []
    Font.Color = clBlack
    Text = ''
  end
  object EWButtonGroup2: TEWButtonGroup
    Left = 164
    Top = 64
    Width = 181
    Height = 41
    Text = 'EWButtonGroup2'
    Items.Strings = (
      'Light'
      'Dark')
    ItemIndex = 0
    OnItemClick = EWButtonGroup2ItemClick
  end
  object EWCheckBox1: TEWCheckBox
    Left = 392
    Top = 64
    Width = 108
    Height = 41
    OnClick = EWCheckBox1Click
    Checked = False
    Text = 'Hover Effect'
  end
  object EWCheckBox2: TEWCheckBox
    Left = 392
    Top = 111
    Width = 108
    Height = 41
    OnClick = EWCheckBox2Click
    Checked = False
    Text = 'Striped'
  end
  object EWButton2: TEWButton
    Left = 32
    Top = 111
    Width = 121
    Height = 41
    OnClick = EWButton2Click
    Text = 'Add Row'
  end
  object EWNavBar1: TEWNavBar
    Left = 0
    Top = 0
    Width = 805
    Height = 41
    Items = <>
    Title = 'EasyWeb'
    SearchOptions.ButtonText = 'Search'
    SearchOptions.Placeholder = 'Search'
  end
end
