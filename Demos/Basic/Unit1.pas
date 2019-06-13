unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, EWEdits, EWBase, EWButtons,
  EWLabel, EWProgressBars, Vcl.Imaging.jpeg, EWImages, EWCheckbox;
type
  TForm1 = class(TEwForm)
    EWLabel2: TEWLabel;
    EWDropDown1: TEWDropDown;
    EWButton1: TEWButton;
    EWProgressBar1: TEWProgressBar;
    EWButton2: TEWButton;
    EWLabel1: TEWLabel;
    EWImage1: TEWImage;
    EWLabel3: TEWLabel;
    EWLabel4: TEWLabel;
    EWLabel5: TEWLabel;
    EWDropDown2: TEWDropDown;
    EWCheckBox1: TEWCheckBox;
    EWLabel6: TEWLabel;
    EWButton3: TEWButton;
    EWEdit1: TEWEdit;
    procedure EWImage1MouseEnter(Sender: TObject);
    procedure EWImage1MouseLeave(Sender: TObject);
    procedure EWImage1Click(Sender: TObject);
    procedure EWImage1DblClick(Sender: TObject);
    procedure EWImage1RightClick(Sender: TObject);
    procedure EWButton2Click(Sender: TObject);
    procedure EWButton1Click(Sender: TObject);
    procedure EWCheckBox1Click(Sender: TObject);
    procedure EWDropDown2ItemClick(Sender: TObject; AItem: string;
      AIndex: Integer);
    procedure EWButton3Click(Sender: TObject);
    procedure EWDropDown1ItemClick(Sender: TObject; AItem: TCollectionItem;
      ADropDownIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



implementation

uses EWTypes, Unit2;

{%CLASSGROUP 'Vcl.Controls.TControl'}


{$R *.dfm}


procedure TForm1.EWButton1Click(Sender: TObject);
begin
  EWProgressBar1.Position := EWProgressBar1.Position +10;

end;

procedure TForm1.EWButton2Click(Sender: TObject);
begin
  EWProgressBar1.Position := EWProgressBar1.Position -10;
end;

procedure TForm1.EWButton3Click(Sender: TObject);
begin
  PushForm(TForm2);
end;

procedure TForm1.EWCheckBox1Click(Sender: TObject);
begin
  EWProgressBar1.Animated := EWCheckBox1.Checked;
end;

procedure TForm1.EWDropDown1ItemClick(Sender: TObject; AItem: TCollectionItem;
  ADropDownIndex: Integer);
begin
  EWLabel3.Text := 'You clicked '+TEWDropDownItem(AItem).Text;
end;

procedure TForm1.EWDropDown2ItemClick(Sender: TObject; AItem: string;
  AIndex: Integer);
begin
  case AIndex of
    0: EWProgressBar1.Style := TEWButtonType.btDanger;
    1: EWProgressBar1.Style := TEWButtonType.btSuccess;
    2: EWProgressBar1.Style := TEWButtonType.btWarning;
    3: EWProgressBar1.Style := TEWButtonType.btPrimary;
  end;
end;

procedure TForm1.EWImage1Click(Sender: TObject);
begin
  EWLabel5.Text := 'Clicked';

end;

procedure TForm1.EWImage1DblClick(Sender: TObject);
begin
    EWLabel5.Text := 'Double clicked';
end;

procedure TForm1.EWImage1MouseEnter(Sender: TObject);
begin
  EWLabel5.Text := 'Mouse Enter';
end;

procedure TForm1.EWImage1MouseLeave(Sender: TObject);
begin
    EWLabel5.Text := 'Mouse Leave';

end;

procedure TForm1.EWImage1RightClick(Sender: TObject);
begin
  EWLabel5.Text := 'Right click';

end;

initialization

TForm1.SetAsMainForm;

end.