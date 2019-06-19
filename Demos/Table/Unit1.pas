unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, EWBase, EWEdits, EWTable,
  EWButtons, EWLabel, EWCheckbox, EWNavBar, SessionDataUnit;
type
  TForm1 = class(TEwForm)
    EWTable1: TEWTable;
    EWButton1: TEWButton;
    EWLabel1: TEWLabel;
    EWButtonGroup2: TEWButtonGroup;
    EWCheckBox1: TEWCheckBox;
    EWCheckBox2: TEWCheckBox;
    EWButton2: TEWButton;
    EWNavBar1: TEWNavBar;
    procedure EWButton1Click(Sender: TObject);
    procedure EWTable1ClickCell(Sender: TObject; ACol, ARow: Integer);
    procedure EWButtonGroup2ItemClick(Sender: TObject; AItem: string;
      AIndex: Integer);
    procedure EWCheckBox1Click(Sender: TObject);
    procedure EWCheckBox2Click(Sender: TObject);
    procedure EWButton2Click(Sender: TObject);
  private
    function GetSessionData: TEWSessionData;
    { Private declarations }
  public
    property SessionData: TEWSessionData read GetSessionData;
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function TForm1.GetSessionData: TEWSessionData;
begin
  Result := (Session.DataModule as TEWSessionData);
end;

procedure TForm1.EWButton1Click(Sender: TObject);
begin
  EWTable1.Clear;
end;

procedure TForm1.EWButton2Click(Sender: TObject);
begin
  EWTable1.AddRow(['Col 1', 'Col 2', 'Col 3','Col 4']);
end;

procedure TForm1.EWButtonGroup2ItemClick(Sender: TObject; AItem: string;
  AIndex: Integer);
begin
  case AIndex of
    0: EWTable1.Theme := ttLight;
    1: EWTable1.Theme := ttDark;
  end;
end;

procedure TForm1.EWCheckBox1Click(Sender: TObject);
begin
  EWTable1.Hover := EWCheckBox1.Checked;
end;

procedure TForm1.EWCheckBox2Click(Sender: TObject);
begin
  EWTable1.Striped := EWCheckBox2.Checked;
end;

procedure TForm1.EWTable1ClickCell(Sender: TObject; ACol, ARow: Integer);
begin
  EWLabel1.Text := 'You clicked cell: '+ACol.ToString+', '+ARow.ToString;
end;

initialization

TForm1.SetAsMainForm;

end.