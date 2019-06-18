unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, EWBase, EWButtons, EWDialogs,
  EWLabel;
type
  TForm62 = class(TEwForm)
    EWButton1: TEWButton;
    EWLabel1: TEWLabel;
    EWButton2: TEWButton;
    EWButton3: TEWButton;
    EWLabel2: TEWLabel;
    EWDialog1: TEWDialog;
    procedure EWButton1Click(Sender: TObject);
    procedure EWDialog1ConfirmResult(Sender: TObject; AConfirmed: Boolean);
    procedure EWButton2Click(Sender: TObject);
    procedure EWButton3Click(Sender: TObject);
    procedure EWDialog1Prompt(Sender: TObject; AValue: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TForm62.EWButton1Click(Sender: TObject);
begin
  EWDialog1.ShowConfirmation('Click on OK or Cancel...');
end;

procedure TForm62.EWButton2Click(Sender: TObject);
begin
  ShowMessage('Hello from EasyWeb!');
end;

procedure TForm62.EWButton3Click(Sender: TObject);
begin
  EWLabel2.Text := '';
  EWDialog1.ShowPrompt('Please enter your name', '');
end;

procedure TForm62.EWDialog1ConfirmResult(Sender: TObject; AConfirmed: Boolean);
begin
  if AConfirmed then
  begin
    EWLabel1.Text := 'Ok Clicked';
    EWLabel1.Font.Color := clWebGreen;
  end
  else
  begin
    EWLabel1.Text := 'Cancel Clicked.';
    EWLabel1.Font.Color := clRed;
  end;
end;

procedure TForm62.EWDialog1Prompt(Sender: TObject; AValue: string);
begin
  EWLabel2.Text := 'Hello '+AValue+'!';
end;

initialization

TForm62.SetAsMainForm;

end.