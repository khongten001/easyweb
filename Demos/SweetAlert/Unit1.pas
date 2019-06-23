unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, SessionDataUnit, EWBase,
  EWDialogs, EWButtons, EWSweetAlert, EWLabel;
type
  TForm108 = class(TEWForm)
    EWButton1: TEWButton;
    EWSweetAlert1: TEWSweetAlert;
    EWButton3: TEWButton;
    EWButton2: TEWButton;
    EWButton4: TEWButton;
    EWLabel1: TEWLabel;
    procedure EWButton1Click(Sender: TObject);
    procedure EWButton3Click(Sender: TObject);
    procedure EWButton2Click(Sender: TObject);
    procedure EWButton4Click(Sender: TObject);
    procedure EWSweetAlert1ConfirmResult(Sender: TObject;
      AButton: TEWSweetAlertButton);
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

procedure TForm108.EWButton1Click(Sender: TObject);
begin
  EWSweetAlert1.ShowConfirmation('SweetAlert Integration', 'Hello from EasyWeb', [sabOk], saSuccess);
end;

procedure TForm108.EWButton2Click(Sender: TObject);
begin
  EWSweetAlert1.ShowConfirmation('Oooops...', 'You can''t do that!', [sabOk], saError);
end;

procedure TForm108.EWButton3Click(Sender: TObject);
begin
  EWSweetAlert1.ShowConfirmation('Is that a good idea?', 'This is a warning dialog', [sabOk], saWarning);
end;

procedure TForm108.EWButton4Click(Sender: TObject);
begin
  EWSweetAlert1.ShowConfirmation('A quick question', 'Do you like EasyWeb?', [sabYes, sabNo], saInfo);
end;

procedure TForm108.EWSweetAlert1ConfirmResult(Sender: TObject;
  AButton: TEWSweetAlertButton);
begin
  case AButton of
    sabOk: ;
    sabYes  : EWLabel1.Text := 'You clicked YES! Thanks!';
    sabNo   : EWLabel1.Text := 'You clicked NO! You''re hard to please ;-)';
    sabCancel: ;
  end;
end;

function TForm108.GetSessionData: TEWSessionData;
begin
  Result := (Session.DataModule as TEWSessionData);
end;

initialization 

TForm108.SetAsMainForm;

end. 