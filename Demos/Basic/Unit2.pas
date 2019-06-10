unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, EWForm, EWLabel, EWBase, EWButtons;
type
  TForm2 = class(TEwForm)
    EWButton1: TEWButton;
    EWLabel1: TEWLabel;
    procedure EWButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



implementation


{%CLASSGROUP 'Vcl.Controls.TControl'}


{$R *.dfm}


procedure TForm2.EWButton1Click(Sender: TObject);
begin
  PopForm;
end;

end.