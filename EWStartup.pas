unit EWStartup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, VCL.Forms, Vcl.StdCtrls, EWConst, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ComCtrls, System.Actions, Vcl.ActnList, Vcl.Menus;

type
  TfrmStartup = class(TForm)
    memoLog: TMemo;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    MainMenu1: TMainMenu;
    Project1: TMenuItem;
    ActionList1: TActionList;
    actRunInBrowser: TAction;
    RunInBrowser1: TMenuItem;
    Timer1: TTimer;
    SpeedButton2: TSpeedButton;
    Bevel1: TBevel;
    actClearSessions: TAction;
    ClearSessions1: TMenuItem;
    N1: TMenuItem;
    Bevel2: TBevel;
    SpeedButton3: TSpeedButton;
    actClearLog: TAction;
    ClearLog1: TMenuItem;
    N2: TMenuItem;
    actStopServer: TAction;
    StopServer1: TMenuItem;
    N3: TMenuItem;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure actRunInBrowserExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure actClearSessionsExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actStopServerExecute(Sender: TObject);
  private
    procedure Initialize;
    procedure RunInBrowser;
    procedure UpdateStatusBar;
    procedure LogText(AText: string); overload;
    procedure LogText(var Message: TMessage);overload; message WM_BS_LOGTEXT;
    { Private declarations }
  protected
    procedure DoShow; override;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

  procedure EWRun;

implementation

uses ShellApi, EWServerControllerBase;

{%CLASSGROUP 'Vcl.Controls.TControl'}

var
  frmStartup: TfrmStartup;

{$R *.dfm}

procedure EWRun;
begin
  Application.CreateForm(TfrmStartup, frmStartup);
  Application.Run;
end;

procedure TfrmStartup.actClearLogExecute(Sender: TObject);
begin
  memoLog.Clear;

end;

procedure TfrmStartup.actClearSessionsExecute(Sender: TObject);
begin
  GlobalServerController.ClearSessions;
  UpdateStatusBar;
end;

procedure TfrmStartup.actRunInBrowserExecute(Sender: TObject);
begin
  RunInBrowser;
end;

procedure TfrmStartup.actStopServerExecute(Sender: TObject);
begin
  LogText('Stopping server. Please wait...');
  GlobalServerController.StopListening;
  LogText('Stopped successfully.');
end;

constructor TfrmStartup.Create(AOwner: TComponent);
begin
  inherited;
  UpdateStatusBar;
end;

procedure TfrmStartup.DoShow;
begin
  inherited;
  Initialize;
end;

procedure TfrmStartup.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 120 then
    RunInBrowser;
end;

procedure TfrmStartup.Initialize;
begin
  LogText('*** EasyWeb v0.1 ***');
end;

procedure TfrmStartup.LogText(AText: string);
begin
  memoLog.Lines.Add(Trim(AText));
end;

procedure TfrmStartup.LogText(var Message: TMessage);
var
  AStrings: TStrings;
begin
  AStrings := TStrings(Message.WParam);
  LogText(Trim(AStrings.Text));
  AStrings.Free;
end;

procedure TfrmStartup.RunInBrowser;
var
  URL: string;
begin
  LogText('Server starting on port '+GlobalServerController.Port.ToString);
  GlobalServerController.StartListening;
  LogText('Started successfully.');
  URL := 'http://localhost:'+GlobalServerController.Port.ToString;
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  UpdateStatusBar;
end;

procedure TfrmStartup.SpeedButton1Click(Sender: TObject);
begin
  RunInBrowser;
end;

procedure TfrmStartup.Timer1Timer(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TfrmStartup.UpdateStatusBar;
begin
  StatusBar1.Panels[0].Text := 'Port: '+GlobalServerController.Port.ToString;
  StatusBar1.Panels[1].Text := 'Sent: '+Round(GlobalServerController.BytesSent/1000).ToString+' KB';
  StatusBar1.Panels[2].Text := 'Session Count: '+GlobalServerController.Sessions.Count.ToString;
end;

end.
