{***************************************************************************}
{                                                                           }
{           EasyWeb - Bootstrap Framework for Delphi                        }
{                                                                           }
{           Copyright (c) 2019 Graham Murt                                  }
{                                                                           }
{           https://bitbucket.org/gmurt/easyweb/                            }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit EWStartup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, VCL.Forms, Vcl.StdCtrls, EWConst, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ComCtrls, System.Actions, Vcl.ActnList, Vcl.Menus, Vcl.OleCtrls, SHDocVw;



type
  TfrmStartup = class(TForm)
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
    Panel2: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    memoLog: TMemo;
    TabSheet2: TTabSheet;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    Bevel14: TBevel;
    WebBrowser1: TWebBrowser;
    Help1: TMenuItem;
    actProjectWebsite: TAction;
    ProjectWebsite1: TMenuItem;
    N4: TMenuItem;
    actAbout: TAction;
    About1: TMenuItem;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure actRunInBrowserExecute(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure actClearSessionsExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actStopServerExecute(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actProjectWebsiteExecute(Sender: TObject);
  private
    procedure Initialize;
    procedure RunInBrowser;
    procedure UpdateStatusBar;
    procedure LogText(AText: string); overload;
    procedure LogText(var Message: TMessage);overload; message WM_BS_LOGTEXT;
    { Private declarations }
  protected
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

  procedure EWRun;

implementation

uses ShellApi, EWServerControllerBase, VCL.Dialogs, System.UITypes;

{%CLASSGROUP 'Vcl.Controls.TControl'}



var
  frmStartup: TfrmStartup;

{$R *.dfm}

procedure EWRun;
begin
  Application.CreateForm(TfrmStartup, frmStartup);
  Application.Run;
end;

procedure TfrmStartup.actAboutExecute(Sender: TObject);
begin
  MessageDlg('EasyWeb v0.1'+#13+#10+#13+#10+
             'Copyright (c) 2019 Graham Murt'+#13+#10+#13+#10+
             'https://bitbucket.org/gmurt/easyweb', mtInformation, [mbOK], 0);
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

procedure TfrmStartup.actProjectWebsiteExecute(Sender: TObject);
var
  URL: string;
begin
  URL := 'https://bitbucket.org/gmurt/easyweb';
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmStartup.actRunInBrowserExecute(Sender: TObject);
begin
  RunInBrowser;
end;

procedure TfrmStartup.actStopServerExecute(Sender: TObject);
begin
  if GlobalServerController.IsListening then
  begin
    LogText(C_SERVER_STOPPING);
    GlobalServerController.StopListening;
    LogText(C_SERVER_STOPPED);
  end;
end;

constructor TfrmStartup.Create(AOwner: TComponent);
begin
  inherited;
  UpdateStatusBar;
  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmStartup.DoClose(var Action: TCloseAction);
begin
  actStopServer.Execute;
  inherited;
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

procedure TfrmStartup.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 1 then
    WebBrowser1.Navigate('https://kernow-s3.s3-eu-west-1.amazonaws.com/EasyWeb/easyweb.htm');
end;

procedure TfrmStartup.RunInBrowser;
var
  URL: string;
begin
  LogText(StringReplace(C_SERVER_STARTING, '%port%', GlobalServerController.Port.ToString, []));
  GlobalServerController.StartListening;
  LogText(C_SERVER_STARTED);
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
