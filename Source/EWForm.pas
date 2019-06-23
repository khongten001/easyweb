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

unit EWForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, EWBase, EWSession, EWIntf, EWTypes, EWDialogs;

type
  TEWForm = class;
  TEWFormClass = class of TEWForm;

  TEWForm = class(TEWBaseForm, IEWForm)
  private
    FJavascriptIncludes: TStrings;
    FExtraMeta: TStrings;
    FExtraScript: TStrings;
    FSession: TewSession;
    FDialogs: TEWDialog;

    function GetHtml: string;
    function GetBootstrapFiles: string;
    function GetBootstrapCSS: string;
    function GetSession: TewSession;
    procedure SetExtraScript(const Value: TStrings);
    procedure SetExtraMeta(const Value: TStrings);
    procedure SetJavascriptIncludes(const Value: TStrings);
    procedure SetSession(const Value: TewSession);
    { Private declarations }
  protected
    procedure PushForm(ABsFormClass: TEWFormClass);
    procedure PopForm;
    procedure ReloadPage;
    procedure LogText(AText: string);
    function SessionID: string;
    procedure ForceReload;
    procedure ValidateInsert(AComponent: TComponent); override;
    procedure ShowMessage(AText: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
    destructor Destroy; override;
    class procedure SetAsMainForm;
    property Session: TewSession read GetSession write SetSession;
  published

    property Caption;
    property Color;
    property ExtraMeta: TStrings read FExtraMeta write SetExtraMeta;
    property ExtraScript: TStrings read FExtraScript write SetExtraScript;
    property JavascriptIncludes: TStrings read FJavascriptIncludes write SetJavascriptIncludes;
    property PixelsPerInch;
    { Public declarations }
  end;




implementation

uses EWServerControllerBase, EWConst, System.Generics.Collections,
  System.Generics.Defaults;

{%CLASSGROUP 'Vcl.Controls.TControl'}

type
  TEWBaseObjectList = TObjectList<TewBaseObject>;



{$R *.dfm}

constructor TEWForm.Create(AOwner: TComponent);
begin
  inherited;
  FExtraMeta := TStringList.Create;
  FExtraScript := TStringList.Create;
  FJavascriptIncludes := TStringList.Create;
end;

constructor TEWForm.CreateNew(AOwner: TComponent; Dummy: Integer  = 0);
begin
  inherited CreateNew(AOwner, Dummy);
  FDialogs := TEWDialog.Create(Self);
  FDialogs.Name := Name+C_DIALOG;
  FExtraMeta := TStringList.Create;
  FExtraScript := TStringList.Create;
  FJavascriptIncludes := TStringList.Create;
end;

destructor TEWForm.Destroy;
begin
  FExtraScript.Free;
  FExtraMeta.Free;
  FJavascriptIncludes.Free;
  FDialogs.Free;
  inherited;
end;

procedure TEWForm.ForceReload;
begin
  FSession.RequiresReload := True;
end;

function TEWForm.GetBootStrapCSS: string;
begin
  case GlobalServerController.BootstrapVersion of
    Bootstrap_v4_0: result := '<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" integrity="sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" crossorigin="anonymous">';
    Bootstrap_v4_1: result := '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">';
    Bootstrap_v4_2: result := '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css" integrity="sha384-GJzZqFGwb1QTTN6wy59ffF1BuGJpLSa9DkKMp0DgiMDm4iYMj70gZWKYbI706tWS" crossorigin="anonymous">';
    else // useNewst
      result := '<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">';
  end;
end;

function TEWForm.GetBootstrapFiles: string;
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    if GlobalServerController.BootstrapVersion = Bootstrap_v4_0 then
    begin
      AStrings.Add('<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" integrity="sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" crossorigin="anonymous">');
      AStrings.Add('<script src="https://code.jquery.com/jquery-3.4.1.min.js" integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" integrity="sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" integrity="sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl" crossorigin="anonymous"></script>');
    end;
    if GlobalServerController.BootstrapVersion = Bootstrap_v4_1 then
    begin
      AStrings.Add('<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" integrity="sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" crossorigin="anonymous">');
      AStrings.Add('<script src="https://code.jquery.com/jquery-3.4.1.min.js" integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" integrity="sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" integrity="sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" crossorigin="anonymous"></script>');
    end;

    if GlobalServerController.BootstrapVersion = Bootstrap_v4_2 then
    begin
      AStrings.Add('<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css" integrity="sha384-GJzZqFGwb1QTTN6wy59ffF1BuGJpLSa9DkKMp0DgiMDm4iYMj70gZWKYbI706tWS" crossorigin="anonymous">');
      AStrings.Add('<script src="https://code.jquery.com/jquery-3.4.1.min.js" integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.6/umd/popper.min.js" integrity="sha384-wHAiFfRlMFy6i5SRaxvfOCifBUQy1xHdJ/yoi7FRNXMRBu5WHdZYu1hA6ZOblgut" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/js/bootstrap.min.js" integrity="sha384-B0UglyR+jN6CkvvICOB2joaf5I4l3gm9GU6Hc1og6Ls7i6U/mkkaduKaBhlAXv9k" crossorigin="anonymous"></script>');
    end;

    if (GlobalServerController.BootstrapVersion in [Bootstrap_v4_3, UseNewest]) then
    begin
      AStrings.Add('<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous">');
      AStrings.Add('<script src="https://code.jquery.com/jquery-3.4.1.min.js" integrity="sha256-CSXorXvZcTkaix6Yvo6HppcZGetbYMGWSFlBw8HfCJo=" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>');
      AStrings.Add('<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>');
    end;

    Result := AStrings.Text;
  finally
    AStrings.Free;
  end;
end;

function TEWForm.GetHtml: string;
var
  c: IEWBaseComponent;
  i: IEWBaseVisualObject;
  ICount: integer;
  AListners: TStrings;
  AIncludes: TStrings;
  AGlobals: TStrings;
  AViewPort: string;
begin
  AListners := TStringList.Create;
  AIncludes := TStringList.Create;
  AGlobals := TStringList.Create;
  try
    for ICount := 0 to FJavascriptIncludes.Count-1 do
      AIncludes.Add('<script src="'+FJavascriptIncludes[ICount]+'"></script>');

    for ICount := 0 to Self.ComponentCount-1 do
    begin
      if Supports(Self.Components[ICount], IEWBaseComponent, c) then
        C.GetGlobalVars(AGlobals);


      if Supports(Self.Components[ICount], IEWBaseVisualObject, i) then
      begin
        i.GetEventListners(AListners);
      end;
    end;

    if GlobalServerController.DisableZoom then
      AViewPort := '<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1,user-scalable=0"/>'
    else
      AViewPort := '<meta name="viewport" content="width=device-width, initial-scale=1">';



    Result := '<!DOCTYPE html>'+CR+
              '<html>'+CR+
              '<head>'+CR+
              '<title>'+Caption+'</title>'+CR+
             // '<meta charset="utf-8">'+CR+
              AViewPort+CR+
              Trim(FExtraMeta.Text)+
              GetBootstrapCSS+CR+
              '<meta name="IncludesNext" />' + CR +
              Trim(AIncludes.Text)+CR+
              '<meta name="StylesNext" />' + CR +
              '<style>'+CR+
              '</style>'+CR+
              '</head>'+CR+
              '<body>'+
              '<easyweb id='+Name+'>';


      for ICount := 0 to ComponentCount-1 do
      begin
        if Self.Components[ICount] is TEWBaseComponent then
          Result := Result + TEWBaseComponent(Self.Components[ICount]).Html +CR;


        if Self.Components[ICount] is TEWBaseObject then
        begin
          if TewBaseObject(Self.Components[ICount] ).Parent = Self then
            Result := Result + TEWBaseObject(Self.Components[ICount]).Html +CR;
        end;
      end;

    Result := Result +CR+
               '</easyweb>'+

              Trim(Trim(AGlobals.Text)+CR+

              GetBootstrapFiles +
              '<script type="text/javascript" src="/ewcore.js"></script>'+CR+
              '<script type="text/javascript">'+CR+
                Trim(AListners.Text)) + CR + CR +
                ' // Extra Script: ' + CR +
                FExtraScript.Text+CR+
              '</script>'+CR+
              '</body>'+CR+
            '</html>';
  finally
    AListners.Free;
    AIncludes.Free;
    AGlobals.Free;
  end;

end;

function TEWForm.GetSession: TewSession;
begin
  Result := FSession;
end;

procedure TEWForm.LogText(AText: string);
var
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  AStrings.Text := Trim(AText);
  PostMessage(Application.MainForm.Handle, WM_BS_LOGTEXT, Integer(AStrings), 0);
end;

procedure TEWForm.PopForm;
begin
  Session.PopForm;
end;

procedure TEWForm.PushForm(ABsFormClass: TewFormClass);
var
  AForm: TEWForm;
begin
  TThread.Synchronize(nil,
  procedure
  begin
    AForm := ABsFormClass.CreateNew(nil);
    InitInheritedComponent(AForm, TewForm);
    AForm.Session := FSession;
  end);
  Session.PushForm(AForm);
end;

procedure TEWForm.ReloadPage;
begin
  Session.RequiresReload := True;
end;

function TEWForm.SessionID: string;
begin
  Result := '';
  if FSession <> nil then
    Result := FSession.SessionID;
end;

class procedure TEWForm.SetAsMainForm;
begin
  GlobalServerController.SetMainform(Self);
end;

procedure TEWForm.SetJavascriptIncludes(const Value: TStrings);
begin
  FJavascriptIncludes.Assign(Value);
end;

procedure TEWForm.SetExtraMeta(const Value: TStrings);
begin
  FExtraMeta.Assign(Value);
end;

procedure TEWForm.SetExtraScript(const Value: TStrings);
begin
  FExtraScript.Assign(Value);
end;

procedure TEWForm.SetSession(const Value: TewSession);
begin
  FSession := Value;
end;

procedure TEWForm.ShowMessage(AText: string);
begin
  FDialogs.ShowMessage(AText);
end;

procedure TEWForm.ValidateInsert(AComponent: TComponent);
var
  AIntf: IEWBaseComponent;
begin
  if (AComponent is TControl) and (Supports(AComponent, IEWBaseComponent, AIntf) = False) then
    raise EInvalidInsert.Create(SInvalidControlType+CR+CR+SUseEasyWebCompsOnly)
  else
    inherited;
end;

end.
