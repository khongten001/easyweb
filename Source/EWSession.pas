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

unit EWSession;

interface

uses Classes, VCL.Forms, System.Generics.Collections, EWBase;

type
  TEWSessionDataClass = class of TDataModule;

  TEWSession = class
  private
    FForms: TObjectList<TCustomForm>;
    FDatamodule: TDataModule;
    FSessionID: string;
    FFormIndex: integer;
    FRequiresReload: Boolean;
    function GetHtml: string;
    function GetSelectedForm: TCustomForm;
    procedure SetSessionID(const Value: string);
  public
    constructor Create(ASessionID: string); virtual;
    destructor Destroy; override;
    class procedure RegisterSessionData(ASessionData: TEWSessionDataClass);
    procedure PushForm(AForm: TCustomForm);
    procedure PopForm;

    property DataModule: TDataModule read FDatamodule;
    property SessionID: string read FSessionID write SetSessionID;
    property Forms: TObjectList<TCustomForm> read FForms;
    property Html: string read GetHtml;
    property SelectedForm: TCustomForm read GetSelectedForm;
    property RequiresReload: Boolean read FRequiresReload write FRequiresReload;
  end;

  TEWSessionList = class(TObjectList<TEWSession>)
  private
    function GetSessionByID(ASessionID: string): TEWSession;
  public
    function AddSession(ASessionID: string): TEWSession;
    property SessionByID[ASessionID: string]: TEWSession read GetSessionByID;
  end;


implementation

uses EWServerControllerBase, SysUtils, System.Threading, EWIntf, EWForm, EWTypes;

var
  SessionDataClass: TEWSessionDataClass;

{ TEWSessionList }

function TEWSessionList.AddSession(ASessionID: string): TewSession;
begin
  Result := TewSession.Create(ASessionID);
  Add(Result);
end;

function TEWSessionList.GetSessionByID(ASessionID: string): TewSession;
var
  s: TewSession;
begin
  Result := nil;
  for s in Self do
  begin
    if s.FSessionID = ASessionID then
    begin
      Result := s;
      Exit;
    end;
  end;
end;

{ TEWSession }

constructor TEWSession.Create(ASessionID: string);
var
  AForm: TewForm;
begin
  inherited Create;
  FForms := TObjectList<TCustomForm>.Create(True);
  FDatamodule := SessionDataClass.Create(nil);
  TThread.Synchronize(nil,
  procedure
  begin
    AForm := EWMainFormClass.CreateNew(nil);
    InitInheritedComponent(AForm, TEWForm);
    TEWForm(AForm).Session := Self;
    FForms.Add(TCustomForm(AForm));
  end);
  FSessionID := ASessionID;

  FFormIndex := 0;
  FRequiresReload := False;
end;

destructor TEWSession.Destroy;
begin
  FForms.Free;
  FDatamodule.Free;
  inherited;
end;

procedure TEWSession.PopForm;
begin
  TThread.Synchronize(nil,
  procedure
  begin
    FForms.Delete(FFormIndex);
    Dec(FFormIndex);
    FRequiresReload := True;
  end);
end;

procedure TewSession.PushForm(AForm: TCustomForm);
begin
  FForms.Add(AForm);
  FFormIndex := FFormIndex+1;
  FRequiresReload := True;
end;

class procedure TEWSession.RegisterSessionData(
  ASessionData: TEWSessionDataClass);
begin
  SessionDataClass := ASessionData;
end;

function TEWSession.GetHtml: string;
var
  f: IEWForm;
begin
  if Supports(SelectedForm, IEWForm, f) then
    Result := f.Html;
end;

function TEWSession.GetSelectedForm: TCustomForm;
begin
  Result := nil;
  if FFormIndex > -1 then
    Result := FForms[FFormIndex];
end;

procedure TEWSession.SetSessionID(const Value: string);
begin
  FSessionID := Value;
end;

end.
