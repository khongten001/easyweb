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

unit EWLayout;

interface

uses Classes, EWIntf, EWBase, VCL.Graphics, EWTypes;

type
  TEWBaseLayout = class(TEWBaseObject, IEWLayout)
  private
    FPadding: TEWPadding;
    procedure PaddingChanged(Sender: TObject);
    procedure SetPadding(const Value: TEWPadding);
  protected
    function DesignTimeCaption: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Padding: TEWPadding read FPadding write SetPadding;
  end;

  TEWLayout = class(TEWBaseLayout)
  private
    FFluid: Boolean;
    procedure SetFluid(const Value: Boolean);
  protected
    function GenerateHtml: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fluid: Boolean read FFluid write SetFluid default True;
  end;

  TEWLayoutGrid = class(TEWBaseLayout, IEWLayoutGrid)
  private
    FMargin: integer;
    procedure SetMargin(const Value: integer);
  protected
    function GenerateHtml: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Margin: integer read FMargin write SetMargin default 6;
  end;

implementation

uses Types, SysUtils, Vcl.Controls;

{ TEWBaseLayout }

constructor TEWBaseLayout.Create(AOwner: TComponent);
begin
  inherited;
  FPadding := TEWPadding.Create(PaddingChanged);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Align := alTop;
end;

function TEWBaseLayout.DesignTimeCaption: string;
begin
  inherited;
  Result := '';
end;

destructor TEWBaseLayout.Destroy;
begin
  FPadding.Free;
  inherited;
end;

procedure TEWBaseLayout.PaddingChanged(Sender: TObject);
begin
  Changed;
end;

procedure TEWBaseLayout.SetPadding(const Value: TEWPadding);
begin
  FPadding.Assign(Value);
end;

{ TEWLayout }

constructor TEWLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFluid := True;
end;

function TEWLayout.GenerateHtml: string;
var
  ICount: integer;
  AFluid: string;
begin
  inherited;
  AFluid := '';
  if FFluid then
    AFluid := '-fluid';
  Result := '<div id="'+Name+'" style="border:solid red 1px; position:relative; height:'+Height.ToString+'px;'+FPadding.AsCssProperty+'" class="container'+AFluid+'" id="'+Name+'">';
    for ICount := 0 to ControlCount-1 do
    Result := Result + TewBaseObject(Controls[ICount]).Html;
  Result := Result + '</div>';
end;

procedure TEWLayout.SetFluid(const Value: Boolean);
begin
  if FFluid <> Value then
  begin
    FFluid := Value;
    Changed;
  end;
end;

{ TEWLayoutGrid }

constructor TEWLayoutGrid.Create(AOwner: TComponent);
begin
  inherited;
  FMargin := 6;
end;


procedure TEWLayoutGrid.SetMargin(const Value: integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Changed;
  end;
end;


function TEWLayoutGrid.GenerateHtml: string;
var
  ICount: integer;
begin
  inherited;
  Result := '<div id="'+Name+'" style="top:'+Top.ToString+'px; min-height:20px;" class="container" id="'+Name+'">';
  Result := Result + '<div style="height:100%; " class="row">';
    for ICount := 0 to ControlCount-1 do
  begin
    Result := Result + '<div style="margin:'+FMargin.ToString+'px;" class="col-sm">';
    Result := Result + TewBaseObject(Controls[ICount]).Html;
    Result := Result + '</div>';
  end;
  Result := Result + '</div>';
  Result := Result + '</div>';
end;


end.
