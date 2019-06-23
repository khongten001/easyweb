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

unit EWBase;

interface

uses Messages, Classes, VCL.Controls, VCL.Forms, EWTypes, EWIntf, System.Types;

type
  TEWBaseComponent = class(TComponent, IEWBaseComponent)
  private
    FChanged: Boolean;
    function GetHasChanged: Boolean;
    function GetName: string;
  protected
    procedure DoEvent(AParams: TStrings); virtual;
    procedure GetRequiredThirdPartySrc(AStrings: TStrings); virtual;
    procedure GetGlobalVars(AStrings: TStrings); virtual;
    procedure Changed; virtual;
    function GetHtml: string;
    function GenerateHtml: string; virtual;
    function GetScript: string; virtual;
    property HasChanged: Boolean read GetHasChanged;
  public
    property Html: string read GetHtml;
  end;

  TewBaseObject = class(TCustomControl, IEWBaseComponent, IEWBaseVisualObject)
  private
    FExtraHtmlTags: TStrings;
    FChanged: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FCssProperties: TStrings;
    FMouseOver: Boolean;
    FOnRightClick: TNotifyEvent;
    function GetHasChanged: Boolean;
    function GetName: string;
    function GetExtraTags: string;
    procedure SetExtraHtmlTags(const Value: TStrings);
  protected
    function GetID: string;
    function GetScript: string; virtual;
    procedure GetRequiredThirdPartySrc(AStrings: TStrings); virtual;
    function ReplaceTokens(AHtml: string): string; virtual;
    procedure GetEventListners(AListners: TStrings); virtual;
    procedure GetGlobalVars(AStrings: TStrings); virtual;
    procedure AddObjectEvent(AID, AAction: string; ANameValues: array of string; AEvents: TStrings; AValueJS: string; const APreventDefault: Boolean = False); overload;
    procedure AddObjectEvent(AID, AAction: string; ANameValues, AEvents: TStrings; AValueJS: string; const APreventDefault: Boolean = False); overload;
    procedure AddMouseEnterEvent(AEvents: TStrings);
    procedure AddMouseLeaveEvent(AEvents: TStrings);
    procedure AddClickEvent(AEvents: TStrings); virtual;
    procedure AddRightClickEvent(AEvents: TStrings);
    procedure AddDblClickEvent(AEvents: TStrings);
    procedure AddEnterEvent(AEvents: TStrings);
    procedure AddExitEvent(AEvents: TStrings);
    procedure AddOnKeyDownEvent(AEvents: TStrings);
    procedure AddOnKeyPressEvent(AEvents: TStrings);
    procedure AddOnKeyUpEvent(AEvents: TStrings);
    procedure AddOnChangeEvent(AEvents: TStrings);
    procedure DoEvent(AParams: TStrings); virtual;
    procedure DoClick(AParams: TStrings); virtual;
    procedure DoRightClick(AParams: TStrings); virtual;
    procedure DoDblClick(AParams: TStrings); virtual;
    procedure DoMouseEnter(AParams: TStrings);  virtual;
    procedure DoMouseLeave(AParams: TStrings);  virtual;
    procedure DoOnChange(AParams: TStrings); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure VisibleChanging; override;
    function GetHtml: string;
    function GenerateHtml: string; virtual;
    function DesignTimeCaption: string; virtual;
    procedure Resize; override;
    procedure BuildCss(AProperties: TStrings); virtual;
    procedure Paint; override;
    function GetCss: string;
    procedure Changed; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function SessionID: string;
    property Html: string read GetHtml;
    property HasChanged: Boolean read GetHasChanged write FChanged;
  published
    property ExtraHtmlTags: TStrings read FExtraHtmlTags write SetExtraHtmlTags;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
  end;

implementation

uses SysUtils, VCL.Graphics, EWServerControllerBase, Json;

{ TEWBaseComponent }

procedure TEWBaseComponent.Changed;
begin
  FChanged := True;
end;

procedure TEWBaseComponent.DoEvent(AParams: TStrings);
begin
  //
end;

function TEWBaseComponent.GenerateHtml: string;
begin
  Result := '';
end;

procedure TEWBaseComponent.GetGlobalVars(AStrings: TStrings);
begin
  //
end;

function TEWBaseComponent.GetHasChanged: Boolean;
begin
  Result := FChanged;
end;

function TEWBaseComponent.GetHtml: string;
begin
  Result := GenerateHtml;
  FChanged := False;
end;

function TEWBaseComponent.GetName: string;
begin
  Result := Name;
end;

procedure TEWBaseComponent.GetRequiredThirdPartySrc(AStrings: TStrings);
begin
  // overridden in descendant classes.
end;

function TEWBaseComponent.GetScript: string;
begin
  Result := '';
end;

{ TBsBaseObject }

procedure TewBaseObject.AddClickEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'click', [], AEvents, '');
end;

procedure TewBaseObject.AddRightClickEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'contextmenu', [], AEvents, '');
end;

procedure TewBaseObject.AddDblClickEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'dblclick', [], AEvents, '');
end;

procedure TewBaseObject.AddMouseEnterEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'mouseenter', [], AEvents, '');
end;

procedure TewBaseObject.AddEnterEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'focus', [], AEvents, '');
end;

procedure TewBaseObject.AddExitEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'blur', [], AEvents, '');
end;

procedure TewBaseObject.AddMouseLeaveEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'mouseleave', [], AEvents, '');
end;

procedure TewBaseObject.AddOnKeyDownEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'keydown', [], AEvents, 'event.keyCode');
end;

procedure TewBaseObject.AddOnKeyPressEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'input', [], AEvents, 'document.getElementById(''' + Name +''').value');
end;

procedure TewBaseObject.AddOnKeyUpEvent(AEvents: TStrings);
begin
  AddObjectEvent(Name, 'keyup', [], AEvents, 'event.keyCode');
end;

procedure TewBaseObject.AddObjectEvent(AID, AAction: string;
                                       ANameValues,
                                       AEvents: TStrings;
                                       AValueJS: string;
                                       const APreventDefault: Boolean = False);
var
  AJson: TJsonObject;
  ICount: integer;
  APreventFunc: string;
begin
  AJson := TJSONObject.Create;
  try
    AJson.AddPair('name', Name);
    AJson.AddPair('event', AAction);
    if APreventDefault then
      APreventFunc := 'event.preventDefault(); ';
    for ICount := 0 to ANameValues.Count-1 do
      AJson.AddPair(ANameValues.Names[ICount], ANameValues.ValueFromIndex[ICount]);
    if Trim(AValueJS) = '' then
      AValueJS := '''''';
    AEvents.Add('$(document).on('''+AAction+''',''#'+AID+''+''',function(event) {'+APreventFunc+'eventCall('''+AAction+''', '+AValueJS+', '''+AJson.ToJSON+'''); }); ');
  finally
    AJson.Free;
  end;
end;

procedure TewBaseObject.AddObjectEvent(AID, AAction: string;
  ANameValues: array of string;
  AEvents: TStrings;
  AValueJS: string;
  const APreventDefault: Boolean = False);
var
  s: string;
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    for s in ANameValues do
      AStrings.Add(s);
    AddObjectEvent(AID, AAction, AStrings, AEvents, AValueJS, APreventDefault);
  finally
    AStrings.Free;
  end;
end;

procedure TewBaseObject.AddOnChangeEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''change'',''#'+Name+''',function(){ asyncEvent("change", "'+Name+'", document.getElementById(''' + Name +''').value); }); ');
end;

procedure TewBaseObject.BuildCss(AProperties: TStrings);
var
  l: IEWLayout;
begin
//  AProperties.Values['border'] := 'solid red 1px';
  if Supports(Parent, IEWLayoutGrid, l) = False then
  begin
    AProperties.Values['position'] := 'absolute';
    if Align = alNone then
    begin
      AProperties.Values['left'] := IntToStr(Left) + 'px';
      AProperties.Values['top'] := IntToStr(Top) + 'px';
      AProperties.Values['width'] := IntToStr(Width) + 'px';
      AProperties.Values['height'] := IntToStr(Height) + 'px';
    end;
    if Align = alTop then
    begin
      AProperties.Values['left'] := IntToStr(Left) + 'px';
      AProperties.Values['top'] := IntToStr(Top) + 'px';
      AProperties.Values['right'] := (Parent.ClientWidth - (Width+Left)).ToString+ 'px';
      AProperties.Values['height'] := 'auto';
    end;

    if Align = alLeft then
    begin
      AProperties.Values['left'] := IntToStr(Left) + 'px';
      AProperties.Values['top'] := IntToStr(Top) + 'px';
      AProperties.Values['width'] := IntToStr(Width) + 'px';
      AProperties.Values['bottom'] := (Parent.ClientHeight - (Height+Top)).ToString+ 'px';
    end;

    if Align = alRight then
    begin
      AProperties.Values['right'] := (Parent.ClientWidth - (Width+Left)).ToString+ 'px';
      AProperties.Values['top'] := IntToStr(Top) + 'px';
      AProperties.Values['width'] := IntToStr(Width) + 'px';
      AProperties.Values['bottom'] := (Parent.ClientHeight - (Height+Top)).ToString+ 'px';
    end;

    if Align = alBottom then
    begin
      AProperties.Values['bottom'] := '0px;';//(BrowserSize.cy - (Height+Top)).ToString+ 'px';
      AProperties.Values['left'] := IntToStr(left) + 'px';
      AProperties.Values['right'] := (Parent.ClientWidth - (Width+Left)).ToString+ 'px';
      AProperties.Values['height'] := IntToStr(Height) + 'px';
    end;

    if Align = alClient then
    begin
      AProperties.Values['left'] := IntToStr(Left) + 'px';
      AProperties.Values['right'] := (Parent.ClientWidth - (Width+Left)).ToString+ 'px';
      AProperties.Values['top'] := IntToStr(Top) + 'px';
      AProperties.Values['bottom'] := (Parent.ClientHeight - (Height+Top)).ToString+ 'px';
    end;
  end;
  if not Visible then
    AProperties.Values['display'] := 'none';
end;

procedure TewBaseObject.Changed;
var
  ICount: integer;
begin
  FChanged := True;
  for ICount := 0 to ControlCount-1 do
  begin
    if (Controls[ICount] is TewBaseObject) then
      (Controls[ICount] as TewBaseObject).Changed;
  end;
  Invalidate;
end;

constructor TewBaseObject.Create(AOwner: TComponent);
begin
  inherited;
  FExtraHtmlTags := TStringList.Create;
  FCssProperties := TStringList.Create;
  FMouseOver := False;

end;

function TewBaseObject.DesignTimeCaption: string;
begin
  Result := Name;
end;

destructor TewBaseObject.Destroy;
begin
  FCssProperties.Free;
  FExtraHtmlTags.Free;
  inherited;
end;


procedure TewBaseObject.DoClick(AParams: TStrings);
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TewBaseObject.DoRightClick(AParams: TStrings);
begin
  if Assigned(FOnRightClick) then FOnRightClick(Self);
end;

procedure TewBaseObject.DoDblClick(AParams: TStrings);
begin
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

procedure TewBaseObject.DoEvent(AParams: TStrings);
var
  AEvent: string;
begin
  inherited;
  AEvent := AParams.Values['event'].ToLower;
  if AEvent = 'click' then DoClick(AParams);
  if AEvent = 'dblclick' then DoDblClick(AParams);
  if AEvent = 'contextmenu' then DoRightClick(AParams);
  if AEvent = 'mouseenter' then DoMouseEnter(AParams);
  if AEvent = 'mouseleave' then DoMouseLeave(AParams);
  //if AEvent = 'blur' then DoMouseLeave(AParams);
end;

procedure TewBaseObject.DoMouseEnter(AParams: TStrings);
begin
  //if FMouseOver then
  //  Exit;
  FMouseOver := True;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);

end;

procedure TewBaseObject.DoMouseLeave(AParams: TStrings);
begin
  //if FMouseOver = False then
  //  Exit;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  FMouseOver := False;
end;

procedure TewBaseObject.DoOnChange(AParams: TStrings);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TewBaseObject.GenerateHtml: string;
begin
  Result := '';
end;

function TewBaseObject.GetCss: string;
var
  ICount: integer;
begin
  FCssProperties.Clear;
  BuildCss(FCssProperties);
  Result := 'style="';
  for ICount := 0 to FCssProperties.Count - 1 do
    Result := Result + FCssProperties.Names[ICount] + ':' +
      FCssProperties.ValueFromIndex[ICount] + ';';
  Result := Result + '" ';
end;

procedure TewBaseObject.GetEventListners(AListners: TStrings);
begin
  if Assigned(FOnMouseEnter) then AddMouseEnterEvent(AListners);
  if Assigned(FOnMouseLeave) then AddMouseLeaveEvent(AListners);
  if Assigned(FOnClick) then AddClickEvent(AListners);
  if Assigned(FOnDblClick) then AddDblClickEvent(AListners);
  if Assigned(FOnRightClick) then AddRightClickEvent(AListners);
end;

function TewBaseObject.GetExtraTags: string;
var
  ICount: integer;
begin
  Result := '';
  for ICount := 0 to FExtraHtmlTags.Count-1 do
    Result := Result + FExtraHtmlTags[ICount]+' ';
  Result := Trim(Result);
end;

procedure TewBaseObject.GetGlobalVars(AStrings: TStrings);
begin
  //
end;

function TewBaseObject.GetHasChanged: Boolean;
begin
  Result := FChanged;
end;

function TewBaseObject.GetHtml: string;
begin
  Result := GenerateHtml;
  FChanged := False;
end;

function TewBaseObject.GetID: string;
begin
  Result := Name;
end;

function TewBaseObject.GetName: string;
begin
  Result := Name;
end;

procedure TewBaseObject.GetRequiredThirdPartySrc(AStrings: TStrings);
begin
  inherited;
  // overridden in descendant classes.
end;

function TewBaseObject.GetScript: string;
begin
  Result := '';
end;

procedure TewBaseObject.Paint;
var
  r: TRect;
  //s: string;
begin
  if (csDesigning in ComponentState) then
  begin
    Canvas.Pen.Color := clGray;
    canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    r := ClientRect;
    Canvas.Rectangle(r);
    //s := DesignTimeCaption;
    //Canvas.TextRect(r, s, []);
  end;
end;

function TewBaseObject.ReplaceTokens(AHtml: string): string;
begin
  Result := AHtml;
  Result := StringReplace(Result, '%name%', Name, [rfReplaceAll]); // name may be used as part of sub-element names
  Result := StringReplace(Result, '%id%', GetID, []);
  Result := StringReplace(Result, '%style%', GetCss, []);
  Result := StringReplace(Result, '%extratags%', GetExtraTags, []);

end;

procedure TewBaseObject.Resize;
begin
  inherited;
  changed;
end;

function TewBaseObject.SessionID: string;
var
  c: TComponent;
  i: IewForm;
begin
  Result := '';
  c := Self;
  while (c is TEWBaseForm) = False do
    c := c.Owner;
  if (c is TEWBaseForm) then
  begin
    Supports(c, IEWForm, I);
    Result := i.SessionID;
  end;
end;

procedure TewBaseObject.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  l: IEWLayout;
begin
  inherited;
  if Supports(Parent, IEWLayoutGrid, L) then
  begin
     Align := alLeft;
  end;
  Changed;
end;

procedure TewBaseObject.SetExtraHtmlTags(const Value: TStrings);
begin
  if Value.Text <> FExtraHtmlTags.Text then
  begin
    FExtraHtmlTags.Assign(Value);
    Changed;
  end;
end;

procedure TewBaseObject.SetName(const Value: TComponentName);
begin
  inherited;
end;

procedure TewBaseObject.VisibleChanging;
begin
  inherited;
  FChanged := True;
end;

end.
