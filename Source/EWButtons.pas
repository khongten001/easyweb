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

unit EWButtons;

interface

uses Windows, Classes, EWIntf, EWBase, EWTypes, EWBadge;

type
  TEWDropDown = class;

  TEWBaseButton = class(TewBaseObject, IEWButton)
  private
    FButtonType: TewButtonType;
    FBorderRadius: integer;
    FOutline: Boolean;
    FText: string;
    FBadge: TEWBadge;
    function GetButtonType: TewButtonType; virtual;
    function GetButtonTypeStr: string;
    procedure SetButtonType(const Value: TewButtonType);
    function GetText: string;
    procedure SetText(const Value: string);
    procedure SetBorderRadius(const Value: integer);
    procedure SetOutline(const Value: Boolean);
    procedure SetBadge(const Value: TEWBadge);
  protected
    function ReplaceTokens(AHtml: string): string; override;
    function GenerateHtml: string; override;
    function DesignTimeCaption: string; override;
    procedure Paint; override;
    procedure BuildCss(AProperties: TStrings); override;
    property Text: string read GetText write SetText;
    property Badge: TEWBadge read FBadge write SetBadge;
    property Outline: Boolean read FOutline write SetOutline default False;
    property BorderRadius: integer read FBorderRadius write SetBorderRadius default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property ButtonType: TewButtonType read GetButtonType write SetButtonType default btSecondary;
  end;

  TEWButton = class(TEWBaseButton)
  published
    property Text;
    property Badge;
    property Outline;
    property BorderRadius;
  end;

  TEWDropDownItem = class(TCollectionItem)
  private
    FButton: TEWDropDown;
    FText: string;
    FEnabled: Boolean;
    FDivider: Boolean;
    procedure SetText(const Value: string);
    procedure Changed;
    procedure SetDivider(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
  public
    function GetHtml: string;
    procedure Assign(Source: TPersistent); override;
  published
    constructor Create(Collection: TCollection); override;
    property Text: string read FText write SetText;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Divider: Boolean read FDivider write SetDivider default False;
  end;

  TEWDropDownItemCollection = class(TCollection)
  private
    FButton: TEWDropDown;
  protected
    function GetItem(Index: Integer): TEWDropDownItem;
    procedure SetItem(Index: Integer; Value: TEWDropDownItem);
  public
    constructor Create(ADropDown: TEWDropDown);
    function GetHtml: string;
    function Add: TEWDropDownItem;
    function Insert( Index: Integer ): TEWDropDownItem;
    property Items[index: Integer]: TEWDropDownItem read GetItem write SetItem; default;
  end;

  TEWDropDown = class(TEWButton)
  private
    FItems: TEWDropDownItemCollection;
    FOnItemClick: TEWDropDownClickItemEvent;
    FSplitButton: Boolean;
    function GetItems: TEWDropDownItemCollection;
    function GetOnItemClick: TEWDropDownClickItemEvent;
    procedure SetOnItemClick(Value: TEWDropDownClickItemEvent);
    procedure SetItems(const Value: TEWDropDownItemCollection);
    procedure SetSplitButton(const Value: Boolean);
  protected
    function ReplaceTokens(AHtml: string): string; override;

    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
    procedure BuildCss(AProperties: TStrings); override;
    procedure DoEvent(AParams: TStrings); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TEWDropDownItemCollection read GetItems write SetItems;
    property SplitButton: Boolean read FSplitButton write SetSplitButton default False;
    property OnItemClick: TEWDropDownClickItemEvent read GetOnItemClick write SetOnItemClick;

  end;

  TEWButtonGroup = class(TEWButton, IEWBaseObjectItemClickable)
  private
    FItems: TStrings;
    FOnItemClick: TEWClickItemEvent;
    FItemIndex: integer;
    FLayout: TEWButtonGroupLayout;
    function GetItems: TStrings;
    function GetOnItemClick: TEWClickItemEvent;
    procedure SetOnItemClick(Value: TEWClickItemEvent);
    procedure SetItems(const Value: TStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(Value: integer);
    procedure SetLayout(const Value: TEWButtonGroupLayout);
  protected
    function ReplaceTokens(AHtml: string): string; override;
    procedure DoClick(AParams: TStrings); override;
    function GenerateHtml: string; override;
    procedure DoItemClick(ASender: TObject; AItem: string; AIndex: integer);
    procedure GetEventListners(AListners: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStrings read GetItems write SetItems;
    property Layout: TEWButtonGroupLayout read FLayout write SetLayout default bgHorizontal;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property OnItemClick: TEWClickItemEvent read GetOnItemClick write SetOnItemClick;
  end;

implementation

uses System.Types, VCL.Graphics, SysUtils, System.UITypes, EWConst;



{ TEWBaseButton }

procedure TEWBaseButton.BuildCss(AProperties: TStrings);
begin
  inherited;
  if FBorderRadius > 0 then AProperties.Values[C_CSS_CORNER_RADIUS] := BorderRadius.ToString+C_CSS_PX;
end;

constructor TEWBaseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBadge := TEWBadge.Create(Self);
  FButtonType := btSecondary;
end;

function TEWBaseButton.DesignTimeCaption: string;
begin
  Result := Text;
end;

destructor TEWBaseButton.Destroy;
begin
  FBadge.Free;
  inherited;
end;

function TEWBaseButton.GetButtonType: TewButtonType;
begin
  Result := FButtonType;
end;

function TEWBaseButton.GetButtonTypeStr: string;
var
  AStyle: string;
begin
  AStyle := 'btn';
  if FOutline then
    AStyle := C_BTN_OUTLINE;
  case FButtonType of
    btBasic:      Result := 'btn';
    btDefault:    Result := 'btn '+AStyle+'-default';
    btPrimary:    Result := 'btn '+AStyle+'-primary';
    btSecondary:  Result := 'btn '+AStyle+'-secondary';
    btSuccess:    Result := 'btn '+AStyle+'-success';
    btDanger:     Result := 'btn '+AStyle+'-danger';
    btWarning:    Result := 'btn '+AStyle+'-warning';
    btInfo:       Result := 'btn '+AStyle+'-info';
    btLight:      Result := 'btn '+AStyle+'-light';
    btDark:       Result := 'btn '+AStyle+'-dark';
    btLink:       Result := 'btn '+AStyle+'-link';
  end;
end;

function TEWBaseButton.GenerateHtml: string;
begin
  inherited;
  Result := ReplaceTokens(C_HTML_BUTTON);
end;

function TEWBaseButton.GetText: string;
begin
  Result := FText;
  if Result = '' then
    Result := Name;
end;

procedure TEWBaseButton.Paint;
var
  ARect: TRect;
  AText: string;
begin
  Canvas.Brush.Color := clSilver;
  Canvas.Font.Color := clWhite;
  Canvas.Font.Size := 11;
  Canvas.Font.Name := 'Arial';
  if FButtonType = btPrimary then Canvas.Brush.Color := $FF7B00;
  if FButtonType = btSecondary then Canvas.Brush.Color := $7D756C;
  if FButtonType = btSuccess then Canvas.Brush.Color := $45A728;
  if FButtonType = btDanger then Canvas.Brush.Color := $4535DC;
  if FButtonType = btWarning then Canvas.Brush.Color := $07C1FF;
  if FButtonType = btInfo then Canvas.Brush.Color := $B8A217;
  if FButtonType = btDark then Canvas.Brush.Color := $403A34;

  if FButtonType = btLight then
  begin
    Canvas.Brush.Color := $FAF9F8;
    Canvas.Font.Color := clBlack;
  end;
  if FButtonType = btLink then
  begin
    Canvas.Brush.Color := $FFFFFF;
    Canvas.Font.Color := clWebDodgerBlue;
  end;

  if FOutline then
  begin
    Canvas.Font.Color := Canvas.Brush.Color;
    Canvas.Pen.Color := Canvas.Brush.Color;
    Canvas.Brush.Style := bsClear;
  end;

  if FButtonType = btSuccess then
  begin
    Canvas.Brush.Color := clWebForestGreen;
    Canvas.Font.Color := clWhite;
  end;

  //Canvas.Pen.Style := psClear;
  Canvas.RoundRect(ClientRect, 6, 6);
  ARect := ClientRect;
  AText := Text;

  Canvas.TextRect(ARect, AText, [tfCenter, tfVerticalCenter, tfSingleLine]);
end;

function TEWBaseButton.ReplaceTokens(AHtml: string): string;
begin
  Result := inherited ReplaceTokens(AHtml);
  Result := StringReplace(Result,'%class%', GetButtonTypeStr, []);
  Result := StringReplace(Result,'%badge%', FBadge.Html, []);
  Result := StringReplace(Result,'%text%', FText, []);
end;

procedure TEWBaseButton.SetButtonType(const Value: TewButtonType);
begin
  if FButtonType <> Value then
  begin
    FButtonType := Value;
    Changed;
  end;
end;

procedure TEWBaseButton.SetOutline(const Value: Boolean);
begin
  if FOutline <> Value then
  begin
    FOutline := Value;
    Changed;
  end;
end;

procedure TEWBaseButton.SetBadge(const Value: TEWBadge);
begin
  FBadge.Assign(Value);
end;

procedure TEWBaseButton.SetBorderRadius(const Value: integer);
begin
  if FBorderRadius <> Value then
  begin
    FBorderRadius := Value;
    Changed;
  end;
end;

procedure TEWBaseButton.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TEWDropDown }

procedure TEWDropDown.BuildCss(AProperties: TStrings);
begin
  inherited;

end;

constructor TEWDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TEWDropDownItemCollection.Create(Self);
  FSplitButton := False;
end;

destructor TEWDropDown.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEWDropDown.DoEvent(AParams: TStrings);
var
  AIndex: integer;
begin
  inherited;
  if Assigned(FOnItemClick) then
  begin
    AIndex := StrToInt(AParams.Values[C_INDEX]);
    if FItems[AIndex].Enabled then
    begin
      FOnItemClick(Self, FItems[AIndex], AIndex);
    end;
  end;
end;

procedure TEWDropDown.GetEventListners(AListners: TStrings);
var
  ICount: integer;
begin
  inherited;
  if Assigned(FOnItemClick) then
  begin
    for ICount := 0 to FItems.Count-1 do
      AddObjectEvent(Name+C_ITEM+ICount.ToString, C_CLICK, [C_INDEX+'='+ICount.ToString], AListners, '');
  end;
end;

function TEWDropDown.GenerateHtml: string;
begin
  inherited;
  { TODO : Split button HTML }
  Result := ReplaceTokens(C_HTML_BUTTON_DROPDOWN);
end;


function TEWDropDown.GetItems: TEWDropDownItemCollection;
begin
  Result := FItems;
end;

function TEWDropDown.GetOnItemClick: TEWDropDownClickItemEvent;
begin
  Result := FOnItemClick;
end;

function TEWDropDown.ReplaceTokens(AHtml: string): string;
begin
  Result := inherited ReplaceTokens(AHtml);
  Result := StringReplace(Result,'%style%', GetButtonTypeStr, []);
  Result := StringReplace(Result, '%items%', FItems.GetHtml, []);
end;

procedure TEWDropDown.SetItems(const Value: TEWDropDownItemCollection);
begin
  FItems.Assign(Value);
  Changed;
end;

procedure TEWDropDown.SetOnItemClick(Value: TEWDropDownClickItemEvent);
begin
  FOnItemClick := Value;
end;

procedure TEWDropDown.SetSplitButton(const Value: Boolean);
begin
  if FSplitButton <> Value then
  begin
    FSplitButton := Value;
    Changed;
  end;
end;

{ TEWButtonGroup }



constructor TEWButtonGroup.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  FLayout := bgHorizontal;
end;

destructor TEWButtonGroup.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TEWButtonGroup.DoClick(AParams: TStrings);
var
  AIndex: integer;
begin
  inherited;
  AIndex := StrToIntDef(AParams.Values[C_INDEX], -1);
  if AIndex <> FItemIndex then
  begin
    FItemIndex := AIndex;
    Changed;
  end;
  if (AIndex >= 0) and (Assigned(FOnItemClick)) then
    FOnItemClick(Self, Items[AIndex], AIndex);

end;

procedure TEWButtonGroup.DoItemClick(ASender: TObject; AItem: string;
  AIndex: integer);
begin
  FItemIndex := AIndex;
  Changed;
  if FItemIndex <> AIndex then
  begin
    if Assigned(FOnItemClick) then
      FOnItemClick(ASender, AItem, AIndex);
  end;
end;

procedure TEWButtonGroup.GetEventListners(AListners: TStrings);
var
  ICount: integer;
begin
  inherited;
  for ICount := 0 to FItems.Count-1 do
    AddObjectEvent(Name+C_ITEM+ICount.ToString, C_CLICK, [C_INDEX+'='+ICount.ToString], AListners, '');
end;

function TEWButtonGroup.GenerateHtml: string;
var
  ICount: integer;
  AActive: string;
  AItem: string;
  AItemHtml: string;
begin
  inherited;
  Result := ReplaceTokens(C_HTML_BUTTON_GROUP);
  AItemHtml := '';
  for ICount := 0 to FItems.Count - 1 do
  begin
    AActive := '';
    if ICount = FItemIndex then
      AActive := C_ACTIVE;
    AItem := C_HTML_BUTTON_GROUP_ITEM;
    AItem := StringReplace(AItem, '%id%', Name+C_ITEM+ICount.ToString, []);
    AItem := StringReplace(AItem, '%class%', GetButtonTypeStr, []);
    AItem := StringReplace(AItem, '%active%', AActive, []);
    AItem := StringReplace(AItem, '%text%', FItems[ICount], []);
    AItemHtml := AItemHtml + AItem;
  end;
  Result := StringReplace(Result, '%items%', AItemHtml, []);

end;

function TEWButtonGroup.GetItemIndex: integer;
begin
  Result := FItemIndex;
end;

function TEWButtonGroup.GetItems: TStrings;
begin
  Result := FItems;
end;

function TEWButtonGroup.GetOnItemClick: TEWClickItemEvent;
begin
  Result := FOnItemClick;
end;

function TEWButtonGroup.ReplaceTokens(AHtml: string): string;
var
  ALayout: string;
begin
  Result := inherited ReplaceTokens(AHtml);
  ALayout := C_BTN_GROUP;
  if FLayout = bgVertical then ALayout := C_BTN_GROUP_VERTICAL;
  Result := StringReplace(Result, '%layout%', ALayout, []);
end;

procedure TEWButtonGroup.SetItemIndex(Value: integer);
begin
  FItemIndex := Value;
end;

procedure TEWButtonGroup.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TEWButtonGroup.SetLayout(const Value: TEWButtonGroupLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

procedure TEWButtonGroup.SetOnItemClick(Value: TEWClickItemEvent);
begin
  FOnItemClick := Value;
end;


{ TEWDropDownItem }

procedure TEWDropDownItem.Assign(Source: TPersistent);
begin
  inherited;
  Text := (Source as TEWDropDownItem).Text;
  Enabled := (Source as TEWDropDownItem).Enabled;
  Divider := (Source as TEWDropDownItem).Divider;
end;

procedure TEWDropDownItem.Changed;
var
  I: IEWBaseComponent;
begin
  if Supports(FButton, IEWBaseComponent, I) then
    i.Changed;
end;

constructor TEWDropDownItem.Create(Collection: TCollection);
begin
  inherited;
  FButton := TEWDropDownItemCollection(Collection).FButton;
  FEnabled := True;
  FDivider := False;
end;

function TEWDropDownItem.GetHtml: string;
var
  ADisabled: string;
begin
  ADisabled := '';
  if FEnabled = False then
    ADisabled := C_DISABLED;

  if FDivider then
    Result := C_HTML_BUTTON_DROPDOWN_DIVIDER
  else
  begin
    Result := C_HTML_BUTTON_DROPDOWN_ITEM;
    Result := StringReplace(Result, '%disabled%', ADisabled, []);
    Result := StringReplace(Result, '%id%', FButton.Name+C_ITEM+Index.ToString, []);
    Result := StringReplace(Result, '%text%', FText, []);
  end;
end;

procedure TEWDropDownItem.SetDivider(const Value: Boolean);
begin
  if FDivider <> Value then
  begin
    FText := '';
    FDivider := Value;
    Changed;
  end;
end;

procedure TEWDropDownItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TEWDropDownItem.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TEWDropDownItemCollection }

function TEWDropDownItemCollection.Add: TEWDropDownItem;
begin
    Result := TEWDropDownItem.Create(Self);
end;

constructor TEWDropDownItemCollection.Create(ADropDown: TEWDropDown);
begin
  inherited Create(TEWDropDownItem);
  FButton := ADropDown;
end;

function TEWDropDownItemCollection.GetHtml: string;
var
  ICount: integer;
  AItemsHtml: string;
begin
  AItemsHtml := '';
  for ICount := 0 to Count - 1 do
    AItemsHtml := AItemsHtml + Items[ICount].GetHtml;
  Result := StringReplace(C_HTML_BUTTON_DROPDOWN_MENU, '%items%', AItemsHtml, []);
end;

function TEWDropDownItemCollection.GetItem(Index: Integer): TEWDropDownItem;
begin
  Result := inherited Items[index] as TEWDropDownItem;
end;

function TEWDropDownItemCollection.Insert(Index: Integer): TEWDropDownItem;
begin
  Result := inherited insert( index ) as TEWDropDownItem;
end;

procedure TEWDropDownItemCollection.SetItem(Index: Integer;
  Value: TEWDropDownItem);
begin
  inherited SetItem(index, Value);
end;

end.

