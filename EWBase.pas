unit EWBase;

interface

uses Messages, Classes, Controls, Forms, EWTypes, EWIntf, Types;

type
  TEWBaseComponent = class(TComponent, IEWBaseComponent)
  private
    FChanged: Boolean;
    function GetHasChanged: Boolean;
    function GetName: string;
  protected
    procedure GetGlobalVars(AStrings: TStrings); virtual;
    procedure Changed; virtual;
    function GetHtml: string; virtual;
    function GetScript: string; virtual;
    property HasChanged: Boolean read GetHasChanged;

  end;

  TewBaseObject = class(TCustomControl, IEWBaseComponent, IEWBaseVisualObject)
  private
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
    function GetScript: string; virtual;
    function BrowserSize: TSize;
  protected
    procedure GetEventListners(AListners: TStrings); virtual;
    procedure GetGlobalVars(AStrings: TStrings); virtual;

    procedure AddMouseEnterEvent(AEvents: TStrings);
    procedure AddMouseLeaveEvent(AEvents: TStrings);
    procedure AddClickEvent(AEvents: TStrings); virtual;
    procedure AddRightClickEvent(AEvents: TStrings);
    procedure AddClickItemEvent(AItemIndex: integer; AEvents: TStrings);
    procedure AddDblClickEvent(AEvents: TStrings);
    procedure AddEnterEvent(AEvents: TStrings);
    procedure AddExitEvent(AEvents: TStrings);
    procedure AddOnKeyDownEvent(AEvents: TStrings);
    procedure AddOnKeyPressEvent(AEvents: TStrings);
    procedure AddOnKeyUpEvent(AEvents: TStrings);

    procedure AddOnChangeEvent(AEvents: TStrings);

    procedure DoClick(AParams: TStrings); virtual;
    procedure DoRightClick(AParams: TStrings); virtual;
    procedure DoDblClick(AParams: TStrings); virtual;
    procedure DoMouseEnter(AParams: TStrings);  virtual;
    procedure DoMouseLeave(AParams: TStrings);  virtual;
    procedure DoOnChange(AParams: TStrings); virtual;

    procedure SetName(const Value: TComponentName); override;
    procedure VisibleChanging; override;
    function GetHtml: string; virtual;
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
    function SessionID: string;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Html: string read GetHtml;
    property HasChanged: Boolean read GetHasChanged write FChanged;
    function CssCommaText: string; virtual;
  published
    property Align;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
  end;

implementation

uses SysUtils, Graphics, EWServerControllerBase;

{ TEWBaseComponent }

procedure TEWBaseComponent.Changed;
begin
  FChanged := True;
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
  FChanged := False;
  Result := '';
end;

function TEWBaseComponent.GetName: string;
begin
  Result := Name;
end;

function TEWBaseComponent.GetScript: string;
begin
  Result := '';
end;

{ TBsBaseObject }

procedure TewBaseObject.AddClickEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''click'',''#'+Name+''',function(){ asyncEvent("click", "'+Name+'", -1); }); ');
end;

procedure TewBaseObject.AddRightClickEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''contextmenu'',''#'+Name+''',function(ev){ ev.preventDefault(); asyncEvent("rightclick", "'+Name+'", -1); }); ');
end;

procedure TewBaseObject.AddDblClickEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''dblclick'',''#'+Name+''',function(){ asyncEvent("dblclick", "'+Name+'", -1); }); ');
end;

procedure TewBaseObject.AddMouseEnterEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''mouseenter'',''#'+Name+''',function(){ asyncEvent("mouseenter", "'+Name+'", -1); }); ');
end;

procedure TewBaseObject.AddEnterEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''focus'',''#'+Name+''',function(){ asyncEvent("enter", "'+Name+'", -1); }); ');
end;

procedure TewBaseObject.AddExitEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''blur'',''#'+Name+''',function(){ asyncEvent("exit", "'+Name+'", -1); }); ');
end;

procedure TewBaseObject.AddMouseLeaveEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''mouseleave'',''#'+Name+''',function(){ asyncEvent("mouseleave", "'+Name+'", -1); }); ');
end;

procedure TewBaseObject.AddOnKeyDownEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''keydown'',''#'+Name+''',function(){ asyncEvent("keydown", "'+Name+'", event.keyCode); }); ');
end;

procedure TewBaseObject.AddOnKeyPressEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''input'',''#'+Name+''',function(){ asyncEvent("input", "'+Name+'", document.getElementById(''' + Name +''').value); }); ');
end;

procedure TewBaseObject.AddOnKeyUpEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''keyup'',''#'+Name+''',function(){ asyncEvent("keyup", "'+Name+'", event.keyCode); }); ');
end;

procedure TewBaseObject.AddOnChangeEvent(AEvents: TStrings);
begin
  AEvents.Add('$(document).on(''change'',''#'+Name+''',function(){ asyncEvent("change", "'+Name+'", document.getElementById(''' + Name +''').value); }); ');
end;

procedure TewBaseObject.AddClickItemEvent(AItemIndex: integer; AEvents: TStrings);
var
  n: string;
  i: string;
begin
  i := AItemIndex.ToString;
  n := Name+'-'+i;
  AEvents.Add('$(document).on(''click'',''#'+n+''',function(){ asyncEvent("clickitem", "'+n+'", '+i+'); }); ');
end;

function TewBaseObject.BrowserSize: TSize;
var
  c: TComponent;
  i: IewForm;
begin
  c := Self;
  while (c is TEWBaseForm) = False do
    c := c.Owner;
  if (c is TEWBaseForm) then
  begin
    Result.cx := (c as TEWBaseForm).ClientRect.Width;
    Result.cy := (c as TEWBaseForm).ClientRect.Height;
  end;
end;

procedure TewBaseObject.BuildCss(AProperties: TStrings);
begin
  //AProperties.Values['border'] := 'solid black 1px';
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
    AProperties.Values['right'] := (BrowserSize.cx - (Width+Left)).ToString+ 'px';
    AProperties.Values['height'] := IntToStr(Height) + 'px';
  end;

  if Align = alLeft then
  begin
    AProperties.Values['left'] := IntToStr(Left) + 'px';
    AProperties.Values['top'] := IntToStr(Top) + 'px';
    AProperties.Values['width'] := IntToStr(Width) + 'px';
    AProperties.Values['bottom'] := (BrowserSize.cy - (Height+Top)).ToString+ 'px';
  end;

  if Align = alRight then
  begin
    AProperties.Values['right'] := (BrowserSize.cx - (Width+Left)).ToString+ 'px';
    AProperties.Values['top'] := IntToStr(Top) + 'px';
    AProperties.Values['width'] := IntToStr(Width) + 'px';
    AProperties.Values['bottom'] := (BrowserSize.cy - (Height+Top)).ToString+ 'px';
  end;

  if Align = alBottom then
  begin
    AProperties.Values['bottom'] := '0px;';//(BrowserSize.cy - (Height+Top)).ToString+ 'px';
    AProperties.Values['left'] := IntToStr(left) + 'px';
    AProperties.Values['right'] := (BrowserSize.cx - (Width+Left)).ToString+ 'px';
    AProperties.Values['height'] := IntToStr(Height) + 'px';
  end;

  if Align = alClient then
  begin
    AProperties.Values['left'] := IntToStr(Left) + 'px';
    AProperties.Values['right'] := (BrowserSize.cx - (Width+Left)).ToString+ 'px';
    AProperties.Values['top'] := IntToStr(Top) + 'px';
    AProperties.Values['bottom'] := (BrowserSize.cy - (Height+Top)).ToString+ 'px';
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
  ControlStyle := ControlStyle + [csAcceptsControls];
  FCssProperties := TStringList.Create;
  FMouseOver := False;

end;

function TewBaseObject.CssCommaText: string;
var
  AStrings:TStrings;
  ICount: integer;
begin
  Result := '';
  AStrings := TStringList.Create;
  try
    BuildCss(AStrings);
    //Result := AStrings.CommaText;
    for ICount := 0 to AStrings.Count-1 do
    begin
      Result := Result + AStrings.Names[ICount]+': '+AStrings.ValueFromIndex[ICount];
      if ICount < AStrings.Count-1 then
        Result := Result + '; ';
    end;

  finally
    AStrings.Free;
  end;
end;

function TewBaseObject.DesignTimeCaption: string;
begin
  Result := Name;
end;

destructor TewBaseObject.Destroy;
begin
  FCssProperties.Free;
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

procedure TewBaseObject.DoMouseEnter(AParams: TStrings);
begin
  if FMouseOver then
    Exit;
  FMouseOver := True;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);

end;

procedure TewBaseObject.DoMouseLeave(AParams: TStrings);
begin
  if FMouseOver = False then
    Exit;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
  FMouseOver := False;
end;

procedure TewBaseObject.DoOnChange(AParams: TStrings);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
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
  FChanged := False;
  Result := '';
end;

function TewBaseObject.GetName: string;
begin
  Result := Name;
end;

function TewBaseObject.GetScript: string;
begin
  Result := '';
end;

procedure TewBaseObject.Paint;
var
  r: TRect;
begin
  if (csDesigning in ComponentState) then
  begin
    Canvas.Pen.Color := clGray;
    canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    r := ClientRect;
    Canvas.Rectangle(r);
  end;
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
begin
  inherited;
  Changed;
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
