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

unit EWTable;

interface

uses Classes, EWIntf, EWBase, VCL.Graphics, EWTypes;

type
  TEWTable = class;

  TEWTableTheme = (ttLight, ttDark);
  TEWTableCellClickEvent = procedure(Sender: TObject; ACol, ARow: integer) of object;

  TEWTableColumn = class(TCollectionItem)
  private
    FTable: TEWTable;
    FText: string;
    procedure SetText(const Value: string);
    procedure Changed;
  public
    procedure Assign(Source: TPersistent); override;
  published
    constructor Create(Collection: TCollection); override;
    property Text: string read FText write SetText;
  end;

  TEWTableColumnCollection = class(TCollection)
  private
    FTable: TEWTable;
  protected
    function GetItem(Index: Integer): TEWTableColumn;
    procedure SetItem(Index: Integer; Value: TEWTableColumn);
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(ATable: TEWTable);
    function Add: TEWTableColumn;
    function Insert( Index: Integer ): TEWTableColumn;
    property Items[index: Integer]: TEWTableColumn read GetItem write SetItem; default;
  end;

  TEWTable = class(TEWBaseObject, IEWTable)
  private
    FColumns: TEWTableColumnCollection;
    FRowCount: integer;
    FCells: array of array of string;
    FStriped: Boolean;
    FTheme: TEWTableTheme;
    FHover: Boolean;
    FOnClickCell: TEWTableCellClickEvent;
    procedure SetColumns(const Value: TEWTableColumnCollection);
    procedure SetRowCount(const Value: integer);
    procedure SetStriped(const Value: Boolean);
    procedure SetTheme(const Value: TEWTableTheme);
    function GetCell(ACol, ARow: integer): string;
    procedure SetCell(ACol, ARow: integer; const Value: string);
    procedure SetHover(const Value: Boolean);
  protected
    function GetScript: string; override;
    function ReplaceTokens(AHtml: string): string; override;
    procedure GetEventListners(AListners: TStrings); override;
    function GenerateHtml: string; override;
    procedure Paint; override;
    procedure DoEvent(AParams: TStrings); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Changed; override;
    procedure Clear;
    procedure AddRow; overload;
    procedure AddRow(ACells: array of string); overload;
    property Cells[ACol, ARow: integer]: string read GetCell write SetCell;
  published
    property Align;
    property Columns: TEWTableColumnCollection read FColumns write SetColumns;
    property Hover: Boolean read FHover write SetHover default False;
    property RowCount: integer read FRowCount write SetRowCount default 5;
    property Striped: Boolean read FStriped write SetStriped default False;
    property Theme: TEWTableTheme read FTheme write SetTheme default ttLight;
    property OnClickCell: TEWTableCellClickEvent read FOnClickCell write FOnClickCell;
  end;

implementation

uses Types, SysUtils, EWConst, Math;

{ TEWLabel }


procedure TEWTable.AddRow;
begin
  RowCount := RowCount+1;
end;

procedure TEWTable.AddRow(ACells: array of string);
var
  ICount: integer;
begin
  AddRow;
  for ICount := 0 to Min(Length(ACells), FColumns.Count-1) do
    Cells[ICount, FRowCount-1] := ACells[ICount];
end;

procedure TEWTable.Changed;
begin
  inherited;
  SetLength(FCells, FColumns.Count, FRowCount);
end;

procedure TEWTable.Clear;
begin
  RowCount := 0;
end;

constructor TEWTable.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TEWTableColumnCollection.Create(Self);
  FRowCount := 5;
  SetLength(FCells, FColumns.Count, FRowCount);
  FStriped := False;
end;

destructor TEWTable.Destroy;
begin
  FColumns.Free;
  inherited;
end;

procedure TEWTable.DoEvent(AParams: TStrings);
begin
  inherited;
  if AParams.Values[C_EVENT] = C_CLICK_CELL then
  begin
    if Assigned(FOnClickCell) then
      FOnClickCell(Self, StrToInt(AParams.Values['x']), StrToInt(AParams.Values['y']));
  end;
end;

function TEWTable.GenerateHtml: string;
var
  h, c: string;
  AStriped: string;
  AHover: string;
  ATheme: string;
  ACol, ARow: integer;
  AText: string;
begin
  inherited;
  AStriped := '';
  AHover := '';
  case FTheme of
    ttLight: ATheme := '';
    ttDark: ATheme := 'table-dark';
  end;
  if FStriped then
    AStriped := 'table-striped';
  if FHover then
    AHover := 'table-hover';

  h  := '';
  for ACol := 0 to FColumns.Count-1 do
    h := h + '<th scope="col">'+FColumns[ACol].Text+'</th>';

  h := '<tr>'+h+'</tr>';

  c := '';
  for ARow := 0 to FRowCount-1 do
  begin
    c := c + '<tr>';
    for ACol := 0 to FColumns.Count-1 do
    begin
      AText := FCells[ACol, ARow];
      //if AText = '' then
      //  AText := acol.ToString+','+arow.ToString;
      c := c + '<td>'+AText+'</td>';
    end;
    c := c + '</tr>';
  end;


  Result := ReplaceTokens(C_HTML_TABLE);
  Result := StringReplace(Result, '%header%', h, []);
  Result := StringReplace(Result, '%content%', c, []);
  Result := StringReplace(Result, '%striped%', AStriped, []);
  Result := StringReplace(Result, '%hover%', AHover, []);
  Result := StringReplace(Result, '%theme%', ATheme, []);



end;

function TEWTable.GetCell(ACol, ARow: integer): string;
begin
  Result := FCells[ACol, ARow];
end;

procedure TEWTable.GetEventListners(AListners: TStrings);
begin
  inherited;
  AListners.Add('function bind'+Name+'CellClick(){$("#'+Name+' td").click(function() '+
                '{ '+
                'var column_num = parseInt( $(this).index() ) ;'+
                'var row_num = parseInt( $(this).parent().index() );'+
                'eventCall(''clickcell'', '''', ''{"name":"'+Name+'","event":"clickcell","x":''+column_num+'',"y":''+row_num+''}''); '+
                '})'+
                '};');

  AListners.Add('$(document).ready('+
    'function() {bind'+Name+'CellClick(); }'+
  ');');

                (*  AListners.Add('$(document).ready(function(){$("#'+Name+' td").click(function() '+
                '{ '+
                'var column_num = parseInt( $(this).index() ) ;'+
                'var row_num = parseInt( $(this).parent().index() );'+
                'eventCall(''clickcell'', '''', ''{"name":"'+Name+'","event":"clickcell","x":''+column_num+'',"y":''+row_num+''}''); '+
                '})'+
                '});'); *)

end;

function TEWTable.GetScript: string;
begin
  Result := 'bind'+Name+'CellClick();';
end;

procedure TEWTable.Paint;
var
  r: TRect;
  AStr: string;
  AFormat: TTextFormat;
begin
  inherited;
  r := ClientRect;
  AFormat := [tfSingleLine];
  AStr := DesignTimeCaption;
  Canvas.TextRect(r, AStr, AFormat);
end;


function TEWTable.ReplaceTokens(AHtml: string): string;
begin
  Result := inherited ReplaceTokens(AHtml);
end;


procedure TEWTable.SetCell(ACol, ARow: integer; const Value: string);
begin
  if FCells[ACol, ARow] <> Value then
  begin
    FCells[ACol, ARow] := Value;
    Changed;
  end;
end;

procedure TEWTable.SetColumns(const Value: TEWTableColumnCollection);
begin
  FColumns.Assign(Value);
end;

procedure TEWTable.SetHover(const Value: Boolean);
begin
  if FHover <> Value then
  begin
    FHover := Value;
    Changed;
  end;
end;

procedure TEWTable.SetRowCount(const Value: integer);
begin
  if FRowCount <> Value then
  begin
    FRowCount := Value;
    SetLength(FCells, FColumns.Count, FRowCount);
    Changed;
  end;
end;

procedure TEWTable.SetStriped(const Value: Boolean);
begin
  if FStriped <> Value then
  begin
    FStriped := Value;
    Changed;
  end;
end;

procedure TEWTable.SetTheme(const Value: TEWTableTheme);
begin
  if FTheme <> Value then
  begin
    FTheme := Value;
    Changed;
  end;
end;

{ TEWTableColumn }

procedure TEWTableColumn.Assign(Source: TPersistent);
begin
  inherited;
  Text := (Source as TEWTableColumn).Text;
end;

procedure TEWTableColumn.Changed;
var
  I: IEWBaseComponent;
begin
  if Supports(FTable, IEWBaseComponent, I) then
    i.Changed;
end;

constructor TEWTableColumn.Create(Collection: TCollection);
begin
  inherited;
  FTable := TEWTableColumnCollection(Collection).FTable;
  FText := '';
end;

procedure TEWTableColumn.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TEWTableColumnCollection }

function TEWTableColumnCollection.Add: TEWTableColumn;
begin
  Result := TEWTableColumn.Create(Self);
end;

constructor TEWTableColumnCollection.Create(ATable: TEWTable);
begin
  inherited Create(TEWTableColumn);
  FTable := ATable;
end;

function TEWTableColumnCollection.GetItem(Index: Integer): TEWTableColumn;
begin
  Result := inherited Items[index] as TEWTableColumn;
end;

function TEWTableColumnCollection.Insert(Index: Integer): TEWTableColumn;
begin
  Result := inherited insert( index ) as TEWTableColumn;
end;

procedure TEWTableColumnCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  FTable.Changed;
end;

procedure TEWTableColumnCollection.SetItem(Index: Integer;
  Value: TEWTableColumn);
begin
  inherited SetItem(index, Value);
end;

end.
