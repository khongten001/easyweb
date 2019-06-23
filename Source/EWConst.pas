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

unit EWConst;

interface

uses Messages;

const
  C_SWEET_ALERT_URL = 'https://unpkg.com/sweetalert/dist/sweetalert.min.js';

  CR = #13#10;

  WM_BS_LOGTEXT = WM_USER + 1000;


  C_ACTION                    = 'action';
  C_ACTIVE                    = 'active';
  C_ALERT                     = 'alert("%text%");';
  C_ALERT_SWEEET_ALERT        = 'swal({title: "%title%",text: "%text%",icon: "%icon%",button: "%button%"});';
  C_APPLICATION_JSON          = 'application/json';
  C_ASYNC                     = 'async';
  C_BACKGROUND_COLOR          = 'background-color';
  C_BLUR                      = 'blur';
  C_CENTER                    = 'center';
  C_CHANGE                    = 'change';
  C_CHECKED                   = 'checked';
  C_CHECKBOX                  = 'checkbox';
  C_CLICK                     = 'click';
  C_CLICK_CELL                = 'clickcell';
  C_COLOR                     = 'color';
  C_CONFIRM                   = 'confirm';
  C_DATA                      = 'data';
  C_DATE                      = 'date';
  C_DATETIME_LOCAL            = 'datetime-local';
  C_DIALOG                    = 'dialog';
  C_EMAIL                     = 'email';
  C_EVENT                     = 'event';
  C_FAVICON                   = 'favicon';
  C_FOCUS                     = 'focus';
  C_FONT                      = 'font';
  C_HIDDEN                    = 'hidden';
  C_HTML                      = 'html';
  C_IMAGE_PNG                 = 'image/png';
  C_IMAGE_JPEG                = 'image/jpeg';
  C_IMAGE_GIF                 = 'image/gif';
  C_IMG_THUMBNAIL             = 'img-thumbnail';
  C_INDEX                     = 'index';
  C_ITEM                      = 'item';
  C_KEYDOWN                   = 'keydown';
  C_KEYUP                     = 'keyup';
  C_LEFT                      = 'left';
  C_MONTH                     = 'month';
  C_NAME                      = 'name';
  C_NUMBER                    = 'number';
  C_PASSWORD                  = 'password';
  C_PROMPT                    = 'prompt';
  C_RADIO                     = 'radio';
  C_RELOAD                    = 'reload';
  C_RIGHT                     = 'right';
  C_ROUNDED                   = 'rounded';
  C_ROUNDED_CIRCLE            = 'rounded-circle';
  C_SCRIPT                    = 'script';
  C_SEARCH                    = 'search';
  C_SELECTED                  = 'selected';
  C_SESSION                   = 'session';
  C_SERVER_STARTING           = 'Server starting on port %port%';
  C_SERVER_STARTED            = 'Started successfully.';
  C_SERVER_STOPPING           = 'Stopping server. Please wait...';
  C_SERVER_STOPPED            = 'Stopped successfully.';
  C_SUB_INDEX                 = 'sub-index';
  C_SUB_ITEM                  = 'sub-item';
  C_TEXT                      = 'text';
  C_TEXT_ALIGN                = 'text-align';
  C_TEXT_HTML                 = 'text/html';
  C_TEXT_JAVASCRIPT           = 'text/javascript';
  C_TIME                      = 'time';
  C_TRUE                      = 'true';
  C_TYPE                      = 'type';
  C_DISABLED                  = 'disabled';
  C_URL                       = 'url';
  C_VALUE                     = 'value';
  C_VERTICAL                  = 'vertical';

  C_BTN_OUTLINE               = 'btn-outline';
  C_BTN_GROUP                 = 'btn-group';
  C_BTN_GROUP_VERTICAL        = 'btn-group-vertical';
  C_CSS_CORNER_RADIUS         = 'corner-radius';
  C_CSS_DROPDOWN_TOGGLE_SPLIT = 'dropdown-toggle-split';
  C_CSS_PX                    = 'px';

  // html
  C_HTML_BADGE                =     '<span class="badge badge-%badgetype%">%text%</span>';
  C_HTML_BUTTON               =     '<button name="%name%" id="%id%" %style% class="%class%" type="button" >%text% %badge%</button>';

  C_HTML_BUTTON_DROPDOWN      =     '<div name="%name%" id="%id%" class="btn-group" %style%><button class="%class% '+
                                    'dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown" '+
                                    'aria-haspopup="true" aria-expanded="false">%text%</button>%items%</div>';
  C_HTML_BUTTON_DROPDOWN_MENU =     '<div class="dropdown-menu" aria-labelledby="dropdownMenuButton">%items%</div>';
  C_HTML_BUTTON_DROPDOWN_DIVIDER  = '<div class="dropdown-divider"></div>';
  C_HTML_BUTTON_DROPDOWN_ITEM     = '<a class="dropdown-item %disabled%" id="%id%" href="#">%text%</a>';
  C_HTML_BUTTON_GROUP             = '<div %style% id="%id%" style="width:100%;height:100%;" class="%layout%" role="group">%items%</div>';
  C_HTML_BUTTON_GROUP_ITEM        = '<button type="button" id="%id%" style="flex:1;box-shadow: none" class="%class% %active%">%text%</button>';
  C_HTML_CHECKBOX                 =     '<div id="%id%" %style%>'+
                                    '<input id="%name%Cbx" class="form-check-input" type="checkbox" %checked%>'+
                                    '<label id="%name%Lbl" class="form-check-label" for="%name%">%text%</label>'+
                                    '</div>';
  C_HTML_CHECKGROUP               = '<div id="%id%" %style%>%items%</div>';
  C_HTML_CHECKGROUP_ITEM          = '<div class="form-check"><input class="form-check-input" type="%type%" name="%name%Checkgroup" id="%id%" value="%index%" %checked% %disabled%>'+
                                    '<label class="form-check-label" for="%name%">%text%</label></div>';
  C_HTML_COMBOBOX                 = '<select class="form-control" id="%id%" %style%>%items%</select>';
  C_HTML_COMBOBOX_ITEM            = '<option %selected%>%text%</option>';

  C_HTML_DIALOG                   = '<div id="%id%"></div>';
  C_HTML_EDIT                     = '<input id="%id%" %style% type="%type%" class="form-control" placeholder="%placeholder%" %extratags% value="%text%" "></span>';

  //C_HTML_EDIT                     = '<div class="form-group" placeholder="%placeholder%" ><div class="input-group" placeholder="%placeholder%" ><span id="%id%" %style% type="%type%" class="form-control" placeholder="%placeholder%" %extratags% value="%text%" "></span></div></div>';

  C_HTML_IMAGE                    = '<img  id="%id%" %style% style="width:100%;height:auto;"  rel="prefetch" class= "%clsss" src="%src%">';
  C_HTML_LABEL                    = '<div id="%id%" %style%>%text%</div>';
  C_HTML_TABLE                    = '<div id="%id%" %style%><table class="table %striped% %theme% %hover%" style="width:100%"><thead>%header%</thead><tbody>%content%</tbody></table></div>';
  C_HTML_TEXT_AREA                = '<textarea id="%id%" %style% class="form-control" cols="80" rows="5">%text%</textarea>';


resourcestring
  SInvalidControlType   = 'EasyWeb forms will only work with EasyWeb controls or non-visual components.';
  SUseEasyWebCompsOnly  = 'You will find the EasyWeb controls under the EasyWeb heading in the Tool Pallete.';



implementation

end.
