{
Copyright (C) 2005  Remco Mulder

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

For source notes please refer to Notes.txt
For license terms please refer to GPL.txt.

These files should be stored in the root of the compression you 
received this source in.
}
unit ScriptRef;

// Shared by both the script compiler and the interpreter.  Contains framework
// for the script commands/constants, etc

interface

uses
  SysUtils,
  Classes;

type
  TCmdParam = class;
  TStringArray = array of string;
  TCmdAction = (caNone, caStop, caPause, caAuth);
  TScriptConstHandler = function(Indexes : TStringArray) : string;
  TScriptCmdHandler = function(Script : TObject; Params : array of TCmdParam) : TCmdAction;
  TParamKind = (pkValue, pkVar);
  EScriptError = class(Exception);

  // TCmdParam: Base class for all command parameters processed within a script.  These
  // parameters are identified during script compilation and stored in a list within the
  // script.  While the compiled script is interpreted these parameters are passed to the
  // script command handling functions within the ScriptCmd unit.
  TCmdParam = class(TObject)
  private
    FValue       : string;
    FIsTemporary : Boolean;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Value : string read FValue write FValue;
    property IsTemporary : Boolean read FIsTemporary write FIsTemporary;
  end;

  // TScriptCmd: A built in script command.  Script commands are defined in the ScriptCmd
  // unit.  Each command has its own internal parameter processing which updates script
  // variables/triggers to allow the scripting language to function.
  TScriptCmd = class(TObject)
  protected
    FName         : string;
    FMinParams,
    FMaxParams    : Integer;
    FParamKinds   : array of TParamKind;
    FonCmd        : TScriptCmdHandler;
    FDefParamKind : TParamKind;

    procedure SetParamKinds(NewParamKinds : Array of TParamKind);
    function GetParamKind(Index : Integer) : TParamKind;

  public
    property Name : string read FName write FName; // must be stored in uppercase
    property MinParams : Integer read FMinParams write FMinParams;
    property MaxParams : Integer read FMaxParams write FMaxParams;
    property DefParamKind : TParamKind read FDefParamKind write FDefParamKind;
    property onCmd : TScriptCmdHandler read FonCmd write FonCmd;

    property ParamKinds[Index : Integer] : TParamKind read GetParamKind;
  end;

  // TScriptSysConst: A 'system constant' within a script.  Technically similar to
  // a TScriptCmd although only returns an environment variable directly without
  // processing any parameters.
  TScriptSysConst = class(TObject)
  protected
    FName   : string;
    FonRead : TScriptConstHandler;

  public
    function Read(Indexes : TStringArray) : string;

    property onRead : TScriptConstHandler read FonRead write FonRead;
    property Name : string read FName write FName; // must be stored in uppercase
  end;

  // TScriptRef: An object that holds all the references to script consts/cmds
  TScriptRef = class(TObject)
  private
    CmdList,
    SysConstList : TList;

  protected
    function GetScriptCmd(Index : Integer) : TScriptCmd;
    function GetScriptSysConst(Index : Integer) : TScriptSysConst;

  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddCommand(Name : string; MinParams, MaxParams : Integer; onCmd : TScriptCmdHandler;
      ParamKinds : array of TParamKind; DefParamKind : TParamKind);
    procedure AddSysConstant(Name : string; onRead : TScriptConstHandler);

    function FindCmd(Name : string) : Integer;
    function FindSysConst(Name : string) : Integer;

    property Cmds[Index : Integer] : TScriptCmd read GetScriptCmd;
    property SysConsts[Index : Integer] : TScriptSysConst read GetScriptSysConst;
  end;

implementation

uses
  ScriptCmd;


// ***************************************************************
// TCmdParam implementation


constructor TCmdParam.Create;
begin
  inherited;

  // set defaults
  Value := '0';
  FIsTemporary := FALSE;
end;

destructor TCmdParam.Destroy;
begin
  Value := '';

  inherited;
end;


// ***************************************************************
// TScriptCmd implementation


procedure TScriptCmd.SetParamKinds(NewParamKinds : Array of TParamKind);
var
  I : Integer;
begin
  SetLength(FParamKinds, Length(NewParamKinds));

  for I := 0 to Length(NewParamKinds) - 1 do
    FParamKinds[I] := NewParamKinds[I];
end;

function TScriptCmd.GetParamKind(Index : Integer) : TParamKind;
begin
  if (Index >= Length(FParamKinds)) then
    // wasn't specified with command - just give the default
    Result := DefParamKind
  else
    Result := FParamKinds[Index];
end;


// ***************************************************************
// TScriptSysConst implementation


function TScriptSysConst.Read(Indexes : TStringArray) : string;
begin
  Result := onRead(Indexes);
end;


// ***************************************************************
// TScriptRef implementation


constructor TScriptRef.Create;
begin
  CmdList := TList.Create;
  SysConstList := TList.Create;

  // build up the command list
  BuildCommandList(Self);

  // build up system constant list
  BuildSysConstList(Self);
end;

destructor TScriptRef.Destroy;
begin
  // free up script commands
  while (CmdList.Count > 0) do
  begin
    TScriptCmd(CmdList[0]).Free;
    CmdList.Delete(0);
  end;

  // free up system constants
  while (SysConstList.Count > 0) do
  begin
    TScriptSysConst(SysConstList[0]).Free;
    SysConstList.Delete(0);
  end;

  CmdList.Free;
  SysConstList.Free;
end;

procedure TScriptRef.AddCommand(Name : string; MinParams, MaxParams : Integer;
  onCmd : TScriptCmdHandler; ParamKinds : array of TParamKind; DefParamKind : TParamKind);
var
  NewCmd : TScriptCmd;
begin
  // build new command and add it to the command list
  NewCmd := TScriptCmd.Create;
  NewCmd.Name := Name;
  NewCmd.MinParams := MinParams;
  NewCmd.MaxParams := MaxParams;
  NewCmd.onCmd := onCmd;
  NewCmd.DefParamKind := DefParamKind;
  NewCmd.SetParamKinds(ParamKinds);

  CmdList.Add(NewCmd);
end;

procedure TScriptRef.AddSysConstant(Name : string; onRead : TScriptConstHandler);
var
  NewConst : TScriptSysConst;
begin
  // build a new system constant and add it to the list
  NewConst := TScriptSysConst.Create;
  NewConst.Name := Name;
  NewConst.onRead := onRead;

  SysConstList.Add(NewConst);
end;

function TScriptRef.GetScriptCmd(Index : Integer) : TScriptCmd;
begin
  // retrieve a command from the command list
  Result := CmdList.Items[Index];
end;

function TScriptRef.GetScriptSysConst(Index : Integer) : TScriptSysConst;
begin
  // retrieve a system const from list
  Result := SysConstList.Items[Index];
end;

function TScriptRef.FindCmd(Name : string) : Integer;
var
  I : Integer;
begin
  Result := -1;
  Name := UpperCase(Name);

  if (CmdList.Count > 0) then
    for I := 0 to CmdList.Count - 1 do
      if (TScriptCmd(CmdList[I]).Name = Name) then
      begin
        Result := I;
        Exit;
      end;
end;

function TScriptRef.FindSysConst(Name : string) : Integer;
var
  I : Integer;
begin
  Result := -1;

  if (SysConstList.Count > 0) then
    for I := 0 to SysConstList.Count - 1 do
      if (TScriptCmd(SysConstList[I]).Name = Name) then
      begin
        Result := I;
        Exit;
      end;
end;

end.
