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
// This unit contains the implementation for all script commands

unit
  ScriptCmd;

interface

uses
  ScriptRef;

procedure BuildSysConstList(ScriptRef : TScriptRef);
procedure BuildCommandList(ScriptRef : TScriptRef);

implementation

uses
  Global,
  Core,
  Classes,
  SysUtils,
  Menu,
  DataBase,
  Utility,
  MMSystem,
  Script,
  Ansi,
  TCP,
  INIFiles,
  ScriptCmp,
  FormScript;

const
  SCSectorParameterError = 'Sector parameter name cannot be longer than 10 characters';
  SCSectorParameterValueError = 'Sector parameter value cannot be longer than 40 characters';

function FormatFloatToStr(Value : Extended; Precision : Integer) : string;
begin
  Result := FloatToStrF(Value, ffFixed, 18, Precision);
end;

procedure ConvertToNumber(const S : string; var N : Integer);
begin
  try
    N := Round(StrToFloat(S));
  except
    raise EScriptError.Create('''' + S + ''' is not a number');
  end;
end;

procedure ConvertToFloat(const S : string; const Precision : Integer; var N : Extended);
begin
  try
    // ugly and slow code ... required for float rounding to specified precision
    N := StrToFloat(FormatFloatToStr(StrToFloat(S), Precision));
  except
    raise EScriptError.Create('''' + S + ''' is not a decimal number');
  end;
end;

procedure ConvertToBoolean(const S : string; var B : Boolean);
begin
  if (S = '0') then
    B := FALSE
  else if (S = '1') then
    B := TRUE
  else
    raise EScriptError.Create('Value must be either 0 or 1 (cannot be "' + S + '")');
end;

function ConvertBoolToString(const B : Boolean) : string;
begin
  if (B) then
    Result := '1'
  else
    Result := '0';
end;

procedure CheckSector(Index : Integer);
begin
  if (Index <= 0) or (Index > TWXDatabase.DBHeader.Sectors) then
    raise EScriptError.Create('Sector index out of bounds');
end;

// *****************************************************************************
//                         SCRIPT COMMAND IMPLEMENTATION
// *****************************************************************************

function CmdAdd(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  F1,
  F2 : Extended;
begin
  // CMD: add var <value>
  // add a value to a variable

  ConvertToFloat(Params[0].Value, TScript(Script).DecimalPrecision, F1);
  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, F2);
  Params[0].Value := FormatFloatToStr(F1 + F2, TScript(Script).DecimalPrecision);

  Result := caNone;
end;

function CmdAddMenu(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  LabelName : string;
begin
  // CMD: addMenu <parent> <name> <description> <hotkey> <reference> <prompt> <closeMenu>

  if (Length(Params[3].Value) <> 1) then
    raise EScriptError.Create('Bad menu hotkey');

  LabelName := Params[4].Value;

  if (LabelName <> '') then
    TScript(Script).Cmp.ExtendLabelName(LabelName, TScript(Script).ExecScript);

  TScript(Script).AddMenu(TWXMenu.AddCustomMenu(UpperCase(Params[0].Value),
    UpperCase(Params[1].Value), Params[2].Value, LabelName, Params[5].Value,
    UpCase(Params[3].Value[1]), (Params[6].Value = '1'), TScript(Script)));

  Result := caNone;
end;

function CmdAnd(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  B1,
  B2 : Boolean;
begin
  // CMD: and var <value>

  ConvertToBoolean(Params[0].Value, B1);
  ConvertToBoolean(Params[1].Value, B2);

  Params[0].Value := ConvertBoolTostring(B1 and B2);
  Result := caNone;
end;

function CmdBranch(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: branch <value> <label>
  // goto <label> if <value> <> 1

  if (Params[0].Value <> '1') then
    TScript(Script).GotoLabel(Params[1].Value);

  Result := caNone;
end;

function CmdClientMessage(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: ClientMessage <value>

  TWXServer.ClientMessage(Params[0].Value);
  Result := caNone;
end;

function CmdCloseMenu(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: closeMenu

  TWXMenu.CloseMenu(FALSE);

  Result := caNone;
end;

function CmdConnect(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: connect

  if not (TWXClient.Connected) then
    TWXClient.Connect;
    
  Result := caNone;
end;

function CmdCutText(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  V1,
  V2 : Integer;
begin
  // CMD: cutText <value> var <start> <length>

  ConvertToNumber(Params[2].Value, V1);
  ConvertToNumber(Params[3].Value, V2);

  if (V1 > Length(Params[0].Value)) then
    raise EScriptError.Create('CutText: Cannot cut from past end of text');

  Params[1].Value := Copy(Params[0].Value, V1, V2);
  Result := caNone;
end;

function CmdDelete(Script : TObject; Params : array of TCmdParam) : TCmdAction;
{$HINTS OFF} // display 'Value assigned to 'I' never used'
var
  F : File;
  I : Integer;
begin
  // CMD: delete <filename>

  AssignFile(F, Params[0].Value);

  {$I-}
  Erase(F);
  {$I+}

  I := IOResult;
  Result := caNone;
end;
{$HINTS ON}

function CmdDisconnect(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: disconnect

  if (TWXClient.Connected) then
    TWXClient.Disconnect;

  Result := caNone;
end;

function CmdDivide(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  F1,
  F2 : Extended;
begin
  // CMD: divide var <value>
  // divide variable by a value

  ConvertToFloat(Params[0].Value, TScript(Script).DecimalPrecision, F1);
  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, F2);

  if (F2 = 0) then
    raise EScriptError.Create('Division by zero');

  Params[0].Value := FormatFloatToStr(F1 / F2, TScript(Script).DecimalPrecision);

  Result := caNone;
end;

function CmdEcho(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  EchoText : string;
  I        : Integer;
begin
  // CMD: echo <values...>

  // string together the parameters and echo to all terms
  for I := 0 to Length(Params) - 1 do
    EchoText := EchoText + Params[I].Value;

  // #13 on its own will warp the terminal display - add a linefeed with it
  TWXServer.Broadcast(StringReplace(EchoText, #13, #13 + #10, [rfReplaceAll]));

  Result := caNone;
end;

function CmdFileExists(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: fileExists var <filename>

  if (FileExists(Params[1].Value)) then
    Params[0].Value := '1'
  else
    Params[0].Value := '0';

  Result := caNone;
end;

function CmdGetCharCode(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getCharCode <char> resultVar

  if (Length(Params[0].Value) <> 1) then
    raise EScriptError.Create('Bad character');

  Params[1].Value := IntToStr(Byte(Char(Params[0].Value[1])));

  Result := caNone;
end;

function CmdGetConsoleInput(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getConsoleInput var [singleKey?]

  TScript(Script).Locked := TRUE;
  TWXMenu.BeginScriptInput(TScript(Script), TVarParam(Params[0]), (Length(Params) = 2));
  Result := caPause;
end;

function CmdGetCourse(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Course   : TList;
  I,
  FromSect,
  ToSect   : Integer;
begin
  // CMD: getCourse varspec <fromSector> <toSector>

  ConvertToNumber(Params[1].Value, FromSect);
  ConvertToNumber(Params[2].Value, ToSect);

  CheckSector(FromSect);
  CheckSector(ToSect);

  Course := TWXDatabase.PlotWarpCourse(FromSect, ToSect);


  try
    Params[0].Value := IntToStr(Course.Count - 1);

    if (Course.Count > 0) then
      for I := Course.Count - 1 downto 0 do
        TScript(Script).SetVariable(TVarParam(Params[0]).Name, IntToStr(Word(Course.Items[I]^)), IntToStr(Course.Count - I));
  finally
    FreeList(Course, 2);
  end;

  Result := caNone;
end;

function CmdGetDate(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getDate var

  if (Length(Params) = 2) then
    Params[0].Value := FormatDateTime(Params[1].Value, Now)
  else
    Params[0].Value := DateToStr(Now);

  Result := caNone;
end;

function CmdGetDistance(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Course   : TList;
  FromSect,
  ToSect   : Integer;
begin
  // CMD: getDistance var <fromSector> <toSector>

  ConvertToNumber(Params[1].Value, FromSect);
  ConvertToNumber(Params[2].Value, ToSect);

  CheckSector(FromSect);
  CheckSector(ToSect);

  Course := TWXDatabase.PlotWarpCourse(FromSect, ToSect);

  try
    Params[0].Value := IntToStr(Course.Count - 1);
  finally
    FreeList(Course, 2);
  end;

  Result := caNone;
end;

function CmdGetInput(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getInput var <prompt>

  TWXServer.Broadcast(endl + ANSI_15 + Params[1].Value + endl);
  TScript(Script).Locked := TRUE;
  TWXMenu.BeginScriptInput(TScript(Script), TVarParam(Params[0]), FALSE);
  Result := caPause;
end;

function CmdGetLength(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getLength <text> var

  Params[1].Value := IntToStr(Length(Params[0].Value));
  Result := caNone;
end;

function CmdGetMenuValue(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getMenuValue <menuName> var

  try
    Params[1].Value := TWXMenu.GetMenuByName(UpperCase(Params[0].Value)).Value;
  except
    on E : Exception do
      raise EScriptError.Create(E.Message);
  end;

  Result := caNone;
end;

function CmdGetOutText(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getOutText var

  Params[0].Value := TScript(Script).OutText;
  Result := caNone;
end;

function CmdGetRnd(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  RndMin,
  RndMax : Integer;
begin
  // CMD: getRnd var <lowestValue> <highestValue>

  ConvertToNumber(Params[1].Value, RndMin);
  ConvertToNumber(Params[2].Value, RndMax);
  Params[0].Value := IntToStr(Trunc(Random(RndMax + 1 - RndMin) + RndMin));
  Result := caNone;
end;

function CmdGetSector(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  I,
  Index   : Integer;
  S       : TSector;
  Items   : TList;
  VarName : string;
begin
  // CMD: getSector <index> var

  ConvertToNumber(Params[0].Value, Index);

  CheckSector(Index);

  S := TWXDatabase.LoadSector(Index);
  VarName := TVarParam(Params[1]).Name;
  Result := caNone;

  with (Script as TScript) do
  begin
    if (S.Explored = etNo) then
      SetVariable(VarName + '.EXPLORED', 'NO', '')
    else if (S.Explored = etCalc) then
      SetVariable(VarName + '.EXPLORED', 'CALC', '')
    else if (S.Explored = etDensity) then
      SetVariable(VarName + '.EXPLORED', 'DENSITY', '')
    else if (S.Explored = etHolo) then
      SetVariable(VarName + '.EXPLORED', 'YES', '');

    SetVariable(VarName + '.INDEX', IntToStr(Index), '');
    SetVariable(VarName + '.BEACON', S.Beacon, '');
    SetVariable(VarName + '.CONSTELLATION', S.Constellation, '');
    SetVariable(VarName + '.ARMIDMINES.QUANTITY', IntToStr(S.Mines_Armid.Quantity), '');
    SetVariable(VarName + '.LIMPETMINES.QUANTITY', IntToStr(S.Mines_Limpet.Quantity), '');
    SetVariable(VarName + '.ARMIDMINES.OWNER', S.Mines_Armid.Owner, '');
    SetVariable(VarName + '.LIMPETMINES.OWNER', S.Mines_Limpet.Owner, '');
    SetVariable(VarName + '.FIGS.QUANTITY', IntToStr(S.Figs.Quantity), '');
    SetVariable(VarName + '.FIGS.OWNER', S.Figs.Owner, '');
    SetVariable(VarName + '.WARPS', IntToStr(S.Warps), '');
    SetVariable(VarName + '.DENSITY', IntToStr(S.Density), '');
    SetVariable(VarName + '.NAVHAZ', IntToStr(S.NavHaz), '');

    for I := 1 to 6 do
      SetVariable(VarName + '.WARP', IntToStr(S.Warp[I]), IntToStr(I));

    SetVariable(VarName + '.UPDATED', DateToStr(S.Update) + ' ' + TimeToStr(S.Update), '');
    SetVariable(VarName + '.PORT.NAME', S.SPort.Name, '');

    if (S.Figs.FigType = ftToll) then
      SetVariable(VarName + '.FIGS.TYPE', 'TOLL', '')
    else if (S.Figs.FigType = ftDefensive) then
      SetVariable(VarName + '.FIGS.TYPE', 'DEFENSIVE', '')
    else
      SetVariable(VarName + '.FIGS.TYPE', 'OFFENSIVE', '');

    if (S.Anomoly) then
      SetVariable(VarName + '.ANOMOLY', 'YES', '')
    else
      SetVariable(VarName + '.ANOMOLY', 'NO', '');

    if (S.SPort.Name = '') then
    begin
      SetVariable(VarName + '.PORT.CLASS', '0', '');
      SetVariable(VarName + '.PORT.EXISTS', '0', '');
    end
    else
    begin
      SetVariable(VarName + '.PORT.CLASS', IntToStr(S.SPort.ClassIndex), '');
      SetVariable(VarName + '.PORT.EXISTS', '1', '');
      SetVariable(VarName + '.PORT.BUILDTIME', IntToStr(S.SPort.BuildTime), '');
      SetVariable(VarName + '.PORT.PERC_ORE', IntToStr(S.SPort.ProductPercent[ptFuelOre]), '');
      SetVariable(VarName + '.PORT.PERC_ORG', IntToStr(S.SPort.ProductPercent[ptOrganics]), '');
      SetVariable(VarName + '.PORT.PERC_EQUIP', IntToStr(S.SPort.ProductPercent[ptEquipment]), '');
      SetVariable(VarName + '.PORT.ORE', IntToStr(S.SPort.ProductAmount[ptFuelOre]), '');
      SetVariable(VarName + '.PORT.ORG', IntToStr(S.SPort.ProductAmount[ptOrganics]), '');
      SetVariable(VarName + '.PORT.EQUIP', IntToStr(S.SPort.ProductAmount[ptEquipment]), '');
      SetVariable(VarName + '.PORT.UPDATED', DateToStr(S.SPort.Update) + ' ' + TimeToStr(S.SPort.Update), '');

      if (S.SPort.BuyProduct[ptFuelOre]) then
        SetVariable(VarName + '.PORT.BUY_ORE', 'YES', '')
      else
        SetVariable(VarName + '.PORT.BUY_ORE', 'NO', '');

      if (S.SPort.BuyProduct[ptOrganics]) then
        SetVariable(VarName + '.PORT.BUY_ORG', 'YES', '')
      else
        SetVariable(VarName + '.PORT.BUY_ORG', 'NO', '');

      if (S.SPort.BuyProduct[ptEquipment]) then
        SetVariable(VarName + '.PORT.BUY_EQUIP', 'YES', '')
      else
        SetVariable(VarName + '.PORT.BUY_EQUIP', 'NO', '');
    end;

    // set planet variables
    Items := TWXDatabase.GetSectorItems(itPlanet, S);
    SetVariable(VarName + '.PLANETS', IntToStr(Items.Count), '');
    I := 0;

    while (Items.Count > 0) do
    begin
      Inc(I);
      SetVariable(VarName + '.PLANET', TPlanet(Items[0]^).Name, IntToStr(I));
      FreeMem(Items[0]);
      Items.Delete(0);
    end;

    Items.Free;

    // set trader variables
    Items := TWXDatabase.GetSectorItems(itTrader, S);
    SetVariable(VarName + '.TRADERS', IntToStr(Items.Count), '');
    I := 0;

    while (Items.Count > 0) do
    begin
      Inc(I);
      SetVariable(VarName + '.TRADER.NAME', TTrader(Items[0]^).Name, IntToStr(I));
      SetVariable(VarName + '.TRADER.SHIP', TTrader(Items[0]^).ShipType, IntToStr(I));
      SetVariable(VarName + '.TRADER.SHIPNAME', TTrader(Items[0]^).ShipName, IntToStr(I));
      SetVariable(VarName + '.TRADER.FIGS', IntToStr(TTrader(Items[0]^).Figs), IntToStr(I));
      FreeMem(Items.Items[0]);
      Items.Delete(0);
    end;

    Items.Free;

    // set ship variables
    Items := TWXDatabase.GetSectorItems(itShip, S);
    SetVariable(VarName + '.SHIPS', IntToStr(Items.Count), '');
    I := 0;

    while (Items.Count > 0) do
    begin
      Inc(I);
      SetVariable(VarName + '.SHIP.NAME', TShip(Items[0]^).Name, IntToStr(I));
      SetVariable(VarName + '.SHIP.SHIP', TShip(Items[0]^).ShipType, IntToStr(I));
      SetVariable(VarName + '.SHIP.OWNER', TShip(Items[0]^).Owner, IntToStr(I));
      SetVariable(VarName + '.SHIP.FIGS', IntToStr(TTrader(Items[0]^).Figs), IntToStr(I));
      FreeMem(Items.Items[0]);
      Items.Delete(0);
    end;

    Items.Free;

    // set backdoors
    if (S.Explored <> etNo) then
    begin
      Items := TWXDatabase.GetBackDoors(S, Index);
      I := 0;

      while (Items.Count > 0) do
      begin
        Inc(I);
        SetVariable(VarName + '.BACKDOOR', IntToStr(Word(Items[0]^)), IntToStr(I));
        FreeMem(Items[0]);
        Items.Delete(0);
      end;

      Items.Free;
    end;
  end;
end;

function CmdGetSectorParameter(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Index: Integer;
begin
  // CMD: getSectorParameter <sectorIndex> <parameterName> var

  ConvertToNumber(Params[0].Value, Index);
  CheckSector(Index);

  if (Length(Params[1].Value) > 10) then
    raise EScriptError.Create(SCSectorParameterError);

  if (Length(Params[2].Value) > 40) then
    raise EScriptError.Create(SCSectorParameterValueError);

  Params[2].Value := TWXDatabase.GetSectorVar(Index, Params[1].Value);
  Result := caNone;
end;

function CmdGetText(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  S,
  Line,
  StartStr,
  EndStr   : string;
  StartPos,
  EndPos   : Integer;
begin
  // CMD: getText <line> var <startValue> <endValue>

  Line := Params[0].Value;
  StartStr := Params[2].Value;
  EndStr := Params[3].Value;

  if (StartStr = '') then
    StartPos := 1
  else
    StartPos := Pos(StartStr, Line);

  if (EndStr = '') then
    EndPos := Length(Line) + 1
  else
    EndPos := Pos(EndStr, Line);

  if (EndPos > 0) then
  begin
    Inc(StartPos, Length(StartStr));
    S := Copy(Line, StartPos, EndPos - StartPos);

    Params[1].Value := S;
  end
  else
    Params[1].Value := '';

  Result := caNone;
end;

function CmdGetTime(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getTime var [<format>]

  if (Length(Params) = 2) then
    Params[0].Value := FormatDateTime(Params[1].Value, Now)
  else
    Params[0].Value := TimeToStr(Now);

  Result := caNone;
end;

function CmdGetWord(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  I : Integer;
begin
  // CMD: getWord <line> var <index> <default>

  ConvertToNumber(Params[2].Value, I);
  Params[1].Value := GetParameter(Params[0].Value, I);

  if (Params[1].Value = '') then
  begin
    if (Length(Params) > 3) then
      Params[1].Value := Params[3].Value
    else
      Params[1].Value := '0';
  end;

  Result := caNone;
end;

function CmdGetWordPos(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: getWordPos <text> storageVar <subString>

  Params[1].Value := IntToStr(Pos(Params[2].Value, Params[0].Value));
  Result := caNone;
end;

function CmdGosub(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: gosub <label>

  TScript(Script).Gosub(Params[0].Value);
  Result := caNone;
end;

function CmdGoto(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: goto <label>

  TScript(Script).GotoLabel(Params[0].Value);
  Result := caNone;
end;

function CmdHalt(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: halt

  Result := caStop;
end;

function CmdIsEqual(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  F1,
  F2: Extended;
begin
  // CMD: isEqual var <value1> <value2>
  // var = 1 if <value1> = <value2> else var = 0

  Result := caNone;

  if (TScript(Script).DecimalPrecision > 0) then
  begin
    // attempt floating point comparison
    if (TextToFloat(PChar(Params[1].Value), F1, fvExtended) and TextToFloat(PChar(Params[2].Value), F2, fvExtended)) then
    begin
      // need to round it too
      ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, F1);
      ConvertToFloat(Params[2].Value, TScript(Script).DecimalPrecision, F2);

      if (F1 = F2) then
        Params[0].Value := '1'
      else
        Params[0].Value := '0';

      Exit;
    end;
  end;

  if (Params[1].Value = Params[2].Value) then
    Params[0].Value := '1'
  else
    Params[0].Value := '0';
end;

function CmdIsGreater(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  V1,
  V2 : Extended;
begin
  // CMD: isGreater var <value1> <value2>
  // var = 1 if <value1> > <value2> else var = 0

  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, V1);
  ConvertToFloat(Params[2].Value, TScript(Script).DecimalPrecision, V2);

  if (V1 > V2) then
    Params[0].Value := '1'
  else
    Params[0].Value := '0';

  Result := caNone;
end;

function CmdIsGreaterEqual(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  V1,
  V2 : Extended;
begin
  // CMD: isGreaterEqual var <value1> <value2>
  // var = 1 if <value1> >= <value2> else var = 0

  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, V1);
  ConvertToFloat(Params[2].Value, TScript(Script).DecimalPrecision, V2);

  if (V1 >= V2) then
    Params[0].Value := '1'
  else
    Params[0].Value := '0';

  Result := caNone;
end;

function CmdIsLesser(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  V1,
  V2 : Extended;
begin
  // CMD: isLesser var <value1> <value2>
  // var = 1 if <value1> < <value2> else var = 0

  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, V1);
  ConvertToFloat(Params[2].Value, TScript(Script).DecimalPrecision, V2);

  if (V1 < V2) then
    Params[0].Value := '1'
  else
    Params[0].Value := '0';

  Result := caNone;
end;

function CmdIsLesserEqual(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  V1,
  V2 : Extended;
begin
  // CMD: isLesserEqual var <value1> <value2>
  // var = 1 if <value1> <= <value2> else var = 0

  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, V1);
  ConvertToFloat(Params[2].Value, TScript(Script).DecimalPrecision, V2);

  if (V1 <= V2) then
    Params[0].Value := '1'
  else
    Params[0].Value := '0';

  Result := caNone;
end;

function CmdIsNotEqual(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  F1,
  F2: Extended;
begin
  // CMD: isNotEqual var <value1> <value2>
  // var = 1 if <value1> <> <value2> else var = 0

  Result := caNone;

  if (TScript(Script).DecimalPrecision > 0) then
  begin
    // attempt floating point comparison
    if (TextToFloat(PChar(Params[1].Value), F1, fvExtended) and TextToFloat(PChar(Params[2].Value), F2, fvExtended)) then
    begin
      // need to round it too
      ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, F1);
      ConvertToFloat(Params[2].Value, TScript(Script).DecimalPrecision, F2);

      if (F1 = F2) then
        Params[0].Value := '0'
      else
        Params[0].Value := '1';

      Exit;
    end;
  end;

  if (Params[1].Value = Params[2].Value) then
    Params[0].Value := '0'
  else
    Params[0].Value := '1';
end;

function CmdIsNumber(Script : TObject; Params : array of TCmdParam) : TCmdAction;
{$HINTS OFF} // Disable 'Value assigned to I is never used'
var
  I,
  E : Integer;
begin
  // CMD: isNumber storageVar <value>

  Val(Params[1].Value, I, E);

  if (E = 0) then
    Params[0].Value := '1'
  else
    Params[0].Value := '0';

  Result := caNone;
end;
{$HINTS ON}

function CmdKillAllTriggers(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: killAllTriggers

  TScript(Script).KillAllTriggers;
  Result := caNone;
end;

function CmdKillTrigger(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: killTrigger <name>

  TScript(Script).KillTrigger(Params[0].Value);
  Result := caNone;
end;

function CmdKillWindow(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Window : TScriptWindow;
begin
  // CMD: killWindow <windowName>

  Window := TScript(Script).FindWindow(Params[0].Value);
  TScript(Script).RemoveWindow(Window);
  Window.Free;

  Result := caNone;
end;

function CmdLoad(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: load <scriptName>

  TWXInterpreter.Load(FetchScript(Params[0].Value, FALSE), FALSE);
  Result := caNone;
end;

function CmdLoadVar(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  INI : TINIFile;
begin
  // CMD: loadVar var

  INI := TINIFile.Create(TScript(Script).ProgramDir + '\' + StripFileExtension(TWXDatabase.DatabaseName) + '.cfg');

  try
    Params[0].Value := INI.ReadString('Variables', TVarParam(Params[0]).Name, '0');
  finally
    INI.Free;
  end;

  Result := caNone;
end;

function CmdLogging(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: logging <value>

  TScript(Script).LogData := (UpperCase(Params[0].Value) = 'ON') or (Params[0].Value = '1');

  Result := caNone;
end;

function CmdLowerCase(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: lowerCase var

  Params[0].Value := LowerCase(Params[0].Value);
  Result := caNone;
end;

function CmdMergeText(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: mergeText <value1> <value2> var
  // Concatenate two values and store the result

  Params[2].Value := Params[0].Value + Params[1].Value;

  Result := caNone;
end;

function CmdMultiply(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  F1,
  F2 : Extended;
begin
  // CMD: multiply var <value>
  // multiply a variable by a value

  ConvertToFloat(Params[0].Value, TScript(Script).DecimalPrecision, F1);
  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, F2);
  Params[0].Value := FormatFloatToStr(F1 * F2, TScript(Script).DecimalPrecision);

  Result := caNone;
end;

function CmdOpenMenu(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: openMenu <name> [<pause>]

  try
    TWXMenu.OpenMenu(UpperCase(Params[0].Value), 0);
  except
    on E : Exception do
      raise EScriptError.Create(E.Message);
  end;

  if (Length(Params) > 1) and (Params[1].Value = '0') then
    Result := caNone
  else
    Result := caPause;
end;

function CmdOr(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  B1,
  B2 : Boolean;
begin
  // CMD: or var <value>

  ConvertToBoolean(Params[0].Value, B1);
  ConvertToBoolean(Params[1].Value, B2);

  Params[0].Value := ConvertBoolToString(B1 or B2);
  Result := caNone;
end;

function CmdPause(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: pause

  Result := caPause;
end;

function CmdProcessIn(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: processIn processType <text>

  if (Params[0].Value = '1') then
  begin
    // process globally for all scripts
    TScript(Script).Controller.TextEvent(Params[1].Value, TRUE);
    TScript(Script).Controller.TextLineEvent(Params[1].Value, TRUE);
  end
  else
  begin
    // process locally only
    TScript(Script).TextEvent(Params[1].Value, TRUE);
    TScript(Script).TextLineEvent(Params[1].Value, TRUE);
  end;

  Result := caNone;
end;

function CmdProcessOut(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: processOut <text>

  if not (TWXInterpreter.TextOutEvent(Params[0].Value, TScript(Script))) then
    TWXClient.Send(Params[0].Value);

  Result := caNone;
end;

function CmdRead(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  InputFile : TextFile;
  Line,
  I,
  E         : Integer;
  Value     : string;
begin
  // CMD: read <file> storageVar <line>

  ConvertToNumber(Params[2].Value, Line);

  // Read line from file
  {$I-}
  Assign(InputFile, Params[0].Value);
  Reset(InputFile);

  if (IOResult <> 0) then
    raise EScriptError.Create('File ''' + Params[0].Value + ''' not found')
  else
  begin
    I := 1;
    Value := '';
    E := 0;

    while (I <= Line) and (E = 0) and not (Eof(InputFile)) do
    begin
      ReadLn(InputFile, Value);
      E := IOResult;
      Inc(I);
    end;

    if (E <> 0) then
    begin
      CloseFile(InputFile);
      raise EScriptError.Create('Unable to read from file ''' + Params[0].Value + '''');
    end;

    if (I - 1 < Line) then
      Value := 'EOF';

    Params[1].Value := Value;
  end;

  CloseFile(InputFile);
  {$I+}

  Result := caNone;
end;

function CmdRename(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: rename <oldfile> <newfile>

  if not (RenameFile(Params[0].Value, Params[1].Value)) then
    EScriptError.Create('Cannot rename file ''' + Params[0].Value + '''');

  Result := caNone;
end;

function CmdReplaceText(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: replaceText var <oldText> <newText>

  Params[0].Value := StringReplace(Params[0].Value, Params[1].Value, Params[2].Value, [rfReplaceAll]);

  Result := caNone;
end;

function CmdReqRecording(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: reqRecording

  if (TWXDatabase.Recording) then
    Result := caNone
  else
  begin
    TWXServer.ClientMessage('This script requires recording to be enabled');
    Result := caStop;
  end;
end;

function CmdReturn(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: return

  TScript(Script).Return;
  Result := caNone;
end;

function CmdRound(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Value     : Extended;
  Precision : Integer;
begin
  // CMD: round var <precision>

  ConvertToFloat(Params[0].Value, TScript(Script).DecimalPrecision, Value);

  if (Length(Params) < 2) then
    Precision := 0
  else
    ConvertToNumber(Params[1].Value, Precision);

  Params[0].Value := FormatFloatToStr(Value, Precision);

  Result := caNone;
end;

function CmdSaveVar(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  INI : TINIFile;
begin
  // CMD: saveVar var

  INI := TINIFile.Create(TScript(Script).ProgramDir + '\' + StripFileExtension(TWXDatabase.DatabaseName) + '.cfg');

  try
    INI.WriteString('Variables', TVarParam(Params[0]).Name, Params[0].Value);
  finally
    INI.Free;
  end;

  Result := caNone;
end;

function CmdSend(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  SendText : string;
  I        : Integer;
begin
  // CMD: send <values...>

  // string together the parameters and echo to all terms
  for I := 0 to Length(Params) - 1 do
    SendText := SendText + Params[I].Value;

  TWXClient.Send(SendText);
  Result := caNone;
end;

function CmdSetArray(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  I,
  ValInt     : Integer;
  Dimensions : array of Integer;
begin
  // CMD: setArray var <dimensions...>

  SetLength(Dimensions, Length(Params) - 1);

  for I := 1 to Length(Params) - 1 do
  begin
    ConvertToNumber(Params[I].Value, ValInt);
    Dimensions[I - 1] := ValInt;
  end;

  TVarParam(Params[0]).SetArray(Dimensions);
  Result := caNone;
end;

function CmdSetDelayTrigger(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Value : Integer;
begin
  // CMD: setDelayTrigger <name> <label> <tics>

  ConvertToNumber(Params[2].Value, Value);
  TScript(Script).SetDelayTrigger(Params[0].Value, Params[1].Value, Value);
  Result := caNone;
end;

function CmdSetEventTrigger(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Param : string;
begin
  // CMD: setEventTrigger <name> <label> <event> [<parameter>]

  if (Length(Params) < 4) then
    Param := ''
  else
    Param := Params[3].Value;

  TScript(Script).SetEventTrigger(Params[0].Value, Params[1].Value, Params[2].Value, Param);
  Result := caNone;
end;

function CmdSetMenuHelp(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: setMenuHelp <menuName> <helpText>

  try
    TWXMenu.GetMenuByName(UpperCase(Params[0].Value)).Help := StringReplace(Params[1].Value, #13, endl, [rfReplaceAll]);
  except
    on E : Exception do
      raise EScriptError.Create(E.Message);
  end;

  Result := caNone;
end;

function CmdSetMenuValue(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: setMenuValue <menuName> <value>

  try
    TWXMenu.GetMenuByName(UpperCase(Params[0].Value)).Value := Params[1].Value;
  except
    on E : Exception do
      raise EScriptError.Create(E.Message);
  end;

  Result := caNone;
end;

function CmdSetMenuOptions(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  MenuItem : TTWXMenuItem;
begin
  // CMD: setMenuOptions <menuName> <Q> <?> <+>

  try
    MenuItem := TWXMenu.GetMenuByName(UpperCase(Params[0].Value));
  except
    on E : Exception do
      raise EScriptError.Create(E.Message);
  end;

  MenuItem.SetOptions((Params[1].Value = '1'), (Params[2].Value = '1'), (Params[3].Value = '1'));

  Result := caNone;
end;

function CmdSetPrecision(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  V1 : Integer;
begin
  // CMD: setPrecision <value>

  ConvertToNumber(Params[0].Value, V1);
  TScript(Script).DecimalPrecision := V1;

  Result := caNone;
end;

function CmdSetProgVar(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: setProgVar <varName> <value>

  Result := caNone;
end;

function CmdSetSectorParameter(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Index: Integer;
begin
  // CMD: setSectorParameter <sectorIndex> <parameterName> <value>

  ConvertToNumber(Params[0].Value, Index);
  CheckSector(Index);

  if (Length(Params[1].Value) > 10) then
    raise EScriptError.Create(SCSectorParameterError);

  if (Length(Params[2].Value) > 10) then
    raise EScriptError.Create(SCSectorParameterValueError);

  TWXDatabase.SetSectorVar(Index, Params[1].Value, Params[2].Value);
  Result := caNone;
end;

function CmdSetTextLineTrigger(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Value : string;
begin
  // CMD: setTextLineTrigger <name> <label> [<value>]

  if (Length(Params) < 3) then
    Value := ''
  else
    Value := Params[2].Value;

  TScript(Script).SetTextLineTrigger(Params[0].Value, Params[1].Value, Value);
  Result := caNone;
end;

function CmdSetTextOutTrigger(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Value : string;
begin
  // CMD: setTextOutTrigger <name> <label> [<value>]

  if (Length(Params) < 3) then
    Value := ''
  else
    Value := Params[2].Value;

  TScript(Script).SetTextOutTrigger(Params[0].Value, Params[1].Value, Value);
  Result := caNone;
end;

function CmdSetTextTrigger(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Value : string;
begin
  // CMD: setTextTrigger <name> <label> [<value>]

  if (Length(Params) < 3) then
    Value := ''
  else
    Value := Params[2].Value;

  TScript(Script).SetTextTrigger(Params[0].Value, Params[1].Value, Value);
  Result := caNone;
end;

function CmdSetVar(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: setVar var <value>

  Params[0].Value := Params[1].Value;
  Result := caNone;
end;

function CmdSetWindowContents(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: setWindowContents <windowName> <value>

  TScriptWindow(TScript(Script).FindWindow(Params[0].Value)).TextContent := Params[1].Value;

  Result := caNone;
end;

function CmdSound(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: sound <filename>

  PlaySound(PChar(Params[0].Value), 0, SND_NODEFAULT);
  Result := caNone;
end;

function CmdStop(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  I    : Integer;
  Name : string;
begin
  // CMD: stop <scriptName>
  Name := UpperCase(ShortFilename(StripFileExtension(Params[0].Value)));
  Result := caNone;

  for I := 0 to TWXInterpreter.Count - 1 do
    if (UpperCase(ShortFilename(StripFileExtension(TWXInterpreter.Scripts[I].Cmp.ScriptFile))) = Name) then
    begin
      if (TWXInterpreter.Scripts[I] = Script) then
        Result := caStop
      else
        TWXInterpreter.Stop(I);

      Break;
    end;
end;

function CmdStripText(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  I       : Integer;
  RemText,
  Value   : string;
begin
  // CMD: stripText var <value>

  Value := Params[0].Value;
  RemText := Params[1].Value;
  
  I := 1;
  while (I <= Length(Value)) do
  begin
    if (Copy(Value, I, Length(RemText)) = RemText) then
    begin
      Delete(Value, I, Length(RemText));
      I := 0;
    end;

    Inc(I);
  end;

  Params[0].Value := Value;
  Result := caNone;
end;

function CmdSubtract(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  F1,
  F2 : Extended;
begin
  // CMD: subtract var <value>
  // subtract a value from a variable

  ConvertToFloat(Params[0].Value, TScript(Script).DecimalPrecision, F1);
  ConvertToFloat(Params[1].Value, TScript(Script).DecimalPrecision, F2);
  Params[0].Value := FormatFloatToStr(F1 - F2, TScript(Script).DecimalPrecision);

  Result := caNone;
end;

function CmdSys_Check(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  X : array of TCmdParam;
begin
  // CMD: sys_check

  SetLength(X, 0); // to correct strange compiler problem!?

  Result := TScript(Script).Controller.ScriptRef.Cmds[TWXAuth.Key].onCmd(Script, X);
end;

function CmdSys_Fail(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: sys_fail

  if (TWXAuth.AuthMsg <> '') then
    TWXServer.ClientMessage(TWXAuth.AuthMsg);

  TWXServer.ClientMessage('Unable to access subroutine - ' + ANSI_12 + 'Authentication failure.');

  Result := caStop;
end;

function CmdSys_NoAuth(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: sys_noAuth

  TWXServer.ClientMessage('Unable to access subroutine - ' + ANSI_12 + 'You have not been authenticated with a valid registration key.');
  Result := caAuth;
end;

function CmdSys_Kill(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: sys_kill

  Halt; // will throw exceptions on exit - gives the user an impression of a serious bug out

  Result := caStop;
end;

function CmdSys_Nop(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: sys_nop

  Result := caNone;
end;

function CmdSys_ShowMsg(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: sys_showMsg

  if (TWXAuth.AuthMsg <> '') then
    TWXServer.ClientMessage(TWXAuth.AuthMsg);

  Result := caNone;
end;

function CmdSystemScript(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: systemScript

  TScript(Script).System := TRUE;

  Result := caNone;
end;

function CmdUpperCase(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: upperCase var

  Params[0].Value := UpperCase(Params[0].Value);

  Result := caNone;
end;

function CmdWaitFor(Script : TObject; Params : array of TCmdParam) : TCmdAction;
begin
  // CMD: waitFor <value>

  TScript(Script).WaitText := Params[0].Value;
  TScript(Script).WaitForActive := TRUE;

  Result := caPause;
end;

function CmdWindow(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  Window : TScriptWindow;
  SizeX,
  SizeY  : Integer;
begin
  // CMD: window <windowName> <sizeX> <sizeY> <title> [<ontop>]

  ConvertToNumber(Params[1].Value, SizeX);
  ConvertToNumber(Params[2].Value, SizeY);

  Window := TScriptWindow.Create(
    Params[0].Value,
    Params[3].Value,
    SizeX,
    SizeY,
    (Length(Params) = 5)
    );

  TScript(Script).AddWindow(Window);  
  Window.Show;

  Result := caNone;
end;

function CmdWrite(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  F : TextFile;
begin
  // CMD: write <file> <value>

  SetCurrentDir(TScript(Script).ProgramDir);
  AssignFile(F, Params[0].Value);

{$I-}
  Append(F);

  if (IOResult <> 0) then
    ReWrite(F);

  if (IOResult <> 0) then
    raise EScriptError.Create('Unable to write to file ''' + Params[0].Value + '''');
{$I+}

  WriteLn(F, Params[1].Value);
  CloseFile(F);

  Result := caNone;
end;

function CmdXor(Script : TObject; Params : array of TCmdParam) : TCmdAction;
var
  B1,
  B2 : Boolean;
begin
  // CMD: xor var <value>

  ConvertToBoolean(Params[0].Value, B1);
  ConvertToBoolean(Params[1].Value, B2);

  Params[0].Value := ConvertBoolTostring(B1 xor B2);
  Result := caNone;
end;

// *****************************************************************************
//                      SCRIPT SYSTEM CONST IMPLEMENTATION
// *****************************************************************************

function SCAnsi_0(Indexes : TStringArray) : string;
begin
  Result := ANSI_0;
end;

function SCAnsi_1(Indexes : TStringArray) : string;
begin
  Result := ANSI_1;
end;

function SCAnsi_2(Indexes : TStringArray) : string;
begin
  Result := ANSI_2;
end;

function SCAnsi_3(Indexes : TStringArray) : string;
begin
  Result := ANSI_3;
end;

function SCAnsi_4(Indexes : TStringArray) : string;
begin
  Result := ANSI_4;
end;

function SCAnsi_5(Indexes : TStringArray) : string;
begin
  Result := ANSI_5;
end;

function SCAnsi_6(Indexes : TStringArray) : string;
begin
  Result := ANSI_6;
end;

function SCAnsi_7(Indexes : TStringArray) : string;
begin
  Result := ANSI_7;
end;

function SCAnsi_8(Indexes : TStringArray) : string;
begin
  Result := ANSI_8;
end;

function SCAnsi_9(Indexes : TStringArray) : string;
begin
  Result := ANSI_9;
end;

function SCAnsi_10(Indexes : TStringArray) : string;
begin
  Result := ANSI_10;
end;

function SCAnsi_11(Indexes : TStringArray) : string;
begin
  Result := ANSI_11;
end;

function SCAnsi_12(Indexes : TStringArray) : string;
begin
  Result := ANSI_12;
end;

function SCAnsi_13(Indexes : TStringArray) : string;
begin
  Result := ANSI_13;
end;

function SCAnsi_14(Indexes : TStringArray) : string;
begin
  Result := ANSI_14;
end;

function SCAnsi_15(Indexes : TStringArray) : string;
begin
  Result := ANSI_15;
end;

function SCConnected(Indexes : TStringArray) : string;
begin
  if (TWXClient.Connected) then
    Result := '1'
  else
    Result := '0';
end;

function SCCurrentANSILine(Indexes : TStringArray) : string;
begin
  Result := TWXExtractor.CurrentANSILine;
end;

function SCCurrentLine(Indexes : TStringArray) : string;
begin
  Result := TWXExtractor.CurrentLine;
end;

function SCDate(Indexes : TStringArray) : string;
begin
  Result := DateToStr(Now);
end;

function SCFalse(Indexes : TStringArray) : string;
begin
  Result := '0';
end;

function SCGame(Indexes : TStringArray) : string;
begin
  Result := TWXDatabase.DBHeader.Game;
end;

function SCGameName(Indexes : TStringArray) : string;
begin
  Result := StripFileExtension(ShortFileName(TWXDatabase.DatabaseName));
end;

function SCLicenseName(Indexes : TStringArray) : string;
begin
  Result := TWXAuth.UserName;
end;

function SCLoginName(Indexes : TStringArray) : string;
begin
  Result := TWXDatabase.DBHeader.LoginName;
end;

function SCPassword(Indexes : TStringArray) : string;
begin
  Result := TWXDatabase.DBHeader.Password;
end;

function SCPort_Class(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.CLASS[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  if (TWXDatabase.Sectors[SectIndex].SPort.Name = '') then
    Result := '-1'
  else
    Result := IntToStr(TWXDatabase.Sectors[SectIndex].SPort.ClassIndex);
end;

function SCPort_BuyFuel(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.BUYFUEL[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  if (TWXDatabase.Sectors[SectIndex].SPort.BuyProduct[ptFuelOre]) then
    Result := '1'
  else
    Result := '0';
end;

function SCPort_BuyOrg(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.BUYORG[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  if (TWXDatabase.Sectors[SectIndex].SPort.BuyProduct[ptOrganics]) then
    Result := '1'
  else
    Result := '0';
end;

function SCPort_BuyEquip(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.BUYEQUIP[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  if (TWXDatabase.Sectors[SectIndex].SPort.BuyProduct[ptEquipment]) then
    Result := '1'
  else
    Result := '0';
end;

function SCPort_Equip(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.EQUIPMENT[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].SPort.ProductAmount[ptEquipment]);
end;

function SCPort_Exists(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.EXISTS[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  if (TWXDatabase.Sectors[SectIndex].SPort.Name = '') then begin
    Result := '0';
  end else begin
    Result := '1';
  end;
end;

function SCPort_Fuel(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.FUEL[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].SPort.ProductAmount[ptFuelOre]);
end;

function SCPort_Name(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.NAME[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := TWXDatabase.Sectors[SectIndex].SPort.Name;
end;

function SCPort_Org(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.ORGANICS[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].SPort.ProductAmount[ptOrganics]);
end;

function SCPort_PercentFuel(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.PERCENTORE[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].SPort.ProductPercent[ptFuelOre]);
end;

function SCPort_PercentOrg(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.PERCENTORG[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].SPort.ProductPercent[ptOrganics]);
end;

function SCPort_PercentEquip(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for PORT.PERCENTEQUIP[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].SPort.ProductPercent[ptEquipment]);
end;

function SCSector_Anomoly(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.ANOMOLY[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(Byte(TWXDatabase.Sectors[SectIndex].Anomoly));
end;

function SCSector_BackdoorCount(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
  WarpsIn   : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.BACKDOORCOUNT[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  WarpsIn := TWXDatabase.GetBackdoors(TWXDatabase.LoadSector(SectIndex), SectIndex);
  Result := IntToStr(WarpsIn.Count);

  while (WarpsIn.Count > 0) do
  begin
    FreeMem(WarpsIn[0]);
    WarpsIn.Delete(0);
  end;

  WarpsIn.Free;
end;

function SCSector_Backdoors(Indexes : TStringArray) : string;
var
  SectIndex,
  WarpIndex : Integer;
  WarpsIn   : TList;
begin
  if (Length(Indexes) < 2) then
    raise EScriptError.Create('Invalid parameters for SECTOR.BACKDOORS[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  ConvertToNumber(Indexes[1], WarpIndex);
  CheckSector(SectIndex);

  WarpsIn := TWXDatabase.GetBackdoors(TWXDatabase.LoadSector(SectIndex), SectIndex);

  if (WarpIndex < 1) or (WarpIndex > WarpsIn.Count) then
    Result := '0'
  else
    Result := IntToStr(TWarpIn(WarpsIn[WarpIndex - 1]^).Origin);

  while (WarpsIn.Count > 0) do
  begin
    FreeMem(WarpsIn[0]);
    WarpsIn.Delete(0);
  end;

  WarpsIn.Free;
end;

function SCSector_Density(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.DENSITY[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].Density);
end;

function SCSector_Explored(Indexes : TStringArray) : string;
var
  Explored  : TSectorExploredType;
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.EXPLORED[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Explored := TWXDatabase.Sectors[SectIndex].Explored;

  case (Explored) of
    etNo      : Result := 'NO';
    etCalc    : Result := 'CALC';
    etDensity : Result := 'DENSITY';
    etHolo    : Result := 'YES';
  end;
end;

function SCSector_Figs_Owner(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.FIGS.OWNER[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := TWXDatabase.Sectors[SectIndex].Figs.Owner;
end;

function SCSector_Figs_Quantity(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.FIGS.QUANTITY[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].Figs.Quantity);
end;

function SCSector_Limpets_Owner(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.LIMPETS.OWNER[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := TWXDatabase.Sectors[SectIndex].Mines_Limpet.Owner;
end;

function SCSector_Limpets_Quantity(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.LIMPETS.QUANTITY[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].Mines_Limpet.Quantity);
end;

function SCSector_Mines_Owner(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.MINES.OWNER[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := TWXDatabase.Sectors[SectIndex].Mines_Armid.Owner;
end;

function SCSector_Mines_Quantity(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.MINES.QUANTITY[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].Mines_Armid.Quantity);
end;

function SCSector_NavHaz(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.NAVHAZ[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].NavHaz);
end;

function SCSector_PlanetCount(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
  ItemList  : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.PLANETCOUNT[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  ItemList := TWXDatabase.GetSectorItems(itPlanet, TWXDatabase.LoadSector(SectIndex));
  Result := IntToStr(ItemList.Count);
  FreeList(ItemList, SizeOf(TPlanet));
end;

function SCSector_Planets(Indexes : TStringArray) : string;
var
  PlanetIndex,
  SectIndex   : Integer;
  ItemList    : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.PLANETS[sector][planetIndex]');

  ConvertToNumber(Indexes[0], SectIndex);
  ConvertToNumber(Indexes[1], PlanetIndex);
  CheckSector(SectIndex);

  ItemList := TWXDatabase.GetSectorItems(itPlanet, TWXDatabase.LoadSector(SectIndex));

  if ((PlanetIndex > ItemList.Count) or (PlanetIndex < 1)) then
    Result := '0'
  else
    Result := TPlanet(ItemList[PlanetIndex - 1]^).Name;

  FreeList(ItemList, SizeOf(TPlanet));
end;

function SCSector_Ships(Indexes : TStringArray) : string;
var
  ShipIndex,
  SectIndex   : Integer;
  ItemList    : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.SHIPS[sector][shipIndex]');

  ConvertToNumber(Indexes[0], SectIndex);
  ConvertToNumber(Indexes[1], ShipIndex);
  CheckSector(SectIndex);

  ItemList := TWXDatabase.GetSectorItems(itShip, TWXDatabase.LoadSector(SectIndex));

  if ((ShipIndex > ItemList.Count) or (ShipIndex < 1)) then
    Result := '0'
  else
    Result := TShip(ItemList[ShipIndex - 1]^).Name;

  FreeList(ItemList, SizeOf(TShip));
end;

function SCSector_ShipCount(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
  ItemList  : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.SHIPCOUNT[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  ItemList := TWXDatabase.GetSectorItems(itShip, TWXDatabase.LoadSector(SectIndex));
  Result := IntToStr(ItemList.Count);
  FreeList(ItemList, SizeOf(TShip));
end;

function SCSector_Traders(Indexes : TStringArray) : string;
var
  TraderIndex,
  SectIndex   : Integer;
  ItemList    : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.TRADERS[sector][traderIndex]');

  ConvertToNumber(Indexes[0], SectIndex);
  ConvertToNumber(Indexes[1], TraderIndex);
  CheckSector(SectIndex);

  ItemList := TWXDatabase.GetSectorItems(itTrader, TWXDatabase.LoadSector(SectIndex));

  if ((TraderIndex > ItemList.Count) or (TraderIndex < 1)) then
    Result := '0'
  else
    Result := TTrader(ItemList[TraderIndex - 1]^).Name;

  FreeList(ItemList, SizeOf(TTrader));
end;

function SCSector_TraderCount(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
  ItemList  : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.TRADERCOUNT[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  ItemList := TWXDatabase.GetSectorItems(itTrader, TWXDatabase.LoadSector(SectIndex));
  Result := IntToStr(ItemList.Count);
  FreeList(ItemList, SizeOf(TTrader));
end;

function SCSector_Updated(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.UPDATED[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := DateTimeToStr(TWXDatabase.Sectors[SectIndex].Update);
end;

function SCSector_WarpCount(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.WARPCOUNT[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  Result := IntToStr(TWXDatabase.Sectors[SectIndex].Warps);
end;

function SCSector_Warps(Indexes : TStringArray) : string;
var
  SectIndex,
  WarpIndex : Integer;
begin
  if (Length(Indexes) < 2) then
    raise EScriptError.Create('Invalid parameters for SECTOR.WARPS[sector][warpIndex]');

  ConvertToNumber(Indexes[0], SectIndex);
  ConvertToNumber(Indexes[1], WarpIndex);
  CheckSector(SectIndex);

  if (WarpIndex < 1) or (WarpIndex > 6) then
    Result := '0'
  else
    Result := IntToStr(TWXDatabase.Sectors[SectIndex].Warp[WarpIndex]);
end;

function SCSector_WarpInCount(Indexes : TStringArray) : string;
var
  SectIndex : Integer;
  WarpsIn   : TList;
begin
  if (Length(Indexes) < 1) then
    raise EScriptError.Create('Invalid parameters for SECTOR.WARPINCOUNT[sector]');

  ConvertToNumber(Indexes[0], SectIndex);
  CheckSector(SectIndex);

  WarpsIn := TWXDatabase.GetWarpsIn(SectIndex);
  Result := IntToStr(WarpsIn.Count);
  WarpsIn.Free;
end;

function SCSector_WarpsIn(Indexes : TStringArray) : string;
var
  SectIndex,
  WarpIndex : Integer;
  WarpsIn   : TList;
begin
  if (Length(Indexes) < 2) then
    raise EScriptError.Create('Invalid parameters for SECTOR.WARPSIN[sector][warpIndex]');

  ConvertToNumber(Indexes[0], SectIndex);
  ConvertToNumber(Indexes[1], WarpIndex);
  CheckSector(SectIndex);

  WarpsIn := TWXDatabase.GetWarpsIn(SectIndex);

  if (WarpIndex < 1) or (WarpIndex > WarpsIn.Count) then
    Result := '0'
  else
    Result := IntToStr(TWarpIn(WarpsIn[WarpIndex - 1]^).Origin);

  WarpsIn.Free;
end;

function SCSectors(Indexes : TStringArray) : string;
begin
  Result := IntToStr(TWXDatabase.DBHeader.Sectors);
end;

function SCStardock(Indexes : TStringArray) : string;
begin
  Result := IntToStr(TWXDatabase.DBHeader.StarDock);
end;

function SCTime(Indexes : TStringArray) : string;
begin
  Result := TimeToStr(Now);
end;

function SCTrue(Indexes : TStringArray) : string;
begin
  Result := '1';
end;

// *****************************************************************************
//                             LIST BUILDER METHODS
// *****************************************************************************

procedure BuildSysConstList(ScriptRef : TScriptRef);
begin
  with (ScriptRef) do
  begin
    AddSysConstant('ANSI_0', SCAnsi_0);
    AddSysConstant('ANSI_1', SCAnsi_1);
    AddSysConstant('ANSI_2', SCAnsi_2);
    AddSysConstant('ANSI_3', SCAnsi_3);
    AddSysConstant('ANSI_4', SCAnsi_4);
    AddSysConstant('ANSI_5', SCAnsi_5);
    AddSysConstant('ANSI_6', SCAnsi_6);
    AddSysConstant('ANSI_7', SCAnsi_7);
    AddSysConstant('ANSI_8', SCAnsi_8);
    AddSysConstant('ANSI_9', SCAnsi_9);
    AddSysConstant('ANSI_10', SCAnsi_10);
    AddSysConstant('ANSI_11', SCAnsi_11);
    AddSysConstant('ANSI_12', SCAnsi_12);
    AddSysConstant('ANSI_13', SCAnsi_13);
    AddSysConstant('ANSI_14', SCAnsi_14);
    AddSysConstant('ANSI_15', SCAnsi_15);
    AddSysConstant('CONNECTED', SCConnected);
    AddSysConstant('CURRENTANSILINE', SCCurrentANSILine);
    AddSysConstant('CURRENTLINE', SCCurrentLine);
    AddSysConstant('DATE', SCDate);
    AddSysConstant('FALSE', SCFalse);
    AddSysConstant('GAME', SCGame);
    AddSysConstant('GAMENAME', SCGameName);
    AddSysConstant('LICENSENAME', SCLicenseName);
    AddSysConstant('LOGINNAME', SCLoginName);
    AddSysConstant('PASSWORD', SCPassword);
    AddSysConstant('PORT.CLASS', SCPort_Class);
    AddSysConstant('PORT.BUYFUEL', SCPort_BuyFuel);
    AddSysConstant('PORT.BUYORG', SCPort_BuyOrg);
    AddSysConstant('PORT.BUYEQUIP', SCPort_BuyEquip);
    AddSysConstant('PORT.EXISTS', SCPort_Exists);
    AddSysConstant('PORT.FUEL', SCPort_Fuel);
    AddSysConstant('PORT.NAME', SCPort_Name);
    AddSysConstant('PORT.ORG', SCPort_Org);
    AddSysConstant('PORT.EQUIP', SCPort_Equip);
    AddSysConstant('PORT.PERCENTFUEL', SCPort_PercentFuel);
    AddSysConstant('PORT.PERCENTORG', SCPort_PercentOrg);
    AddSysConstant('PORT.PERCENTEQUIP', SCPort_PercentEquip);
    AddSysConstant('SECTOR.ANOMOLY', SCSector_Anomoly);
    AddSysConstant('SECTOR.BACKDOORCOUNT', SCSector_BackDoorCount);
    AddSysConstant('SECTOR.BACKDOORS', SCSector_BackDoors);
    AddSysConstant('SECTOR.DENSITY', SCSector_Density);
    AddSysConstant('SECTOR.EXPLORED', SCSector_Explored);
    AddSysConstant('SECTOR.FIGS.OWNER', SCSector_Figs_Owner);
    AddSysConstant('SECTOR.FIGS.QUANTITY', SCSector_Figs_Quantity);
    AddSysConstant('SECTOR.LIMPETS.OWNER', SCSector_Limpets_Owner);
    AddSysConstant('SECTOR.LIMPETS.QUANTITY', SCSector_Limpets_Quantity);
    AddSysConstant('SECTOR.MINES.OWNER', SCSector_Mines_Owner);
    AddSysConstant('SECTOR.MINES.QUANTITY', SCSector_Mines_Quantity);
    AddSysConstant('SECTOR.NAVHAZ', SCSector_Navhaz);
    AddSysConstant('SECTOR.PLANETCOUNT', SCSector_PlanetCount);
    AddSysConstant('SECTOR.PLANETS', SCSector_Planets);
    AddSysConstant('SECTOR.SHIPCOUNT', SCSector_ShipCount);
    AddSysConstant('SECTOR.SHIPS', SCSector_Ships);
    AddSysConstant('SECTOR.TRADERCOUNT', SCSector_TraderCount);
    AddSysConstant('SECTOR.TRADERS', SCSector_Traders);
    AddSysConstant('SECTOR.UPDATED', SCSector_Updated);
    AddSysConstant('SECTOR.WARPCOUNT', SCSector_WarpCount);
    AddSysConstant('SECTOR.WARPS', SCSector_Warps);
    AddSysConstant('SECTOR.WARPSIN', SCSector_WarpsIn);
    AddSysConstant('SECTOR.WARPINCOUNT', SCSector_WarpInCount);
    AddSysConstant('SECTORS', SCSectors);
    AddSysConstant('STARDOCK', SCStardock);
    AddSysConstant('TIME', SCTime);
    AddSysConstant('TRUE', SCTrue);
  end;
end;

procedure BuildCommandList(ScriptRef : TScriptRef);
begin
  with (ScriptRef) do
  begin
    AddCommand('ADD', 2, 2, CmdAdd, [pkVar, pkValue], pkValue);
    AddCommand('ADDMENU', 7, 7, CmdAddMenu, [pkValue, pkValue, pkValue, pkValue, pkValue, pkValue], pkValue);
    AddCommand('AND', 2, 2, CmdAnd, [pkVar, pkValue], pkValue);
    AddCommand('BRANCH', 2, 2, CmdBranch, [pkValue, pkValue], pkValue);
    AddCommand('CLIENTMESSAGE', 1, 1, CmdClientMessage, [pkValue], pkValue);
    AddCommand('CLOSEMENU', 0, 0, CmdCloseMenu, [], pkValue);
    AddCommand('CONNECT', 0, 0, CmdConnect, [], pkValue);
    AddCommand('CUTTEXT', 4, 4, CmdCutText, [pkValue, pkVar, pkValue, pkValue], pkValue);
    AddCommand('DELETE', 1, 1, CmdDelete, [pkValue], pkValue);
    AddCommand('DISCONNECT', 0, 0, CmdDisconnect, [], pkValue);
    AddCommand('DIVIDE', 2, 2, CmdDivide, [pkVar, pkValue], pkValue);
    AddCommand('ECHO', 1, -1, CmdEcho, [pkValue], pkValue);
    AddCommand('FILEEXISTS', 2, 2, CmdFileExists, [pkVar, pkValue], pkValue);
    AddCommand('GETCHARCODE', 2, 2, CmdGetCharCode, [pkValue, pkVar], pkValue);
    AddCommand('GETCONSOLEINPUT', 1, 2, CmdGetConsoleInput, [pkVar, pkValue], pkValue);
    AddCommand('GETCOURSE', 3, 3, CmdGetCourse, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('GETDATE', 1, 1, CmdGetDate, [pkVar], pkValue);
    AddCommand('GETDISTANCE', 3, 3, CmdGetDistance, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('GETINPUT', 2, 3, CmdGetInput, [pkVar, pkValue], pkValue);
    AddCommand('GETLENGTH', 2, 2, CmdGetLength, [pkValue, pkVar], pkValue);
    AddCommand('GETMENUVALUE', 2, 2, CmdGetMenuValue, [pkValue, pkValue], pkValue);
    AddCommand('GETOUTTEXT', 1, 1, CmdGetOutText, [pkVar], pkValue);
    AddCommand('GETRND', 3, 3, CmdGetRnd, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('GETSECTOR', 2, 2, CmdGetSector, [pkValue, pkVar], pkValue);
    AddCommand('GETSECTORPARAMETER', 3, 3, CmdGetSectorParameter, [pkValue, pkValue, pkVar], pkValue);
    AddCommand('GETTEXT', 4, 4, CmdGetText, [pkValue, pkVar, pkValue, pkValue], pkValue);
    AddCommand('GETTIME', 1, 2, CmdGetTime, [pkVar, pkValue], pkValue);
    AddCommand('GOSUB', 1, 1, CmdGosub, [pkValue], pkValue);
    AddCommand('GOTO', 1, 1, CmdGoto, [pkValue], pkValue);
    AddCommand('GETWORD', 3, 4, CmdGetWord, [pkValue, pkVar, pkValue], pkValue);
    AddCommand('GETWORDPOS', 3, 3, CmdGetWordPos, [pkValue, pkVar, pkValue], pkValue);
    AddCommand('HALT', 0, 0, CmdHalt, [], pkValue);
    AddCommand('ISEQUAL', 3, 3, CmdIsEqual, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('ISGREATER', 3, 3, CmdIsGreater, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('ISGREATEREQUAL', 3, 3, CmdIsGreaterEqual, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('ISLESSER', 3, 3, CmdIsLesser, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('ISLESSEREQUAL', 3, 3, CmdIsLesserEqual, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('ISNOTEQUAL', 3, 3, CmdIsNotEqual, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('ISNUMBER', 2, 2, CmdIsNumber, [pkVar, pkValue], pkValue);
    AddCommand('KILLWINDOW', 1, 1, CmdKillWindow, [pkValue], pkValue);
    AddCommand('KILLALLTRIGGERS', 0, 0, CmdKillAllTriggers, [], pkValue);
    AddCommand('KILLTRIGGER', 1, 1, CmdKillTrigger, [pkValue], pkValue);
    AddCommand('LOAD', 1, 1, CmdLoad, [pkValue], pkValue);
    AddCommand('LOADVAR', 1, 1, CmdLoadVar, [pkVar], pkValue);
    AddCommand('LOGGING', 1, 1, CmdLogging, [pkValue], pkValue);
    AddCommand('LOWERCASE', 1, 1, CmdLowerCase, [pkVar], pkValue);
    AddCommand('MERGETEXT', 3, 3, CmdMergeText, [pkValue, pkValue, pkVar], pkValue);
    AddCommand('MULTIPLY', 2, 2, CmdMultiply, [pkVar, pkValue], pkValue);
    AddCommand('OPENMENU', 1, 1, CmdOpenMenu, [pkValue], pkValue);
    AddCommand('OR', 2, 2, CmdOr, [pkVar, pkValue], pkValue);
    AddCommand('PAUSE', 0, 0, CmdPause, [], pkValue);
    AddCommand('PROCESSIN', 2, 2, CmdProcessIn, [pkValue, pkValue], pkValue);
    AddCommand('PROCESSOUT', 1, 1, CmdProcessOut, [pkValue], pkValue);
    AddCommand('READ', 3, 3, CmdRead, [pkValue, pkVar, pkValue], pkValue);
    AddCommand('RENAME', 2, 2, CmdRename, [pkValue, pkValue], pkValue);
    AddCommand('REPLACETEXT', 3, 3, CmdReplaceText, [pkVar, pkValue, pkValue], pkValue);
    AddCommand('REQRECORDING', 0, 0, CmdReqRecording, [], pkValue);
    AddCommand('RETURN', 0, 0, CmdReturn, [], pkValue);
    AddCommand('ROUND', 1, 2, CmdRound, [pkVar, pkValue], pkValue);
    AddCommand('SAVEVAR', 1, 1, CmdSaveVar, [pkVar], pkValue);
    AddCommand('SEND', 1, -1, CmdSend, [pkValue], pkValue);
    AddCommand('SETARRAY', 2, -1, CmdSetArray, [pkVar, pkValue], pkValue);
    AddCommand('SETDELAYTRIGGER', 3, 3, CmdSetDelayTrigger, [pkValue, pkValue, pkValue], pkValue);
    AddCommand('SETEVENTTRIGGER', 3, 4, CmdSetEventTrigger, [pkValue, pkValue, pkValue], pkValue);
    AddCommand('SETMENUHELP', 2, 2, CmdSetMenuHelp, [pkValue, pkValue], pkValue);
    AddCommand('SETMENUVALUE', 2, 2, CmdSetMenuValue, [pkValue, pkValue], pkValue);
    AddCommand('SETMENUOPTIONS', 4, 4, CmdSetMenuOptions, [pkValue, pkValue, pkValue], pkValue);
    AddCommand('SETPRECISION', 1, 1, CmdSetPrecision, [pkValue], pkValue);
    AddCommand('SETPROGVAR', 2, 2, CmdSetProgVar, [pkValue, pkValue], pkValue);
    AddCommand('SETSECTORPARAMETER', 3, 3, CmdSetSectorParameter, [pkValue, pkValue, pkValue], pkValue);
    AddCommand('SETTEXTLINETRIGGER', 2, 3, CmdSetTextLineTrigger, [pkValue, pkValue, pkValue], pkValue);
    AddCommand('SETTEXTOUTTRIGGER', 2, 3, CmdSetTextOutTrigger, [pkValue, pkValue, pkValue], pkValue);
    AddCommand('SETTEXTTRIGGER', 2, 3, CmdSetTextTrigger, [pkValue, pkValue, pkValue], pkValue);
    AddCommand('SETVAR', 2, 2, CmdSetVar, [pkVar, pkValue], pkValue);
    AddCommand('SETWINDOWCONTENTS', 2, 2, CmdSetWindowContents, [pkValue, pkValue], pkValue);
    AddCommand('SOUND', 1, 1, CmdSound, [pkValue], pkValue);
    AddCommand('STOP', 1, 1, CmdStop, [pkValue], pkValue);
    AddCommand('STRIPTEXT', 2, 2, CmdStripText, [pkVar, pkValue], pkValue);
    AddCommand('SUBTRACT', 2, 2, CmdSubtract, [pkVar, pkValue], pkValue);
    AddCommand('SYS_CHECK', 0, 0, CmdSys_Check, [pkValue], pkValue);
    AddCommand('SYS_FAIL', 0, 0, CmdSys_Fail, [pkValue], pkValue);
    AddCommand('SYS_KILL', 0, 0, CmdSys_Kill, [pkValue], pkValue);
    AddCommand('SYS_NOAUTH', 0, 0, CmdSys_NoAuth, [pkValue], pkValue);
    AddCommand('SYS_NOP', 0, 0, CmdSys_Nop, [pkValue], pkValue);
    AddCommand('SYS_SHOWMSG', 0, 0, CmdSys_ShowMsg, [pkValue], pkValue);
    AddCommand('SYSTEMSCRIPT', 0, 0, CmdSystemScript, [], pkValue);
    AddCommand('UPPERCASE', 1, 1, CmdUpperCase, [pkVar], pkValue);
    AddCommand('XOR', 2, 2, CmdXor, [pkVar, pkValue], pkValue);
    AddCommand('WAITFOR', 1, 1, CmdWaitFor, [pkValue], pkValue);
    AddCommand('WINDOW', 4, 5, CmdWindow, [pkValue, pkValue, pkValue, pkValue, pkValue], pkValue);
    AddCommand('WRITE', 2, 2, CmdWrite, [pkValue, pkValue], pkValue);
  end;
end;

end.

