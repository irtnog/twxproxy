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
program TWXProxy;

{%File 'changes.txt'}

uses
{$IFNDEF RELEASE}
  MemCheck,
{$ENDIF}
  Forms,
  Windows,
  SysUtils,
  Dialogs,
  FileCtrl,
  FormMain in 'FormMain.pas' {frmMain},
  FormSetup in 'FormSetup.pas' {frmSetup},
  Process in 'Process.pas',
  Script in 'Script.pas',
  Menu in 'Menu.pas',
  Database in 'Database.pas',
  Utility in 'Utility.pas',
  FormHistory in 'FormHistory.pas' {frmHistory},
  Bubble in 'Bubble.pas',
  Log in 'Log.pas',
  ScriptCmd in 'ScriptCmd.pas',
  TWXExport in 'TWXExport.pas',
  ScriptCmp in 'ScriptCmp.pas',
  Ansi in 'Ansi.pas',
  ScriptRef in 'ScriptRef.pas',
  FormAbout in 'FormAbout.pas' {frmAbout},
  FormLicense in 'FormLicense.pas' {frmLicense},
  TCP in 'TCP.pas',
  Auth in 'Auth.pas',
  FormUpgrade in 'FormUpgrade.pas' {frmUpgrade},
  FormScript in 'FormScript.pas',
  core in 'core.pas',
  Global in 'Global.pas',
  Persistence in 'Persistence.pas',
  GUI in 'GUI.pas',
  Observer in 'Observer.pas',
  Messages;

{$R *.RES}

type
  TModuleClass = class of TTWXModule;
  TModuleType = (mtDatabase, mtBubble, mtExtractor, mtMenu, mtServer, mtAuth, mtInterpreter, mtClient, mtLog, mtGUI);

  // TMessageHandler: This class exists only because the Application.OnMessage has
  // to be implmented as a property on an object.
  TMessageHandler = class(TObject)
  public
    procedure OnApplicationMessage(var Msg: TMsg; var Handled: Boolean);
  end;

const
  // ModuleClasses: Must line up with TModuleType for constructors to work properly
  ModuleClasses: array[TModuleType] of TModuleClass = (TModDatabase, TModBubble, TModExtractor, TModMenu, TModServer, TModAuth, TModInterpreter, TModClient, TModLog, TModGUI);

var
  PersistenceManager: TPersistenceManager;
  MessageHandler: TMessageHandler;
  ProgramDir: string;

function ModuleFactory(Module: TModuleType): TTWXModule;
var
  Globals: ITWXGlobals;
begin
  Result := ModuleClasses[Module].Create(Application, PersistenceManager);

  if (Result.GetInterface(ITWXGlobals, Globals)) then
  begin
    // set globals for this module
    Globals.ProgramDir := ProgramDir;
  end;

  // Not ideal.  This completely breaks the idea behind the factory method.  Having
  // all of these objects existing in a global scope destroys the modularity of
  // the application but is unfortunately necessary because of their current
  // interdependency.  The vision was to have each module abstracted through the
  // use of interfaces - I just never had time to pull this off.
  case Module of
    mtMenu: TWXMenu               := Result as TModMenu;
    mtDatabase: TWXDatabase       := Result as TModDatabase;
    mtLog: TWXLog                 := Result as TModLog;
    mtExtractor: TWXExtractor     := Result as TModExtractor;
    mtInterpreter: TWXInterpreter := Result as TModInterpreter;
    mtServer: TWXServer           := Result as TModServer;
    mtClient: TWXClient           := Result as TModClient;
    mtAuth: TWXAuth               := Result as TModAuth;
    mtBubble: TWXBubble           := Result as TModBubble;
    mtGUI: TWXGUI                 := Result as TModGUI;
  end;
end;

procedure InitProgram;
var
  I     : Integer;
  Switch: string;
  ModuleType: TModuleType;
begin
  Randomize;
  ProgramDir := GetCurrentDir;

  MessageHandler := TMessageHandler.Create;
  Application.OnMessage := MessageHandler.OnApplicationMessage;

  // Create dirs if they aren't there
  if not (DirectoryExists(ProgramDir + '\data')) then
    CreateDir(ProgramDir + '\data');

  if not (DirectoryExists(ProgramDir + '\scripts')) then
    CreateDir(ProgramDir + '\scripts');

  if not (DirectoryExists(ProgramDir + '\logs')) then
    CreateDir(ProgramDir + '\logs');

  PersistenceManager := TPersistenceManager.Create(Application);
  PersistenceManager.OutputFile := 'TWXSetup.dat';

  // call object constructors
  for ModuleType := Low(TModuleType) to High(TModuleType) do
    ModuleFactory(ModuleType);

  PersistenceManager.LoadStateValues;

  // check command line values
  for I := 1 to ParamCount do
  begin
    Switch := UpperCase(ParamStr(I));

    if (Copy(Switch, 1, 2) = '/P') and (Length(Switch) > 2) then
      TWXServer.ListenPort := StrToIntSafe(Copy(Switch, 3, Length(Switch)));
  end;
end;

procedure FinaliseProgram;
begin
  PersistenceManager.SaveStateValues;
  MessageHandler.Free;

  // More hacks ... force a destruction order using global variables to prevent
  // AVs on exit (some modules have extra processing on shutdown)
  // Note that modules not freed here are owned by the application object anyway -
  // so they will be freed implicitly.
  TWXInterpreter.Free;
  TWXGUI.Free;
  TWXClient.Free;
  TWXServer.Free;
  TWXLog.Free;
  TWXMenu.Free;
  TWXDatabase.Free;
  TWXBubble.Free;
  TWXAuth.Free;
  TWXExtractor.Free;
end;

procedure TMessageHandler.OnApplicationMessage(var Msg: TMsg; var Handled: Boolean);
var
  NotificationEvent: TNotificationEvent;
begin
  if (Msg.Message = WM_USER) then
  begin
    // Dispatch message to the object its meant for
    NotificationEvent := TNotificationEvent(Pointer(Msg.wParam)^);
    NotificationEvent(Pointer(Msg.lParam));
    Dispose(Pointer(Msg.wParam));

    Handled := True;
  end;
end;

begin
{$IFNDEF RELEASE}
  MemChk;
{$ENDIF}

  Application.Initialize;
  Application.Title := 'TWX Proxy';
  SetCurrentDir(ExtractFilePath(Application.ExeName));
  InitProgram;

  try
    // we don't use the TApplication message loop, as it requires a main form
    repeat
      Application.HandleMessage
    until Application.Terminated;
  finally
    FinaliseProgram;
  end;
end.
