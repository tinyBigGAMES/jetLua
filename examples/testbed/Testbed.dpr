{===============================================================================
    _      _   _
   (_) ___| |_| |   _   _  __ _ ™
   | |/ _ \ __| |  | | | |/ _` |
   | |  __/ |_| |__| |_| | (_| |
  _/ |\___|\__|_____\__,_|\__,_|
 |__/
  A best-in-class Lua scripting
       solution for Delphi

 Copyright © 2024-present tinyBigGAMES™ LLC
 All Rights Reserved.

===============================================================================}

program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  jetLua in '..\..\src\jetLua.pas',
  UTestbed in 'UTestbed.pas';

begin
  RunTests();
end.
