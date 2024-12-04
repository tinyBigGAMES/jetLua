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

unit UTestbed;

interface

// Entry point for running test cases
procedure RunTests();

implementation

uses
  // Essential Delphi units for collections, system utilities, classes,
  // I/O operations, and type information, Lua scripting
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.TypInfo,
  jetLua;

// Procedure to pause execution and wait for user input
procedure Pause();
begin
  WriteLn;
  Write('Press ENTER to continue...');
  ReadLn;
  WriteLn;
end;

// Definition of a record type for testing purposes
type
  TTestRecord = record
    ID: Integer;
    Name: string;
    Value: Double;
  end;

  PTestRecord = ^TTestRecord; // Pointer to TTestRecord

  {$M+} // Enable runtime type information (RTTI) for the class below
  // A test class exposing various methods to demonstrate functionality
  TTestClass = class
  published
    // Arithmetic operations
    class function Add(A, B: Integer): Integer;
    class function Multiply(A, B: Double): Double;
    // String concatenation
    class function Concat(const A, B: string): string;
    // Methods for handling TStringList
    class function CreateList: TStringList;
    class function GetListCount(List: TStringList): Integer;
    // Record manipulation methods
    class function CreateRecord(ID: Integer; const Name: string; Value: Double): TTestRecord;
    class procedure UpdateRecord(P: PTestRecord; NewValue: Double);
    class function GetRecordValue(P: PTestRecord): Double;
    // Memory management methods
    class function AllocateMemory(Size: Integer): Pointer;
    class procedure FreeMemory(P: Pointer);
    class procedure WriteToMemory(P: Pointer; Value: Integer);
    class function ReadFromMemory(P: Pointer): Integer;
  end;
  {$M-} // Disable RTTI

// Implementation of TTestClass methods
class function TTestClass.Add(A, B: Integer): Integer;
begin
  Result := A + B;
end;

class function TTestClass.Multiply(A, B: Double): Double;
begin
  Result := A * B;
end;

class function TTestClass.Concat(const A, B: string): string;
begin
  Result := A + B;
end;

class function TTestClass.CreateList: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('Test1');
  Result.Add('Test2');
end;

class function TTestClass.GetListCount(List: TStringList): Integer;
begin
  Result := List.Count;
end;

class function TTestClass.CreateRecord(ID: Integer; const Name: string; Value: Double): TTestRecord;
begin
  Result.ID := ID;
  Result.Name := Name;
  Result.Value := Value;
end;

class procedure TTestClass.UpdateRecord(P: PTestRecord; NewValue: Double);
begin
  if P <> nil then
    P^.Value := NewValue;
end;

class function TTestClass.GetRecordValue(P: PTestRecord): Double;
begin
  if P <> nil then
    Result := P^.Value
  else
    Result := 0;
end;

class function TTestClass.AllocateMemory(Size: Integer): Pointer;
begin
  Result := AllocMem(Size);
end;

class procedure TTestClass.FreeMemory(P: Pointer);
begin
  FreeMem(P);
end;

class procedure TTestClass.WriteToMemory(P: Pointer; Value: Integer);
begin
  if P <> nil then
    PInteger(P)^ := Value;
end;

class function TTestClass.ReadFromMemory(P: Pointer): Integer;
begin
  if P <> nil then
    Result := PInteger(P)^
  else
    Result := 0;
end;

// Foreign Function Interface (FFI) function to add two integers
function FFIAdd(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Exporting the FFIAdd function for external access
exports
  FFIAdd;

// Lua script embedded as a Delphi string constant
const
  CScript =
  '''
  -- Custom "import" example: importing a Lua module
  local mm = import("./res/scripts/mymath.lua")
  mm.add(50,50)

  -- Accessing Delphi-defined variables from Lua
  test_var = 0
  test_table = {test="test"}
  print("test_var: " .. test_var)
  print("test_table: " .. test_table.test)

  -- Accessing Delphi-provided variables from Lua
  print("LuaJIT Version: " .. jetLua.luaJitVersion)
  print("Lua Version: " .. jetLua.luaVersion)
  print("jetLua Version: " .. jetLua.version)

  -- Demonstrating Delphi class methods in Lua
  print("Testing TTestClass methods...")
  print("Add:", TTestClass.Add(5, 3))
  print("Multiply:", TTestClass.Multiply(4.5, 2.0))
  print("Concat:", TTestClass.Concat("Hello ", "World"))
  local list = TTestClass.CreateList()
  print("List count:", TTestClass.GetListCount(list))

  -- Handling Delphi records in Lua
  local rec = TTestClass.CreateRecord(6889, "Test", 3.14)
  print("Initial Value:", TTestClass.GetRecordValue(rec))
  TTestClass.UpdateRecord(rec, 6.28)
  print("Updated Value:", TTestClass.GetRecordValue(rec))

  -- Pointer manipulation in Lua
  local ptr = TTestClass.AllocateMemory(4)
  TTestClass.WriteToMemory(ptr, 42)
  print("Memory Value:", TTestClass.ReadFromMemory(ptr))
  TTestClass.FreeMemory(ptr)

  -- Defining Lua functions
  function add(a, b)
     return a + b
  end

  function concat(str1, str2)
    return str1 .. str2
  end

  function process_record(rec, str)
    print("Value:", TTestClass.GetRecordValue(rec))
    print("str: " .. str)
  end

  -- FFI example: Calling a native function directly from Lua
  local ffi = require("ffi")
  ffi.cdef[[
      int FFIAdd(int a, int b);
  ]]
  print("FFI Add:", ffi.C.FFIAdd(10, 20))

  -- Loading an external DLL and accessing its functions via FFI
  ffi.cdef[[
      void MessageBoxA(void* hwnd, const char* text, const char* caption, int type);
  ]]
  local user32 = ffi.load("user32.dll")
  user32.MessageBoxA(nil, "Hello from LuaJIT!", "FFI Example", 0)
  ''';

// Test procedure demonstrating Lua integration and various functionalities
procedure Test01();
var
  LjetLua: TjetLua; // Instance interfacing with Lua
  LRec: TTestRecord; // Test record instance
  LStream: TMemoryStream; // Memory stream for compiled Lua scripts
begin
  LjetLua := TjetLua.Create();
  try
    try
      // Attempt to run any embedded payload; exit if successful
      if LjetLua.RunPayload() then
        Exit;

      // Add search path for Lua scripts
      LjetLua.AddSearchPath('.\res\scripts');

      // Register Delphi class methods with Lua
      LjetLua.RegisterRoutines(TTestClass);

      // Load and execute the Lua script defined in CScript
      LjetLua.LoadString(CScript);

      // Execute Lua functions and print results
      LjetLua.PrintLn('Integer value: %d', [LjetLua.Call('add', [50, 50]).AsInteger]);
      LjetLua.PrintLn('String value: %s', [LjetLua.Call('concat', ['Hello, ', 'World!']).AsString]);

      // Initialize and manipulate a Delphi record from Lua
      LRec.ID := 1;
      LRec.Name := 'test';
      LRec.Value := 200;
      LjetLua.Call('process_record', [@LRec, 'test string']);

      // Call Delphi class methods from Lua and print results
      LjetLua.PrintLn('TTestClass.Multiply(2,2): %3.2f', [LjetLua.Call('TTestClass.Multiply', [2, 2]).AsExtended]);
      LjetLua.PrintLn('math.sqrt(25): %3.2f', [LjetLua.Call('math.sqrt', [25]).AsExtended]);

      // Check existence of routines in Lua environment
      LjetLua.PrintLn('Routine "TTestClass.Multiply" exist: %s', [BoolToStr(LjetLua.RoutineExist('TTestClass.Multiply'), True)]);
      LjetLua.PrintLn('Routine "add" exist: %s', [BoolToStr(LjetLua.RoutineExist('add'), True)]);

      // Set and get Lua variables from Delphi
      LjetLua.SetVariable('v.test', 'test');
      LjetLua.PrintLn('Table variable v.test: "%s"', [LjetLua.GetVariable('v.test').AsString]);
      LjetLua.SetVariable('test_var', 502);
      LjetLua.PrintLn('test_var: "%3.2f"', [LjetLua.GetVariable('test_var').AsExtended]);
      LjetLua.PrintLn('test_table.test: "%s"', [LjetLua.GetVariable('test_table.test').AsString]);

      // Reset Lua state and prepare to store payload
      LjetLua.Reset();
      LjetLua.AddSearchPath('.\res\scripts');

      // Copy executable to create a payload
      TFile.Copy('Testbed.exe', 'Payload.exe', True);

      // Attempt to store compiled Lua bytecode into the payload executable
      if LjetLua.StorePayload('.\res\scripts\compiled.lua', 'Payload.exe') then
        LjetLua.PrintLn('Saved bytecode to "Payload.exe"', [])
      else
        LjetLua.PrintLn('Failed to save bytecode to "Payload.exe"', []);

      // Reset Lua state again
      LjetLua.Reset();

      // Compile Lua script to a memory stream and load it
      LStream := TMemoryStream.Create();
      try
        LjetLua.CompileToStream('.\res\scripts\compiled.lua', LStream, False);
        LjetLua.LoadBuffer(LStream.Memory, LStream.Size);
      finally
        LStream.Free();
      end;

      // Final reset and loading of the compiled Lua script from file
      LjetLua.Reset();
      LjetLua.LoadFile('.\res\scripts\compiled.lua');

    except
      on E: Exception do
        // Handle and display any exceptions that occur during testing
        LjetLua.PrintLn('Error: %s', [E.Message]);
    end;
  finally
    // Ensure the Lua interface is properly freed
    LjetLua.Free();
  end;
end;

// Placeholder for additional test procedures
procedure Test02();
begin
  // Implementation for Test02 would go here
end;

procedure Test03();
begin
  // Implementation for Test03 would go here
end;

// Main procedure to run selected tests
procedure RunTests();
var
  LNum: Integer;
begin
  LNum := 01; // Select which test to run

  case LNum of
    01: Test01(); // Execute Test01
    02: Test02(); // Execute Test02
    03: Test03(); // Execute Test03
  end;

  Pause(); // Pause execution after running the test
end;

end.
