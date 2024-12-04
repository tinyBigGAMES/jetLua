![jetLua](media/jetLua.png)  
[![Chat on Discord](https://img.shields.io/discord/754884471324672040?style=for-the-badge)](https://discord.gg/tPWjMwK)
[![Follow on Bluesky](https://img.shields.io/badge/Bluesky-tinyBigGAMES-blue?style=for-the-badge&logo=bluesky)](https://bsky.app/profile/tinybiggames.com)

# A best-in-class Lua scripting solution for Delphi️

jetLua is an advanced LuaJIT 🔥 integration library meticulously crafted for Delphi 🛠️ developers, providing a seamless interface between Delphi applications and LuaJIT scripts. By merging Delphi's inherent robustness 💪 with the exceptional runtime efficiency of LuaJIT's Just-In-Time (JIT) compilation 🚀, jetLua is optimized for professional developers aiming to incorporate dynamic ⚡ capabilities into their software without compromising on performance or stability.

## Overview 📝

jetLua integrates LuaJIT version 2.1+, statically compiled into your Delphi application, eliminating the need for external DLL dependencies 📦. This greatly enhances the portability 🚚 of the application, simplifies distribution, and facilitates the incorporation of scripting capabilities within Delphi environments.

Using jetLua, developers can expose Delphi classes methods to Lua scripts, thereby endowing their applications with dynamic extensibility 🌀. The library leverages Delphi's RTTI (Run-Time Type Information) 🧠 to facilitate efficient method registration and integration with LuaJIT in a streamlined manner.

Moreover, jetLua harnesses the powerful Foreign Function Interface (FFI) 🔗 offered by LuaJIT, allowing exported Delphi/DLL routines to be directly registered and utilized within Lua scripts. Leveraging FFI eliminates the necessity for complex intermediary bindings, facilitating faster and more effective integration between Delphi and LuaJIT.

## Key Features ✨

- **Integrated LuaJIT Version**: Embeds LuaJIT 2.1+ directly, eliminating any requirement for external dependencies 📦.
- **Automatic Routine Registration** 🔄: Delphi methods that are published are automatically available to LuaJIT, simplifying integration.
- **FFI Routine Registration** 🔗: LuaJIT's FFI allows direct registration and invocation of Delphi routines, providing an efficient mechanism for exposing native functionality. See LuaJIT's <a href="https://luajit.org/ext_ffi.html" target="_blank">FFI guide</a> for more information.
- **External DLL Loading** 📦: Utilize LuaJIT's FFI to load external DLLs and directly access their exported functions, enabling sophisticated third-party library integrations.
- **Support for Fundamental Types** 🧩: Parameters and return types such as strings 📝, floating-point numbers 🔢, Booleans ✅❌, and pointers ➡️ are fully supported.
- **Robust Pointer Management** 🔧: Ensures stability 🔒 while managing pointers for intricate data structures.
- **Exception Handling** ⚠️: Robust error handling mechanisms allow your application to continue executing smoothly 🚀 even in the presence of Lua script errors.
- **Interactive Debugging** 🐞: Inserting `dbg()` within LuaJIT code initiates an interactive debugging session, essential for runtime issue identification.
- **Script Importing and Bundling** 📦: Custom `import` commands allow scripts to be combined, compiled into a single unit, and optionally embedded as an EXE resource, resulting in a fully self-contained application.

## Usage Instructions 🛠️

To begin utilizing jetLua, follow these steps:

1. **Compilation** 📦: Compile your program in the Delphi IDE, ensuring the jetLua library is included.
2. **Execution** ▶️: Run the application to register the Delphi published classe methods with the LuaJIT runtime.
3. **Script Authoring** 📝📜: Develop LuaJIT scripts to interact with your Delphi routines, leveraging the scripting capabilities integrated within jetLua.

## Example Integration: Testbed Program 🧪

The `Testbed` Program provides a comprehensive example of jetLua's capabilities, demonstrating the integration of LuaJIT scripting with a Delphi application. The key functionalities illustrated include arithmetic operations ➕, string manipulation ✂️, record handling 🗂️, memory management 💾, and sophisticated error handling ⚠️.

### Features Demonstrated 🧩

- **Arithmetic Operations** ➕: Exposes addition and multiplication operations to LuaJIT scripts.
- **String Manipulation** ✂️: Facilitates concatenation and list generation accessible from LuaJIT.
- **Record Handling** 🗂️: Supports creating, updating, and retrieving Delphi records via pointers, accessible directly from LuaJIT.
- **Memory Management** 💾: Demonstrates allocation, writing, reading, and freeing memory blocks through LuaJIT scripts, showcasing effective pointer management.
- **Native Lua Integration** 📜: Provides examples of LuaJIT scripts interacting with Delphi functions.
- **FFI-Based Function Calls** 🔗: Uses LuaJIT's FFI for seamless invocation of Delphi routines.
- **Loading and Utilizing External DLLs** 📦: Demonstrates how LuaJIT's FFI loads external DLLs and directly accesses their exported functions to extend application capabilities.

### Example LuaJIT Script 📜

```lua
print("Testing TTestClass methods...")
print("Add:", TTestClass.Add(5, 3))
print("Multiply:", TTestClass.Multiply(4.5, 2.0))
print("Concat:", TTestClass.Concat("Hello ", "World"))

-- Record handling example
local rec = TTestClass.CreateRecord(1, "Test Record", 42.0)
print("Initial Record Value:", TTestClass.GetRecordValue(rec))
TTestClass.UpdateRecord(rec, 100.0)
print("Updated Record Value:", TTestClass.GetRecordValue(rec))

-- Memory management example
local mem = TTestClass.AllocateMemory(4)
TTestClass.WriteToMemory(mem, 12345)
print("Memory Value:", TTestClass.ReadFromMemory(mem))
TTestClass.FreeMemory(mem)

-- FFI example: Calling a native function directly
local ffi = require("ffi")
ffi.cdef[[
    int Add(int a, int b);
]]
print("FFI Add:", ffi.C.Add(10, 20))

-- Loading an external DLL and accessing its functions
ffi.cdef[[
    void MessageBoxA(void* hwnd, const char* text, const char* caption, int type);
]]
local user32 = ffi.load("user32.dll")
user32.MessageBoxA(nil, "Hello from LuaJIT!", "FFI Example", 0)
```

### Delphi Side Example 🛠️

Below is an example illustrating how to utilize jetLua within a Delphi application:

```delphi  
  
type
  TTestRecord = record
    ID: Integer;
    Name: string;
    Value: Double;
  end;
  PTestRecord = ^TTestRecord;

  {$M+}
  TTestClass = class
  published
    class function Add(A, B: Integer): Integer;
    class function Multiply(A, B: Double): Double;
    class function Concat(const A, B: string): string;
    class function CreateList: TStringList;
    class function GetListCount(List: TStringList): Integer;
    class function CreateRecord(ID: Integer; const Name: string; Value: Double): TTestRecord;
    class procedure UpdateRecord(P: PTestRecord; NewValue: Double);
    class function GetRecordValue(P: PTestRecord): Double;
    class function AllocateMemory(Size: Integer): Pointer;
    class procedure FreeMemory(P: Pointer);
    class procedure WriteToMemory(P: Pointer; Value: Integer);
    class function ReadFromMemory(P: Pointer): Integer;
  end;
  {$M-}  

var
  LJetLua: TJetLua;
  LRec: TTestRecord;

begin
  LJetLua := TJetLua.Create();
  try
    try
      LJetLua.RegisterRoutines(TTestClass);
      LJetLua.LoadString(CScript);

      WriteLn('Integer value: ', LJetLua.Call('add', [50, 50]).AsInteger);
      WriteLn('String value: ', LJetLua.Call('concat', ['Hello, ', 'World!']).AsString);

      LRec.ID := 1;
      LRec.Name := 'test';
      LRec.Value := 200;

      LJetLua.Call('process_record', [@LRec, 'test']);
    except
      on E: Exception do
      begin
        WriteLn(Format('Error: %s', [E.Message]));
      end;
    end;
  finally
    LJetLua.Free();
  end;
end;
```

### Key Takeaways 📌

This example demonstrates the versatility and interoperability 🔗 of Delphi and LuaJIT:

- **Enhanced Application Functionality** ⚡: Leverage LuaJIT scripts to enrich Delphi applications dynamically.
- **Safe Memory Handling** 💾: Showcases best practices in managing pointers between Delphi and LuaJIT, promoting application stability.
- **Direct Native Function Access via FFI** 🔗: Simplifies and optimizes integration by bypassing traditional binding processes.
- **Third-Party Library Support** 📦: LuaJIT's FFI enables direct loading of external DLLs, significantly expanding application capabilities.
- **Robust Error Management** 🐞: Presents efficient debugging techniques for addressing integration challenges.

## Advanced Usage Notes 🧠

### Integrated LuaJIT Version 🐍

jetLua uses LuaJIT 2.1+, statically embedded into the Delphi application to ensure a self-contained deployment without external library dependencies.

### Automatic Registration of Delphi Routines 🔄

Delphi routines declared as published class methods are automatically registered by jetLua, eliminating the need for manual binding. Declaring methods as `published` guarantees their accessibility from LuaJIT scripts.

### FFI Integration 🔗

LuaJIT's Foreign Function Interface (FFI) allows the registration and invocation of native Delphi routines directly from Lua scripts. Key advantages include:

- **Direct Access to Native Functions**: Avoid the inefficiencies of traditional binding mechanisms by invoking Delphi routines directly.
- **Simplicity in Integration**: Reduce boilerplate code, simplifying the incorporation of complex Delphi functions into LuaJIT.
- **Optimized Performance**: Benefit from optimized FFI calls that approach native execution speed.
- **External DLL Loading**: Load any external DLL via LuaJIT's FFI and directly invoke its functions. The FFI also has the capability to parse C headers to extract routine signatures, allowing for highly flexible integration with third-party libraries.

### Supported Parameter and Return Types ✅

jetLua supports fundamental data types for both parameters and return values, including:

- `string` 📝
- `float` (single or double) 🔢
- `Boolean` ✅❌
- `Pointer` ➡️

When designing methods for LuaJIT interoperability, ensure all parameters and return values conform to these supported types.

### Pointer Management 🔧

Pointers generated within Delphi should be exclusively managed by Delphi. Important considerations include:

- **Pointer References**: While pointers passed to LuaJIT can be referenced within Lua scripts, their modification must occur through Delphi.
- **Memory Cleanup**: Properly clean up dynamically allocated pointers to prevent memory leaks and ensure stability.

### Prerequisites 📋

- **Delphi Version**: Delphi 12.2 or newer.
- **Operating System**: Windows 10 or higher, validated on Windows 11 (64-bit, version 23H2).

## Getting Started with the jetLua Testbed Program 🚀

The Testbed Program provides a demonstrative example of integrating LuaJIT scripting with a Delphi application, encompassing:

- **LuaJIT Script Integration** 📜: Illustrates how to extend a Delphi application with scripting capabilities.
- **Exposure of Delphi Methods** 🛠️: Shows how to expose various Delphi data structures and methods to LuaJIT.
- **Mixed Programming Paradigms** 🔄: Combines static and dynamic programming approaches for application extensibility.

### How to Run the Testbed Program ▶️

1. Compile the Testbed Program in the Delphi IDE, ensuring jetLua is included.
2. Execute the program and observe LuaJIT interacting with Delphi methods.
3. Review the output that demonstrates features such as arithmetic, record handling, and memory management.

### Example Workflow 🔄

- **LuaJIT Arithmetic Operations** ➕: Perform basic arithmetic through LuaJIT calls to Delphi methods.
- **String Concatenation** ✂️: Use LuaJIT to concatenate strings with Delphi-exposed functions.
- **Record Management** 🗂️: Create and manipulate records from LuaJIT to demonstrate data exchange between the environments.
- **Memory Operations** 💾: Utilize LuaJIT for allocating, reading, and freeing memory, with Delphi managing the lifecycle.
- **FFI and DLL Usage** 📦: Load external DLLs and perform advanced FFI calls directly from LuaJIT, showcasing extended application capabilities.

## Notes and Recommendations 📝

- **LuaJIT Setup** 🐍: Ensure the LuaJIT version embedded in the application is correctly compiled and linked.
- **Pointer Management** 🔒: Handle pointers carefully to avoid memory corruption or leaks.
- **Thorough Documentation** 📖: Adequately document all Delphi methods exposed to LuaJIT, detailing usage and parameter constraints.

## Conclusion 🎯

jetLua provides an efficient and robust solution for integrating LuaJIT scripting into Delphi applications. It empowers developers to leverage the dynamic flexibility of LuaJIT without compromising on Delphi's intrinsic strengths in typing and performance. By incorporating jetLua, developers can substantially augment the extensibility, maintainability, and customization potential of their Delphi applications.

Whether your objective is to facilitate runtime customization or implement complex script-based configurations, jetLua offers a powerful and accessible means for Delphi developers to achieve enhanced scripting integration 💪.

## Requirements ✅

- **Delphi 12.2 or higher** 🛠️
- **Windows 10 or higher** 🖥️ (Tested on Windows 11 64-bit, version 23H2)

### Contributing

Contributions to **jetLua** are highly encouraged. Please feel free to submit issues, suggest new features, or create pull requests to expand the capabilities and robustness of the scripting engine.

### License

**jetLua** is distributed under the 🆓 **BSD-3-Clause License**, allowing for redistribution and use in both source and binary forms, with or without modification, under specific conditions. See the [LICENSE](https://github.com/tinyBigGAMES/jetLua?tab=BSD-3-Clause-1-ov-file#BSD-3-Clause-1-ov-file) file for more details.

### Support

- <a href="https://github.com/tinyBigGAMES/jetLua/issues" target="_blank">Issues</a>
- <a href="https://github.com/tinyBigGAMES/jetLua/discussions" target="_blank">Discussions</a>
- <a href="https://learndelphi.org/" target="_blank">Learn Delphi</a>
- <a href="https://luajit.org/" target="_blank">Learn LuaJIT</a>
---
For any professional Delphi developer interested in enhancing application flexibility with scripting capabilities, jetLua offers a tested and reliable solution that keeps everything self-contained and performant. 🚀

<p align="center">
<img src="media/delphi.png" alt="Delphi">
</p>
<h5 align="center">

Made with :heart: in Delphi
</h5>
