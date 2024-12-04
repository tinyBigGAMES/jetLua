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

 https://github.com/tinyBigGAMES/Chandra

 BSD 3-Clause License

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

 ------------------------------------------------------------------------------
 This project used the following open-source libraries:
  - LuaJIT (https://github.com/LuaJIT/LuaJIT)

 ------------------------------------------------------------------------------

 jetLua Usage Notes:
 ===================
 1. Integrated Lua Version
    jetLua uses LuaJIT, which is statically compiled into the Delphi
    application.
    - There are no external DLL dependencies to manage, ensuring portability
      and simplicity.

 2. Automatic Registration of Delphi Routines
    jetLua automatically registers native Delphi routines that are declared as
    published class methods.
    - You don’t need to manually bind methods to Lua; simply mark the desired
      routines as `published`.

 3. Supported Parameter and Return Types
    jetLua supports only basic types and pointers for parameters and return
    values.
    - Supported types include:
      - string
      - float (single or double)
      - Boolean
      - pointer
    - When designing methods for Lua, ensure all parameters and return types
      use these supported types.

 4. Pointer Management
    Pointers created on the native (Delphi) side must be managed by native
    code.
    - For example:
      - If you create a Delphi record and pass its pointer to Lua, Lua can
        hold and reference the pointer.
      - However, only native Delphi code can modify or operate on the
        underlying data.
      - If the pointer was dynamically allocated, it must be released on the
        Delphi side.

 5. Script Commands/Variables:
    - jetLua.version       - jetLua version (string)
    - jetLua.luaJitVersion - LuaJIT version (string)
    - jetLua.luaVersion    - Lua version (string)
    - dbg()                - Place in your Lua source to invoke the interactive
                             debugger

 6. Prerequisites
    - Delphi 12.2 or higher
    - Windows 10 or higher
    - Tested on Windows 11 64-bit (23H2), Delphi 12.2

 7. Lua Garbage Collection (GC) Management
    Effective memory management is crucial for maintaining optimal performance
    in applications that embed Lua. jetLua provides a set of routines to
    control and monitor Lua's garbage collector (GC) directly from Delphi.
    Below are detailed explanations of these routines, including when and how
    to use them.

    7.1. SetGCStepSize(const AStep: Integer)
         What It Does:
           Sets the step multiplier for Lua's garbage collector. The step
           multiplier determines the amount of work the GC performs in each
           incremental step, influencing its aggressiveness.

         When to Use It:
           - Performance Optimization: Increase the step size to make GC
             more aggressive if memory usage is high.
           - Reducing Latency: Decrease the step size to spread GC workload,
             minimizing pauses in performance-critical applications.
           - Memory-Constrained Environments: Adjust step size to better manage
             limited memory resources.

         How to Use It:
           // Example: Setting the GC step size to 200%
           SetGCStepSize(200);

         Parameters:
           - AStep: A positive integer representing the GC step multiplier.
                    Lua's default is typically around 200. Higher values make
                    GC more aggressive.

         Considerations:
           - Balance: Too high a value may increase CPU usage, while too low
             may lead to inadequate garbage collection.
           - Testing: Experiment with different values to find the optimal
             balance for your application.

    7.2. GetGCStepSize(): Integer
         What It Does:
           Retrieves the current step multiplier value of Lua's garbage
           collector, allowing you to monitor the GC's configuration.

         When to Use It:
           - Monitoring: Keep track of the current GC settings.
           - Debugging: Diagnose memory-related issues by understanding GC
             behavior.
           - Dynamic Adjustments: Inform further adjustments based on runtime
             conditions.

         How to Use It:
           var
             CurrentStepSize: Integer;
           begin
             CurrentStepSize := GetGCStepSize();
             ShowMessage('Current GC Step Size: ' + IntToStr(CurrentStepSize));
           end;

         Returns:
           - An integer representing the current GC step multiplier.

         Considerations:
           - Regularly check to ensure GC is configured as intended, especially
             in complex applications.

    7.3. GetGCMemoryUsed(): Integer
         What It Does:
           Returns the amount of memory currently used by Lua's garbage
           collector, measured in bytes.

         When to Use It:
           - Memory Monitoring: Track memory usage trends to identify leaks or
             excessive consumption.
           - Performance Tuning: Use memory usage data to adjust GC settings.
           - Resource Management: Ensure memory usage stays within acceptable
             limits in constrained environments.

         How to Use It:
           var
             MemoryUsed: Integer;
           begin
             MemoryUsed := GetGCMemoryUsed();
             ShowMessage('Lua GC Memory Used: ' + IntToStr(MemoryUsed) +
              ' bytes');
           end;

         Returns:
           - An integer representing the memory usage of Lua's GC in bytes.

         Considerations:
           - Combine memory data with GC step size and performance metrics for
             informed memory management decisions.

    7.4. CollectGarbage()
         What It Does:
           Initiates an immediate garbage collection cycle in Lua, forcing the
           GC to reclaim memory from unused objects.

         When to Use It:
           - Explicit Memory Management: Trigger GC during moments when
             temporary pauses are acceptable, such as after loading large
             datasets.
           - Resource Cleanup: Free up memory promptly after operations that
             generate significant temporary objects.
           - Manual Control: Supplement automated GC triggers to maintain
             optimal performance.

         How to Use It:
           begin
             CollectGarbage();
             ShowMessage('Lua garbage collection cycle initiated.');
           end;

         Considerations:
           - Performance Impact: Forcing GC can cause temporary pauses; use
             judiciously to avoid negatively impacting user experience.
           - Timing: Identify suitable application moments, like idle times, to
             perform manual GC.
           - Complementary Use: Combine manual GC with automated settings for
             balanced memory management.

    Detailed Guidance on Using Lua GC Management Routines

    Overview of Lua's Garbage Collector
      Lua's incremental garbage collector automatically manages memory by
      reclaiming unused objects in small steps to prevent long pauses.
      Adjusting the GC's behavior can optimize memory usage and application
      responsiveness.

    Best Practices and Considerations
      1. Understand Lua's GC Mechanics:
         - Familiarize yourself with Lua's incremental garbage collection to
            make informed adjustments.

      2. Avoid Overusing Manual GC Triggers:
         - Excessive CollectGarbage calls can degrade performance. Use them
           sparingly.

      3. Monitor Application Performance:
         - Assess the impact of GC adjustments on both memory usage and
           responsiveness.

      4. Test Across Scenarios:
         - Different workloads may respond differently to GC settings. Conduct
           thorough testing.

      5. Handle GC States Appropriately:
         - Ensure your application manages state changes introduced by garbage
           collection, especially with weak references.

      6. Stay Updated with Lua Versions:
         - GC behavior may vary between Lua versions. Ensure compatibility with
           the Lua version used by jetLua.

    Example Usage in a Delphi Application
      Below is a practical example demonstrating how to integrate and utilize
      the GC management routines within a Delphi application interfacing with
      Lua via jetLua.

       uses
        jetLua;

       var
         LuaState: PLua_State; // Assume this is initialized elsewhere

     Usage Example:
       procedure TForm1.ButtonOptimizeGCClick(Sender: TObject);
       begin
         try
           // Set GC step size to 150%
           SetGCStepSize(150);
           ShowMessage('GC Step Size set to 150%.');

           // Retrieve and display current step size
           ShowMessage('Current GC Step Size: ' + IntToStr(GetGCStepSize()));

           // Check memory usage
           ShowMessage('Lua GC Memory Used: ' + IntToStr(GetGCMemoryUsed()) +
             ' bytes');

           // Force a garbage collection cycle
           CollectGarbage();
           ShowMessage('Garbage collection cycle initiated.');
         except
           on E: Exception do
             ShowMessage('Error: ' + E.Message);
         end;
       end;

  Additional Notes
    - Lua Integration: Ensure that the Lua state (LuaState) is correctly
      initialized and managed within your application.

    - Error Handling: Implement robust error handling to manage scenarios where
      GC operations might fail or behave unexpectedly.

    - Performance Considerations: Adjusting the GC's step size can
      significantly impact application performance and memory usage. Test
      different configurations to identify the optimal settings for your use
      case.

 ------------------------------------------------------------------------------

>>> CHANGELOG <<<

Version 0.1.0
-------------
  - Initial release.

===============================================================================}

unit jetLua;

{$IF CompilerVersion >= 36.0}
  // Code specific to Delphi Athens (12.2) and above
{$ELSE}
  {$MESSAGE ERROR 'This code requires  Delphi Athens (12.2) or later'}
{$IFEND}

{$IFNDEF WIN64}
  // Generates a compile-time error if the target platform is not Win64
  {$MESSAGE Error 'Unsupported platform'}
{$ENDIF}

{$Z4}  // Sets the enumeration size to 4 bytes
{$A8}  // Sets the alignment for record fields to 8 bytes

{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}

{$WARN UNIT_PLATFORM OFF}
{$WARN UNIT_DEPRECATED OFF}

interface

{$REGION ' USES '}
uses
  WinApi.Windows,
  System.Math,
  System.Classes,
  System.IOUtils,
  System.AnsiStrings,
  System.Generics.Collections,
  System.SysUtils,
  System.TypInfo,
  System.RTTI;
{$ENDREGION}

{$REGION ' JETLUA '}
/// <summary>
/// Defines the major version component of the jetLua library.
/// </summary>
const
  JETLUA_VERSION_MAJOR = '0';

/// <summary>
/// Defines the minor version component of the jetLua library.
/// </summary>
  JETLUA_VERSION_MINOR = '1';

/// <summary>
/// Defines the patch version component of the jetLua library.
/// </summary>
  JETLUA_VERSION_PATCH = '0';

/// <summary>
/// Combines the major, minor, and patch versions to form the full version string of the jetLua library.
/// </summary>
  JETLUA_VERSION_FULL = JETLUA_VERSION_MAJOR + '.' + JETLUA_VERSION_MINOR + '.' + JETLUA_VERSION_PATCH;

/// <summary>
/// Represents an exception specific to the jetLua library.
/// </summary>
type
  /// <summary>
  /// EjetLuaException is the base exception class for all jetLua-related errors.
  /// </summary>
  EjetLuaException = class(Exception);

  /// <summary>
  /// TjetLuaMethodWrapper is responsible for wrapping Delphi methods to be callable from Lua.
  /// It handles the conversion of parameters and return values between Lua and Delphi, ensuring type compatibility and proper execution flow.
  /// </summary>
  TjetLuaMethodWrapper = class
  protected
    /// <summary>
    /// Holds the RTTI method information of the Delphi method being wrapped.
    /// RTTI (Run-Time Type Information) is used to dynamically invoke methods and handle type conversions.
    /// </summary>
    FMethod: TRttiMethod;

    /// <summary>
    /// Provides runtime type information context.
    /// TRttiContext is essential for accessing RTTI data about types, methods, and properties at runtime.
    /// </summary>
    FContext: TRttiContext;

    /// <summary>
    /// References the class type that contains the method being wrapped.
    /// This allows the wrapper to correctly invoke instance methods on the appropriate class.
    /// </summary>
    FClass: TClass;

    /// <summary>
    /// Converts a Lua parameter at the specified stack index to a native Delphi value based on the provided RTTI type.
    /// This method ensures that Lua data types are accurately translated to their Delphi counterparts, facilitating seamless integration.
    /// </summary>
    /// <param name="AState">Pointer to the Lua state.</param>
    /// <param name="AParamType">The RTTI type information of the parameter.</param>
    /// <param name="AStackIndex">The index on the Lua stack where the parameter is located.</param>
    /// <returns>The converted TValue representing the native Delphi value.</returns>
    /// <exception cref="EjetLuaException">Thrown if the conversion fails due to type mismatch or invalid data.</exception>
    function ConvertLuaToNative(const AState: Pointer; const AParamType: TRttiType; const AStackIndex: Integer): TValue;

    /// <summary>
    /// Pushes a native Delphi value onto the Lua stack, converting it to a Lua-compatible format.
    /// This method ensures that Delphi data types are accurately translated to Lua types, enabling Lua scripts to utilize Delphi values seamlessly.
    /// </summary>
    /// <param name="AState">Pointer to the Lua state.</param>
    /// <param name="AValue">The TValue to be pushed onto the Lua stack.</param>
    /// <returns>The number of return values pushed onto the Lua stack.</returns>
    /// <exception cref="EjetLuaException">Thrown if the push operation fails due to unsupported types or Lua state issues.</exception>
    function ConvertNativeToLua(const AState: Pointer; const AValue: TValue): Integer;
  public
    /// <summary>
    /// Initializes a new instance of the TjetLuaMethodWrapper class.
    /// Sets up the method wrapper by storing the RTTI method and the associated class type.
    /// </summary>
    /// <param name="AMethod">The RTTI method to be wrapped.</param>
    /// <param name="AClass">The class type that contains the method.</param>
    /// <remarks>
    /// This constructor prepares the method wrapper for execution by storing necessary RTTI information and the class reference.
    /// </remarks>
    constructor Create(const AMethod: TRttiMethod; const AClass: TClass);

    /// <summary>
    /// Executes the wrapped Delphi method using the provided Lua state.
    /// Handles the retrieval of parameters from Lua, invocation of the Delphi method, and pushing of return values back to Lua.
    /// </summary>
    /// <param name="AState">Pointer to the Lua state.</param>
    /// <returns>The number of return values pushed onto the Lua stack.</returns>
    /// <exception cref="EjetLuaException">Thrown if the method execution fails due to runtime errors or type mismatches.</exception>
    function Execute(const AState: Pointer): Integer;
  end;

  /// <summary>
  /// TjetLua is the main class that integrates Lua scripting into Delphi applications using the jetLua library.
  /// It manages the Lua state, registers Delphi routines, handles variable interactions, and manages Lua's garbage collector.
  /// </summary>
  /// <remarks>
  /// jetLua leverages LuaJIT, a Just-In-Time Compiler for Lua, which is statically compiled into the Delphi application.
  /// This integration ensures high performance and eliminates external DLL dependencies, enhancing portability and simplifying deployment.
  /// </remarks>
  TjetLua = class
  protected
    /// <summary>
    /// Provides runtime type information context for registering and invoking methods.
    /// TRttiContext is essential for accessing RTTI data about types, methods, and properties at runtime.
    /// </summary>
    FContext: TRttiContext;

    /// <summary>
    /// A dictionary mapping method names to their corresponding TjetLuaMethodWrapper instances.
    /// TObjectDictionary is used to efficiently manage and access method wrappers by name, facilitating method invocation from Lua scripts.
    /// </summary>
    FWrappers: TObjectDictionary<string, TjetLuaMethodWrapper>;

    /// <summary>
    /// Pointer to the Lua state managed by jetLua.
    /// The Lua state represents the execution environment for Lua scripts, maintaining variables, functions, and other state information.
    /// </summary>
    FState: Pointer;

    /// <summary>
    /// Determines the step multiplier for Lua's garbage collector.
    /// This value influences the aggressiveness of the garbage collection process, balancing memory usage and performance.
    /// </summary>
    FGCStep: Integer;

    /// <summary>
    /// Registers a Delphi method with the Lua state, allowing it to be called from Lua scripts.
    /// This involves creating a method wrapper and associating it with the Lua state for invocation.
    /// </summary>
    /// <param name="AMethod">The RTTI method to be registered.</param>
    /// <param name="AClass">The class type that contains the method.</param>
    /// <remarks>
    /// Only methods that are published and adhere to supported parameter and return types can be registered.
    /// This automatic registration simplifies the integration process, eliminating the need for manual bindings.
    /// </remarks>
    procedure RegisterMethod(const AMethod: TRttiMethod; const AClass: TClass);

    /// <summary>
    /// Validates that a given Delphi method is suitable for registration with Lua.
    /// Ensures that the method adheres to supported parameter and return types, preventing runtime errors during execution.
    /// </summary>
    /// <param name="AMethod">The RTTI method to be validated.</param>
    /// <exception cref="EjetLuaException">Thrown if the method contains unsupported types or does not meet the required criteria.</exception>
    /// <remarks>
    /// jetLua supports only basic types and pointers for parameters and return values.
    /// Supported types include:
    /// <list type="bullet">
    ///   <item>string</item>
    ///   <item>float (single or double)</item>
    ///   <item>Boolean</item>
    ///   <item>pointer</item>
    /// </list>
    /// Methods with parameters or return types outside of these supported types will be rejected.
    /// </remarks>
    procedure ValidateMethod(const AMethod: TRttiMethod);

    /// <summary>
    /// Pushes a TValue onto the Lua stack, converting it to a Lua-compatible type.
    /// Handles the translation of Delphi types to their Lua equivalents, ensuring accurate data representation.
    /// </summary>
    /// <param name="AValue">The TValue to push.</param>
    /// <returns>True if the value was successfully pushed; otherwise, False.</returns>
    /// <exception cref="EjetLuaException">Thrown if the value cannot be converted or pushed due to type incompatibility or Lua state issues.</exception>
    /// <remarks>
    /// This method abstracts the complexity of data type conversion, allowing developers to work with Delphi's strongly-typed values while maintaining Lua's dynamic nature.
    /// </remarks>
    function PushValueToLua(const AValue: TValue): Boolean;

    /// <summary>
    /// Retrieves a TValue from the Lua stack at the specified index, converting it to a Delphi-compatible type.
    /// This method ensures that Lua values are accurately translated to their Delphi counterparts, facilitating seamless data exchange.
    /// </summary>
    /// <param name="AStackIndex">The index on the Lua stack where the value is located.</param>
    /// <returns>The retrieved TValue.</returns>
    /// <exception cref="EjetLuaException">Thrown if the value cannot be converted due to type mismatches or Lua state issues.</exception>
    /// <remarks>
    /// This method abstracts the complexity of data type conversion, allowing developers to work with Delphi's strongly-typed values while maintaining Lua's dynamic nature.
    /// </remarks>
    function GetValueFromLua(const AStackIndex: Integer): TValue;

    /// <summary>
    /// Converts a Lua parameter record to a Delphi string.
    /// Handles the extraction and conversion of Lua parameter data to a format suitable for Delphi methods.
    /// </summary>
    /// <param name="AValue">The TVarRec representing the Lua parameter.</param>
    /// <returns>The converted string value.</returns>
    /// <remarks>
    /// This utility function is essential for handling string parameters passed from Lua to Delphi methods.
    /// It ensures that Lua strings are correctly interpreted and utilized within Delphi's type system.
    /// </remarks>
    function LuaParamToString(const AValue: TVarRec): string;

    /// <summary>
    /// Pushes a pointer onto the Lua stack, optionally associating it with type information.
    /// This allows Lua scripts to interact with Delphi-managed memory through pointers, enabling advanced integrations.
    /// </summary>
    /// <param name="APtr">The pointer to push.</param>
    /// <param name="ATypeInfo">Optional type information for the pointer.</param>
    /// <returns>True if the pointer was successfully pushed; otherwise, False.</returns>
    /// <exception cref="EjetLuaException">Thrown if the pointer cannot be pushed due to type incompatibility or Lua state issues.</exception>
    /// <remarks>
    /// When using pointers, it's crucial to manage memory correctly to prevent leaks or undefined behavior.
    /// Pointers created on the Delphi side must be managed by native code, ensuring that Lua scripts do not inadvertently modify or free memory.
    /// </remarks>
    function PushPointer(const APtr: Pointer; const ATypeInfo: PTypeInfo = nil): Boolean;

    /// <summary>
    /// Checks the result of a Lua operation for errors and raises an exception if an error occurred.
    /// This method centralizes error handling, ensuring that Lua-related errors are consistently managed.
    /// </summary>
    /// <param name="AError">The error code returned by a Lua operation.</param>
    /// <exception cref="EjetLuaException">Thrown if the error code indicates a failure in the Lua operation.</exception>
    /// <remarks>
    /// Proper error checking is essential for robust applications. This method ensures that any Lua errors are promptly identified and handled.
    /// </remarks>
    procedure CheckLuaError(const AError: Integer);

    /// <summary>
    /// Saves the compiled Lua bytecode to a specified stream.
    /// This allows for precompilation of Lua scripts, improving load times and protecting source code.
    /// </summary>
    /// <param name="AStream">The stream where the bytecode will be saved.</param>
    /// <exception cref="EjetLuaException">Thrown if the bytecode cannot be saved due to stream issues or compilation errors.</exception>
    /// <remarks>
    /// Precompiling Lua scripts to bytecode can enhance performance and provide a layer of obfuscation for sensitive scripts.
    /// However, it is important to manage bytecode compatibility with the Lua version used by jetLua.
    /// </remarks>
    procedure SaveByteCode(const AStream: TStream);

    /// <summary>
    /// Bundles a Lua script from an input file and outputs the bundled version to an output file.
    /// Bundling can include processes like compression, encryption, or bytecode compilation to enhance performance and security.
    /// </summary>
    /// <param name="AInFilename">The path to the input Lua script file.</param>
    /// <param name="AOutFilename">The path where the bundled Lua script will be saved.</param>
    /// <exception cref="EjetLuaException">Thrown if the bundling process fails due to file access issues or processing errors.</exception>
    /// <remarks>
    /// Bundling scripts can be an effective way to manage and distribute Lua scripts within Delphi applications, ensuring that scripts are optimized and secured.
    /// </remarks>
    procedure Bundle(const AInFilename: string; const AOutFilename: string);

  public
    /// <summary>
    /// Initializes a new instance of the TjetLua class, setting up the necessary contexts and dictionaries.
    /// Prepares the Lua environment by initializing RTTI contexts and method wrappers.
    /// </summary>
    /// <remarks>
    /// The constructor sets up the foundational elements required for jetLua to function, including RTTI context initialization and method dictionary setup.
    /// It ensures that the Lua state is ready for script execution and interaction with Delphi routines.
    /// </remarks>
    constructor Create(); virtual;

    /// <summary>
    /// Finalizes an instance of the TjetLua class, releasing all associated resources.
    /// Cleans up the Lua state, method wrappers, and RTTI contexts to prevent memory leaks.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that all resources allocated during the lifetime of the TjetLua instance are properly released.
    /// This includes closing the Lua state and freeing method wrappers to maintain application stability.
    /// </remarks>
    destructor Destroy(); override;

    /// <summary>
    /// Opens and initializes the Lua state, preparing it for script execution and interaction.
    /// This involves setting up the Lua environment, loading standard libraries, and preparing for script loading.
    /// </summary>
    /// <returns>True if the Lua state was successfully opened; otherwise, False.</returns>
    /// <exception cref="EjetLuaException">Thrown if the Lua state cannot be initialized due to memory allocation failures or other critical errors.</exception>
    /// <remarks>
    /// Opening the Lua state is a critical step in enabling Lua script execution within the Delphi application.
    /// Successful initialization ensures that scripts can be loaded, executed, and interacted with seamlessly.
    /// </remarks>
    function Open(): Boolean;

    /// <summary>
    /// Closes the Lua state, releasing all associated resources.
    /// Ensures that the Lua environment is properly terminated, preventing resource leaks and ensuring application stability.
    /// </summary>
    /// <remarks>
    /// Closing the Lua state is essential when the Lua environment is no longer needed or before the application terminates.
    /// This method ensures that all Lua-related resources are cleanly released.
    /// </remarks>
    procedure Close();

    /// <summary>
    /// Resets the Lua state, clearing all loaded scripts, variables, and registered methods.
    /// This provides a clean slate for reinitializing the Lua environment without residual data.
    /// </summary>
    /// <remarks>
    /// Resetting the Lua state can be useful in scenarios where scripts need to be reloaded or when the application requires a fresh Lua environment.
    /// The method also triggers pre- and post-reset event handlers for custom actions.
    /// </remarks>
    /// <exception cref="EjetLuaException">Thrown if the reset process encounters errors during Lua state manipulation.</exception>
    procedure Reset();

    /// <summary>
    /// Automatically registers all published class methods of the specified class with Lua, optionally under a given table name.
    /// This simplifies the process of exposing Delphi methods to Lua scripts, eliminating the need for manual bindings.
    /// </summary>
    /// <param name="AClass">The class whose published methods are to be registered.</param>
    /// <param name="ATableName">Optional. The name of the Lua table under which methods will be registered. If empty, methods are registered globally.</param>
    /// <remarks>
    /// Only methods that are published and adhere to supported parameter and return types will be registered.
    /// This automatic registration leverages RTTI to discover and bind suitable methods, streamlining the integration process.
    /// </remarks>
    /// <example>
    /// <code>
    /// type
    ///   TMyClass = class
    ///   published
    ///     function AddNumbers(a, b: Double): Double;
    ///   end;
    ///
    /// var
    ///   Lua: TjetLua;
    /// begin
    ///   Lua := TjetLua.Create;
    ///   Lua.Open;
    ///   Lua.RegisterRoutines(TMyClass, 'Math');
    ///   Lua.LoadString('print(Math.AddNumbers(5, 10))');
    /// end;
    /// </code>
    /// </example>
    procedure RegisterRoutines(AClass: TClass; const ATableName: string = '');

    /// <summary>
    /// Loads and executes a Lua script file.
    /// This method allows for the integration of external Lua scripts, enabling dynamic script execution within the Delphi application.
    /// </summary>
    /// <param name="AFilename">The path to the Lua script file.</param>
    /// <param name="AAutoRun">If True, the script will be executed immediately after loading.</param>
    /// <returns>True if the file was successfully loaded (and executed, if AAutoRun is True); otherwise, False.</returns>
    /// <exception cref="EjetLuaException">Thrown if the file cannot be loaded or executed due to file access issues, syntax errors, or runtime errors within the script.</exception>
    /// <remarks>
    /// This method supports the dynamic inclusion of Lua scripts, allowing developers to modify or extend application behavior without recompiling Delphi code.
    /// It is essential to ensure that the loaded scripts adhere to the supported type constraints and do not introduce security vulnerabilities.
    /// </remarks>
    /// <example>
    /// <code>
    /// if Lua.LoadFile('C:\Scripts\Initialize.lua') then
    ///   ShowMessage('Script loaded and executed successfully.')
    /// else
    ///   ShowMessage('Failed to load script.')
    /// end;
    /// </code>
    /// </example>
    function LoadFile(const AFilename: string; const AAutoRun: Boolean = True): Boolean;

    /// <summary>
    /// Loads and optionally executes a Lua script provided as a string.
    /// This allows for the execution of dynamically generated or in-memory scripts without the need for external files.
    /// </summary>
    /// <param name="AData">The Lua script as a string.</param>
    /// <param name="AAutoRun">If True, the script will be executed immediately after loading.</param>
    /// <exception cref="EjetLuaException">Thrown if the script cannot be loaded or executed due to syntax errors or runtime errors within the script.</exception>
    /// <remarks>
    /// Loading scripts from strings is useful for scenarios where scripts are generated on-the-fly or retrieved from non-file sources such as databases or network streams.
    /// </remarks>
    /// <example>
    /// <code>
    /// Lua.LoadString('print("Hello from Lua!")', True);
    /// </code>
    /// </example>
    procedure LoadString(const AData: string; const AAutoRun: Boolean = True);

    /// <summary>
    /// Loads and optionally executes a Lua script from a memory buffer.
    /// This method is ideal for scenarios where scripts are stored in memory rather than in files or strings.
    /// </summary>
    /// <param name="AData">Pointer to the buffer containing the Lua script.</param>
    /// <param name="ASize">The size of the buffer in bytes.</param>
    /// <param name="AAutoRun">If True, the script will be executed immediately after loading.</param>
    /// <exception cref="EjetLuaException">Thrown if the buffer cannot be read or the script cannot be executed due to syntax errors or runtime errors within the script.</exception>
    /// <remarks>
    /// Loading scripts from memory buffers is useful for embedded scripts or when scripts are received from network sources or other non-file mediums.
    /// Proper memory management must be ensured to prevent buffer overflows or invalid memory access.
    /// </remarks>
    /// <example>
    /// <code>
    /// var
    ///   Script: string;
    /// begin
    ///   Script := 'print("Script from memory buffer")';
    ///   Lua.LoadBuffer(Pointer(Script), Length(Script), True);
    /// end;
    /// </code>
    /// </example>
    procedure LoadBuffer(const AData: Pointer; const ASize: NativeUInt; const AAutoRun: Boolean = True);

    /// <summary>
    /// Checks if a specific Lua routine (function) exists within the current Lua state.
    /// This allows Delphi code to verify the availability of Lua functions before attempting to call them.
    /// </summary>
    /// <param name="AName">The name of the Lua routine to check.</param>
    /// <returns>True if the routine exists; otherwise, False.</returns>
    /// <remarks>
    /// This method is useful for ensuring that required Lua functions are defined before invocation, preventing runtime errors.
    /// It can also be used to conditionally execute Lua functions based on their presence.
    /// </remarks>
    /// <example>
    /// <code>
    /// if Lua.RoutineExist('Initialize') then
    ///   Lua.Call('Initialize', [])
    /// else
    ///   ShowMessage('Initialize function not found in Lua script.');
    /// end;
    /// </code>
    /// </example>
    function RoutineExist(const AName: string): Boolean;

    /// <summary>
    /// Checks if a specific Lua variable exists within the current Lua state.
    /// This allows Delphi code to verify the existence of Lua variables before attempting to access or modify them.
    /// </summary>
    /// <param name="AName">The name of the Lua variable to check.</param>
    /// <returns>True if the variable exists; otherwise, False.</returns>
    /// <remarks>
    /// Verifying the existence of Lua variables can prevent undefined behavior and runtime errors when interacting with Lua scripts.
    /// This method is particularly useful when dealing with dynamic scripts where variable definitions may vary.
    /// </remarks>
    /// <example>
    /// <code>
    /// if Lua.VariableExist('PlayerHealth') then
    ///   var Health := Lua.GetVariable('PlayerHealth').AsInteger;
    /// else
    ///   ShowMessage('PlayerHealth variable not defined in Lua script.');
    /// end;
    /// </code>
    /// </example>
    function VariableExist(const AName: string): Boolean;

    /// <summary>
    /// Sets the value of a Lua variable from Delphi.
    /// This enables Delphi code to modify Lua script variables, facilitating interaction and data exchange between Delphi and Lua.
    /// </summary>
    /// <param name="AName">The name of the Lua variable to set.</param>
    /// <param name="AValue">The TValue to assign to the Lua variable.</param>
    /// <exception cref="EjetLuaException">Thrown if the variable cannot be set due to type mismatches or Lua state issues.</exception>
    /// <remarks>
    /// This method allows for dynamic modification of Lua script variables, enabling Delphi applications to influence Lua script behavior at runtime.
    /// Proper type handling is essential to ensure that Lua scripts interpret the values correctly.
    /// </remarks>
    /// <example>
    /// <code>
    /// Lua.SetVariable('GameMode', 'Hardcore');
    /// </code>
    /// </example>
    procedure SetVariable(const AName: string; const AValue: TValue);

    /// <summary>
    /// Retrieves the value of a Lua variable from Delphi.
    /// This allows Delphi code to access and utilize Lua script variables, enabling data retrieval and decision-making based on Lua state.
    /// </summary>
    /// <param name="AName">The name of the Lua variable to retrieve.</param>
    /// <returns>The TValue representing the Lua variable's value.</returns>
    /// <exception cref="EjetLuaException">Thrown if the variable cannot be retrieved due to it not existing or Lua state issues.</exception>
    /// <remarks>
    /// This method facilitates the exchange of data from Lua scripts to Delphi applications, enabling responsive and interactive behaviors based on Lua state.
    /// Proper type handling is essential to ensure that Delphi applications interpret the values correctly.
    /// </remarks>
    /// <example>
    /// <code>
    /// if Lua.VariableExist('PlayerScore') then
    /// begin
    ///   var Score := Lua.GetVariable('PlayerScore').AsInteger;
    ///   ShowMessage('Player Score: ' + IntToStr(Score));
    /// end;
    /// </code>
    /// </example>
    function GetVariable(const AName: string): TValue;

    /// <summary>
    /// Calls a Lua function with the specified name and parameters, returning the result.
    /// This method enables Delphi applications to invoke Lua functions dynamically, passing data and receiving results seamlessly.
    /// </summary>
    /// <param name="AName">The name of the Lua function to call.</param>
    /// <param name="AParams">An array of parameters to pass to the Lua function.</param>
    /// <returns>The TValue representing the Lua function's return value.</returns>
    /// <exception cref="EjetLuaException">Thrown if the function cannot be called due to it not existing, parameter mismatches, or runtime errors within the Lua function.</exception>
    /// <remarks>
    /// This method abstracts the complexity of invoking Lua functions from Delphi, handling parameter conversion and return value retrieval automatically.
    /// It is essential to ensure that the Lua function exists and that the provided parameters match the expected types to prevent runtime errors.
    /// </remarks>
    /// <example>
    /// <code>
    /// var
    ///   Result: TValue;
    /// begin
    ///   Result := Lua.Call('CalculateScore', [10, 20]);
    ///   ShowMessage('Calculated Score: ' + Result.AsInteger.ToString);
    /// end;
    /// </code>
    /// </example>
    function Call(const AName: string; const AParams: array of const): TValue;

    /// <summary>
    /// Compiles a Lua script to bytecode and writes it to the specified stream.
    /// This allows for precompilation and storage of Lua scripts in a binary format, enhancing load times and providing source code protection.
    /// </summary>
    /// <param name="AFilename">The path to the Lua script file to compile.</param>
    /// <param name="AStream">The stream where the compiled bytecode will be written.</param>
    /// <param name="ACleanOutput">If True, the output will be optimized for size by removing unnecessary debug information.</param>
    /// <exception cref="EjetLuaException">Thrown if the compilation fails due to syntax errors or file access issues.</exception>
    /// <remarks>
    /// Compiling Lua scripts to bytecode can improve performance by reducing parsing overhead and can also obscure the original source code.
    /// However, bytecode is typically specific to the Lua version used, so compatibility must be maintained.
    /// </remarks>
    /// <example>
    /// <code>
    /// var
    ///   ByteCodeStream: TMemoryStream;
    /// begin
    ///   ByteCodeStream := TMemoryStream.Create;
    ///   try
    ///     Lua.CompileToStream('Script.lua', ByteCodeStream, True);
    ///     // ByteCodeStream now contains the compiled bytecode
    ///   finally
    ///     ByteCodeStream.Free;
    ///   end;
    /// end;
    /// </code>
    /// </example>
    procedure CompileToStream(const AFilename: string; const AStream: TStream; const ACleanOutput: Boolean);

    /// <summary>
    /// Adds a directory to Lua's package search path, allowing Lua scripts to require modules from the specified path.
    /// This enables Lua scripts to include and utilize external modules stored in custom directories.
    /// </summary>
    /// <param name="APath">The directory path to add to Lua's search paths.</param>
    /// <exception cref="EjetLuaException">Thrown if the path cannot be added due to Lua state issues or invalid path formats.</exception>
    /// <remarks>
    /// Modifying the Lua package search path is essential for organizing and managing Lua modules effectively.
    /// This method ensures that Lua scripts can locate and load modules from additional directories beyond the default paths.
    /// </remarks>
    /// <example>
    /// <code>
    /// Lua.AddSearchPath('C:\LuaModules');
    /// Lua.LoadString('local mod = require("MyModule"); mod.Execute();', True);
    /// </code>
    /// </example>
    procedure AddSearchPath(const APath: string);

    /// <summary>
    /// Executes a formatted print statement in Lua, similar to Lua's print function.
    /// This allows Delphi applications to send formatted output to Lua's standard output, facilitating debugging and logging.
    /// </summary>
    /// <param name="AText">The format string.</param>
    /// <param name="AArgs">An array of arguments to format into the string.</param>
    /// <exception cref="EjetLuaException">Thrown if the print operation fails due to Lua state issues or formatting errors.</exception>
    /// <remarks>
    /// This method provides a convenient way to output formatted strings from Delphi to Lua, enhancing interactivity and debugging capabilities.
    /// </remarks>
    /// <example>
    /// <code>
    /// Lua.Print('Player %s has scored %d points.', ['Alice', 150]);
    /// </code>
    /// </example>
    procedure Print(const AText: string; const AArgs: array of const);

    /// <summary>
    /// Executes a formatted print statement in Lua, appending a newline at the end.
    /// This enhances readability by ensuring that each print statement starts on a new line.
    /// </summary>
    /// <param name="AText">The format string.</param>
    /// <param name="AArgs">An array of arguments to format into the string.</param>
    /// <exception cref="EjetLuaException">Thrown if the print operation fails due to Lua state issues or formatting errors.</exception>
    /// <remarks>
    /// Similar to the Print method, PrintLn ensures that output is neatly organized, making logs and debug statements easier to read.
    /// </remarks>
    /// <example>
    /// <code>
    /// Lua.PrintLn('Game started at %s.', [FormatDateTime('hh:nn:ss', Now)]);
    /// </code>
    /// </example>
    procedure PrintLn(const AText: string; const AArgs: array of const);

    /// <summary>
    /// Checks if a payload script exists within the Lua state.
    /// Payload scripts are typically embedded or stored within the application for execution under specific conditions.
    /// </summary>
    /// <returns>True if a payload exists; otherwise, False.</returns>
    /// <remarks>
    /// This method is useful for verifying the presence of embedded scripts before attempting to execute them, preventing runtime errors.
    /// Payload management is essential for applications that rely on pre-packaged scripts for functionality or updates.
    /// </remarks>
    function PayloadExist(): Boolean;

    /// <summary>
    /// Stores a payload script by embedding it into an executable file.
    /// This allows for the distribution of Lua scripts within the application's executable, enhancing portability and security.
    /// </summary>
    /// <param name="ASourceFilename">The path to the source Lua script file.</param>
    /// <param name="AEXEFilename">The path to the executable file where the payload will be stored.</param>
    /// <returns>True if the payload was successfully stored; otherwise, False.</returns>
    /// <exception cref="EjetLuaException">Thrown if the payload cannot be stored due to file access issues or processing errors.</exception>
    /// <remarks>
    /// Embedding payloads within executables can simplify deployment by reducing the number of external files.
    /// However, it is crucial to manage payload sizes and ensure that the embedding process does not corrupt the executable.
    /// </remarks>
    /// <example>
    /// <code>
    /// if Lua.StorePayload('Payload.lua', 'Game.exe') then
    ///   ShowMessage('Payload successfully embedded.')
    /// else
    ///   ShowMessage('Failed to embed payload.');
    /// end;
    /// </code>
    /// </example>
    function StorePayload(const ASourceFilename, AEXEFilename: string): Boolean;

    /// <summary>
    /// Executes the stored payload script within the current executable's resource.
    /// This allows for the dynamic execution of embedded scripts, enabling functionalities such as updates or hidden features.
    /// </summary>
    /// <returns>True if the payload was successfully executed; otherwise, False.</returns>
    /// <exception cref="EjetLuaException">Thrown if the payload cannot be executed due to it not existing, execution errors, or Lua state issues.</exception>
    /// <remarks>
    /// Executing payloads enables applications to perform complex operations without exposing the underlying scripts directly.
    /// Proper error handling and validation are essential to ensure that payload execution does not introduce vulnerabilities.
    /// </remarks>
    /// <example>
    /// <code>
    /// if Lua.RunPayload() then
    ///   ShowMessage('Payload executed successfully.')
    /// else
    ///   ShowMessage('Failed to execute payload.');
    /// end;
    /// </code>
    /// </example>
    function RunPayload(): Boolean;

    /// <summary>
    /// Sets the step multiplier for Lua's garbage collector, influencing its aggressiveness.
    /// Adjusting the GC step size can optimize memory management based on application performance and memory usage requirements.
    /// </summary>
    /// <param name="AStep">A positive integer representing the GC step multiplier. Lua's default is typically around 200.</param>
    /// <exception cref="EjetLuaException">Thrown if the step size is invalid or if the GC cannot be adjusted due to Lua state issues.</exception>
    /// <remarks>
    /// <para>
    /// The step multiplier determines the amount of work the garbage collector performs in each incremental step. Higher values make the GC more aggressive, potentially reducing memory usage but increasing CPU usage.
    /// </para>
    /// <para>
    /// <strong>Usage Considerations:</strong>
    /// <list type="bullet">
    ///   <item><strong>Performance Optimization:</strong> Increase the step size to make GC more aggressive if memory usage is high.</item>
    ///   <item><strong>Reducing Latency:</strong> Decrease the step size to spread GC workload, minimizing pauses in performance-critical applications.</item>
    ///   <item><strong>Memory-Constrained Environments:</strong> Adjust step size to better manage limited memory resources.</item>
    /// </list>
    /// </para>
    /// <para>
    /// It is recommended to experiment with different values to find the optimal balance for your specific application needs.
    /// </para>
    /// </remarks>
    /// <example>
    /// <code>
    /// // Setting the GC step size to 200%
    /// Lua.SetGCStepSize(200);
    /// </code>
    /// </example>
    procedure SetGCStepSize(const AStep: Integer);

    /// <summary>
    /// Retrieves the current step multiplier value of Lua's garbage collector.
    /// This allows developers to monitor and adjust the GC's configuration dynamically based on runtime conditions.
    /// </summary>
    /// <returns>An integer representing the current GC step multiplier.</returns>
    /// <exception cref="EjetLuaException">Thrown if the GC step size cannot be retrieved due to Lua state issues.</exception>
    /// <remarks>
    /// Monitoring the GC step size is essential for understanding and optimizing garbage collection behavior.
    /// This method can be used in conjunction with SetGCStepSize to dynamically adjust GC settings based on application performance metrics.
    /// </remarks>
    /// <example>
    /// <code>
    /// var
    ///   CurrentStepSize: Integer;
    /// begin
    ///   CurrentStepSize := Lua.GetGCStepSize();
    ///   ShowMessage('Current GC Step Size: ' + IntToStr(CurrentStepSize));
    /// end;
    /// </code>
    /// </example>
    function GetGCStepSize(): Integer;

    /// <summary>
    /// Retrieves the amount of memory currently used by Lua's garbage collector, measured in bytes.
    /// This provides insights into memory consumption trends, aiding in performance tuning and leak detection.
    /// </summary>
    /// <returns>An integer representing the memory usage of Lua's GC in bytes.</returns>
    /// <exception cref="EjetLuaException">Thrown if the memory usage cannot be retrieved due to Lua state issues.</exception>
    /// <remarks>
    /// Understanding memory usage is crucial for maintaining optimal performance, especially in memory-constrained environments.
    /// This method allows developers to monitor and respond to memory usage patterns dynamically.
    /// </remarks>
    /// <example>
    /// <code>
    /// var
    ///   MemoryUsed: Integer;
    /// begin
    ///   MemoryUsed := Lua.GetGCMemoryUsed();
    ///   ShowMessage('Lua GC Memory Used: ' + IntToStr(MemoryUsed) + ' bytes');
    /// end;
    /// </code>
    /// </example>
    function GetGCMemoryUsed(): Integer;

    /// <summary>
    /// Initiates an immediate garbage collection cycle in Lua, forcing the GC to reclaim memory from unused objects.
    /// This method provides manual control over garbage collection, allowing developers to optimize memory usage during specific application states.
    /// </summary>
    /// <exception cref="EjetLuaException">Thrown if the garbage collection cycle cannot be initiated due to Lua state issues.</exception>
    /// <remarks>
    /// Forcing garbage collection can help manage memory proactively, especially after operations that generate significant temporary objects.
    /// However, it should be used judiciously to avoid introducing performance pauses.
    /// </remarks>
    /// <example>
    /// <code>
    /// begin
    ///   Lua.CollectGarbage();
    ///   ShowMessage('Lua garbage collection cycle initiated.');
    /// end;
    /// </code>
    /// </example>
    procedure CollectGarbage();

    /// <summary>
    /// Event handler that is called before the Lua state is reset.
    /// Can be overridden to perform custom actions, such as saving state or cleaning up resources.
    /// </summary>
    /// <remarks>
    /// This method provides a hook for developers to inject custom logic before the Lua environment is reset.
    /// Overriding this method allows for tailored pre-reset behaviors, enhancing application flexibility and control.
    /// </remarks>
    procedure OnBeforeReset(); virtual;

    /// <summary>
    /// Event handler that is called after the Lua state has been reset.
    /// Can be overridden to perform custom actions, such as reinitializing variables or re-registering methods.
    /// </summary>
    /// <remarks>
    /// This method provides a hook for developers to inject custom logic after the Lua environment has been reset.
    /// Overriding this method allows for tailored post-reset behaviors, ensuring that the Lua state is correctly reconfigured.
    /// </remarks>
    procedure OnAfterReset(); virtual;
  end;

{$ENDREGION}

implementation

{$L jetLua.o}

{$REGION ' COMMON '}
var
  Marshaller: TMarshaller;

function EnableVirtualTerminalProcessing(): DWORD;
var
  HOut: THandle;
  LMode: DWORD;
begin
  HOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if HOut = INVALID_HANDLE_VALUE then
  begin
    Result := GetLastError;
    Exit;
  end;

  if not GetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  LMode := LMode or ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  if not SetConsoleMode(HOut, LMode) then
  begin
    Result := GetLastError;
    Exit;
  end;

  Result := 0;  // Success
end;

function HasConsoleOutput: Boolean;
var
  Stdout: THandle;
begin
  Stdout := GetStdHandle(Std_Output_Handle);
  Win32Check(Stdout <> Invalid_Handle_Value);
  Result := Stdout <> 0;
end;

function IsValidWin64PE(const AFilePath: string): Boolean;
var
  LFile: TFileStream;
  LDosHeader: TImageDosHeader;
  LPEHeaderOffset: DWORD;
  LPEHeaderSignature: DWORD;
  LFileHeader: TImageFileHeader;
begin
  Result := False;

  if not FileExists(AFilePath) then
    Exit;

  LFile := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    // Check if file is large enough for DOS header
    if LFile.Size < SizeOf(TImageDosHeader) then
      Exit;

    // Read DOS header
    LFile.ReadBuffer(LDosHeader, SizeOf(TImageDosHeader));

    // Check DOS signature
    if LDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then // 'MZ'
      Exit;

      // Validate PE header offset
    LPEHeaderOffset := LDosHeader._lfanew;
    if LFile.Size < LPEHeaderOffset + SizeOf(DWORD) + SizeOf(TImageFileHeader) then
      Exit;

    // Seek to the PE header
    LFile.Position := LPEHeaderOffset;

    // Read and validate the PE signature
    LFile.ReadBuffer(LPEHeaderSignature, SizeOf(DWORD));
    if LPEHeaderSignature <> IMAGE_NT_SIGNATURE then // 'PE\0\0'
      Exit;

   // Read the file header
    LFile.ReadBuffer(LFileHeader, SizeOf(TImageFileHeader));

    // Check if it is a 64-bit executable
    if LFileHeader.Machine <> IMAGE_FILE_MACHINE_AMD64 then   Exit;

    // If all checks pass, it's a valid Win64 PE file
    Result := True;
  finally
    LFile.Free;
  end;
end;

function AddResFromMemory(const aModuleFile: string; const aName: string; aData: Pointer; aSize: Cardinal): Boolean;
var
  LHandle: THandle;
begin
  Result := False;
  if not TFile.Exists(aModuleFile) then Exit;
  LHandle := WinApi.Windows.BeginUpdateResourceW(PWideChar(aModuleFile), False);
  if LHandle <> 0 then
  begin
    WinApi.Windows.UpdateResourceW(LHandle, RT_RCDATA, PChar(aName), 1033 {ENGLISH, ENGLISH_US}, aData, aSize);
    Result := WinApi.Windows.EndUpdateResourceW(LHandle, False);
  end;
end;

function ResourceExists(aInstance: THandle; const aResName: string): Boolean;
begin
  Result := Boolean((FindResource(aInstance, PChar(aResName), RT_RCDATA) <> 0));
end;

function RemoveBOM(const AString: string): string; overload;
const
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
var
  LBytes: TBytes;
begin
  // Convert the input string to a byte array
  LBytes := TEncoding.UTF8.GetBytes(AString);

  // Check for UTF-8 BOM at the beginning
  if (Length(LBytes) >= 3) and
     (LBytes[0] = UTF8BOM[0]) and
     (LBytes[1] = UTF8BOM[1]) and
     (LBytes[2] = UTF8BOM[2]) then
  begin
    // Remove the BOM by copying the bytes after it
    Result := TEncoding.UTF8.GetString(LBytes, 3, Length(LBytes) - 3);
  end
  else
  begin
    // Return the original string if no BOM is detected
    Result := AString;
  end;
end;

function RemoveBOM(const ABytes: TBytes): TBytes; overload;
const
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  UTF16LEBOM: array[0..1] of Byte = ($FF, $FE);
  UTF16BEBOM: array[0..1] of Byte = ($FE, $FF);
var
  LStartIndex: Integer;
begin
  Result := ABytes;

  // Check for UTF-8 BOM
  if (Length(ABytes) >= 3) and
     (ABytes[0] = UTF8BOM[0]) and
     (ABytes[1] = UTF8BOM[1]) and
     (ABytes[2] = UTF8BOM[2]) then
  begin
    LStartIndex := 3; // Skip the UTF-8 BOM
  end
  // Check for UTF-16 LE BOM
  else if (Length(ABytes) >= 2) and
          (ABytes[0] = UTF16LEBOM[0]) and
          (ABytes[1] = UTF16LEBOM[1]) then
  begin
    LStartIndex := 2; // Skip the UTF-16 LE BOM
  end
  // Check for UTF-16 BE BOM
  else if (Length(ABytes) >= 2) and
          (ABytes[0] = UTF16BEBOM[0]) and
          (ABytes[1] = UTF16BEBOM[1]) then
  begin
    LStartIndex := 2; // Skip the UTF-16 BE BOM
  end
  else
  begin
    Exit; // No BOM found, return the original array
  end;

  // Create a new array without the BOM
  Result := Copy(ABytes, LStartIndex, Length(ABytes) - LStartIndex);
end;

function AsUTF8(const AText: string; const ARemoveBOM: Boolean=False): Pointer;
var
  LText: string;
begin
  if ARemoveBOM then
    LText := RemoveBOM(AText)
  else
    LText := AText;
  Result := Marshaller.AsUtf8(LText).ToPointer;
end;

{$ENDREGION}

{$REGION ' LUAJIT API '}

const
  LUAJIT_VERSION = 'LuaJIT 2.1.1732813678';

  LUA_RELEASE = 'Lua 5.1.4';

  LUA_REGISTRYINDEX = (-10000);
  LUA_GLOBALSINDEX = (-10002);

  LUA_TNIL = 0;
  LUA_TBOOLEAN = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER = 3;
  LUA_TSTRING = 4;
  LUA_TTABLE = 5;
  LUA_TFUNCTION = 6;
  LUA_TUSERDATA = 7;

  LUA_OK = 0;

  LUA_ERRRUN = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM = 4;
  LUA_ERRERR = 5;

  LUA_MULTRET = (-1);

  LUA_GCCOUNT = 3;
  LUA_GCSTEP = 5;

type
  Plua_State = Pointer;
  lua_Number = Double;
  lua_Integer = NativeInt;
  lua_CFunction = function(L: Plua_State): Integer; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: NativeUInt; ud: Pointer): Integer; cdecl;

function  luaL_loadbufferx(L: Plua_State; const buff: PUTF8Char; sz: NativeUInt; const name: PUTF8Char; const mode: PUTF8Char): Integer; cdecl; external;
function  lua_pcall(L: Plua_State; nargs: Integer; nresults: Integer; errfunc: Integer): Integer; cdecl; external;
function  lua_error(L: Plua_State): Integer; cdecl; external;
procedure lua_getfield(L: Plua_State; idx: Integer; const k: PUTF8Char); cdecl; external;
procedure lua_pushstring(L: Plua_State; const s: PUTF8Char); cdecl; external;
procedure lua_setfield(L: Plua_State; idx: Integer; const k: PUTF8Char); cdecl; external;
function  luaL_loadfile(L: Plua_State; const filename: PUTF8Char): Integer; cdecl; external;
function  lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl; external;
procedure lua_pushnil(L: Plua_State); cdecl; external;
function  luaL_newmetatable(L: Plua_State; const tname: PUTF8Char): Integer; cdecl; external;
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl; external;
function  lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external;
function  luaL_error(L: Plua_State; const fmt: PUTF8Char): Integer varargs; cdecl; external;
procedure lua_insert(L: Plua_State; idx: Integer); cdecl; external;
procedure lua_remove(L: Plua_State; idx: Integer); cdecl; external;
function  lua_newuserdata(L: Plua_State; sz: NativeUInt): Pointer; cdecl; external;
function  lua_type(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external;
function  luaL_loadstring(L: Plua_State; const s: PUTF8Char): Integer; cdecl; external;
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl; external;
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl; external;
procedure lua_pushboolean(L: Plua_State; b: Integer); cdecl; external;
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl; external;
procedure lua_createtable(L: Plua_State; narr: Integer; nrec: Integer); cdecl; external;
procedure lua_settop(L: Plua_State; idx: Integer); cdecl; external;
function  lua_gettop(L: Plua_State): Integer; cdecl; external;
function  luaL_loadbuffer(L: Plua_State; const buff: PUTF8Char; sz: NativeUInt; const name: PUTF8Char): Integer; cdecl; external;
function  lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer): Integer; cdecl; external;
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl; external;
function  lua_toboolean(L: Plua_State; idx: Integer): Integer; cdecl; external;
procedure lua_pushvalue(L: Plua_State; idx: Integer); cdecl; external;
function  lua_iscfunction(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_tolstring(L: Plua_State; idx: Integer; len: PNativeUInt): PUTF8Char; cdecl; external;
function  lua_isnumber(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_tonumber(L: Plua_State; idx: Integer): lua_Number; cdecl; external;
function  luaL_checklstring(L: Plua_State; numArg: Integer; l_: PNativeUInt): PUTF8Char; cdecl; external;
function  luaL_newstate(): Plua_State; cdecl; external;
procedure luaL_openlibs(L: Plua_State); cdecl; external;
procedure lua_close(L: Plua_State); cdecl; external;
function  lua_tointeger(L: Plua_State; idx: Integer): lua_Integer; cdecl; external;
function  lua_isstring(L: Plua_State; idx: Integer): Integer; cdecl; external;
function  lua_gc(L: Plua_State; what: Integer; data: Integer): Integer; cdecl; external;

{ compatibility }
function lua_istable(L: Plua_State; N: Integer): Boolean;
begin
  Result := lua_type(L, N) = LUA_TTABLE;
end;

function lua_isfunction(aState: Pointer; n: Integer): Boolean;
begin
  Result := Boolean(lua_type(aState, n) = LUA_TFUNCTION);
end;

function lua_isvariable(aState: Pointer; n: Integer): Boolean;
var
  aType: Integer;
begin
  aType := lua_type(aState, n);

  if (aType = LUA_TBOOLEAN) or (aType = LUA_TLIGHTUSERDATA) or (aType = LUA_TNUMBER) or (aType = LUA_TSTRING) then
    Result := True
  else
    Result := False;
end;

procedure lua_newtable(aState: Pointer);
begin
  lua_createtable(aState, 0, 0);
end;

procedure lua_pop(aState: Pointer; n: Integer);
begin
  lua_settop(aState, -n - 1);
end;

function lua_getglobal(L: Plua_State; const AName: PAnsiChar): Integer;
begin
  // Get the value directly from the globals table
  lua_getfield(L, LUA_GLOBALSINDEX, AName);

  // Return the type of the value
  Result := lua_type(L, -1);
end;

procedure lua_setglobal(aState: Pointer; aName: PAnsiChar);
begin
  lua_setfield(aState, LUA_GLOBALSINDEX, aName);
end;

procedure lua_pushcfunction(aState: Pointer; aFunc: lua_CFunction);
begin
  lua_pushcclosure(aState, aFunc, 0);
end;

procedure lua_register(aState: Pointer; aName: PAnsiChar; aFunc: lua_CFunction);
begin
  lua_pushcfunction(aState, aFunc);
  lua_setglobal(aState, aName);
end;

function lua_isnil(aState: Pointer; n: Integer): Boolean;
begin
  Result := Boolean(lua_type(aState, n) = LUA_TNIL);
end;

function lua_tostring(aState: Pointer; idx: Integer): string;
begin
  Result := string(lua_tolstring(aState, idx, nil));
end;

function luaL_dofile(aState: Pointer; aFilename: PAnsiChar): Integer;
Var
  Res: Integer;
begin
  Res := luaL_loadfile(aState, aFilename);
  if Res = 0 then
    Res := lua_pcall(aState, 0, 0, 0);
  Result := Res;
end;

function luaL_dostring(aState: Pointer; aStr: PAnsiChar): Integer;
Var
  Res: Integer;
begin
  Res := luaL_loadstring(aState, aStr);
  if Res = 0 then
    Res := lua_pcall(aState, 0, 0, 0);
  Result := Res;
end;

function luaL_dobuffer(aState: Pointer; aBuffer: Pointer; aSize: NativeUInt;
  aName: PAnsiChar): Integer;
var
  Res: Integer;
begin
  Res := luaL_loadbuffer(aState, aBuffer, aSize, aName);
  if Res = 0 then
    Res := lua_pcall(aState, 0, 0, 0);
  Result := Res;
end;

function lua_upvalueindex(i: Integer): Integer;
begin
  Result := LUA_GLOBALSINDEX - i;
end;

function luaL_checkstring(L: Plua_State; n: Integer): PAnsiChar;
begin
  Result := luaL_checklstring(L, n, nil);
end;

(*
procedure luaL_requiref(L: Plua_State; modname: PAnsiChar; openf: lua_CFunction; glb: Integer);
begin
  lua_pushcfunction(L, openf);
  lua_pushstring(L, modname);
  lua_call(L, 1, 1);
  lua_getfield(L, LUA_REGISTRYINDEX, '_LOADED');
  lua_pushvalue(L, -2);
  lua_setfield(L, -2, modname);
  lua_pop(L, 1);
  if glb <> 0 then
  begin
    lua_pushvalue(L, -1);
    lua_setglobal(L, modname);
  end;
end;
*)

procedure luaL_requiref(L: Plua_State; modname: PAnsiChar; openf: lua_CFunction; glb: Integer);
begin
  lua_pushcfunction(L, openf);  // Push the module loader function
  lua_pushstring(L, modname);   // Push module name as argument

  // Use pcall instead of call for error handling
  if lua_pcall(L, 1, 1, 0) <> 0 then
  begin
    // Get error message and raise
    raise EjetLuaException.CreateFmt('Error loading module "%s": %s',
      [modname, string(lua_tostring(L, -1))]);
  end;

  // Get _LOADED table from registry
  lua_getfield(L, LUA_REGISTRYINDEX, '_LOADED');
  if not lua_istable(L, -1) then
  begin
    lua_pop(L, 2);  // Pop module and non-table value
    raise EjetLuaException.Create('_LOADED is not a table');
  end;

  // Store module in _LOADED[modname]
  lua_pushvalue(L, -2);        // Copy the module
  lua_setfield(L, -2, modname);
  lua_pop(L, 1);              // Pop _LOADED table

  // If global is requested, set it
  if glb <> 0 then
  begin
    lua_pushvalue(L, -1);     // Copy the module again
    lua_setglobal(L, modname);
  end;
end;

function luaL_getmetatable(L: Plua_State; const ATableName: PAnsiChar): Boolean;
begin
  // Get the metatable directly from the registry
  lua_getfield(L, LUA_REGISTRYINDEX, ATableName);

  // Check if the field exists and is a table
  Result := lua_type(L, -1) = LUA_TTABLE;
  if not Result then
    lua_pop(L, 1); // Remove the nil value from the stack if not found
end;

function lua_isinteger(L: Plua_State; AIndex: Integer): Boolean;
var
  LNumber: lua_Number;
  LIntValue: Int64;
begin
  // First check if it's a number
  if lua_isnumber(L, AIndex) = 0 then
    Exit(False);

  // Get the number
  LNumber := lua_tonumber(L, AIndex);

  // Try to convert to integer and back, then compare
  LIntValue := Trunc(LNumber);
  Result := (LNumber = LIntValue) and
            (LIntValue >= -9223372036854775808.0) and  // Min Int64
            (LIntValue <= 9223372036854775807.0);      // Max Int64
end;

{$REGION ' LUA DEBUGGER '}
const
  DEBUGGER_LUA =
'''
--[[---------------------------------------------------------------------------
Acknowledgment:
   This code is based on the original debugger.lua project by
   slembcke, available at:
     https://github.com/slembcke/debugger.lua
   Credit goes to the original developer for their foundational work, which
   this unit builds upon.
-----------------------------------------------------------------------------]]

local dbg = {}

-- ANSI Colors
local COLOR_GRAY = string.char(27) .. "[90m"
local COLOR_RED = string.char(27) .. "[91m"
local COLOR_BLUE = string.char(27) .. "[94m"
local COLOR_YELLOW = string.char(27) .. "[33m"
local COLOR_RESET = string.char(27) .. "[0m"
local GREEN_CARET = string.char(27) .. "[92m => " .. COLOR_RESET

-- Check for Windows
local function is_windows()
    return package.config:sub(1,1) == '\\'
end

-- Check if colors are supported
local function supports_colors()
    if is_windows() then
        -- Windows 10+ supports ANSI colors
        local version = os.getenv("WINVER") or os.getenv("VERSION")
        return version ~= nil
    else
        -- Unix-like systems
        return os.getenv("TERM") and os.getenv("TERM") ~= "dumb"
    end
end

-- Disable colors if terminal doesn't support them
if not supports_colors then
    COLOR_GRAY = ""
    COLOR_RED = ""
    COLOR_BLUE = ""
    COLOR_YELLOW = ""
    COLOR_RESET = ""
    GREEN_CARET = " => "
end

-- State tracking
local current_frame = 0
local step_mode = nil
local current_func = nil
local last_cmd = "h"  -- Move last_cmd to file scope

-- Source cache
local source_cache = {}

local function pretty(obj, max_depth)
    max_depth = max_depth or 3
    local function pp(obj, depth)
        if depth > max_depth then return tostring(obj) end
        if type(obj) == "string" then return string.format("%q", obj) end
        if type(obj) ~= "table" then return tostring(obj) end
        local mt = getmetatable(obj)
        if mt and mt.__tostring then return tostring(obj) end

        local parts = {}
        for k, v in pairs(obj) do
            local key = type(k) == "string" and k or "[" .. pp(k, depth) .. "]"
            table.insert(parts, key .. " = " .. pp(v, depth + 1))
        end
        return "{" .. table.concat(parts, ", ") .. "}"
    end
    return pp(obj, 1)
end

local function get_locals(level)
    local vars = {}
    local i = 1
    while true do
        local name, value = debug.getlocal(level, i)
        if not name then break end
        if name:sub(1, 1) ~= "(" then  -- Skip internal variables
            vars[name] = value
        end
        i = i + 1
    end
    return vars
end

local function get_upvalues(func)
    local vars = {}
    local i = 1
    while true do
        local name, value = debug.getupvalue(func, i)
        if not name then break end
        vars[name] = value
        i = i + 1
    end
    return vars
end

local function get_source_lines(info)
    if source_cache[info.source] then
        return source_cache[info.source]
    end

    local lines = {}
    if info.source:sub(1, 1) == "@" then
        local file = io.open(info.source:sub(2))
        if file then
            for line in file:lines() do
                table.insert(lines, line)
            end
            file:close()
        end
    else
        for line in info.source:gmatch("[^\n]+") do
            table.insert(lines, line)
        end
    end
    source_cache[info.source] = lines
    return lines
end

local function get_short_src(source)
    if source:sub(1, 1) == "@" then
        return source:sub(2)  -- Remove @ prefix
    end
    -- For non-file sources, return just "[string]"
    return "[string]"
end

local function print_break_location(info, reason)
    reason = reason or "dbg()"
    local short_src = get_short_src(info.source)
    local prefix = reason and (COLOR_YELLOW .. "break via " .. COLOR_RED .. reason .. GREEN_CARET) or ""
    print(string.format("%s%s%s:%s%d%s in %s",
        prefix,
        COLOR_BLUE, short_src,
        COLOR_YELLOW, info.currentline,
        COLOR_RESET,
        info.name or "main chunk"
    ))
end

local function print_frame_source(info, context_lines)
    context_lines = context_lines or 2
    local lines = get_source_lines(info)
    if not lines then return end

    local line_num = info.currentline
    for i = math.max(1, line_num - context_lines),
             math.min(#lines, line_num + context_lines) do
        local marker = i == line_num and GREEN_CARET or "    "
        print(string.format(COLOR_GRAY .. "% 4d%s%s",
            i, marker, lines[i] .. COLOR_RESET))
    end
end

local function evaluate_expression(expr, level)
    if not expr or expr == "" then
        print(COLOR_RED .. "Usage: p <expression>" .. COLOR_RESET)
        return
    end

    local locals = get_locals(level)
    local info = debug.getinfo(level, "f")
    local upvalues = get_upvalues(info.func)

    -- Create environment with locals, upvalues, and globals
    local env = setmetatable(locals, {__index = _G})
    for k, v in pairs(upvalues) do env[k] = v end

    local chunk, err = load("return " .. expr, "=expr", "t", env)
    if not chunk then
        print(COLOR_RED .. "Error: " .. err .. COLOR_RESET)
        return
    end

    local success, result = pcall(chunk)
    if not success then
        print(COLOR_RED .. "Error: " .. result .. COLOR_RESET)
        return
    end

    print(COLOR_BLUE .. expr .. GREEN_CARET .. pretty(result))
end

local function print_locals(level)
    local locals = get_locals(level)
    local info = debug.getinfo(level, "f")
    local upvalues = get_upvalues(info.func)

    print(COLOR_BLUE .. "Local variables:" .. COLOR_RESET)
    local sorted_locals = {}
    for name, value in pairs(locals) do
        table.insert(sorted_locals, {name = name, value = value})
    end
    table.sort(sorted_locals, function(a, b) return a.name < b.name end)

    for _, var in ipairs(sorted_locals) do
        print(string.format("  %s = %s", var.name, pretty(var.value)))
    end

    if next(upvalues) then
        print(COLOR_BLUE .. "\nUpvalues:" .. COLOR_RESET)
        local sorted_upvalues = {}
        for name, value in pairs(upvalues) do
            table.insert(sorted_upvalues, {name = name, value = value})
        end
        table.sort(sorted_upvalues, function(a, b) return a.name < b.name end)

        for _, var in ipairs(sorted_upvalues) do
            print(string.format("  %s = %s", var.name, pretty(var.value)))
        end
    end
end

local function print_help()
    local help = {
        {cmd = "<return>", desc = "re-run last command"},
        {cmd = "c(ontinue)", desc = "continue execution"},
        {cmd = "s(tep)", desc = "step forward by one line (into functions)"},
        {cmd = "n(ext)", desc = "step forward by one line (skipping over functions)"},
        {cmd = "f(inish)", desc = "step forward until exiting the current function"},
        {cmd = "u(p)", desc = "move up the stack by one frame"},
        {cmd = "d(own)", desc = "move down the stack by one frame"},
        {cmd = "w(here) [count]", desc = "print source code around the current line"},
        {cmd = "p(rint) [expr]", desc = "evaluate expression and print the result"},
        {cmd = "t(race)", desc = "print the stack trace"},
        {cmd = "l(ocals)", desc = "print the function arguments, locals and upvalues"},
        {cmd = "h(elp)", desc = "print this message"},
        {cmd = "q(uit)", desc = "halt execution"},
    }

    for _, item in ipairs(help) do
        print(string.format("%s%s%s%s%s",
            COLOR_BLUE, item.cmd,
            COLOR_YELLOW, GREEN_CARET, item.desc))
    end
end

local function print_stack_trace()
    local level = 1
    print(COLOR_BLUE .. "Stack trace:" .. COLOR_RESET)
    while true do
        local info = debug.getinfo(level, "Snl")
        if not info then break end

        local is_current = level == current_frame + 2
        local marker = is_current and GREEN_CARET or "    "
        local name = info.name or "<unknown>"
        local source = get_short_src(info.source)

        print(string.format(COLOR_GRAY .. "% 4d%s%s:%d in %s",
            level - 1, marker, source, info.currentline, name))

        level = level + 1
    end
end

-- Debug hook
local function debug_hook(event, line)

    if event ~= "line" then return end

    if step_mode == "over" and current_func then
        local info = debug.getinfo(2, "f")
        if info.func ~= current_func then return end
    end

    local info = debug.getinfo(2, "Snl")
    if not info then return end

    print_break_location(info)
    print_frame_source(info)

    while true do
        io.write(COLOR_RED .. "cdb> " .. COLOR_RESET)
        local input = io.read()
        if not input then return end

        -- Handle empty input - reuse last command
        if input == "" then
            input = last_cmd
        else
            last_cmd = input  -- Update last_cmd only for non-empty input
        end

        local cmd, args = input:match("^(%S+)%s*(.*)")
        cmd = cmd or ""

        if cmd == "c" then
            step_mode = nil
            debug.sethook()
            return
        elseif cmd == "s" then
            step_mode = "into"
            return
        elseif cmd == "n" then
            step_mode = "over"
            current_func = debug.getinfo(2, "f").func
            return
        elseif cmd == "f" then
            step_mode = "out"
            current_func = debug.getinfo(2, "f").func
            return
        elseif cmd == "l" then
            print_locals(2 + current_frame)
        elseif cmd == "t" then
            print_stack_trace()
        elseif cmd == "w" then
            local count = tonumber(args) or 5
            print_frame_source(info, count)
        elseif cmd == "u" then
            local new_frame = current_frame + 1
            local frame_info = debug.getinfo(new_frame + 2, "Snl")
            if frame_info then
                current_frame = new_frame
                print_break_location(frame_info)
                print_frame_source(frame_info)
            else
                print("Already at top of stack")
            end
        elseif cmd == "d" then
            if current_frame > 0 then
                current_frame = current_frame - 1
                local frame_info = debug.getinfo(current_frame + 2, "Snl")
                print_break_location(frame_info)
                print_frame_source(frame_info)
            else
                print("Already at bottom of stack")
            end
        elseif cmd == "p" then
            evaluate_expression(args, 2 + current_frame)
        elseif cmd == "h" then
            print_help()
        elseif cmd == "q" then
            os.exit(0)
        else
            print(COLOR_RED .. "Unknown command. Type 'h' for help." .. COLOR_RESET)
        end
    end
end

-- Make dbg callable
setmetatable(dbg, {
    __call = function(_, condition)
        if condition then return end
        current_frame = 0
        step_mode = "into"
        debug.sethook(debug_hook, "l")
    end
})

-- Expose API
dbg.pretty = pretty
dbg.pretty_depth = 3
dbg.auto_where = false

return dbg
''';

function luaopen_debugger(lua: Plua_State): Integer; cdecl;
begin
  if (luaL_loadbufferx(lua, DEBUGGER_LUA, Length(DEBUGGER_LUA), '<debugger.lua>', nil) <> 0) or
     (lua_pcall(lua, 0, LUA_MULTRET, 0) <> 0) then
    lua_error(lua);
  Result := 1;
end;

const
  MODULE_NAME: PAnsiChar = 'DEBUGGER_LUA_MODULE';
  MSGH: PAnsiChar = 'DEBUGGER_LUA_MSGH';

procedure dbg_setup(lua: Plua_State; name: PAnsiChar; globalName: PAnsiChar; readFunc: lua_CFunction; writeFunc: lua_CFunction); cdecl;
begin
  // Check that the module name was not already defined.
  lua_getfield(lua, LUA_REGISTRYINDEX, MODULE_NAME);
  Assert(lua_isnil(lua, -1) or (System.AnsiStrings.StrComp(name, luaL_checkstring(lua, -1)) = 0));
  lua_pop(lua, 1);

  // Push the module name into the registry.
  lua_pushstring(lua, name);
  lua_setfield(lua, LUA_REGISTRYINDEX, MODULE_NAME);

  // Preload the module
  luaL_requiref(lua, name, luaopen_debugger, 0);

  // Insert the msgh function into the registry.
  lua_getfield(lua, -1, 'msgh');
  lua_setfield(lua, LUA_REGISTRYINDEX, MSGH);

  if Assigned(readFunc) then
  begin
    lua_pushcfunction(lua, readFunc);
    lua_setfield(lua, -2, 'read');
  end;

  if Assigned(writeFunc) then
  begin
    lua_pushcfunction(lua, writeFunc);
    lua_setfield(lua, -2, 'write');
  end;

  if globalName <> nil then
  begin
    lua_setglobal(lua, globalName);
  end else
  begin
    lua_pop(lua, 1);
  end;
end;

procedure dbg_setup_default(lua: Plua_State); cdecl;
begin
  dbg_setup(lua, 'debugger', 'dbg', nil, nil);
end;

function dbg_pcall(lua: Plua_State; nargs: Integer; nresults: Integer; msgh: Integer): Integer; cdecl;
begin
  // Call regular lua_pcall() if a message handler is provided.
  if msgh <> 0 then
    Exit(lua_pcall(lua, nargs, nresults, msgh));

  // Grab the msgh function out of the registry.
  lua_getfield(lua, LUA_REGISTRYINDEX, PUTF8Char(MSGH));
  if lua_isnil(lua, -1) then
    luaL_error(lua, 'Tried to call dbg_call() before calling dbg_setup().');

  // Move the error handler just below the function.
  msgh := lua_gettop(lua) - (1 + nargs);
  lua_insert(lua, msgh);

  // Call the function.
  Result := lua_pcall(lua, nargs, nresults, msgh);

  // Remove the debug handler.
  lua_remove(lua, msgh);
end;

function dbg_dofile(lua: Plua_State; filename: PAnsiChar): Integer;
begin
  Result := luaL_loadfile(lua, filename);
  if Result = 0 then
    Result := dbg_pcall(lua, 0, LUA_MULTRET, 0);
end;
{$ENDREGION}

{$ENDREGION}

{$REGION ' JETLUA '}

{$REGION ' LUA CODE '}
const cLOADER_LUA : array[1..436] of Byte = (
$2D, $2D, $20, $55, $74, $69, $6C, $69, $74, $79, $20, $66, $75, $6E, $63, $74,
$69, $6F, $6E, $20, $66, $6F, $72, $20, $68, $61, $76, $69, $6E, $67, $20, $61,
$20, $77, $6F, $72, $6B, $69, $6E, $67, $20, $69, $6D, $70, $6F, $72, $74, $20,
$66, $75, $6E, $63, $74, $69, $6F, $6E, $0A, $2D, $2D, $20, $46, $65, $65, $6C,
$20, $66, $72, $65, $65, $20, $74, $6F, $20, $75, $73, $65, $20, $69, $74, $20,
$69, $6E, $20, $79, $6F, $75, $72, $20, $6F, $77, $6E, $20, $70, $72, $6F, $6A,
$65, $63, $74, $73, $0A, $28, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $29,
$0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20, $73, $63, $72, $69, $70,
$74, $5F, $63, $61, $63, $68, $65, $20, $3D, $20, $7B, $7D, $3B, $0A, $20, $20,
$20, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $69, $6D, $70, $6F, $72,
$74, $28, $6E, $61, $6D, $65, $29, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
$69, $66, $20, $73, $63, $72, $69, $70, $74, $5F, $63, $61, $63, $68, $65, $5B,
$6E, $61, $6D, $65, $5D, $20, $3D, $3D, $20, $6E, $69, $6C, $20, $74, $68, $65,
$6E, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $73, $63,
$72, $69, $70, $74, $5F, $63, $61, $63, $68, $65, $5B, $6E, $61, $6D, $65, $5D,
$20, $3D, $20, $6C, $6F, $61, $64, $66, $69, $6C, $65, $28, $6E, $61, $6D, $65,
$29, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $65, $6E, $64, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $69,
$66, $20, $73, $63, $72, $69, $70, $74, $5F, $63, $61, $63, $68, $65, $5B, $6E,
$61, $6D, $65, $5D, $20, $7E, $3D, $20, $6E, $69, $6C, $20, $74, $68, $65, $6E,
$0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $72, $65, $74,
$75, $72, $6E, $20, $73, $63, $72, $69, $70, $74, $5F, $63, $61, $63, $68, $65,
$5B, $6E, $61, $6D, $65, $5D, $28, $29, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $65, $6E, $64, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $65, $72, $72,
$6F, $72, $28, $22, $46, $61, $69, $6C, $65, $64, $20, $74, $6F, $20, $6C, $6F,
$61, $64, $20, $73, $63, $72, $69, $70, $74, $20, $22, $20, $2E, $2E, $20, $6E,
$61, $6D, $65, $29, $0A, $20, $20, $20, $20, $65, $6E, $64, $0A, $65, $6E, $64,
$29, $28, $29, $0A
);

const cLUABUNDLE_LUA : array[1..3478] of Byte = (
$28, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $61, $72, $67, $73, $29, $0D,
$0A, $6C, $6F, $63, $61, $6C, $20, $6D, $6F, $64, $75, $6C, $65, $73, $20, $3D,
$20, $7B, $7D, $0D, $0A, $6D, $6F, $64, $75, $6C, $65, $73, $5B, $27, $61, $70,
$70, $2F, $62, $75, $6E, $64, $6C, $65, $5F, $6D, $61, $6E, $61, $67, $65, $72,
$2E, $6C, $75, $61, $27, $5D, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F,
$6E, $28, $2E, $2E, $2E, $29, $0D, $0A, $2D, $2D, $20, $43, $6C, $61, $73, $73,
$20, $66, $6F, $72, $20, $63, $6F, $6C, $6C, $65, $63, $74, $69, $6E, $67, $20,
$74, $68, $65, $20, $66, $69, $6C, $65, $27, $73, $20, $63, $6F, $6E, $74, $65,
$6E, $74, $20, $61, $6E, $64, $20, $62, $75, $69, $6C, $64, $69, $6E, $67, $20,
$61, $20, $62, $75, $6E, $64, $6C, $65, $20, $66, $69, $6C, $65, $0D, $0A, $6C,
$6F, $63, $61, $6C, $20, $73, $6F, $75, $72, $63, $65, $5F, $70, $61, $72, $73,
$65, $72, $20, $3D, $20, $69, $6D, $70, $6F, $72, $74, $28, $22, $61, $70, $70,
$2F, $73, $6F, $75, $72, $63, $65, $5F, $70, $61, $72, $73, $65, $72, $2E, $6C,
$75, $61, $22, $29, $0D, $0A, $0D, $0A, $72, $65, $74, $75, $72, $6E, $20, $66,
$75, $6E, $63, $74, $69, $6F, $6E, $28, $65, $6E, $74, $72, $79, $5F, $70, $6F,
$69, $6E, $74, $29, $0D, $0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20,
$73, $65, $6C, $66, $20, $3D, $20, $7B, $7D, $0D, $0A, $20, $20, $20, $20, $6C,
$6F, $63, $61, $6C, $20, $66, $69, $6C, $65, $73, $20, $3D, $20, $7B, $7D, $0D,
$0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $2D, $2D, $20, $53, $65,
$61, $72, $63, $68, $65, $73, $20, $74, $68, $65, $20, $67, $69, $76, $65, $6E,
$20, $66, $69, $6C, $65, $20, $72, $65, $63, $75, $72, $73, $69, $76, $65, $6C,
$79, $20, $66, $6F, $72, $20, $69, $6D, $70, $6F, $72, $74, $20, $66, $75, $6E,
$63, $74, $69, $6F, $6E, $20, $63, $61, $6C, $6C, $73, $0D, $0A, $20, $20, $20,
$20, $73, $65, $6C, $66, $2E, $70, $72, $6F, $63, $65, $73, $73, $5F, $66, $69,
$6C, $65, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $66, $69,
$6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $6C, $6F, $63, $61, $6C, $20, $70, $61, $72, $73, $65, $72, $20, $3D, $20,
$73, $6F, $75, $72, $63, $65, $5F, $70, $61, $72, $73, $65, $72, $28, $66, $69,
$6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $66, $69, $6C, $65, $73, $5B, $66, $69, $6C, $65, $6E, $61, $6D, $65, $5D,
$20, $3D, $20, $70, $61, $72, $73, $65, $72, $2E, $63, $6F, $6E, $74, $65, $6E,
$74, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $0D, $0A, $20, $20, $20,
$20, $20, $20, $20, $20, $66, $6F, $72, $20, $5F, $2C, $20, $66, $20, $69, $6E,
$20, $70, $61, $69, $72, $73, $28, $70, $61, $72, $73, $65, $72, $2E, $69, $6E,
$63, $6C, $75, $64, $65, $73, $29, $20, $64, $6F, $0D, $0A, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $20, $73, $65, $6C, $66, $2E, $70, $72, $6F,
$63, $65, $73, $73, $5F, $66, $69, $6C, $65, $28, $66, $29, $0D, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $65,
$6E, $64, $0D, $0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $2D, $2D,
$20, $43, $72, $65, $61, $74, $65, $20, $61, $20, $62, $75, $6E, $64, $6C, $65,
$20, $66, $69, $6C, $65, $20, $77, $68, $69, $63, $68, $20, $63, $6F, $6E, $74,
$61, $69, $6E, $73, $20, $74, $68, $65, $20, $64, $65, $74, $65, $63, $74, $65,
$64, $20, $66, $69, $6C, $65, $73, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C,
$66, $2E, $62, $75, $69, $6C, $64, $5F, $62, $75, $6E, $64, $6C, $65, $20, $3D,
$20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $64, $65, $73, $74, $5F, $66,
$69, $6C, $65, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $6C, $6F,
$63, $61, $6C, $20, $66, $69, $6C, $65, $20, $3D, $20, $69, $6F, $2E, $6F, $70,
$65, $6E, $28, $64, $65, $73, $74, $5F, $66, $69, $6C, $65, $2C, $20, $22, $77,
$22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $0D, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65,
$28, $22, $28, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $61, $72, $67, $73,
$29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66,
$69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $6C, $6F, $63, $61, $6C,
$20, $6D, $6F, $64, $75, $6C, $65, $73, $20, $3D, $20, $7B, $7D, $5C, $6E, $22,
$29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $0D, $0A, $20, $20, $20,
$20, $20, $20, $20, $20, $2D, $2D, $20, $43, $72, $65, $61, $74, $65, $20, $61,
$20, $73, $6F, $72, $74, $65, $64, $20, $6C, $69, $73, $74, $20, $6F, $66, $20,
$6B, $65, $79, $73, $20, $73, $6F, $20, $74, $68, $65, $20, $6F, $75, $74, $70,
$75, $74, $20, $77, $69, $6C, $6C, $20, $62, $65, $20, $74, $68, $65, $20, $73,
$61, $6D, $65, $20, $77, $68, $65, $6E, $20, $74, $68, $65, $20, $69, $6E, $70,
$75, $74, $20, $64, $6F, $65, $73, $20, $6E, $6F, $74, $20, $63, $68, $61, $6E,
$67, $65, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $6C, $6F, $63, $61,
$6C, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $73, $20, $3D, $20, $7B, $7D,
$0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $6F, $72, $20, $66, $69,
$6C, $65, $6E, $61, $6D, $65, $2C, $20, $5F, $20, $69, $6E, $20, $70, $61, $69,
$72, $73, $28, $66, $69, $6C, $65, $73, $29, $20, $64, $6F, $0D, $0A, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $74, $61, $62, $6C, $65, $2E,
$69, $6E, $73, $65, $72, $74, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $73,
$2C, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20,
$20, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $20, $20,
$20, $20, $74, $61, $62, $6C, $65, $2E, $73, $6F, $72, $74, $28, $66, $69, $6C,
$65, $6E, $61, $6D, $65, $73, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $2D, $2D, $20, $41, $64,
$64, $20, $66, $69, $6C, $65, $73, $20, $61, $73, $20, $6D, $6F, $64, $75, $6C,
$65, $73, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $6F, $72, $20,
$5F, $2C, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $20, $69, $6E, $20, $70,
$61, $69, $72, $73, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $73, $29, $20,
$64, $6F, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
$66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $6D, $6F, $64, $75,
$6C, $65, $73, $5B, $27, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28,
$66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74,
$65, $28, $22, $27, $5D, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E,
$28, $2E, $2E, $2E, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20,
$20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74,
$65, $28, $66, $69, $6C, $65, $73, $5B, $66, $69, $6C, $65, $6E, $61, $6D, $65,
$5D, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20,
$66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $5C, $6E, $22, $29,
$0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69,
$6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $65, $6E, $64, $5C, $6E, $22,
$29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A,
$20, $20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69,
$74, $65, $28, $22, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $69, $6D, $70,
$6F, $72, $74, $28, $6E, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20,
$20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22,
$72, $65, $74, $75, $72, $6E, $20, $6D, $6F, $64, $75, $6C, $65, $73, $5B, $6E,
$5D, $28, $74, $61, $62, $6C, $65, $2E, $75, $6E, $70, $61, $63, $6B, $28, $61,
$72, $67, $73, $29, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20,
$20, $20, $20, $66, $69, $6C, $65, $3A, $77, $72, $69, $74, $65, $28, $22, $65,
$6E, $64, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20,
$0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $77,
$72, $69, $74, $65, $28, $22, $6C, $6F, $63, $61, $6C, $20, $65, $6E, $74, $72,
$79, $20, $3D, $20, $69, $6D, $70, $6F, $72, $74, $28, $27, $22, $20, $2E, $2E,
$20, $65, $6E, $74, $72, $79, $5F, $70, $6F, $69, $6E, $74, $20, $2E, $2E, $20,
$22, $27, $29, $5C, $6E, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20,
$20, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69, $6C, $65, $3A,
$77, $72, $69, $74, $65, $28, $22, $65, $6E, $64, $29, $28, $7B, $2E, $2E, $2E,
$7D, $29, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $66, $69,
$6C, $65, $3A, $66, $6C, $75, $73, $68, $28, $29, $0D, $0A, $20, $20, $20, $20,
$20, $20, $20, $20, $66, $69, $6C, $65, $3A, $63, $6C, $6F, $73, $65, $28, $29,
$0D, $0A, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $0D,
$0A, $20, $20, $20, $20, $72, $65, $74, $75, $72, $6E, $20, $73, $65, $6C, $66,
$0D, $0A, $65, $6E, $64, $0D, $0A, $65, $6E, $64, $0D, $0A, $6D, $6F, $64, $75,
$6C, $65, $73, $5B, $27, $61, $70, $70, $2F, $6D, $61, $69, $6E, $2E, $6C, $75,
$61, $27, $5D, $20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $2E,
$2E, $2E, $29, $0D, $0A, $2D, $2D, $20, $4D, $61, $69, $6E, $20, $66, $75, $6E,
$63, $74, $69, $6F, $6E, $20, $6F, $66, $20, $74, $68, $65, $20, $70, $72, $6F,
$67, $72, $61, $6D, $0D, $0A, $6C, $6F, $63, $61, $6C, $20, $62, $75, $6E, $64,
$6C, $65, $5F, $6D, $61, $6E, $61, $67, $65, $72, $20, $3D, $20, $69, $6D, $70,
$6F, $72, $74, $28, $22, $61, $70, $70, $2F, $62, $75, $6E, $64, $6C, $65, $5F,
$6D, $61, $6E, $61, $67, $65, $72, $2E, $6C, $75, $61, $22, $29, $0D, $0A, $0D,
$0A, $72, $65, $74, $75, $72, $6E, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E,
$28, $61, $72, $67, $73, $29, $0D, $0A, $20, $20, $20, $20, $69, $66, $20, $23,
$61, $72, $67, $73, $20, $3D, $3D, $20, $31, $20, $61, $6E, $64, $20, $61, $72,
$67, $73, $5B, $31, $5D, $20, $3D, $3D, $20, $22, $2D, $76, $22, $20, $74, $68,
$65, $6E, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $70, $72, $69, $6E,
$74, $28, $22, $6C, $75, $61, $62, $75, $6E, $64, $6C, $65, $20, $76, $30, $2E,
$30, $31, $22, $29, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $6F, $73,
$2E, $65, $78, $69, $74, $28, $29, $0D, $0A, $20, $20, $20, $20, $65, $6C, $73,
$65, $69, $66, $20, $23, $61, $72, $67, $73, $20, $7E, $3D, $20, $32, $20, $74,
$68, $65, $6E, $0D, $0A, $20, $20, $20, $20, $20, $20, $20, $20, $70, $72, $69,
$6E, $74, $28, $22, $75, $73, $61, $67, $65, $3A, $20, $6C, $75, $61, $62, $75,
$6E, $64, $6C, $65, $20, $69, $6E, $20, $6F, $75, $74, $22, $29, $0D, $0A, $20,
$20, $20, $20, $20, $20, $20, $20, $6F, $73, $2E, $65, $78, $69, $74, $28, $29,
$0D, $0A, $20, $20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $0D,
$0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20, $69, $6E, $66, $69, $6C,
$65, $20, $3D, $20, $61, $72, $67, $73, $5B, $31, $5D, $0D, $0A, $20, $20, $20,
$20, $6C, $6F, $63, $61, $6C, $20, $6F, $75, $74, $66, $69, $6C, $65, $20, $3D,
$20, $61, $72, $67, $73, $5B, $32, $5D, $0D, $0A, $20, $20, $20, $20, $6C, $6F,
$63, $61, $6C, $20, $62, $75, $6E, $64, $6C, $65, $20, $3D, $20, $62, $75, $6E,
$64, $6C, $65, $5F, $6D, $61, $6E, $61, $67, $65, $72, $28, $69, $6E, $66, $69,
$6C, $65, $29, $0D, $0A, $20, $20, $20, $20, $62, $75, $6E, $64, $6C, $65, $2E,
$70, $72, $6F, $63, $65, $73, $73, $5F, $66, $69, $6C, $65, $28, $69, $6E, $66,
$69, $6C, $65, $2C, $20, $62, $75, $6E, $64, $6C, $65, $29, $0D, $0A, $20, $20,
$20, $20, $0D, $0A, $20, $20, $20, $20, $62, $75, $6E, $64, $6C, $65, $2E, $62,
$75, $69, $6C, $64, $5F, $62, $75, $6E, $64, $6C, $65, $28, $6F, $75, $74, $66,
$69, $6C, $65, $29, $0D, $0A, $65, $6E, $64, $0D, $0A, $65, $6E, $64, $0D, $0A,
$6D, $6F, $64, $75, $6C, $65, $73, $5B, $27, $61, $70, $70, $2F, $73, $6F, $75,
$72, $63, $65, $5F, $70, $61, $72, $73, $65, $72, $2E, $6C, $75, $61, $27, $5D,
$20, $3D, $20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $2E, $2E, $2E, $29,
$0D, $0A, $2D, $2D, $20, $43, $6C, $61, $73, $73, $20, $66, $6F, $72, $20, $65,
$78, $74, $72, $61, $63, $74, $69, $6E, $67, $20, $69, $6D, $70, $6F, $72, $74,
$20, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $63, $61, $6C, $6C, $73, $20,
$66, $72, $6F, $6D, $20, $73, $6F, $75, $72, $63, $65, $20, $66, $69, $6C, $65,
$73, $0D, $0A, $72, $65, $74, $75, $72, $6E, $20, $66, $75, $6E, $63, $74, $69,
$6F, $6E, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20, $20,
$20, $20, $6C, $6F, $63, $61, $6C, $20, $66, $69, $6C, $65, $20, $3D, $20, $69,
$6F, $2E, $6F, $70, $65, $6E, $28, $66, $69, $6C, $65, $6E, $61, $6D, $65, $2C,
$20, $22, $72, $22, $29, $0D, $0A, $20, $20, $20, $20, $69, $66, $20, $66, $69,
$6C, $65, $20, $3D, $3D, $20, $6E, $69, $6C, $20, $74, $68, $65, $6E, $0D, $0A,
$20, $20, $20, $20, $20, $20, $20, $20, $65, $72, $72, $6F, $72, $28, $22, $46,
$69, $6C, $65, $20, $6E, $6F, $74, $20, $66, $6F, $75, $6E, $64, $3A, $20, $22,
$20, $2E, $2E, $20, $66, $69, $6C, $65, $6E, $61, $6D, $65, $29, $0D, $0A, $20,
$20, $20, $20, $65, $6E, $64, $0D, $0A, $20, $20, $20, $20, $6C, $6F, $63, $61,
$6C, $20, $66, $69, $6C, $65, $5F, $63, $6F, $6E, $74, $65, $6E, $74, $20, $3D,
$20, $66, $69, $6C, $65, $3A, $72, $65, $61, $64, $28, $22, $2A, $61, $22, $29,
$0D, $0A, $20, $20, $20, $20, $66, $69, $6C, $65, $3A, $63, $6C, $6F, $73, $65,
$28, $29, $0D, $0A, $20, $20, $20, $20, $6C, $6F, $63, $61, $6C, $20, $69, $6E,
$63, $6C, $75, $64, $65, $64, $5F, $66, $69, $6C, $65, $73, $20, $3D, $20, $7B,
$7D, $0D, $0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $2D, $2D, $20,
$53, $65, $61, $72, $63, $68, $20, $66, $6F, $72, $20, $69, $6D, $70, $6F, $72,
$74, $28, $29, $20, $63, $61, $6C, $6C, $73, $20, $77, $69, $74, $68, $20, $64,
$6F, $62, $75, $6C, $65, $20, $71, $75, $6F, $74, $65, $73, $20, $28, $21, $29,
$0D, $0A, $20, $20, $20, $20, $66, $6F, $72, $20, $66, $20, $69, $6E, $20, $73,
$74, $72, $69, $6E, $67, $2E, $67, $6D, $61, $74, $63, $68, $28, $66, $69, $6C,
$65, $5F, $63, $6F, $6E, $74, $65, $6E, $74, $2C, $20, $27, $69, $6D, $70, $6F,
$72, $74, $25, $28, $5B, $22, $5C, $27, $5D, $28, $5B, $5E, $5C, $27, $22, $5D,
$2D, $29, $5B, $22, $5C, $27, $5D, $25, $29, $27, $29, $20, $64, $6F, $0D, $0A,
$20, $20, $20, $20, $20, $20, $20, $20, $74, $61, $62, $6C, $65, $2E, $69, $6E,
$73, $65, $72, $74, $28, $69, $6E, $63, $6C, $75, $64, $65, $64, $5F, $66, $69,
$6C, $65, $73, $2C, $20, $66, $29, $0D, $0A, $20, $20, $20, $20, $65, $6E, $64,
$0D, $0A, $20, $20, $20, $20, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66,
$20, $3D, $20, $7B, $7D, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66, $2E,
$66, $69, $6C, $65, $6E, $61, $6D, $65, $20, $3D, $20, $66, $69, $6C, $65, $6E,
$61, $6D, $65, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66, $2E, $63, $6F,
$6E, $74, $65, $6E, $74, $20, $3D, $20, $66, $69, $6C, $65, $5F, $63, $6F, $6E,
$74, $65, $6E, $74, $0D, $0A, $20, $20, $20, $20, $73, $65, $6C, $66, $2E, $69,
$6E, $63, $6C, $75, $64, $65, $73, $20, $3D, $20, $69, $6E, $63, $6C, $75, $64,
$65, $64, $5F, $66, $69, $6C, $65, $73, $0D, $0A, $20, $20, $20, $20, $72, $65,
$74, $75, $72, $6E, $20, $73, $65, $6C, $66, $0D, $0A, $65, $6E, $64, $0D, $0A,
$65, $6E, $64, $0D, $0A, $6D, $6F, $64, $75, $6C, $65, $73, $5B, $27, $6C, $75,
$61, $62, $75, $6E, $64, $6C, $65, $2E, $6C, $75, $61, $27, $5D, $20, $3D, $20,
$66, $75, $6E, $63, $74, $69, $6F, $6E, $28, $2E, $2E, $2E, $29, $0D, $0A, $2D,
$2D, $20, $45, $6E, $74, $72, $79, $20, $70, $6F, $69, $6E, $74, $20, $6F, $66,
$20, $74, $68, $65, $20, $70, $72, $6F, $67, $72, $61, $6D, $2E, $0D, $0A, $2D,
$2D, $20, $4F, $6E, $6C, $79, $20, $62, $61, $73, $69, $63, $20, $73, $74, $75,
$66, $66, $20, $69, $73, $20, $73, $65, $74, $20, $75, $70, $20, $68, $65, $72,
$65, $2C, $20, $74, $68, $65, $20, $61, $63, $74, $75, $61, $6C, $20, $70, $72,
$6F, $67, $72, $61, $6D, $20, $69, $73, $20, $69, $6E, $20, $61, $70, $70, $2F,
$6D, $61, $69, $6E, $2E, $6C, $75, $61, $0D, $0A, $6C, $6F, $63, $61, $6C, $20,
$61, $72, $67, $73, $20, $3D, $20, $7B, $2E, $2E, $2E, $7D, $0D, $0A, $0D, $0A,
$2D, $2D, $20, $43, $68, $65, $63, $6B, $20, $69, $66, $20, $77, $65, $20, $61,
$72, $65, $20, $61, $6C, $72, $65, $61, $64, $79, $20, $62, $75, $6E, $64, $6C,
$65, $64, $0D, $0A, $69, $66, $20, $69, $6D, $70, $6F, $72, $74, $20, $3D, $3D,
$20, $6E, $69, $6C, $20, $74, $68, $65, $6E, $0D, $0A, $20, $20, $20, $20, $64,
$6F, $66, $69, $6C, $65, $28, $22, $75, $74, $69, $6C, $2F, $6C, $6F, $61, $64,
$65, $72, $2E, $6C, $75, $61, $22, $29, $0D, $0A, $65, $6E, $64, $0D, $0A, $0D,
$0A, $69, $6D, $70, $6F, $72, $74, $28, $22, $61, $70, $70, $2F, $6D, $61, $69,
$6E, $2E, $6C, $75, $61, $22, $29, $28, $61, $72, $67, $73, $29, $0D, $0A, $65,
$6E, $64, $0D, $0A, $66, $75, $6E, $63, $74, $69, $6F, $6E, $20, $69, $6D, $70,
$6F, $72, $74, $28, $6E, $29, $0D, $0A, $72, $65, $74, $75, $72, $6E, $20, $6D,
$6F, $64, $75, $6C, $65, $73, $5B, $6E, $5D, $28, $74, $61, $62, $6C, $65, $2E,
$75, $6E, $70, $61, $63, $6B, $28, $61, $72, $67, $73, $29, $29, $0D, $0A, $65,
$6E, $64, $0D, $0A, $6C, $6F, $63, $61, $6C, $20, $65, $6E, $74, $72, $79, $20,
$3D, $20, $69, $6D, $70, $6F, $72, $74, $28, $27, $6C, $75, $61, $62, $75, $6E,
$64, $6C, $65, $2E, $6C, $75, $61, $27, $29, $0D, $0A, $65, $6E, $64, $29, $28,
$7B, $2E, $2E, $2E, $7D, $29
);
{$ENDREGION}

type
  { TLuaBridgeRegistry }
  TLuaBridgeRegistry = class
  private
    class var
      FWrapperMap: TDictionary<Pointer, TObject>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterWrapper(Key: Pointer; Wrapper: TObject);
    class function GetWrapper(Key: Pointer): TObject;
    class procedure RemoveWrapper(Key: Pointer);
  end;

{ Lua C functions }
function LuaMethodCallback(L: Plua_State): Integer; cdecl;
var
  LWrapper: TjetLuaMethodWrapper;
  LWrapperObj: TObject;
begin
  LWrapperObj := TLuaBridgeRegistry.GetWrapper(lua_touserdata(L, lua_upvalueindex(1)));
  if LWrapperObj is TjetLuaMethodWrapper then
  begin
    LWrapper := TjetLuaMethodWrapper(LWrapperObj);
    Result := LWrapper.Execute(L);
  end
  else
  begin
    lua_pushstring(L, 'Invalid method wrapper');
    lua_error(L);
    Result := 0;
  end;
end;

function LuaRecordGC(L: Plua_State): Integer; cdecl;
begin
  // Records are value types, so we don't need to do anything special
  // The memory allocated by lua_newuserdata will be freed by Lua
  Result := 0;
end;

// Add these GC functions at unit level
function LuaPointerGC(L: Plua_State): Integer; cdecl;
begin
  // By default, do nothing - pointer cleanup should be handled by the application
  Result := 0;
end;

function LuaObjectGC(L: Plua_State): Integer; cdecl;
var
  LUserData: Pointer;
begin
  LUserData := lua_touserdata(L, 1);
  if LUserData <> nil then
  begin
    TObject(LUserData^).Free;
    TObject(LUserData^) := nil;
  end;
  Result := 0;
end;

{ TLuaBridgeRegistry }
class constructor TLuaBridgeRegistry.Create;
begin
  FWrapperMap := TDictionary<Pointer, TObject>.Create;
end;

class destructor TLuaBridgeRegistry.Destroy;
begin
  FWrapperMap.Free;
end;

class procedure TLuaBridgeRegistry.RegisterWrapper(Key: Pointer; Wrapper: TObject);
begin
  FWrapperMap.AddOrSetValue(Key, Wrapper);
end;

class function TLuaBridgeRegistry.GetWrapper(Key: Pointer): TObject;
begin
  if not FWrapperMap.TryGetValue(Key, Result) then
    Result := nil;
end;

class procedure TLuaBridgeRegistry.RemoveWrapper(Key: Pointer);
begin
  FWrapperMap.Remove(Key);
end;

{ TjetLuaMethodWrapper }
constructor TjetLuaMethodWrapper.Create(const AMethod: TRttiMethod; const AClass: TClass);
begin
  inherited Create();

  FMethod := AMethod;
  FClass := AClass;
  FContext := TRttiContext.Create;
end;

function TjetLuaMethodWrapper.ConvertNativeToLua(const AState: Pointer; const AValue: TValue): Integer;
var
  LObj: TObject;
  LUserData: Pointer;
  LRecordSize: Integer;
begin
  case AValue.Kind of
    tkInteger:
      begin
        lua_pushinteger(AState, AValue.AsInteger);
        Result := 1;
      end;
    tkFloat:
      begin
        lua_pushnumber(AState, AValue.AsExtended);
        Result := 1;
      end;
    tkString, tkUString:
      begin
        lua_pushstring(AState, AsUTF8(AValue.AsString));
        Result := 1;
      end;
    tkEnumeration:
      begin
        if AValue.TypeInfo = TypeInfo(Boolean) then
          lua_pushboolean(AState, Ord(AValue.AsBoolean))
        else
          lua_pushinteger(AState, AValue.AsOrdinal);
        Result := 1;
      end;
    tkClass:
      begin
        if AValue.IsObject then
        begin
          LObj := AValue.AsObject;
          if LObj <> nil then
          begin
            LUserData := lua_newuserdata(AState, SizeOf(TObject));
            TObject(LUserData^) := LObj;

            if luaL_newmetatable(AState, AsUTF8(LObj.ClassName)) <> 0 then
            begin
              lua_pushstring(AState, '__gc');
              lua_pushcclosure(AState, @LuaObjectGC, 0);
              lua_rawset(AState, -3);
            end;
            lua_setmetatable(AState, -2);
          end
          else
            lua_pushnil(AState);
        end
        else
          lua_pushnil(AState);
        Result := 1;
      end;
    tkPointer:
      begin
        if AValue.IsEmpty then
          lua_pushnil(AState)
        else
        begin
          LUserData := lua_newuserdata(AState, SizeOf(Pointer));
          PPointer(LUserData)^ := AValue.AsType<Pointer>;
        end;
        Result := 1;
      end;
    tkRecord:
      begin
        // Get the size of the record from RTTI
        LRecordSize := AValue.DataSize;
        // Allocate userdata to hold the record
        LUserData := lua_newuserdata(AState, LRecordSize);
        // Copy the record data
        Move(AValue.GetReferenceToRawData^, LUserData^, LRecordSize);
        // Add a metatable with the record type name
        if luaL_newmetatable(AState, AsUTF8(string(AValue.TypeInfo.Name))) <> 0 then
        //if luaL_newmetatable(AState, PAnsiChar(AnsiString(AValue.TypeInfo.Name))) <> 0 then
        begin
          // Set up metatable if needed
          lua_pushstring(AState, '__gc');
          lua_pushcclosure(AState, @LuaRecordGC, 0);
          lua_rawset(AState, -3);
        end;
        lua_setmetatable(AState, -2);
        Result := 1;
      end;
    else
      begin
        lua_pushnil(AState);
        Result := 1;
      end;
  end;
end;

function TjetLuaMethodWrapper.ConvertLuaToNative(const AState: Pointer; const AParamType: TRttiType; const AStackIndex: Integer): TValue;
var
  LUserData: Pointer;
  LTypeInfo: PTypeInfo;
  LRecordValue: TValue;
  LRecordSize: Integer;
begin
  case AParamType.TypeKind of
    tkInteger:
      Result := TValue.From<Integer>(lua_tointeger(AState, AStackIndex));
    tkFloat:
      Result := TValue.From<Double>(lua_tonumber(AState, AStackIndex));
    tkString, tkUString:
      Result := TValue.From<string>(string(lua_tostring(AState, AStackIndex)));
    tkEnumeration:
      if AParamType.Handle = TypeInfo(Boolean) then
        Result := TValue.From<Boolean>(LongBool(lua_toboolean(AState, AStackIndex)))
      else
        Result := TValue.FromOrdinal(AParamType.Handle, lua_tointeger(AState, AStackIndex));
    tkClass:
      begin
        if lua_type(AState, AStackIndex) = LUA_TUSERDATA then
        begin
          LUserData := lua_touserdata(AState, AStackIndex);
          if LUserData <> nil then
            Result := TValue.From<TObject>(TObject(LUserData^))
          else
            Result := TValue.From<TObject>(nil);
        end
        else
          Result := TValue.From<TObject>(nil);
      end;

    tkPointer:
      begin
        case lua_type(AState, AStackIndex) of
          LUA_TUSERDATA:
            begin
              LUserData := lua_touserdata(AState, AStackIndex);
              if LUserData <> nil then
              begin
                // First check if it's a pointer to a pointer (regular pointer case)
                if LongBool(luaL_getmetatable(AState, AsUTF8(AParamType.Name))) then
                begin
                  lua_pop(AState, 1);  // Pop metatable
                  Result := TValue.From<Pointer>(LUserData);
                end
                else
                begin
                  // Check if we have any metatable (could be a record)
                  if lua_getmetatable(AState, AStackIndex) = 1 then
                  begin
                    lua_pop(AState, 1); // Pop metatable
                    // If it has a metatable, treat it as a record pointer
                    Result := TValue.From<Pointer>(LUserData);
                  end
                  else
                    // No metatable, treat as regular pointer
                    Result := TValue.From<Pointer>(PPointer(LUserData)^);
                end;
              end
              else
                Result := TValue.From<Pointer>(nil);
            end;
          LUA_TLIGHTUSERDATA:
            Result := TValue.From<Pointer>(lua_touserdata(AState, AStackIndex));
        else
          Result := TValue.From<Pointer>(nil);
        end;
      end;

    tkRecord:
      begin
        if lua_type(AState, AStackIndex) = LUA_TUSERDATA then
        begin
          LUserData := lua_touserdata(AState, AStackIndex);
          if LUserData <> nil then
          begin
            LTypeInfo := AParamType.Handle;
            LRecordSize := AParamType.TypeSize;

            // Create a new TValue to hold the record
            TValue.Make(nil, LTypeInfo, LRecordValue);
            // Copy the data from Lua userdata to the new record
            Move(LUserData^, LRecordValue.GetReferenceToRawData^, LRecordSize);
            Result := LRecordValue;
          end
          else
            raise EjetLuaException.Create('Nil userdata for record parameter');
        end
        else
          raise EjetLuaException.Create('Expected userdata for record parameter');
      end;
    else
      raise EjetLuaException.CreateFmt('Unsupported parameter type: %s',
        [AParamType.Name]);
  end;
end;

function TjetLuaMethodWrapper.Execute(const AState: Pointer): Integer;
var
  LParams: array of TValue;
  LParam: TRttiParameter;
  LReturnValue: TValue;
  I: Integer;
  LParameters: TArray<TRttiParameter>;
begin
  LParameters := FMethod.GetParameters;
  SetLength(LParams, Length(LParameters));

  // Convert Lua parameters to native types
  for I := 0 to High(LParameters) do
  begin
    LParam := LParameters[I];
    LParams[I] := ConvertLuaToNative(AState, LParam.ParamType, I + 1);
  end;

  try
    // Execute the class method
    LReturnValue := FMethod.Invoke(FClass, LParams);

    // Convert return value to Lua
    if FMethod.ReturnType <> nil then
      Result := ConvertNativeToLua(AState, LReturnValue)
    else
      Result := 0;
  except
    on E: Exception do
    begin
      lua_pushstring(AState, AsUTF8(E.Message));
      lua_error(AState);
      Result := 0;
    end;
  end;
end;

{ TjetLua }
constructor TjetLua.Create();
begin
  inherited;

  Open();
end;

destructor TjetLua.Destroy;
begin
  Close();

  inherited;
end;

function TjetLua.Open(): Boolean;
begin
  Result := False;
  if Assigned(FState) then Exit;

  FContext := TRttiContext.Create;
  FWrappers := TObjectDictionary<string, TjetLuaMethodWrapper>.Create([doOwnsValues]);

  FState := luaL_newstate();
  if not Assigned(FState) then Exit;

  SetGCStepSize(200);

  luaL_openlibs(FState);

  LoadBuffer(@cLOADER_LUA, Length(cLOADER_LUA));

  // Create global jetLua table
  SetVariable('jetLua.luaJitVersion', LUAJIT_VERSION);
  SetVariable('jetLua.luaVersion', LUA_RELEASE);
  SetVariable('jetLua.version', JETLUA_VERSION_FULL);

  dbg_setup_default(FState);

  Result := True;
end;

procedure TjetLua.Close();
var
  LWrapper: TjetLuaMethodWrapper;
begin
  if not Assigned(FState) then Exit;

  lua_close(FState);
  FState := nil;

  for LWrapper in FWrappers.Values do
    TLuaBridgeRegistry.RemoveWrapper(Pointer(LWrapper));

  FWrappers.Free;
  FContext.Free;
end;

procedure TjetLua.Reset();
begin
  OnBeforeReset();
  Close();
  Open();
  OnAfterReset();
end;

procedure TjetLua.ValidateMethod(const AMethod: TRttiMethod);
var
  LParam: TRttiParameter;
  LValidType: Boolean;
begin
  if not AMethod.IsClassMethod then
    raise EjetLuaException.CreateFmt(
      'Method %s must be a class method (declare with class keyword)',
      [AMethod.Name]);

  if AMethod.ReturnType <> nil then
  begin
    LValidType := AMethod.ReturnType.TypeKind in
      [tkInteger, tkFloat, tkString, tkUString, tkEnumeration, tkClass, tkPointer, tkRecord];
    if not LValidType then
      raise EjetLuaException.CreateFmt(
        'Unsupported return type for method %s: %s',
        [AMethod.Name, AMethod.ReturnType.Name]);
  end;

  for LParam in AMethod.GetParameters do
  begin
    LValidType := LParam.ParamType.TypeKind in
      [tkInteger, tkFloat, tkString, tkUString, tkEnumeration, tkClass, tkPointer, tkRecord];
    if not LValidType then
      raise EjetLuaException.CreateFmt(
        'Unsupported parameter type in method %s, parameter %s: %s',
        [AMethod.Name, LParam.Name, LParam.ParamType.Name]);
  end;
end;

function TjetLua.PushValueToLua(const AValue: TValue): Boolean;
var
  LObj: TObject;
  LPtr: Pointer;
  LUserData: Pointer;
  LTypeInfo: PTypeInfo;
begin
  Result := True;
  LTypeInfo := AValue.TypeInfo;

  case AValue.Kind of
    tkInteger:
      lua_pushinteger(FState, AValue.AsInteger);

    tkFloat:
      lua_pushnumber(FState, AValue.AsExtended);

    tkString, tkUString, tkLString:
      lua_pushstring(FState, AsUTF8(AValue.AsString));

    tkEnumeration:
      if LTypeInfo = TypeInfo(Boolean) then
        lua_pushboolean(FState, Ord(AValue.AsBoolean))
      else
        lua_pushinteger(FState, AValue.AsOrdinal);

    tkClass:
      begin
        LObj := AValue.AsObject;
        if LObj <> nil then
        begin
          LUserData := lua_newuserdata(FState, SizeOf(TObject));
          TObject(LUserData^) := LObj;

          if luaL_newmetatable(FState, AsUTF8(LObj.ClassName)) <> 0 then
          begin
            lua_pushstring(FState, '__gc');
            lua_pushcclosure(FState, @LuaObjectGC, 0);
            lua_rawset(FState, -3);
          end;
          lua_setmetatable(FState, -2);
        end
        else
          lua_pushnil(FState);
      end;

    tkPointer:
      begin
        // Raw pointer
        LPtr := AValue.AsType<Pointer>;
        if LPtr <> nil then
          Result := PushPointer(LPtr)
        else
          lua_pushnil(FState);
      end;

    else
      begin
        lua_pushnil(FState);
        Result := False;
      end;
  end;
end;

function TjetLua.GetValueFromLua(const AStackIndex: Integer): TValue;
var
  LUserData: Pointer;
begin
  case lua_type(FState, AStackIndex) of
    LUA_TNIL:
      Result := TValue.Empty;

    LUA_TBOOLEAN:
      Result := TValue.From<Boolean>(LongBool(lua_toboolean(FState, AStackIndex)));

    LUA_TNUMBER:
      begin
        // Check if it's an integer or float
        if LongBool(lua_isinteger(FState, AStackIndex)) then
          Result := TValue.From<Int64>(lua_tointeger(FState, AStackIndex))
        else
          Result := TValue.From<Double>(lua_tonumber(FState, AStackIndex));
      end;

    LUA_TSTRING:
      Result := TValue.From<string>(string(lua_tostring(FState, AStackIndex)));

    LUA_TUSERDATA:
      begin
        LUserData := lua_touserdata(FState, AStackIndex);
        if LUserData <> nil then
        begin
          // Check if it's an object
          if lua_getmetatable(FState, AStackIndex) = 1 then
          begin
            lua_pop(FState, 1); // Pop metatable
            Result := TValue.From<TObject>(TObject(LUserData^));
          end
          else
            // Just a regular pointer
            Result := TValue.From<Pointer>(LUserData);
        end
        else
          Result := TValue.Empty;
      end;

    LUA_TLIGHTUSERDATA:
      Result := TValue.From<Pointer>(lua_touserdata(FState, AStackIndex));

    else
      Result := TValue.Empty;
  end;
end;

function TjetLua.LuaParamToString(const AValue: TVarRec): string;
var
  LPointerID: string;
begin
  case AValue.VType of
    vtInteger:
      Result := IntToStr(AValue.VInteger); // Convert integer to string
    vtInt64:
      Result := IntToStr(AValue.VInt64^); // Convert int64 to string
    vtExtended:
      Result := FloatToStr(AValue.VExtended^); // Convert floating-point to string
    vtString:
      Result := '"' + StringReplace(string(AValue.VString^), '"', '\"', [rfReplaceAll]) + '"'; // Escape and quote
    vtAnsiString:
      Result := '"' + StringReplace(string(AnsiString(AValue.VAnsiString)), '"', '\"', [rfReplaceAll]) + '"'; // Escape and quote
    vtUnicodeString:
      Result := '"' + StringReplace(string(UnicodeString(AValue.VUnicodeString)), '"', '\"', [rfReplaceAll]) + '"'; // Escape and quote
    vtBoolean:
      if AValue.VBoolean then
        Result := 'true' // Lua true literal
      else
        Result := 'false'; // Lua false literal
    vtChar:
      Result := '"' + AValue.VChar + '"'; // Single character as string
    vtWideChar:
      Result := '"' + AValue.VWideChar + '"'; // Wide character as string
    vtPointer:
      begin
        if AValue.VPointer = nil then
          Result := 'nil'
        else
        begin
          // Generate a unique ID for the pointer
          LPointerID := Format('_p%p', [AValue.VPointer]);

          // Register the pointer in Lua's global "pointers" table
          lua_getglobal(FState, 'pointers'); // _G["pointers"]
          if lua_type(FState, -1) <> LUA_TTABLE then
          begin
            // Create the "pointers" table if it doesn't exist
            lua_pop(FState, 1);
            lua_newtable(FState);
            lua_setglobal(FState, 'pointers');
            lua_getglobal(FState, 'pointers');
          end;

          // Store the pointer in the "pointers" table
          lua_pushlightuserdata(FState, AValue.VPointer); // Push the pointer as userdata
          lua_setfield(FState, -2, AsUTF8(LPointerID)); // pointers[_p<address>] = userdata
          lua_pop(FState, 1); // Pop the "pointers" table

          // Return the identifier to use in the Lua script
          Result := Format('pointers["%s"]', [LPointerID]);
        end;
      end;
  else
    raise Exception.Create('Unsupported parameter type in LuaParamToString');
  end;
end;

procedure TjetLua.RegisterMethod(const AMethod: TRttiMethod; const AClass: TClass);
var
  LWrapper: TjetLuaMethodWrapper;
  LWrapperPtr: Pointer;
begin
  ValidateMethod(AMethod);

  LWrapper := TjetLuaMethodWrapper.Create(AMethod, AClass);
  FWrappers.Add(AMethod.Name, LWrapper);

  LWrapperPtr := Pointer(LWrapper);
  TLuaBridgeRegistry.RegisterWrapper(LWrapperPtr, LWrapper);

  lua_pushlightuserdata(FState, LWrapperPtr);
  lua_pushcclosure(FState, @LuaMethodCallback, 1);
  lua_setfield(FState, -2, AsUTF8(AMethod.Name));
end;

function TjetLua.PushPointer(const APtr: Pointer; const ATypeInfo: PTypeInfo = nil): Boolean;
var
  LUserData: Pointer;
begin
  Result := True;

  if APtr = nil then
  begin
    lua_pushnil(FState);
    Exit;
  end;

  // Regular pointer
  LUserData := lua_newuserdata(FState, SizeOf(Pointer));
  PPointer(LUserData)^ := APtr;

  // If we know the type, create a metatable for it
  if ATypeInfo <> nil then
  begin
    if luaL_newmetatable(FState, AsUTF8(string(ATypeInfo.Name))) <> 0 then
    begin
      // Could add type-specific metamethods here if needed
      lua_pushstring(FState, '__gc');
      lua_pushcclosure(FState, @LuaPointerGC, 0);
      lua_rawset(FState, -3);
    end;
    lua_setmetatable(FState, -2);
  end;
end;

procedure TjetLua.CheckLuaError(const AError: Integer);
var
  LErr: string;
begin
  if FState = nil then Exit;

  case AError of
    // success
    0:
      begin

      end;
    // a runtime error.
    LUA_ERRRUN:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EjetLuaException.CreateFmt('Runtime error [%s]', [LErr]);
      end;
    // memory allocation error. For such errors, Lua does not call the error handler function.
    LUA_ERRMEM:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EjetLuaException.CreateFmt('Memory allocation error [%s]', [LErr]);
      end;
    // error while running the error handler function.
    LUA_ERRERR:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EjetLuaException.CreateFmt
          ('Error while running the error handler function [%s]', [LErr]);
      end;
    LUA_ERRSYNTAX:
      begin
        LErr := lua_tostring(FState, -1);
        lua_pop(FState, 1);
        raise EjetLuaException.CreateFmt('Syntax Error [%s]', [LErr]);
      end
  else
    begin
      LErr := lua_tostring(FState, -1);
      lua_pop(FState, 1);
      raise EjetLuaException.CreateFmt('Unknown Error [%s]', [LErr]);
    end;
  end;
end;

function LuaWrapperWriter(aState: Plua_State; const aBuffer: Pointer; aSize: NativeUInt; aData: Pointer): Integer; cdecl;
var
  LStream: TStream;
begin
  LStream := TStream(aData);
  try
    LStream.WriteBuffer(aBuffer^, aSize);
    Result := 0;
  except
    on E: EStreamError do
      Result := 1;
  end;
end;

procedure TjetLua.SaveByteCode(const AStream: TStream);
var
  LRet: Integer;
begin
  if not Assigned(FState) then Exit;

  if lua_type(FState, lua_gettop(FState)) <> LUA_TFUNCTION then Exit;

  try
    LRet := lua_dump(FState, LuaWrapperWriter, AStream);
    if LRet <> 0 then
      raise EjetLuaException.CreateFmt('lua_dump returned code %d', [LRet]);
  finally
    lua_pop(FState, 1);
  end;
end;

procedure TjetLua.Bundle(const AInFilename: string; const AOutFilename: string);
var
  LInFilename: string;
  LOutFilename: string;
  LStatus: Integer;
begin
  if FState = nil then Exit;

  if AInFilename.IsEmpty then  Exit;
  if AOutFilename.IsEmpty then Exit;
  LInFilename := AInFilename.Replace('\', '/');
  LOutFilename := AOutFilename.Replace('\', '/');
  LoadBuffer(@cLUABUNDLE_LUA, Length(cLUABUNDLE_LUA), False);

  lua_pushstring(FState, AsUTF8(AInFilename));
  lua_pushstring(FState, AsUTF8(AOutFilename));

  LStatus := lua_pcall(FState, 2, 0, 0);
  CheckLuaError(LStatus);

  lua_pop(FState, lua_gettop(FState));
end;

procedure TjetLua.RegisterRoutines(AClass: TClass; const ATableName: string);
var
  LRttiType: TRttiType;
  LMethod: TRttiMethod;
  LActualTableName: string;
begin
  if not Assigned(FState) then Exit;

  LRttiType := FContext.GetType(AClass);

  LActualTableName := ATableName;
  if LActualTableName = '' then
    LActualTableName := AClass.ClassName;

  lua_createtable(FState, 0, 0);

  lua_pushlightuserdata(FState, AClass);
  lua_setfield(FState, -2, '_class');

  for LMethod in LRttiType.GetMethods do
    if (LMethod.Visibility = mvPublished) and LMethod.IsClassMethod then
      RegisterMethod(LMethod, AClass);

  lua_setglobal(FState, AsUTF8(LActualTableName));
end;

function TjetLua.LoadFile(const AFilename: string; const AAutoRun: Boolean): Boolean;
var
  LFileContent: TBytes;
  LErr: string;
  LRes: Integer;
begin
  Result := False;
  if not Assigned(FState) then Exit;
  if AFilename.IsEmpty then Exit;
  if not TFile.Exists(AFilename) then Exit;

  // Read the file content and remove the BOM
  LFileContent := RemoveBOM(TFile.ReadAllBytes(AFilename));

  // Load or execute the script
  LRes := luaL_loadbuffer(FState, AsUTF8(TEncoding.UTF8.GetString(LFileContent)), Length(LFileContent), AsUtf8(AFilename));

  if LRes <> 0 then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EjetLuaException.CreateFmt('%s: %s', [AFilename, LErr]);
  end;

  // Execute the script if AAutoRun is True
  if AAutoRun and (lua_pcall(FState, 0, LUA_MULTRET, 0) <> 0) then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EjetLuaException.CreateFmt('%s: %s', [AFilename, LErr]);
  end;

  Result := True;
end;

procedure TjetLua.LoadString(const AData: string; const AAutoRun: Boolean);
var
  LErr: string;
  LRes: Integer;
  LData: string;
begin
  if not Assigned(FState) then Exit;

  LData := AData;
  if LData.IsEmpty then Exit;

  if AAutoRun then
    LRes := luaL_dostring(FState, AsUTF8(LData, True))
  else
    LRes := luaL_loadstring(FState, AsUTF8(LData, True));

  if LRes <> 0 then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EjetLuaException.Create(LErr);
  end;
end;

procedure TjetLua.LoadBuffer(const AData: Pointer; const ASize: NativeUInt; const AAutoRun: Boolean);
var
  LMemStream: TMemoryStream;
  LRes: Integer;
  LErr: string;
  LSize: NativeUInt;
begin
  if not Assigned(FState) then Exit;

  LMemStream := TMemoryStream.Create;
  try
    LMemStream.Write(AData^, ASize);
    LMemStream.Position := 0;
    LSize := LMemStream.Size;
    if AAutoRun then
      LRes := luaL_dobuffer(FState, LMemStream.Memory, LSize, 'LoadBuffer')
    else
      LRes := luaL_loadbuffer(FState, LMemStream.Memory, LSize, 'LoadBuffer');
  finally
    FreeAndNil(LMemStream);
  end;

  if LRes <> 0 then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EjetLuaException.Create(LErr);
  end;
end;

function TjetLua.RoutineExist(const AName: string): Boolean;
var
  LStatus: Integer;
begin
  // Attempt to load the name as a Lua expression
  LStatus := luaL_loadstring(FState, AsUTF8('return ' + AName));

  if LStatus = LUA_OK then
  begin
    // Execute the loaded chunk
    if lua_pcall(FState, 0, 1, 0) = LUA_OK then
      Result := lua_isfunction(FState, -1) or LongBool(lua_iscfunction(FState, -1))
    else
      Result := False;
  end
  else
    Result := False;

  // Clean up the Lua stack
  lua_settop(FState, 0);
end;

function TjetLua.VariableExist(const AName: string): Boolean;
var
  LTokens: TArray<string>;
  I: Integer;
begin
  Result := False;

  // Split the variable name into tokens
  LTokens := AName.Split(['.']);
  if Length(LTokens) = 0 then
    Exit; // Invalid input

  // Get the base variable
  lua_getglobal(FState, AsUTF8(LTokens[0]));

  for I := 1 to High(LTokens) do
  begin
    if lua_type(FState, -1) <> LUA_TTABLE then
    begin
      lua_pop(FState, 1); // Clean up the stack
      Exit(False); // Not a table, so the field doesn't exist
    end;

    // Navigate to the next field
    lua_getfield(FState, -1, AsUTF8(LTokens[I]));
    lua_remove(FState, -2); // Remove the parent table
  end;

  // If the final value is not nil, the variable exists
  Result := lua_type(FState, -1) <> LUA_TNIL;

  // Clean up the stack
  lua_pop(FState, 1);
end;

procedure TjetLua.SetVariable(const AName: string; const AValue: TValue);
var
  LTokens: TArray<string>;
  I: Integer;
  LFinalKey: string;
  LType: Integer;
begin
  // Split the variable name into components for nested table navigation
  LTokens := AName.Split(['.']);
  if Length(LTokens) = 0 then
    raise EjetLuaException.Create('Invalid variable name');

  // Get or create the base variable
  LType := lua_getglobal(FState, AsUTF8(LTokens[0])); // Push base variable onto the stack
  if LType = LUA_TNIL then
  begin
    // Base variable does not exist; create a new table
    lua_pop(FState, 1); // Remove nil
    lua_newtable(FState); // Create a new table
    lua_setglobal(FState, AsUTF8(LTokens[0])); // Assign the table globally
    lua_getglobal(FState, AsUTF8(LTokens[0])); // Push the new table onto the stack

    // Navigate through the nested fields
    for I := 1 to High(LTokens) - 1 do
    begin
      lua_getfield(FState, -1, AsUTF8(LTokens[I])); // Get the next field
      if lua_type(FState, -1) = LUA_TNIL then
      begin
        lua_pop(FState, 1); // Remove nil
        lua_newtable(FState); // Create a new table
        lua_pushvalue(FState, -1); // Duplicate the new table
        lua_setfield(FState, -3, AsUTF8(LTokens[I])); // Assign the new table to the parent
      end;
      lua_remove(FState, -2); // Remove the parent table
    end;

    // Push the value to set
    if not PushValueToLua(AValue) then
    begin
      lua_pop(FState, 1); // Clean up the stack
      raise EjetLuaException.CreateFmt('Unsupported value for "%s"', [AName]);
    end;

    // Set the final field
    LFinalKey := LTokens[High(LTokens)];
    lua_setfield(FState, -2, AsUTF8(LFinalKey)); // Assign the value to the field
    lua_pop(FState, 1); // Remove the remaining table
  end
  else
  begin
    // If it's not nil, handle it differently based on its type
    if LType <> LUA_TTABLE then
    begin
      // Push the value directly for global variable assignment
      if not PushValueToLua(AValue) then
      begin
        lua_pop(FState, 1); // Clean up the stack
        raise EjetLuaException.CreateFmt('Unsupported value for "%s"', [AName]);
      end;
      lua_setglobal(FState, AsUTF8(AName)); // Set the global variable
    end
    else
    begin
      // If it's a table, navigate and handle nested fields (same logic as for nil)
      for I := 1 to High(LTokens) - 1 do
      begin
        lua_getfield(FState, -1, AsUTF8(LTokens[I])); // Get the next field
        if lua_type(FState, -1) = LUA_TNIL then
        begin
          lua_pop(FState, 1); // Remove nil
          lua_newtable(FState); // Create a new table
          lua_pushvalue(FState, -1); // Duplicate the new table
          lua_setfield(FState, -3, AsUTF8(LTokens[I])); // Assign the new table to the parent
        end;
        lua_remove(FState, -2); // Remove the parent table
      end;

      // Push the value to set
      if not PushValueToLua(AValue) then
      begin
        lua_pop(FState, 1); // Clean up the stack
        raise EjetLuaException.CreateFmt('Unsupported value for "%s"', [AName]);
      end;

      // Set the final field
      LFinalKey := LTokens[High(LTokens)];
      lua_setfield(FState, -2, AsUTF8(LFinalKey)); // Assign the value to the field
      lua_pop(FState, 1); // Remove the remaining table
    end;
  end;
end;

function TjetLua.GetVariable(const AName: string): TValue;
var
  LScript: string;
  LStatus: Integer;
begin
  Result := TValue.Empty;

  // Check for valid variable
  if not VariableExist(AName) then Exit;

  // Construct the Lua script to get the variable
  LScript := Format('return %s', [AName]);

  // Load the script
  LStatus := luaL_loadstring(FState, AsUTF8(LScript));
  if LStatus <> LUA_OK then
    raise EjetLuaException.CreateFmt('Invalid variable name "%s": %s', [AName, lua_tostring(FState, -1)]);

  // Execute the script
  LStatus := lua_pcall(FState, 0, 1, 0); // 0 arguments, 1 result
  if LStatus <> LUA_OK then
    raise EjetLuaException.CreateFmt('Error retrieving variable "%s": %s', [AName, lua_tostring(FState, -1)]);

  // Check if the result is nil
  if lua_type(FState, -1) = LUA_TNIL then
  begin
    lua_pop(FState, 1); // Clean up the stack
    raise EjetLuaException.CreateFmt('Variable "%s" does not exist', [AName]);
  end;

  // Convert the Lua value to a Delphi TValue
  Result := GetValueFromLua(-1);
  lua_pop(FState, 1); // Clean up the stack
end;

function TjetLua.Call(const AName: string; const AParams: array of const): TValue;
var
  LScript: string;
  LParamStr: string;
  I: Integer;
  LStatus: Integer;
  LNumResults: Integer;
  LType: Integer;
begin
  Result := TValue.Empty;

  // Check if the function or construct exists
  lua_getglobal(FState, AsUTF8(AName));
  LType := lua_type(FState, -1);
  if not LType in [LUA_TFUNCTION, LUA_TTABLE, LUA_TUSERDATA, LUA_TNIL] then
  begin
    lua_pop(FState, 1); // Clean up the stack
    raise EjetLuaException.CreateFmt('Invalid Lua construct "%s": not callable', [AName]);
  end;

  // Convert parameters into a comma-separated string
  LParamStr := '';
  for I := Low(AParams) to High(AParams) do
  begin
    if I > Low(AParams) then
      LParamStr := LParamStr + ', ';

    // Convert parameter to Lua-compatible representation
    LParamStr := LParamStr + LuaParamToString(AParams[I]);
  end;

  // Form the Lua script (wrap the function call in a return statement)
  LScript := Format('return %s(%s)', [AName, LParamStr]);

  // Load the script
  LStatus := luaL_loadstring(FState, AsUTF8(LScript));
  if LStatus <> LUA_OK then
  begin
    // Retrieve and raise the Lua error
    raise EjetLuaException.CreateFmt('Error compiling Lua script: %s', [lua_tostring(FState, -1)]);
  end;

  // Execute the loaded script
  LNumResults := 1; // Expecting 1 result (function return value)
  LStatus := lua_pcall(FState, 0, LNumResults, 0);
  CheckLuaError(LStatus);

  // Retrieve the return value (if present)
  if lua_gettop(FState) > 0 then
  begin
    Result := GetValueFromLua(-1); // Extract value from the top of the stack
    lua_pop(FState, 1); // Remove the result from the stack
  end;

  // Clean up the Lua stack (ensure it's empty after execution)
  lua_settop(FState, 0);
end;

procedure TjetLua.CompileToStream(const AFilename: string; const AStream: TStream; const ACleanOutput: Boolean);
var
  LInFilename: string;
  LBundleFilename: string;
begin
  if not Assigned(FState) then Exit;

  LInFilename := AFilename;
  LBundleFilename := TPath.GetFileNameWithoutExtension(LInFilename) + '_bundle.lua';
  LBundleFilename := TPath.Combine(TPath.GetDirectoryName(LInFilename), LBundleFilename);

  LInFilename := LInFilename.Replace('\', '/');
  LBundleFilename := LBundleFilename.Replace('\', '/');

  Bundle(LInFilename, LBundleFilename);
  LoadFile(PChar(LBundleFilename), False);
  SaveByteCode(AStream);
  lua_pop(FState, lua_gettop(FState));

  if ACleanOutput then
  begin
    if TFile.Exists(LBundleFilename) then
    begin
      TFile.Delete(LBundleFilename);
    end;
  end;
end;

procedure TjetLua.AddSearchPath(const APath: string);
var
  LPathToAdd: string;
  LCurrentPath: string;
  LPath: string;
begin
  if not Assigned(FState) then Exit;

  LPath := APath;
  LPath := LPath.Replace('\', '/');

  // Check if APath already ends with "?.lua"
  if LPath.EndsWith('?.lua') then
    LPathToAdd := APath
  else
    LPathToAdd := IncludeTrailingPathDelimiter(LPath) + '?.lua';

  // Retrieve the current package.path
  lua_getglobal(FState, 'package'); // Get the "package" table
  if not lua_istable(FState, -1) then
    raise Exception.Create('"package" is not a table in the Lua state');

  lua_getfield(FState, -1, 'path'); // Get the "package.path" field
  if LongBool(lua_isstring(FState, -1)) then
    LCurrentPath := string(lua_tostring(FState, -1))
  else
    LCurrentPath := ''; // Default to empty if "path" is not set

  lua_pop(FState, 1); // Pop the "package.path" field

  // Check if the path is already included
  if Pos(LPathToAdd, LCurrentPath) = 0 then
  begin
    // Append the new path if not already included
    LCurrentPath := LPathToAdd + ';' + LCurrentPath;

    // Update package.path
    lua_pushstring(FState, AsUTF8(LCurrentPath)); // Push the updated path
    lua_setfield(FState, -2, 'path'); // Update "package.path"
  end;

  lua_pop(FState, 1); // Pop the "package" table
end;

procedure TjetLua.Print(const AText: string; const AArgs: array of const);
begin
  if not HasConsoleOutput() then Exit;
  Write(Format(AText, AArgs));
end;

procedure TjetLua.PrintLn(const AText: string; const AArgs: array of const);
begin
  if not HasConsoleOutput() then Exit;
  WriteLn(Format(AText, AArgs));
end;

const
  PAYLOADID = 'fa12d33b4ed84bc6a6dc4c2fd07a31e8';

function TjetLua.PayloadExist(): Boolean;
begin
  Result := False;
  if not Assigned(FState) then Exit;

  Result := ResourceExists(HInstance, PAYLOADID);
end;

function TjetLua.StorePayload(const ASourceFilename, AEXEFilename: string): Boolean;
var
  LStream: TMemoryStream;
begin
  Result := False;
  if not Assigned(FState) then Exit;

  if not TFile.Exists(ASourceFilename) then Exit;
  if not TFile.Exists(AEXEFilename) then Exit;
  if not IsValidWin64PE(AEXEFilename) then Exit;

  LStream := TMemoryStream.Create();
  try
    CompileToStream(ASourceFilename, LStream, True);
    if LStream.Size > 0 then
    begin
      Result := AddResFromMemory(AEXEFilename, PAYLOADID, LStream.Memory, LStream.Size);
    end;
  finally
    LStream.Free();
  end;
end;

function TjetLua.RunPayload(): Boolean;
var
  LResStream: TResourceStream;
  LErr: string;
  LRes: Integer;
begin
  Result := False;
  if not Assigned(FState) then Exit;

  if not PayloadExist() then Exit;

  Reset();

  LResStream := TResourceStream.Create(HInstance, PAYLOADID, RT_RCDATA);
  try
    LoadBuffer(LResStream.Memory, LResStream.Size, False);
    LResStream.Free();
    LResStream := nil;
  finally
    if Assigned(LResStream) then
      LResStream.Free();
  end;

  // Check if the stack has any values
  if lua_gettop(FState) = 0 then
    raise EjetLuaException.Create('Lua stack is empty. Nothing to run.');

  // Check if the top of the stack is a function
  if lua_type(FState, lua_gettop(FState)) <> LUA_TFUNCTION then
    raise EjetLuaException.Create('Top of the stack is not a callable function.');

  // Call the function on the stack
  LRes := lua_pcall(FState, 0, LUA_MULTRET, 0);

  // Handle errors from pcall
  if LRes <> LUA_OK then
  begin
    LErr := lua_tostring(FState, -1);
    lua_pop(FState, 1);
    raise EjetLuaException.Create(LErr);
  end;

  Result := True;
end;

procedure TjetLua.SetGCStepSize(const AStep: Integer);
begin
  FGCStep := AStep;
end;

function TjetLua.GetGCStepSize(): Integer;
begin
  Result := FGCStep;
end;

function TjetLua.GetGCMemoryUsed(): Integer;
begin
  Result := 0;
  if not Assigned(FState) then Exit;

  Result := lua_gc(FState, LUA_GCCOUNT, FGCStep);
end;

procedure TjetLua.CollectGarbage();
begin
  if not Assigned(FState) then Exit;

  lua_gc(FState, LUA_GCSTEP, FGCStep);
end;

procedure TjetLua.OnBeforeReset();
begin
end;

procedure TjetLua.OnAfterReset();
begin
end;

{$ENDREGION}

{$REGION ' CRUNTIME '}
const
  ucrtbase = 'ucrtbase.dll';
  ucrt = 'api-ms-win-crt-stdio-l1-1-0.dll';
  msvcrt = 'msvcrt.dll';
  ntdll = 'ntdll.dll';

{ ntdll }
procedure RtlRestoreContext; stdcall; external ntdll;
procedure RtlUnwindEx; stdcall; external ntdll;
procedure RtlLookupFunctionEntry; stdcall; external ntdll;
procedure RtlCaptureContext; stdcall; external ntdll;
procedure RtlVirtualUnwind; stdcall; external ntdll;

{ kenerl32}
procedure GetModuleHandleExA; stdcall; external kernel32;

{ msvcrt }
procedure __mingw_vsprintf; cdecl; external msvcrt name 'vsprintf';
procedure __mingw_vfprintf; cdecl; external msvcrt name 'vfprintf';
procedure fscanf; cdecl; external msvcrt;

{ ucrtbase }
procedure feof; cdecl; external ucrtbase;

{ ucrt }
procedure _ftelli64; cdecl; external ucrt;
procedure _fseeki64; cdecl; external ucrt;
procedure __DestructExceptionObject; cdecl; external ucrt;
procedure __p__fmode; cdecl; external ucrt;
procedure putchar; cdecl; external ucrt;
procedure cosh; cdecl; external ucrt;
procedure memmove; cdecl; external ucrt;
procedure _fscalef; cdecl; external ucrt;
procedure strtoul; cdecl; external ucrt;
procedure strncpy; cdecl; external ucrt;
procedure sinh; cdecl; external ucrt;
procedure atan; cdecl; external ucrt;
procedure modf; cdecl; external ucrt;
procedure tanh; cdecl; external ucrt;
procedure __mingw_strtod; cdecl; external ucrt name 'strtod';
procedure system; cdecl; external ucrt;
procedure rename; cdecl; external ucrt;
procedure malloc; cdecl; external ucrt;
procedure exit; cdecl; external ucrt;
procedure memset; cdecl; external ucrt;
procedure __intrinsic_setjmpex; cdecl; external ucrt;
procedure _time64; cdecl; external ucrt;
procedure strcmp; cdecl; external ucrt;
procedure strlen; cdecl; external ucrt;
procedure memcpy; external ucrt;
procedure strchr; cdecl; external ucrt;
procedure longjmp; cdecl; external ucrt;
procedure abort; cdecl; external ucrt;
procedure floor; cdecl; external ucrt;
procedure memcmp; cdecl; external ucrt;
procedure strcoll; cdecl; external ucrt;
procedure strpbrk; cdecl; external ucrt;
procedure strcpy; cdecl; external ucrt;
procedure localeconv; cdecl; external ucrt;
procedure strspn; cdecl; external ucrt;
procedure strncmp; cdecl; external ucrt;
procedure _errno; cdecl; external ucrt;
procedure strerror; cdecl; external ucrt;
procedure fopen; cdecl; external ucrt;
procedure __acrt_iob_func; cdecl; external ucrt;
procedure freopen; cdecl; external ucrt;
procedure ferror; cdecl; external ucrt;
procedure fclose; cdecl; external ucrt;
procedure getc; cdecl; external ucrt;
procedure fread; cdecl; external ucrt;
procedure strstr; cdecl; external ucrt;
procedure realloc; cdecl; external ucrt;
procedure free; cdecl; external ucrt;
procedure fflush; cdecl; external ucrt;
procedure getenv; cdecl; external ucrt;
procedure pow; cdecl; external ucrt;
procedure fmod; cdecl; external ucrt;
procedure frexp; cdecl; external ucrt;
procedure ldexp; cdecl; external ucrt;
procedure fwrite; cdecl; external ucrt;
procedure fputs; cdecl; external ucrt;
procedure fputc; cdecl; external ucrt;
procedure isalnum; cdecl; external ucrt;
procedure toupper; cdecl; external ucrt;
procedure fgets; cdecl; external ucrt;
procedure memchr; cdecl; external ucrt;
procedure _popen; cdecl; external ucrt;
procedure tmpfile; cdecl; external ucrt;
procedure clearerr; cdecl; external ucrt;
procedure ungetc; cdecl; external ucrt;
procedure isspace; cdecl; external ucrt;
procedure isxdigit; cdecl; external ucrt;
procedure _pclose; cdecl; external ucrt;
procedure fseek; cdecl; external ucrt;
procedure ftell; cdecl; external ucrt;
procedure setvbuf; cdecl; external ucrt;
procedure acos; cdecl; external ucrt;
procedure atan2; cdecl; external ucrt;
procedure log; cdecl; external ucrt;
procedure tan; cdecl; external ucrt;
procedure clock; cdecl; external ucrt;
procedure _gmtime64; cdecl; external ucrt;
procedure _localtime64; cdecl; external ucrt;
procedure strftime; cdecl; external ucrt;
procedure _difftime64; cdecl; external ucrt;
procedure remove; cdecl; external ucrt;
procedure setlocale; cdecl; external ucrt;
procedure _mktime64; cdecl; external ucrt;
procedure tmpnam; cdecl; external ucrt;
procedure isalpha; cdecl; external ucrt;
procedure iscntrl; cdecl; external ucrt;
procedure tolower; cdecl; external ucrt;
procedure isgraph; cdecl; external ucrt;
procedure islower; cdecl; external ucrt;
procedure ispunct; cdecl; external ucrt;
procedure isupper; cdecl; external ucrt;
procedure strrchr; cdecl; external ucrt;
procedure asin; cdecl; external ucrt;
procedure ceil; cdecl; external ucrt;
procedure log10; cdecl; external ucrt;
{$ENDREGION}

{$REGION ' UNIT INIT '}
initialization
begin
  ReportMemoryLeaksOnShutdown := True;
  SetConsoleCP(CP_UTF8);
  SetConsoleOutputCP(CP_UTF8);
  EnableVirtualTerminalProcessing();
end;

finalization
begin
end;
{$ENDREGION}

end.


