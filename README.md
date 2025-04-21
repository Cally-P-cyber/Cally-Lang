# About Cally-Lang
Dont like pythons white space but like its readability and easy of use.<br/>
Well look no further Cally Lang is here.<br/>
Cally-Lang is a great alternative if you don't care about performace, since the interpreter is written entirely in Powershell 7.4 (Compatible with 5.4 => lower version might not function properly).<br/>
Why Powershell?<br/>
![360_F_166100342_KbTGIRrnrlwGDZSXSMpH3zfn2dxyTKae](https://github.com/user-attachments/assets/5c8e4128-cc89-480e-9142-820d99d6366d)<br/>

# Running Cally-Lang<br/>

## Command-Line Parameters

- **Switch flags:**  
  - **-ast** : Outputs the AST (in JSON format).  
  - **-ts**  : Outputs the token stream.

- **args:**  
  Additional arguments passed at startup. These can be a single argument or multiple arguments.

### Usage

```
clc.ps1 <Path_To_Your_Program.clp> <args> [-ast] [-ts]
```

- **Example without arguments or switches:**

  ```
  clc.ps1 my_program.clp
  ```

- **Example with 1 argument and both switch flags:**

  ```
  clc.ps1 my_program.clp 1234 -ast -ts
  ```

- **Example with many arguments:**

  ```
  clc.ps1 my_program.clp 1 2 3 4 -ast -ts
  ```

## Language Syntax

### Initialization Syntax

Comments are signified by a double slash (`//`).  
Users may define the main entry point with or without explicit parameters.

**With parameters:**

```
Main(args){
    // Code goes here.
}
```

**Without parameters:**

```
Main {
    // Code goes here.
}
```

### Using `args`

- **Single argument example:**  
  Passed args: `Hello`
  
  ```
  Main(args){
      print(args); // Output: Hello
  }
  ```

- **Multiple arguments with a custom name:**  
  Passed args: `1 2 3 4`
  
  ```
  Main(valuesPassed){
      print(valuesPassed[0]); // Output: 1
  }
  ```

*Note:* A semicolon is required at the end of all variable declarations, built-in functions (print, len, input), assignment operations, and mathematical operations.

### Variable Syntax

```
string_Example = "Hello";
int_Example = 1234;
array_Example = [1,2,3,4];
object_Example = { name: "Bob", age: 30 };
```

### Print and Input Operator Syntax

```
name = input("What is your name: ");
age = input("What is your age: ");
print("Name: " + name + "\n");
print("Age: " + age + "\n");
```

### Length Operator Syntax

```
val = "abcd";
arr = [1,2];
len(val); // Returns 4.
len(arr); // Returns 2.
```

### For Loop Syntax

```
for(i=0; i < 10; i++){
    // Code goes here.
}

b = 5;
for(i=0; i <= b; i++){
    // Code goes here.
}
```

### While Loop Syntax

```
i = 0;
while(i < 10){
    // Code goes here.
    i++;
}
```

## Supported Operations

Cally‑Lang supports the following math and comparison operations:

- **Addition:** `+`
- **Subtraction:** `-`
- **Multiplication:** `*`
- **Division:** `/` (integer)
- **Modulus:** `%`
- **Equality:** `==`
- **Inequality:** `!=`
- **Less than:** `<`
- **Less than or equal to:** `<=`
- **Greater than:** `>`
- **Greater than or equal to:** `>=`
- **Post-increment:** `++`
- **Post-decrement:** `--`

## Native PowerShell Integration and Variable Interpolation

Cally‑Lang allows embedding native PowerShell code using the `p{ … }` syntax.

### Example: Get the Current Date Using PowerShell

```
Main {
    a = "Hello Cally-Lang";
    x = p{
         return Get-Date
    };
    print(x);
}
```


## Additional Notes

- Semicolons required on all statements including on powershell interpolated statments.
- Comments start with `//`.

---





