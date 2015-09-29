During a Merlin session, the editor launches an ocamlmerlin process
and communicates with it by writing queries on stdin and reading
responses on stdout.

Merlin processes queries synchronously, reading them one by one and
writing a response for each query, in the same order.  It will wait
for more queries until stdin reaches end-of-file.

The complete set of commands is defined in `src/frontend/protocol.ml`.

Queries and responses can be serialized in two different formats:
- JSON, defined in `src/frontend/IO.ml`;
- SEXP, defined in `src/frontend/IO_sexp.ml`.

JSON is the default, SEXP can be selected by passing `-protocol sexp`
to merlin process.

The rest of the document will describe sample sessions and commands
using JSON format.  The SEXP format is mechanically derived from JSON,
flow is the same.

# Merlin commands

Commands can be classified in three categories:
- _synchronization_, to share and update the content of the editor
  buffer;
- _query_, to ask merlin for information (type, completion, documentation);
- _context_, to describe the file being worked on and how it is
  related to the environment (dependencies, include paths, ...).

The basic workflow for an editor is to synchronize then run a query
each time merlin is invoked.

When working on a project with multiple files, context becomes useful
to switch between buffers.

A simple session (user-commands prefixed by >, merlin responses by <):

```json
> ["tell","source","let f x = x;;"]
< ["return",{"cursor":{"line":1,"col":13},"marker":false}]
> ["type","expression","f"]
< ["return","'a -> 'a"]
```

## Responses

Responses are always of the form `[kind,payload]` where `payload`
depends on the value of `kind`, which can be:

`"return"` when the command succeeded, `payload` depends on the
command being responded to.

`"failure"` when merlin was used in an incorrect way (e.g command was
malformed or unknown), `payload` is a string describing the failure.
Editor mode should be fixed.

`"error"` when something wrong prevented merlin from answering:
invalid files (for instance wrong OCaml version), missing packages,
etc.  `payload` is a string describing the error, user should be
notified to fix the environment.

`"exception"` when something unexpected happened.  Merlin should be
fixed, please report the error.  `payload` is an arbitraty json value.
    
## Synchronization

Merlin maintains a "virtual cursor", similar to the cursor that allows
to enter text in a text editor.

Sharing the content of the buffer to merlin is done by moving the
cursor to the desired position then sending text.

Two differences with the text cursor you are familiar with:
- text always replace what comes after the cursor, sending something clears previous contents,
- the cursor cannot be freely moved, merlin may align it with entities which it finds meaningful (usually a token).

Thus, after a move, merlin will always return the actual position of
the cursor.

Positions are represented as a pair of a line and a
column, `{"line":1,"col":0}` in JSON.
The first line has number 1.

### Tell

All telling commands return a cursor state.

```json
["tell","start"]
["tell","start","at",{"line":int, "col":int}]
```

Prepare merlin for receiving text. If a position is specified, the cursor will be moved there.
Merlin will return the actual position where text will be inserted as a cursor state object `{"cursor":position, "marker":bool}`, so the editor should be prepared to send more text.
Don't bother about the `"marker"` field yet.

```json
["tell","source",string]
["tell","file",string]
```

`"source"` appends the string argument to buffer contents (at the cursor position). Returns the updated cursor state.

`"file"` appends the contents of the file specified as argument.
Like calling `["tell","source",...]` with the contents of the file as argument.

Be careful that EOF is not set, see the next commands.

```json
["tell","eof"]
["tell","source-eof",string]
["tell","file-eof",string]
```

Signal EOF at the current cursor position, or after appending some text.
Merlin behaves slightly differently when EOF is not set, for instance by not reporting errors about unterminated statements.
You shouln't usually bother about that: unless you know you are working with unfinished contents (e.g REPL input), always set EOF at the end of the buffer.

```json
["tell","marker"]
```

Set the marker at the current position. Useful only in advanced use-cases, see the marker section for more information.

```json
["drop"]
```

This command makes merlin forget everything after the cursor.

### Seek

These commands move the cursor without affecting the content of the buffer. They also return a cursor state.

Moving the cursor is useful to control the prefix of the buffer that must be considered by merlin when processing queries.
Practical for debugging but doesn't matter for basic usecases.


```json
["seek","position"]
```

Returns the current position of the cursor without doing anything else.

```json
["seek","before",position]
["seek","exact",position]
```

Move the cursor to the requested position. If this position happens to be in the middle of a token, `"exact"` will set the cursor at this token while `"before"` will move to the preceding one.

```json
["seek","end"]
```

Move the cursor to the last position known to merlin.

```json
["seek","marker"]
```

Move the cursor to the last state where the marker was on stack. For advanced uses, see marker section.

### Configuration

#### Flags

```json
["flags","add",["-rectypes", "-safe-string", ...]]
["flags","clear"]
```

Pass the same flags as you would pass to the OCaml compiler. Run `ocamlmerlin -help` to get a list of known flags.
`"add"` appends to the list of flags, `"clear"` resets to the empty flag list.

Returns `{"result":true}` if everything went well or `{"failures":string list, "result":true}` in case of error.

```json
["flags","get"]
```

Returns a `string list list` (eg `[["-rectypes"],["-safe-string"]]`) resulting from the previous invocations of `["flags",("clear"/"add")]`.

#### Findlib packages

```json
["find","use",["lwt","yojson",...]]
```

Load findlib packages in current buffer.
Returns `{"result":true}` if everything went well or `{"failures":string list, "result":true}` in case of error.

```json
["find","list"]
```

Returns a `string list` of all known packages.

#### Syntax extensions

```json
["extension","enable",["lwt","js",...]]
["extension","disable",["lwt","js",...]]
```

Enable or disable syntax extensions in current buffer.

```json
["extension","list"]
["extension","list","enabled"]
["extension","list","disabled"]
```

List all known / currently enabled / currently disabled extensions as a `string list`.

#### Paths

```json
["path","add","source",[path1, path2, ...]]
["path","add","build",[path1, path2, ...]]
["path","remove","source",[path1, path2, ...]]
["path","remove","build",[path1, path2, ...]]
```

Merlin maintains different list of paths to process buffer and queries.
`"source"` is where `.ml` and `.mli` files are searched for, `"build"` is for `.cmi` and `.cmt`.

```json
["path","list","source"]
["path","list","build"]
```

Get current value of path variables as a `string list`.

```json
["path","reset"]
```

Reset path variables to default value (by default just the standard library and the buffer directory).

### Queries

```json
["type","expression",string]
["type","expression",string,"at",{"line":int,"col":int}]
["type","enclosing","at",{"line":int,"col":int}]
["type","enclosing",{"expr":string,"offset":int},{"line":int,"col":int}]
["type","case","analysis","from",position,"to",position]
["enclosing",position]
["complete","prefix",string,"at",position]
["complete","prefix",string,"at",position,"with","doc"]
["expand","prefix",string,"at",position]
["document",string,"at",position]
["document",null,"at",position]
["occurrences","ident","at",position]
["locate",string,"ml","at",position]
["locate",null,"ml","at",position]
["locate",string,"mli","at",position]
["locate",null,"mli","at",position]
["outline"]
["errors"]
["which","path",string]
["which","path",string list]
["which","with_ext",string]
["which","with_ext",string list]
["project","get"]
["version"]
```

### Context

Merlin keep tracks of multiple buffer. All commands apply to the active buffer.
`"checkout"` command allows to change the active buffer.
It returns a `cursor state` object describind the state of the checked out buffer (see `"tell"` command).


```json
["checkout", "ml"]
["checkout", "mli"]
```

Switch to "default" buffer for "ml", "mli".
It will be in the state you left it last time it was used, unless merlin decided to garbage collect because of memory pressure (any buffer left in background is either untouched or resetted because of collection).

```json
["checkout", "auto", string]
["checkout", "ml"  , string]
["checkout", "mli" , string]
```

Checkout buffer at a given path, interpreting it as an ml, an mli, or infer that from path extension (defaulting to ml).
File at path is not loaded, path is only used as a key to refer to the buffer and look for _.merlin_ files.

```json
["checkout", "dot_merlin", string list, "auto", string]
["checkout", "dot_merlin", string list, "ml"  , string]
["checkout", "dot_merlin", string list, "mli" , string]
```

Same as `["checkout", _, string]`, but rather than inferring the _.merlin_ from the path, use the explicit list of files.

#### Contextual commands

An important variant of this scheme are the _contextual commands_.
All merlin commands except `"checkout"` can be wrapped in a dictionary looking like:

```json
{
  "context": context,
  "query": command
}
```

Where `command` is a merlin command and context would be the list of arguments passed to `"checkout"`.

This has the same effect as executing:

```json
["checkout", context...]
[command...]
```

This is useful to prevent race conditions resulting from concurrent manipulations of different buffers in the editor, by making sure each command is interpreted in the appropriate context.


### Misc

```json
["boundary","next","at",position]
["boundary","prev","at",position]
["boundary","current","at",position]
["boundary","at",position]
```

#### Debugging Merlin

```json
["dump","env"]
["dump","env","at",position]
["dump","full_env"]
["dump","full_env","at",position]
["dump","sig"]
["dump","parser"]
["dump","recover"]
["dump","exn"]
["dump","browse"]
["dump","typer","input"]
["dump","typer","output"]
["dump","tokens"]
["dump","flags"]
["dump","warnings"]
```

--

# Basic types

## Position

A json-object of the form: 

    position = {"line":int, "col":int}

## Cusor state

A json-object of the form: 

    cursor = {"cursor":position, "marker":bool}

## Location

A json-object having at least the fields: 

    location <= {"start":position,"end":position}

## Filenames

Strings addressing file system objects:

    path = string, points any file or directory
    directory = string, expected to name a directory
    filename = string, expected to name a regular file

# Buffer management

## tell

Use ['tell','struct','...ocaml impl...'] to send source code from editor to
merlin.  The command answers the boolean 'true' if parsing ended, 'false' if it
needs more input.  In this case, the only allowed command is
['tell','struct','next source content'].  To interrupt the parser, run
['tell','struct',''] (meaning eof) or ['tell','struct',null].

### ["tell","struct",string]

Send a chunk of ml source code. Merlin will wait for the rest of the input.

### ["tell","end",string]

Same as above, but tell merlin that you want to stop as soon as possible.
Merlin will stop waiting for input once the current definition is complete.

### ["tell","struct",null]

Notify merlin that EOF has been reached.

### ["tell","end",null]

Same as ["tell","struct",null]

## seek

All seek functions return the position of Merlin virtual cursor after
their execution, as a position object.

### ["seek","position"]

Return the current position of the cursor.

### ["seek","before",position]

Move cursor just before the definition around specified position.

### ["seek","exact",position]

Move cursor to the position that has been given, or just after if this is in
the middle of an ml definition.

### ["seek","end"]

Put cursor at the end of file.

### ["seek","maximize\_scope"]

Move cursor down until the current scope is escaped (usually when
encountering an "end" of structure).

## Buffers management, dependency reloading

### ["reset"]

Clear content of virtual buffer.

### ["reset",path]

Clear content of virtual buffer and set its name.
This is used to report more informative error messages (the same the compiler
would report for a file with the given path).

### ["refresh"]

Reload all dependencies and try to retype the current file.

### ["refresh","quick"]

Try to detect dependencies that have changed and reload them. If needed,
retype current file.

# Wizardry

## type

Returns the type of an expression as a string.
['type','expression','... ml expression'] tries to type expression in global env.
['type','at',{'line':l,'col':c}] returns the type of the expression at given position(BUGGY)

### ["type","expression",string]
### ["type","expression",string,"at",position]
### ["type","at",position]
### ["type","enclosing",position]

## complete

['complete','prefix','...identifier path...'] or
['complete','prefix','...identifier path...', 'at', {'line':l,'col':c}]
returns possible completions in global environement or in environment
surrounding given position for given prefix/path (path of the form: Module.ident)

### ["complete","prefix",string]
### ["complete","prefix",string,"at",position]

## locate

['locate', ident] returns the position where this identifier is introduced.

Answers are of this shape :

- "Not found" (string)
- ["file": string , "pos": { "line" : int , "col": int }]

## boundary

### ["boundary", "prev|current|next"]
Return the boundary of the phrase before, at, or after the cursor.

### ["boundary", "prev|current|next", "at", pos]

Return the boundary of the phrase before, at, or after the given position.

## errors

### ["errors"]

# Path management

## find

### ["find","use",string list]
### ["find","list"]

## which

### ["which","path",string]
### ["which","with\_ext",string]

## cd

### ["cd",directory]

## path

    var = string

### ["path","list"]

List known path variables. As of today (07/2013), there is "build" and
"source".

### ["path","list",var]

List content of variable.

### ["path","add",var,directory]

Add a path to a variable.

### ["path","remove",var,directory]

Remove a path from a variable.

### ["path","reset"]

Reset all variables to default content (usually, ocaml std lib path).

### ["path","reset",var]

Reset specified variable to its default value.

## Project file

Loading of ".merlin" files.

### ["project","load",filename]

Parse and load given filename as a ".merlin".
Returns the list of files that have been loaded (as a ".merlin" can depend on
another one that will be loaded recursively).

### ["project","find",path]

Try to find a file named ".merlin" in any directory from given path to "/",
then behave as ["project","load"].with this file.

# Debug

## dump

### ["dump","env"]
### ["dump","env","at",position]
### ["dump","sig"]
### ["dump","chunks"]
### ["dump","tree"]

## help

## ["help"]

List known commands
