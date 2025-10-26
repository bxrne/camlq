# Camlq

A JSON parser and query tool written in OCaml, inspired by jq.
Yet to be benchmarked and fuzzed against the [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259) standard.

## Features

- **Pure OCaml Implementation** - No external dependencies for JSON parsing
- **Pipe Support** - Read JSON from stdin for easy integration
- **jq-like Querying** - Extract and filter JSON data with familiar syntax

## Installation

### Prerequisites

- OCaml 5.0+
- Dune build system

### Build from Source

```bash
git clone https://github.com/bxrne/camlq.git
cd camlq
dune build
```

## Usage

### Command Line

Camlq reads JSON from stdin and outputs the parsed result. Optionally, you can provide a jq-like query to extract specific data:

```bash
# Parse from a file
cat sample.json | camlq

# Parse from echo
echo '{"name": "Alice", "age": 25}' | camlq

# Parse from curl
curl -s https://api.example.com/data.json | camlq

# Query specific data
cat sample.json | camlq .name
cat sample.json | camlq .address.city
cat sample.json | camlq .courses[0]
cat sample.json | camlq .courses[]
```

### Examples

```bash
# Using the provided sample
$ cat sample.json | camlq
{"name": "John Doe", "age": 30., "is_student": false, "courses": ["Math", "Science", "History"], "address": {"street": "123 Main St", "city": "Anytown", "zip": "12345"}, "scores": [85.5, 92., 78.3], "active": true, "metadata": null}

# Simple object
$ echo '{"key": "value"}' | camlq
{"key": "value"}

# Array of numbers
$ echo '[1, 2, 3]' | camlq
[1., 2., 3.]

# Error handling
$ echo '{"invalid": json}' | camlq
Error: Parse error: Expected colon in object
```

## Querying

Camlq supports jq-like syntax for querying and extracting data from JSON. The query syntax is similar to jq but simplified for common use cases.

### Query Syntax

- `.` - Root object (identity operation)
- `.key` - Access object property
- `.key.subkey` - Nested property access
- `.[index]` - Array element access (0-based)
- `.[]` - Array iteration (returns all elements)

### Query Examples

```bash
# Get a property
$ cat sample.json | camlq .name
"John Doe"

# Access nested properties
$ cat sample.json | camlq .address.city
"Anytown"

# Array indexing
$ cat sample.json | camlq .courses[0]
"Math"

$ cat sample.json | camlq .courses[2]
"History"

# Array iteration (returns multiple lines)
$ cat sample.json | camlq .courses[]
"Math"
"Science"
"History"

# Complex queries
$ echo '{"users": [{"name": "Alice", "age": 25}, {"name": "Bob", "age": 30}]}' | camlq .users[1].name
"Bob"

# Error cases
$ cat sample.json | camlq .nonexistent
Error: Query '.nonexistent' returned no results

$ cat sample.json | camlq .courses[10]
Error: Query '.courses[10]' returned no results
```

### Query Language Details

- **Property Access**: Use `.property` to access object properties
- **Array Indexing**: Use `.[number]` for 0-based array access
- **Array Iteration**: Use `.[]` to iterate over all array elements
- **Chaining**: Combine operations like `.users[0].name`
- **Error Handling**: Invalid queries or missing data return appropriate error messages

## Development

### Running Tests

```bash
# Run the full test suite
dune runtest
```


### Building

```bash
# Build the library and executable
dune build

# Build in release mode
dune build --profile=release

```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

