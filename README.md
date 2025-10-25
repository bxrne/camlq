# Camlq

A JSON parser written in OCaml.
Yet to be benchmarked and fuzzed against the [RFC 8259](https://datatracker.ietf.org/doc/html/rfc8259) standard.

## Features

- **Pure OCaml Implementation** - No external dependencies for JSON parsing
- **Pipe Support** - Read JSON from stdin for easy integration

## Installation

### Prerequisites

- OCaml 4.12+ 
- Dune build system
- OCamlfind

### Build from Source

```bash
git clone https://github.com/bxrne/camlq.git
cd camlq
dune build
```

## Usage

### Command Line

Camlq reads JSON from stdin and outputs the parsed result:

```bash
# Parse from a file
cat sample.json | camlq

# Parse from echo
echo '{"name": "Alice", "age": 25}' | camlq

# Parse from curl
curl -s https://api.example.com/data.json | camlq
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

