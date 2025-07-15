# Statix — Codebase Statistics Analyzer

Statix is a lightweight, command-line tool written in Haskell that scans entire codebases to provide detailed statistics, helping developers understand their projects at a glance.

## Features

- Recursively scans directories for source files with common programming language extensions including Haskell, Python, JavaScript, TypeScript, C, C++, Java, Go, Ruby, PHP, Rust, Zig, Prolog, HTML, CSV, and more.
- Calculates key metrics for each file:
  - Total lines of code
  - Number of comment lines
  - Number of TODO comments
  - Count of function/method definitions
  - Average function length (lines per function)
  - File size in bytes
- Designed to be extensible for additional languages and metrics.
- Simple CLI usage for quick insights into any code folder.

## Usage

```bash
cabal run statix -- <path-to-your-codebase>
