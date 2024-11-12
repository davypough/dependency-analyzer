# Dependency Analyzer Test Suite

This directory contains the test suite for the dependency-analyzer system. The tests verify the functionality of package dependency analysis, file dependency tracking, and system component relationships.

## Structure

The test suite is organized into several components:

```
tests/
├── README.md              # This file
├── test-suite.lisp       # Main test suite definition
├── helpers/              # Support code for tests
│   ├── test-utils.lisp   # Common utility functions
│   ├── test-data.lisp    # Shared test data structures
│   └── fixtures.lisp     # Test fixtures
├── package-tests/        # Package dependency tests
├── file-tests/          # File dependency tests
├── symbol-tests/        # Symbol-level dependency tests
└── system-tests/        # ASDF system dependency tests
```

## Running Tests

### Loading the Test System

First, ensure the dependency-analyzer system is loaded:

```lisp
(asdf:load-system :dependency-analyzer/tests)
```

### Running All Tests

To run the complete test suite:

```lisp
(dep/tests:test-all)
```

### Running Individual Test Suites

You can run specific test suites:

```lisp
;; Package dependency tests
(dep/tests:test-packages)

;; File dependency tests
(dep/tests:test-files)

;; Symbol dependency tests
(dep/tests:test-symbols)

;; System dependency tests
(dep/tests:test-systems)
```

## Test Output

Test results are displayed using FiveAM's output format. Each test will show:
- A description of what's being tested
- Pass/fail status
- Any error messages for failed tests
- A summary of total tests run, passed, and failed

Example output:
```
Running test suite DEPENDENCY-ANALYZER-TESTS
 Running test PACKAGE-TESTS
  Running test TEST-PACKAGE-DEFINITIONS..ok
  Running test TEST-PACKAGE-EXPORTS..ok
 Running test FILE-TESTS
  Running test TEST-FILE-DEPENDENCIES..ok
Did 24 checks.
    Pass: 24 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```

## Adding New Tests

### Test Organization

1. Place new tests in the appropriate subdirectory based on what they test:
   - Package-level tests go in `package-tests/`
   - File-level tests go in `file-tests/`
   - Symbol-level tests go in `symbol-tests/`
   - System-level tests go in `system-tests/`

2. Use the provided fixtures and utilities:
   ```lisp
   (def-fixture with-test-files ()
     ...)

   (test test-my-feature
     "Test description"
     (with-fixture with-test-files ()
       (is ... "Test assertion")))
   ```

### Common Test Utilities

The test suite provides several utilities to make writing tests easier:

```lisp
;; Create temporary test files
(create-temp-file content &key (type "lisp"))

;; Verify symbol definitions
(verify-definition tracker symbol type &key package exported-p)

;; Verify symbol references
(verify-reference tracker symbol type &key package file)

;; Create standard test structures
(create-test-files &key system package module)
```

### Test Fixtures

Common test scenarios are available as fixtures:

```lisp
;; Basic tracker setup
(with-fixture clean-tracker ()
  ...)

;; Complete test environment
(with-fixture with-test-files ()
  ...)

;; System-level testing
(with-fixture with-test-system ()
  ...)

;; Package dependency testing
(with-fixture with-package-dependencies ()
  ...)
```

### Best Practices

1. Always use fixtures for setup/teardown to ensure proper cleanup
2. Write descriptive test names and documentation strings
3. Use the provided verification functions instead of direct accessors
4. Test both success and failure cases
5. Clean up any created packages or files in the test
6. Use meaningful sample data from test-data.lisp

## Example Test

Here's an example of a well-structured test:

```lisp
(in-package #:dep/tests)

(test test-package-exports
  "Verify that exported symbols are properly tracked"
  (with-fixture with-test-files ()
    ;; Test exported function
    (is (verify-definition tracker 'test-package::exported-function
                          :function
                          :package "TEST-PACKAGE"
                          :exported-p t)
        "Exported function should be recorded")
    
    ;; Test exported variable
    (is (verify-definition tracker 'test-package::exported-variable
                          :variable
                          :package "TEST-PACKAGE"
                          :exported-p t)
        "Exported variable should be recorded")))
```

## Troubleshooting

Common issues and solutions:

1. **Test file not found**
   - Ensure the test system is loaded
   - Check ASDF system definition
   - Verify file paths are correct

2. **Package conflicts**
   - Use `clear-test-packages` to clean up
   - Ensure test packages don't conflict with real ones
   - Use unique package names for tests

3. **Resource cleanup**
   - Always use `with-fixture` for proper cleanup
   - Use `unwind-protect` for manual resource management
   - Call `dep:clear-tracker` after tests

For more help, check the test utilities documentation or file an issue.