# frozen_string_literal: true

require_relative 'pandoc_wasm/version'
require_relative 'pandoc_wasm/downloader'
require_relative 'pandoc_wasm/runner'

module PandocWasm
  class Error < StandardError; end
  class BinaryNotFound < Error; end
  class ExecutionError < Error; end

  DEFAULT_BINARY_PATH = File.join(File.dirname(__FILE__), 'pandoc_wasm', 'pandoc.wasm').freeze

  class << self
    attr_writer :binary_path, :runtime

    # Get the path to the pandoc.wasm binary.
    # Defaults to lib/pandoc_wasm/pandoc.wasm inside the installed gem.
    #
    # @return [String]
    def binary_path
      @binary_path || DEFAULT_BINARY_PATH
    end

    # Get the WASI runtime executable name.
    # Defaults to "wasmtime".
    #
    # @return [String]
    def runtime
      @runtime || 'wasmtime'
    end

    # Download the .wasm binary from the latest GitHub Release to binary_path.
    # Creates intermediate directories if needed.
    #
    # @return [true] on success
    # @raise [StandardError] on failure
    def download_to_binary_path!
      Downloader.download(to: binary_path)
    end

    # Run pandoc via the WASI runtime.
    #
    # Translates to:
    #   <runtime> run --dir <wasm_dir> <binary_path> -o <output> [extra_args...] <input>
    #
    # @param input [String] path to the input file
    # @param output [String] path to the output file
    # @param wasm_dir [String] directory to expose to the WASI sandbox (default: ".")
    # @param extra_args [Array<String>] additional pandoc CLI arguments
    # @return [Hash] { stdout: String, stderr: String, success: Boolean }
    # @raise [BinaryNotFound] if binary_path does not exist
    # @raise [ExecutionError] on non-zero exit code
    def run(input, output, wasm_dir: '.', extra_args: [])
      Runner.run(input, output, wasm_dir: wasm_dir, extra_args: extra_args)
    end

    # Check if the .wasm binary exists at binary_path.
    #
    # @return [Boolean]
    def available?
      File.exist?(binary_path)
    end
  end
end
