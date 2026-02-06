# frozen_string_literal: true

require 'open3'

module PandocWasm
  class Runner
    # Run pandoc via the WASI runtime.
    #
    # @param input [String] path to the input file
    # @param output [String] path to the output file
    # @param wasm_dir [String] directory to expose to the WASI sandbox (default: ".")
    # @param extra_args [Array<String>] additional pandoc CLI arguments
    # @return [Hash] { stdout: String, stderr: String, success: Boolean }
    # @raise [PandocWasm::BinaryNotFound] if the binary does not exist
    # @raise [PandocWasm::ExecutionError] on non-zero exit code
    def self.run(input, output, wasm_dir: '.', extra_args: [])
      binary = PandocWasm.binary_path

      unless File.exist?(binary)
        raise PandocWasm::BinaryNotFound,
              "pandoc.wasm not found at #{binary}. " \
              'Run PandocWasm.download_to_binary_path! to download it.'
      end

      cmd = [
        PandocWasm.runtime,
        'run',
        '--dir', wasm_dir,
        binary,
        '-o', output,
        *extra_args,
        input
      ]

      stdout, stderr, status = Open3.capture3(*cmd)

      unless status.success?
        raise PandocWasm::ExecutionError,
              "pandoc exited with status #{status.exitstatus}: #{stderr}"
      end

      { stdout: stdout, stderr: stderr, success: true }
    end
  end
end
