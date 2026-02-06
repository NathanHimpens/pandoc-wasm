# frozen_string_literal: true

require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'

require_relative '../lib/pandoc_wasm'

# Reset PandocWasm module state between tests
module PandocWasmTestHelper
  def setup
    @original_binary_path = PandocWasm.instance_variable_get(:@binary_path)
    @original_runtime = PandocWasm.instance_variable_get(:@runtime)
  end

  def teardown
    PandocWasm.instance_variable_set(:@binary_path, @original_binary_path)
    PandocWasm.instance_variable_set(:@runtime, @original_runtime)
  end
end
