# frozen_string_literal: true

require_relative 'test_helper'

class PandocWasmTest < Minitest::Test
  include PandocWasmTestHelper

  # -- binary_path --

  def test_binary_path_default
    PandocWasm.binary_path = nil
    expected = File.join(File.dirname(__FILE__), '..', 'lib', 'pandoc_wasm', 'pandoc.wasm')
    assert_equal File.expand_path(expected), File.expand_path(PandocWasm.binary_path)
  end

  def test_binary_path_setter
    PandocWasm.binary_path = '/tmp/custom/pandoc.wasm'
    assert_equal '/tmp/custom/pandoc.wasm', PandocWasm.binary_path
  end

  # -- runtime --

  def test_runtime_default
    PandocWasm.runtime = nil
    assert_equal 'wasmtime', PandocWasm.runtime
  end

  def test_runtime_setter
    PandocWasm.runtime = 'wasmer'
    assert_equal 'wasmer', PandocWasm.runtime
  end

  # -- available? --

  def test_available_returns_false_when_missing
    PandocWasm.binary_path = '/tmp/nonexistent_pandoc_wasm_test.wasm'
    refute PandocWasm.available?
  end

  def test_available_returns_true_when_present
    Dir.mktmpdir do |dir|
      path = File.join(dir, 'pandoc.wasm')
      File.write(path, 'fake')
      PandocWasm.binary_path = path
      assert PandocWasm.available?
    end
  end

  # -- error classes --

  def test_error_hierarchy
    assert PandocWasm::BinaryNotFound < PandocWasm::Error
    assert PandocWasm::ExecutionError < PandocWasm::Error
    assert PandocWasm::Error < StandardError
  end

  # -- version --

  def test_version_defined
    refute_nil PandocWasm::VERSION
    assert_match(/\A\d+\.\d+\.\d+\z/, PandocWasm::VERSION)
  end

  # -- download_to_binary_path! delegates --

  def test_download_to_binary_path_delegates_to_downloader
    called_with = nil
    PandocWasm.binary_path = '/tmp/test_download_target.wasm'

    PandocWasm::Downloader.stub(:download, ->(to:) { called_with = to; true }) do
      result = PandocWasm.download_to_binary_path!
      assert_equal true, result
      assert_equal '/tmp/test_download_target.wasm', called_with
    end
  end

  # -- run delegates --

  def test_run_delegates_to_runner
    called_args = nil

    fake_run = lambda do |input, output, wasm_dir:, extra_args:|
      called_args = { input: input, output: output, wasm_dir: wasm_dir, extra_args: extra_args }
      { stdout: '', stderr: '', success: true }
    end

    PandocWasm::Runner.stub(:run, fake_run) do
      result = PandocWasm.run('in.md', 'out.pptx', wasm_dir: '/data', extra_args: ['--slide-level=2'])
      assert_equal true, result[:success]
      assert_equal 'in.md', called_args[:input]
      assert_equal 'out.pptx', called_args[:output]
      assert_equal '/data', called_args[:wasm_dir]
      assert_equal ['--slide-level=2'], called_args[:extra_args]
    end
  end
end
