# frozen_string_literal: true

require_relative 'test_helper'

class DownloaderTest < Minitest::Test
  include PandocWasmTestHelper

  def test_constants_defined
    assert_equal 'NathanHimpens', PandocWasm::Downloader::REPO_OWNER
    assert_equal 'pandoc-wasm', PandocWasm::Downloader::REPO_NAME
    assert_equal 'pandoc.wasm', PandocWasm::Downloader::ASSET_NAME
  end

  def test_download_accepts_to_keyword
    # Verify the method signature accepts to: keyword
    assert PandocWasm::Downloader.method(:download).parameters.any? { |type, name| name == :to }
  end

  def test_download_raises_on_network_error
    # Stub the private method to simulate a network failure
    PandocWasm::Downloader.stub(:get_latest_release_tag, -> { raise 'Network error' }) do
      assert_raises(RuntimeError) do
        PandocWasm::Downloader.download(to: '/tmp/test_pandoc.wasm')
      end
    end
  end

  def test_download_expands_target_path
    # Stub both private methods to verify the path gets expanded
    downloaded_to = nil

    PandocWasm::Downloader.stub(:get_latest_release_tag, -> { 'v1.0.0' }) do
      PandocWasm::Downloader.stub(:download_asset, ->(tag, path) { downloaded_to = path }) do
        PandocWasm::Downloader.download(to: '~/test_pandoc.wasm')
      end
    end

    assert_equal File.expand_path('~/test_pandoc.wasm'), downloaded_to
  end

  def test_download_returns_true_on_success
    PandocWasm::Downloader.stub(:get_latest_release_tag, -> { 'v1.0.0' }) do
      PandocWasm::Downloader.stub(:download_asset, ->(_tag, _path) { nil }) do
        result = PandocWasm::Downloader.download(to: '/tmp/test_pandoc.wasm')
        assert_equal true, result
      end
    end
  end
end
