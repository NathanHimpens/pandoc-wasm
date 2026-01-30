# frozen_string_literal: true

require_relative 'pandoc_wasm/version'
require_relative 'pandoc_wasm/downloader'

module PandocWasm
  class Error < StandardError; end

  # Get the path to the pandoc.wasm binary
  #
  # @return [String] The absolute path to pandoc.wasm
  def self.path
    wasm_path = File.join(File.dirname(__FILE__), 'pandoc_wasm', 'pandoc.wasm')
    
    # If the file doesn't exist, try to download it
    unless File.exist?(wasm_path)
      Downloader.download_if_needed
    end
    
    wasm_path
  end

  # Check if pandoc.wasm is available
  #
  # @return [Boolean] true if pandoc.wasm exists
  def self.available?
    File.exist?(path)
  end

  # Get the absolute path to pandoc.wasm
  #
  # @return [String] The absolute path to pandoc.wasm
  def self.absolute_path
    File.expand_path(path)
  end
end
