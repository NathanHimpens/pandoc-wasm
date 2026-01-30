# frozen_string_literal: true

require 'net/http'
require 'json'
require 'fileutils'
require 'uri'

module PandocWasm
  class Downloader
    REPO_OWNER = 'NathanHimpens'
    REPO_NAME = 'pandoc-wasm'
    ASSET_NAME = 'pandoc.wasm'
    
    def self.wasm_path
      File.join(File.dirname(__FILE__), ASSET_NAME)
    end

    # Download pandoc.wasm from GitHub Releases if it doesn't exist
    def self.download_if_needed
      return if File.exist?(wasm_path)
      
      puts "pandoc.wasm not found. Attempting to download from GitHub Releases..."
      download
    end

    # Download pandoc.wasm from the latest GitHub release
    def self.download
      begin
        tag = get_latest_release_tag
        download_asset(tag)
      rescue StandardError => e
        warn "Error downloading pandoc.wasm: #{e.message}"
        warn "\nYou can:"
        warn "1. Build it yourself following the instructions in README.md"
        warn "2. Manually download it from a GitHub release"
        warn "3. Copy it from the build directory after compilation"
        warn "\n⚠️  Installation will continue, but pandoc.wasm must be added manually."
        false
      end
    end

    private

    # Get the latest release tag from GitHub API
    def self.get_latest_release_tag
      uri = URI("https://api.github.com/repos/#{REPO_OWNER}/#{REPO_NAME}/releases/latest")
      
      http = Net::HTTP.new(uri.host, uri.port)
      http.use_ssl = true
      http.read_timeout = 30
      
      request = Net::HTTP::Get.new(uri)
      request['User-Agent'] = 'pandoc-wasm-ruby-downloader'
      request['Accept'] = 'application/vnd.github.v3+json'
      
      response = http.request(request)
      
      case response.code
      when '200'
        release = JSON.parse(response.body)
        release['tag_name']
      when '404'
        # No releases yet, try to use version from gem
        version = PandocWasm::VERSION
        puts "No GitHub release found. Using version #{version} from gem."
        "v#{version}"
      else
        raise "GitHub API returned status #{response.code}: #{response.body}"
      end
    end

    # Download the asset from GitHub Releases
    def self.download_asset(tag)
      # Get release info to find asset URL
      uri = URI("https://api.github.com/repos/#{REPO_OWNER}/#{REPO_NAME}/releases/tags/#{tag}")
      
      http = Net::HTTP.new(uri.host, uri.port)
      http.use_ssl = true
      http.read_timeout = 30
      
      request = Net::HTTP::Get.new(uri)
      request['User-Agent'] = 'pandoc-wasm-ruby-downloader'
      request['Accept'] = 'application/vnd.github.v3+json'
      
      response = http.request(request)
      
      case response.code
      when '404'
        puts "Release #{tag} not found on GitHub. Skipping download."
        puts 'You can manually download pandoc.wasm from the repository or build it yourself.'
        return false
      when '200'
        # Continue
      else
        raise "GitHub API returned status #{response.code}: #{response.body}"
      end
      
      release = JSON.parse(response.body)
      asset = release['assets'].find { |a| a['name'] == ASSET_NAME }
      
      unless asset
        puts "Asset #{ASSET_NAME} not found in release #{tag}."
        puts 'You can manually download pandoc.wasm from the repository or build it yourself.'
        return false
      end
      
      # Download the asset
      puts "Downloading #{ASSET_NAME} from release #{tag}..."
      puts "Size: #{(asset['size'] / 1024.0 / 1024.0).round(2)} MB"
      
      download_uri = URI(asset['browser_download_url'])
      download_http = Net::HTTP.new(download_uri.host, download_uri.port)
      download_http.use_ssl = true
      download_http.read_timeout = 300 # 5 minutes for large file
      
      download_request = Net::HTTP::Get.new(download_uri)
      download_request['User-Agent'] = 'pandoc-wasm-ruby-downloader'
      download_request['Accept'] = 'application/octet-stream'
      
      FileUtils.mkdir_p(File.dirname(wasm_path))
      
      File.open(wasm_path, 'wb') do |file|
        download_http.request(download_request) do |response|
          case response.code
          when '200'
            response.read_body do |chunk|
              file.write(chunk)
            end
          else
            FileUtils.rm_f(wasm_path)
            raise "Failed to download asset: #{response.code}"
          end
        end
      end
      
      # Make executable
      File.chmod(0o755, wasm_path)
      puts "✓ Successfully downloaded #{ASSET_NAME}"
      true
    end
  end
end
