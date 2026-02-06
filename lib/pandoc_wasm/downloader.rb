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

    # Download pandoc.wasm from the latest GitHub Release.
    #
    # @param to [String] absolute path where the binary will be written
    # @return [true] on success
    # @raise [StandardError] on failure
    def self.download(to:)
      target_path = File.expand_path(to)
      tag = get_latest_release_tag
      download_asset(tag, target_path)
      true
    rescue StandardError => e
      warn "Error downloading pandoc.wasm: #{e.message}"
      warn "\nYou can:"
      warn '1. Build it yourself following the instructions in README.md'
      warn '2. Manually download it from a GitHub release'
      warn '3. Copy it from the build directory after compilation'
      raise
    end

    class << self
      private

      # Get the latest release tag from GitHub API
      def get_latest_release_tag
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
          version = PandocWasm::VERSION
          puts "No GitHub release found. Using version #{version} from gem."
          "v#{version}"
        else
          raise "GitHub API returned status #{response.code}: #{response.body}"
        end
      end

      # Download the asset from a GitHub Release to the given target path
      def download_asset(tag, target_path)
        uri = URI("https://api.github.com/repos/#{REPO_OWNER}/#{REPO_NAME}/releases/tags/#{tag}")

        http = Net::HTTP.new(uri.host, uri.port)
        http.use_ssl = true
        http.read_timeout = 30

        request = Net::HTTP::Get.new(uri)
        request['User-Agent'] = 'pandoc-wasm-ruby-downloader'
        request['Accept'] = 'application/vnd.github.v3+json'

        response = http.request(request)

        case response.code
        when '200'
          # continue
        when '404'
          raise "Release #{tag} not found on GitHub."
        else
          raise "GitHub API returned status #{response.code}: #{response.body}"
        end

        release = JSON.parse(response.body)
        asset = release['assets'].find { |a| a['name'] == ASSET_NAME }

        unless asset
          raise "Asset #{ASSET_NAME} not found in release #{tag}."
        end

        puts "Downloading #{ASSET_NAME} from release #{tag}..."
        puts "Size: #{(asset['size'] / 1024.0 / 1024.0).round(2)} MB"

        download_uri = URI(asset['browser_download_url'])
        download_http = Net::HTTP.new(download_uri.host, download_uri.port)
        download_http.use_ssl = true
        download_http.read_timeout = 300 # 5 minutes for large file

        download_request = Net::HTTP::Get.new(download_uri)
        download_request['User-Agent'] = 'pandoc-wasm-ruby-downloader'
        download_request['Accept'] = 'application/octet-stream'

        FileUtils.mkdir_p(File.dirname(target_path))

        File.open(target_path, 'wb') do |file|
          download_http.request(download_request) do |dl_response|
            case dl_response.code
            when '200'
              dl_response.read_body do |chunk|
                file.write(chunk)
              end
            else
              FileUtils.rm_f(target_path)
              raise "Failed to download asset: HTTP #{dl_response.code}"
            end
          end
        end

        File.chmod(0o755, target_path)
        puts "Successfully downloaded #{ASSET_NAME} to #{target_path}"
      end
    end
  end
end
