# frozen_string_literal: true

require_relative 'lib/pandoc_wasm/version'

Gem::Specification.new do |spec|
  spec.name          = 'pandoc_wasm'
  spec.version       = PandocWasm::VERSION
  spec.authors       = ['Nathan Himpens']
  spec.email         = ['']

  spec.summary       = 'Pandoc 3.8.3 compiled to WebAssembly for document conversion in WASI environments'
  spec.description   = 'Pandoc 3.8.3 compiled to WebAssembly for document conversion in WASI environments'
  spec.homepage      = 'https://github.com/NathanHimpens/pandoc-wasm'
  spec.license       = 'GPL-2.0-or-later'

  spec.metadata['homepage_uri'] = spec.homepage
  spec.metadata['source_code_uri'] = 'https://github.com/NathanHimpens/pandoc-wasm'
  spec.metadata['changelog_uri'] = 'https://github.com/NathanHimpens/pandoc-wasm/blob/main/README.md'

  # Specify which files should be added to the gem when it is released.
  spec.files = Dir.chdir(__dir__) do
    `git ls-files -z`.split("\x0").reject do |f|
      (File.expand_path(f) == __FILE__) ||
        f.start_with?(*%w[bin/ test/ spec/ features/ .git .github .cursor .ralph dist-newstyle/ patches/ tests/])
    end
  end

  spec.bindir        = 'exe'
  spec.executables   = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']

  # Ruby standard library dependencies (no external gems needed)
  spec.required_ruby_version = '>= 2.7.0'

  # Post-install message
  spec.post_install_message = <<~MSG
    pandoc_wasm installed successfully!
    
    Note: pandoc.wasm will be automatically downloaded from GitHub Releases
    the first time you call PandocWasm.path in your code.
    
    If no GitHub release exists yet, you'll need to:
    1. Build pandoc.wasm yourself (see README.md)
    2. Create a GitHub release with pandoc.wasm attached
    3. Or manually copy pandoc.wasm to the gem directory
  MSG
end
