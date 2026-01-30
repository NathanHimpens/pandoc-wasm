# frozen_string_literal: true

require 'bundler/gem_tasks'
require 'rake/testtask'

task default: :test

Rake::TestTask.new(:test) do |t|
  t.libs << 'test'
  t.pattern = 'test/**/*_test.rb'
  t.verbose = true
end

desc 'Build the gem'
task :build do
  system 'gem build pandoc_wasm.gemspec'
end

desc 'Install the gem locally'
task :install => :build do
  system 'gem install ./pandoc_wasm-*.gem'
end

desc 'Publish the gem to RubyGems'
task :release => :build do
  system 'gem push pandoc_wasm-*.gem'
end
