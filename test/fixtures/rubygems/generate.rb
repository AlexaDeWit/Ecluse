# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
require 'rubygems/package'
require 'digest'
require 'fileutils'
require 'json'
require 'tmpdir'
require 'zlib'
ENV['SOURCE_DATE_EPOCH'] = '1577836800'
root = File.expand_path(__dir__)
%w[gems quick/Marshal.4.8 info].each { |path| FileUtils.mkdir_p(File.join(root, path)) }
releases = []
coordinates = [
  ['ecluse-fixture-leaf', '1.0.0', 'ruby'], ['ecluse-fixture-leaf', '2.0.0', 'ruby'],
  ['ecluse-fixture-leaf', '2.1.0.pre.1', 'ruby'], ['ecluse-fixture-root', '1.0.0', 'ruby'],
  ['ecluse-fixture-root', '2.0.0', 'ruby'], ['ecluse-fixture-platform', '1.0.0', 'ruby'],
  ['ecluse-fixture-platform', '1.0.0', 'java'], ['ecluse-fixture-platform', '1.0.0', 'x86_64-linux-musl'],
  ['ecluse-fixture-platform', '1.0.0', 'arm64-darwin'], ['ecluse-fixture-native', '1.0.0', 'ruby'],
  ['ecluse-fixture-future', '1.0.0', 'ruby']
]
coordinates.each do |name, version, platform|
  Dir.mktmpdir('ecluse-gem-fixture') do |directory|
    Dir.chdir(directory) do
      feature = name.tr('-', '_')
      FileUtils.mkdir_p('lib')
      value = name == 'ecluse-fixture-root' ? "require 'ecluse_fixture_leaf'\nmodule EcluseFixtureRoot\n  VALUE = EcluseFixtureLeaf::VALUE + '|root-#{version}'\nend\n" : "module #{feature.split('_').map(&:capitalize).join}\n  VALUE = '#{name}-#{version}-#{platform}'\nend\n"
      File.write("lib/#{feature}.rb", value)
      spec = Gem::Specification.new do |s|
        s.name = name
        s.version = version
        s.platform = platform
        s.summary = 'Owned package protocol fixture'
        s.description = 'Synthetic source owned by the Ecluse test suite.'
        s.authors = ['Ecluse contributors']
        s.license = 'MIT'
        s.homepage = 'https://example.invalid/ecluse-fixture'
        s.date = Time.utc(2020, 1, 1)
        s.files = ["lib/#{feature}.rb"]
        s.require_paths = ['lib']
        s.metadata = {'fixture-kind' => 'owned', 'source_code_uri' => 'https://example.invalid/source'}
        s.required_ruby_version = name == 'ecluse-fixture-future' ? '>= 99.0' : '>= 3.2'
        s.required_rubygems_version = '>= 3.4'
        if name == 'ecluse-fixture-root'
          s.add_runtime_dependency('ecluse-fixture-leaf', '>= 1.0', version == '1.0.0' ? '< 2.0' : '< 3.0')
          s.add_development_dependency('ecluse-fixture-dev', '~> 1.0', '>= 1.0.1')
          FileUtils.mkdir_p('exe')
          File.write('exe/ecluse-fixture', "#!/usr/bin/env ruby\nrequire 'ecluse_fixture_root'\nputs EcluseFixtureRoot::VALUE\n")
          File.chmod(0755, 'exe/ecluse-fixture')
          s.bindir = 'exe'
          s.executables = ['ecluse-fixture']
          s.files << 'exe/ecluse-fixture'
        end
        if name == 'ecluse-fixture-native'
          FileUtils.mkdir_p('ext/ecluse_fixture_native')
          File.write('ext/ecluse_fixture_native/extconf.rb', "require 'mkmf'\ncreate_makefile('ecluse_fixture_native')\n")
          File.write('ext/ecluse_fixture_native/ecluse_fixture_native.c', "#include <ruby.h>\nvoid Init_ecluse_fixture_native(void) {}\n")
          s.extensions = ['ext/ecluse_fixture_native/extconf.rb']
          s.files.concat(Dir['ext/**/*'].select { |path| File.file?(path) })
        end
      end
      archive = Gem::Package.build(spec, false, false)
      bytes = File.binread(archive)
      compact = Marshal.dump(spec)
      compact_path = "quick/Marshal.4.8/#{spec.full_name}.gemspec.rz"
      File.binwrite(File.join(root, compact_path), Zlib::Deflate.deflate(compact))
      File.binwrite(File.join(root, 'gems', archive), bytes)
      decoded = Marshal.load(compact)
      releases << {
        name: spec.name, version: spec.version.to_s, platform: spec.platform.to_s,
        filename: archive, sha256: Digest::SHA256.hexdigest(bytes), bytes: bytes.bytesize,
        compact: compact_path, compact_sha256: Digest::SHA256.file(File.join(root, compact_path)).hexdigest,
        created_at: platform == 'ruby' ? '2020-01-01T00:00:00Z' : '2020-01-03T00:00:00Z',
        dependencies: spec.dependencies.map { |d| {name: d.name, type: d.type, requirements: d.requirement.requirements.map { |op, v| [op, v.to_s] }} },
        required_ruby_version: spec.required_ruby_version.to_s,
        required_rubygems_version: spec.required_rubygems_version.to_s,
        extensions: spec.extensions, metadata: spec.metadata,
        decoded_compact: {class: decoded.class.name, name: decoded.name, version: decoded.version.to_s,
                          platform: decoded.platform.to_s, extensions: decoded.extensions,
                          dependencies: decoded.dependencies.map { |d| [d.name, d.requirement.to_s, d.type.to_s] }}
      }
    end
  end
end
releases.group_by { |row| row[:name] }.each do |name, rows|
  lines = rows.map do |row|
    key = row[:version] + (row[:platform] == 'ruby' ? '' : "-#{row[:platform]}")
    deps = row[:dependencies].select { |dep| dep[:type] == :runtime }.map do |dep|
      dep[:name] + ':' + dep[:requirements].map { |op, version| "#{op} #{version}" }.join('&')
    end.join(',')
    "#{key} #{deps}|checksum:#{row[:sha256]},ruby:#{row[:required_ruby_version]},rubygems:#{row[:required_rubygems_version]},created_at:#{row[:created_at]}"
  end
  File.write(File.join(root, 'info', name), "---\n" + lines.join("\n") + "\n")
end
dependencies = releases.map do |row|
  {name: row[:name], number: row[:version], platform: row[:platform], dependencies: row[:dependencies].select { |d| d[:type] == :runtime }.map { |d| [d[:name], d[:requirements].map { |op, v| "#{op} #{v}" }.join(', ')] }}
end
File.binwrite(File.join(root, 'dependencies.marshal'), Marshal.dump(dependencies))
File.write(File.join(root, 'manifest.json'), JSON.pretty_generate({ruby: RUBY_VERSION, rubygems: Gem::VERSION, source_date_epoch: ENV['SOURCE_DATE_EPOCH'], releases: releases}) + "\n")
