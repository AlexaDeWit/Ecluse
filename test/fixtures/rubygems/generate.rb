# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
require 'rubygems/package'
require 'digest'
require 'fileutils'
require 'json'
require 'tmpdir'
require 'zlib'
require 'optparse'
require 'date'
ENV['SOURCE_DATE_EPOCH'] = '1577836800'
options = {output: File.expand_path(__dir__), payload_bytes: nil, state_versions: nil, metadata_bytes: 0}
OptionParser.new do |parser|
  parser.on('--coordinate NAME,VERSION,PLATFORM', Array) { |value| options[:coordinate] = value }
  parser.on('--output DIRECTORY') { |value| options[:output] = File.expand_path(value) }
  parser.on('--payload-bytes INTEGER', Integer) { |value| options[:payload_bytes] = value }
  parser.on('--state-versions INTEGER', Integer) { |value| options[:state_versions] = value }
  parser.on('--metadata-bytes INTEGER', Integer) { |value| options[:metadata_bytes] = value }
end.parse!
abort 'payload bytes must be positive' if options[:payload_bytes] && options[:payload_bytes] <= 0
stress = !options[:state_versions].nil?
selected = options[:coordinate]
abort 'coordinate requires exactly a name, version and platform' if selected && (selected.length != 3 || selected.any?(&:empty?))
abort 'coordinate selection cannot combine with state stress' if selected && stress
abort 'coordinate selection requires a separate output directory' if selected && options[:output] == File.expand_path(__dir__)
abort 'state versions must be between 1 and 256' if stress && !(1..256).cover?(options[:state_versions])
abort 'metadata bytes must be between 0 and 2097152' unless (0..2_097_152).cover?(options[:metadata_bytes])
abort 'metadata bytes require state versions' if options[:metadata_bytes] > 0 && !stress
abort 'synthetic stress requires a separate output directory' if stress && options[:output] == File.expand_path(__dir__)
abort 'aggregate specification descriptions exceed 16 MiB' if stress && options[:state_versions] * 4 * options[:metadata_bytes] > 16_777_216
root = options[:output]
%w[gems quick/Marshal.4.8 info].each { |path| FileUtils.mkdir_p(File.join(root, path)) }
releases = []
coordinates = [
  ['ecluse-fixture-leaf', '1.0.0', 'ruby'], ['ecluse-fixture-leaf', '2.0.0', 'ruby'],
  ['ecluse-fixture-leaf', '2.1.0.pre.1', 'ruby'], ['ecluse-fixture-root', '1.0.0', 'ruby'],
  ['ecluse-fixture-root', '2.0.0', 'ruby'], ['ecluse-fixture-platform', '1.0.0', 'ruby'],
  ['ecluse-fixture-platform', '1.0.0', 'java'], ['ecluse-fixture-platform', '1.0.0', 'x86_64-linux-musl'],
  ['ecluse-fixture-platform', '1.0.0', 'arm64-darwin'], ['ecluse-fixture-native', '1.0.0', 'ruby'],
  ['ecluse-fixture-future', '1.0.0', 'ruby'],
  ['ecluse-fixture-advisory', '1.18.0', 'ruby'], ['ecluse-fixture-advisory', '1.19.0', 'ruby'],
  ['ecluse-fixture-root', '3.0.0', 'ruby'],
  ['ecluse-fixture-firstparty', '1.0.0', 'ruby'], ['ecluse-fixture-firstparty', '2.0.0', 'ruby'],
  ['ecluse-fixture-readmitted', '1.0.1.pre.readmitted', 'ruby'],
  ['ecluse-fixture-missing', '1.0.0', 'ruby'], ['ecluse-fixture-late', '1.0.0', 'ruby'],
  ['ecluse-fixture-sentinel', '1.0.0', 'ruby']
]
if stress
  coordinates = (0...options[:state_versions]).flat_map do |index|
    %w[ruby java x86_64-linux-musl arm64-darwin].map { |platform| ['ecluse-fixture-platform', "1.0.#{index}", platform] }
  end
end
coordinates = [selected] if selected
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
        s.description = if options[:metadata_bytes] > 0
          Random.new(1886).bytes((options[:metadata_bytes] + 1) / 2).unpack1('H*').slice(0, options[:metadata_bytes])
        else
          'Synthetic source owned by the Ecluse test suite.'
        end
        s.authors = ['Ecluse contributors']
        s.license = 'MIT'
        s.homepage = 'https://example.invalid/ecluse-fixture'
        s.date = Time.utc(2020, 1, 1)
        s.files = ["lib/#{feature}.rb"]
        s.require_paths = ['lib']
        if options[:payload_bytes] && name == 'ecluse-fixture-platform'
          File.binwrite('lib/payload.bin', Random.new(1886).bytes(options[:payload_bytes]))
          s.files << 'lib/payload.bin'
        end
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
      if stress || selected
        package = Gem::Package.new(File.join(root, 'gems', archive))
        abort 'native archive verification failed' unless package.verify
        abort 'native compact description mismatch' unless decoded.description == spec.description
        abort 'native archive description mismatch' unless package.spec.description == spec.description
        abort 'native compact identity mismatch' unless [decoded.name, decoded.version, decoded.platform] == [spec.name, spec.version, spec.platform]
        abort 'native archive identity mismatch' unless [package.spec.name, package.spec.version, package.spec.platform] == [spec.name, spec.version, spec.platform]
      end
      releases << {
        name: spec.name, version: spec.version.to_s, platform: spec.platform.to_s,
        filename: archive, sha256: Digest::SHA256.hexdigest(bytes), bytes: bytes.bytesize,
        compact: compact_path, compact_sha256: Digest::SHA256.file(File.join(root, compact_path)).hexdigest,
        created_at: platform == 'ruby' ? '2020-01-01T00:00:00Z' : '2020-01-03T00:00:00Z',
        dependencies: spec.dependencies.map { |d| {name: d.name, type: d.type, requirements: d.requirement.requirements.map { |op, v| [op, v.to_s] }} },
        required_ruby_version: spec.required_ruby_version.to_s,
        required_rubygems_version: spec.required_rubygems_version.to_s,
        extensions: spec.extensions, metadata: spec.metadata,
        payload_bytes: options[:payload_bytes] && name == 'ecluse-fixture-platform' ? options[:payload_bytes] : nil,
        decoded_compact: {class: decoded.class.name, name: decoded.name, version: decoded.version.to_s,
                          platform: decoded.platform.to_s, extensions: decoded.extensions,
                          dependencies: decoded.dependencies.map { |d| [d.name, d.requirement.to_s, d.type.to_s] }}
      }.reject { |key, value| key == :payload_bytes && value.nil? }
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

if selected
  File.write(File.join(root, 'manifest.json'), JSON.pretty_generate({ruby: RUBY_VERSION, rubygems: Gem::VERSION, source_date_epoch: ENV['SOURCE_DATE_EPOCH'], selected_coordinate: selected, releases: releases}) + "\n")
  exit
end

if stress
  File.write(File.join(root, 'manifest.json'), JSON.pretty_generate({ruby: RUBY_VERSION, rubygems: Gem::VERSION, source_date_epoch: ENV['SOURCE_DATE_EPOCH'], synthetic_stress: {kind: 'owned-generated-state-stress', versions: options[:state_versions], platforms: 4, description_bytes: options[:metadata_bytes], encoding: 'deterministic-pseudorandom-hex'}, releases: releases}) + "\n")
  exit
end

legacy_directory = File.join(root, 'legacy')
FileUtils.mkdir_p(legacy_directory)
original = Marshal.load(Zlib::Inflate.inflate(File.binread(File.join(root, 'quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz'))))
fields = Marshal.load(original._dump(-1))
original_dump = Gem::Specification.instance_method(:_dump)
legacy = []
capture_legacy = lambda do |label, native_fields|
  nested = Marshal.dump(native_fields)
  begin
    Gem::Specification.define_method(:_dump) { |_level| nested }
    bytes = Marshal.dump(original)
  ensure
    Gem::Specification.define_method(:_dump, original_dump)
  end
  decoded = Marshal.load(bytes)
  path = "legacy/#{label}.gemspec.rz"
  compressed = Zlib::Deflate.deflate(bytes)
  File.binwrite(File.join(root, path), compressed)
  legacy << {path: path, specification_version: native_fields[1], field_count: native_fields.length,
             sha256: Digest::SHA256.hexdigest(compressed), decoded_name: decoded.name,
             decoded_version: decoded.version.to_s, decoded_platform: decoded.platform.to_s,
             decoded_licenses: decoded.licenses, decoded_metadata: decoded.metadata,
             dependencies: decoded.dependencies.map { |dependency| [dependency.name, dependency.type.to_s, dependency.requirement.to_s] }}
end
(Gem::Specification::MARSHAL_FIELDS.to_a + [[4, 19]]).each do |version, count|
  native_fields = fields.first(count)
  native_fields[1] = version
  capture_legacy.call("spec-v#{version}-fields#{count}", native_fields)
end
date_fields = fields.dup
date_fields[4] = Date.new(2020, 1, 1)
capture_legacy.call('spec-date', date_fields)
rational_date_fields = fields.dup
rational_date_fields[4] = Date.jd(Rational(17_150_001, 7))
rejected = []
begin
  capture_legacy.call('spec-rational-date', rational_date_fields)
rescue TypeError => error
  rejected << {input: 'Date.jd(Rational(17150001, 7))', class: error.class.name, message: error.message}
end
File.write(File.join(root, 'legacy-rejections.json'), JSON.pretty_generate({ruby: RUBY_VERSION, rubygems: Gem::VERSION, observations: rejected}) + "\n")
fractional_fields = fields.dup
fractional_fields[4] = Time.at(Rational(4_733_510_401, 3)).utc
capture_legacy.call('spec-fractional-time', fractional_fields)
legacy_dependencies = fields.dup
legacy_dependencies[9] = Marshal.load(Marshal.dump(fields[9])).map do |dependency|
  dependency.remove_instance_variable(:@requirement)
  dependency
end
capture_legacy.call('spec-version-requirements', legacy_dependencies)
File.write(File.join(root, 'legacy.json'), JSON.pretty_generate({ruby: RUBY_VERSION, rubygems: Gem::VERSION, layouts: legacy}) + "\n")
require 'psych'
shared = ['owned', 'native']
alias_yaml = Psych.dump({'first' => shared, 'second' => shared})
File.write(File.join(root, 'aliases.yaml'), alias_yaml)
File.write(File.join(root, 'aliases.json'), JSON.pretty_generate(Psych.safe_load(alias_yaml, aliases: true)) + "\n")

File.write(File.join(root, 'manifest.json'), JSON.pretty_generate({ruby: RUBY_VERSION, rubygems: Gem::VERSION, source_date_epoch: ENV['SOURCE_DATE_EPOCH'], releases: releases}) + "\n")
