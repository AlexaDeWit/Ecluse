# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
require 'rubygems/package'
require 'json'
require 'zlib'
require 'tempfile'
require 'digest'
require 'stringio'
require 'date'

def specification(spec)
  {name: spec.name, version: spec.version.to_s, platform: spec.platform.to_s,
   dependencies: spec.dependencies.map { |d| {name: d.name, type: d.type.to_s, requirements: d.requirement.requirements.map { |op, v| [op, v.to_s] }} },
   required_ruby_version: spec.required_ruby_version.to_s,
   required_rubygems_version: spec.required_rubygems_version.to_s,
   metadata: spec.metadata, extensions: spec.extensions,
   executables: spec.executables, require_paths: spec.require_paths}
end

bytes = ARGV[1] ? File.binread(ARGV[1]) : STDIN.binmode.read
result = case ARGV.fetch(0)
when 'dependencies'
  Marshal.load(bytes)
when 'encode-dependencies'
  rows = JSON.parse(bytes)
  records = rows.map do |row|
    {name: row.fetch('name'), number: row.fetch('version'), platform: row.fetch('platform'),
     dependencies: row.fetch('dependencies').select { |dependency| dependency.fetch('type') == 'runtime' }.map do |dependency|
       [dependency.fetch('name'), dependency.fetch('requirements').map { |operator, version| "#{operator} #{version}" }.join(', ')]
     end}
  end
  {bytes: Marshal.dump(records).bytes}
when 'compact'
  specification(Marshal.load(Zlib::Inflate.inflate(bytes)))
when 'archive'
  Tempfile.create(['ecluse-reference', '.gem']) do |file|
    file.binmode.write(bytes)
    file.flush
    package = Gem::Package.new(file.path)
    package.verify
    file_sizes = {}
    Gem::Package::TarReader.new(StringIO.new(bytes)) do |outer|
      outer.each do |entry|
        next unless entry.full_name == 'data.tar.gz'
        Zlib::GzipReader.wrap(StringIO.new(entry.read)) do |gzip|
          Gem::Package::TarReader.new(gzip) do |files|
            files.each { |member| file_sizes[member.full_name] = member.header.size if member.file? }
          end
        end
      end
    end
    specification(package.spec).merge(sha256: Digest::SHA256.hexdigest(bytes), files: package.contents.sort, file_sizes: file_sizes)
  end
when 'info'
  require 'bundler'
  require 'bundler/compact_index_client'
  parser = Gem::Resolver::APISet::GemParser.new
  bytes.lines.drop_while { |line| line.chomp != '---' }.drop(1).reject { |line| line.strip.empty? }.map { |line| parser.parse(line) }
else
  abort 'unknown Ruby fixture observation'
end
puts JSON.generate(result)
