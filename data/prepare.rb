#!/usr/bin/env ruby

require 'yaml'

# String
notice = %(

### Notice

Original content from the ShellCheck https://github.com/koalaman/shellcheck/wiki.
)

# IO ()
Dir.chdir('data/wiki')

# [String]
paths = Dir.glob('*.md').select { |path| path =~ /SC\d{3,}.md/ }

# { String => { remediation_points: Integer, content: { body: String } } }
mapping = {}

# IO ()
paths.each do |path|
  id = path.scan(/SC\d{3,}/).join
  body = File.read(path) + notice
  mapping[id] = { "remediation_points" => 50_000, "content" => { "body" => body } }
end

# IO ()
Dir.chdir('..')

# String
yaml = mapping.to_yaml

# IO ()
File.open('mapping.yml', 'w') do |file|
  file.write(yaml)
end
