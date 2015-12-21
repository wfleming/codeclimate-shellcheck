#!/usr/bin/env ruby

require 'json'

# String
notice = %(

### Notice

Original content from the ShellCheck https://github.com/koalaman/shellcheck/wiki.
)

# IO ()
Dir.chdir('data/wiki')

# [String]
paths = Dir.glob('*.md').select { |path| path =~ /SC\d{3,}.md/ }

# { String => { body: String } }
mapping = {}

# IO ()
paths.each do |path|
  key = path.scan(/SC\d{3,}/).join
  contents = File.read(path) + notice
  mapping[key] = { remediation_points: 50_000, content: { body: contents } }
end

# IO ()
Dir.chdir('..')

# String
json = JSON.pretty_generate(mapping)

# IO ()
File.open('mapping.json', 'w') do |file|
  file.write(json)
end
