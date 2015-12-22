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

# { String => { String => Integer, String => { String => String } } }
new_mapping = {}

# IO ()
paths.each do |path|
  id = path.scan(/SC\d{3,}/).join
  body = File.read(path) + notice
  new_mapping[id] = {
    'remediation_points' => 50_000,
    'content' => {
      'body' => body
    }
  }
end

# IO ()
Dir.chdir('..')

# IO String
old_mapping = YAML.load(File.read('mapping.yml'))

# IO ()
old_mapping.each do |key, val|
  new_mapping[key]['remediation_points'] = val['remediation_points']
end

# String
yaml = new_mapping.to_yaml

# IO ()
File.open('mapping.yml', 'w') do |file|
  file.write(yaml)
end
