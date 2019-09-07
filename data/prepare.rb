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
new_env = {}

# IO ()
paths.each do |path|
  id = path.scan(/SC\d{3,}/).join
  body = File.read(path) + notice
  new_env[id] = {
    'remediation_points' => 50_000,
    'content' => {
      'body' => body
    }
  }
end

# IO ()
Dir.chdir('..')

# IO String
old_env = YAML.load(File.read('env.yml'))

# IO ()
old_env.each do |key, val|
  if (new_val = new_env[key])
    new_val['remediation_points'] = val['remediation_points']
  end
end

# String
yaml = new_env.to_yaml

# IO ()
File.open('env.yml', 'w') do |file|
  file.write(yaml)
end
