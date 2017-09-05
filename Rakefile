require 'bundler/setup'

require 'albacore'
require 'albacore/tasks/release'
require 'albacore/tasks/versionizer'
require 'albacore/ext/teamcity'

Configuration = ENV['CONFIGURATION'] || 'Release'

Albacore::Tasks::Versionizer.new :versioning

desc 'create assembly infos'
asmver_files :assembly_info do |a|
  a.files = FileList['**/*fsproj'] # optional, will find all projects recursively by default

  a.attributes assembly_description: 'i18n for Suave',
               assembly_configuration: Configuration,
               assembly_copyright: "(c) 2015 by Henrik FEldt",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']

  a.handle_config do |proj, conf|
    conf.namespace = "#{conf.namespace}AsmVer"
    conf
  end
end

task :yolo do
  system %{ruby -pi.bak -e "gsub(/module YoLo/, 'module internal Suave.Locale.YoLo')" paket-files/haf/YoLo/YoLo.fs} unless Albacore.windows?
end

desc 'Perform fast build (warn: doesn\'t d/l deps)'
build :quick_compile do |b|
  b.prop 'Configuration', Configuration
  b.logging = 'detailed'
  b.sln     = 'src/Suave.Locale.sln'
end

task :paket_bootstrap do
system 'tools/paket.bootstrapper.exe', clr_command: true unless File.exists? 'tools/paket.exe'
end

desc 'restore all nugets as per the packages.config files'
task :restore => :paket_bootstrap do
  system 'tools/paket.exe', 'restore', clr_command: true
end

desc 'Perform full build'
build :compile => [:versioning, :restore, :assembly_info, :yolo] do |b|
  b.prop 'Configuration', Configuration
  b.sln = 'src/Suave.Locale.sln'
end

directory 'build/pkg'

nugets_pack :create_nugets_quick => [:versioning] do |x|
  x.configuration = Configuration
  x.output  = 'build/pkg'
  x.exe     = 'tools/paket.exe'
  x.version = ENV['NUGET_VERSION']
  x.files   = [ 'src/Suave.Locale/Suave.Locale.fsproj' ]
  x.authors = 'Henrik Feldt'
  x.description = 'An internationalisation WebPart and library for Suave for use with React-Intl or other i18n systems.'
end

desc 'package nugets - finds all projects and package them'
task :create_nugets => ['build/pkg', :versioning, :compile, :create_nugets_quick]

namespace :tests do
  task :unit do
    system "src/Suave.Locale.Tests/bin/#{Configuration}/Suave.Locale.Tests.exe",
      %|--sequenced|,
      clr_command: true
  end
end

task :tests => :'tests:unit'

task :default => [:create_nugets, :tests ]

task :ensure_nuget_key do
  raise 'missing env NUGET_KEY value' unless ENV['NUGET_KEY']
end

Albacore::Tasks::Release.new :release,
                             pkg_dir: 'build/pkg',
                             depend_on: [:create_nugets, :ensure_nuget_key],
                             nuget_exe: 'tools/paket.exe',
                             api_key: ENV['NUGET_KEY']
