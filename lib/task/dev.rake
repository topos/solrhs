# -*- coding: utf-8 -*-
namespace :dev do
  desc "start"
  task :start, [:build_mode] do |t,arg|
    require 'listen'
    arg.with_defaults(build_mode: 'all')
    Dir.chdir(SRC_DIR) do
      make_all
      @listener = Listen.to!('.', relative_path: true, filter: /\.hs$/) do |modified, added, removed|
        if arg.build_mode == 'all'
          make_all unless FileList[([modified]+[added]).flatten].exclude(/^.+Spec\.hs$/).empty?
        else # anything, e.g.: "inc"
          make_inc(modified, added, removed)
        end
        make_spec(modified, added) unless FileList[([modified]+[added]).flatten].ext('Spec.hs').empty?
      end
    end
  end

  task :all do
    Dir.chdir(SRC_DIR) do
      make_all
    end
  end
    
  desc "run all tests (specs)"
  task :test => :spec
    
  desc "run specs or a single one"
  task :spec, [:spec] do |t,arg|
    arg.with_defaults(spec: 'all')
    if arg.spec =~ /^.+Spec$/
      sh "#{arg.spec}"
    elsif arg.spec == '-1'
      Dir.chdir(SRC_DIR) do
        sh "./#{Dir.entries('.').sort_by{|f|File.mtime(f)}.select{|f|f =~ /^.+Spec$/}.last}"
      end
    elsif File.exists?(SRC_DIR+arg.spec+'Spec')
      sh SRC_DIR+arg.spec+'Spec'
    else # all
      Dir.chdir(SRC_DIR) do
        Dir.entries('.').sort_by{|f|File.mtime(f)}.select{|f|f=~/^.+Spec$/}.each do |spec|
          sh "./#{spec}"
        end
      end
    end
  end
  
  desc "build app"
  task :build, [:dev] do |t,arg|
    #sh "#{['clean', 'configure', 'build'].map{|s|"#{cabal(arg)} #{s}"}.join(' && ')}"
  end

  desc "install app"
  task :install, [:dev] do |t,arg|
    #sh "#{['clean', 'configure', 'install'].map{|s|"#{cabal(arg)} #{s}"}.join(' && ')}"
  end

  desc "clean"
  task :clean, [:dev] do |t,arg|
    Dir.chdir(SRC_DIR) do
      fs = FileList.new(['*Spec','*.o','*.hi','*.hc'].map{|g|"./**/#{g}"})
      sh "rm -f Main #{fs.join(' ')}"
    end
  end
  
  desc "ghci"
  task :ghci do
    unless Dir.exists?(File.expand_path('~/.ghci'))
      File.open(File.expand_path('~/.ghci'),'w'){|f|f.write(dot_ghci)}
    end
    Dir.chdir("#{PROJ_HOME}/lib") do
      sh "export GHC_PACKAGE_PATH=#{GHC_PACKAGE_PATH}:; ghci -cpp"
    end
  end

  desc "graphical rep. of \"git diff\""
  task :diff, [:csv] do |t,arg| 
    arg.with_defaults(csv: "")
    if arg[:csv].empty?
      sh "gdiff"
    else
      arg[:csv].split.each{|f| sh "gdiff #{f}"}
    end
  end
  
  desc "init dev. env.: cabal-dev install"
  task :init => [:gems] do
    task('cabal:init').invoke
  end

  desc "reset (remove cabal-dev) dev. env."
  task :reset => [:clean] do
    task('cabal:clean').invoke
  end

  desc "install gems"
  task :gems do
    Dir.chdir(PROJ_DIR) do
      sh "bundle install"
    end
  end

  desc "update dev env., e.g.: cabal:update"
  task :update do
    sh "rake cabal:update"
  end
  
  desc "clean: rm -rf ./dist/*"
  task :clean do
    sh "rm -rf #{PROJ_HOME}/dist/*"
  end

  desc "info"
  task :info do
    puts PROJ_HOME.red
    puts "rake: PATH=#{ENV['PATH']}"
    vs = []
    ['ghc', 'cabal', 'cabal-dev'].each do |c|
       vs << `#{c} --version`.strip
    end
    vs.each_with_index do |v,i|
      if i%2 == 0
        puts "#{v}".green
      else
        puts "#{v}"
      end
    end
  end

  # cabal may not be needed--static compilation may not require external packages
  def cabal(arg)
    arg.with_defaults(:dev => 'dev')
    if arg[:dev] == 'prod'
      'cabal'
    else
      'cabal-dev'
    end
  end

  desc "rsync/init a GHC project"
  task :rsync_proj, [:proj_name,:delete] do |t,arg|
    arg.defaults(delete:'false')
    PROJ_DIR = File.expand_path("#{File.dirname(__FILE__)}/../../../.")
    Dir.chdir(PROJ_DIR) do
      sh "mkdir -p arg.proj_name"
      rsync = [] << "rsync -axv --exclude .git --exclude '*~*'"
      if arg.delete =~ /^(del|delete)$/
        rsync << '--delete --delete-excluded'
      end
      rsync << "#{PROJ_HOME}/"
      rsync << "#{PROJ_DIR}/#{arg.proj_name}/"
      sh "#{rsync.join(' ')}"
    end
    Dir.chdir("#{PROJ_DIR}/#{arg.proj_name}") do
      ['bin','etc','src'].each do |dir|
        sh "mkdir -p #{dir}" unless Dir.exists?(dir)
      end
      sh "git init" unless Dir.exists?('.git')
    end
  end

  def make_all
    puts "- #{DateTime.now.strftime('%I:%M:%S')} (all)".yellow
    IO.popen("#{GHC} --make #{FileList.new('*.hs').exclude('*Spec.hs').join(' ')} -o Main 2>&1") do |io|
      Process.wait(io.pid)
      putsh($? == 0, io.readlines.select{|l|l.size > 0})
      io.close
    end
  end

  def make_inc(modified, added, removed)
    puts "- #{DateTime.now.strftime('%I:%M:%S')} (inc)".yellow
    unless (modified+added).empty?
      if compiler(modified+added)
        src = FileList['*.hs']
        src2obj = src.ext('o')
        obj = FileList['*.o']
        (src2obj-obj).ext('hs').each{|f|puts "#{f}".green}
        linker
      end
    end
  end

  def make_spec(modified, added)
    puts "- #{DateTime.now.strftime('%H:%M:%S')} (spec)".yellow
    ([modified]+[added]).flatten.each do |s|
      if s =~ /^.+Spec\.hs$/
        IO.popen("#{GHC} --make #{s} -o #{s.split('.').first} 2>&1") do |io|
          Process.wait(io.pid)
          putsh($? == 0, io.readlines.select{|l|l.size > 0})
          io.close
        end
      end
    end
  end
  
  def compiler(src)
    FileList[src].ext('o').each{|f|File.delete(f) if File.exists?(f)}
    (src.class == String ? [src] : src).each do |f|
      puts "#{GHC} -c #{f}".yellow
      IO.popen("#{GHC} -c #{f} 2>&1") do |io|
        Process.wait(io.pid)
        putsh($? == 0, io.readlines.select{|l|l.size > 0}, __callee__)
        io.close
        if $? != 0
          puts "status=#{$?}"
          return false
        end
      end
    end
    true
  end

  def linker
    puts "#{GHC} *.o -o Main".yellow
    IO.popen("#{GHC} *.o -o Main 2>&1") do |io|
      Process.wait(io.pid)
      putsh($? == 0, io.readlines.select{|l|l.size > 1}, __callee__)
      io.close
    end
  end
  
  def putsh(ok,res,phase =nil)
    if ok
      res.each{|l|print "#{l}".green}
      puts phase.nil? ? "passed".green.bold : "#{phase} passed".green.bold
    else
      res.each{|l|print "#{l}".red}
      puts phase.nil? ? "failed".red.bold : "#{phase} failed".red.bold
    end
  end
  
  def dot_ghci
    <<EOF
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Parallel

import Data.String
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad.IO.Class

:set prompt "λ: "

:set -fno-warn-unused-imports
:def hlint const . return $ ":! hlint \\"src\\""
:def hoogle \\s -> return $ ":! hoogle --count=15 \\"" ++ s ++ "\\""
:def pl \\s -> return $ ":! pointfree \\"" ++ s ++ "\\""
EOF
  end
end
