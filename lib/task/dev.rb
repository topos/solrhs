require 'rubygems' 
require 'bundler/setup'
require 'date'
require 'yaml'
require 'smart_colored/extend'

def platform?(p)
  `uname -s`.strip.downcase == p.downcase
end

def proj_dir(subdir =nil)
  path = [] << PROJ_DIR
  path << subdir unless subdir.nil?
  path.join('/')
end

def process_running?(name, argfilter =nil)
  require 'sys/proctable'
  include Sys
  ProcTable.ps do |proc|
    if argfilter.nil?
      return true if proc.comm == name
    else
      return true if proc.comm == name && proc.cmdline.split.include?(argfilter)
    end
  end
  false
end

def proj_mode
  ENV['PROJ_MODE'].nil? ? 'Development' : ENV['PROJ_MODE']
end


PROJ_DIR = File.expand_path("#{File.dirname(__FILE__)}/../../.")
SRC_DIR = proj_dir('src')
ETC_DIR = proj_dir('etc')
LIB_DIR = proj_dir('lib')
CABAL_DEV_DIR = proj_dir('lib/cabal-dev')

PROJ_HOME = PROJ_DIR

GHC_PACKAGE_PATH = "#{PROJ_DIR}/.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"
EXTRA_INC,EXTRA_LIB = platform?('darwin') ? ['/usr/local/include','/usr/local/lib'] : ['','']
GHC = "ghc -no-user-package-db -package-db #{GHC_PACKAGE_PATH} -threaded"

_path = []
_path << "#{PROJ_DIR}/bin"
_path << "#{PROJ_DIR}/lib/cabal-dev/bin"
_path << '~/.cabal/bin'
_path << (platform?('darwin') ? '~/Library/Haskell/bin' : '/opt/hp/bin')
_path << '/usr/local/bin'
_path << '/usr/bin'
_path << '/bin'

ENV['PATH'] = _path.join(':')
