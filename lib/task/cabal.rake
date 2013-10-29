namespace :cabal do
  desc "install cabal-sandbox packages"
  task :install, [:cabal] do |t,arg|
    pkgs = `rake cabal:list`.split("\n").map{|l|l.strip.split.join('-')}
    Dir.chdir(PROJ_DIR) do
      pkg_list = []
      File.readlines('./lib/cabal.list').map{|l|l.strip}.each do |pkg|
        next if pkg =~ /^\s*#.*$/ || pkg =~ /^\s*$/
        unless pkgs.include?(pkg)
          pkg_list << pkg
        else
          puts "#{pkg} " + "already installed".green
        end
      end
      if pkg_list.size > 0
        sh "cabal update"
        pkg_list.each do |pkg|
          # hack--since "cabal-dev list --installed" doesn't report "yesod-bin" we check for its binary "yesod"
          next if pkg.start_with?("yesod-bin-") && File.exists?("#{CABAL_DEV_DIR}/bin/yesod")
          if platform?('linux')
            sh "cabal install #{pkg}"
          elsif platform?('darwin')
            sh "cabal install --extra-include-dirs=#{EXTRA_INC} --extra-lib-dirs=#{EXTRA_LIB} #{pkg}"
          else
            raise "unrecognized platform"
          end
        end
      end
    end
  end

  desc "list cabal-sandbox packages"
  task :list, [:cabal,:remote] do |t,arg|
    Dir.chdir(LIB_DIR) do
      if arg.cabal.nil?
        sh "cabal list --installed --simple-output"
      else
        if arg.remote.nil?
          sh "cabal list --installed --simple-output #{arg.cabal}"
        else
          sh "cabal list --simple-output #{arg.cabal}"
        end
      end
    end
  end
  
  desc "init. your cabal sandbox"
  task :init do
    Dir.chdir(PROJ_DIR) do
      sh "cabal update"
      sh "cabal sandbox init"
    end
    #task('cabal:link').invoke
  end


  task :deps => :install_dependencies
  desc "install only dependencies"
  task :install_dependencies do
    sh "cabal install --only-dependencies"
  end
  
  desc "clobber (remove) your cabal sandbox"
  task :clobber do
    Dir.chrdir(PROJ_DIR) do
      sh "cabal sandbox delete"
    end
    # sh "rm -f #{PROJ_DIR}/cabal.sandbox.config"
    # sh "rm -rf #{PROJ_DIR}/.cabal-sandbox"
  end
end
